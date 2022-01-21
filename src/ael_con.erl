%%% @doc
%%% ÆL Controller
%%%
%%% This process is a in charge of maintaining the program's core state, coordination and
%%% commuication among interface processes, and any other state that is globally depended
%%% upon. The closest thing to a GUI companion to the controller is the ael_gui process.
%%% @end

-module(ael_con).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(gen_server).
% Node GUI interface
-export([run_node/0, stop_ae/0, build_complete/4]).
% Conf GUI interface
-export([show_ui/1]).
% File manipulation
-export([read_conf/1, save_conf/1, drop_conf/1, var/0]).
% gen_server
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).
-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").


%%% Type and Record Definitions


-record(s,
        {window     = none :: none | wx:wx_object(),
         node       = none :: none | stopped | running | {building, pid()},
         base_dir   = none :: none | file:filename(),
         build      = none :: none | {AEVer :: string(), ERTSVer :: string()},
         deps       = []   :: [dep()],
         loaded     = []   :: [atom()],
         tasks      = []   :: [ui()],
         manifest   = []   :: [ael_conf:meta()]}).

-record(ui,
        {name = none :: none | ui_name(),
         pid  = none :: none | pid(),
         wx   = none :: none | wx:wx_object(),
         mon  = none :: none | reference()}).

-type state()   :: #s{}.
-type ui_name() :: ael_v_conf
                 | ael_v_node
                 | ael_v_chain
                 | ael_v_dev
                 | ael_v_network
                 | ael_v_mempool.
-type ui()      :: #ui{}.
-type dep()     :: {AppName :: atom(), Version :: string(), RelPath :: string()}.



%%% Interface

-spec run_node() -> ok.

run_node() ->
    gen_server:call(?MODULE, run_node).


-spec stop_ae() -> ok.

stop_ae() ->
    gen_server:cast(?MODULE, stop_ae).


-spec build_complete(AEVer, ERTSVer, BaseDir, Deps) -> ok
    when AEVer      :: string(),
         ERTSVer    :: string(),
         BaseDir    :: file:filename(),
         Deps       :: [dep()].

build_complete(AEVer, ERTSVer, BaseDir, Deps) ->
    gen_server:cast(?MODULE, {build_complete, self(), {AEVer, ERTSVer, BaseDir, Deps}}).


-spec show_ui(Name) -> ok
    when Name :: ui_name().

show_ui(Name) ->
    gen_server:cast(?MODULE, {show_ui, Name}).


-spec read_conf(Index) -> {ok, map()} | error
    when Index :: integer().

read_conf(Index) ->
    gen_server:call(?MODULE, {read_conf, Index}).


-spec save_conf(Meta) -> ok
    when Meta :: ael_conf:meta().

save_conf(Meta) ->
    gen_server:cast(?MODULE, {save_conf, Meta}).


-spec drop_conf(Index) -> ok
    when Index :: integer().

drop_conf(Index) ->
    gen_server:cast(?MODULE, {drop_conf, Index}).



%%% Lifecycle Functions

-spec start_link() -> Result
    when Result :: {ok, pid()}
                 | {error, Reason},
         Reason :: {already_started, pid()}
                 | {shutdown, term()}
                 | term().
%% @private
%% Called by ael_sup.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).


-spec stop() -> ok.

stop() ->
    gen_server:cast(?MODULE, stop).


%%% Initialization

-spec init(none) -> {ok, state()}.

init(none) ->
    ok = log(info, "Starting"),
    BuildMeta = get_build_meta(),
    Window = ael_gui:start_link(BuildMeta),
    ok = log(info, "Window: ~p", [Window]),
    State = #s{window = Window, build = BuildMeta},
    {ok, State}.

get_build_meta() ->
    case file:consult(build_meta_path()) of
        {ok, Meta}      -> proplists:get_value(build, Meta, none);
        {error, enoent} -> none
    end.


%%% gen_server Message Handling Callbacks

-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().
%% @private
%% The gen_server:handle_call/3 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_call-3

handle_call({show_editor, ConfMeta}, _, State) ->
    Win = ael_conf:start_link(ConfMeta),
    {reply, Win, State};
handle_call(run_node, _, State) ->
    {Response, NewState} = do_run_node(State),
    {reply, Response, NewState};
handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


-spec handle_cast(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_cast/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2

handle_cast({show_ui, Name}, State) ->
    NewState = do_show_ui(Name, State),
    {noreply, NewState};
handle_cast({save_conf_meta, Entries}, State) ->
    ok = do_save_conf_meta(Entries),
    {noreply, State};
handle_cast({build_complete, Builder, Meta}, State) ->
    NewState = do_build_complete(Builder, Meta, State),
    {noreply, NewState};
handle_cast(stop_ae, State) ->
    NewState = do_stop_ae(State),
    {noreply, NewState};
handle_cast(stop, State) ->
    ok = log(info, "Received a 'stop' message."),
    {stop, normal, State};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_info(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_info/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2

handle_info({'DOWN', Mon, process, PID, Info}, State) ->
    NewState = handle_down(Mon, PID, Info, State),
    {noreply, NewState};
handle_info(Unexpected, State) ->
    ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


handle_down(Mon, PID, Info, State = #s{tasks = Tasks}) ->
    case lists:keytake(Mon, #ui.mon, Tasks) of
        {value, #ui{}, NewTasks} ->
            State#s{tasks = NewTasks};
        false ->
            Unexpected = {'DOWN', Mon, process, PID, Info},
            ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
            State
    end.


%% @private
%% gen_server callback to handle state transformations necessary for hot
%% code updates. This template performs no transformation.

code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    zx:stop().



%%% Doer functions

do_show_ui(Name, State = #s{tasks = Tasks}) ->
    case lists:keyfind(Name, #ui.name, Tasks) of
        #ui{wx = Win} ->
            ok = Name:to_front(Win),
            State;
        false ->
            Conf = init_args(Name, State),
            Win = Name:start_link(Conf),
            PID = wx_object:get_pid(Win),
            Mon = monitor(process, PID),
            UI = #ui{name = Name, pid = PID, wx = Win, mon = Mon},
            State#s{tasks = [UI | Tasks]}
    end.


init_args(ael_v_conf, _) ->
    MetaPath = conf_meta_path(),
    ok = filelib:ensure_dir(MetaPath),
    Desc = fun(A, B) -> element(2, A) > element(2, B) end,
    case file:consult(MetaPath) of
        {ok, Entries}   -> lists:sort(Desc, Entries);
        {error, enoent} -> []
    end;
init_args(_, _) ->
    none.


do_save_conf_meta(Entries) ->
    MetaPath = conf_meta_path(),
    ok = filelib:ensure_dir(MetaPath),
    zx_lib:write_terms(MetaPath, Entries).

do_run_node(State = #s{node = none, build = BuildMeta}) ->
    {ok, BPID} = ael_builder:start_link(BuildMeta),
    {ok, State#s{node = {building, BPID}}};
do_run_node(State = #s{node = stopped}) ->
    NewState = start_node(State),
    {ok, NewState};
do_run_node(State) ->
    {running, State}.


do_stop_ae(State = #s{node = running, loaded = Apps}) ->
    ok = ael_monitor:stop_poll(),
    Ignore =
        [asn1,
         xmerl,
         public_key,
         compiler,
         crypto,
         syntax_tools,
         observer,
         sasl,
         runtime_tools],
    ToRemove = Apps -- Ignore,
    AppsInOrder = [aecore | lists:delete(aecore, ToRemove)],
    ok = lists:foreach(fun remove/1, AppsInOrder),
    State#s{node = stopped, loaded = []};
do_stop_ae(State) ->
    State.

remove(App) ->
    Format =
        case application:stop(App) of
            ok                          -> "Stopped: ~tp~n";
            {error, {not_started, App}} -> "Unloaded: ~tp~n"
        end,
    Message = io_lib:format(Format, [App]),
    ok = application:unload(App),
    ael_gui:show(Message).


do_build_complete(BPID,
                  {AEVer, ERTSVer, BaseDir, Deps},
                  State = #s{node = {building, BPID}}) ->
    Meta = {build, {AEVer, ERTSVer}},
    ok = zx_lib:write_terms(build_meta_path(), [Meta]),
    start_node(State#s{base_dir = BaseDir, build = {AEVer, ERTSVer}, deps = Deps});
do_build_complete(_, _, State) ->
    State.


%%% TODO: Make this dependent on the configuration chosen
start_node(State = #s{base_dir = BaseDir, deps = Deps}) ->
    ok = maybe_move_files(BaseDir),
    Apps = add_libs(BaseDir, Deps),
    {ok, Started} = application:ensure_all_started(app_ctrl),
    ok = ael_gui:show(io_lib:format("Prestarted: ~p.~n", [Started])),
    {ok, _} = application:ensure_all_started(aesync),
    ok = ael_monitor:start_poll(1000),
    State#s{node = running, loaded = Apps}.

maybe_move_files(BaseDir) ->
    case filelib:is_file(filename:join(ael_con:var(), "data")) of
        true ->
            ok = ael_gui:show("No need to move files to data/\n");
        false ->
            ok = ael_gui:show("Populating data/\n"),
            ok = copy_silly_files(BaseDir),
            ok = run_once_out_of_context(BaseDir),
            ok = move_delicious_data_bits(BaseDir)
    end.

copy_silly_files(BaseDir) ->
    NonsenseThatShouldBeOptional = ["REVISION", "VERSION"],
    AE_Home = ael_con:var(),
    Copy =
        fun(F) ->
            Src = filename:join(BaseDir, F),
            Dst = filename:join(AE_Home, F),
            {ok, _} = file:copy(Src, Dst)
        end,
    lists:foreach(Copy, NonsenseThatShouldBeOptional).

run_once_out_of_context(BaseDir) ->
    AE = filename:join(BaseDir, "bin/aeternity"),
    Command = AE ++ " foreground",
    Tron = spawn_link(fun() -> stop_the_madness(AE) end),
    _ = erlang:send_after(60000, Tron, redrum),
    ael_os:cmd(Command).

stop_the_madness(AE) ->
    receive redrum -> os:cmd(AE ++ " stop") end.

move_delicious_data_bits(BaseDir) ->
    Var = ael_con:var(),
    Files = filelib:wildcard("data/**", BaseDir),
    Srcs = [filename:join(BaseDir, F) || F <- Files],
    Dsts = [filename:join(Var, F) || F <- Files],
    lists:foreach(fun relocate/1, lists:zip(Srcs, Dsts)).

relocate({Src, Dst}) ->
    ok = filelib:ensure_dir(Dst),
    case filelib:is_dir(Src) of
        false ->
            {ok, _} = file:copy(Src, Dst),
            ael_gui:show(io_lib:format("Moved ~s to ~s~n", [Src, Dst]));
        true ->
            Message = io_lib:format("Making dir ~s~n", [Dst]),
            ael_gui:show(Message),
            ok = file:make_dir(Dst)
    end.

add_libs(BaseDir, Deps) ->
    ok = file:set_cwd(ael_con:var()),
    Paths = [filename:join([BaseDir, element(3, Dep), "ebin"]) || Dep <- Deps],
    ok = code:add_pathsa(Paths),
    ok = ael_gui:show("Added paths successfully!\n"),
    PrintableDeps = io_lib:format("~tp", [Deps]),
    ok = ael_gui:show(PrintableDeps),
    load_apps(Paths).

load_apps(Paths) ->
    {ok, [Config]} = file:consult(filename:join(zx:get_home(), "priv/sys.config")),
    ok = application:set_env(Config),
    {ok, _} = net_kernel:start(['aeternity@localhost', longnames]),
    Loaded = [element(1, A) || A <- application:loaded_applications()],
    Load =
        fun(Path, AppNames) ->
            [AppFile] = filelib:wildcard(filename:join(Path, "*.app")),
            {ok, [AppDesc]} = file:consult(AppFile),
            AppName = element(2, AppDesc),
            {Message, NextAppNames} =
                case lists:member(AppName, Loaded) of
                    true ->
                        M = io_lib:format("App ~p already loaded.~n", [AppName]),
                        {M, AppNames};
                    false ->
                        ok = application:load(AppDesc),
                        M = io_lib:format("Loaded app ~p.~n", [AppName]),
                        {M, [AppName | AppNames]}
                end,
            ok = ael_gui:show(Message),
            NextAppNames
        end,
    lists:foldl(Load, [], Paths).


var() ->
    zx_daemon:dir(var).

build_meta_path() ->
    filename:join(var(), "aeternity_build.meta").

conf_meta_path() ->
    filename:join(var(), "aeternity_conf.meta").
