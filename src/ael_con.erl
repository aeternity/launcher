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
-export([run_node/0, build_complete/2]).
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
         node       = none :: none | stopped | running | {building, pid(), pid()},
         build      = none :: none | {AEVer :: string(), ERTSVer :: string()},
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



%%% Interface

-spec run_node() -> ok.

run_node() ->
    gen_server:cast(?MODULE, run_node).


-spec build_complete(AEVer, ERTSVer) -> ok
    when AEVer   :: string(),
         ERTSVer :: string().

build_complete(AEVer, ERTSVer) ->
    gen_server:cast(?MODULE, {build_complete, self(), AEVer, ERTSVer}).


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
handle_cast(run_node, State) ->
    NewState = do_run_node(State),
    {noreply, NewState};
handle_cast({build_complete, Builder, AEVer, ERTSVer}, State) ->
    NewState = do_build_complete(Builder, AEVer, ERTSVer, State),
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
%   GPID = spawn_link(ael_builder_gui, start, []),
%   State#s{node = {building, BPID, GPID}};
    State#s{node = {building, BPID, none}};
do_run_node(State) ->
    State.

do_build_complete(BPID, AEVer, ERTSVer, State = #s{node = {building, BPID, _}}) ->
    Meta = {build, {AEVer, ERTSVer}},
    ok = zx_lib:write_terms(build_meta_path(), [Meta]),
    {ok, Started} = application:ensure_all_started(app_ctrl),
    ok = ael_gui:show(io_lib:format("Prestarted: ~p.~n", [Started])),
    {ok, _} = application:ensure_all_started(aesync),
    ok = ael_monitor:start_poll(1000),
    State#s{node = stopped};
do_build_complete(_, _, _, State) ->
    State.


var() ->
    zx_daemon:dir(var).

build_meta_path() ->
    filename:join(var(), "aeternity_build.meta").

conf_meta_path() ->
    filename:join(var(), "aeternity_conf.meta").
