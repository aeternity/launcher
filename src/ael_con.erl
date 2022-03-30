%%% @doc
%%% ÆL Controller
%%%
%%% This process is a in charge of maintaining the program's core state, coordination
%%% and commuication among interface processes, and any other state that is globally
%%% depended upon. The closest thing to a GUI companion to the controller is the
%%% ael_gui process.
%%% @end

-module(ael_con).
-vsn("0.1.3").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(gen_server).
% General GUI interface
-export([show_ui/1]).
% Node GUI interface
-export([toggle_node/1, build_complete/4, build_cancelled/0]).
% Config Interface
-export([save_conf/3, read_conf/1, drop_conf/1]).
% Utility functions
-export([var/0, conf_dir_path/0, data_root/0, platform/0]).
% gen_server
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).
-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").


%%% Type and Record Definitions

-record(s,
        {window   = none :: none | wx:wx_object(),
         node     = none :: none | stopped | running | {building, pid()},
         base_dir = none :: none | file:filename(),
         build    = none :: none | ael:build_meta(),
         platform = none :: none | ael:platform(),
         cores    = 2    :: pos_integer(),
         deps     = []   :: [dep()],
         loaded   = []   :: [atom()],
         tasks    = []   :: [ui()],
         manifest = []   :: [ael:conf_meta()],
         conf     = none :: string()}).

-record(ui,
        {name = none :: none | ui_name(),
         pid  = none :: none | pid(),
         wx   = none :: none | wx:wx_object(),
         mon  = none :: none | reference()}).

-type state()     :: #s{}.
-type ui_name()   :: ael_v_conf
                   | ael_v_node
                   | ael_v_chain
                   | ael_v_dev
                   | ael_v_network
                   | ael_v_mempool
                   | {ael_v_conf_editor, Name :: string() | new}.
-type ui()        :: #ui{}.
-type dep()       :: {AppName :: atom(), Version :: string(), RelPath :: string()}.


%%% Interface

-spec toggle_node(ConfName :: string()) -> ok | running.

toggle_node(ConfName) ->
    gen_server:call(?MODULE, {toggle_node, ConfName}).


-spec build_complete(AEVer, ERTSVer, BaseDir, Deps) -> ok
    when AEVer      :: string(),
         ERTSVer    :: string(),
         BaseDir    :: file:filename(),
         Deps       :: [dep()].

build_complete(AEVer, ERTSVer, BaseDir, Deps) ->
    gen_server:cast(?MODULE, {build_complete, self(), {AEVer, ERTSVer, BaseDir, Deps}}).


-spec build_cancelled() -> ok.

build_cancelled() ->
    gen_server:cast(?MODULE, build_cancelled).


-spec show_ui(Name) -> ok
    when Name :: ui_name().

show_ui(Name) ->
    gen_server:cast(?MODULE, {show_ui, Name}).


-spec save_conf(OldMeta, NewMeta, Conf) -> ok
    when OldMeta :: ael:conf_meta(),
         NewMeta :: ael:conf_meta(),
         Conf    :: map().

save_conf(OldMeta, NewMeta, Conf) ->
    gen_server:cast(?MODULE, {save_conf, OldMeta, NewMeta, Conf}).


-spec read_conf(Name) -> {ok, map()} | error
    when Name :: string().

read_conf("") ->
    {ok, #{}};
read_conf(Name) ->
    gen_server:call(?MODULE, {read_conf, Name}).


-spec drop_conf(Name) -> ok
    when Name :: string().

drop_conf(Name) ->
    gen_server:cast(?MODULE, {drop_conf, Name}).


-spec platform() -> ael:platform().

platform() ->
    gen_server:call(?MODULE, platform).



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
    Platform = determine_platform(),
    Window = ael_gui:start_link(BuildMeta, Platform),
    Manifest = check_conf_manifest(),
    ok = log(info, "Window: ~p", [Window]),
    State = #s{window   = Window,
               build    = BuildMeta,
               platform = Platform,
               cores    = core_count(),
               manifest = Manifest},
    {ok, State}.

get_build_meta() ->
    case file:consult(build_meta_path()) of
        {ok, Meta}      -> proplists:get_value(build, Meta, none);
        {error, enoent} -> none
    end.

check_conf_manifest() ->
    ManifestPath = conf_manifest_path(),
    case file:consult(ManifestPath) of
        {ok, Manifest} ->
            Manifest;
        {error, enoent} ->
            ok = filelib:ensure_dir(ManifestPath),
            generate_manifest(ManifestPath)
    end.

generate_manifest(ManifestPath) ->
    ConfRoot = filename:dirname(ManifestPath),
    Mainnet = "ae_mainnet",
    Testnet = "ae_uat",
    MainnetData = filename:join(data_root(), Mainnet),
    TestnetData = filename:join(data_root(), Testnet),
    Confs =
        [{#conf_meta{name = "Mainnet Node",
                     path = filename:join(ConfRoot, "mainnet_node.json"),
                     memo = "Mainnet base configuration",
                     data = MainnetData},
          #{"fork_management" => #{"network_id" => Mainnet},
            "chain"           => #{"db_path"    => MainnetData}}},
         {#conf_meta{name = "Testnet Node",
                     path = filename:join(ConfRoot, "testnet_node.json"),
                     memo = "Testnet base configuration",
                     data = TestnetData},
          #{"fork_management" => #{"network_id" => Testnet},
            "chain"           => #{"db_path"    => TestnetData}}}],
    ok = lists:foreach(fun write_conf/1, Confs),
    Meta = [element(1, C) || C <- Confs],
    ok = do_save_manifest(Meta),
    Meta.


%%% gen_server Message Handling Callbacks

handle_call({toggle_node, ConfName}, _, State) ->
    {Response, NewState} = do_toggle_node(State, ConfName),
    {reply, Response, NewState};
handle_call({read_conf, Name}, _, State) ->
    Response = do_read_conf(Name, State),
    {reply, Response, State};
handle_call(platform, _, State = #s{platform = Platform}) ->
    {reply, Platform, State};
handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


handle_cast({show_ui, Name}, State) ->
    NewState = do_show_ui(Name, State),
    {noreply, NewState};
handle_cast({save_manifest, Entries}, State) ->
    ok = do_save_manifest(Entries),
    {noreply, State};
handle_cast({save_conf, OldMeta, NewMeta, Conf}, State) ->
    NewState = do_save_conf(OldMeta, NewMeta, Conf, State),
    {noreply, NewState};
handle_cast({drop_conf, Name}, State) ->
    NewState = do_drop_conf(Name, State),
    {noreply, NewState};
handle_cast({build_complete, Builder, Meta}, State) ->
    NewState = do_build_complete(Builder, Meta, State),
    {noreply, NewState};
handle_cast(build_cancelled, State) ->
    NewState = do_build_cancelled(State),
    {noreply, NewState};
handle_cast(stop, State) ->
    ok = log(info, "Received a 'stop' message."),
    {stop, normal, State};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


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


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    zx:stop().



%%% Doer functions

do_show_ui(Task = {ael_v_conf_editor, Name}, State = #s{tasks = Tasks}) ->
    case lists:keyfind(Task, #ui.name, Tasks) of
        #ui{wx = Win} ->
            ok = ael_v_conf_editor:to_front(Win),
            State;
        false ->
            Win = ael_v_conf_editor:start_link(conf_args(Name, State)),
            PID = wx_object:get_pid(Win),
            Mon = monitor(process, PID),
            UI = #ui{name = Name, pid = PID, wx = Win, mon = Mon},
            State#s{tasks = [UI | Tasks]}
    end;
do_show_ui(Name, State = #s{tasks = Tasks}) ->
    case lists:keyfind(Name, #ui.name, Tasks) of
        #ui{wx = Win} ->
            ok = Name:to_front(Win),
            State;
        false ->
            Win = Name:start_link(task_args(Name, State)),
            PID = wx_object:get_pid(Win),
            Mon = monitor(process, PID),
            UI = #ui{name = Name, pid = PID, wx = Win, mon = Mon},
            State#s{tasks = [UI | Tasks]}
    end.

conf_args("", #s{cores = Cores}) ->
    {#conf_meta{}, #{}, Cores};
conf_args(Name, #s{manifest = Manifest, cores = Cores}) ->
    Meta = #conf_meta{path = Path} = lists:keyfind(Name, #conf_meta.name, Manifest),
    {ok, Bin} = file:read_file(Path),
    {ok, Conf} = zj:decode(Bin),
    {Meta, Conf, Cores}.

task_args(ael_v_node, #s{manifest = Manifest}) ->
    [Name || #conf_meta{name = Name} <- Manifest];
task_args(ael_v_conf, #s{manifest = Manifest}) ->
    Manifest;
task_args(_, _) ->
    none.


do_save_manifest(Entries) ->
    ManifestPath = conf_manifest_path(),
    ok = filelib:ensure_dir(ManifestPath),
    zx_lib:write_terms(ManifestPath, Entries).



do_save_conf(Meta,
             Meta = #conf_meta{name = Name},
             Conf,
             State = #s{manifest = Manifest}) ->
    ok = log(info, "Saving conf"),
    NewManifest = lists:keystore(Name, #conf_meta.name, Manifest, Meta),
    ok = write_conf({Meta, Conf}),
    ok = do_save_manifest(NewManifest),
    ok = notify_manifest_update(NewManifest),
    State#s{manifest = NewManifest};
do_save_conf(#conf_meta{name = OldName},
             NewMeta,
             Conf,
             State) ->
    ok = log(info, "Saving with new conf meta"),
    NewState = do_drop_conf(OldName, State),
    do_save_conf(NewMeta, NewMeta, Conf, NewState).


do_read_conf(Name, #s{manifest = Manifest}) ->
    #conf_meta{path = Path} = lists:keyfind(Name, #conf_meta.name, Manifest),
    {ok, Bin} = file:read_file(Path),
    zj:decode(Bin).


do_drop_conf(Name, State = #s{manifest = Manifest}) ->
    NewManifest =
        case lists:keytake(Name, #conf_meta.name, Manifest) of
            {value, #conf_meta{path = Path}, []} ->
                ok = file:delete(Path),
                ResetManifest = generate_manifest(conf_manifest_path()),
                ok = notify_manifest_update(ResetManifest),
                ResetManifest;
            {value, #conf_meta{path = Path}, NextManifest} ->
                ok = file:delete(Path),
                ok = do_save_manifest(NextManifest),
                ok = notify_manifest_update(NextManifest),
                NextManifest;
            false ->
                Manifest
        end,
    State#s{manifest = NewManifest}.


notify_manifest_update(NewManifest) ->
    ok = ael_v_node:set_manifest(NewManifest),
    ok = ael_v_conf:set_manifest(NewManifest).


write_conf({#conf_meta{path = Path}, Conf}) ->
    JSON = zj:encode(Conf),
    ok = file:write_file(Path, [JSON, "\n"]).


do_toggle_node(State = #s{node = none, build = BuildMeta}, ConfName) ->
    ok = ael_v_node:set_button(run, false),
    {ok, BPID} = ael_builder:start_link(BuildMeta),
    {ok, State#s{node = {building, BPID}, conf = ConfName}};
do_toggle_node(State = #s{node = stopped}, ConfName) ->
    NewState = start_node(State#s{conf = ConfName}),
    {ok, NewState};
do_toggle_node(State = #s{node = running}, _) ->
    NewState = stop_ae(State),
    {running, NewState};
do_toggle_node(State = #s{node = {buildig, _}}, _) ->
    {ok, State}.


stop_ae(State = #s{node = running, loaded = Apps}) ->
    ok = ael_monitor:stop_poll(),
    Ignore =
        [aecore,
         asn1,
         xmerl,
         public_key,
         compiler,
         crypto,
         syntax_tools,
         observer,
         ssl,
         sasl,
         runtime_tools,
         mnesia_rocksdb,
         rocksdb,
         mnesia],
    ToRemove = Apps -- Ignore,
    DoFirst = [aecore],
    DoLast = [mnesia_rocksdb, rocksdb, mnesia],
    AppsInOrder = DoFirst ++ ToRemove ++ DoLast,
    ok = lists:foreach(fun remove/1, AppsInOrder),
    ok = net_kernel:stop(),
    State#s{node = stopped, loaded = []};
stop_ae(State) ->
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


do_build_cancelled(State = #s{node = {building, _}}) ->
    ok = ael_v_node:set_button(run, true),
    State#s{node = none}.


start_node(State = #s{base_dir = BaseDir,  deps     = Deps,
                      conf     = ConfName, manifest = Manifest}) ->
    ok = ael_v_node:set_button(stop, true),
    Selected = lists:keyfind(ConfName, #conf_meta.name, Manifest),
    #conf_meta{path = ConfPath, data = DataPath} = Selected,
    true = os:putenv("AETERNITY_CONFIG", ConfPath),
    ok = maybe_move_files(BaseDir, DataPath),
    Apps = add_libs(BaseDir, Deps),
    {ok, Started} = application:ensure_all_started(app_ctrl),
    ok = ael_gui:show(io_lib:format("Prestarted: ~p.~n", [Started])),
    {ok, _} = application:ensure_all_started(aesync),
    ok = ael_monitor:start_poll(1000),
    State#s{node = running, loaded = Apps}.

maybe_move_files(BaseDir, DataPath) ->
    ok = maybe_copy_silly_files(BaseDir),
    case filelib:is_dir(DataPath) of
        true ->
            ok = ael_gui:show("No need to populate...\n");
        false ->
            ok = ael_gui:show("Populating data directory...\n"),
            ok = run_once_out_of_context(BaseDir),
            ok = move_delicious_data_bits(BaseDir)
    end.

maybe_copy_silly_files(BaseDir) ->
    NonsenseThatShouldBeOptional = ["REVISION", "VERSION"],
    AE_Home = var(),
    Copy =
        fun(F) ->
            Dst = filename:join(AE_Home, F),
            case filelib:is_regular(Dst) of
                true ->
                    ok;
                false ->
                    Src = filename:join(BaseDir, F),
                    {ok, _} = file:copy(Src, Dst),
                    ok
            end
        end,
    lists:foreach(Copy, NonsenseThatShouldBeOptional).

run_once_out_of_context(BaseDir) ->
    AE = filename:join(BaseDir, "bin/aeternity"),
    Command = AE ++ " foreground",
    Tron = spawn_link(fun() -> stop_the_madness(AE) end),
    _ = erlang:send_after(15000, Tron, redrum),
    ael_os:cmd(Command).

stop_the_madness(AE) ->
    receive redrum -> os:cmd(AE ++ " stop") end.

move_delicious_data_bits(BaseDir) ->
    Var = var(),
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
    ok = file:set_cwd(var()),
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
    filename:join(var(), "aeternity_build.eterms").

conf_manifest_path() ->
    filename:join(conf_dir_path(), "manifest.eterms").

conf_dir_path() ->
    filename:join(var(), "conf").

data_root() ->
    filename:join(var(), "data").



%%% Platform Identification

-spec determine_platform() -> Result
    when Result  :: {OS, OTP, ERTS},
         OS      :: {OSType, Version},
         OSType  :: devuan
                  | debian
                  | ubuntu
                  | gentoo
                  | slack
                  | fedora
                  | rhel
                  | arch
                  | suse
                  | linux
                  | osx
                  | unix
                  | windows
                  | unknown,
         Version :: string() | unknown,
         OTP     :: string(),
         ERTS    :: string().

determine_platform() ->
    OS =
        case os:type() of
            {unix,  linux}  -> linux();
            {unix,  darwin} -> osx();
            {win32, nt}     -> windows();
            Other           -> {unknown, Other}
        end,
    OTP = erlang:system_info(otp_release),
    ERTS = erlang:system_info(version),
    {OS, OTP, ERTS}.

linux() ->
    Methods = [fun os_release/0, fun hostnamectl/0, fun uname_r/0],
    attempt(Methods).

attempt([H | T]) ->
    case H() of
        {ok, Result} -> Result;
        failed       -> attempt(T)
    end;
attempt([]) ->
    {linux, unknown}.

os_release() ->
    PossibleReleaseFiles =
        ["/etc/os-release",
         "/usr/lib/os-release",
         "/etc/initrd-release"],
    case try_read(PossibleReleaseFiles) of
        {ok, Contents} ->
            Text = unicode:characters_to_list(Contents),
            Entries = sploot(Text, "="),
            ID = proplists:get_value("ID", Entries),
            LIKE = proplists:get_value("ID_LIKE", Entries),
            VERSION = proplists:get_value("VERSION_ID", Entries),
            os_release(ID, LIKE, VERSION);
        error ->
            failed
    end.

try_read([H | T]) ->
    case file:read_file(H) of
        {ok, Contents} -> {ok, Contents};
        _              -> try_read(T)
    end;
try_read([]) ->
    error.

os_release("devuan", "debian", Version) -> {ok, {devuan, Version}};
os_release("debian", "debian", Version) -> {ok, {debian, Version}};
os_release("ubuntu", "debian", Version) -> {ok, {ubuntu, Version}};
os_release(_,        "debian", Version) -> {ok, {debian, Version}};
os_release(_,        _,        Version) -> {ok, {linux,  Version}}.

hostnamectl() ->
    case length(os:cmd("which hostnamectl")) > 0 of
        true ->
            Entries = sploot(os:cmd("hostnamectl"), ":"),
            VerString = proplists:get_value("Kernel", Entries),
            {ok, {linux, VerString}};
        false ->
            failed
    end.

osx() ->
    MajorVersion = hd(string:lexemes(os:cmd("sw_vers -productVersion"), ".")),
    {osx, MajorVersion}.

windows() ->
    Version = proplists:get_value("OS Version", sploot(os:cmd("systeminfo"), ":")),
    {windows, Version}.

sploot(Text, Delimiter) ->
    [list_to_tuple(split_strip(E, Delimiter)) || E <- string:split(Text, "\n", all)].

split_strip(String, Delimiter) ->
    Trim = fun(S) -> string:trim(S, both, [32, $\t, $\n, $"]) end,
    lists:map(Trim, string:split(String, Delimiter)).

uname_r() ->
    {linux, unknown}.


core_count() ->
    Count = core_count(erlang:system_info(cpu_topology), 0),
    ok = tell("Core count: ~w", [Count]),
    Count.

core_count([], C) ->
    C;
core_count([{_, {logical, N}} | T], C) when is_integer(N) ->
    core_count(T, C + 1);
core_count([{_, Sub} | T], C) when is_list(Sub) ->
    core_count(T, core_count(Sub, C));
core_count([{_, _, {logical, N}} | T], C) when is_integer(N) ->
    core_count(T, C + 1);
core_count([{_, _, Sub} | T], C) when is_list(Sub) ->
    core_count(T, core_count(Sub, C)).
