%%% @doc
%%% Aeternity Builder
%%%
%%% This module is the builder process for the Aeternity node code.
%%% It's job is to:
%%%  1. Identify what stage of completion a build may be in
%%%  2. Report the reason a build failed (if possible, otherwise simply report the
%%%     build error encountered and point to the error log output)
%%%  3. Execute whatever build step is required next.
%%%  4. Clean and retry a build if a partial build continuation fails.
%%%
%%% TODO:
%%% The build task should be made independent of the environment setting/wiping
%%% and loading tasks. The reason for this is to enable starting, stopping and
%%% re-starting the node in a variety of configurations, modes and chains.
%%% Future task definitions:
%%% 1. Check build / build
%%% 2. Environment wipe and code unload
%%% 3. Environment setup and code loading based on given config and mode
%%% NOTE: Tasks 2 and 3 should eventually become the responsibility of app_ctrl
%%% @end

-module(ael_builder).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-define(tag, "v6.3.0").
-define(ver, "6.3.0").

-export([start_link/1]).
-include("$zx_include/zx_logger.hrl").

%%% Interface

-spec start_link(BuildMeta :: term()) -> {ok, pid()}.

start_link(BuildMeta) ->
    BPID = spawn_link(fun() -> init(BuildMeta) end),
    {ok, BPID}.

init(BuildMeta) ->
    ok = ael_gui:show("Checking for presence of local node...\n"),
    {{OS, Version}, OTP, ERTS} = determine_platform(),
    Format =
        "OS: ~p-~s~n"
        "OTP R~s (v~s)~n",
    String = io_lib:format(Format, [OS, Version, OTP, ERTS]),
    ok = ael_gui:show(String),
    Var = ael_con:var(),
    Git = filename:join(Var, "repo"),
    ok = filelib:ensure_dir(Git),
    ok = file:set_cwd(Var),
    check_git(Git, ERTS, BuildMeta).

check_git(Git, ERTS, {?ver, ERTS}) ->
    case file:set_cwd(Git) of
        ok              -> check_build(Git, ERTS);
        {error, enoent} -> clone_repo(Git, ERTS)
    end;
check_git(Git, ERTS, {AEVer, OldERTS}) ->
    Format = "Built: AE ~s with ERTS ~s. Rebuilding as AE ~s with ERTS ~s.~n",
    Message = io_lib:format(Format, [AEVer, OldERTS, ?ver, ERTS]),
    ok = ael_gui:show(Message),
    clone_repo(Git, ERTS);
check_git(Git, ERTS, none) ->
    Format = "No local node built yet. Initiating build of AE ~s with ERTS ~s~n",
    Message = io_lib:format(Format, [?ver, ERTS]),
    ok = ael_gui:show(Message),
    clone_repo(Git, ERTS).

clone_repo(Git, ERTS) ->
    "" = os:cmd("rm -rf repo"),
    ok = ael_gui:ask_install(),
    Command = "git clone https://github.com/aeternity/aeternity.git repo",
    ok = ael_gui:show("Fetching code...\n"),
    ok = external(Command),
    ok = file:set_cwd(Git),
%   ok = external("git checkout " ++ ?tag),
    ok = external("git checkout gh3786-add-app_ctrl-dep"),
    check_build(Git, ERTS).

check_build(Git, ERTS) ->
    BaseDir = filename:join(Git, "_build/prod/rel/aeternity"),
    RELEASES = filename:join(BaseDir, "releases/RELEASES"),
    case file:consult(RELEASES) of
        {ok, [[{release, "aeternity", BuildVer, BuildERTS, Deps, permanent}]]} ->
            Message = io_lib:format("Build successful with BuildVer: ~p BuildERTS: ~p!\n", [BuildVer, BuildERTS]),
            ok = ael_gui:show(Message),
            add_libs(?ver, ERTS, BaseDir, Deps);
%       {ok, [[{release, "aeternity", ?ver, ERTS, Deps, permanent}]]} ->
%           ok = ael_gui:show("Build successful!\n"),
%           add_libs(?ver, ERTS, BaseDir, Deps);
%       {ok, [[{release, "aeternity", ?ver, BadERTS, _, permanent}]]} ->
%           Format = "Erlang version mismatch: ~s VS ~s~n",
%           Message = io_lib:format(Format, [BadERTS, ERTS]),
%           ok = ael_gui:show(Message),
%           ok = ael_gui:show("Running `make clean` and rebilding...\n"),
%           ok = external(Git, "make clean"),
%           check_build(Git, ERTS);
%       {ok, [[{release, "aeternity", OhNoVer, ERTS, _, permanent}]]} ->
%           Format = "Aeternity version mismatch: ~p VS ~p~n",
%           Message = io_lib:format(Format, [OhNoVer, ?ver]),
%           ok = ael_gui:show(Message),
%           ok = ael_gui:show("Running a make clean and rebild...\n"),
%           ok = external(Git, "make clean"),
%           check_build(Git, ERTS);
        {error, enoent} ->
            ok = ael_gui:show("Building Aeternity node...\n"),
            ok = external(Git, "make prod-build"),
            ok = maybe_move_files(BaseDir),
            check_build(Git, ERTS);
        WeirdPoo ->
            ok = ael_gui:show("Received weird poo:"),
            StringyPoo = io_lib:format("~tp", [WeirdPoo]),
            ok = ael_gui:show(StringyPoo),
            {error, mismatch, WeirdPoo}
    end.

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
    external(Command).

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

add_libs(AEVer, ERTS, BaseDir, Deps) ->
    ok = file:set_cwd(ael_con:var()),
    Paths = [filename:join([BaseDir, element(3, Dep), "ebin"]) || Dep <- Deps],
    ok = code:add_pathsa(Paths),
    ok = ael_gui:show("Added paths successfully!\n"),
    PrintableDeps = io_lib:format("~tp", [Deps]),
    ok = ael_gui:show(PrintableDeps),
    load_apps(AEVer, ERTS, Paths).

load_apps(AEVer, ERTS, Paths) ->
    {ok, [Config]} = file:consult(filename:join(zx:get_home(), "priv/sys.config")),
    ok = application:set_env(Config),
    {ok, _} = net_kernel:start(['aeternity@localhost', longnames]),
    Loaded = [element(1, A) || A <- application:loaded_applications()],
    Load =
        fun(Path) ->
            [AppFile] = filelib:wildcard(filename:join(Path, "*.app")),
            {ok, [AppDesc]} = file:consult(AppFile),
            AppName = element(2, AppDesc),
            Message =
                case lists:member(AppName, Loaded) of
                    true ->
                        io_lib:format("App ~p already loaded.~n", [AppName]);
                    false ->
                        ok = application:load(AppDesc),
                        io_lib:format("Loaded app ~p.~n", [AppName])
                end,
            ael_gui:show(Message)
        end,
    ok = lists:foreach(Load, Paths),
    ael_con:build_complete(AEVer, ERTS).

external(Dir, Command) ->
    {ok, PWD} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    ok = external(Command),
    file:set_cwd(PWD).

external(Command) ->
    Port = open_port({spawn, Command}, [stream, binary, exit_status, stderr_to_stdout]),
    {os_pid, OS_PID} = erlang:port_info(Port, os_pid),
    listen(Port, OS_PID).
    
listen(Port, OS_PID) ->
    receive
        {Port, {data, Bin}} ->
            Cleaned = unicode:characters_to_list(clean(Bin)),
            ok = ael_gui:show(Cleaned),
            listen(Port, OS_PID);
        {Port, {exit_status, 0}} ->
            ael_gui:show("Operation complete!\n");
        {Port, Error = {exit_status, N}} ->
            Message = io_lib:format("Operation failed with status code: ~w", [N]),
            ok = ael_gui:show(Message),
            {error, Error};
        Unexpected ->
            ok = ael_gui:show("UNEXPECTED\n"),
            ok = ael_gui:show(Unexpected),
            Out = os:cmd(io_lib:format("kill -9 ~p", [OS_PID])),
            {error, wtf, Unexpected, Out}
    end.

clean(<<>>)                           -> <<>>;
clean(<<"\e[0m", Rest/binary>>)       -> <<(clean(Rest))/binary>>;
clean(<<"\e[0;32m", Rest/binary>>)    -> <<(clean(Rest))/binary>>;
clean(<<"\e[0;35m", Rest/binary>>)    -> <<(clean(Rest))/binary>>;
clean(<<"~", Rest/binary>>)           -> <<"-",(clean(Rest))/binary>>;
clean(<<Byte:1/binary, Rest/binary>>) -> <<Byte/binary, (clean(Rest))/binary>>.


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
%% @private
%% TODO
%% The purpose here is pretty obvious, but for now we just need to fake it for basic
%% dev purposes and revisit when we can refine on other systems (Windows will probably
%% be pretty boring to do, but OSX might be a minor rodeo).

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
