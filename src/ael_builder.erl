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
%%% @end

-module(ael_builder).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-define(tag, "v6.4.0").
-define(ver, "6.4.0").

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
    Git = filename:join(Var, "aeternity"),
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
    "" = os:cmd("rm -rf aeternity"),
    ok = ael_gui:ask_install(),
    Command = "git clone https://github.com/aeternity/aeternity.git aeternity",
    ok = ael_gui:show("Fetching code...\n"),
    ok = ael_os:cmd(Command),
    ok = file:set_cwd(Git),
    ok = ael_os:cmd("git checkout " ++ ?tag),
    check_build(Git, ERTS).

check_build(Git, ERTS) ->
    BaseDir = filename:join(Git, "_build/prod/rel/aeternity"),
    RELEASES = filename:join(BaseDir, "releases/RELEASES"),
    case file:consult(RELEASES) of
        {ok, [[{release, "aeternity", ?ver, ERTS, Deps, permanent}]]} ->
            ok = ael_gui:show("Build successful!\n"),
            great_success(?ver, ERTS, BaseDir, Deps);
        {ok, [[{release, "aeternity", ?ver, BadERTS, _, permanent}]]} ->
            Format = "Erlang version mismatch: ~s VS ~s~n",
            Message = io_lib:format(Format, [BadERTS, ERTS]),
            ok = ael_gui:show(Message),
            ok = ael_gui:show("Running `make clean` and rebilding...\n"),
            ok = ael_os:cmd(Git, "make clean"),
            check_build(Git, ERTS);
        {ok, [[{release, "aeternity", OhNoVer, ERTS, _, permanent}]]} ->
            Format = "Aeternity version mismatch: ~p VS ~p~n",
            Message = io_lib:format(Format, [OhNoVer, ?ver]),
            ok = ael_gui:show(Message),
            ok = ael_gui:show("Running a make clean and rebild...\n"),
            ok = ael_os:cmd(Git, "make clean"),
            check_build(Git, ERTS);
        {error, enoent} ->
            ok = ael_gui:show("Building Aeternity node...\n"),
            ok = ael_os:cmd(Git, "make prod-build"),
            check_build(Git, ERTS);
        WeirdPoo ->
            ok = ael_gui:show("Received weird poo:"),
            StringyPoo = io_lib:format("~tp", [WeirdPoo]),
            ok = ael_gui:show(StringyPoo),
            {error, mismatch, WeirdPoo}
    end.

great_success(AEVer, ERTS, BaseDir, Deps) ->
    ael_con:build_complete(AEVer, ERTS, BaseDir, Deps).


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
