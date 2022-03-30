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
-vsn("0.1.3").
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
    ERTS = erlang:system_info(version),
    ok = ael_gui:show("Checking for presence of local node...\n"),
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
    case ael_v_node:ask_install() of
        ok ->
            Command = "git clone https://github.com/aeternity/aeternity.git aeternity",
            ok = ael_gui:show("Fetching code...\n"),
            ok = ael_os:cmd(Command),
            ok = file:set_cwd(Git),
            ok = ael_os:cmd("git checkout " ++ ?tag),
            check_build(Git, ERTS);
        cancel ->
            ael_con:build_cancelled()
    end.

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
