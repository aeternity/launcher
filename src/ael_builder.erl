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
-vsn("0.2.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-define(sophia_ver, {6, 1, 0}).
-define(aecore_ver, {6, 7, 0}).
-define(sophia_url,
        "https://github.com/aeternity/aesophia/archive/refs/tags/v6.1.0.tar.gz").
-define(ae_core_url,
        "https://github.com/aeternity/aeternity/archive/refs/tags/v6.7.0.tar.gz").

-export([start_link/1, aecore_ok/1, sophia_ok/1]).
-include("$zx_include/zx_logger.hrl").

%%% Interface

-spec start_link(Dir :: file:filename()) -> {ok, pid()}.

start_link(Dir) ->
    BPID = spawn_link(fun() -> build(Dir) end),
    {ok, BPID}.

build(Dir) ->
    tell(info, "Dir: ~p", [Dir]),
    ok = filelib:ensure_dir(Dir),
    ok = file:set_cwd(Dir),
    ok = ael_gui:show("Building AE Core and Sophia in " ++ Dir ++ "...\n"),
    AECoreFile = "aeternity-" ++ filename:basename(?ae_core_url),
    SophiaFile = "sophia-" ++ filename:basename(?sophia_url),
    "" = os:cmd(wget_cmd(AECoreFile, ?ae_core_url)),
    "" = os:cmd(wget_cmd(SophiaFile, ?sophia_url)),
    AECoreDir = unpack(AECoreFile),
    SophiaDir = unpack(SophiaFile),
    {ok, _} = file:copy(filename:join(AECoreDir, "rebar3"),
                        filename:join(SophiaDir, "rebar3")),
    ok = ael_os:cmd(AECoreDir, "make prod-build"),
    ok = ael_os:cmd(SophiaDir, "./rebar3 release"),
    AECoreBuild = filename:join(AECoreDir, "_build/prod/rel/aeternity/"),
    SophiaBuild = filename:join(SophiaDir, "_build/default/rel/aesophia"),
    AECore = {?aecore_ver, AECoreBuild},
    Sophia = {?sophia_ver, SophiaBuild},
    ERTS = erlang:system_info(version),
    ael_con:build_complete(AECore, Sophia, ERTS).

wget_cmd(Out, URL) ->
    "wget --quiet --no-clobber --output-document " ++ Out ++ " " ++ URL.

rm_cmd(Target) ->
    "rm -rf " ++ Target.

unpack(File) ->
    {ok, ["pax_global_header", DirName | _]} = erl_tar:table(File, [compressed]),
    "" = os:cmd(rm_cmd(DirName)),
    ok = erl_tar:extract(File, [compressed]),
    true = filelib:is_dir(DirName),
    {ok, CWD} = file:get_cwd(),
    filename:join(CWD, DirName).


aecore_ok(?aecore_ver) -> true;
aecore_ok({_, _, _})   -> obsolete;
aecore_ok(none)        -> false.

sophia_ok(?sophia_ver) -> true;
sophia_ok({_, _, _})   -> obsolete;
sophia_ok(none)        -> false.
