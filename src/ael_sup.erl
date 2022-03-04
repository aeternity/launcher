%%% @doc
%%% ÆL Top-level Supervisor
%%%
%%% The very top level supervisor in the system. It only has one service branch: the
%%% "con" (program controller). The con is the 
%%% only be one part of a larger system. Were this a game system, for example, the
%%% item data management service would be a peer, as would a login credential provision
%%% service, game world event handling, and so on.
%%%
%%% See: http://erlang.org/doc/design_principles/applications.html
%%% See: http://zxq9.com/archives/1311
%%% @end

-module(ael_sup).
-vsn("0.1.1").
-behaviour(supervisor).
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.
%% @private
%% This supervisor's own start function.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @private
%% The OTP init/1 function.

init([]) ->
    RestartStrategy = {one_for_one, 0, 60},
    Monitor   = {ael_monitor,
                 {ael_monitor, start_link, []},
                 permanent,
                 brutal_kill,
                 worker,
                 [ael_monitor]},
    Clients   = {ael_con,
                 {ael_con, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [ael_con]},
    Children  = [Monitor, Clients],
    {ok, {RestartStrategy, Children}}.
