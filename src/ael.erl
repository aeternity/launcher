%%% @doc
%%% Aeternity Launcher (ÆL)
%%% @end

-module(ael).
-vsn("0.1.3").
-behavior(application).
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-export([start/2, stop/1]).
-include("ael_conf.hrl").


%%% Type and Record Definitions

-export_type([conf_meta/0, platform/0, build_meta/0]).

-type conf_meta()  :: #conf_meta{}.
-type platform()   :: {System :: {OS :: atom(), Version :: string() | unknown},
                       OTP    :: string(),
                       ERTS   :: string()}.
-type build_meta() :: {AE_Version :: string(), ERTS_Version :: string()}.


%%% Interface

-spec start(normal, Args :: term()) -> {ok, pid()}.
%% @private
%% Called by OTP to kick things off. This is for the use of the "application" part of
%% OTP, not to be called by user code.
%%
%% NOTE:
%%   The commented out second argument would come from ebin/aelternity.app's 'mod'
%%   section, which is difficult to define dynamically so is not used by default
%%   here (if you need this, you already know how to change it).
%%
%%   Optional runtime arguments passed in at start time can be obtained by calling
%%   zx_daemon:argv/0 anywhere in the body of the program.
%%
%% See: http://erlang.org/doc/apps/kernel/application.html

start(normal, _Args) ->
    ok = application:ensure_started(inets),
    ael_sup:start_link().


-spec stop(term()) -> ok.
%% @private
%% Similar to start/2 above, this is to be called by the "application" part of OTP,
%% not client code. Causes a (hopefully graceful) shutdown of the application.

stop(_State) ->
    ok.
