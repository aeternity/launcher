%%% @doc
%%% ÆL Node Monitor
%%%
%%% This process checks the node's state and updates the GUI.
%%% @end

-module(ael_monitor).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(gen_server).

-export([start_poll/1, stop_poll/0]).
% gen_server
-export([start_link/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).
-include("$zx_include/zx_logger.hrl").


%%% Type and Record Definitions

-record(s,
        {interval = none :: none | pos_integer(),
         timer    = none :: none | reference()}).

-type state() :: term().

-spec start_poll(Interval) -> ok
    when Interval :: pos_integer().

start_poll(Interval) ->
    gen_server:cast(?MODULE, {start_poll, Interval}).


-spec stop_poll() -> ok.

stop_poll() ->
    gen_server:cast(?MODULE, stop_poll).



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


%%% Initialization

-spec init(none) -> {ok, state()}.

init(none) ->
    ok = log(info, "Starting"),
    State = #s{},
    {ok, State}.



handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


handle_cast({start_poll, Interval}, State) ->
    NewState = do_start_poll(Interval, State),
    {noreply, NewState};
handle_cast(stop_poll, State) ->
    NewState = do_stop_poll(State),
    {noreply, NewState};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


handle_info(poll, State) ->
    NewState = do_poll(State),
    {noreply, NewState};
handle_info(Unexpected, State) ->
    ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    zx:stop().



%%% Doers

do_start_poll(Interval, State = #s{timer = none}) ->
    T = erlang:send_after(Interval, self(), poll),
    State#s{interval = Interval, timer = T};
do_start_poll(Interval, State = #s{timer = T}) ->
    _ = erlang:cancel_timer(T),
    NewT = erlang:send_after(Interval, self(), poll),
    State#s{interval = Interval, timer = NewT}.


do_stop_poll(State = #s{timer = none}) ->
    State;
do_stop_poll(State = #s{timer = T}) ->
    _ = erlang:cancel_timer(T),
    State#s{interval = none, timer = none}.


do_poll(State = #s{interval = Interval}) ->
    TopBlock = aec_chain:top_block(),
    TopBlockHeight = aec_blocks:height(TopBlock),
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    Difficulty = aec_blocks:difficulty(TopKeyBlock),
    Sync =
        case is_pid(whereis(aec_sync)) of
            true  -> aec_sync:sync_progress();
            false -> {true, 0.0}
        end,
    {PeerCount, PeerConnI, PeerConnO} =
        case is_pid(whereis(aec_peers)) of
            true ->
                {aec_peers:count(peers),
                 aec_peers:count(inbound),
                 aec_peers:count(outbound)};
            false ->
                {0, 0, 0}
        end,
    TXPoolSize = aec_tx_pool:size(),
    Update =
        [{height,     TopBlockHeight},
         {difficulty, Difficulty},
         {sync,       Sync},
         {peers,      {PeerCount, PeerConnI, PeerConnO}},
         {txpool,     TXPoolSize}],
    ok = ael_gui:stat(Update),
    T = erlang:send_after(Interval, self(), poll),
    State#s{timer = T}.
