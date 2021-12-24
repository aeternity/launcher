%%% @doc
%%% ÆL Node View
%%%
%%% This process is responsible for displaying a graphical representation of what is
%%% happening within the node -- the sync, connection, memory, warnings, activity rate
%%% and similar statistics. Exploring the chain, exploring the contents of blocks,
%%% exploring the network, configuring the node, etc. are separate activities.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(ael_v_node).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(ael_view).
-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([stat/1, ask_install/0]).
-export([start_link/1, to_front/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2,
         handle_event/2, handle_sync_event/3]).
-include("$zx_include/zx_logger.hrl").


-record(s,
        {frame       = none         :: none | wx:wx_object(),
         conf_bn     = none         :: none | wx:wx_object(),
         node_bn     = none         :: none | wx:wx_object(),
         height      = 0            :: non_neg_integer(),
         sync        = {true, 0.1}  :: {true, float()} | false,
         height_ct   = none         :: none | wx:wx_object(),
         sync_ct     = none         :: none | wx:wx_object(),
         peer_ct     = none         :: none | wx:wx_object(),
         diff_ct     = none         :: none | wx:wx_object(),
         txpool_ct   = none         :: none | wx:wx_object(),
         graphs      = {#{}, #{}}   :: {IDs :: map(), Graphs :: map()}}).


-type state() :: term().

-define(CONF, 11).
-define(NODE, 12).


%%% Interface functions


to_front(Win) ->
    wx_object:cast(Win, to_front).


-spec stat([Element]) -> ok
    when Element :: {height,     non_neg_integer()}
                  | {difficulty, non_neg_integer()}
                  | {sync,       {boolean(), float()}}
                  | {peers,      {non_neg_integer(),
                                  non_neg_integer(),
                                  non_neg_integer()}}
                  | {txpool,     non_neg_integer()}.

stat(Elements) ->
    wx_object:cast(?MODULE, {stat, Elements}).


ask_install() ->
    wx_object:call(?MODULE, ask_install, infinity).



%%% Startup Functions

start_link(none) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, none, []).


init(none) ->
    ok = log(info, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Local ÆL Node"),
    FrameSz = wxBoxSizer:new(?wxHORIZONTAL),
    Window = wxScrolledWindow:new(Frame),
    _ = wxBoxSizer:add(FrameSz, Window, zxw:flags(wide)),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    StatSz = wxBoxSizer:new(?wxVERTICAL),

    ConfBn = wxButton:new(Window, ?CONF, [{label, "Configuration"}]),
    _ = wxButton:disable(ConfBn),
    NodeBn = wxButton:new(Window, ?NODE, [{label, "Run Node"}]),
    _ = wxSizer:add(ButtSz, ConfBn, zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, NodeBn, zxw:flags(wide)),

    StatOpts = [{value, "0"}, {style, ?wxTE_READONLY bor ?wxTE_RIGHT}],
    HeightBx = wxStaticBox:new(Window, ?wxID_ANY, "Height"),
    HeightSz = wxStaticBoxSizer:new(HeightBx, ?wxHORIZONTAL),
    HeightCt = wxTextCtrl:new(HeightBx, ?wxID_ANY, StatOpts),

    TxPoolBx = wxStaticBox:new(Window, ?wxID_ANY, "TX Pool"),
    TxPoolSz = wxStaticBoxSizer:new(TxPoolBx, ?wxVERTICAL),
    TxPoolCt = wxTextCtrl:new(TxPoolBx, ?wxID_ANY, StatOpts),
    _ = wxStaticBoxSizer:add(TxPoolSz, TxPoolCt, zxw:flags(base)),
    TxPoolGrSz = wxBoxSizer:new(?wxVERTICAL),
    TxPoolGr = ael_graph:new(TxPoolBx, TxPoolGrSz, "X", "Y"),
    true = wxSizer:setItemMinSize(TxPoolGrSz, 0, {150, 150}),
    TxPoolGrID = ael_graph:get_wx_id(TxPoolGr),
    _ = wxStaticBoxSizer:add(TxPoolSz, TxPoolGrSz, zxw:flags(wide)),

    SyncBx = wxStaticBox:new(Window, ?wxID_ANY, "Sync"),
    SyncSz = wxStaticBoxSizer:new(SyncBx, ?wxVERTICAL),
    SyncCt = wxTextCtrl:new(SyncBx, ?wxID_ANY, StatOpts),
    _ = wxStaticBoxSizer:add(SyncSz, SyncCt, zxw:flags(base)),
    SyncGrSz = wxBoxSizer:new(?wxVERTICAL),
    SyncGr = ael_graph:new(SyncBx, SyncGrSz, "X", "Y"),
    true = wxSizer:setItemMinSize(SyncGrSz, 0, {150, 150}),
    SyncGrID = ael_graph:get_wx_id(SyncGr),
    _ = wxStaticBoxSizer:add(SyncSz, SyncGrSz, zxw:flags(wide)),

    DiffBx = wxStaticBox:new(Window, ?wxID_ANY, "Difficulty"),
    DiffSz = wxStaticBoxSizer:new(DiffBx, ?wxVERTICAL),
    DiffCt = wxTextCtrl:new(DiffBx, ?wxID_ANY, StatOpts),
    _ = wxStaticBoxSizer:add(DiffSz, DiffCt, zxw:flags(base)),
    DiffGrSz = wxBoxSizer:new(?wxVERTICAL),
    DiffGr = ael_graph:new(DiffBx, DiffGrSz, "X", "Y"),
    true = wxSizer:setItemMinSize(DiffGrSz, 0, {150, 150}),
    DiffGrID = ael_graph:get_wx_id(DiffGr),
    _ = wxStaticBoxSizer:add(DiffSz, DiffGrSz, zxw:flags(wide)),

    PeerBx = wxStaticBox:new(Window, ?wxID_ANY, "Peer"),
    PeerSz = wxStaticBoxSizer:new(PeerBx, ?wxVERTICAL),
    PeerCt = wxTextCtrl:new(PeerBx, ?wxID_ANY, StatOpts),
    _ = wxStaticBoxSizer:add(PeerSz, PeerCt, zxw:flags(base)),
    PeerGrSz = wxBoxSizer:new(?wxVERTICAL),
    PeerGr = ael_graph:new(PeerBx, PeerGrSz, "X", "Y"),
    true = wxSizer:setItemMinSize(PeerGrSz, 0, {150, 150}),
    PeerGrID = ael_graph:get_wx_id(PeerGr),
    _ = wxStaticBoxSizer:add(PeerSz, PeerGrSz, zxw:flags(wide)),

    _ = wxStaticBoxSizer:add(HeightSz, HeightCt, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  HeightSz, [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxBoxSizer:add(StatSz,  TxPoolSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  DiffSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  PeerSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  SyncSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),

    _ = wxSizer:add(MainSz, ButtSz, zxw:flags(base)),
    _ = wxSizer:add(MainSz, StatSz, zxw:flags(base)),
    _ = wxScrolledWindow:setSizerAndFit(Window, MainSz),
    ok = wxScrolledWindow:setScrollRate(Window, 0, 5),
    _ = wxFrame:setSizer(Frame, FrameSz),
    ok = wxFrame:setSize(Frame, {500, 900}),
    _ = wxSizer:layout(FrameSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    {ok, DiffGrI} = ael_graph:show(DiffGr),
    {ok, PeerGrI} = ael_graph:show(PeerGr),
    {ok, SyncGrI} = ael_graph:show(SyncGr),
    {ok, TxPoolGrI} = ael_graph:show(TxPoolGr),
    Graphs =
        {#{pool => TxPoolGrID,
           diff => DiffGrID,
           peer => PeerGrID,
           sync => SyncGrID},
         #{TxPoolGrID => TxPoolGrI,
           DiffGrID   => DiffGrI,
           PeerGrID   => PeerGrI,
           SyncGrID   => SyncGrI}},
    State = #s{frame     = Frame,
               conf_bn   = ConfBn,   node_bn = NodeBn,
               height_ct = HeightCt, sync_ct = SyncCt,
               diff_ct   = DiffCt,   peer_ct = PeerCt, txpool_ct = TxPoolCt,
               graphs    = Graphs},
    {Frame, State}.


-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().

handle_call(ask_install, _, State) ->
    Response = do_ask_install(State),
    {reply, Response, State};
handle_call(Unexpected, From, State) ->
    ok = tell(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


-spec handle_cast(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_cast/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2

handle_cast({stat, Elements}, State) ->
    NewState = lists:foldl(fun do_stat/2, State, Elements),
    {noreply, NewState};
handle_cast(to_front, State = #s{frame = Frame}) ->
    ok = wxWindow:raise(Frame),
    {noreply, State};
handle_cast(Unexpected, State) ->
    ok = tell(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_info(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The gen_server:handle_info/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2

handle_info(Unexpected, State) ->
    ok = tell(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_event(Event, State) -> {noreply, NewState}
    when Event    :: term(),
         State    :: state(),
         NewState :: state().
%% @private
%% The wx_object:handle_event/2 callback.
%% See: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2

handle_event(#wx{event = #wxPaint{}}, State = #s{graphs = {_, Graphs}}) ->
    ok = lists:foreach(fun ael_graph:render/1, maps:values(Graphs)),
    {noreply, State};
handle_event(Event = #wx{event = #wxMouse{}}, State) ->
    NewState = handle_mouse(Event, State),
    {noreply, NewState};
handle_event(#wx{id = ID, event = #wxCommand{type = command_button_clicked}}, State) ->
    NewState =
        case ID of
            ?CONF       -> conf(State);
            ?NODE       -> run_node(State);
            ?wxID_EXIT  -> close(State)
        end,
    {noreply, NewState};
handle_event(#wx{event = #wxClose{}}, State) ->
    NewState = close(State),
    {noreply, NewState};
handle_event(Event, State) ->
    ok = tell(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.

handle_mouse(#wx{id = ID, event = Event}, State = #s{graphs = {GraphIDs, Graphs}}) ->
    case maps:find(ID, Graphs) of
        {ok, Graph} ->
            NewGraphs = maps:put(ID, do_graph_update(Graph, Event), Graphs),
            State#s{graphs = {GraphIDs, NewGraphs}};
        error ->
            State
    end.

do_graph_update(Graph, #wxMouse{type = motion, leftDown = true, rightDown = false,
                                x = X, y = Y}) ->
    {ok, NewGraph} = ael_graph:traverse(X, Y, Graph),
    NewGraph;
do_graph_update(Graph, #wxMouse{type = left_up}) ->
    ael_graph:clear_t_pin(Graph);
do_graph_update(Graph, #wxMouse{type = motion, leftDown = false}) ->
    ael_graph:clear_t_pin(Graph);
do_graph_update(Graph, #wxMouse{type = motion, leftDown = true, rightDown = true,
                                x = X, y = Y}) ->
    {ok, NewGraph} = ael_graph:rotate(X, Y, Graph),
    NewGraph;
do_graph_update(Graph, #wxMouse{type = right_down, x = X, y = Y}) ->
    {ok, NewGraph} = ael_graph:rotate(X, Y, ael_graph:clear_t_pin(Graph)),
    NewGraph;
do_graph_update(Graph, #wxMouse{type = right_up}) ->
    ael_graph:clear_r_pin(Graph);
do_graph_update(Graph, #wxMouse{type = motion, rightDown = false}) ->
    ael_graph:clear_r_pin(Graph);
do_graph_update(Graph, #wxMouse{}) ->
    Graph.


-spec handle_sync_event(Event, Ref, State) -> ok | noreply | Error
    when Event :: #wx{},
         Ref   :: term(),
         State :: term(),
         Error :: term().

handle_sync_event(Event, Ref, State) ->
    Message = "Unexpected sync event ~tp (ref: ~tp) State: ~tp~n",
    tell(info, Message, [Event, Ref, State]).


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


%%% Doers

do_ask_install(#s{frame = Frame}) ->
    Message =
        "You want to run a node? EXCELLENT!\n\n"
        "To run a node one must first be built.\n"
        "Building a node requires a number of packages be available on the host "
        "system in order for the node to be able to run. "
        "To ensure that the necessary packages are installed please run the "
        "following command as root or using the `sudo` command:\n\n"
        "apt install gcc curl g++ dpkg-dev build-essential\\\n"
        "    automake autoconf libncurses5-dev libssl-dev\\\n"
        "    flex xsltproc wget vim git cmake libsodium-dev\\\n"
        "    libgmp-dev",
    ask_yes_no(Frame, Message).


ask_yes_no(Frame, Message) ->
    Text = io_lib:format("~ts", [Message]),
    Style = {style, ?wxOK bor ?wxCANCEL},
    Modal = wxMessageDialog:new(Frame, Text, [Style]),
    Response =
        case wxMessageDialog:showModal(Modal) of
            ?wxID_OK     -> ok;
            ?wxID_CANCEL -> cancel
        end,
    ok = wxMessageDialog:destroy(Modal),
    Response.


do_stat({height, Now},
        State = #s{sync = {true, Complete}, height_ct = HeightCt, height = OldTop,
                   graphs = Graphs}) ->
    Top = calc_top(Now, Complete),
    Diff = OldTop - Top,
    NewGraphs = update_graph(sync, Graphs, Diff),
    Text = unicode:characters_to_list([integer_to_list(Now), "/", integer_to_list(Top)]),
    ok = wxTextCtrl:changeValue(HeightCt, Text),
    State#s{height = Now, graphs = NewGraphs};
do_stat({height, Now}, State = #s{sync = false, height_ct = HeightCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, integer_to_list(Now)),
    State;
do_stat({difficulty, Rating},
        State = #s{diff_ct = DiffCt, graphs = Graphs}) ->
    NewGraphs = update_graph(diff, Graphs, Rating),
    ok = wxTextCtrl:changeValue(DiffCt, integer_to_list(Rating)),
    State#s{graphs = NewGraphs};
do_stat({sync, Sync = {true, Complete}},
        State = #s{height = Now, height_ct = HeightCt, sync_ct = SyncCt,
                   graphs = Graphs}) ->
    Top = calc_top(Now, Complete),
    NewGraphs = update_graph(sync, Graphs, 0),
    Text = unicode:characters_to_list([integer_to_list(Now), "/", integer_to_list(Top)]),
    ok = wxTextCtrl:changeValue(HeightCt, Text),
    SyncText = io_lib:format("~.2f%", [Complete]),
    ok = wxTextCtrl:changeValue(SyncCt, SyncText),
    State#s{sync = Sync, graphs = NewGraphs};
do_stat({sync, {false, Complete}},
        State = #s{height = Now, height_ct = HeightCt, sync_ct = SyncCt,
                   graphs = Graphs}) ->
    NewGraphs = update_graph(sync, Graphs, 0),
    ok = wxTextCtrl:changeValue(HeightCt, integer_to_list(Now)),
    SyncText = io_lib:format("~.2f%", [Complete]),
    ok = wxTextCtrl:changeValue(SyncCt, SyncText),
    State#s{sync = false, graphs = NewGraphs};
do_stat({peers, {PeerCount, PeerConnI, PeerConnO}},
        State = #s{peer_ct = PeerCt, graphs = Graphs}) ->
    Figures =
        ["Total: ", integer_to_list(PeerCount),
         " (In: ", integer_to_list(PeerConnI), " / ",
         "Out : ",  integer_to_list(PeerConnO), ") "],
    NewGraphs = update_graph(peer, Graphs, PeerCount),
    Text = unicode:characters_to_list(Figures),
    ok = wxTextCtrl:changeValue(PeerCt, Text),
    State#s{graphs = NewGraphs};
do_stat({txpool, Count},
        State = #s{txpool_ct = TxPoolCt, graphs = Graphs}) ->
    NewGraphs = update_graph(pool, Graphs, Count),
    ok = wxTextCtrl:changeValue(TxPoolCt, integer_to_list(Count)),
    State#s{graphs = NewGraphs}.

calc_top(Now, Complete) when Complete > 0 ->
    trunc((Now * 100) / Complete);
calc_top(_, _) ->
    0.

update_graph(Name, G = {GraphIDs, Graphs}, Diff) ->
    case maps:find(Name, GraphIDs) of
        {ok, ID} ->
            TS = erlang:timestamp(),
            Graph = maps:get(ID, Graphs),
            {ok, NewGraph} = ael_graph:update(Graph, {TS, Diff}),
            NewGraphs = maps:put(ID, NewGraph, Graphs),
            {GraphIDs, NewGraphs};
        error ->
            G
    end.


conf(State) ->
    ok = ael_con:show_conf(),
    State.


run_node(State) ->
    ok = ael_con:run_node(),
    State.


close(State = #s{frame = Frame}) ->
    ok = wxFrame:destroy(Frame),
    State.
