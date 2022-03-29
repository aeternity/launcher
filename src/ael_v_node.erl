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
-vsn("0.1.2").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(ael_view).
-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([set_button/2, set_manifest/1, stat/1, ask_install/0]).
-export([start_link/1, to_front/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2,
         handle_event/2, handle_sync_event/3]).
-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").


-record(s,
        {frame     = none         :: none | wx:wx_object(),
         conf_pk   = none         :: none | wx:wx_object(),
         node_bn   = none         :: none | wx:wx_object(),
         height    = 0            :: non_neg_integer(),
         sync      = {true, 0.1}  :: {true, float()} | false,
         height_ct = none         :: none | wx:wx_object(),
         peer_ct   = none         :: none | wx:wx_object(),
         diff_ct   = none         :: none | wx:wx_object(),
         txpool_ct = none         :: none | wx:wx_object(),
         graphs    = {#{}, #{}}   :: {IDs :: map(), Graphs :: map()}}).


-type state() :: term().

-define(CONF, 11).
-define(NODE, 12).


%%% Interface functions

-spec to_front(Win) -> ok
    when Win :: module() | pid() | wx:wx_object().

to_front(Win) ->
    wx_object:cast(Win, to_front).


-spec set_button(Text, Enabled) -> ok
    when Text    :: string(),
         Enabled :: boolean().

set_button(Text, Enabled) ->
    cast({set_button, Text, Enabled}).


-spec set_manifest(Entries) -> ok
    when Entries :: [ael:conf_meta()].

set_manifest(Entries) ->
    Names = [Name || #conf_meta{name = Name} <- Entries],
    cast({set_manifest, Names}).


-spec stat([Element]) -> ok
    when Element :: {height,     non_neg_integer()}
                  | {difficulty, non_neg_integer()}
                  | {sync,       {boolean(), float()}}
                  | {peers,      {non_neg_integer(),
                                  non_neg_integer(),
                                  non_neg_integer()}}
                  | {txpool,     non_neg_integer()}.

stat(Elements) ->
    cast({stat, Elements}).


ask_install() ->
    wx_object:call(?MODULE, ask_install, infinity).



cast(Message) ->
    case is_pid(whereis(?MODULE)) of
        true  -> wx_object:cast(?MODULE, Message);
        false -> ok
    end.



%%% Startup Functions

start_link(ConfNames) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, ConfNames, []).


init(ConfNames) ->
    ok = log(info, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Local ÆL Node"),
    FrameSz = wxBoxSizer:new(?wxHORIZONTAL),
    Window = wxScrolledWindow:new(Frame),
    _ = wxBoxSizer:add(FrameSz, Window, zxw:flags(wide)),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    StatSz = wxBoxSizer:new(?wxVERTICAL),

    NodeBn = wxButton:new(Window, ?NODE, [{label, "Run Node"}]),
    ConfPk = wxChoice:new(Window, ?wxID_ANY, [{choices, ConfNames}]),
    ok = wxChoice:setSelection(ConfPk, 0),
    _ = wxSizer:add(ButtSz, NodeBn, zxw:flags(base)),
    _ = wxSizer:add(ButtSz, ConfPk, zxw:flags(wide)),

    StatOpts = [{value, "0"}, {style, ?wxTE_READONLY bor ?wxTE_RIGHT}],
    HeightBx = wxStaticBox:new(Window, ?wxID_ANY, "Height"),
    HeightSz = wxStaticBoxSizer:new(HeightBx, ?wxHORIZONTAL),
    HeightCt = wxTextCtrl:new(HeightBx, ?wxID_ANY, StatOpts),

    {TxPoolSz, TxPoolCt, TxPoolGr, TxPoolGrID} = make_graph(Window, "TX Pool"),
    {DiffSz,   DiffCt,   DiffGr,   DiffGrID}   = make_graph(Window, "Difficulty"),
    {PeerSz,   PeerCt,   PeerGr,   PeerGrID}   = make_graph(Window, "Peers"),

    _ = wxStaticBoxSizer:add(HeightSz, HeightCt, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  HeightSz, [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxBoxSizer:add(StatSz,  TxPoolSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  DiffSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  PeerSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),

    _ = wxSizer:add(MainSz, ButtSz, zxw:flags(base)),
    _ = wxSizer:add(MainSz, StatSz, zxw:flags(base)),
    _ = wxScrolledWindow:setSizerAndFit(Window, MainSz),
    ok = wxScrolledWindow:setScrollRate(Window, 0, 5),
    _ = wxFrame:setSizer(Frame, FrameSz),
    ok = wxFrame:setSize(Frame, {510, 700}),
    _ = wxSizer:layout(FrameSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    {ok, DiffGrI} = ael_graph:show(DiffGr),
    {ok, PeerGrI} = ael_graph:show(PeerGr),
    {ok, TxPoolGrI} = ael_graph:show(TxPoolGr),
    Graphs =
        {#{pool => TxPoolGrID,
           diff => DiffGrID,
           peer => PeerGrID},
         #{TxPoolGrID => TxPoolGrI,
           DiffGrID   => DiffGrI,
           PeerGrID   => PeerGrI}},
    State = #s{frame     = Frame,
               conf_pk   = ConfPk,   node_bn   = NodeBn,
               height_ct = HeightCt, diff_ct   = DiffCt,
               peer_ct   = PeerCt,   txpool_ct = TxPoolCt,
               graphs    = Graphs},
    {Frame, State}.

make_graph(Parent, Label) ->
    StatOpts = [{value, "0"}, {style, ?wxTE_READONLY bor ?wxTE_RIGHT}],
    StaticBox = wxStaticBox:new(Parent, ?wxID_ANY, Label),
    Sizer = wxStaticBoxSizer:new(StaticBox, ?wxVERTICAL),
    TextCtrl = wxTextCtrl:new(StaticBox, ?wxID_ANY, StatOpts),
    _ = wxStaticBoxSizer:add(Sizer, TextCtrl, zxw:flags(base)),
    GridSizer = wxBoxSizer:new(?wxVERTICAL),
    Graph = ael_graph:new(StaticBox, GridSizer, "X", "Y"),
    true = wxSizer:setItemMinSize(GridSizer, 0, {150, 150}),
    ID = ael_graph:get_wx_id(Graph),
    _ = wxStaticBoxSizer:add(Sizer, GridSizer, zxw:flags(wide)),
    {Sizer, TextCtrl, Graph, ID}.


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
handle_cast({set_button, Text, Enabled}, State) ->
    ok = do_set_button(Text, Enabled, State),
    {noreply, State};
handle_cast({set_manifest, Names}, State) ->
    ok = do_set_manifest(Names, State),
    {noreply, State};
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
    Message = build_notice(ael_con:platform()),
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

build_notice(_) ->
    "You want to run a node? EXCELLENT!\n\n"
    "To run a node one must first be built.\n"
    "Building a node requires a number of packages be available on the host "
    "system in order for the node to be able to run. "
    "To ensure that the necessary packages are installed please run the "
    "following command as root or using the `sudo` command:\n\n"
    "apt install gcc curl g++ dpkg-dev build-essential\\\n"
    "    automake autoconf libncurses5-dev libssl-dev\\\n"
    "    flex xsltproc wget vim git cmake libsodium-dev\\\n"
    "    libgmp-dev".


do_stat({height, Now},
        State = #s{sync = {true, Complete}, height_ct = HeightCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, sync_format(Now, Complete)),
    State#s{height = Now};
do_stat({height, Now}, State = #s{sync = false, height_ct = HeightCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, integer_to_list(Now)),
    State;
do_stat({difficulty, Rating},
        State = #s{diff_ct = DiffCt, graphs = Graphs}) ->
    NewGraphs = update_graph(diff, Graphs, Rating),
    ok = wxTextCtrl:changeValue(DiffCt, integer_to_list(Rating)),
    State#s{graphs = NewGraphs};
do_stat({sync, Sync = {true, Complete}},
        State = #s{height = Now, height_ct = HeightCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, sync_format(Now, Complete)),
    State#s{sync = Sync};
do_stat({sync, {false, _}},
        State = #s{height = Now, height_ct = HeightCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, integer_to_list(Now)),
    State#s{sync = false};
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

sync_format(Now, Complete) ->
    Top = calc_top(Now, Complete),
    Format =
        [integer_to_list(Now), "/", integer_to_list(Top),
         " (Syncing: ", io_lib:format("~.2f%", [Complete]), ")"],
    unicode:characters_to_list(Format).

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


do_set_button(Text, Enabled, #s{node_bn = NodeBn}) ->
    _ = wxButton:setLabel(NodeBn, Text),
    _ = wxButton:enable(NodeBn, [{enable, Enabled}]),
    ok.


do_set_manifest(Names, #s{conf_pk = ConfPk}) ->
    tell(info, "ConfPk: ~p", [ConfPk]),
    Selected = wxChoice:getStringSelection(ConfPk),
    tell(info, "Selected: ~p", [Selected]),
    LastIndex = length(Names) - 1,
    ok = wxChoice:clear(ConfPk),
    LastIndex = wxChoice:insertStrings(ConfPk, Names, 0),
    case wxChoice:setStringSelection(ConfPk, Selected) of
        true ->
            ok;
        false ->
            ok = wxChoice:setSelection(ConfPk, 0),
            log(info, "Config update removed selected item ~ts", [Selected])
    end.


run_node(State = #s{conf_pk = ConfPk}) ->
    Selected = wxChoice:getStringSelection(ConfPk),
    case ael_con:run_node(Selected) of
        ok      -> State;
        running -> State
    end.


close(State = #s{frame = Frame}) ->
    ok = ael_con:stop_ae(),
    ok = wxFrame:destroy(Frame),
    State.
