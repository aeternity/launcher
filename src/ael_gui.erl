%%% @doc
%%% ÆL GUI
%%%
%%% This process is responsible for creating the main GUI frame displayed to the user.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(ael_gui).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([show/1, stat/1, ask_install/0]).
-export([start_link/1]).
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
         diff_gr     = none         :: none | wx:wx_object(),
         peer_gr     = none         :: none | wx:wx_object(),
         pool_gr     = none         :: none | wx:wx_object(),
         text_ct     = none         :: none | wx:wx_object()}).


-type state() :: term().

-define(CONF, 11).
-define(NODE, 12).


%%% Interface functions

-spec show(term()) -> ok.

show(Terms) ->
    wx_object:cast(?MODULE, {show, Terms}).


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

start_link(BuildMeta) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, BuildMeta, []).


init(BuildMeta) ->
    ok = log(info, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "ÆL"),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    StatSz = wxBoxSizer:new(?wxVERTICAL),

    ConfBn = wxButton:new(Frame, ?CONF, [{label, "Configuration"}]),
    _ = wxButton:disable(ConfBn),
    NodeBn = wxButton:new(Frame, ?NODE, [{label, "Run Node"}]),
    _ = wxSizer:add(ButtSz, ConfBn, zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, NodeBn, zxw:flags(wide)),

    BlockSz = wxBoxSizer:new(?wxHORIZONTAL),
    WorkSz = wxBoxSizer:new(?wxHORIZONTAL),
    StatOpts = [{value, "0"}, {style, ?wxTE_READONLY bor ?wxTE_RIGHT}],
    HeightBx  = wxStaticBox:new(Frame, ?wxID_ANY, "Height"),
    HeightSz  = wxStaticBoxSizer:new(HeightBx, ?wxHORIZONTAL),
    HeightCt  = wxTextCtrl:new(HeightBx, ?wxID_ANY, StatOpts),
    SyncBx  = wxStaticBox:new(Frame, ?wxID_ANY, "Sync"),
    SyncSz  = wxStaticBoxSizer:new(SyncBx, ?wxHORIZONTAL),
    SyncCt  = wxTextCtrl:new(SyncBx, ?wxID_ANY, StatOpts),
    DiffBx  = wxStaticBox:new(Frame, ?wxID_ANY, "Difficulty"),
    DiffSz  = wxStaticBoxSizer:new(DiffBx, ?wxHORIZONTAL),
    DiffCt  = wxTextCtrl:new(DiffBx, ?wxID_ANY, StatOpts),
    PeerBx  = wxStaticBox:new(Frame, ?wxID_ANY, "Peers"),
    PeerSz  = wxStaticBoxSizer:new(PeerBx, ?wxHORIZONTAL),
    PeerCt  = wxTextCtrl:new(PeerBx, ?wxID_ANY, StatOpts),
    TxPoolBx = wxStaticBox:new(Frame, ?wxID_ANY, "TX Pool"),
    TxPoolSz = wxStaticBoxSizer:new(TxPoolBx, ?wxHORIZONTAL),
    TxPoolCt = wxTextCtrl:new(TxPoolBx, ?wxID_ANY, StatOpts),
    _ = wxStaticBoxSizer:add(HeightSz, HeightCt, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxStaticBoxSizer:add(SyncSz,   SyncCt,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxStaticBoxSizer:add(DiffSz,   DiffCt,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxStaticBoxSizer:add(PeerSz,   PeerCt,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxStaticBoxSizer:add(TxPoolSz, TxPoolCt, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(BlockSz, HeightSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(BlockSz, SyncSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(WorkSz,  DiffSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(WorkSz,  PeerSz,   [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(WorkSz,  TxPoolSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxBoxSizer:add(StatSz,  BlockSz,  [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxBoxSizer:add(StatSz,  WorkSz,   [{flag, ?wxEXPAND}, {proportion, 0}]),

    
    DiffGr = wxPanel:new(Frame),
    PeerGr = wxPanel:new(Frame),
    PoolGr = wxPanel:new(Frame),
    _ = wxWindow:setBackgroundStyle(DiffGr, ?wxBG_STYLE_PAINT),
    F = wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT),
    Size = wxFont:getPointSize(F),
    Fam = wxFont:getFamily(F),
    NFont = wxFont:new(Size - 1, Fam, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    SFont = wxFont:new(Size - 2, Fam, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    {R, G, B, _} = wxWindow:getBackgroundColour(DiffGr),
    FontColor =
        case ((R + G + B) div 3) > 100 of
            false -> {0, 0, 0};
            true  -> wxSystemSettings:getColour(?wxSYS_COLOUR_BTNTEXT)
        end,
    Pen = wxPen:new(wxSystemSettings:getColour(?wxSYS_COLOUR_HIGHLIGHT), [{width, 2}]),
    ok = wxPanel:connect(DiffGr, paint, [callback]),

    _ = wxBoxSizer:add(StatSz, DiffGr, [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxBoxSizer:add(StatSz, PeerGr, [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxBoxSizer:add(StatSz, PoolGr, [{flag, ?wxEXPAND}, {proportion, 0}]),
    

    TextStyle = [{style, ?wxTE_MULTILINE}],
    TextCt = wxTextCtrl:new(Frame, ?wxID_ANY, TextStyle),

    _ = wxSizer:add(MainSz, ButtSz, zxw:flags(base)),
    _ = wxSizer:add(MainSz, StatSz, zxw:flags(base)),
    _ = wxSizer:add(MainSz, TextCt, zxw:flags(wide)),
    _ = wxFrame:setSizer(Frame, MainSz),
    ok = wxFrame:setSize(Frame, {700, 600}),
    _ = wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    BuildString =
        case BuildMeta of
            {AEVer, ERTSVer} ->
                Format = "Current build: AE ~s built with ERTS ~s.~n",
                io_lib:format(Format, [AEVer, ERTSVer]);
            none ->
                ok = wxButton:setLabel(NodeBn, "Build Node"),
                "No AE node is currently built.\n"
        end,
    ok = wxTextCtrl:appendText(TextCt, BuildString),
    State = #s{frame     = Frame,
               conf_bn   = ConfBn,   node_bn = NodeBn,
               height_ct = HeightCt, sync_ct = SyncCt,
               diff_ct   = DiffCt,   peer_ct = PeerCt, txpool_ct = TxPoolCt,
               diff_gr   = DiffGr,   peer_gr = PeerGr, pool_gr   = PoolGr,
               text_ct   = TextCt},
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

handle_cast({show, Terms}, State) ->
    NewState = do_show(Terms, State),
    {noreply, NewState};
handle_cast({stat, Elements}, State) ->
    NewState = lists:foldl(fun do_stat/2, State, Elements),
    {noreply, NewState};
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


-spec handle_sync_event(Event, Ref, State) -> ok | noreply | Error
    when Event :: #wx{},
         Ref   :: term(),
         State :: term(),
         Error :: term().

handle_sync_event(Event, Ref, State) ->
    ok = tell(info, "Unexpected sync event ~tp (ref: ~tp) State: ~tp~n", [Event, Ref, State]),
    ok.


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



do_show(Terms, State = #s{text_ct = TextC}) ->
    ok = log(info, Terms),
    String =
        case io_lib:deep_char_list(Terms) of
            true  -> Terms;
            false -> io_lib:format("~tw~n", [Terms])
        end,
    ok = wxTextCtrl:appendText(TextC, unicode:characters_to_list(String)),
    State.


do_stat({height, Now}, State = #s{sync = {true, Complete}, height_ct = HeightCt}) ->
    Top = trunc((Now * 100) / Complete),
    Text = unicode:characters_to_list([integer_to_list(Now), "/", integer_to_list(Top)]),
    ok = wxTextCtrl:changeValue(HeightCt, Text),
    State#s{height = Now};
do_stat({height, Now}, State = #s{sync = false, height_ct = HeightCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, integer_to_list(Now)),
    State;
do_stat({difficulty, Rating}, State = #s{diff_ct = DiffCt}) ->
    ok = wxTextCtrl:changeValue(DiffCt, integer_to_list(Rating)),
    State;
do_stat({sync, Sync = {true, Complete}},
        State = #s{height = Now, height_ct = HeightCt, sync_ct = SyncCt}) ->
    Top = trunc((Now * 100) / Complete),
    Text = unicode:characters_to_list([integer_to_list(Now), "/", integer_to_list(Top)]),
    ok = wxTextCtrl:changeValue(HeightCt, Text),
    SyncText = io_lib:format("~.2f%", [Complete]),
    ok = wxTextCtrl:changeValue(SyncCt, SyncText),
    State#s{sync = Sync};
do_stat({sync, {false, Complete}},
        State = #s{height = Now, height_ct = HeightCt, sync_ct = SyncCt}) ->
    ok = wxTextCtrl:changeValue(HeightCt, integer_to_list(Now)),
    SyncText = io_lib:format("~.2f%", [Complete]),
    ok = wxTextCtrl:changeValue(SyncCt, SyncText),
    State#s{sync = false};
do_stat({peers, {PeerCount, PeerConnI, PeerConnO}}, State = #s{peer_ct = PeerCt}) ->
    Figures =
        ["Total: ", integer_to_list(PeerCount),
         " (In: ", integer_to_list(PeerConnI), " / ",
         "Out : ",  integer_to_list(PeerConnO), ") "],
    Text = unicode:characters_to_list(Figures),
    ok = wxTextCtrl:changeValue(PeerCt, Text),
    State;
do_stat({txpool, Count}, State = #s{txpool_ct = TXPoolCt}) ->
    ok = wxTextCtrl:changeValue(TXPoolCt, integer_to_list(Count)),
    State.


conf(State) ->
    ok = ael_con:show_conf(),
    State.


run_node(State = #s{text_ct = TextC}) ->
    ok = wxTextCtrl:appendText(TextC, "NODE button clicked!\n"),
    ok = ael_con:run_node(),
    State.


close(State = #s{frame = Frame}) ->
    ok = ael_con:stop(),
    ok = wxWindow:destroy(Frame),
    State.
