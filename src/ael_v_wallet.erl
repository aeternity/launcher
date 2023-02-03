%%% @doc
%%% ÆL Wallet
%%%
%%% This task allows a user to explore the contents of the chain graphically.
%%% @end

-module(ael_v_wallet).
-vsn("0.2.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(ael_view).
-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([start_link/1, to_front/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).

-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").

-record(s,
        {frame = none :: none | wx:wx_object(),
         text  = none :: none | wx:wx_object()}).


-define(keyCREATE, 11).
-define(keyREVIVE, 12).
-define(keyDELETE, 13).


%%% Interface

to_front(Win) ->
    wx_object:cast(Win, to_front).



%%% Startup Functions

start_link(Args) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Args, []).


init(Args) ->
    io:format("wxID_LOWEST: ~w wxID_HIGHEST: ~w~n", [?wxID_LOWEST, ?wxID_HIGHEST]),
    Title = "ÆL Chain Explorer",
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, Title),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    StatOpts = [{style, ?wxTE_CENTRE}],
    KeyBx = wxStaticBox:new(Frame, ?wxID_ANY, "Active Key"),
    KeySz = wxStaticBoxSizer:new(KeyBx, ?wxHORIZONTAL),
    Key   = wxStaticText:new(KeyBx, ?wxID_ANY, "", StatOpts),
    _ = wxStaticBoxSizer:add(KeySz, Key, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    CreateBn = wxButton:new(Frame, ?wxID_ANY, [{label, "Create New Key"}]),
    ReviveBn = wxButton:new(Frame, ?wxID_ANY, [{label, "Recover Key"}]),
    DeleteBn = wxButton:new(Frame, ?wxID_ANY, [{label, "Delete Key"}]),
    _ = wxSizer:add(ButtSz, CreateBn, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxSizer:add(ButtSz, ReviveBn, [{flag, ?wxEXPAND}, {proportion, 1}]),
    _ = wxSizer:add(ButtSz, DeleteBn, [{flag, ?wxEXPAND}, {proportion, 1}]),

    TreeC = wxTreeCtrl:new(Frame),
    _ = wxSizer:add(MainSz, KeySz, [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxSizer:add(MainSz, ButtSz, [{flag, ?wxEXPAND}, {proportion, 0}]),
    _ = wxSizer:add(MainSz, TreeC, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ok = wxFrame:setSizer(Frame, MainSz),
    ok = wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:center(Frame),
    ok = set_rect(Frame, Args),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, text = TextC},
    {Frame, State}.

set_rect(Frame, Args) ->
    case proplists:get_value(rect, Args, none) of
        none -> wxFrame:center(Frame);
        Rect -> wxWindow:setSize(Frame, Rect)
    end.


handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


handle_cast({show, Terms}, State) ->
    ok = do_show(Terms, State),
    {noreply, State};
handle_cast(to_front, State = #s{frame = Frame}) ->
    ok = wxWindow:raise(Frame),
    {noreply, State};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


handle_info(Unexpected, State) ->
    ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


handle_event(#wx{event = #wxClose{}}, State) ->
    NewState = close(State),
    {noreply, NewState};
handle_event(Event, State) ->
    ok = log(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


do_show(Terms, #s{text = TextC}) ->
    String = io_lib:format("Received args: ~tp", [Terms]),
    wxTextCtrl:changeValue(TextC, String).


close(State = #s{frame = Frame}) ->
    {X, Y} = wxWindow:getPosition(Frame),
    {W, H} = wxWindow:getSize(Frame),
    ok = ael_con:save_rect(?MODULE, {X, Y, W, H}),
    ok = wxWindow:destroy(Frame),
    State.
