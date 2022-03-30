%%% @doc
%%% ÆL Developer Interface
%%%
%%% This task allows a user to explore the contents of the chain graphically.
%%% @end

-module(ael_v_dev).
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

-define(OPEN, 10).
-define(SAVE, 11).


% -type state() :: term().


%%% Interface

to_front(Win) ->
    wx_object:cast(Win, to_front).



%%% Startup Functions

start_link(none) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, "ÆL Developers' Bench", []).


init(Title) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, Title),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),

    OpenBn = wxButton:new(Frame, ?OPEN, [{label, "Open"}]),
    SaveBn = wxButton:new(Frame, ?SAVE, [{label, "Save"}]),
    _ = wxSizer:add(ButtonSz, OpenBn, zxw:flags(wide)),
    _ = wxSizer:add(ButtonSz, SaveBn, zxw:flags(wide)),

    TextC = wxTextCtrl:new(Frame, ?wxID_ANY, [{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    Mono = wxFont:new(8, ?wxMODERN, ?wxNORMAL, ?wxNORMAL, [{face, "Monospace"}]),
    true = wxTextCtrl:setFont(TextC, Mono),

    wxSizer:add(MainSz, ButtonSz),
    wxSizer:add(MainSz, TextC, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxFrame:setSizer(Frame, MainSz),
    wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, text = TextC},
    {Frame, State}.


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


handle_event(#wx{id = ID, event = #wxCommand{type = command_button_clicked}}, State) ->
    ok =
        case ID of
            ?OPEN -> tell(info, "Opening!");
            ?SAVE -> tell(info, "Saving!")
        end,
    {noreply, State};
handle_event(#wx{event = #wxClose{}}, State = #s{frame = Frame}) ->
    ok = wxWindow:destroy(Frame),
    {noreply, State};
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
