%%% @doc
%%% ÆL GUI
%%%
%%% This process is responsible for creating the main GUI frame displayed to the user.
%%% The main GUI frame is a control center from which any activities can be started.
%%% An activity may be running a local node, exploring the chain, using the config tool
%%% managing account keys, or dev-mode activities like interacting with the Sophia
%%% compiler, contract deployment, transaction formation, network exploration, etc.
%%% Each selected activity opens its own frame (for now) on the desktop and runs
%%% independently of the others. The ael_con process is responsibile for any state
%%% tracking or communication that is required to be common to all running activities
%%% (knowing which activities are running so that deadly duplicates are not spawned,
%%% for example), otherwise activities should run independently of one another and
%%% communicate through the ael_con if lateral communication is necessary.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(ael_gui).
-vsn("0.1.1").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([show/1]).
-export([start_link/2]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2,
         handle_event/2, handle_sync_event/3]).
-include("$zx_include/zx_logger.hrl").


-record(s,
        {frame   = none :: none | wx:wx_object(),
         buttons = []   :: [button()],
         console = none :: none | wx:wx_object()}).

-record(button,
        {name = none :: none | module(),
         id   = none :: none | integer(),
         wx   = none :: none | wx:wx_object()}).


-type state()  :: #s{}.
-type button() :: #button{}.


%%% Interface functions

-spec show(term()) -> ok.

show(Terms) ->
    wx_object:cast(?MODULE, {show, Terms}).



%%% Startup Functions

-spec start_link(BuildMeta, Platform) -> Result
    when BuildMeta :: ael:build_meta(),
         Platform  :: ael:platform(),
         Result    :: wx:wx_object() | {error, Reason :: term()}.

start_link(BuildMeta, Platform) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, {BuildMeta, Platform}, []).


init({BuildMeta, Platform}) ->
    ok = log(info, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "ÆL"),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    ButtonConf =
        [{"Configurator",        ael_v_conf},
         {"Run Local Node",      ael_v_node},
         {"Developer Workbench", ael_v_dev},
         {"Chain Explorer",      ael_v_chain},
         {"Network Explorer",    ael_v_network},
         {"Mempool Explorer",    ael_v_mempool}],
    Disable = [ael_v_dev, ael_v_chain, ael_v_network, ael_v_mempool],

    MakeButton =
        fun({Label, Mod}) ->
            Button = wxButton:new(Frame, ?wxID_ANY, [{label, Label}]),
            ID = wxButton:getId(Button),
            _ = wxSizer:add(MainSz, Button, zxw:flags(base)),
            ok =
                case lists:member(Mod, Disable) of
                    true ->
                        _ = wxButton:disable(Button),
                        ok;
                    false ->
                        ok
                end,
            #button{name = Mod, id = ID, wx = Button}
        end,
    Buttons = lists:map(MakeButton, ButtonConf),

    TextStyle = [{style, ?wxTE_MULTILINE}],
    Console = wxTextCtrl:new(Frame, ?wxID_ANY, TextStyle),
    _ = wxSizer:add(MainSz, Console, zxw:flags(wide)),

    _ = wxFrame:setSizer(Frame, MainSz),
    ok = wxFrame:setSize(Frame, {500, 500}),
    _ = wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),

    BuildString =
        case BuildMeta of
            {AEVer, ERTSVer} ->
                F = "Current build: AE ~s built with ERTS ~s.~n",
                io_lib:format(F, [AEVer, ERTSVer]);
            none ->
                "No AE node is currently built.\n"
        end,
    {{OS, Version}, OTP, ERTS} = Platform,
    Format =
        "OS: ~p-~s~n"
        "OTP R~s (v~s)~n",
    PlatformString = io_lib:format(Format, [OS, Version, OTP, ERTS]),
    ok = wxTextCtrl:appendText(Console, BuildString),
    ok = wxTextCtrl:appendText(Console, PlatformString),

    State = #s{frame = Frame, buttons = Buttons, console = Console},
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

handle_event(#wx{id = ID, event = #wxCommand{type = command_button_clicked}},
             State = #s{buttons = Buttons}) ->
    ok =
        case lists:keyfind(ID, #button.id, Buttons) of
            #button{name = Name} -> ael_con:show_ui(Name);
            false                -> ok
        end,
    {noreply, State};
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
    Message = "Unexpected sync event ~tp (ref: ~tp) State: ~tp~n",
    tell(info, Message, [Event, Ref, State]).


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


%%% Doers

do_show(Terms, State = #s{console = Console}) ->
    ok = log(info, Terms),
    String =
        case io_lib:deep_char_list(Terms) of
            true  -> Terms;
            false -> io_lib:format("~tw~n", [Terms])
        end,
    ok = wxTextCtrl:appendText(Console, unicode:characters_to_list(String)),
    State.


close(State = #s{frame = Frame}) ->
    ok = ael_con:stop(),
    ok = wxFrame:destroy(Frame),
    State.
