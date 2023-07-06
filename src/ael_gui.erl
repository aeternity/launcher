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
-vsn("0.2.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([show/1]).
-export([start_link/4, await_build/0, stop/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2,
         handle_event/2, handle_sync_event/3]).
-include("$zx_include/zx_logger.hrl").


-record(s,
        {frame    = none  :: none | wx:wx_object(),
         buttons  = []    :: [button()],
         console  = none  :: none | wx:wx_object(),
         ae_core  = false :: boolean(),
         sophia   = false :: boolean(),
         platform = none  :: none | ael:platform()}).

-record(button,
        {name = none :: none | module(),
         id   = none :: none | integer(),
         wx   = none :: none | wx:wx_object()}).


% -type state()  :: #s{}.
-type button() :: #button{}.


%%% Interface functions

-spec show(term()) -> ok.

show(Terms) ->
    wx_object:cast(?MODULE, {show, Terms}).


-spec await_build() -> ok.

await_build() ->
    wx_object:cast(?MODULE, await_build).



%%% Lifecycle Functions

-spec start_link(BuildERTS, AECore, Sophia, Platform) -> Result
    when BuildERTS :: string(),
         AECore    :: none | {zx:version(), file:filename()},
         Sophia    :: none | {zx:version(), file:filename()},
         Platform  :: ael:platform(),
         Result    :: wx:wx_object() | {error, Reason :: term()}.

start_link(BuildERTS, AECore, Sophia, Platform) ->
    Args = {BuildERTS, AECore, Sophia, Platform},
    wx_object:start_link({local, ?MODULE}, ?MODULE, Args, []).


-spec stop() -> ok.

stop() ->
    wx_object:cast(?MODULE, stop).


init({BuildERTS, AECore, Sophia, Platform = {{OS, Version}, OTP, ERTS}}) ->
    ok = log(info, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "ÆL"),
    MainSz = wxBoxSizer:new(?wxVERTICAL),

    {AEcoreString, AEcoreOK} =
        case AECore of
            {A_V = {AX, AY, AZ}, _} ->
                A_OK = ael_builder:aecore_ok(A_V),
                A_Format =
                    case A_OK of
                        true ->
                            "Build: AE v~w.~w.~w built with ERTS ~s.~n";
                        obsolete ->
                            "Build: AE v~w.~w.~w built with ERTS ~s. (OBSOLETE)~n"
                    end,
                A_St = io_lib:format(A_Format, [AX, AY, AZ, BuildERTS]),
                {A_St, A_OK};
            none ->
                {"No AE node is currently built.\n", false}
        end,
    {SophiaString, SophiaOK} =
        case Sophia of
            {S_V = {SX, SY, SZ}, _} ->
                S_OK = ael_builder:sophia_ok(S_V),
                S_Format =
                    case S_OK of
                        true     -> "Sophia: v~w.~w.~w.~n";
                        obsolete -> "Sophia: v~w.~w.~w. (OBSOLETE)~n"
                    end,
                S_St = io_lib:format(S_Format, [SX, SY, SZ]),
                {S_St, S_OK};
            none  ->
                {"No Sophia compiler is currently built.\n", false}
        end,

    SameERTS = ERTS =:= BuildERTS,
    ButtonConf =
        case AEcoreOK =:= true andalso SophiaOK =:= true andalso SameERTS of
            true ->
                [{"Wallet",              ael_v_wallet},
                 {"Run Local Node",      ael_v_node},
                 {"Developer Workbench", ael_v_dev},
                 {"Configurator",        ael_v_conf},
                 {"Chain Explorer",      ael_v_chain},
                 {"Network Explorer",    ael_v_network},
                 {"Mempool Explorer",    ael_v_mempool}];
            false ->
                BuildLabel =
                    case {AEcoreOK, SophiaOK, SameERTS} of
                        {false, false, _} -> "BUILD AETERNITY";
                        {obsolete, _, _}  -> "RE-BUILD AETERNITY";
                        {_, obsolete, _}  -> "RE-BUILD AETERNITY";
                        {_, _, false}     -> "RE-BUILD AETERNITY";
                        {_, _, _}         -> "(RE)BUILD AETERNITY"
                    end,
                [{BuildLabel, ael_builder}]
        end,
    Disable = [ael_v_chain, ael_v_network, ael_v_mempool],

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
    ok = wxFrame:setSize(Frame, {700, 500}),
    _ = wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),

    PF =
        "OS: ~p-~s~n"
        "OTP R~s (v~s)~n",
    PlatformString = io_lib:format(PF, [OS, Version, OTP, ERTS]),
    ok = wxTextCtrl:appendText(Console, AEcoreString),
    ok = wxTextCtrl:appendText(Console, SophiaString),
    ok = wxTextCtrl:appendText(Console, PlatformString),

    State = #s{frame   = Frame,    buttons = Buttons, console  = Console,
               ae_core = Platform, sophia  = Sophia,  platform = Platform},
    {Frame, State}.


handle_call(Unexpected, From, State) ->
    ok = tell(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


handle_cast({show, Terms}, State) ->
    NewState = do_show(Terms, State),
    {noreply, NewState};
handle_cast(await_build, State) ->
    NewState = do_await_build(State),
    {noreply, NewState};
handle_cast(stop, State = #s{frame = Frame}) ->
    ok = wxFrame:destroy(Frame),
    {noreply, State};
handle_cast(Unexpected, State) ->
    ok = tell(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


handle_info(Unexpected, State) ->
    ok = tell(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


handle_event(#wx{id = ID, event = #wxCommand{type = command_button_clicked}},
             State = #s{buttons = Buttons}) ->
    ok =
        case lists:keyfind(ID, #button.id, Buttons) of
            #button{name = ael_builder} -> ask_build_ae(State);
            #button{name = Name}        -> ael_con:show_ui(Name);
            false                       -> ok
        end,
    {noreply, State};
handle_event(#wx{event = #wxClose{}}, State = #s{frame = Frame}) ->
    ok = ael_con:stop(),
    ok = wxFrame:destroy(Frame),
    {noreply, State};
handle_event(Event, State) ->
    ok = tell(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.


handle_sync_event(Event, Ref, State) ->
    Message = "Unexpected sync event ~tp (ref: ~tp) State: ~tp~n",
    tell(info, Message, [Event, Ref, State]).


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


%%% Doers

ask_build_ae(#s{frame = Frame, platform = {OS, _, _}}) ->
    {Message, Supported} = check(OS),
    case ask_yes_no(Frame, Message, Supported) of
        ok     -> ael_con:build();
        cancel -> ok
    end.


ask_yes_no(Frame, Message, true) ->
    Text = io_lib:format("~ts", [Message]),
    Style = {style, ?wxOK bor ?wxCANCEL},
    Modal = wxMessageDialog:new(Frame, Text, [Style]),
    Response =
        case wxMessageDialog:showModal(Modal) of
            ?wxID_OK     -> ok;
            ?wxID_CANCEL -> cancel
        end,
    ok = wxMessageDialog:destroy(Modal),
    Response;
ask_yes_no(Frame, Message, false) ->
    Text = io_lib:format("~ts", [Message]),
    Style = {style, ?wxCANCEL},
    Modal = wxMessageDialog:new(Frame, Text, [Style]),
    ?wxID_CANCEL = wxMessageDialog:showModal(Modal),
    ok = wxMessageDialog:destroy(Modal),
    cancel.

check({devuan, _})  -> {build_notice(debian),  true};
check({debian, _})  -> {build_notice(debian),  true};
check({ubuntu, _})  -> {build_notice(debian),  true};
check({gentoo, _})  -> {build_notice(linux),   true};
check({arch, _})    -> {build_notice(linux),   true};
check({slack, _})   -> {build_notice(linux),   true};
check({fedora, _})  -> {build_notice(linux),   true};
check({rhel, _})    -> {build_notice(linux),   true};
check({suse, _})    -> {build_notice(linux),   true};
check({unix, _})    -> {build_notice(unix),    true};
check({osx, _})     -> {build_notice(osx),     true};
check({windows, _}) -> {build_notice(windows), false};
check({unknown, _}) -> {build_notice(unknown), false}.

build_notice(debian) ->
    "Building a node requires a number of packages be available on the host "
    "system in order for the node to be able to run.\n"
    "To ensure that the necessary packages are installed please run the "
    "following command as root or using the `sudo` command:\n\n"
    "apt install gcc curl g++ dpkg-dev build-essential\\\n"
    "    automake autoconf libncurses5-dev libssl-dev\\\n"
    "    flex xsltproc wget vim git cmake libsodium-dev\\\n"
    "    libgmp-dev libtool\n\n"
    "After installing the required packages, press 'OK' below.";
build_notice(linux) ->
    "Building a node requires a number of packages be available on the host "
    "system in order for the node to be able to build and run.\n"
    "Unfortunately this Aeternity launcher project is still in its infancy "
    "and we do not (yet) have complete copy-paste level instructions available for "
    "how you should install the necessary packages. We do have, however, an "
    "accurate list of build dependencies listed below. The names listed here are "
    "the package names by which they are known in Debian-flavored repos. Your distro "
    "may have some slight naming differences.\n\n"
    "The necessary packages are:\n"
    "  - gcc\n"
    "  - curl\n"
    "  - g++\n"
    "  - build-essential (NOTE: this may represent several packages on your system)\n"
    "  - automake\n"
    "  - autoconf\n"
    "  - libncurses5-dev\n"
    "  - libssl-dev\n"
    "  - flex\n"
    "  - xsltproc\n"
    "  - wget\n"
    "  - vim\n"
    "  - git\n"
    "  - cmake\n"
    "  - libsodium-dev\n"
    "  - libgmp-dev\n\n"
    "After installing the required packages, press 'OK' below.";
build_notice(unix) ->
    build_notice(linux);
build_notice(osx) ->
    build_notice(linux);
build_notice(windows) ->
    "Unfortunately building on Windows is not yet supported by this launcher, but "
    "support is planned in the future.";
build_notice(unknown) ->
    "The launcher has had trouble figuring out what platform you are on and so cannot "
    "plan an accurate build procedure.\n"
    "If you would like to help us support your platform please get in touch!\n"
    "The author's email is: ceverett@tsuriai.jp\n"
    "Thank you!".


do_await_build(State = #s{buttons = [#button{wx = Button}]}) ->
    _ = wxButton:disable(Button),
    State.


do_show(Terms, State = #s{console = Console}) ->
    ok = log(info, Terms),
    String =
        case io_lib:deep_char_list(Terms) of
            true  -> Terms;
            false -> io_lib:format("~tw~n", [Terms])
        end,
    ok = wxTextCtrl:appendText(Console, unicode:characters_to_list(String)),
    State.
