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
         text  = none :: none | wx:wx_object(),
         font  = none :: none | wx:wx_object(),
         pwd   = none :: none | file:filename(),
         sb    = none :: none | wx:wx_object()}).

-define(OPEN, 10).
-define(SAVE, 11).
-define(COMP, 12).


% -type state() :: #s{}.


%%% Interface

to_front(Win) ->
    wx_object:cast(Win, to_front).



%%% Startup Functions

start_link(Args) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Args, []).


init(Args) ->
    PWD = read_conf(),

    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "ÆL Developers' Bench"),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),

    FontPk = wxFontPickerCtrl:new(Frame, ?wxID_ANY),
    OpenBn = wxButton:new(Frame, ?OPEN, [{label, "Open"}]),
    SaveBn = wxButton:new(Frame, ?SAVE, [{label, "Save"}]),
    CompBn = wxButton:new(Frame, ?COMP, [{label, "Compile"}]),
    _ = wxSizer:add(ButtonSz, FontPk, [{proportion, 2}, {flag, ?wxEXPAND}]),
    _ = wxSizer:addSpacer(ButtonSz, 20),
    _ = wxSizer:add(ButtonSz, OpenBn, zxw:flags(wide)),
    _ = wxSizer:add(ButtonSz, SaveBn, zxw:flags(wide)),
    _ = wxSizer:add(ButtonSz, CompBn, zxw:flags(wide)),

    TextC = wxStyledTextCtrl:new(Frame),
    Mono = wxFont:new(10, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL, [{face, "Monospace"}]),
    ok = wxStyledTextCtrl:styleSetFont(TextC, ?wxSTC_STYLE_DEFAULT, Mono),
    ok = wxFontPickerCtrl:setSelectedFont(FontPk, Mono),

    SB = wxFrame:createStatusBar(Frame),

    wxSizer:add(MainSz, ButtonSz),
    wxSizer:add(MainSz, TextC, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxFrame:setSizer(Frame, MainSz),
    wxSizer:layout(MainSz),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:connect(Frame, command_fontpicker_changed),
    ok = wxFrame:center(Frame),
    ok = set_rect(Frame, Args),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, text = TextC, font = FontPk, pwd = PWD, sb = SB},
    {Frame, State}.

set_rect(Frame, Args) ->
    case proplists:get_value(rect, Args, none) of
        none -> ok;
        Rect -> wxWindow:setSize(Frame, Rect)
    end.

read_conf() ->
    case file:consult(conf_path()) of
        {ok, Terms}     -> proplists:get_value(pwd, Terms, os:getenv("HOME"));
        {error, enoent} -> os:getenv("HOME")
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


handle_event(#wx{id = ID, event = #wxCommand{type = command_button_clicked}}, State) ->
    NewState =
        case ID of
            ?OPEN -> do_open(State);
            ?SAVE -> do_save(State);
            ?COMP -> do_comp(State)
        end,
    {noreply, NewState};
handle_event(#wx{event = #wxFontPicker{type = command_fontpicker_changed}},
             State = #s{text = TextC, font = FontPicker}) ->
    Font = wxFontPickerCtrl:getSelectedFont(FontPicker),
    ok = wxStyledTextCtrl:styleSetFont(TextC, ?wxSTC_STYLE_DEFAULT, Font),
    {noreply, State};
handle_event(#wx{event = #wxClose{}}, State = #s{frame = Frame, pwd = PWD}) ->
    {X, Y} = wxWindow:getPosition(Frame),
    {W, H} = wxWindow:getSize(Frame),
    ok = save_conf([{pwd, PWD}]),
    ok = ael_con:save_rect(?MODULE, {X, Y, W, H}),
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


do_open(State = #s{frame = Frame, text = Text, pwd = PWD, sb = SB}) ->
    Style = {style, ?wxFD_OPEN},
    Options = file_dialog_options([Style], PWD),
    Dialog = wxFileDialog:new(Frame, Options),
    NewPWD =
        case wxFileDialog:showModal(Dialog) of
            ?wxID_OK ->
                Path = wxFileDialog:getPath(Dialog),
                ok = wxFileDialog:destroy(Dialog),
                ok = set_text(Path, Text, SB),
                filename:dirname(Path);
            ?wxID_CANCEL ->
                PWD
        end,
    State#s{pwd = NewPWD}.

file_dialog_options(Opts, none) -> [wildcard() | Opts];
file_dialog_options(Opts, Path) -> [{defaultDir, Path}, wildcard() | Opts].

wildcard() ->
    {wildCard, "Sophia Contract Code (*.aes)|*.aes|All Files (*)|*"}.

set_text(Path, Text, SB) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Contents = unicode:characters_to_list(Bin),
            ok = wxStatusBar:setStatusText(SB, Path),
            wxStyledTextCtrl:setText(Text, Contents);
        {error, eisdir} ->
            ok;
        {error, enoent} ->
            ok
    end.


do_save(State = #s{frame = Frame, text = Text, pwd = PWD, sb = SB}) ->
    Style = {style, ?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT},
    Options = file_dialog_options([Style], PWD),
    Dialog = wxFileDialog:new(Frame, Options),
    NewPWD =
        case wxFileDialog:showModal(Dialog) of
            ?wxID_OK ->
                Path = wxFileDialog:getPath(Dialog),
                ok = wxFileDialog:destroy(Dialog),
                ok = put_text(Path, Text, SB),
                filename:dirname(Path);
            ?wxID_CANCEL ->
                PWD
        end,
    State#s{pwd = NewPWD}.

put_text(Path, Text, SB) ->
    ok = filelib:ensure_dir(Path),
    Bytes = unicode:characters_to_binary(wxStyledTextCtrl:getText(Text)),
    erlang:display(Bytes),
    case file:write_file(Path, Bytes) of
        ok ->
            wxStatusBar:setStatusText(SB, Path);
        {error, eisdir} ->
            ok;
        {error, enoent} ->
            ok
    end.


do_comp(State = #s{frame = Frame, text = Text}) ->
    ok =
        case unicode:characters_to_list(wxStyledTextCtrl:getText(Text)) of
            ""   -> ok;
            Code -> compile(Frame, Code)
        end,
    State.

compile(Frame, Code) ->
    ok = tell(info, "Compiling!"),
    case aeso_compiler:from_string(Code, []) of
        {ok, Out}       -> erlang:display(Out);
        {error, Errors} -> show_comp_errors(Frame, Errors)
    end.

show_comp_errors(Frame, Errors) ->
    Show =
        fun({err, {pos, File, Row, Col}, Type, Expl, Rec}) ->
            Format = "File: ~tp~nRow: ~p, Col: ~p~nType: ~tp~n~ts~n~n~ts",
            Formed = io_lib:format(Format, [File, Row, Col, Type, Expl, Rec]),
            Text = unicode:characters_to_list(Formed),
            zxw:show_message(Frame, Text)
        end,
    lists:foreach(Show, Errors).


save_conf(Data) ->
    zx_lib:write_terms(conf_path(), Data).


conf_path() ->
    filename:join(ael_con:conf_dir_path(), "workbench.eterms").
