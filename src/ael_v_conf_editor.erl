%%% @doc
%%% ÆL Conf Editor
%%%
%%% The configuration window itself.
%%% Each window represents a single configuration file.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(ael_v_conf_editor).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(ael_view).
-behavior(wx_object).
-export([start_link/1, to_front/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).
-include_lib("wx/include/wx.hrl").
-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").


-record(s,
        {frame          = none         :: none | wx:wx_object(),
         conf           = #{}          :: map(),
         meta           = #conf_meta{} :: ael:conf_meta(),
         name           = none         :: none | wx:object(),
         memo           = none         :: none | wx:object(),
         controls       = #{}          :: map(),
         cores          = 2            :: pos_integer()}).


-type state() :: term().

-define(saveCONF,   11).


%%% Interface

to_front(Win) ->
    wx_object:cast(Win, to_front).


%%% Startup Functions

start_link(ConfMeta) ->
    wx_object:start_link(?MODULE, ConfMeta, []).


init({Meta, Conf, Cores}) ->
    tell(info, "Meta: ~p", [Meta]),
    tell(info, "Conf: ~p", [Conf]),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Edit Aeternity Node Configuration"),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    AppWn  = wxScrolledWindow:new(Frame),
    AppSz = wxBoxSizer:new(?wxVERTICAL),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    SaveBn = wxButton:new(Frame, ?saveCONF,   [{label, "Save"}]),

    MetaSz = wxFlexGridSizer:new(2),
    ok = wxFlexGridSizer:setFlexibleDirection(MetaSz, ?wxHORIZONTAL),
    ok = wxFlexGridSizer:addGrowableCol(MetaSz, 1),
    #conf_meta{name = Name, memo = Memo} = Meta,
    NameLabel = wxStaticText:new(Frame, ?wxID_ANY, "Name"),
    _ = wxSizer:add(MetaSz, NameLabel, zxw:flags(base)),
    NameTx = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, Name}]),
    _ = wxSizer:add(MetaSz, NameTx, zxw:flags(wide)),
    MemoLabel = wxStaticText:new(Frame, ?wxID_ANY, "Memo"),
    _ = wxSizer:add(MetaSz, MemoLabel, zxw:flags(base)),
    MemoTx = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, Memo}]),
    _ = wxSizer:add(MetaSz, MemoTx, zxw:flags(wide)),
    _ = wxSizer:add(MainSz, MetaSz, zxw:flags(wide)),

    SchemaPath = filename:join(zx:get_home(), "priv/aeternity_config_schema.json"),
    {ok, Bin} = file:read_file(SchemaPath),
    {ok, GarbageJoggerScriptFormat} = zj:decode(Bin),
    Schema = maps:get("properties", GarbageJoggerScriptFormat),

    _ = wxSizer:add(ButtSz, SaveBn, zxw:flags(wide)),
    _ = wxSizer:add(MainSz, ButtSz, zxw:flags(wide)),
    Controls = build_schema(Schema, Conf, AppWn, AppSz),
    
    ok = wxFrame:setSizer(Frame, MainSz),
    ok = wxFrame:setSize(Frame, {600, 600}),
    ok = wxScrolledWindow:setSizerAndFit(AppWn, AppSz),
    ok = wxScrolledWindow:setScrollRate(AppWn, 5, 5),
    _ = wxBoxSizer:add(MainSz, AppWn, zxw:flags(wide)),
    ok = wxSizer:layout(MainSz),
    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:connect(Frame, command_menu_selected),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, meta = Meta,
               name = NameTx, memo = MemoTx,
               controls = Controls, cores = Cores},
    {Frame, State}.

%% NOTE: Because platform and node configuration are conflated in the JSON schema and
%%       not all elements of the schema lend themselves to menu generation, this
%%       section is likely to change radically as the configuration system gets more
%%       work put into it.
build_schema(Schema, Conf, Parent, Sizer) ->
    EditableSections =
        ["fork_management",
%        "chain",
         "http",
         "logging",
         "peers",
%        "regulators",
%        "mining",
         "system",
         "websocket"],
    Extract =
        fun
            ("http") ->
                HTTP = maps:get("http", Schema),
                Properties = maps:get("properties", HTTP),
                SubSections = maps:with(["external", "internal"], Properties),
                TrimmedHTTP = maps:put("properties", SubSections, HTTP),
                Configured = maps:get("http", Conf, none),
                {"http", TrimmedHTTP, Configured};
%           ("chain") ->
%               Chain = maps:get("chain", Schema),
%               Properties = maps:get("properties", Chain),
%               SubSections = maps:with(["persist", "db_path"], Properties),
%               TrimmedChain = maps:put("properties", SubSections, Chain),
%               Configured = maps:get("chain", Conf, none),
%               {"chain", TrimmedChain, Configured};
            ("fork_management") ->
                FM = maps:get("fork_management", Schema),
                Properties = maps:get("properties", FM),
                SubSections = maps:with(["network_id"], Properties),
                TrimmedFM = maps:put("properties", SubSections, FM),
                Configured = maps:get("fork_management", Conf, none),
                {"fork_management", TrimmedFM, Configured};
            (Key) ->
                {Key, maps:get(Key, Schema), maps:get(Key, Conf, none)}
        end,
    ToRender = lists:map(Extract, EditableSections),
    Render = fun(Element, Elements) -> render(Element, Parent, Sizer, Elements) end,
    lists:foldl(Render, #{}, ToRender).

render({Key, Scheme, Conf}, Parent, Sizer, Elements) ->
    tell(info, "Key: ~p, Conf: ~p", [Key, Conf]),
    Title =
        case string:lexemes(Key, "_") of
            ["http"] -> "HTTP";
            Other    -> string:join(lists:map(fun string:titlecase/1, Other), " ")
        end,
    Box = wxStaticBox:new(Parent, ?wxID_ANY, Title),
    BoxSz = wxStaticBoxSizer:new(Box, ?wxVERTICAL),
    NewElements =
        case maps:get("type", Scheme) of
            "string" ->
                ok = set_description(Parent, BoxSz, Scheme),
                Control = make_control(Parent, Conf, Scheme),
                Checker = text_checker(maps:get("pattern", Scheme, none)),
                _ = wxStaticBoxSizer:add(BoxSz, Control, zxw:flags(base)),
                maps:put(Key, {string, Control, Checker}, Elements);
            "integer" ->
                ok = set_description(Parent, BoxSz, Scheme),
                Control = make_control(Parent, Conf, Scheme),
                Checker = int_checker(Scheme),
                _ = wxStaticBoxSizer:add(BoxSz, Control, zxw:flags(base)),
                maps:put(Key, {integer, Control, Checker}, Elements);
            "boolean" ->
                ok = set_description(Parent, BoxSz, Scheme),
                Value =
                    case determine_value(Conf, Scheme) of
                        "" -> false;
                        V  -> V
                    end,
                Control = wxCheckBox:new(Parent, ?wxID_ANY, Key),
                ok = wxCheckBox:setValue(Control, Value),
                _ = wxStaticBoxSizer:add(BoxSz, Control, zxw:flags(base)),
                maps:put(Key, {boolean, Control, none}, Elements);
            "object" ->
                Render = fun(P, Ps) -> render(P, Parent, BoxSz, Ps) end,
                List = maps:to_list(maps:get("properties", Scheme)),
                Parts =
                    case Conf =:= none of
                        true  -> [{K, S, none} || {K, S} <- List];
                        false -> [{K, S, maps:get(K, Conf, none)} || {K, S} <- List]
                    end,
                maps:put(Key, lists:foldl(Render, #{}, Parts), Elements);
            "array" ->
%               Render = fun(P, Ps) -> render(P, Parent, BoxSz, Ps) end,
%               maps:put(Key, lists:map(Render, 
                Elements
        end,
    _ = wxSizer:add(Sizer, BoxSz, zxw:flags(base)),
    NewElements.

text_checker(none) ->
    none;
text_checker(Pattern) ->
    fun(Subject) ->
        case re:run(Subject, Pattern) of
            {match, _} -> ok;
            nomatch    -> error
        end
    end.

int_checker(Scheme) ->
    case {maps:get("minimum", Scheme, none), maps:get("maximum", Scheme, none)} of
        {none, none} -> none;
        {none, Max}  -> fun(N) -> N   =< Max end;
        {Min,  none} -> fun(N) -> Min =< N end;
        {Min,  Max}  -> fun(N) -> Min =< N andalso N =< Max end
    end.

set_description(Parent, Sizer, Scheme) ->
    case maps:find("description", Scheme) of
        {ok, Text} ->
            Description = wxStaticText:new(Parent, ?wxID_ANY, Text),
            _ = wxSizer:add(Sizer, Description, zxw:flags(wide)),
            ok;
        error ->
            ok
    end.

make_control(Parent, Conf, Scheme) ->
    case determine_value(Conf, Scheme) of
        "" ->
            wxTextCtrl:new(Parent, ?wxID_ANY);
        Value ->
            Preset = stringify(Value),
            wxTextCtrl:new(Parent, ?wxID_ANY, [{value, Preset}])
    end.

stringify(V) when is_integer(V) -> integer_to_list(V);
stringify(V) when is_float(V)   -> float_to_list(V);
stringify(V)                    -> V.

determine_value(none, Scheme) -> maps:get("default", Scheme, "");
determine_value(Val, _)       -> Val.


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

handle_event(#wx{event = #wxCommand{type = command_button_clicked}}, State) ->
    NewState =
        case do_save(State) of
            ok    -> close(State);
            error -> State
        end,
    {noreply, NewState};
handle_event(#wx{event = #wxClose{}}, State) ->
    NewState = close(State),
    {noreply, NewState};
handle_event(Event, State) ->
    ok = tell(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


%%% Doers

do_save(#s{meta = OldMeta, name = NameTx, memo = MemoTx, controls = Controls}) ->
    case wxTextCtrl:getValue(NameTx) of
        "" ->
            error;
        Name ->
            Memo = wxTextCtrl:getValue(MemoTx),
            PartialConf = maps:map(fun extract/2, Controls),
            #{"fork_management" := #{"network_id" := NetID}} = PartialConf,
            File = string:join(string:lexemes(Name, " \　"), "_") ++ ".json",
            Path = filename:join(ael_con:conf_dir_path(), File),
            Data = filename:join(ael_con:data_root(), NetID),
            Conf = maps:put("chain", #{"db_path" => Data}, PartialConf),
            NewMeta = OldMeta#conf_meta{name = Name,
                                        memo = Memo,
                                        path = Path,
                                        data = Data},
            ok = ael_con:save_conf(OldMeta, NewMeta, Conf)
    end.


% TODO: This procedure needs to be changed for explicit recursion so it can set
%       a final `ok | error` return while also highlighting (and maybe assembling
%       an explanation about) the failed parts of the configuration input if a
%       check actually fails instead of silently accepting the bad input and nixing
%       it!
extract(Key, {string, Control, none}) ->
    log(info, "Key: ~p, Control: ~p", [Key, Control]),
    wxTextCtrl:getValue(Control);
extract(Key, {string, Control, Check}) ->
    log(info, "Key: ~p, Control: ~p", [Key, Control]),
    String = wxTextCtrl:getValue(Control),
    case Check(String) of
        ok    -> String;
        error -> ""
    end;
extract(Key, {integer, Control, none}) ->
    log(info, "Key: ~p, Control: ~p", [Key, Control]),
    try
        list_to_integer(wxTextCtrl:getValue(Control))
    catch
        _:_ -> 0
    end;
extract(Key, {integer, Control, Check}) ->
    log(info, "Key: ~p, Control: ~p", [Key, Control]),
    try
        N = list_to_integer(wxTextCtrl:getValue(Control)),
        true = Check(N),
        N
    catch
        _:_ -> 0
    end;
extract(Key, {boolean, Control, none}) ->
    log(info, "Key: ~p, Control: ~p", [Key, Control]),
    wxCheckBox:getValue(Control);
extract(Key, Controls) when is_map(Controls) ->
    log(info, "Key: ~p, Control: ~p", [Key, Controls]),
    maps:map(fun extract/2, Controls).


close(State = #s{frame = Frame}) ->
    ok = wxWindow:destroy(Frame),
    State.
