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
         controls       = #{}          :: map(),
%        % sync
%        upnp_enabled   = none         :: wx:wx_object(),
%        listen_address = none         :: {Addr1 :: wx:wx_object(),
%                                          Addr2 :: wx:wx_object(), 
%                                          Addr3 :: wx:wx_object(),
%                                          Addr4 :: wx:wx_object()},
%        port           = none         :: wx:wx_object(),
%        external_port  = none         :: wx:wx_object(),
%        % mining
%        autostart      = none         :: wx:wx_object(),
%        beneficiary    = none         :: wx:wx_object(),
%        cuckoo         = []           :: [{Exec :: string(), Cores :: integer()}],
%        % fork_management
%        network_id     = none         :: wx:wx_object(),
%        % system
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
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Edit Aeternity Node Configuration"),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    AppWn  = wxScrolledWindow:new(Frame),
    AppSz = wxBoxSizer:new(?wxVERTICAL),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    SaveBn = wxButton:new(Frame, ?saveCONF,   [{label, "Save"}]),

    SchemaPath = filename:join(zx:get_home(), "priv/aeternity_config_schema.json"),
    {ok, Bin} = file:read_file(SchemaPath),
    {ok, GarbageJoggerScriptFormat} = zj:decode(Bin),
    Schema = maps:get("properties", GarbageJoggerScriptFormat),
    ok = ael_gui:show(io_lib:format("~tp~n", [Schema])),

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
    ok = wxFrame:connect(Frame, command_menu_selected),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, meta = Meta, controls = Controls, cores = Cores},
    {Frame, State}.

%% NOTE: Because platform and node configuration are conflated in the JSON schema and
%%       not all elements of the schema lend themselves to menu generation, this
%%       section is likely to change radically as the configuration system gets more
%%       work put into it.
build_schema(Schema, Conf, Parent, Sizer) ->
    EditableSections =
        ["http",
         "logging",
         "peers",
         "regulators",
         "system",
         "websocket"],
    Extract =
        fun
            ("http") ->
                HTTP = maps:get("http", Schema),
                Properties = maps:get("properties", HTTP),
                SubSections = maps:with(["external", "internal"], Properties),
                TrimmedHTTP = maps:put("properties", SubSections, HTTP),
                Configured = maps:get("http", Conf, #{}),
                {"http", TrimmedHTTP, Configured};
            (Key) ->
                {Key, maps:get(Key, Schema), maps:get(Key, Conf, #{})}
        end,
    ToRender = lists:map(Extract, EditableSections),
    Render = fun(Element, Elements) -> render(Element, Parent, Sizer, Elements) end,
    lists:foldl(Render, #{}, ToRender).

render({Key, Scheme, Conf}, Parent, Sizer, Elements) ->
    Title =
        case string:lexemes(Key, "_") of
            ["http"] -> "HTTP";
            Other    -> string:join(lists:map(fun string:titlecase/1, Other), " ")
        end,
    Box = wxStaticBox:new(Parent, ?wxID_ANY, Title),
    BoxSz = wxStaticBoxSizer:new(Box, ?wxVERTICAL),
    tell(info, "Scheme: ~p", [Scheme]),
    tell(info, "Conf: ~p", [Conf]),
    NewElements =
        case maps:get("type", Scheme) of
            "string" ->
                ok = set_description(Parent, BoxSz, Scheme),
                Control = make_control(Parent, Key, Conf, Scheme),
                Checker = text_checker(maps:get("pattern", Scheme, none)),
                _ = wxStaticBoxSizer:add(BoxSz, Control, zxw:flags(base)),
                maps:put(Key, {string, Control, Checker}, Elements);
            "integer" ->
                ok = set_description(Parent, BoxSz, Scheme),
                Control = make_control(Parent, Key, Conf, Scheme),
                Checker = int_checker(Scheme),
                _ = wxStaticBoxSizer:add(BoxSz, Control, zxw:flags(base)),
                maps:put(Key, {integer, Control, Checker}, Elements);
            "boolean" ->
                ok = set_description(Parent, BoxSz, Scheme),
                Value =
                    case determine_value(Key, Conf, Scheme) of
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
                Parts = [{K, S, maps:get(K, Conf, #{})} || {K, S} <- List],
                maps:put(Key, lists:foldl(Render, #{}, Parts), Scheme);
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
        {none, Max}  -> fun(N) -> N =< Max end;
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

make_control(Parent, Key, Conf, Scheme) ->
    case determine_value(Key, Conf, Scheme) of
        "" ->
            wxTextCtrl:new(Parent, ?wxID_ANY);
        Value ->
            Preset = stringify(Value),
            wxTextCtrl:new(Parent, ?wxID_ANY, [{value, Preset}])
    end.

stringify(V) when is_integer(V) -> integer_to_list(V);
stringify(V) when is_float(V)   -> float_to_list(V);
stringify(V)                    -> V.

determine_value(Key, Conf, Scheme) ->
    case maps:find(Key, Conf) of
        {ok, V} -> V;
        error   -> maps:get("default", Scheme, "")
    end.


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

%configure(Conf, State) ->
%    Defaults = defaults(),
%    Defaulty =
%        fun(Section) ->
%            Default = maps:get(Section, Defaults),
%            maps:merge(Default, maps:get(Section, Conf, Default))
%        end,
%    #{"upnp_enabled"   := UPNP,
%      "listen_address" := ListenAddress,
%      "port"           := LocalPort,
%      "external_port"  := ExternalPort}             = Defaulty("sync"),
%    #{"autostart"      := AutoStart,
%      "beneficiary"    := BeneficiaryM,
%      "cuckoo"         := #{"miners" := Miners}}    = Defaulty("mining"),
%    #{"network_id"     := NetworkID}                = Defaulty("fork_management"),
%    [#{"executable" := "mean29-generic",
%       "extra_args" := "-t " ++ CoresS}] = Miners,
%    Beneficiary =
%        case BeneficiaryM =:= unknown of
%            false -> BeneficiaryM;
%            true  -> ""
%        end,
%    Cores = list_to_integer(CoresS),
%    MinerTypes =
%        ["mean29-generic",
%         "mean29-avx2",
%         "lean29-generic",
%         "lean29-avx2"],
%    ok.

%defaults() ->
%    Cores = integer_to_list(core_count() - 1),
%    #{"sync" =>
%        #{"upnp_enabled"   => false,
%          "listen_address" => "0.0.0.0",
%          "port"           => 3015,
%          "external_port"  => 3015},
%      "mining" =>
%        #{"autostart"   => false,
%          "beneficiary" => undefined,
%          "cuckoo"      =>
%            #{"miners" => [#{"executable" => "mean29-generic",
%                             "extra_args" => "-t " ++ Cores}]}},
%      "fork_management" =>
%        #{"network_id" => "ae_mainnet"}}.


close(State = #s{frame = Frame}) ->
    ok = wxWindow:destroy(Frame),
    State.
