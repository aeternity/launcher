%%% @doc
%%% ÆL Conf Editor
%%%
%%% The configuration window itself.
%%% Each window represents a single configuration file.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(ael_conf).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("MIT").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).

-export_type([meta/0]).

-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").


-record(s,
        {frame          = none          :: none | wx:wx_object(),
         conf           = defaults()    :: map(),
         meta           = #conf_meta{}  :: meta(),
         % sync
         upnp_enabled   = none          :: wx:wx_object(),
         listen_address = none          :: {Addr1 :: wx:wx_object(),
                                            Addr2 :: wx:wx_object(), 
                                            Addr3 :: wx:wx_object(),
                                            Addr4 :: wx:wx_object()},
         port           = none          :: wx:wx_object(),
         external_port  = none          :: wx:wx_object(),
         % mining
         autostart      = none          :: wx:wx_object(),
         beneficiary    = none          :: wx:wx_object(),
         cuckoo         = []            :: [{Executable :: string(), Cores :: integer()}],
         % fork_management
         network_id     = none          :: wx:wx_object(),
         % system
         cores          = core_count()  :: pos_integer()}).


-type state() :: term().
-type meta()  :: #conf_meta{}.

-define(saveCONF,   11).


%%% Startup Functions

start_link(ConfMeta) ->
    wx_object:start_link(?MODULE, ConfMeta, []).


init(Meta) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Edit Aeternity Node Configuration"),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    SaveBn = wxButton:new(Frame, ?saveCONF,   [{label, "Save"}]),
    UPnPTx = wxStaticText:new(Frame, ?wxID_ANY, "UPnP", []),
    UPnPCk = wxCheckBox:new(Frame, ?wxID_ANY, "Enabled"),
    AddrTx = wxStaticText:new(Frame, ?wxID_ANY, "Listen Address", []),
    Addr1T = wxTextCtrl:new(Frame, ?wxID_ANY),
    Addr2T = wxTextCtrl:new(Frame, ?wxID_ANY),
    Addr3T = wxTextCtrl:new(Frame, ?wxID_ANY),
    Addr4T = wxTextCtrl:new(Frame, ?wxID_ANY),
    PortCt = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, "3015"}]),
    ExtPortTx = wxStaticText:new(Frame, ?wxID_ANY, "External Port", []),
    ExtPortCt = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, "3015"}]),
%   MiningLabel = wxStaticText:new(Frame, ?wxID_ANY, "Mining", []),
%   AutoStartCk = wxCheckBox:new(Frame, ?wxID_ANY, "Autostart"),
%   BeneficiaryTx = wxStaticText:new(Frame, ?wxID_ANY, "Beneficiary", []),
%   BeneficiaryCt = wxTextCtrl:new(Frame, ?wxID_ANY),
%   NetworkLabel = wxStaticText:new(Frame, ?wxID_ANY, "Network", []),
%   NetworkID = wxTextCtrl:new(Frame, ?wxID_ANY),
%   Cores = core_count() - 1,
%   CoreCt = wxTextCtrl:new(Frame, ?wxID_ANY, [{value, integer_to_list(Cores)}]),

    _ = wxSizer:add(ButtSz, SaveBn, zxw:flags(wide)),
    _ = wxSizer:add(MainSz, ButtSz, zxw:flags(wide)),
    InputSz = wxFlexGridSizer:new(3, 2, 4, 4),
    ok = wxFlexGridSizer:setFlexibleDirection(InputSz, ?wxHORIZONTAL),
    ok = wxFlexGridSizer:addGrowableCol(InputSz, 1),
    _ = wxSizer:add(InputSz, UPnPTx, [{flag, ?wxCENTER}]),
    _ = wxSizer:add(InputSz, UPnPCk, zxw:flags(wide)),
    AddrSz = wxBoxSizer:new(?wxHORIZONTAL),
    _ = wxSizer:add(InputSz, AddrTx, [{flag, ?wxCENTER}]),
    _ = wxSizer:add(AddrSz, Addr1T, zxw:flags(base)),
    _ = wxSizer:add(AddrSz, Addr2T, zxw:flags(base)),
    _ = wxSizer:add(AddrSz, Addr3T, zxw:flags(base)),
    _ = wxSizer:add(AddrSz, Addr4T, zxw:flags(base)),
    Colon = wxStaticText:new(Frame, ?wxID_ANY, ":", []),
    _ = wxSizer:add(AddrSz, Colon, zxw:flags(base)),
    _ = wxSizer:add(AddrSz, PortCt, zxw:flags(base)),
    _ = wxSizer:add(InputSz, AddrSz, zxw:flags(wide)),
    _ = wxSizer:add(InputSz, ExtPortTx, [{flag, ?wxCENTER}]),
    _ = wxSizer:add(InputSz, ExtPortCt),
    _ = wxSizer:add(MainSz, InputSz, zxw:flags(base)),
    
    wxFrame:setSizer(Frame, MainSz),
    wxSizer:layout(MainSz),

    ok = wxFrame:fit(Frame),
    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_menu_selected),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, meta = Meta,
               upnp_enabled   = UPnPCk,
               listen_address = {Addr1T, Addr2T, Addr3T, Addr4T},
               port           = PortCt,
               external_port  = ExtPortCt},
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

defaults() ->
    Cores = integer_to_list(core_count() - 1),
    #{"sync" =>
        #{"upnp_enabled"   => false,
          "listen_address" => "0.0.0.0",
          "port"           => 3015,
          "external_port"  => 3015},
      "mining" =>
        #{"autostart"   => false,
          "beneficiary" => undefined,
          "cuckoo"      =>
            #{"miners" => [#{"executable" => "mean29-generic",
                             "extra_args" => "-t " ++ Cores}]}},
      "fork_management" =>
        #{"network_id" => "ae_mainnet"}}.

core_count() ->
    Count = core_count(erlang:system_info(cpu_topology), 0),
    ok = tell("Core count: ~w", [Count]),
    Count.

core_count([], C) ->
    C;
core_count([{_, {logical, N}} | T], C) when is_integer(N) ->
    core_count(T, C + 1);
core_count([{_, Sub} | T], C) when is_list(Sub) ->
    core_count(T, core_count(Sub, C));
core_count([{_, _, {logical, N}} | T], C) when is_integer(N) ->
    core_count(T, C + 1);
core_count([{_, _, Sub} | T], C) when is_list(Sub) ->
    core_count(T, core_count(Sub, C)).


close(State = #s{frame = Frame}) ->
    ok = wxWindow:destroy(Frame),
    State.
