%%% @doc
%%% ÆL Conf
%%%
%%% This process is responsible for creating the conf GUI frame which should show the
%%% user the configs known to the system, their names, and any comments for each.
%%% A user should be able to create, edit and delete config files as well as export
%%% them (actually just a copy operation) to an arbitrary location as a .json file.
%%%
%%% Config files have metadata that includes the name of the config, its path, and
%%% comments.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(ael_v_conf).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([to_front/1, set_manifest/1]).
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).
-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").

-record(s,
        {frame    = none :: none | wx:wx_object(),
         selector = none :: none | wx:wx_object(),
         selected = none :: none | non_neg_integer()}).


-type state()  :: term().

-define(newCONF,    11).
-define(editCONF,   12).
-define(dropCONF,   13).
-define(exportCONF, 14).


%%% Interface functions

-spec to_front(Win) -> ok
    when Win :: module() | pid() | wx:wx_object().

to_front(Win) ->
    wx_object:cast(Win, to_front).


-spec set_manifest(Entries) -> ok
    when Entries :: [ael:conf_meta()].

set_manifest(Entries) ->
    case is_pid(whereis(?MODULE)) of
        true  -> wx_object:cast(?MODULE, {set_manifest, Entries});
        false -> ok
    end.


%%% Startup Functions

start_link(Manifest) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Manifest, []).


init(Manifest) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Aeternity Configuration"),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    NewBn    = wxButton:new(Frame, ?newCONF, [{label, "New"}]),
    EditBn   = wxButton:new(Frame, ?editCONF,   [{label, "Edit"}]),
    DropBn   = wxButton:new(Frame, ?dropCONF,   [{label, "Delete"}]),
    ExportBn = wxButton:new(Frame, ?exportCONF, [{label, "Export"}]),
    Headers = [{"Name", 200}, {"Memo", 400}],
    Items = [{Name, Memo} || #conf_meta{name = Name, memo = Memo} <- Manifest],
    Selector = zxw:list_control(Frame, ?wxID_ANY, Headers, Items),
    _ = wxSizer:add(ButtSz, NewBn,    zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, EditBn,   zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, DropBn,   zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, ExportBn, zxw:flags(wide)),
    _ = wxSizer:add(MainSz, ButtSz,   zxw:flags(base)),
    _ = wxSizer:add(MainSz, Selector, zxw:flags(wide)),
    _ = wxFrame:setSizer(Frame, MainSz),
    _ = wxSizer:layout(MainSz),

    _ = wxButton:disable(ExportBn),

    ok = wxFrame:setSize(Frame, {600, 200}),
    ok = wxFrame:center(Frame),
    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:connect(Selector, command_list_item_selected),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, selector = Selector},
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

handle_cast(to_front, State = #s{frame = Frame}) ->
    ok = wxFrame:raise(Frame),
    {noreply, State};
handle_cast({set_manifest, Entries}, State) ->
    ok = do_set_manifest(Entries, State),
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

handle_event(#wx{event = #wxList{type = command_list_item_selected,
                                 itemIndex = Index}},
             State) ->
    {noreply, State#s{selected = Index}};
handle_event(#wx{event = #wxCommand{type = command_button_clicked}, id = ID}, State) ->
    ok =
        case ID of
            ?newCONF    -> new();
            ?editCONF   -> edit(State);
            ?dropCONF   -> drop(State);
            ?exportCONF -> export(State)
        end,
    {noreply, State};
handle_event(#wx{event = #wxList{type = command_list_item_activated,
                                 itemIndex = Index}},
             State) ->
    NewState = State#s{selected = Index},
    ok = edit(NewState),
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

new() ->
    ael_con:show_ui({ael_v_conf_editor, ""}).


edit(#s{selector = Selector, selected = Selected}) ->
    Name = wxListCtrl:getItemText(Selector, Selected, [{col, 0}]),
    ael_con:show_ui({ael_v_conf_editor, Name}).


drop(#s{selected = none}) ->
    ok;
drop(#s{selector = Selector, selected = Selected}) ->
    Name = wxListCtrl:getItemText(Selector, Selected, [{col, 0}]),
    ael_con:drop_conf(Name).


export(#s{selected = none}) ->
    ok;
export(#s{selector = Selector, selected = Selected}) ->
    Name = wxListCtrl:getItemText(Selector, Selected, [{col, 0}]),
    tell(info, "Would be exporting ~p", [Name]),
    ok.


do_set_manifest(Entries, #s{selector = Selector}) ->
    Items = [{Name, Memo} || #conf_meta{name = Name, memo = Memo} <- Entries],
    true = wxListCtrl:deleteAllItems(Selector),
    ColNums = [0, 1],
    AddRow =
        fun({Row, Atts}) ->
            _ = wxListCtrl:insertItem(Selector, Row, ""),
            Set = fun({Col, Data}) -> wxListCtrl:setItem(Selector, Row, Col, Data) end,
            ok = lists:foreach(Set, lists:zip(ColNums, tuple_to_list(Atts)))
        end,
    lists:foreach(AddRow, lists:zip(lists:seq(0, length(Items) -1), Items)).


close(State = #s{frame = Frame}) ->
    ok = wxWindow:destroy(Frame),
    State.
