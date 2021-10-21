%%% @doc
%%% ÆL Conf Con
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

-module(ael_conf_con).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([to_front/0]).
-export([save/1]).
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).
-include("$zx_include/zx_logger.hrl").
-include("ael_conf.hrl").

-record(editor,
        {id  = 0    :: integer(),
         win = none :: wx:wx_object(),
         pid = none :: none | pid(),
         mon = none :: none | reference()}).

-record(s,
        {frame    = none :: none | wx:wx_object(),
         selector = none :: none | wx:wx_object(),
         selected = none :: none | integer(),
         manifest = []   :: [ael_conf:meta()],
         next_id  = 0    :: integer(),
         editors  = []   :: [editor()]}).


-type state()     :: term().
-type editor()    :: #editor{}.

-define(createCONF, 11).
-define(editCONF,   12).
-define(dropCONF,   13).
-define(exportCONF, 14).


%%% Interface functions

to_front() ->
    wx_object:cast(?MODULE, to_front).


save(ConfMeta) ->
    wx_object:cast(?MODULE, {save, ConfMeta}).


%%% Startup Functions

start_link(Manifest) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Manifest, []).


init(Manifest) ->
    _ = process_flag(trap_exit, true),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Aeternity Configuration"),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    ButtSz = wxBoxSizer:new(?wxHORIZONTAL),
    CreateBn = wxButton:new(Frame, ?createCONF, [{label, "New"}]),
    EditBn   = wxButton:new(Frame, ?editCONF,   [{label, "Edit"}]),
    DropBn   = wxButton:new(Frame, ?dropCONF,   [{label, "Delete"}]),
    ExportBn = wxButton:new(Frame, ?exportCONF, [{label, "Export"}]),
    Headers = [{"ID", 100}, {"Name", 200}, {"Memo", 400}],
    {Items, NextID} = lists:foldl(fun organize/2, {[], 0}, Manifest),
    Selector = zxw:list_control(Frame, ?wxID_ANY, Headers, Items),
    _ = wxSizer:add(ButtSz, CreateBn, zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, EditBn,   zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, DropBn,   zxw:flags(wide)),
    _ = wxSizer:add(ButtSz, ExportBn, zxw:flags(wide)),
    _ = wxSizer:add(MainSz, ButtSz, zxw:flags(base)),
    _ = wxSizer:add(MainSz, Selector, zxw:flags(wide)),
    _ = wxFrame:setSizer(Frame, MainSz),
    _ = wxSizer:layout(MainSz),

    ok = wxFrame:setSize(Frame, {600, 200}),
    ok = wxFrame:center(Frame),
    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:connect(Selector, command_list_item_selected),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),
    State = #s{frame = Frame, selector = Selector, manifest = Manifest, next_id = NextID},
    {Frame, State}.

organize(#conf_meta{id = ID, name = Name, memo = Memo}, {Items, Highest}) ->
    {[{integer_to_list(ID), Name, Memo} | Items], max(Highest, ID)}.


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

handle_info({'DOWN', Mon, process, PID, Info}, State) ->
    NewState = handle_down(Mon, PID, Info, State),
    {noreply, NewState};
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

handle_event(#wx{event = #wxList{type = command_list_item_selected, itemIndex = Index}},
             State) ->
    {noreply, State#s{selected = Index}};
handle_event(#wx{event = #wxCommand{type = command_button_clicked}, id = ID}, State) ->
    NewState =
        case ID of
            ?createCONF -> create(State);
            ?editCONF   -> edit(State);
            ?dropCONF   -> drop(State);
            ?exportCONF -> export(State)
        end,
    {noreply, NewState};
handle_event(#wx{event = #wxList{type = command_list_item_activated, itemIndex = Index}},
             State) ->
    NewState = edit(Index, State),
    {noreply, NewState};
handle_event(#wx{event = #wxClose{}}, State) ->
    NewState = close(State),
    {noreply, NewState};
handle_event(Event, State) ->
    ok = tell(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.


handle_down(Mon, PID, Info, State = #s{editors = Editors}) ->
    case lists:keytake(Mon, #editor.mon, Editors) of
        {value, _, NewEditors} ->
            State#s{editors = NewEditors};
        false ->
            Unexpected = {'DOWN', Mon, process, PID, Info},
            ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
            State
    end.


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


%%% Doers

create(State = #s{editors = Editors, next_id = ID}) ->
    Editor = open_editor(#conf_meta{id = ID}),
    State#s{editors = [Editor | Editors], next_id = ID + 1}.


edit(State = #s{selected = none}) ->
    State;
edit(State = #s{selected = Index}) ->
    edit(Index, State).

edit(Index, State = #s{manifest = Manifest, selector = Selector, editors = Editors}) ->
    ID = list_to_integer(wxListCtrl:getItemText(Selector, Index)),
    case lists:keyfind(ID, #editor.id, Editors) of
        false ->
            ConfMeta = lists:keyfind(ID, #conf_meta.id, Manifest),
            NewEditor = open_editor(ConfMeta),
            NewEditors = [NewEditor | Editors],
            State#s{editors = NewEditors};
        #editor{pid = PID} ->
            ok = ael_conf:to_front(PID),
            State
    end.


open_editor(ConfMeta = #conf_meta{id = ID}) ->
    Win = ael_con:show_editor(ConfMeta),
    PID = wx_object:get_pid(Win),
    Mon = monitor(process, PID),
    #editor{id = ID, win = Win, pid = PID, mon = Mon}.


drop(State = #s{selected = none}) ->
    State;
drop(State = #s{selector = Selector, selected = Index, manifest = Manifest}) ->
    ok = wxListCtrl:deleteItem(Selector, Index),
    ID = list_to_integer(wxListCtrl:getItemText(Selector, Index)),
    case lists:keytake(ID, #conf_meta.id, Manifest) of
        {value, #conf_meta{path = Path}, NewManifest} ->
            ok = case file:delete(Path) of ok -> ok; {error, enoent} -> ok end,
            State#s{manifest = NewManifest};
        false ->
            State
    end.


export(State = #s{selected = none}) ->
    State;
export(State = #s{selector = Selector, selected = Index}) ->
    ID = list_to_integer(wxListCtrl:getItemText(Selector, Index)),
    tell(info, "Would be exporting ~p", [ID]),
    State.


close(State = #s{frame = Frame}) ->
    ok = wxWindow:destroy(Frame),
    State.
