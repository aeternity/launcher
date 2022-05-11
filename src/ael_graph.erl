%%% @doc
%%% Aeternity Graph
%%%
%%% This is a meta widget module for WX that should eventually be pushed over into
%%% zx_widgets but for now is an experimental part of ael. The purpose is to provide a
%%% graph widget API that can draw, display, and update a visual graph provided an
%%% abstract reference to the graph state, and either data to plot or incremental
%%% plot update data.
%%% @end

-module(ael_graph).
-vsn("0.1.4").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-export([new/4, new/5, show/1,
         get_wx_id/1,
         update/2, updates/2, clear/1,
         render/1,
         traverse/3, rotate/3,
         clear_t_pin/1,
         clear_r_pin/1,
         destroy/1]).
-opaque([graph/0]).
-include("$zx_include/zx_logger.hrl").

-record(h,
        {entries = []  :: [entry()],
         min     = 0   :: number(),
         max     = 0   :: number(),
         size    = 0   :: integer(),
         full    = 500 :: integer()}).

-record(g,
        {parent  = none :: wx:wx_object(),
         sizer   = none :: wx:wx_object(),
         canvas  = none :: wx:wx_object(),
         context = none :: none | wx:wx_object(),
         gl      = none :: new | old | none,
         label_x = ""   :: string(),
         label_y = ""   :: string(),
         history = #h{} :: [entry()],
         tpin    = none :: none | pin(),
         rpin    = none :: none | pin(),
         tx      =  0.0 :: number(),
         ty      = -0.9 :: number(),
         rx      =  0.0 :: number(),
         ry      =  0.0 :: number()}).


-type graph()   :: #g{}.
-type pin()     :: {ClickX :: non_neg_integer(),
                    ClickY :: non_neg_integer(),
                    OrigX  :: non_neg_integer(),
                    OrigY  :: non_neg_integer()}.
-type entry()   :: {erlang:timestamp(), number()}.



%%% Interface

-spec new(Parent, Sizer, LabelX, LabelY) -> Graph
    when Parent :: wx:wx_object(),
         Sizer  :: wx:wx_object(),
         LabelX :: string(),
         LabelY :: string(),
         Graph  :: graph().

new(Parent, Sizer, LabelX, LabelY) ->
    CanvasAttributes =
        [{style, ?wxFULL_REPAINT_ON_RESIZE},
         {attribList,
          [?WX_GL_RGBA,
           ?WX_GL_MIN_RED,        8,
           ?WX_GL_MIN_GREEN,      8,
           ?WX_GL_MIN_BLUE,       8,
           ?WX_GL_DEPTH_SIZE,     24,
           ?WX_GL_DOUBLEBUFFER,
           ?WX_GL_SAMPLE_BUFFERS, 1,
           ?WX_GL_SAMPLES,        4,
           0]}],
    Canvas = wxGLCanvas:new(Parent, CanvasAttributes),
    _ = wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ok = wxGLCanvas:connect(Canvas, paint),
    ok = wxGLCanvas:connect(Canvas, left_down),
    ok = wxGLCanvas:connect(Canvas, left_up),
    ok = wxGLCanvas:connect(Canvas, right_down),
    ok = wxGLCanvas:connect(Canvas, right_up),
    ok = wxGLCanvas:connect(Canvas, motion),
    GL =
        case erlang:system_info(otp_release) >= "24" of
            true  -> new;
            false -> old
        end,
    #g{parent = Parent, canvas = Canvas, sizer = Sizer,
       label_x = LabelX, label_y = LabelY, gl = GL}.


-spec new(Parent, Sizer, LabelX, LabelY, Entries) -> Graph
    when Parent  :: wx:wx_object(),
         Sizer   :: wx:wx_object(),
         LabelX  :: string(),
         LabelY  :: string(),
         Entries :: [entry()],
         Graph   :: graph().

new(Parent, Sizer, LabelX, LabelY, Entries) ->
    Graph = new(Parent, Sizer, LabelX, LabelY),
    lists:foldl(fun add_entry/2, Graph, lists:reverse(Entries)).


-spec show(Graph) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

show(Graph = #g{canvas = Canvas, gl = new}) ->
    Context = wxGLContext:new(Canvas),
    true = wxGLCanvas:setCurrent(Canvas, Context),
    ok = initialize(),
    NewGraph = Graph#g{context = Context},
    render(NewGraph);
show(Graph = #g{canvas = Canvas, gl = old}) ->
    ok = wxGLCanvas:setCurrent(Canvas),
    ok = initialize(),
    render(Graph).


-spec get_wx_id(graph()) -> integer().

get_wx_id(#g{canvas = Canvas}) ->
    wxWindow:getId(Canvas).

initialize() ->
    ok = gl:enable(?GL_DEPTH_TEST),
    ok = gl:depthFunc(?GL_LESS),
    ok = gl:enable(?GL_CULL_FACE),
    ok = gl:enable(?GL_MULTISAMPLE),
    ok = gl:cullFace(?GL_BACK),
    ok.


-spec update(Graph, Entry) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         Entry    :: entry(),
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

update(Graph = #g{history = History}, Entry) ->
    NewGraph = Graph#g{history = add_entry(Entry, History)},
    render(NewGraph).


-spec updates(Graph, Entries) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         Entries  :: [entry()],
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

updates(Graph = #g{history = History}, Entries) ->
    NewHistory = lists:foldl(fun add_entry/2, History, lists:reverse(Entries)),
    NewGraph = Graph#g{history = NewHistory},
    render(NewGraph).

add_entry(Entry = {_, Value},
          History = #h{entries = Entries, min = Min, max = Max}) ->
    NewMin = min(Min, Value),
    NewMax = max(Max, Value),
    History#h{entries = [Entry | Entries], min = NewMin, max = NewMax}.


-spec clear(Graph) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

clear(Graph) ->
    NewGraph = Graph#g{history = #h{}, tx = 0.0, ty = -1.0, rx = 0.0, ry = 0.0},
    render(NewGraph).


-spec traverse(X, Y, Graph) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         X        :: number(),
         Y        :: number(),
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

traverse(X, Y, Graph) ->
    NewGraph = tupdate(X, Y, Graph),
    render(NewGraph).

tupdate(X, Y, Graph = #g{tpin = none, tx = TX, ty = TY}) ->
    TPIN = {X, Y, TX, TY},
    Graph#g{tpin = TPIN};
tupdate(X, Y, Graph = #g{tpin = {PX, PY, OrigX, OrigY}}) ->
    TX = OrigX - ((PX - X) / 40),
    TY = OrigY + ((PY - Y) / 40),
    Graph#g{tx = TX, ty = TY}.



-spec rotate(X, Y, Graph) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         X        :: number(),
         Y        :: number(),
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

rotate(X, Y, Graph) ->
    NewGraph = rupdate(X, Y, Graph),
    render(NewGraph).

rupdate(X, Y, Graph = #g{rpin = none, rx = RX, ry = RY}) ->
    RPIN = {X, Y, RX, RY},
    Graph#g{rpin = RPIN};
rupdate(X, Y, Graph = #g{rpin = {PX, PY, OrigX, OrigY}}) ->
    RX = OrigX + ((PX - X) / -2),
    RY = OrigY + ((PY - Y) / -2),
    Graph#g{rx = RX, ry = RY}.


-spec clear_t_pin(Graph) -> NewGraph
    when Graph    :: graph(),
         NewGraph :: graph().

clear_t_pin(Graph = #g{tpin = {_, _, _, _}}) ->
    Graph#g{tpin = none};
clear_t_pin(Graph) ->
    Graph.


-spec clear_r_pin(Graph) -> NewGraph
    when Graph    :: graph(),
         NewGraph :: graph().

clear_r_pin(Graph = #g{rpin = {_, _, _, _}}) ->
    Graph#g{rpin = none};
clear_r_pin(Graph) ->
    Graph.


-spec render(Graph) -> {Outcome, NewGraph}
    when Graph    :: graph(),
         Outcome  :: ok | {error, gl},
         NewGraph :: graph().

render(Graph = #g{gl = none}) ->
    Graph;
render(Graph = #g{gl = new, context = none}) ->
    Graph;
render(Graph = #g{gl = new, canvas = Canvas, context = Context}) ->
    true = wxGLCanvas:setCurrent(Canvas, Context),
    draw(Graph);
render(Graph = #g{gl = old, canvas = Canvas}) ->
    ok = wxGLCanvas:setCurrent(Canvas),
    draw(Graph).

draw(Graph = #g{sizer = Sizer, canvas = Canvas, history = History,
                rx = RX, ry = RY, tx = TX, ty = TY}) ->
    ok = gl:clearColor(0.1, 0.1, 0.2, 1.0),
    ok = gl:color3f(1.0, 1.0, 1.0),
    {W, H} = wxSizer:getSize(Sizer),
    ok = gl:viewport(0, 0, W, H),
    ok = gl:matrixMode(?GL_PROJECTION),
    ok = gl:loadIdentity(),
%   ok = gl:frustum(-3.0, 3.0, -2.0 * H / W, 2.0 * H / W, 1.0, 40.0),
    ok = gl:ortho(-3.0, 3.0, -3.0 * H / W, 3.0 * H / W, -20.0, 20.0),
    ok = gl:matrixMode(?GL_MODELVIEW),
    ok = gl:loadIdentity(),
    ok = gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    ok = gl:translatef(TX, TY, 0.0),
    ok = gl:rotatef(RY, 1.0, 0.0, 0.0),
    ok = gl:rotatef(RX, 0.0, 1.0, 0.0),
    ok = gl:'begin'(?GL_LINES),
    ok = grid(3.0),
    ok = gl:'end'(),
    ok = gl:'begin'(?GL_LINES),
    ok = gl:color3f(1.0, 0.0, 1.0),
    NewHistory = bars(History),
    ok = gl:'end'(),
    NewGraph = Graph#g{history = NewHistory},
    case wxGLCanvas:swapBuffers(Canvas) of
        true  -> {ok, NewGraph};
        ok    -> {ok, NewGraph}; % =< R23
        false -> {{error, gl}, NewGraph}
    end.

grid(Size) -> grid(-Size, Size).

grid(N, Max) when N =< Max ->
    ok = gl:vertex3f(N, 0.0, Max),
    ok = gl:vertex3f(N, 0.0, 0.0),
    ok = gl:vertex3f(N, Max, 0.0),
    ok = gl:vertex3f(N, 0.0, 0.0),
    ok =
        case N >= 0.0 of
            true ->
                ok = gl:vertex3f( Max, 0.0,   N),
                ok = gl:vertex3f(-Max, 0.0,   N),
                ok = gl:vertex3f( Max,   N, 0.0),
                ok = gl:vertex3f(-Max,   N, 0.0);
            false ->
                ok
        end,
    grid(N + 0.5, Max);
grid(_, _) ->
    ok.

bars(History = #h{entries = Entries, min = Min, max = Max}) ->
    Offset = 3.0,
    NewEntries = bars(Entries, Min, Max, Offset, -Offset),
    History#h{entries = NewEntries}.

bars([E = {_, Value} | Rest], Min, Max, Offset, Limit) when Offset > Limit ->
    Scaled = scale(Value, Min, Max),
    ok = gl:vertex3f(Offset,    0.0, 0.01),
    ok = gl:vertex3f(Offset, Scaled, 0.01),
    [E | bars(Rest, Min, Max, Offset - 0.01, Limit)];
bars(_, _, _, _, _) ->
    [].

scale(Value, Min, Max) ->
    Magnitude = max(1, max(abs(Min), abs(Max))),
    (Value * 1.5) / Magnitude.

-spec destroy(graph()) -> ok.

destroy(#g{canvas = Canvas}) ->
    wxGLCanvas:destroy(Canvas).
