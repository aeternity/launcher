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
-vsn("0.1.0").
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

-record(g,
        {parent  = none :: wx:wx_object(),
         sizer   = none :: wx:wx_object(),
         canvas  = none :: wx:wx_object(),
         context = none :: none | wx:wx_object(),
         gl      = none :: new | old | none,
         label_x = ""   :: string(),
         label_y = ""   :: string(),
%        history = []   :: [entry()],
         history = history()   :: [entry()],
         min     = 0    :: number(),
         max     = 0    :: number(),
         tpin    = none :: none | pin(),
         rpin    = none :: none | pin(),
         tx      =  0.0 :: number(),
         ty      = -1.0 :: number(),
         rx      =  0.0 :: number(),
         ry      =  0.0 :: number()}).


-type graph() :: #g{}.
-type pin()   :: {ClickX :: non_neg_integer(),
                  ClickY :: non_neg_integer(),
                  OrigX  :: non_neg_integer(),
                  OrigY  :: non_neg_integer()}.
-type entry() :: {erlang:timestamp(), number()}.


history() ->
    [{C, rand:uniform()} || C <- lists:seq(1, 500)].

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
    Graph#g{history = Entries}.


-spec show(Graph) -> NewGraph
    when Graph    :: graph(),
         NewGraph :: graph().

show(Graph = #g{canvas = Canvas, gl = new}) ->
    Context = wxGLContext:new(Canvas),
    true = wxGLCanvas:setCurrent(Canvas, Context),
    ok = initialize(),
    NewGraph = Graph#g{context = Context},
    ok = render(NewGraph),
    NewGraph;
show(Graph = #g{canvas = Canvas, gl = old}) ->
    ok = wxGLCanvas:setCurrent(Canvas),
    ok = initialize(),
    ok = render(Graph),
    Graph.


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


-spec update(Graph, Entry) -> NewGraph
    when Graph    :: graph(),
         Entry    :: entry(),
         NewGraph :: graph().

update(Graph, Entry) ->
    NewGraph = add_entry(Graph, Entry),
    ok = render(NewGraph),
    NewGraph.


-spec updates(Graph, Entries) -> NewGraph
    when Graph    :: graph(),
         Entries  :: [entry()],
         NewGraph :: graph().

updates(Graph, Entries) ->
    NewGraph = lists:foldl(fun add_entry/2, Graph, lists:reverse(Entries)),
    ok = render(NewGraph),
    NewGraph.

add_entry(Entry = {_, Value}, Graph = #g{history = History, min = Min, max = Max}) ->
    NewMin = min(Min, Value),
    NewMax = max(Max, Value),
    Graph#g{history = [Entry | History], min = NewMin, max = NewMax}.


-spec clear(Graph) -> NewGraph
    when Graph    :: graph(),
         NewGraph :: graph().

clear(Graph) ->
    NewGraph = Graph#g{history = [],
                       min = 0, max = 0,
                       tx = 0.0, ty = -1.0, rx = 0.0, ry = 0.0},
    ok = render(NewGraph),
    NewGraph.


-spec traverse(X, Y, Graph) -> NewGraph
    when Graph    :: graph(),
         X        :: number(),
         Y        :: number(),
         NewGraph :: graph().

traverse(X, Y, Graph) ->
    NewGraph = tupdate(X, Y, Graph),
    ok = render(NewGraph),
    NewGraph.

tupdate(X, Y, Graph = #g{tpin = none, tx = TX, ty = TY}) ->
    TPIN = {X, Y, TX, TY},
    Graph#g{tpin = TPIN};
tupdate(X, Y, Graph = #g{tpin = {PX, PY, OrigX, OrigY}}) ->
    TX = OrigX - ((PX - X) / 40),
    TY = OrigY + ((PY - Y) / 40),
    Graph#g{tx = TX, ty = TY}.



-spec rotate(X, Y, Graph) -> NewGraph
    when Graph    :: graph(),
         X        :: number(),
         Y        :: number(),
         NewGraph :: graph().

rotate(X, Y, Graph) ->
    NewGraph = rupdate(X, Y, Graph),
    ok = render(NewGraph),
    NewGraph.

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


-spec render(Graph) -> ok
    when Graph :: graph().

render(#g{gl = none}) ->
    ok;
render(#g{gl = new, context = none}) ->
    ok;
render(Graph = #g{gl = new, canvas = Canvas, context = Context}) ->
    true = wxGLCanvas:setCurrent(Canvas, Context),
    draw(Graph);
render(Graph = #g{gl = old, canvas = Canvas}) ->
    ok = wxGLCanvas:setCurrent(Canvas),
    draw(Graph).

draw(#g{sizer = Sizer, canvas = Canvas, history = History,
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
    ok = grid(4.0),
    ok = gl:'end'(),
    ok = gl:'begin'(?GL_LINES),
    ok = gl:color3f(1.0, 0.0, 1.0),
    ok = bars(History),
    ok = gl:'end'(),
%   ok = gl:'begin'(?GL_TRIANGLE_STRIP),
%   T =
%       fun({{R, G, B}, {X, Y, Z}}) ->
%           ok = gl:color3f(R, G, B),
%           ok = gl:vertex3f(X, Y, Z)
%       end,
%   Data =
%       [{{1.0, 0.0, 1.0}, { 0.0,  1.0,  0.0}},
%        {{1.0, 0.0, 0.0}, {-1.0,  0.0,  1.0}},
%        {{0.0, 1.0, 0.0}, { 1.0,  0.0,  1.0}},
%        {{0.0, 0.0, 1.0}, { 0.0,  0.0, -1.0}},
%        {{1.0, 0.0, 1.0}, { 0.0,  1.0,  0.0}},
%        {{1.0, 0.0, 0.0}, {-1.0,  0.0,  1.0}}],
%   ok = lists:foreach(T, Data),
%   ok = gl:'end'(),
    case wxGLCanvas:swapBuffers(Canvas) of
        true  -> ok;
        false -> {error, open_gl};
        ok    -> ok % =< R23
    end.

grid(Size) -> grid(-Size, Size).

grid(N, Max) when N =< Max ->
    ok = gl:vertex3f(   N,  0.0,  Max),
    ok = gl:vertex3f(   N,  0.0, -Max),
    ok = gl:vertex3f( Max,  0.0,    N),
    ok = gl:vertex3f(-Max,  0.0,    N),
    ok = gl:vertex3f( 0.0,    N,  Max),
    ok = gl:vertex3f( 0.0,    N, -Max),
    ok = gl:vertex3f( 0.0,  Max,    N),
    ok = gl:vertex3f( 0.0, -Max,    N),
    ok = gl:vertex3f( Max,    N,  0.0),
    ok = gl:vertex3f(-Max,    N,  0.0),
    ok = gl:vertex3f(   N,  Max,  0.0),
    ok = gl:vertex3f(   N, -Max,  0.0),
    grid(N + 0.2, Max);
grid(_, _) ->
    ok.

bars(History) -> bars(History, 2.5).

bars([{_, N} | Rest], Offset) ->
    ok = gl:vertex3f(Offset, 0.0, 0.0),
    ok = gl:vertex3f(Offset,   N, 0.0),
    bars(Rest, Offset - 0.01);
bars([], _) ->
    ok.

-spec destroy(graph()) -> ok.

destroy(#g{canvas = Canvas}) ->
    wxGLCanvas:destroy(Canvas).
