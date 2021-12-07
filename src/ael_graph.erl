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
-export([new/4, new/5, show/1, update/2, updates/2, render/1, destroy/1]).
-opaque([graph/0]).
-include("$zx_include/zx_logger.hrl").

-record(g,
        {parent  = none :: wx:wx_object(),
         sizer   = none :: wx:wx_object(),
         canvas  = none :: wx:wx_object(),
         gl      = none :: new | old | none,
         label_x = ""   :: string(),
         label_y = ""   :: string(),
%        history = []   :: [entry()]}).
         history = history()   :: [entry()]}).

-type graph() :: #g{}.
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
%   ok = wxGLCanvas:setSizer(Canvas, Sizer),
    _ = wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND}, {proportion, 1}]),
    ok = wxGLCanvas:connect(Canvas, paint),
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


-spec show(graph()) -> ok.

show(Graph = #g{canvas = Canvas, gl = new}) ->
    Context = wxGLContext:new(Canvas),
    true = wxGLCanvas:setCurrent(Canvas, Context),
    ok = initialize(),
    render(Graph);
show(Graph = #g{canvas = Canvas, gl = old}) ->
    ok = wxGLCanvas:setCurrent(Canvas),
    ok = initialize(),
    render(Graph).

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

update(Graph, _Entry) -> Graph.


-spec updates(Graph, Entries) -> NewGraph
    when Graph    :: graph(),
         Entries  :: [entry()],
         NewGraph :: graph().

updates(Graph, _Entries) -> Graph.


-spec render(Graph) -> ok
    when Graph :: graph().

render(#g{gl = none}) ->
    ok;
render(#g{sizer = Sizer, canvas = Canvas, history = History}) ->
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
%   ok = gl:translatef(TX, TY, -3.0),
%   ok = gl:rotatef(RY, 1.0, 0.0, 0.0),
%   ok = gl:rotatef(RX, 0.0, 1.0, 0.0),
    ok = gl:'begin'(?GL_LINES),
    ok = grid(-2.5, 2.5),
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
        false -> {error, open_gl}
    end.

grid(N, Max) when N =< Max ->
    ok = gl:vertex3f(   N,  0.0,  2.5),
    ok = gl:vertex3f(   N,  0.0, -2.5),
    ok = gl:vertex3f( 2.5,  0.0,    N),
    ok = gl:vertex3f(-2.5,  0.0,    N),
    ok = gl:vertex3f( 0.0,    N,  2.5),
    ok = gl:vertex3f( 0.0,    N, -2.5),
    ok = gl:vertex3f( 0.0,  2.5,    N),
    ok = gl:vertex3f( 0.0, -2.5,    N),
    ok = gl:vertex3f( 2.5,    N,  0.0),
    ok = gl:vertex3f(-2.5,    N,  0.0),
    ok = gl:vertex3f(   N,  2.5,  0.0),
    ok = gl:vertex3f(   N, -2.5,  0.0),
    grid(N + 0.25, Max);
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
