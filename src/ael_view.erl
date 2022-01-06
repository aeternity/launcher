%%% @doc
%%% This module is a behavior definition for ael_v_* module types that provide a view/UI
%%% for a particular task.

-module(ael_view).
-vsn("0.1.0").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").


-callback start_link(Conf :: term()) -> wx:wx_object().
%% The start_link/1 function should be implemented in a way that links to the caller and
%% returns the Wx reference instead of the PID. Because the Wx runtime tends to cause the
%% Erlang runtime to crash out when a C++/C side error is encountered (calling with
%% inappropriate C types that can be passed through the Erlang arguments, for example) we
%% insist on linking the process requesting the spawn (should be ael_con in most forseeable
%% cases), even though the ael_con process will also require a monitor in addition (though
%% this is not a concern of the ael_v_* process itself).


-callback to_front(Win :: wx:wx_object()) -> ok.
%% The to_front function should be implemented as a wx_object:cast that passes the call
%% through to `wxWindow:raise(Win)'. The ael_con should normally be able to call
%% wxWindow:raise/1 directly itself, but this has proven to be less reliable than
%% hoped in some versions of Wx, so we are passing this through as a message to the process
%% that owns the window so the call can be made in its own context.
