%%% @doc
%%% ÆL OS Command Library
%%%
%%% This is a tiny helper library to abstract some simple interactions with the
%%% OS over ports.
%%% @end

-module(ael_os).

-vsn("0.1.3").
-author("Craig Everett <zxq9@zxq9.com>").
-copyright("Craig Everett <zxq9@zxq9.com>").
-license("ISC").

-export([cmd/1, cmd/2]).


cmd(Command) ->
    Port = open_port({spawn, Command}, [stream, binary, exit_status, stderr_to_stdout]),
    {os_pid, OS_PID} = erlang:port_info(Port, os_pid),
    listen(Port, OS_PID).


cmd(Dir, Command) ->
    {ok, PWD} = file:get_cwd(),
    ok = file:set_cwd(Dir),
    ok = cmd(Command),
    file:set_cwd(PWD).
    

listen(Port, OS_PID) ->
    receive
        {Port, {data, Bin}} ->
            Cleaned = unicode:characters_to_list(clean(Bin)),
            ok = ael_gui:show(Cleaned),
            listen(Port, OS_PID);
        {Port, {exit_status, 0}} ->
            ael_gui:show("Operation complete!\n");
        {Port, Error = {exit_status, N}} ->
            Message = io_lib:format("Operation failed with status code: ~w", [N]),
            ok = ael_gui:show(Message),
            {error, Error};
        Unexpected ->
            ok = ael_gui:show("UNEXPECTED\n"),
            ok = ael_gui:show(Unexpected),
            Out = os:cmd(io_lib:format("kill -9 ~p", [OS_PID])),
            {error, wtf, Unexpected, Out}
    end.


clean(<<>>)                           -> <<>>;
clean(<<"\e[0m", Rest/binary>>)       -> <<(clean(Rest))/binary>>;
clean(<<"\e[0;32m", Rest/binary>>)    -> <<(clean(Rest))/binary>>;
clean(<<"\e[0;35m", Rest/binary>>)    -> <<(clean(Rest))/binary>>;
clean(<<"~", Rest/binary>>)           -> <<"-",(clean(Rest))/binary>>;
clean(<<Byte:1/binary, Rest/binary>>) -> <<Byte/binary, (clean(Rest))/binary>>.
