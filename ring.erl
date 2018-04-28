-module(ring).
-export([start/1, start_proc/2, bigflush/0]).

start(Num) ->
    start_proc(Num, self()).

start_proc(0, Pid) ->
    io:format("done ~w~n", [Pid]),
    Pid ! ok;
start_proc(Num, Pid) ->
    io:format("started ~w ~w~n", [Num, Pid]),
    NPid = spawn(?MODULE, start_proc, [Num-1, Pid]),
    receive ok -> ok end,
    io:format("received ~w ~w~n", [Num, Pid]),
    NPid ! ok.

bigflush() ->
    receive Msg ->
            io:format("received ~w~n", [Msg]),
            bigflush()
    after
        100 -> 'end'
    end.
