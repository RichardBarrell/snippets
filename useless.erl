-module(useless).

-compile(debug_info).
-compile(export_all).
%% -export([add/2, hello/0, fib/1, tail_test_local/0, tail_test_module/0, tail_test_module/1, tail_test_bad/0, start_tennis/0, tennis/1]).

add(A,B) -> A + B.
hello()  -> io:format("Hello, world!~n").

reduc(N) -> reduc(N, true).
reduc(N, A) -> case N of
    0 -> A;
    N -> reduc(N - 1, not(A))
end.

fib(N) when N < 0 -> 0;
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

tail_test_local() ->
    tail_test_local(0).
tail_test_local(N) when N > 16#FFFFFF ->
    io:get_line("pause module here> "),
    N;
tail_test_local(N) ->
    tail_test_local(N + 1).

tail_test_module() ->
    tail_test_module(0).
tail_test_module(N) when N > 16#FFFFFF ->
    io:get_line("pause module here> "),
    N;
tail_test_module(N) ->
    useless:tail_test_module(N + 1).

tail_test_bad() ->
    tail_test_bad(0).
tail_test_bad(N) when N > 16#FFFFFF ->
    io:get_line("pause bad here> "),
    0;
tail_test_bad(N) ->
    1 + tail_test_bad(N + 1).

start_tennis() ->
    T1 = spawn(fun pre_tennis/0),
    T2 = spawn(fun pre_tennis/0),
    T1 ! T2,
    T2 ! T1,
    T1 ! 0,
    {T1, T2}.

pre_tennis() ->
    receive Target ->
            tennis(Target)
    end.

tennis(Target) ->
    receive N ->
            io:format("Tennis ~w (~w)~n", [N, self()])
    end,
    timer:sleep(550),
    Target ! N + 1,
    useless:tennis(Target).

show_bytes(N) ->
    show_f_unit(fun pick_binary_suffix/1, N).

show_hz(N) ->
    show_f_unit(fun pick_decimal_suffix/1, N).

show_f_unit(F, N) ->
    {Suffix, Dividend} = F(N),
    N1 = (N / Dividend),
    lists:flatten(io_lib:format("~.2f~s", [N1, Suffix])).

unit_suffixes(Factor, Ebi) ->
    {S, _} = lists:mapfoldl(
               fun (Name, Acc) ->
                       {{Name, Acc}, Acc*Factor}
               end,
               1,
               Ebi),
    S.                              

pick_decimal_suffix(N) ->
    Ebi=["", "k", "M", "G", "T", "P", "E", "Z", "Y"],
    Suffs=unit_suffixes(1.0e3, Ebi),
    pick_suffix(N, Suffs).
pick_binary_suffix(N) -> 
    Ebi=["", "ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi", "Yi"],
    Suffs=unit_suffixes(1024.0, Ebi),
    pick_suffix(N, Suffs).

pick_suffix(_, [S]) -> S;
pick_suffix(N, [S|Ss]) ->
    [{_, NextFactor}|_] = Ss,
    if (NextFactor > N) ->
            S;
       true ->
            pick_suffix(N, Ss)
    end.
