-module(statistics).
-export([mean/1, median/1, variance/1, variance/2, deviation/1, deviation/2]).

mean([]) -> 0;
mean(L) ->
    lists:sum(L) / length(L).

median([]) -> 0;
median(L) when length(L) rem 2 == 1 ->
    lists:nth(trunc(length(L)/2) + 1, lists:sort(L));
median(L) when length(L) rem 2 == 0 ->
    mean(lists:sublist(lists:sort(L), trunc(length(L)/2), 2)).

variance(L) ->
    variance(L, mean(L)).

variance([], _E) -> 0;
variance(L, E) -> 
    N = length(L),
    (1/(N - 1)) *
	lists:foldl(fun (X, Sum) ->
			    Num = X - E,
			    Sum + Num * Num
		    end,
		    0,
		    L
		   ).

deviation(L) ->
    deviation(L, mean(L)).

deviation(L, E) ->
    math:sqrt(variance(L, E)).
