-module(statistics).
-export([mean/1, median/1, variance/1, variance/2,
	 deviation/1, deviation/2, meanAndDev/1,
	 bar/2, bar/4, meanAndBar/2, meanAndBar/4]).
-include_lib("eunit/include/eunit.hrl").

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

meanAndDev(L) ->
    Mean = mean(L),
    Dev = deviation(L, Mean),
    {Mean, Dev}.

meanAndBar(L, Width) ->
    Mean = mean(L),
    Bar = bar(L, Width),
    {Mean, Bar}.

meanAndBar(L, Width, Min, Max) ->
    Mean = mean(L),
    Bar = bar(L, Width, Min, Max),
    {Mean, Bar}.

%% 0 [.:|:. .|:. . ] 100
bar(Foo, Width) ->
    Max = lists:max(Foo),
    Min = lists:min(Foo),
    bar(Foo, Width, Min, Max).

bar(Foo, Width, Min, Max) ->
    Count = length(Foo),
    Interval = max(1, (Max-Min)/Width),
    Factor = Count / Width,
    
    ArrayAdd = fun(E, A) ->
		       Index = min(trunc((E - Min) / Interval), Width-1),
		       Old = array:get(Index, A),
		       array:set(Index, Old + 1, A)
	       end,

    Array = lists:foldl(ArrayAdd,
			array:new([{size,Width},{fixed,true},{default,0}]),
			Foo),
    
    AddChar = fun(_Index, Value, String) ->
		      Char = if
				 Value >= Factor * 2-> $| ;
				 Value >= Factor * 1 -> $: ;
				 Value >= Factor * 0.1 -> $. ;
				 true -> $ end,
		      [Char | String]
	      end,
    Bar = array:foldr(AddChar, "", Array),
    io_lib:format("~3.B [~s] ~-3.B", [trunc(Min), Bar, trunc(Max)]).
    

%% Tests %%

mean_test_() ->
    Values = [{0, []},
	      {5.0, [5]},
	      {4.0, [4, 4]},
	      {5.0, [10, 0, 5]},
	      {5.0, [10, 0, 10, 0]},
	      {-2.5, [-10, 5]} ],
     [?_assertEqual(Correct, mean(List)) || {Correct, List} <- Values].

median_test_() ->
    Values = [{0, []},
	      {5, [5]},
	      {4.0, [4, 4]},
	      {5, [10, 0, 5]},
	      {-10, [-20, -10, 25]},
	      {50.0, [0, 25, 75, 100]} ],

    [?_assertEqual(Correct, median(List)) || {Correct, List} <- Values].

