%% @copyright Kopimi

%% @doc Generellt statistik-bibliotek för arbete med listor
-module(statistics).
-export([mean/1, median/1, variance/1, variance/2,
	 deviation/1, deviation/2, meanAndDev/1,
	 bar/2, bar/4, meanAndBar/2, meanAndBar/4]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Medelvärdet av L
mean([]) -> 0;
mean(L) ->
    lists:sum(L) / length(L).

%% @doc Medianen av L
median([]) -> 0;
median(L) when length(L) rem 2 == 1 ->
    lists:nth(trunc(length(L)/2) + 1, lists:sort(L));
median(L) when length(L) rem 2 == 0 ->
    mean(lists:sublist(lists:sort(L), trunc(length(L)/2), 2)).

%% @doc Variansen av L med väntevärdet definierat som medelvärdet
variance(L) ->
    variance(L, mean(L)).

%% @doc Variansen av L med väntevärdet E
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

%% @doc Standardavvikelsen i L med medelvärdet som väntevärde
deviation(L) ->
    deviation(L, mean(L)).

%% @doc Standardavvikelsen i L med väntevärdet E
deviation(L, E) ->
    math:sqrt(variance(L, E)).

%% @doc Medelvärde och standardavvikelse för L
meanAndDev(L) ->
    Mean = mean(L),
    Dev = deviation(L, Mean),
    {Mean, Dev}.

%% @doc Medelvärde och resultatet av {@link bar/2}
meanAndBar(L, Width) ->
    Mean = mean(L),
    Bar = bar(L, Width),
    {Mean, Bar}.

%% @doc Medelvärde och resultatet av {@link bar/4}
meanAndBar(L, Width, Min, Max) ->
    Mean = mean(L),
    Bar = bar(L, Width, Min, Max),
    {Mean, Bar}.

%% @doc
%% <p>Skapa en mätare som visualiserar fördelningen av element i Foo. Antalet tecken mellan [ och ] är Width.</p>
%% <p>Mätarens intervall definieras att gå från minsta till största värdet i Foo.</p>
%% <p>Exempel: 0 [.:|:. .|:. . ] 100</p>
bar(Foo, Width) ->
    Max = lists:max(Foo),
    Min = lists:min(Foo),
    bar(Foo, Width, Min, Max).

%% @doc Som {@link bar/2} men intervallet bestäms till [Min, Max]. Alla element måste ligga inom intervallet.
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
				 Value >= 1 -> $. ; %Factor * 0.1 -> $. ;
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

