%% @copyright Kopimi

%% @doc En nyckel-v�rde-struktur. Implementerad som en l�nkad lista.
%%      Strukturen �r omuterbar s� ett nytt stats-objekt returneras av varje funktion.
%% @type stats(). {@link stats}
-module(stats).
-export([new/0, set/3, set/2, get/2, get/3]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Skapa ett tomt stats-objekt
%% @spec new() -> stats()
new() ->
    {stats, []}.

%% @doc S�tt nyckeln Key till v�rdet Value i Stats
%% @spec set(term(), term(), stats()) -> stats()
set(Key, Value, _Stats = {stats, List}) ->
    {stats, lists:keystore(Key, 1, List, {Key, Value})}.

%% @doc F�r varje nyckel-v�rde-par i Values, s�tt
%%      nyckeln till motsvarande v�rde i Stats.
%% @spec set(list(tuple(term(), term())), stats()) -> stats()
set(Values, Stats = {stats, _}) when is_list(Values) ->
    lists:foldr(fun ({K, V}, S) -> stats:set(K, V, S) end,
		Stats,
		Values).

%% @doc Tupeln {Key, Value} d�r Value �r v�rdet f�r Key i Stats.
%%      <em>false</em> om Key saknas i Stats.
get(Key, _Stats = {stats, List}) ->
    lists:keyfind(Key, 1, List).

%% @ doc Som {@link get/2} men Default om Key saknas i Stats.
get(Key, Stats, Default) ->
    case get(Key, Stats) of
	{Key, _Value} = T ->
	    T;
	false ->
	    {Key, Default}
    end.
	    

%% Test cases %%

new_test() ->
    ?assertEqual({stats, []}, new()).

set_test_() ->
    L1 = set(awesome, 1337, set(hello, world, new())),
    L2 = set(hello, hell, L1),
    L3 = set(awesome, 42, L2),
    [?_assertEqual({stats, [{hello, world}, {awesome, 1337}]}, L1),
     ?_assertEqual({stats, [{hello, hell}, {awesome, 1337}]}, L2),
     ?_assertEqual({stats, [{hello, hell}, {awesome, 42}]}, L3)].

get_test_() ->
    L = set(awesome, 1337, set(hello, world, new())),
    [?_assertEqual({awesome, 1337}, get(awesome, L)),
     ?_assertEqual({hello, world}, get(hello, L)),
     ?_assertEqual(false, get(animal, L)),
     ?_assertEqual({awesome, 1337}, get(awesome, L, 42)),
     ?_assertEqual({animal, pony}, get(animal, L, pony))].
