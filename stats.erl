-module(stats).
-export([new/0, set/3, set/2, get/2, get/3]).
-include_lib("eunit/include/eunit.hrl").

new() ->
    {stats, []}.


set(Key, Value, {stats, List}) ->
    {stats, lists:keystore(Key, 1, List, {Key, Value})}.

set(Values, Stats = {stats, _}) when is_list(Values) ->
    lists:foldr(fun ({K, V}, S) -> stats:set(K, V, S) end,
		Stats,
		Values).

get(Key, {stats, List}) ->
    lists:keyfind(Key, 1, List).

get(Key, L, Default) ->
    case get(Key, L) of
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
