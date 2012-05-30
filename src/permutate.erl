%% Givet en lista, skapa ett antal slumpade dellistor element.
-module(permutate).
-export([get/3]).
-export([work/4]).

get(L, SublistSize, Sublists) ->
    Id = now(),
    [spawn_link(?MODULE, work, [L, SublistSize, Id, self()]) || _ <- lists:seq(1, Sublists)],
    collect(Sublists, Id, []).

collect(0, _Id, Acc) ->
    Acc;
collect(N, Id, Acc) ->
    receive
	{permutate, Id, L} ->
	    collect(N-1, Id, [L|Acc])
    end.

%% @doc Välj ett slumpmässigt element ur L
random_element(L) ->
    lists:nth(random:uniform(length(L)), L).

%% @doc Välj N slumpmässiga element ur L
random_elements(L, N) ->
    [random_element(L) || _ <- lists:seq(1, N)].

% Worker process %
work(L, N, Id, Caller) ->
    random:seed(now()),
    Caller ! {permutate, Id, random_elements(L, N)}.
