-module(platypus).
-export([start/1, step/1, get_stats/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behavior(gen_server).


%% API %%

start(Habitat) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Habitat, stats:new()}, []),
    Pid.

step(Name) ->
    gen_server:cast(Name, step).

get_stats(Name) ->
    gen_server:call(Name, get_stats).


%% Callbacks %%

init(State) ->
    {ok, State}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(step, {Habitat, Stats}) ->
    {energy, Energy} = stats:get(energy, Stats, 10),
    Food = lists:sum(
	     [ habitat:get_food(self(), Habitat) || _ <- lists:seq(0, 2) ]
	    ),
    NewEnergy = Energy - 1 + Food,
    if
    	NewEnergy == 0 ->
    	    {stop, normal, {Habitat, Stats}};
    	true ->
    	    NewStats = stats:set(energy, NewEnergy, Stats),
    	    {noreply, {Habitat, NewStats}}
    end.

handle_call(get_stats, _From, S = {_Habitat, Stats}) ->
    {reply, Stats, S}.

