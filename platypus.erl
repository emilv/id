-module(platypus).
-export([start/1, start/2, step/1, get_stats/1, get_action/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behavior(gen_server).

-record(actions, {reproduce = 20,
		  get_food = 80}).

%% API %%

start(Habitat) ->
    start(
      stats:set([{energy, 10},
		 {actions, #actions{}},
		 {age, 0},
		 {maxage, 10}
		],
		stats:new()), Habitat).

start(Stats, Habitat) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Habitat, Stats}, []),
    Pid.

step(Name) ->
    gen_server:cast(Name, step).

get_stats(Name) ->
    gen_server:call(Name, get_stats, 60000).

% Internal functions

act(Stats, Habitat) ->
    Random = habitat:random(Habitat, 1, 100),
    {actions, Actions} = stats:get(actions, Stats),

    Reproduce = Actions#actions.reproduce,
    GetFood   = Actions#actions.get_food,

    if
    	Random < Reproduce ->
    	    reproduce(Stats, Habitat),
 	    {energy, Energy} = stats:get(energy, Stats),
	    NewStats = stats:set(energy, Energy-2, Stats);
    	Random < GetFood + Reproduce ->
    	    get_food(Stats, Habitat);
	true ->
	    Stats
    end.

normalize_actions(#actions{reproduce = Reproduce,
			   get_food  = GetFood
			  }) ->
    F = 100 / (Reproduce + GetFood),
    #actions{reproduce = Reproduce * F,
	     get_food  = GetFood   * F}.    

get_action(get_food,  #actions{get_food  = R}) -> R;
get_action(reproduce, #actions{reproduce = R}) -> R.
    

mutate(Stats, Habitat) ->
    {actions, A} = stats:get(actions, Stats),
    R1 = habitat:random(Habitat, -1, 1),
    R2 = habitat:random(Habitat, -1, 1),
    NewA = normalize_actions(#actions{reproduce = A#actions.reproduce + R1,
				      get_food  = A#actions.get_food  + R2}),
    stats:set(actions, NewA, Stats).

reproduce(Stats, Habitat) ->
    Random = habitat:random(Habitat, 1, 5),
    if
	Random == 1 ->
	    NewS = mutate(Stats, Habitat),
	    {energy, Energy} = stats:get(energy, Stats),
	    NewStats = stats:set([{energy, Energy-2},{age, 0}], NewS),
	    habitat:create_animal(NewStats, Habitat),
	    true;
	true -> false
    end.

get_food(Stats, Habitat) ->
    {energy, Energy} = stats:get(energy, Stats),
    NewEnergy = min(10, Energy + habitat:get_food(self(), Habitat)),
    NewStats = stats:set(energy, NewEnergy, Stats),
    NewStats.

%% Callbacks %%

init(State) ->
    {ok, State}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(step, {Habitat, Stats}) ->

    Stats2 = lists:foldr(fun (_, Acc) -> act(Acc, Habitat) end,
			 Stats,
			 lists:seq(1, 10)),

    {energy, Energy} = stats:get(energy, Stats2),
    {age, Age} = stats:get(age, Stats2),
    {maxage, MaxAge} = stats:get(maxage, Stats2),

    NewStats = stats:set([{energy, Energy - 1},{age, Age +1}], Stats2),
    if
	Energy =< 1 ->
    	    {stop, normal, {Habitat, NewStats}};

	Age >= MaxAge ->
    	    {stop, normal, {Habitat, NewStats}};

    	true ->
    	    {noreply, {Habitat, NewStats}}
    end.

handle_call(get_stats, _From, S = {_Habitat, Stats}) ->
    {reply, Stats, S}.

