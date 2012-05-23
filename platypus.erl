-module(platypus).
-export([start/2, start/3, step/1, get_stats/1, get_action/2, attack/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behavior(gen_server).

-record(actions, {reproduce = 20,
		  get_food = 80,
		  fight = 0
		 }).

%% API %%

start(Habitat, World) ->
    start(
      stats:set([{energy, 10},
		 {actions, #actions{}},
		 {age, 0},
		 {maxage, 10},
		 {defence, 10},
		 {attack, 10}
		],
		stats:new()), Habitat, World).

start(Stats, Habitat, World) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Habitat, Stats, World}, []),
    Pid.

step(Name) ->
    gen_server:cast(Name, step).

get_stats(Name) ->
    gen_server:call(Name, get_stats).

attack(Name, Power) ->
    gen_server:call(Name, {fight, Power}).

% Internal functions

act(Stats, Habitat, World) ->

    %% Sleep for better scheduling mix
    timer:sleep(random(1, 4)),

    Random = random(1, 100),
    {actions, Actions} = stats:get(actions, Stats),

    Reproduce = Actions#actions.reproduce,
    GetFood   = Actions#actions.get_food,
    Fight     = Actions#actions.fight,

    if
    	Random < Reproduce ->
    	    reproduce(Stats, Habitat),
 	    {energy, Energy} = stats:get(energy, Stats),
	    NewStats = stats:set(energy, Energy-2, Stats);
    	Random < GetFood + Reproduce ->
    	    get_food(Stats, World);
	Random < GetFood + Reproduce + Fight ->
	    fight(Stats, Habitat);
	true ->
	    Stats
    end.

normalize_actions(#actions{reproduce = Reproduce,
			   get_food  = GetFood,
			   fight     = Fight
			  }) ->
    F = 100 / (Reproduce + GetFood + Fight),
    #actions{reproduce = Reproduce * F,
	     get_food  = GetFood   * F,
	     fight     = Fight     * F}.

get_action(get_food,  #actions{get_food  = R}) -> R;
get_action(reproduce, #actions{reproduce = R}) -> R;
get_action(fight,     #actions{fight     = R}) -> R.
    

mutate(Stats) ->
    {actions, A} = stats:get(actions, Stats),

    NewA = normalize_actions(#actions{reproduce = A#actions.reproduce + random(-3, 3),
				      get_food  = A#actions.get_food  + random(-3, 3),
				      fight     = A#actions.fight     + random(-3, 3)}),
    {maxage, M} = stats:get(maxage, Stats),
    NewMaxAge = M + (random(-1, 1)/10),

    {attack, Atk} = stats:get(attack, Stats),
    NewAttack = Atk + random(-3, 3),
    
    {defence, Def} = stats:get(defence, Stats),
    NewDefence = Def + random(-3, 3),

    stats:set([{actions, NewA},
	       {maxage, NewMaxAge},
	       {attack, NewAttack},
	       {defence, NewDefence}
	       ], Stats).

reproduce(Stats, Habitat) ->
    Random = random(1, 5),
    if
	Random == 1 ->
	    NewS = mutate(Stats),
	    {energy, Energy} = stats:get(energy, Stats),
	    NewStats = stats:set([{energy, Energy-2},{age, 0}], NewS),
	    habitat:create_animal(NewStats, Habitat),
	    true;
	true -> false
    end.

get_food(Stats, World) ->
    {energy, Energy} = stats:get(energy, Stats),
    NewEnergy = min(10, Energy + world:get_food(self(), World)),
    NewStats = stats:set(energy, NewEnergy, Stats),
    NewStats.

fight(Stats, Habitat) ->
    {energy, Energy} = stats:get(energy, Stats),
    {attack, Attack} = stats:get(attack, Stats),
    Power = random(0, Attack),
    Prey = habitat:random_animal(Habitat),
    case platypus:attack(Prey, Power) of
	{win, Food} ->
	    NewEnergy = Energy + Food;
	{lose, Injury} ->
	    NewEnergy = Energy - Injury
    end,
    stats:set(energy, NewEnergy, Stats).
	

random(Min, Max) ->
    Random = random:uniform(Max - Min + 1),
    Random + Min - 1.

%% Callbacks %%

init(State) ->
    random:seed(now()),
    {ok, State}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(step, {Habitat, Stats, World}) ->

    Stats2 = lists:foldr(fun (_, Acc) -> act(Acc, Habitat, World) end,
			 Stats,
			 lists:seq(1, 10)),

    {energy, Energy} = stats:get(energy, Stats2),
    {age, Age} = stats:get(age, Stats2),
    {maxage, MaxAge} = stats:get(maxage, Stats2),
    {defence, Def} = stats:get(defence, Stats2),
    {attack, Atk} = stats:get(attack, Stats2),
    
    NewEnergy = Energy - trunc((Def + Atk) / 10),

    NewStats = stats:set([{energy, NewEnergy},
			  {age, Age + 1}],
			 Stats2),
    if
	NewEnergy =< 0 ->
	    habitat:remove_animal(self(), Habitat),
    	    Status = {stop, normal, {Habitat, NewStats, World}};

	Age >= MaxAge ->
	    habitat:remove_animal(self(), Habitat),
    	    Status = {stop, normal, {Habitat, NewStats, World}};

    	true ->
    	    Status = {noreply, {Habitat, NewStats, World}}
    end,
    Habitat ! done,
    Status.

handle_call({fight, Power}, _From, S = {_Habitat, Stats, _World}) ->
    {defence, Defence} = stats:get(defence, Stats),
    Protection = random(0, Defence),
    case Power - Protection of
	N when N > 0 -> % I lost
	    {energy, Energy} = stats:get(energy, Stats),
	    {stop, normal, {win, Energy}, S};
	N when N =< 0 -> % I won
	    {reply, {lose, N}, S}
    end;

handle_call(get_stats, _From, S = {_Habitat, Stats, _World}) ->
    {reply, Stats, S}.

