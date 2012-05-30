-module(platypus).
-export([start/2, start/3, step/3, get_stats/1, get_action/2, attack/2, create_stat/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, extreme/0]).
-behavior(gen_server).

-record(actions, {reproduce = 10,
		  get_food = 30,
		  fight = 3
		 }).

%% API %%

create_stat({GF, R, F, D, A}) ->
    Stats = stats:new(),
    Actions = #actions{reproduce = R,
		      get_food = GF,
		      fight = F},
    NewS = stats:set([{actions, normalize_actions(Actions)},
		      {attack, A},
		      {defence, D},
		      {temperature, 20},
		      {age, 4},
		      {alive, true},
		      {maxage, 200},
		      {energy, 20}
		      ], Stats).

start(Habitat, World) ->
    Actions = #actions{},
    start(
      stats:set([{energy, 4},
		 {actions, normalize_actions(Actions)},
		 {age, 4},
		 {maxage, 200},
		 {defence, 2},
		 {attack, 10},
		 {temperature, 20},
		 {alive, true}
		],
		stats:new()), Habitat, World).

start(Stats, Habitat, World) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Habitat, Stats, World}, []),
    Pid.

step(Name, Prey, Temperature) ->
    gen_server:cast(Name, {step, Prey, Temperature}).

get_stats(Name) ->
    gen_server:call(Name, get_stats).

attack(Name, Power) ->
    Alive = is_process_alive(Name),
    if
	Name == self() ->
	    false;
	Alive ->
	    gen_server:cast(Name, {fight, self(), Power}),
	    receive
		{win, Name, Result} ->
		    {win, Result};
		{lose, Name, Result} ->
		    {lose, Result}
	    after 10 ->
		    false
	    end;
	true ->
	    false
    end.

% Internal functions

act(Stats, Habitat, World, Opponent) ->

    %% Sleep for better scheduling mix
    timer:sleep(random(1, 4)),

    Random = random(1, 100),
    {actions, Actions} = stats:get(actions, Stats),

    Reproduce = Actions#actions.reproduce,
    GetFood   = Actions#actions.get_food,
    Fight     = Actions#actions.fight,

    {energy, E} = stats:get(energy, Stats),
    if
	E > 0 ->
	    Stats2 = Stats;
	true ->
	    Stats2 = stats:set(alive, false, Stats)
    end,

    {alive, Alive} = stats:get(alive, Stats),

    if
	Alive == false ->
	    Stats2;
    	Random < Reproduce ->
    	    Reprod = reproduce(Stats2, Habitat),
	    if Reprod == true ->
		    {energy, Energy} = stats:get(energy, Stats2),
		    NewStats = stats:set(energy, Energy-3, Stats2);
	       true ->
		    Stats2
	    end;
    	Random < GetFood + Reproduce ->
    	    get_food(Stats2, World);
	Random < GetFood + Reproduce + Fight ->
	    fight(Stats2, Opponent);
	true ->
	    Stats2
    end.

normalize_actions(#actions{reproduce = Reproduce,
			   get_food  = GetFood,
			   fight     = Fight
			  }) ->
    F = 100 / (Reproduce + GetFood + Fight),
    #actions{reproduce = Reproduce * F,
	     get_food  = GetFood   * F,
	     fight     = Fight     * F}.

or_zero(N) when N < 0 ->
    0;
or_zero(N) ->
    N.

get_action(get_food,  #actions{get_food  = R}) -> R;
get_action(reproduce, #actions{reproduce = R}) -> R;
get_action(fight,     #actions{fight     = R}) -> R.
    
extreme() ->
    trunc(random(0,990)/1000)*10 + 1.

mutate(Stats) ->
    {actions, A} = stats:get(actions, Stats),
    
    NewA = normalize_actions(#actions{reproduce = or_zero(A#actions.reproduce + random(-3, 3)*extreme()),
				      get_food  = or_zero(A#actions.get_food  + random(-3, 3)*extreme()),
				      fight     = or_zero(A#actions.fight     + random(-3, 3)*extreme())}),

    {maxage, M} = stats:get(maxage, Stats),
    NewMaxAge = or_zero(M + (random(-10, 10)*extreme())),

    {attack, Atk} = stats:get(attack, Stats),
    NewAttack = or_zero(Atk + random(-3, 3)*extreme()),
    
    {defence, Def} = stats:get(defence, Stats),
    NewDefence = or_zero(Def + random(-3, 3)*extreme()),

    {temperature, Temp} = stats:get(temperature, Stats),
    NewTemp = Temp + random(-1, 1)*extreme(),

    stats:set([{actions, NewA},
	       {maxage, NewMaxAge},
	       {attack, NewAttack},
	       {defence, NewDefence},
	       {temperature, NewTemp}
	       ], Stats).

reproduce(Stats, Habitat) ->
    Random = random(1, 5),
    {age, Age} = stats:get(age, Stats),
%    {maxage, Maxage} = stats:get(maxage, Stats),
    {energy, Energy} = stats:get(energy, Stats),
	   
    if
%	Age < Maxage / 10 ->
%	    false;
%	Age > 4* Maxage / 5 ->
%	    false;
	Energy < 4 ->
	    false;
	Random == 1 ->
	    NewS = mutate(Stats),
	    NewStats = stats:set([{energy, 2},{age, 0}], NewS),
	    habitat:create_animal(NewStats, Habitat),
	    true;
	true -> false
    end.

get_food(Stats, World) ->
    {energy, Energy} = stats:get(energy, Stats),
    NewEnergy = min(10, Energy + world:get_food(self(), World)),
    NewStats = stats:set(energy, NewEnergy, Stats),
    NewStats.

temperature_loss(Stats, WT) ->
    {temperature, T} = stats:get(temperature, Stats),
    abs(T-WT).

fight(Stats, Opponent) ->
    {energy, Energy} = stats:get(energy, Stats),
    {attack, Attack} = stats:get(attack, Stats),
    Power = random(0, trunc(Attack)),
    case platypus:attack(Opponent, Power) of
	{win, Food} ->
	    NewEnergy = Energy + Food;
	{lose, Injury} ->
	    NewEnergy = Energy - Injury;
	false ->
	    NewEnergy = Energy
    end,
    if NewEnergy > 0 ->
	    stats:set(energy, NewEnergy, Stats);
       true ->
	    stats:set(alive, false, Stats)
    end.
	

random(Min, Max) when Max >= Min ->
    Random = random:uniform(Max - Min + 1),
    Random + Min - 1.

%% Callbacks %%

init(State) ->
    random:seed(now()),
    {ok, State}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast({step, Prey, Temperature}, {Habitat, Stats, World}) ->

    Run = lists:zip(lists:seq(1, 10), Prey),
    Stats2 = lists:foldr(fun ({_, Opponent}, Acc) -> act(Acc, Habitat, World, Opponent) end,
			 Stats,
			 Run),

    {energy, Energy} = stats:get(energy, Stats2),
    {age, Age} = stats:get(age, Stats2),
    {maxage, MaxAge} = stats:get(maxage, Stats2),
    {defence, Def} = stats:get(defence, Stats2),
    {attack, Atk} = stats:get(attack, Stats2),
    {alive, Alive} = stats:get(alive, Stats2),
    
    NewEnergy = Energy -
	(Def + Atk) / 10 -
	temperature_loss(Stats2, Temperature) - 1,
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
	
	Alive == false ->
	    habitat:remove_animal(self(), Habitat),
    	    Status = {stop, normal, {Habitat, NewStats, World}};
	
    	true ->
    	    Status = {noreply, {Habitat, NewStats, World}}
    end,
    Habitat ! done,
    Status;

handle_cast({fight, From, Power}, S = {Habitat, Stats, World}) ->
    {defence, Defence} = stats:get(defence, Stats),
    Protection = random(0, trunc(Defence)),
    case Power - Protection of
	N when N > 0 -> % I lost
	    {energy, Energy} = stats:get(energy, Stats),
	    From ! {win, self(), Energy},
	    NewS=stats:set([{alive, false}, {energy, 0}], Stats);
	    
	N when N =< 0 -> % I won
	    From ! {lose, self(), -N},
	    NewS=Stats
    end,
    {noreply, {Habitat, NewS, World}}.



handle_call(get_stats, _From, S = {_Habitat, Stats, _World}) ->
    {reply, Stats, S}.


handle_info({win, _, _}, S) ->
    {noreply, S};
handle_info({lose, _, _}, S) ->
    {noreply, S}.
