-module(world).
-export([start/0, get_food/2, get_temperature/1, step/1, list/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-behavior(gen_server).

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

step(World) ->
    gen_server:cast(World, step).

list(World) ->
    gen_server:call(World, list, infinity).

get_food(Animal, World) ->
    gen_server:call(World, {get_food, Animal}, infinity).

get_temperature(World) ->
    gen_server:call(World, get_temperature, infinity).

random(Min, Max) ->
    Random = random:uniform(Max - Min + 1),
    Random + Min - 1.


% Callbacks %

init(_) ->
    random:seed(now()),
    Stats = stats:set([{max_food, 10000},
		       {food_growth, 2000},
		       {food, 500},
		       {temperature, 20}
		      ], stats:new()),
    {ok, Stats}.

handle_cast(step, Stats) ->
    {food_growth, Growth} = stats:get(food_growth, Stats),
    {food, Food} = stats:get(food, Stats),
    {max_food, MaxFood} = stats:get(max_food, Stats),
    {temperature, T} = stats:get(temperature, Stats),
    NewTemp = T + random(-1,1)*platypus:extreme() + (20-T)/40,
    NewFood = min(MaxFood, Food + Growth),
    NewStats = stats:set(temperature, NewTemp, Stats),
    NewStats2 = stats:set(food, NewFood, NewStats),
    {noreply, NewStats2}.
    
handle_call(list, _From, Stats) ->
    {reply, Stats, Stats};

handle_call(get_temperature, _From, Stats) ->
    {temperature, Temp} = stats:get(temperature, Stats),
    {reply, Temp, Stats};

handle_call({get_food, _Animal}, _From, Stats) ->
    case stats:get(food, Stats) of
	{food, S} when S > 0 ->
	    NewStats = stats:set(food, S-1, Stats),
	    {reply, 1, NewStats};
	_ ->
	    {reply, 0, Stats}
    end.
