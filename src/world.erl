%% @copyright Kopimi

%% @doc Håller reda på ett antal egenskaper om simuleringens värld, såsom temperatur och matmängd. Skapar ny mat.
%% Världen är en egen process.
-module(world).
-export([start/0, start/2, get_food/2, get_temperature/1, step/1, list/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-behavior(gen_server).

%% @doc Skapar en värld med några standardattribut.
start() ->
    start(2000, 20).

%% @doc Skapa en värld med mattillväxten FoodGrowth och temperaturen Temperature.
start(FoodGrowth, Temperature) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {FoodGrowth, Temperature}, []),
    Pid.

%% @doc Kör ett steg i simulationen. Skapar ny mat och varierar eventuellt temperaturen.
step(World) ->
    gen_server:cast(World, step).

%% @doc Returnerar världens {@link stats}-objekt med egenskaper.
list(World) ->
    gen_server:call(World, list, infinity).

%% @doc Ät en enhet mat. Anropas av {@link platypus}-processer.
%% Returnerar den mängd mat djuret får (0 eller 1).
get_food(Animal, World) ->
    gen_server:call(World, {get_food, Animal}, infinity).

%% @doc Nuvarande temperatur
get_temperature(World) ->
    gen_server:call(World, get_temperature, infinity).

%% @doc Slumässigt heltal mellan Min och Max (inklusive)
random(Min, Max) ->
    Random = random:uniform(Max - Min + 1),
    Random + Min - 1.


% Callbacks %

%% @private
init({FoodGrowth, Temperature}) ->
    random:seed(now()),
    Stats = stats:set([{food_growth, FoodGrowth},
		       {food, FoodGrowth},
		       {temperature, Temperature}
		      ], stats:new()),
    {ok, Stats}.

%% @private
handle_cast(step, Stats) ->
    {food_growth, Growth} = stats:get(food_growth, Stats),
    {food, Food} = stats:get(food, Stats),
    {temperature, T} = stats:get(temperature, Stats),
    NewTemp = T + random(-1,1) + (20-T)/40,
    NewFood = Food + Growth,
    NewStats = stats:set(temperature, NewTemp, Stats),
    NewStats2 = stats:set(food, NewFood, NewStats),
    {noreply, NewStats2}.
    
%% @private
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
