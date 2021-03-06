%% @copyright Kopimi

%% @doc Användargränssnitt för att köra en evolutionssimulering.
-module(ui).
-export([start/0]).

-record(statistics, {
	  animals,
	  food,
	  food_growth,
	  temperature,
	  get_food_mean,
	  get_food_bar,
	  reprod_mean,
	  reprod_bar,
	  fight_mean,
	  fight_bar,
	  attack_mean,
	  attack_bar,
	  defence_mean,
	  defence_bar,
	  energy_mean,
	  energy_bar,
	  age_mean,
	  age_bar,
	  maxage_mean,
	  maxage_bar,
	  temp_mean,
	  temp_bar
	 }).


%% @doc Kör en simulering. Frågar efter kommandon via standard-ut/standard-in.
%% Returnerar <em>ok</em> när simulationen avslutats.
start() ->
    Animals = readInt("Number of animals: "),
    FoodGrowth = readInt("Food growth (recommended 2000): "),
    Temperature = readInt("Temperature: "),
    PidForHabitat = habitat:start(Animals, FoodGrowth, Temperature),
    io:format("Type 'help' for command list.\n"),
    looper(PidForHabitat, 0).

%% @private
looper(Pid, Steps) ->
    Message = "Command: ",
    case io:get_line(Message) of
	"help\n" ->
	    io:format("add animals: add\n"
		      "change environment: set\n"
		      "step:        (enter)\n"
		      "many steps:  (number + enter)\n"
		      "quit:        quit\n"),
	    looper(Pid, Steps);
	"quit\n" ->
	    ok;
	"add\n" ->
	    add(Pid),
	    printStatistics(Pid, Steps),
	    looper(Pid, Steps);
	"set\n" ->
	    set(Pid),
	    looper(Pid, Steps);
        "\n" ->
	    habitat:step(Pid),%% skicka meddelande till habitatet om ett step
	    printStatistics(Pid, Steps+1),
	    looper(Pid, Steps+1);
	X ->
	    I = element(1,string:to_integer(X)),
  	    makeManySteps(I, Pid),
	    printStatistics(Pid, Steps+I),
  	    looper(Pid, Steps+I)
    end.

add(Habitat) ->
    GetF = readInt("Get food: "),
    Rep =  readInt("Reproduce: "),
    Fight  =  readInt("Fight: "),
    Def  =  readInt("Defence: "),
    Att  =  readInt("Attack: "),
    N  =  readInt("Number of animals: "),
    Stats = platypus:create_stat({GetF, Rep, Fight, Def, Att}),
    [habitat:create_animal(Stats, Habitat) || _ <- lists:seq(1,N)].

set(Habitat) ->
    Temperature = readInt("New temperature: "),
    FoodGrowth = readInt("New food growth rate: "),
    habitat:change(Habitat, Temperature, FoodGrowth).
  
readInt(Prompt) ->
    X = io:get_line(Prompt),
    element(1, string:to_integer(X)).

%% @private
makeManySteps(N, Pid) when N > 0 ->
    [ {io:format("*"),habitat:step(Pid)} || _ <-lists:seq(1,N)],
    io:format("\n").

%% @private
printStatistics(Pid, Steps) ->
    S = getStatistics(Pid),
    io:format("~n"
	      "Steps run:    ~p~n"
	      "WORLD STATS:~n"
	      "World food:   ~p~n"
	      "Food growth:  ~p~n"
	      "World temp:   ~p~n"
	      "Animal count: ~p~n"
	      "ANIMAL STATS:~n"
	      "Priorities:~n"
	      "Get food:     ~.2f %\t~s~n"
	      "Reproduce:    ~.2f %\t~s~n"
	      "Fight:        ~.2f %\t~s~n"
	      "Physical properties:~n"
	      "Attack:       ~.2f\t~s~n"
	      "Defence:      ~.2f\t~s~n"
	      "Temperature:  ~.1f\t~s~n"
	      "Max age:      ~.2f\t~s~n"
	      "Current status:~n"
	      "Age:          ~.2f\t~s~n"
	      "Energy:       ~.2f\t~s~n"
	      , [Steps,
		 S#statistics.food, S#statistics.food_growth, S#statistics.temperature,
		 S#statistics.animals,
		 S#statistics.get_food_mean, S#statistics.get_food_bar,
		 S#statistics.reprod_mean, S#statistics.reprod_bar,
		 S#statistics.fight_mean, S#statistics.fight_bar,
		 S#statistics.attack_mean, S#statistics.attack_bar,
		 S#statistics.defence_mean, S#statistics.defence_bar,
		 S#statistics.temp_mean, S#statistics.temp_bar,
		 S#statistics.maxage_mean, S#statistics.maxage_bar,
		 S#statistics.age_mean, S#statistics.age_bar,
		 S#statistics.energy_mean, S#statistics.energy_bar
		]
	     ).

%% @private
getStatistics(Pid) ->
    W = 30, % Bar width
    L = habitat:list(Pid),
    World = habitat:world(Pid),
    Len = length(L),
    {food, Food} = stats:get(food, World),
    {food_growth, Growth} = stats:get(food_growth, World),
    {temperature, Temperature} = stats:get(temperature, World),
    {FoodMean, FoodBar} = statistics:meanAndBar(stats(get_food, L), W, 0, 100),
    {ReMean, ReBar} = statistics:meanAndBar(stats(reproduce, L), W, 0, 100),
    {FightMean, FightBar} = statistics:meanAndBar(stats(fight, L), W, 0, 100),
    {AttackMean, AttackBar} = statistics:meanAndBar(stats(attack, L), W),
    {DefenceMean, DefenceBar} = statistics:meanAndBar(stats(defence, L), W),
    {EnergyMean, EnergyBar} = statistics:meanAndBar(stats(energy, L), W),
    {AgeMean, AgeBar} = statistics:meanAndBar(stats(age, L), W),
    {MaxAgeMean, MaxAgeBar} = statistics:meanAndBar(stats(maxage, L), W),
    {TempMean, TempBar} = statistics:meanAndBar(stats(temperature, L), W),
    #statistics{animals       = Len,
		food          = Food,
		food_growth   = Growth,
		temperature   = Temperature,
		get_food_mean = FoodMean,
		get_food_bar  = FoodBar,
		reprod_mean   = ReMean, 
		reprod_bar    = ReBar,
		fight_mean    = FightMean,
		fight_bar     = FightBar,
		attack_mean   = AttackMean,
		attack_bar    = AttackBar,
		defence_mean  = DefenceMean,
		defence_bar   = DefenceBar,
		energy_mean   = EnergyMean,
		energy_bar    = EnergyBar,
		age_mean      = AgeMean,
		age_bar       = AgeBar,
		maxage_mean   = MaxAgeMean,
		maxage_bar    = MaxAgeBar,
		temp_mean     = TempMean,
		temp_bar      = TempBar
	       }.
%% @private
stats(A, L) when A == get_food; A == reproduce; A == fight ->
    lists:map(fun(E) -> platypus:get_action(A, E) end, stats(actions, L));
stats(Key, L) ->
    lists:map(fun(E) -> {_, R} = stats:get(Key, E), R end, L).

