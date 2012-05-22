-module(ui).
-export([start/0]).

-record(statistics, {
	  animals,
	  get_food_mean,
	  get_food_dev,
	  reprod_mean,
	  reprod_dev,
	  energy_mean,
	  energy_dev,
	  age_mean,
	  age_median,
	  age_dev
	 }).


start() ->
    Animals = element(2,io:read("Number of animals: ")),
    PidForHabitat = habitat:start(Animals),
    io:format("Type 'help' for command list.\n"),
    looper(PidForHabitat),
    ok.


looper(Pid) ->
    Message = "Command: ",
    case io:get_line(Message) of
	"help\n" ->
	    io:format("step:\t(enter)\nmany steps:\t(number + enter)\nquit:\tquit\n"),
	    looper(Pid);
	"quit\n" ->
	    ok;
        "\n" ->
	    habitat:step(Pid),%% skicka meddelande till habitatet om ett step
	    printStatistics(Pid),
	    looper(Pid);
	X ->
  	    makeManySteps(element(1,string:to_integer(X)), Pid),
	    printStatistics(Pid),
  	    looper(Pid)
    end.


makeManySteps(N, Pid) when N > 0 ->
    habitat:step(Pid, N).


printStatistics(Pid) ->
    S = getStatistics(Pid),
    {food, Food} = stats:get(food, habitat:world(Pid)),
    io:format("~nWorld food:\t~p~n"
	      "Count:\t~p~n"
	      "Get food:\t~.2f % (deviation: ~.1f)~n"
	      "Reproduce:\t~.2f % (deviation: ~.1f)~n"
	      "Energy:\t~.2f (deviation: ~.1f)~n"
	      "Age:\t~.2f (deviation: ~.1f)~n"
	      , [Food, S#statistics.animals,
		 S#statistics.get_food_mean, S#statistics.get_food_dev,
		 S#statistics.reprod_mean, S#statistics.reprod_dev,
		 S#statistics.energy_mean, S#statistics.energy_dev,
		 S#statistics.age_mean, S#statistics.age_dev
		]
	     ).


getStatistics(Pid) ->
    L = habitat:list(Pid),
    Len = length(L),
    {FoodMean, FoodDev} = statistics:meanAndDev(stats(get_food, L)),
    {ReMean, ReDev} = statistics:meanAndDev(stats(reproduce, L)),
    {EnergyMean, EnergyDev} = statistics:meanAndDev(stats(energy, L)),
    {AgeMean, AgeDev} = statistics:meanAndDev(stats(age, L)),
    #statistics{animals       = Len,
		get_food_mean = FoodMean,
		get_food_dev  = FoodDev,
		reprod_mean   = ReMean, 
		reprod_dev    = ReDev,
		energy_mean   = EnergyMean,
		energy_dev    = EnergyDev,
		age_mean      = AgeMean,
		age_median    = null,
		age_dev       = AgeDev
	       }.

stats(get_food, L) ->
    lists:map(fun(E) -> platypus:get_action(get_food, E) end, stats(actions, L));
stats(reproduce, L) ->
    lists:map(fun(E) -> platypus:get_action(reproduce, E) end, stats(actions, L));
stats(Key, L) ->
    lists:map(fun(E) -> {_, R} = stats:get(Key, E), R end, L).

