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
    Animals = element(2,io:read("Ange antal djur: ")),
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
	    looper(Pid);
	"print\n" ->
	    printStatistics(Pid),
	    looper(Pid);
	X ->
  	    makeManySteps(element(1,string:to_integer(X)), Pid),
  	    looper(Pid)
    end.


makeManySteps(0 , _) ->
    ok;
makeManySteps(N , Pid) when N > 0 ->
    habitat:step(Pid), %% skicka meddelande till habitatet om ett step
    makeManySteps(N-1 , Pid).

printStatistics(Pid) ->
    S = getStatistics(Pid),
    io:format("Count:\t~p~n"
	      "Get food:\t~p %~n"
	      "Reproduce:\t~p %~n"
	      , [S#statistics.animals,
		 S#statistics.get_food_mean,
		 S#statistics.reprod_mean
		]
	     ).


getStatistics(Pid) ->
    L = habitat:list(Pid),
    Len = length(L),
    io:format("~p", [stats(get_food, L)]),
    {FoodMean, FoodDev} = statistics:meanAndDev(stats(get_food, L)),
    {ReMean, ReDev} = statistics:meanAndDev(stats(reproduce, L)),
    #statistics{animals       = Len,
		get_food_mean = FoodMean,
		get_food_dev  = FoodDev,
		reprod_mean   = ReMean, 
		reprod_dev    = ReDev,
		energy_mean   = null,
		energy_dev    = null,
		age_mean      = null,
		age_median    = null,
		age_dev       = null
	       }.

stats(get_food, L) ->
    lists:map(fun(E) -> platypus:get_action(get_food, E) end, stats(actions, L));
stats(reproduce, L) ->
    lists:map(fun(E) -> platypus:get_action(reproduce, E) end, stats(actions, L));
stats(Key, L) ->
    lists:map(fun(E) -> {_, R} = stats:get(Key, E), R end, L).

