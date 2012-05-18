-module(ui).
-export([start/0]).


start() ->
    AntalDjur = element(2,io:read("Ange antal djur: ")),
    PidTillHabitat = habitat:start(AntalDjur),
    looper(PidTillHabitat),
    ok.


looper(Pid) ->
    Message = "Commands: step(enter), many steps(number + enter), quit :\n",
    case io:get_line(Message) of
	"quit\n" ->
	    ok;
        "\n" ->
	    Pid:step(),%% skicka meddelande till habitatet om ett step
	    looper(Pid);
	"print\n" ->
	    ok;
          X ->
  	    makeManySteps(element(1,string:to_integer(X)), Pid),
  	    looper(Pid)
    end.


makeManySteps(0 , _) ->
    ok;
makeManySteps(N , Pid) when N > 0 ->
    Pid:step(), %% skicka meddelande till habitatet om ett step
    makeManySteps(N-1 , Pid).
	    
