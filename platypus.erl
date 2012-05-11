-module(platypus).
-export([start/0, step/1, get_stats/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-behavior(gen_server).


%% API %%

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, stats:new(), []),
    Pid.

step(Name) ->
    gen_server:cast(Name, step).

get_stats(Name) ->
    gen_server:call(Name, get_stats).


%% Callbacks %%

init(Stats) ->
    {ok, Stats}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(step, Stats) ->
    {energy, Energy} = stats:get(energy, Stats, 10),
    NewEnergy = Energy - 1,
    if
    	NewEnergy == 0 ->
    	    {stop, normal, Stats};
    	true ->
    	    NewStats = stats:set(energy, NewEnergy, Stats),
    	    {noreply, NewStats}
    end.

handle_call(get_stats, _From, Stats) ->
    {reply, Stats, Stats}.

