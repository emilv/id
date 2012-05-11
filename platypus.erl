-module(platypus).
-export([init/1, handle_cast/2, handle_call/3]).


init(Stats) ->
    {ok, Stats}.

handle_cast(step, Stats) ->
    NewEnergy = stats:get(energy, Stats, 10) - 1,
    if
	NewEnergy == 0 ->
	    {stop, energy, Stats};
	true ->
	    NewStats = stats:set(energy, NewEnergy, Stats),
	    {noreply, NewStats}
    end.

handle_call(get_stats, _From, Stats) ->
    {reply, Stats, Stats}.
