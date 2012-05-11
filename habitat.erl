-module(habitat).
-export([start/1, create_animal/1, list/1, step/1]).
-export([init/1, handle_cast/2, handle_call/3]).
-behavior(gen_server).

start(N) ->
    {ok, Pid} = gen_server:start(?MODULE, N, []),
    Pid.

create_animal(Name) ->
    gen_server:cast(Name, create).

list(Name) ->
    gen_server:call(Name, list).

step(Name) ->
    gen_server:cast(Name, step).


%% Callbacks %%

init(N) ->
    Animals = [ platypus:start() || _ <- lists:seq(1, N) ],
    {ok, Animals}.

handle_cast(step, Animals) ->
    [ platypus:step(Name) || Name <- Animals ],
    {noreply, Animals};

handle_cast(create, Animals) ->
    NewAnimals = [ platypus:start() | Animals ],
    {noreply, NewAnimals}.

handle_call(list, _From, Animals) ->
    Reply = [ platypus:get_stats(Name) || Name <- Animals ],
    {reply, Reply, Animals}.
