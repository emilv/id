-module(habitat).
-export([start/1, create_animal/1, create_animal/2, get_food/2,
	 list/1, world/1, step/1, step/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).

start(N) ->
    {ok, Pid} = gen_server:start(?MODULE, N, []),
    Pid.

create_animal(Name) ->
    gen_server:cast(Name, create).

create_animal(Stats, Name) ->
    gen_server:cast(Name, {create, Stats}).

get_food(Animal, Name) ->
    gen_server:call(Name, {get_food, Animal}).

list(Name) ->
    gen_server:call(Name, list).

world(Name) ->
    gen_server:call(Name, world).

step(Name) ->
    gen_server:cast(Name, step).

step(Name, N) ->
    [step(Name) || _ <- lists:seq(1,N)].

%% Internal functions %%
wait(N) when N =< 0 ->
    ok;
wait(N) ->
    receive
	done ->
	    wait(N-1)
    end.

%% Callbacks %%

init(N) ->
    process_flag(trap_exit, true),
    World = world:start(),
    Animals = [ platypus:start(self(), World) || _ <- lists:seq(1, N) ],
    {ok, {World, Animals}}.

handle_cast(step, {World, Animals}) ->
    world:step(World),
    [ platypus:step(Name) || Name <- Animals ],
    wait(length(Animals)), %% We don't need to wait for world
    {noreply, {World, Animals}};

handle_cast(create, {World, Animals}) ->
    NewAnimals = [ platypus:start(self(), World) | Animals ],
    {noreply, {World, NewAnimals}};

handle_cast({create, Stats}, {World, Animals}) ->
    NewAnimals = [ platypus:start(Stats, self(), World) | Animals ],
    {noreply, {World, NewAnimals}}.

handle_call(list, _From, S = {_World, Animals}) ->
    Alive = lists:filter(fun erlang:is_process_alive/1, Animals),
    Reply = [ platypus:get_stats(Name) || Name <- Alive ],
    {reply, Reply, S};

handle_call(world, _From, S = {World, _Animals}) ->
    Reply = world:list(World),
    {reply, Reply, S}.

%handle_call({get_food, Animal}, _From, S = {World, _Animals}) ->
%    {stop, old_code, S}.
    %{reply, world:get_food(Animal, World), S}.

handle_info({'EXIT', Pid, _Reason}, {World, Animals}) ->
    NewAnimals = lists:filter(fun (Pid2) -> Pid =/= Pid2 end, Animals),
    {noreply, {World, NewAnimals}}.
