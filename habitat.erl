-module(habitat).
-export([start/1, create_animal/1, get_food/2, list/1, step/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).

start(N) ->
    {ok, Pid} = gen_server:start(?MODULE, N, []),
    Pid.

create_animal(Name) ->
    gen_server:cast(Name, {create, Name}).

get_food(Animal, Name) ->
    gen_server:call(Name, {get_food, Animal}).

list(Name) ->
    gen_server:call(Name, list).

step(Name) ->
    gen_server:cast(Name, step).


%% Callbacks %%

init(N) ->
    process_flag(trap_exit, true),
    World = world:start(),
    Animals = [ platypus:start(self()) || _ <- lists:seq(1, N) ],
    {ok, {World, Animals}}.

handle_cast(step, S = {World, Animals}) ->
    [ platypus:step(Name) || Name <- Animals ],
    world:step(World),
    {noreply, S};

handle_cast({create, Habitat}, {World, Animals}) ->
    NewAnimals = [ platypus:start(Habitat) | Animals ],
    {noreply, {World, NewAnimals}}.

handle_call(list, _From, S = {_World, Animals}) ->
    Reply = [ platypus:get_stats(Name) || Name <- Animals ],
    {reply, Reply, S};

handle_call({get_food, Animal}, _From, S = {World, _Animals}) ->
    {reply, world:get_food(Animal, World), S}.


handle_info({'EXIT', Pid, _Reason}, {World, Animals}) ->
    NewAnimals = lists:filter(fun (Pid2) -> Pid =/= Pid2 end, Animals),
    {noreply, {World, NewAnimals}}.
