%% @copyright Kopimi
-module(habitat).
-export([start/1, create_animal/1, create_animal/2, remove_animal/2, random_animal/1,
	 list/1, world/1, step/1, step/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).

%% @doc
%% Startar en simulation med N individer.
%%
%% Returnerar pid till skapad simulation
%%
%% @spec start(integer()) -> pid()
start(N) ->
    {ok, Pid} = gen_server:start(?MODULE, N, []),
    Pid.

%% @doc Skapar nytt djur i en befintlig simulation
create_animal(Name) ->
    gen_server:cast(Name, create).

%% @doc
%% Som create_animal/1 men med definierade egenskaper
%% i form av ett stats-objekt.
create_animal(Stats, Name) ->
    gen_server:cast(Name, {create, Stats}).

%% @doc Tar bort djuret Pid ur simulationen Name.
remove_animal(Pid, Name) ->
    gen_server:cast(Name, {remove, Pid}).


%% @doc
%% Välj slumpmÃ¤ssigt ett av djuren i habitet
random_animal(Name) ->
    gen_server:call(Name, random_animal).

%% @doc
%% Returnerar en lista med stats frÃ¥n djuren i habitatet Name
list(Name) ->
    gen_server:call(Name, list, infinity).

%% @doc Returnerar stats från simulationens värld
world(Name) ->
    gen_server:call(Name, world, infinity).

%% @doc Kör simulationen ett steg.
step(Name) ->
    gen_server:call(Name, step, infinity).

%% @doc Kör simulationen N steg.
step(Name, N) ->
    [step(Name) || _ <- lists:seq(1,N)].

%% Internal functions %%

%% @doc
%% Invänta N stycken done-meddelanden.
%% Dessa skickas från simulationens djur när de är klara med varje steg.
wait(N) when N =< 0 ->
    ok;
wait(N) ->
    receive
	done ->
	    wait(N-1)
    end.

%% @doc Välj ett slumpmässigt elemnt ur en lista
random_element(L, Not) ->
    E = lists:nth(random:uniform(length(L)), L),
    case E of
	Not ->
	    random_element(L, Not);
	_ ->
	    E
    end.

%% Callbacks %%

init(N) ->
    process_flag(trap_exit, true),
    random:seed(now()),
    World = world:start(),
    Animals = [ platypus:start(self(), World) || _ <- lists:seq(1, N) ],
    {ok, {World, Animals}}.

handle_cast(create, {World, Animals}) ->
    NewAnimals = [ platypus:start(self(), World) | Animals ],
    {noreply, {World, NewAnimals}};

handle_cast({create, Stats}, {World, Animals}) ->
    NewAnimals = [ platypus:start(Stats, self(), World) | Animals ],
    {noreply, {World, NewAnimals}};

handle_cast({remove, Pid}, {World, Animals}) ->
    NewAnimals = [ P || P <- Animals,
			P =/= Pid],
    {noreply, {World, NewAnimals}}.

handle_call(step, _From, S = {World, Animals}) ->
    world:step(World),
    Temperature = world:get_temperature(World),
    [ platypus:step(Name, random_element(Animals, Name), Temperature) || Name <- Animals ],
    wait(length(Animals)), %% We don't need to wait for world
    {reply, ok, S};    

handle_call(random_animal, _From, S = {_World, Animals}) ->
    {reply, lists:nth(random:uniform(length(Animals)), Animals), S};

handle_call(list, _From, S = {_World, Animals}) ->
    %%Alive = lists:filter(fun erlang:is_process_alive/1, Animals),
    Reply = [ platypus:get_stats(Name) || Name <- Animals,
					  is_process_alive(Name) ],
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
