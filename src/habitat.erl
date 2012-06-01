%% @copyright Kopimi

%% @doc
%% Huvudmodulen för en simulation. Genom att köra {@link start/3} påbörjas en simulation.
%% En simulation består av en {@link world} med data om miljön (temperatur, mat, ...) samt
%% ett antal {@link platypus}-objekt (kallade <em>djur</em>). Det är här nya djur i simulationen
%% skapas och det är den här modulen som håller koll på alla levande djur.
%% @end
-module(habitat).
-export([start/3, create_animal/1, create_animal/2, remove_animal/2, random_animal/1,
	 list/1, world/1, step/1, step/2, change/3]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-behavior(gen_server).

%% @doc
%% Startar en simulation med N individer,
%% mattillväxten FoodGrowth samt temperaturen Temperature
%% Returnerar pid till skapad simulation
%%
%% @spec start(integer(), integer(), integer()) -> pid()
start(N, FoodGrowth, Temperature) ->
    {ok, Pid} = gen_server:start(?MODULE, {N, FoodGrowth, Temperature}, []),
    Pid.

%% @doc Skapar nytt djur i en befintlig simulation
create_animal(Name) ->
    gen_server:cast(Name, create).

%% @doc
%% Som {@link create_animal/1} men med definierade egenskaper
%% i form av ett stats-objekt.
create_animal(Stats, Name) ->
    gen_server:cast(Name, {create, Stats}).

%% @doc Tar bort djuret Pid ur simulationen Name.
remove_animal(Pid, Name) ->
    gen_server:cast(Name, {remove, Pid}).


%% @doc
%% Välj slumpmässigt ett av djuren i habitet
random_animal(Name) ->
    gen_server:call(Name, random_animal).

%% @doc
%% Returnerar en lista med stats från djuren i habitatet Name
list(Name) ->
    gen_server:call(Name, list, infinity).

%% @doc Returnerar stats från simulationens värld
world(Name) ->
    gen_server:call(Name, world, infinity).

%% @doc Ändra omgivningens temperatur och mattillväxt
change(Name, Temperature, FoodGrowth) ->
    gen_server:cast(Name, {change, Temperature, FoodGrowth}).

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

%% @doc Välj ett slumpmässigt element ur en lista
%%      Elementet får inte vara Not
random_element(L, Not) ->
    E = lists:nth(random:uniform(length(L)), L),
    case E of
	Not ->
	    random_element(L, Not);
	_ ->
	    E
    end.

%% Callbacks %%

%% @private
init({N, FoodGrowth, Temperature}) ->
    process_flag(trap_exit, true),
    random:seed(now()),
    World = world:start(FoodGrowth, Temperature),
    Animals = [ platypus:start(self(), World) || _ <- lists:seq(1, N) ],
    {ok, {World, Animals}}.

%% @private
handle_cast(create, {World, Animals}) ->
    NewAnimals = [ platypus:start(self(), World) | Animals ],
    {noreply, {World, NewAnimals}};

%% @private
handle_cast({create, Stats}, {World, Animals}) ->
    NewAnimals = [ platypus:start(Stats, self(), World) | Animals ],
    {noreply, {World, NewAnimals}};

handle_cast({remove, Pid}, {World, Animals}) ->
    NewAnimals = [ P || P <- Animals,
			P =/= Pid],
    {noreply, {World, NewAnimals}};

handle_cast({change, Temperature, FoodGrowth}, S = {World, _Animals}) ->
    world:change(World, Temperature, FoodGrowth),
    {noreply, S}.

%% @private
handle_call(step, _From, S = {World, Animals}) ->
    world:step(World),
    Temperature = world:get_temperature(World),
    Fights = permutate:get(Animals, 10, length(Animals)),
    Run = lists:zip(Animals, Fights),
    [ platypus:step(Name, Prey, Temperature) || {Name, Prey} <- Run ],
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

%% @private
handle_info({'EXIT', Pid, _Reason}, {World, Animals}) ->
    NewAnimals = lists:filter(fun (Pid2) -> Pid =/= Pid2 end, Animals),
    {noreply, {World, NewAnimals}}.
