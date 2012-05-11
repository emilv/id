-module(world).
-export([]).


init(PlatypusCount) ->
    process_flag(trap_exit, true),
    start_platypus(PlatypusCount).

start_platypus(0) -> [];
start_platypus(N) -> [platypus:start() | start_platypus(N-1)].
