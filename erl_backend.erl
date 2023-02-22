-module(erl_backend).
-export([init/0]).


%% Initialize the program
init() ->
    server:start(8081).


