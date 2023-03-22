-module(list_processes).
-export([pmap/2]).


%% Map values to a list in parallel
pmap(_Function, []) ->
    [];
pmap(Function, List) -> 
    Parent = self(),
    Pids = lists:map(fun(Element) ->
        spawn(fun() -> execute(Parent, Function, Element)
            end)
        end, List),
    gather(Pids).


%% Run a function on a spawned process
execute(Pid, Function, Element) ->
    Pid ! {self(), Function(Element)}.


%% Gather the results in order
gather([]) ->
    [];

gather([H|T]) ->
    receive
        {H, Return} ->
            [Return | gather(T)]
    end.