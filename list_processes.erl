-module(list_processes).
-export([pmap/2]).


%%
pmap(_Function, []) ->
    [];
pmap(Function, List) -> 
    Parent = self(),
    Pids = lists:map(fun(Element) ->
        spawn(fun() -> execute(Parent, Function, Element)
            end)
        end, List),
    gather(Pids).


%%
execute(Pid, Function, Element) ->
    Pid ! {self(), Function(Element)}.


%%
gather([]) ->
    [];
gather([H|T]) ->
    receive
        {H, Return} ->
            [Return | gather(T)]
    end.