-module(processes).
-export([start/1, stop/1, rpc/3, do/0]).


%% Registry function
start(Name) ->
    register(Name, spawn(cluster, do, [])).


%% Kill process
stop(Name) ->
    rpc(Name, stop, stop).


%% Process Server Function
do() ->
    receive
        {Pid, div_by, {Number, Divisors}} -> 
            Pid ! lists:any(fun (Divisor) -> Number rem Divisor == 0 end, Divisors);
        {stop, stop} -> exit(normal);
        {Pid, _other} -> Pid ! "oops"
    end,
    do().


%% Process Client Function
rpc(Pid, Command, Data) ->
    Pid ! {self(), Command, Data},
    receive 
        Resp -> 
            Resp end.


