-module(env).
-export([initialize_env/0, get_var/2]).

initialize_env() ->
    {_Success, Raw_lines} = file:read_file(".env"),
    Lines = string:split(erlang:binary_to_list(Raw_lines), "\n", all),
    lists:map(fun(Line) -> 
        Key_values = string:split(Line, "="),
        {string:trim(lists:nth(1, Key_values)), string:trim(lists:nth(2, Key_values))}
        end, Lines).

get_var(Vars, Name) ->
    Var_tuple = lists:keyfind(Name, 1, Vars),
    {_Key, Value} = Var_tuple,
    Value.