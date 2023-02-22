-module(json).
-export([serialize/1]).


%% serialize JSON from a list of tuples using pattern matching and recursion!
% If there are no other elements, don't add an ending comma or recurse, add a closing bracket
serialize([H|[]], Accum) ->
    {Key, Value} = H,
    Accum ++ "\"" ++ Key ++ "\": \"" ++ Value ++ "\"}";
% If this is the first element, add the opening bracket
serialize(List, "") ->
    serialize(List, "{");
% Otherwise, destructure the inner elements and add them to the JSON string
serialize([H|T], Accum) ->
    {Key, Value} = H,
    serialize(T, Accum ++ "\"" ++ Key ++ "\": \"" ++ Value ++ "\",").

% JSON Generator Facade
serialize(List) ->
    serialize(List, "").


