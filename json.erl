-module(json).
-export([clusters_to_json/1]).


%% serialize JSON from a list of tuples using pattern matching and recursion!
% If there are no other elements, don't add an ending comma or recurse, add a closing bracket
serialize([H|[]], Accum) ->
    {Key, Value} = H,
    Accum ++ "\"" ++ Key ++ "\": " ++ Value ++ "}";
% If this is the first element, add the opening bracket
serialize(List, "") ->
    serialize(List, "{");
% Otherwise, destructure the inner elements and add them to the JSON string
serialize([H|T], Accum) ->
    {Key, Value} = H,
    serialize(T, Accum ++ "\"" ++ Key ++ "\": " ++ Value ++ ",").

% JSON Generator Facade
serialize(List) ->
    serialize(List, "").


serialize_list(List_items) ->
    "[" ++ 
        lists:map(fun(Item) ->
        
            Item ++ ","
        
        end, List_items) ++ 
    "]".

%% Convert clusters to JSON
clusters_to_json(Clusters) -> 
    Pixels = lists:map(fun(Cluster) ->
        {Point, Score, _Threshold} = Cluster,
        {X, Y, Z} = Point,
        
        serialize([{"R", integer_to_list(X)},
                {"G", integer_to_list(Y)},
                {"B", integer_to_list(Z)},
                {"Score", integer_to_list(Score)}])
        
        end, Clusters),
    "{clusters: " ++ serialize_list(Pixels) ++ "}".


