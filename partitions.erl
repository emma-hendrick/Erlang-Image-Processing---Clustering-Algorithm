-module(partitions).
-export([point_index/2, partitions_init/2, partitions_within_radius/4, points_from_partitions/2]).


%% Get the index of a point, given the point, and the number of partitions
point_index(Point, Part_count) ->
    {X, Y, Z} = Point,
    X_part = floor((X / 256) * Part_count),
    Y_part = floor((Y / 256) * Part_count),
    Z_part = floor((Z / 256) * Part_count),
    (Z_part * Part_count * Part_count) + (Y_part * Part_count) + X_part.


%% Get the partitions within a radius from a specific point
partitions_within_radius(Point, Radius, Part_count, Partitions) ->
    Partition_keys = dict:fetch_keys(Partitions),
    lists:filter(
        fun(Partition_key) ->
            point_math:point_distance(Point, closest_vertex(Point, Part_count, Partition_key)) < Radius
            end, 
    Partition_keys).


%% Get the partitions within a radius from a specific point
points_from_partitions(Partitions, Dict) ->
    lists:foldl(
        fun(Key, Accum) -> 
            lists:merge(Accum, dict:fetch(Key, Dict)) 
        end, 
    [], Partitions).


%% Get the closest vertex of a partitions cube to a point
closest_vertex(Point, Part_count, Partition_key) ->
    {X, Y, Z} = Point,
    X_part = Partition_key rem Part_count,
    Y_part = ((Partition_key - X_part) div Part_count) rem Part_count,
    Z_part = ((((Partition_key - X_part) div Part_count) - Y_part) div Part_count) rem Part_count,
    Center_offset = 256 / (Part_count * 2),
    Center_x_part = (X_part * 256 / Part_count) + Center_offset,
    Center_y_part = (Y_part * 256 / Part_count) + Center_offset,
    Center_z_part = (Z_part * 256 / Part_count) + Center_offset,
    Close_x = case Center_x_part < X of true -> Center_x_part + Center_offset; false -> Center_x_part - Center_offset end,
    Close_y = case Center_y_part < Y of true -> Center_y_part + Center_offset; false -> Center_y_part - Center_offset end,
    Close_z = case Center_z_part < Z of true -> Center_z_part + Center_offset; false -> Center_z_part - Center_offset end,
    {Close_x, Close_y, Close_z}.


%% Initialize partitions
partitions_init(Points, Part_count) ->
    Dict = dict:new(),
    partitions_init_recurse(Points, Part_count, Dict).


%% Partitions Initialization Recursor
partitions_init_recurse([], _, Dict) -> Dict;
partitions_init_recurse([H | T], Part_count, Dict) ->
    New_dict = dict:append(point_index(H, Part_count), H, Dict),
    partitions_init_recurse(T, Part_count, New_dict).