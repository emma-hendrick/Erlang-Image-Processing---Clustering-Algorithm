-module(point_math).
-export([point_distance/2, point_similarity/2, scale_point_to_dist_circle/2, calc_average/2, round_point/1]).


%% Find the difference from one three dimensional point to another
point_distance(A, B) -> 
    {Ax, Ay, Az} = A,
    {Bx, By, Bz} = B,
    math:sqrt(
        math:pow(Ax - Bx, 2) + 
        math:pow(Ay - By, 2) + 
        math:pow(Az - Bz, 2)
    ).


%% Calculate a similarity value for two colors
point_similarity(A, B) ->
    1 / (1 + point_distance(A, B)).


%% Scale a point to the unit circle, then scale it to dist
scale_point_to_dist_circle(Point, Distance) ->
    Distance_to_center = point_distance(Point, {0, 0, 0}),
    {X, Y, Z} = Point,
    {
        (X * Distance) / Distance_to_center,
        (Y * Distance) / Distance_to_center,
        (Z * Distance) / Distance_to_center
    }.


%% Calculate the average of two values
calc_average(A, B) -> (A + B) / 2.


%% Calculate average point
round_point(Point) ->
    {X, Y, Z} = Point,
    {
        round(X),
        round(Y),
        round(Z)
    }.