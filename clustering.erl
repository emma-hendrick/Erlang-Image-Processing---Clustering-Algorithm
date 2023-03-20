-module(clustering).
-export([test/0, run/0]).
-import(debugging, [debug_log/2]).


%% Sampling Constants
-define(SAMPLE_COUNT, 400).


%% Partitioning Constant
-define(PARTITIONING_CONSTANT, 4).


%% Distance thresholds
-define(MIN_THRESHOLD, 15).
-define(MAX_THRESHOLD, 45).


%% Threshold Grading Constants
-define(INVERSE_THRESHOLD_GRADING_CONSTANT, 10).


%% Brightness Constants, the threshold will cut off all values below that brightness
-define(BRIGHTNESS_GRADING_CONSTANT, 1/2).
-define(BRIGHTNESS_THRESHOLD, 40).


%% Clustering Constants
-define(MAX_CLUSTERS_TO_KEEP, 3).


%% Testing
test() ->
    analyze_points(image_parser:sample_image("toucan.png", ?SAMPLE_COUNT)).


%% Testing
run() ->
    analyze_points(image_parser:sample_image("image.jpg", ?SAMPLE_COUNT)).


%% Entry Point for the clustering algorithm
analyze_points(Points) ->

    %% Remove points that are too dark for the LEDs, and create the starting partitioned points and clusters
    Removed_dark_points = remove_dark(Points),
    Partitioned_points = partitions:partitions_init(Removed_dark_points, ?PARTITIONING_CONSTANT),
    Partitioned_clusters = generate_clusters(Partitioned_points),

    %% For each cluster, check all other clusters within the threshold to find the perfect threshold
    Refined_clusters = brute_force_refine_clusters(Partitioned_clusters, Partitioned_points),

    %% Sort them, take the first ?MAX_CLUSTERS_TO_KEEP unique clusters, and return them
    Sorted_clusters = sort_clusters(Refined_clusters),
    Best_clusters = pick_best_clusters(Sorted_clusters),
    csv_output(Best_clusters),
    json:clusters_to_json(Best_clusters).


%% Remove all pixels below a certain brightness
remove_dark(Points) ->
    lists:filter(
        fun(Point) ->
            point_math:point_distance(Point, {0, 0, 0}) > ?BRIGHTNESS_THRESHOLD
            end, 
    Points).


%% Return only points within the threshold
point_threshold(Points, Center, Threshold) ->
    lists:filter(
        fun(Point) ->
            point_math:point_distance(Point, Center) < Threshold
            end, 
    Points).


%% A cluster will be in the form of
% Point: {X, Y, Z}, this is the point which represents the clusters center
% Score: This is the clusters current score
% Threshold: {Min, Current, Max}, this is the current threshold range for the cluster, used to hone the perfect distance
cluster(Point, Score, Threshold) ->
    {Point, Score, Threshold}.


% To make a cluster score multiplier based on the brightness
graded_center_distance(Point) ->
    Dist = point_math:point_distance(Point, {0, 0, 0}),
    Axis_scale = point_math:point_distance({0, 0, 0}, {255, 255, 255}),
    Normalized_dist = Dist / Axis_scale,
    math:pow(Normalized_dist, ?BRIGHTNESS_GRADING_CONSTANT).


%% Assign a score to a cluster
score_cluster(Points, Center, Threshold) -> 
    Points_in_threshold = point_threshold(Points, Center, Threshold),
    Cluster_score_multiplier = graded_center_distance(Center),
    Total_score = lists:foldl(
        fun(Point, Acc) ->
            point_math:point_similarity(Point, Center) + Acc
        end,
        0,
        Points_in_threshold),
    round((Total_score * Cluster_score_multiplier * 1000000) / math:pow(Threshold, 1 / ?INVERSE_THRESHOLD_GRADING_CONSTANT)).


%% Create a list of clusters given the data points
generate_clusters(Partitions) ->
    dict:map(fun(_Key, Points) ->

        lists:map(fun(Point) -> 

            Partitions_within_range = partitions:partitions_within_radius(Point, ?MIN_THRESHOLD, ?PARTITIONING_CONSTANT, Partitions),
            Points_within_range = partitions:points_from_partitions(Partitions_within_range, Partitions),
            cluster(
                Point,
                score_cluster(Points_within_range, Point, ?MIN_THRESHOLD),
                ?MIN_THRESHOLD
            ) end, Points
        ) end, Partitions
    ).


%% Calculate the average of an array of points
calc_average_point(Points) -> 
    Point_count = length(Points),
    Total_point = lists:foldl(
        fun(Point, Acc) ->
            {Point_x, Point_y, Point_z} = Point,
            {Total_x, Total_y, Total_z} = Acc,
            {
                Point_x + Total_x,
                Point_y + Total_y,
                Point_z + Total_z
            }
            end, 
        {0, 0, 0}, 
        Points),
    {X, Y, Z} = Total_point,
    {
        X / Point_count,
        Y / Point_count,
        Z / Point_count
    }.


%% A brute force cluster refinement technique
brute_force_refine_clusters(Partitioned_clusters, Partitioned_points) ->
    dict:fold(fun(_Key, Clusters, Accum) ->

        %% Refine each cluster
        Refined_cluster = lists:map(fun(Cluster) -> 

            {Center, _Score, _Threshold} = Cluster,
            Partitions_within_max_range = partitions:partitions_within_radius(Center, ?MAX_THRESHOLD, ?PARTITIONING_CONSTANT, Partitioned_points),
            Points_within_max_range = partitions:points_from_partitions(Partitions_within_max_range, Partitioned_points),

            Point_tuples = lists:map(fun(Point) -> 
                Dist = point_math:point_distance(Center, Point),
                
                Clamped_dist = case ((Dist < ?MAX_THRESHOLD) and (Dist > ?MIN_THRESHOLD)) of
                    true ->
                        Dist;
                    false ->
                        0
                    end,

                Partitions_within_range = partitions:partitions_within_radius(Center, ?MAX_THRESHOLD, ?PARTITIONING_CONSTANT, Partitioned_points),
                Points_within_range = partitions:points_from_partitions(Partitions_within_range, Partitioned_points),

                Score = 
                    case Clamped_dist == 0 of
                        true -> 0;
                        false -> score_cluster(Points_within_range, Center, Clamped_dist)
                    end,
                {Score, Clamped_dist}
            end, Points_within_max_range),

            case length(Point_tuples) == 0 of
                true -> 
                    cluster(Center, 0, 0);

                false -> 

                    Best_point_tuple = lists:max(Point_tuples),
                    {Tuple_score, Threshold} = Best_point_tuple,

                    case (Threshold == 0) or (Tuple_score == 0) of
                        true ->
                            cluster(Center, 0, 0);

                        false ->

                            Relevant_points = point_threshold(Points_within_max_range, Center, Threshold),
                            Updated_cluster_center = point_math:round_point(calc_average_point(Relevant_points)),
                            Final_score = score_cluster(Relevant_points, Updated_cluster_center, Threshold),

                            cluster(Updated_cluster_center, Final_score, Threshold)
                        end
                end

            end, Clusters),
        
        lists:merge(Accum, Refined_cluster)

        end, [], Partitioned_clusters).


%% Sort clusters by score
sort_clusters(Clusters) ->
    lists:sort(
        fun(A, B) -> 
            {_A_point, A_score, _A_threshold} = A,
            {_B_point, B_score, _B_threshold} = B,
            A_score > B_score
            end, 
    Clusters).


%% Check whether a point falls within a cluster's threshold
is_too_close(Cluster_a, Cluster_b) ->
    {Cluster_center_a, _Score_a, Threshold_a} = Cluster_a,
    {Cluster_center_b, _Score_b, Threshold_b} = Cluster_b,
    point_math:point_distance(Cluster_center_a, Cluster_center_b) < point_math:calc_average(Threshold_a, Threshold_b).


%% Check whether a point it too close to any clusters within a list
too_close_to_any(Protagonist_cluster, Clusters_to_check) ->
    Clusters_too_close = lists:filter(
        fun(Cluster) ->
            is_too_close(Protagonist_cluster, Cluster)
        end,
        Clusters_to_check),
    length(Clusters_too_close) /= 0.


%% Choose the best clusters from the current set, and eliminate any duplicates
pick_best_clusters([], Clusters_kept) ->
    Clusters_kept;

pick_best_clusters([Cluster | Remaining], Clusters_kept) when (length(Clusters_kept) < ?MAX_CLUSTERS_TO_KEEP) ->

    %% If we already have an exact copy of that cluster, discard it
    %% If that cluster is too similar to another in terms of distance, discard it
    case (lists:member(Cluster, Clusters_kept) or too_close_to_any(Cluster, Clusters_kept)) of
        true ->
            pick_best_clusters(Remaining, Clusters_kept);
        false ->
            pick_best_clusters(Remaining, [Cluster|Clusters_kept])
            end;

% Once you've picked three return them. They need to be sorted by score for this to work
pick_best_clusters(_Discarded_clusters, Clusters_kept) ->
    Clusters_kept.

%% A nice facade to make it slightly easier to use, helps if we ever need to refactor
pick_best_clusters(Clusters) ->
    pick_best_clusters(Clusters, []).


%% Output the chosen clusters into a CSV file
csv_output(Clusters) ->
    file:write_file("clusters.csv", "", [write]),
    lists:map(fun(Cluster) -> 
        {Point, Score, Threshold} = Cluster,
        {X, Y, Z} = Point,
        Csv_line = io_lib:format("~p, ~p, ~p, ~p, ~p\n", [X, Y, Z, Score, Threshold]),
        file:write_file("clusters.csv", Csv_line, [append])
    end, Clusters).
    
