-module(clustering).
-export([analyze_points/1, test/0]).
-import(debugging, [debug_log/2]).


%% Grading Constants
-define(INVERSE_THRESHOLD_GRADING_CONSTANT, 8).


%% Clustering Constants
-define(MAX_CLUSTERS_TO_KEEP, 5).
-define(REFINEMENT_STEPS, 10).


%% Distance thresholds
-define(MAX_DISTANCE_THRESHOLD, 24).
-define(MIN_DISTANCE_THRESHOLD, 1).
-define(STARTING_DISTANCE_THRESHOLD, ((?MAX_DISTANCE_THRESHOLD + ?MIN_DISTANCE_THRESHOLD) / 2)).


%% Testing
test() ->
    analyze_points(image_parser:sample_image("paint.png", 1000)).


%% Entry Point for the clustering algorithm
analyze_points(Points) ->
    Clusters = generate_clusters(Points),
    Sorted_clusters = sort_clusters(Clusters),
    Refined_clusters = refine_clusters(Sorted_clusters, Points, ?REFINEMENT_STEPS),
    Best_clusters = pick_best_clusters(Refined_clusters),
    Sorted_best_clusters = sort_clusters(Best_clusters),
    Sorted_best_clusters.


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


%% Assign a score to a cluster
score_cluster(Points, Center, Threshold) -> 
    Points_in_threshold = point_threshold(Points, Center, Threshold),
    Total_score = lists:foldl(
        fun(Point, Acc) ->
            point_math:point_similarity(Point, Center) + Acc
        end,
        0,
        Points_in_threshold),
    Total_score / math:pow(Threshold, 1 / ?INVERSE_THRESHOLD_GRADING_CONSTANT).


%% Create a list of clusters given the data points
generate_clusters(Points) ->
    Default_threshold = {
        ?MIN_DISTANCE_THRESHOLD,
        ?STARTING_DISTANCE_THRESHOLD,
        ?MAX_DISTANCE_THRESHOLD
        },
    lists:map(fun(Point) -> 
        cluster(
            Point,
            score_cluster(Points, Point, ?STARTING_DISTANCE_THRESHOLD),
            Default_threshold
        )
        end, Points).


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

%% Attempt to find a central location, and perfect range for the cluster
refine_clusters(Clusters, _Points, 0) ->
    Clusters;
refine_clusters(Clusters, Points, Refinement_steps) ->
    Updated_clusters = lists:map(
        fun(Cluster) -> 

            %% Destructure the cluster and calculate the scores for the min and max thresholds
            {Cluster_center, Score, Threshold} = Cluster,
            {Min_threshold, Current_threshold, Max_threshold} = Threshold,
            Low_threshold_score = score_cluster(Points, Cluster_center, Min_threshold),
            High_threshold_score = score_cluster(Points, Cluster_center, Max_threshold),

            Updated_cluster_center = calc_average_point(point_threshold(Points, Cluster_center, Current_threshold)),
            
            %% Log the scores for debugging
            debugging:debug_log("DEBUG_CLUSTER_REFINEMENT_VALS", Low_threshold_score),
            debugging:debug_log("DEBUG_CLUSTER_REFINEMENT_VALS", Score),
            debugging:debug_log("DEBUG_CLUSTER_REFINEMENT_VALS", High_threshold_score),

            %% Update the cluster in order to move closer to the perfect score
            case (Score >= Low_threshold_score) and (Score >= High_threshold_score) of
                true -> 
                    debugging:debug_log("DEBUG_CLUSTER_REFINEMENT_CHOICE", "Chose to keep current threshold"),
                    cluster(Updated_cluster_center, Score, 
                        {
                            point_math:calc_average(Min_threshold, Current_threshold),
                            Current_threshold,
                            point_math:calc_average(Max_threshold, Current_threshold)
                        }
                    );
                false ->
                    case (Low_threshold_score >= High_threshold_score) of
                        true ->
                            debugging:debug_log("DEBUG_CLUSTER_REFINEMENT_CHOICE", "Chose to keep low threshold"),
                            cluster(Updated_cluster_center, Low_threshold_score, 
                                {
                                    Min_threshold,
                                    point_math:calc_average(Min_threshold, Current_threshold),
                                    Current_threshold
                                }
                            );
                        false ->
                            debugging:debug_log("DEBUG_CLUSTER_REFINEMENT_CHOICE", "Chose to keep high threshold"),
                            cluster(Updated_cluster_center, High_threshold_score, 
                                {
                                    Current_threshold,
                                    point_math:calc_average(Max_threshold, Current_threshold),
                                    Max_threshold
                                }
                            )
                        end
                end
        end, 
        Clusters
    ),
    refine_clusters(Updated_clusters, Points, Refinement_steps - 1).


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
    {_Min_threshold_a, Current_threshold_a, _Max_threshold_a} = Threshold_a,
    {_Min_threshold_b, Current_threshold_b, _Max_threshold_b} = Threshold_b,
    point_math:point_distance(Cluster_center_a, Cluster_center_b) < point_math:calc_average(Current_threshold_a, Current_threshold_b).


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

pick_best_clusters(_Discarded_clusters, Clusters_kept) ->
    Clusters_kept.

%% A nice facade to make it slightly easier to use, helps if we ever need to refactor
pick_best_clusters(Clusters) ->
    pick_best_clusters(Clusters, []).

