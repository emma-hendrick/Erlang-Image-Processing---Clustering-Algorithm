-module(debugging).
-export([debug_log/2]).
-compile(nowarn_nomatch).

%% Debugging Constants
-define(DEBUG_LOG, true).
    -define(DEBUG_CLUSTER_CREATION, false).
    -define(DEBUG_CLUSTER_REFINEMENT_VALS, false).
    -define(DEBUG_CLUSTER_REFINEMENT_CHOICE, false).
    -define(DEBUG_CLUSTER_GRADING, true).
    -define(DEBUG_CLUSTER_COUNT, false).
    -define(DEBUG_CLUSTER_NEIGHBOR_COUNT, false).


%% Output information if that debug group is active
debug_log("DEBUG_CLUSTER_CREATION", Text) when (?DEBUG_LOG and ?DEBUG_CLUSTER_CREATION) ->
    output(Text);
debug_log("DEBUG_CLUSTER_REFINEMENT_VALS", Text) when (?DEBUG_LOG and ?DEBUG_CLUSTER_REFINEMENT_VALS) ->
    output(Text);
debug_log("DEBUG_CLUSTER_REFINEMENT_CHOICE", Text) when (?DEBUG_LOG and ?DEBUG_CLUSTER_REFINEMENT_CHOICE) ->
    output(Text);
debug_log("DEBUG_CLUSTER_GRADING", Text) when (?DEBUG_LOG and ?DEBUG_CLUSTER_GRADING) ->
    output(Text);
debug_log("DEBUG_CLUSTER_COUNT", Text) when (?DEBUG_LOG and ?DEBUG_CLUSTER_COUNT) ->
    output(Text);
debug_log("DEBUG_CLUSTER_NEIGHBOR_COUNT", Text) when (?DEBUG_LOG and ?DEBUG_CLUSTER_NEIGHBOR_COUNT) ->
    output(Text);
debug_log(_Other, Text) -> {Text}.


%% Output text
output(Text) ->
    io:format("~p~n", [Text]).

