-module(map_sets_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Macros:

-define(PROPER(PROP),
        {timeout, 60,
         ?_assertEqual( true
                      , proper:quickcheck(PROP, [ {numtests, 100}
                                                  %% , {max_size, 100}
                                                ])
                      )}).

%% Testcases:

is_set_test_() ->
    ?PROPER(?FORALL(L, list(term()),
                    mset_set_refl(is_set, [], [L])
                   )).

from_list_test_() ->
    ?PROPER(?FORALL(L, list(term()),
                    mset_set_refl(from_list, [L], [])
                   )).

to_list_test_() ->
    ?PROPER(?FORALL(L, list(term()),
                    mset_set_refl(to_list, [], [L])
                   )).

is_elem_neg_test_() ->
    ?PROPER(?FORALL({E, L}, {term(), list()},
                    begin
                        L2 = [I || I <- L, I /= E],
                        mset_set_refl(is_element, [E], [L2])
                    end
                   )).

is_elem_pos_test_() ->
    ?PROPER(?FORALL({E, L1, L2}, {term(), list(), list()},
                    begin
                        L = L1 ++ [E|L2],
                        mset_set_refl(is_element, [E], [L])
                    end
                   )).

add_elem_test_() ->
    ?PROPER(?FORALL({E, L}, {term(), list()},
                        mset_set_refl(add_element, [E], [L])
                   )).

del_elem_test_() ->
    ?PROPER(?FORALL({E, L}, {term(), list()},
                        mset_set_refl(del_element, [E], [L])
                   )).

is_subset_test_() ->
    ?PROPER(?FORALL({S1, S2}, {list(), list()},
                        mset_set_refl(is_subset, [], [S1, S2])
                   )).

subtract_test_() ->
    ?PROPER(?FORALL({S1, S2}, {list(), list()},
                        mset_set_refl(subtract, [], [S1, S2])
                   )).

union_2_test_() ->
    ?PROPER(?FORALL({S1, S2}, {list(), list()},
                        mset_set_refl(union, [], [S1, S2])
                   )).

union_1_test_() ->
    ?PROPER(?FORALL(LL, list(list()),
                    mset_set_refl_simple(union, LL)
                   )).

intersection_1_test_() ->
    ?PROPER(?FORALL(LL, nonempty_list(list()),
                    mset_set_refl_simple(intersection, LL)
                   )).

intersection_2_test_() ->
    ?PROPER(?FORALL({S1, S2}, {list(), list()},
                        mset_set_refl(intersection, [], [S1, S2])
                   )).

is_disjoint_test_() ->
    ?PROPER(?FORALL({S1, S2}, {list(), list()},
                        mset_set_refl(is_disjoint, [], [S1, S2])
                   )).

intersection_subset_test_() ->
    ?PROPER(?FORALL({L1, L2}, {string(), string()},
                    begin
                        S1 = map_sets:from_list(L1),
                        S2 = map_sets:from_list(L2),
                        I  = map_sets:intersection(S1, S2),
                        map_sets:is_subset(I, S1) andalso
                        map_sets:is_subset(I, S2)
                    end)).

intersection_disjoint_test_() ->
    ?PROPER(?FORALL({L1, L2}, {non_empty(string()), non_empty(string())},
                    begin
                        S1 = map_sets:from_list(L1),
                        S2 = map_sets:from_list(L2),
                        I  = map_sets:intersection(S1, S2),
                        map_sets:is_disjoint(S1, S2) orelse
                        map_sets:size(I) > 0
                    end)).

intersection_union_same_test_() ->
    ?PROPER(?FORALL(L, list(),
                    begin
                        S = map_sets:from_list(L),
                        map_sets:union(S, S) =:= S andalso
                        map_sets:intersection(S, S) =:= S
                    end)).

substract_disjoint_test_() ->
    ?PROPER(?FORALL({L1, L2}, {string(), string()},
                    begin
                        S1 = map_sets:from_list(L1),
                        S2 = map_sets:from_list(L2),
                        S3 = map_sets:subtract(S1, S2),
                        map_sets:is_disjoint(S3, S2)
                    end)).

is_add_del_element_test_() ->
    ?PROPER(?FORALL({L, E}, {string(), term()},
                    begin
                        S = map_sets:from_list(L),
                        S1 = map_sets:add_element(E, S),
                        S2 = map_sets:del_element(E, S1),
                        map_sets:is_element(E, S1) andalso
                        not map_sets:is_element(E, S2)
                    end)).

%% Utility functions:

to_list(M, A) ->
    lists:sort(M:to_list(A)).

mset_set_refl_simple(F, Args) ->
    Ls  = [sets:from_list(I)     || I <- Args],
    Lms = [map_sets:from_list(I) || I <- Args],
    to_list(sets, sets:F(Ls)) == to_list(map_sets, map_sets:F(Lms)).

mset_set_refl(F, Args, Sets) ->
    Set_args  = Args ++ lists:map(fun sets:from_list/1, Sets),
    MSet_args = Args ++ lists:map(fun map_sets:from_list/1, Sets),
    R_s  = apply(sets,     F, Set_args),
    R_ms = apply(map_sets, F, MSet_args),
    %% Prepare results:
    case sets:is_set(R_s) of
        true ->
            R2_s  = to_list(sets, R_s),
            R2_ms = to_list(map_sets, R_ms);
        false ->
            if is_list(R_s) ->
                    R2_s  = lists:sort(R_s),
                    R2_ms = lists:sort(R_ms);
               true ->
                    R2_s  = R_s,
                    R2_ms = R_ms
            end
    end,
    case R2_s == R2_ms of
        true ->
            true;
        false ->
            io:format(user, "Mismatch! ~p /= ~p~n", [R2_s, R2_ms]),
            false
    end.
