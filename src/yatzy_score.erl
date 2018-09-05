%%%-------------------------------------------------------------------
%%% @author rob.williams
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2018 16:32
%%%-------------------------------------------------------------------
-module(yatzy_score).
-author("rob.williams").

%% API
-export([upper/2, one_pair/1, two_pair/1, three_of_a_kind/1, four_of_a_kind/1, full_house/1, small_straight/1,
    large_straight/1, chance/1, yatzy/1, score_dice/2]).

-spec upper(integer(), list()) -> integer().
upper(N, DiceList) ->
    lists:sum(lists:filter(fun(X) -> X =:= N end, DiceList)).

-spec one_pair(list()) -> integer().
one_pair(DiceList) ->
    case lists:reverse(lists:sort(DiceList)) of
        [N, N, _, _, _] ->
            N * 2;
        [_, N, N, _, _] ->
            N * 2;
        [_, _, N, N, _] ->
            N * 2;
        [_, _, _, N, N] ->
            N * 2;
        _ ->
            0
    end.

-spec three_of_a_kind(list()) -> integer().
three_of_a_kind(DiceList) ->
    case lists:sort(DiceList) of
        [N, N, N, _, _] ->
            N * 3;
        [_, N, N, N, _] ->
            N * 3;
        [_, _, N, N, N] ->
            N * 3;
        _ ->
            0
    end.

-spec four_of_a_kind(list()) -> integer().
four_of_a_kind(DiceList) ->
    case lists:sort(DiceList) of
        [N, N, N, N, _] ->
            4 * N;
        [_, N, N, N, N] ->
            4 * N;
        _ ->
            0
    end.

-spec two_pair(list()) -> integer().
two_pair(DiceList) ->
    case lists:sort(DiceList) of
        [A, A, B, B, _] when A =/= B ->
            A * 2 + B * 2;
        [A, A, _, B, B] ->
            A * 2 + B * 2;
        [_, A, A, B, B] when A =/= B ->
            A * 2 + B * 2;
        _ ->
            0
    end.

-spec small_straight(list()) -> integer().
small_straight(DiceList) ->
    case lists:sort(DiceList) of
        [1, 2, 3, 4, 5] ->
            15;
        _ ->
            0
    end.

-spec large_straight(list()) -> integer().
large_straight(DiceList) ->
    case lists:sort(DiceList) of
        [2, 3, 4, 5, 6] ->
            20;
        _ ->
            0
    end.

-spec full_house(list()) -> integer().
full_house(DiceList) ->
    case lists:sort(DiceList) of
        [A, A, A, B, B] when A =/= B ->
            lists:sum(DiceList);
        [A, A, B, B, B] when A =/= B ->
            lists:sum(DiceList);
        _ ->
            0
    end.

-spec chance(list()) -> integer().
chance(DiceList) ->
    lists:sum(DiceList).

-spec yatzy(list()) -> integer().
yatzy([N, N, N, N, N]) ->
    50;
yatzy(_) ->
    0.

-spec score_dice(atom(), list()) -> integer().
score_dice(ones, DiceList) ->
    upper(1, DiceList);
score_dice(twos, DiceList) ->
    upper(2, DiceList);
score_dice(threes, DiceList) ->
    upper(3, DiceList);
score_dice(fours, DiceList) ->
    upper(4, DiceList);
score_dice(fives, DiceList) ->
    upper(5, DiceList);
score_dice(sixes, DiceList) ->
    upper(6, DiceList);
score_dice(one_pair, DiceList) ->
    one_pair(DiceList);
score_dice(three_of_a_kind, DiceList) ->
    three_of_a_kind(DiceList);
score_dice(four_of_a_kind, DiceList) ->
    four_of_a_kind(DiceList);
score_dice(two_pair, DiceList) ->
    two_pair(DiceList);
score_dice(full_house, DiceList) ->
    full_house(DiceList);
score_dice(small_straight, DiceList) ->
    small_straight(DiceList);
score_dice(large_straight, DiceList) ->
    large_straight(DiceList);
score_dice(chance, DiceList) ->
    chance(DiceList);
score_dice(yatzy, DiceList) ->
    yatzy(DiceList).
