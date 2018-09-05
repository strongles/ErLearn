%%%-------------------------------------------------------------------
%%% @author rob.williams
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2018 12:06
%%%-------------------------------------------------------------------
-module(yatzy_score_SUITE).
-include_lib("common_test/include/ct.hrl").
-author("rob.williams").

%% API
-export([all/0]).
-export([test_one_pair_scored_correctly/1, test_three_of_a_kind_scored_correctly/1,
    test_four_of_a_kind_scored_correctly/1, test_two_pair_scored_correctly/1, test_full_house_scored_correctly/1,
    test_yatzy_scored_correctly/1, test_small_straight_scored_correctly/1, test_large_straight_scored_correctly/1,
    test_chance_scored_correctly/1, test_upper_scored_correctly/1]).

all() -> [test_one_pair_scored_correctly, test_three_of_a_kind_scored_correctly, test_four_of_a_kind_scored_correctly,
    test_two_pair_scored_correctly, test_full_house_scored_correctly, test_yatzy_scored_correctly,
    test_small_straight_scored_correctly, test_large_straight_scored_correctly, test_chance_scored_correctly,
    test_upper_scored_correctly].

test_upper_scored_correctly(_Config) ->
    0 = yatzy_score:upper(5, [1, 1, 1, 1, 1]),
    4 = yatzy_score:upper(2, [2, 3, 3, 1, 2]),
    3 = yatzy_score:upper(3, [3, 4, 1, 4, 2]),
    12 = yatzy_score:upper(4, [4, 5, 6, 4, 4]).

test_one_pair_scored_correctly(_Config) ->
    2 = yatzy_score:one_pair([1, 1, 6, 3, 2]),
    4 = yatzy_score:one_pair([1, 2, 3, 2, 5]),
    6 = yatzy_score:one_pair([3, 1, 3, 4, 5]),
    8 = yatzy_score:one_pair([4, 1, 1, 2, 4]),
    10 = yatzy_score:one_pair([1, 4, 2, 5, 5]),
    12 = yatzy_score:one_pair([5, 6, 3, 6, 2]),
    0 = yatzy_score:one_pair([1, 2, 3, 4, 6]).

test_three_of_a_kind_scored_correctly(_Config) ->
    3 = yatzy_score:three_of_a_kind([1, 1, 6, 3, 1]),
    6 = yatzy_score:three_of_a_kind([1, 2, 3, 2, 2]),
    9 = yatzy_score:three_of_a_kind([3, 1, 3, 3, 5]),
    12 = yatzy_score:three_of_a_kind([4, 1, 1, 4, 4]),
    15 = yatzy_score:three_of_a_kind([5, 4, 2, 5, 5]),
    18 = yatzy_score:three_of_a_kind([5, 6, 6, 6, 2]),
    0 = yatzy_score:three_of_a_kind([1, 2, 3, 4, 6]).

test_four_of_a_kind_scored_correctly(_Config) ->
    4 = yatzy_score:four_of_a_kind([1, 1, 6, 1, 1]),
    8 = yatzy_score:four_of_a_kind([1, 2, 2, 2, 2]),
    12 = yatzy_score:four_of_a_kind([3, 3, 3, 3, 5]),
    16 = yatzy_score:four_of_a_kind([4, 1, 4, 4, 4]),
    20 = yatzy_score:four_of_a_kind([5, 5, 5, 3, 5]),
    24 = yatzy_score:four_of_a_kind([5, 6, 6, 6, 6]),
    0 = yatzy_score:four_of_a_kind([1, 2, 3, 4, 6]).

test_two_pair_scored_correctly(_Config) ->
    6 = yatzy_score:two_pair([1, 1, 6, 2, 2]),
    10 = yatzy_score:two_pair([1, 2, 3, 2, 3]),
    16 = yatzy_score:two_pair([1, 3, 3, 5, 5]),
    10 = yatzy_score:two_pair([4, 1, 4, 1, 2]),
    14 = yatzy_score:two_pair([5, 2, 2, 3, 5]),
    22 = yatzy_score:two_pair([5, 6, 2, 6, 5]),
    0 = yatzy_score:two_pair([1, 2, 3, 4, 6]),
    0 = yatzy_score:two_pair([1, 1, 1, 4, 6]),
    0 = yatzy_score:two_pair([1, 2, 2, 2, 2]),
    0 = yatzy_score:two_pair([1, 2, 3, 4, 6]).

test_full_house_scored_correctly(_Config) ->
    8 = yatzy_score:full_house([1, 1, 2, 2, 2]),
    13 = yatzy_score:full_house([3, 2, 3, 2, 3]),
    21 = yatzy_score:full_house([5, 3, 3, 5, 5]),
    11 = yatzy_score:full_house([4, 1, 4, 1, 1]),
    16 = yatzy_score:full_house([5, 2, 2, 2, 5]),
    27 = yatzy_score:full_house([5, 6, 5, 6, 5]),
    0 = yatzy_score:full_house([1, 2, 3, 4, 6]),
    0 = yatzy_score:full_house([6, 6, 6, 6, 6]).

test_small_straight_scored_correctly(_Config) ->
    0 = yatzy_score:small_straight([1, 1, 2, 2, 2]),
    15 = yatzy_score:small_straight([1, 2, 3, 4, 5]),
    0 = yatzy_score:small_straight([5, 3, 3, 5, 5]),
    0 = yatzy_score:small_straight([4, 1, 4, 1, 1]),
    0 = yatzy_score:small_straight([5, 2, 2, 2, 5]),
    0 = yatzy_score:small_straight([5, 6, 5, 6, 5]),
    0 = yatzy_score:small_straight([1, 2, 3, 4, 6]).

test_large_straight_scored_correctly(_Config) ->
    0 = yatzy_score:large_straight([1, 1, 2, 2, 2]),
    0 = yatzy_score:large_straight([3, 2, 3, 2, 3]),
    0 = yatzy_score:large_straight([5, 3, 3, 5, 5]),
    0 = yatzy_score:large_straight([4, 1, 4, 1, 1]),
    0 = yatzy_score:large_straight([5, 2, 2, 2, 5]),
    20 = yatzy_score:large_straight([2, 3, 4, 5, 6]),
    0 = yatzy_score:large_straight([1, 2, 3, 4, 6]).

test_chance_scored_correctly(_Config) ->
    16 = yatzy_score:chance([1, 6, 2, 3, 4]),
    15 = yatzy_score:chance([3, 2, 4, 1, 5]),
    15 = yatzy_score:chance([5, 3, 1, 2, 4]),
    16 = yatzy_score:chance([4, 1, 6, 3, 2]),
    20 = yatzy_score:chance([5, 6, 4, 2, 3]).

test_yatzy_scored_correctly(_Config) ->
    0 = yatzy_score:yatzy([1, 1, 2, 2, 2]),
    0 = yatzy_score:yatzy([3, 2, 3, 2, 3]),
    50 = yatzy_score:yatzy([5, 5, 5, 5, 5]),
    0 = yatzy_score:yatzy([4, 1, 4, 1, 1]),
    50 = yatzy_score:yatzy([2, 2, 2, 2, 2]),
    0 = yatzy_score:yatzy([5, 6, 5, 6, 5]),
    0 = yatzy_score:yatzy([1, 2, 3, 4, 6]).