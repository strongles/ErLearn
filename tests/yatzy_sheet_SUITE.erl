%%%-------------------------------------------------------------------
%%% @author rob.williams
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2018 16:38
%%%-------------------------------------------------------------------
-module(yatzy_sheet_SUITE).
-include_lib("common_test/include/ct.hrl").
-author("rob.williams").

%% API
-export([all/0]).
-export([test_fill/1, test_get_score/1, test_fill_upper/1]).

all() -> [test_fill, test_get_score, test_fill_upper].

test_fill(_Config) ->
  {ok, #{ones := 5, bonus := 0}} = yatzy_sheet:fill(ones, [1, 1, 1, 1, 1], #{}),
  {ok, #{three_of_a_kind := 6}} = yatzy_sheet:fill(three_of_a_kind, [2, 2, 3, 4, 2], #{}),
  {ok, #{four_of_a_kind := 12}} = yatzy_sheet:fill(four_of_a_kind, [3, 3, 5, 3, 3], #{}),
  {ok, #{two_pair := 14}} = yatzy_sheet:fill(two_pair, [4, 3, 4, 3, 2], #{}),
  {ok, #{full_house := 27}} = yatzy_sheet:fill(full_house, [6, 5, 5, 5, 6], #{}),
  {ok, #{small_straight := 15}} = yatzy_sheet:fill(small_straight, [5, 2, 3, 1, 4], #{}),
  {ok, #{large_straight := 20}} = yatzy_sheet:fill(large_straight, [2, 6, 3, 5, 4], #{}),
  {ok, #{bonus := 50, fives := 25, fours := 60}} = yatzy_sheet:fill(fives, [5, 5, 5, 5, 5], #{fours => 60}),
  {ok, #{yatzy := 50}} = yatzy_sheet:fill(yatzy, [3, 3, 3, 3, 3], #{}),
  already_filled = yatzy_sheet:fill(twos, [1, 2, 3, 4, 5], #{twos => 6}),
  invalid_scoring = yatzy_sheet:fill(foo, [1, 2, 3, 4, 5], #{}).

test_get_score(_Config) ->
  15 = yatzy_sheet:get_score(fives, #{fives => 15}),
  40 = yatzy_sheet:get_score(upper, #{fives => 20, sixes => 12, twos => 8}),
  73 = yatzy_sheet:get_score(total, #{fours => 8, bonus => 50, small_straight => 15}),
  score_not_filled = yatzy_sheet:get_score(ones, #{twos => 4}),
  error = yatzy_sheet:get_score(foo, #{full_house => 30}).

test_fill_upper(_Config) ->
  {ok, #{bonus := 0,fives := 0,fours := 0,ones := 3,sixes := 0,threes := 3,twos := 2}} = yatzy_sheet:fill_upper([1, 2, 3, 1, 1], #{}).