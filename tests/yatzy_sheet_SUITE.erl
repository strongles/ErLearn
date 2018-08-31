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
-export([test_fill/1]).

all() -> [test_fill].

test_fill(_Config) ->
  FirstResult = #{ones => 5, bonus => 0},
  {ok, FirstResult} = yatzy_sheet:fill(ones, [1, 1, 1, 1, 1], #{}),
  already_filled = yatzy_sheet:fill(ones, [1, 1, 1, 1, 2], FirstResult),
  SecondResult = #{ones => 5, bonus => 0, four_of_a_kind => 16},
  {ok, SecondResult} = yatzy_sheet:fill(four_of_a_kind, [4, 4, 4, 2, 4], FirstResult).
