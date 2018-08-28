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
-export([upper/2, one_pair/1, two_pair/1, three_of_a_kind/1, four_of_a_kind/1, full_house/1, small_straight/1, large_straight/1, chance/1, yatzy/1, consecutive_dice/2]).

-spec upper(integer(), list()) -> integer().
upper(N, DiceList) ->
  upper(N, DiceList, 0).
upper(_, [], Acc) ->
  Acc;
upper(N, [H | T], Acc) ->
  Sum = if
      H =:= N ->
        Acc + H;
      H =/= N ->
        Acc
    end,
  upper(N, T, Sum).

-spec consecutive_dice(integer(), list()) -> {boolean(), integer()} | boolean.
consecutive_dice(N, DiceList) ->
  SortedDice = lists:reverse(lists:sort(DiceList)),
  consecutive_dice(N, SortedDice, 0, 6).
consecutive_dice(N, [H|_], N, Number) when H =/= Number ->
  {true, Number};
consecutive_dice(N, [], N, Number) ->
  {true, Number};
consecutive_dice(_, [], _, _) ->
  false;
consecutive_dice(N, [H|T], Count, Number) ->
  if
    H =:= Number ->
      consecutive_dice(N, T, Count+1, Number);
    H =/= Number ->
      consecutive_dice(N, T, 1, H)
  end.

-spec score_consecutive(integer(), list()) -> integer().
score_consecutive(Occurrences, DiceList) ->
  case consecutive_dice(Occurrences, DiceList) of
    false ->
      0;
    {true, N} ->
      Occurrences * N
  end.

-spec remove_all_occurrences(integer(), list()) -> list().
remove_all_occurrences(N, DiceList) ->
  ListLength = length(DiceList),
  TrimmedList = lists:delete(N, DiceList),
  case length(TrimmedList) of
    ListLength ->
      TrimmedList;
    _ ->
      remove_all_occurrences(N, TrimmedList)
  end.

-spec one_pair(list()) -> integer().
one_pair(DiceList) ->
  score_consecutive(2, DiceList).

-spec three_of_a_kind(list()) -> integer().
three_of_a_kind(DiceList) ->
  score_consecutive(3, DiceList).

-spec four_of_a_kind(list()) -> integer().
four_of_a_kind(DiceList) ->
  score_consecutive(4, DiceList).

-spec two_pair(list()) -> integer().
two_pair(DiceList) ->
  case score_consecutive(2, DiceList) of
    false ->
      0;
    {true, N} ->
      PairScore = 2 * N,
      PrunedList = remove_all_occurrences(N, DiceList),
      case score_consecutive(2, PrunedList) of
        false ->
          0;
        {true, Y} ->
          PairScore + 2 * Y
      end
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
  case consecutive_dice(3, DiceList) of
    false ->
      0;
    {true, TripleNumber} ->
      PrunedList = remove_all_occurrences(TripleNumber, DiceList),
      case consecutive_dice(2, PrunedList) of
        false ->
          0;
        {true, DoubleNumber} ->
          3 * TripleNumber + 2 * DoubleNumber
      end
  end.

-spec chance(list()) -> integer().
chance(DiceList) ->
  lists:sum(DiceList).

-spec yatzy(list()) -> integer().
yatzy(DiceList) ->
  case consecutive_dice(5, DiceList) of
    false ->
      0;
    {true, _} ->
      50
  end.

