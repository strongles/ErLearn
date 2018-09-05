%%%-------------------------------------------------------------------
%%% @author rob.williams
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2018 11:44
%%%-------------------------------------------------------------------
-module(yatzy_turn).
-author("rob.williams").

%% API
-export([start/0, first_roll/0, second_roll/1, third_roll/1]).
-export([generic_start/0, generic_dice_roll/2, dice_report/1, generic_stop/1]).
-export_type([game_state/0]).

-define(NUM_DICE, 5).
-define(MAX_ROLLS, 3).

-opaque game_state() :: map().

-spec start_state() -> game_state().
start_state() ->
  #{}.
%% Last Roll
%% Kept Dice

start() ->
  {ok, spawn(?MODULE, first_roll, [start_state()])}.

-spec generate_dice(integer()) -> list().
generate_dice(Num) ->
  generate_dice(Num, []).
generate_dice(0, Acc) ->
  Acc;
generate_dice(Count, Acc) ->
  generate_dice(Count - 1, [rand:uniform(6) | Acc]).

is_subset(Sub, Full) ->
    Sub -- Full =:= [].

%% If you want to capture the generality of 1st and 2nd move I suggest that you create one
%% state for them and a final state.
%% Why that way? It allows you to have the logic for how to deal with the messages being
%% driven from the state you're in. I.e., in rolling you do accept roll, but in final you
%% will reject roll.

%% first_roll/0 will actually never be called since in start you spawn first_roll with a
%% list with one element, ie, you will spawn first_roll/1.
first_roll() ->
  DiceList = generate_dice(?NUM_DICE),
  GameState = maps:put(last_roll, DiceList, start_state()),
  io:format("NEW ROUND~nDice roll ~p~n", [DiceList]),
  first_roll(GameState).
first_roll(DiceList) ->
  receive
    {From, dice} ->
      From ! DiceList,
      first_roll(DiceList);
    {From, {roll, KeptDice}} ->
      case is_subset(KeptDice, DiceList) of
        false ->
          From ! {cheat, dice_mismatch},
          first_roll(DiceList);
        true ->
              % no need to spawn another process here, just call it:
              % second_roll(NewDice)
              % but do calculate the new dice before you call.
              % Or continue with rolling the dice in the first clause of the second_roll/1
              % function. I'd go with rolling the dice here and only have the receive
              % logic in the second_roll/1 clauses.
          spawn(?MODULE, second_roll, [new, KeptDice])
      end;
    {From, score} ->
      From ! DiceList
  end.

second_roll(KeptDice) when length(KeptDice) < 5 ->
  DiceList = KeptDice ++ generate_dice(?NUM_DICE - length(KeptDice)),
  io:format("Dice roll, ~p~n", [DiceList]),
  second_roll(DiceList);
second_roll(DiceList) ->
  receive
    {From, dice} ->
      From ! DiceList,
      second_roll(DiceList);
    {From, {roll, KeptDice}} ->
      case is_subset(KeptDice, DiceList) of
        false ->
          From ! {cheat, dice_mismatch},
          first_roll(DiceList);
        true ->
          spawn(?MODULE, third_roll, [new, KeptDice])
      end;
    {From, score} ->
      From ! DiceList
  end.

third_roll(KeptDice) when length(KeptDice) < 5 ->
  DiceList = KeptDice ++ generate_dice(?NUM_DICE - length(KeptDice)),
  io:format("Dice roll, ~p~n", [DiceList]),
  third_roll(DiceList);
third_roll(DiceList) ->
  receive
    {From, dice} ->
      From ! DiceList,
      third_roll(DiceList);
    {From, {roll, _}} ->
      From ! {error, re_rolls_exceeded}, %%io:format("Re-rolls exeeded~n"),
      third_roll(DiceList);
    {From, score} ->
      From ! DiceList
  end.

generic_roll() ->
  DiceList = generate_dice(?NUM_DICE),
  io:format("NEW ROUND~nDice roll ~p~n", [DiceList]),
  generic_roll(DiceList, 1).
generic_roll(KeptDice, RollNum) when length(KeptDice) < ?NUM_DICE ->
  DiceList = KeptDice ++ generate_dice(?NUM_DICE - length(KeptDice)),
  io:format("Dice roll, ~p~n", [DiceList]),
  generic_roll(DiceList, RollNum);
generic_roll(DiceList, RollNum) ->
  receive
    {From, dice} ->
      From ! DiceList,
      generic_roll(DiceList, RollNum);
    {From, {roll, KeptDice}} ->
      case is_subset(KeptDice, DiceList) of
        false ->
          From ! {cheat, dice_mismatch},
          generic_roll(DiceList, RollNum);
        true ->
          case RollNum >= ?MAX_ROLLS of
            true ->
              From ! {cheat, rolls_exceeded},
              generic_roll(DiceList, RollNum);
            false ->
              From ! ok,
              generic_roll(KeptDice, RollNum + 1)
          end
      end;
    {From, stop} ->
      From ! DiceList
  end.

generic_call(Pid, Args) ->
    % Add the self() here to make it simpler to call this one:
    % Pid ! {self(), Args},
  Pid ! Args,
  receive
    Res ->
      Res
  after 4000 ->
    timeout
  end.


%% Put the exported functions at the top - then you don't have to look at all the internal
%% stuff to understand the API.
-spec generic_start() -> {ok, pid()}.
generic_start() ->
  {ok, spawn(fun() -> generic_roll() end)}.

-spec generic_dice(pid()) -> list().
generic_dice(Pid) ->
  generic_call(Pid, {self(), dice}).
    % generic_call(Pid, dice).


-spec dice_report(pid()) -> no_return().
dice_report(Pid) ->
  io:format("Current dice hand is ~p~n", [generic_dice(Pid)]).

generic_dice_roll(Pid, KeptDice) ->
  case generic_call(Pid, {self(), {roll, KeptDice}}) of
    ok ->
      dice_report(Pid);
    {cheat, rolls_exceeded} ->
      io:format("Maximum rolls of ~p exceeded~n", [?MAX_ROLLS]),
      dice_report(Pid);
    {cheat, dice_mismatch} ->
      io:format("~p were not part of your current hand.~n", [KeptDice]),
      dice_report(Pid)
  end.

-spec generic_stop(pid()) -> list().
generic_stop(Pid) ->
  generic_call(Pid, {self(), stop}).
