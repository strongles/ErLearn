%%%-------------------------------------------------------------------
%%% @author rob.williams
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2018 16:32
%%%-------------------------------------------------------------------
-module(yatzy_player).
-author("rob.williams").

%% API
-export([loop/1, new_player/1, roll_dice/1, re_roll/2, record_score/2, final_score/1]).

generate_dice(Num) ->
  generate_dice(Num, []).
generate_dice(0, Acc) ->
  Acc;
generate_dice(Count, Acc) ->
  generate_dice(Count - 1, [rand:uniform(6) | Acc]).

loop(Sheet) ->
  receive
    {Pid, roll_dice} ->
      DiceList = generate_dice(5),
      NewSheet = maps:put(dice_hand, DiceList, Sheet),
      NewSheet2 = maps:put(roll_number, 1, NewSheet),
      Pid ! {ok, DiceList},
      loop(NewSheet2);
    {Pid, {re_roll, KeptDice}} ->
      LastRoll = maps:get(dice_hand, Sheet),
      RollNumber = maps:get(roll_number, Sheet, not_rolled),
      case RollNumber of
        not_rolled ->
          Pid ! {error, not_rolled},
          loop(Sheet);
        3 ->
          Pid ! {error, rolls_exceeded},
          loop(Sheet);
        _ ->
          case KeptDice -- LastRoll =:= [] of
            true ->
              NewDice = generate_dice(5 - length(KeptDice)) ++ KeptDice,
              NewSheet = maps:put(dice_hand, NewDice, Sheet),
              NewSheet2 = maps:put(roll_number, RollNumber + 1, NewSheet),
              Pid ! {ok, {NewDice, 3 - maps:get(roll_number, NewSheet2)}},
              loop(NewSheet2);
            false ->
              Pid ! {error, cheater},
              loop(Sheet)
          end
      end;
    {Pid, {score, upper}} ->
      {ok, ScoredSheet} = yatzy_sheet:fill_upper(maps:get(dice_hand, Sheet), Sheet), %% Reimplement scoring based on the sheet containing the hand?
      Pid ! {ok, yatzy_sheet:get_score(upper, ScoredSheet)},
      loop(ScoredSheet);
    {Pid, {score, Hand}} ->
      {ok, ScoredSheet} = yatzy_sheet:fill(Hand, maps:get(dice_hand, Sheet), Sheet),
      Pid ! {ok, yatzy_sheet:get_score(Hand, ScoredSheet)},
      loop(ScoredSheet);
    {Pid, final_score} ->
      Pid ! {ok, yatzy_sheet:get_score(total, Sheet)};
    code_change ->
      ?MODULE:loop(Sheet)
  end.

new_player(Name) ->
  Pid = spawn(?MODULE, loop, [yatzy_sheet:new()]),
  register(Name, Pid),
  {ok, Pid}.

roll_dice(Name) ->
  Name ! {self(), roll_dice},
  receive
    {ok, DiceList} ->
      io:format("~p rolled ~p~n", [Name, DiceList])
  after 4000 ->
    timeout
  end.

re_roll(Name, KeptDice) ->
  Name ! {self(), {re_roll, KeptDice}},
  receive
    {ok, {DiceList, RemainingRolls}} ->
      io:format("~p , rolls remaining: ~p~n", [DiceList, RemainingRolls]);
    {error, rolls_exceeded} ->
      io:format("Re-rolls all used.~n");
    {error, cheater} ->
      io:format("~p is a filthy cheater! ~p were not your dice!~n", [Name, KeptDice]);
    {error, not_rolled} ->
      io:format("You need to ROLL before you can RE-roll")
  after 4000 ->
    timeout
  end.

record_score(Name, Hand) ->
  Name ! {self(), {score, Hand}},
  receive
    {ok, Score} ->
      io:format("~p added ~p for ~p points~n", [Name, Hand, Score])
  after 4000 ->
    timeout
  end.

final_score(Name) ->
  Name ! {self(), final_score},
  receive
    {ok, Score} ->
      io:format("~p's final score: ~p~n", [Name, Score])
  after 4000 ->
    timeout
  end.