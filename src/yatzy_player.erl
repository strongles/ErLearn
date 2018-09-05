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
-export([loop/1, new_player/1, record_score/3, final_score/1, print_score/1]). %% roll_dice/1, re_roll/2,

loop(Sheet) ->
  receive
%%    {From, roll_dice} ->
%%      DiceList = generate_dice(5),
%%      NewState = maps:put(dice_hand, DiceList, GameState),
%%      NewState2 = maps:put(roll_number, 1, NewState),
%%      NewState3 = maps:put(score_recorded, false, NewState2),
%%      From ! {ok, DiceList},
%%      loop(Sheet, NewState3);
%%    {From, {re_roll, KeptDice}} ->
%%      LastRoll = maps:get(dice_hand, GameState),
%%      RollNumber = maps:get(roll_number, GameState, not_rolled),
%%      case RollNumber of
%%        not_rolled ->
%%          From ! {error, not_rolled},
%%          loop(Sheet, GameState);
%%        3 ->
%%          From ! {error, rolls_exceeded},
%%          loop(Sheet, GameState);
%%        _ ->
%%          case KeptDice -- LastRoll =:= [] of
%%            true ->
%%              NewDice = generate_dice(5 - length(KeptDice)) ++ KeptDice,
%%              NewState = maps:put(dice_hand, NewDice, GameState),
%%              NewState2 = maps:put(roll_number, RollNumber + 1, NewState),
%%              From ! {ok, {NewDice, 3 - maps:get(roll_number, NewState2)}},
%%              loop(Sheet, NewState2);
%%            false ->
%%              From ! {error, cheater},
%%              loop(Sheet, GameState)
%%          end
%%      end;
    {From, {record_score, Hand, DiceList}} ->
      case yatzy_sheet:fill(Hand, DiceList, Sheet) of
        {ok, ScoredSheet} ->
          From ! {ok, yatzy_sheet:get_score(Hand, ScoredSheet)},
          loop(ScoredSheet);
        already_filled ->
          From ! {error, already_filled},
          loop(Sheet)
      end;
    {From, print_score} ->
      print_score_list([[Hand, Score]|| {Hand, Score} <- maps:to_list(Sheet)]),
      From ! ok,
      loop(Sheet);
    {From, code_change} ->
      From ! ok,
      ?MODULE:loop(Sheet);
    {From, final_score} ->
      From ! {ok, yatzy_sheet:get_score(total, Sheet)}
  end.

print_score_list([]) ->
  ok;
print_score_list([H|T]) ->
  io:format("~p: ~p~n", H),
  print_score_list(T).

-spec new_player(atom()) -> {ok, pid()}.
new_player(Name) ->
  Pid = spawn(?MODULE, loop, [yatzy_sheet:new()]),
  register(Name, Pid),
  {ok, Pid}.

call(Pid, Args) ->
  Pid ! Args,
  receive
    Res ->
      Res
  after 4000 ->
    timeout
  end.

%%roll_dice(Name) ->
%%  {ok, DiceList} = call(Name, {self(), roll_dice}),
%%  io:format("~p rolled ~p~n", [Name, DiceList]).

%%re_roll(Name, KeptDice) ->
%%  case call(Name, {self(), {re_roll, KeptDice}}) of
%%    {ok, {DiceList, RemainingRolls}} ->
%%      io:format("~p , rolls remaining: ~p~n", [DiceList, RemainingRolls]);
%%    {error, rolls_exceeded} ->
%%      io:format("Re-rolls all used.~n");
%%    {error, cheater} ->
%%      io:format("~p is a filthy cheater! ~p were not your dice!~n", [Name, KeptDice]);
%%    {error, not_rolled} ->
%%      io:format("You need to ROLL before you can RE-roll~n")
%%  end.

record_score(Name, Hand, DiceList) ->
  case call(Name, {self(), {record_score, Hand, DiceList}}) of
    {ok, Score} ->
      io:format("~p added ~p for ~p points~n", [Name, Hand, Score]);
    {error, multi_score} ->
      io:format("~p is a filthy cheater! You cannot add more than one score per round!~n", [Name]);
    {error, already_filled} ->
      io:format("Score already filled for ~p~n", [Hand])
  end.

final_score(Name) ->
  {ok, Score} = call(Name, {self(), final_score}),
  io:format("~p's final score: ~p~n", [Name, Score]).

print_score(Name) ->
  ok = call(Name, {self(), print_score}).