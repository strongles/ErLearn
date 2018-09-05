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
-export([start/0, roll_dice/2, dice_report/1, stop/1]).
-export_type([game_state/0]).

-define(NUM_DICE, 5).
-define(MAX_ROLLS, 3).

-spec start() -> {ok, pid()}.
start() ->
    {ok, spawn(fun() -> turn_roll() end)}.

-spec generic_dice(pid()) -> list().
generic_dice(Pid) ->
    call(Pid, dice).

-spec dice_report(pid()) -> no_return().
dice_report(Pid) ->
    io:format("Current dice hand is ~p~n", [generic_dice(Pid)]).

roll_dice(Pid, KeptDice) ->
    case call(Pid, {roll, KeptDice}) of
        ok ->
            dice_report(Pid);
        {cheat, rolls_exceeded} ->
            io:format("Maximum rolls of ~p exceeded~n", [?MAX_ROLLS]),
            dice_report(Pid);
        {cheat, dice_mismatch} ->
            io:format("~p were not part of your current hand.~n", [KeptDice]),
            dice_report(Pid)
    end.

-spec stop(pid()) -> list().
stop(Pid) ->
    call(Pid, stop).

-spec generate_dice(integer()) -> list().
generate_dice(Num) ->
    generate_dice(Num, []).
generate_dice(0, Acc) ->
    Acc;
generate_dice(Count, Acc) ->
    generate_dice(Count - 1, [rand:uniform(6) | Acc]).

-spec is_subset(list(), list()) -> boolean().
is_subset(Sub, Full) ->
        Sub -- Full =:= [].

-spec rolls_remaining(integer()) -> integer().
rolls_remaining(RollNum) ->
    ?MAX_ROLLS - RollNum.

-spec refill_dice(list()) -> list().
refill_dice(KeptDice) ->
    KeptDice ++ generate_dice(?NUM_DICE - length(KeptDice)).

turn_roll() ->
    DiceList = generate_dice(?NUM_DICE),
    io:format("NEW ROUND~nDice roll ~p~n", [DiceList]),
    turn_roll(DiceList, 1).
turn_roll(KeptDice, RollNum) when length(KeptDice) < ?NUM_DICE ->
    DiceList = refill_dice(KeptDice),
    io:format("Dice roll, ~p~n", [DiceList]),
    turn_roll(DiceList, RollNum);
turn_roll(DiceList, RollNum) ->
    receive
        {From, dice} ->
            From ! DiceList,
            turn_roll(DiceList, RollNum);
        {From, {roll, KeptDice}} ->
            case is_subset(KeptDice, DiceList) of
                false ->
                    From ! {cheat, dice_mismatch},
                    turn_roll(DiceList, RollNum);
                true ->
                    case rolls_remaining(RollNum) > 1 of
                        true ->
                            From ! ok,
                            turn_roll(DiceList, RollNum + 1);
                        false ->
                            From ! ok,
                            final_roll(KeptDice)
                    end
            end;
        {From, stop} ->
            From ! DiceList
    end.

final_roll(KeptDice) when length(KeptDice) < 5 ->
    DiceList = refill_dice(KeptDice),
    io:format("Dice roll, ~p~n", [DiceList]),
    final_roll(DiceList);
final_roll(DiceList) ->
    receive
        {From, dice} ->
            From ! DiceList,
            final_roll(DiceList);
        {From, {roll, _}} ->
            From ! {cheat, rolls_exceeded},
            final_roll(DiceList);
        {From, stop} ->
            From ! DiceList
    end.

call(Pid, Args) ->
    Pid ! {self(), Args},
    receive
        Res ->
            Res
    after 4000 ->
        timeout
    end.