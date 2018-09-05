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
    Pid ! Args,
    receive
        Res ->
            Res
    after 4000 ->
        timeout
    end.

-spec generic_start() -> {ok, pid()}.
generic_start() ->
    {ok, spawn(fun() -> generic_roll() end)}.

-spec generic_dice(pid()) -> list().
generic_dice(Pid) ->
    generic_call(Pid, {self(), dice}).

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