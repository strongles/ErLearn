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
-export([start/0, roll/2, dice/1, stop/1]).
-export_type([game_state/0]).

-define(NUM_DICE, 5).
-define(MAX_ROLLS, 3).

-spec start() -> {ok, pid()}.
start() ->
    {ok, spawn(fun() -> turn_roll() end)}.

-spec generic_dice(Pid :: pid()) -> list().
generic_dice(Pid) ->
    call(Pid, dice).

-spec dice(Pid :: pid()) -> no_return().
dice(Pid) ->
    io:format("Current dice hand is ~p~n", [generic_dice(Pid)]).

-spec roll(Pid :: pid(), KeptDice :: list()) -> no_return().
roll(Pid, KeptDice) ->
    case call(Pid, {roll, KeptDice}) of
        ok ->
            dice(Pid);
        finished ->
            io:format("Maximum rolls of ~p exceeded~n", [?MAX_ROLLS]),
            dice(Pid);
        invalid_keepers ->
            io:format("~p were not part of your current hand.~n", [KeptDice]),
            dice(Pid)
    end.

-spec stop(Pid :: pid()) -> list().
stop(Pid) ->
    call(Pid, stop).

-spec generate_dice(Num :: integer()) -> list().
generate_dice(Num) ->
    generate_dice(Num, []).
generate_dice(0, Acc) ->
    Acc;
generate_dice(Count, Acc) ->
    generate_dice(Count - 1, [rand:uniform(6) | Acc]).

-spec is_subset(Sub :: list(), Full :: list()) -> boolean().
is_subset(Sub, Full) ->
        Sub -- Full =:= [].

-spec refill_dice(KeptDice :: list()) -> list().
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
turn_roll(DiceList, ?MAX_ROLLS)->
    final_roll(DiceList);
turn_roll(DiceList, RollNum) ->
    receive
        {From, dice} ->
            From ! DiceList,
            turn_roll(DiceList, RollNum);
        {From, {roll, KeptDice}} ->
            case is_subset(KeptDice, DiceList) of
                false ->
                    From ! invalid_keepers,
                    turn_roll(DiceList, RollNum);
                true ->
                    From ! ok,
                    turn_roll(KeptDice, RollNum + 1)
            end;
        {From, stop} ->
            From ! DiceList
    end.

final_roll(DiceList) ->
    receive
        {From, dice} ->
            From ! DiceList,
            final_roll(DiceList);
        {From, {roll, _}} ->
            From ! finished,
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