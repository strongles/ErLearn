%%%-------------------------------------------------------------------
%%% @author rob.williams
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2018 09:11
%%%-------------------------------------------------------------------
-module(yatzy_sheet).
-author("rob.williams").

%% API
-export([new/0, fill/3, get_score/2]).

-define(UPPER, [ones, twos, threes, fours, fives, sixes]).
-define(HANDS, [one_pair, three_of_a_kind, four_of_a_kind, two_pair,
    small_straight, large_straight, full_house, chance, yatzy]).
-define(VALID_SCORING, ?UPPER ++ ?HANDS).

-export_type([t/0]).

-opaque t() :: map().

-spec new() -> t().
new() ->
    #{}.

-spec fill(atom(), list(), t()) -> {ok, t()} | already_filled | invalid_scoring.
fill(Key, DiceList, Sheet) ->
    case lists:member(Key, ?VALID_SCORING) of
        false ->
            invalid_scoring;
        true ->
            case maps:get(Key, Sheet, empty) of
                empty ->
                    NewSheet = maps:put(Key, yatzy_score:score_dice(Key, DiceList), Sheet),
                    {ok, NewSheet};
                _ ->
                    already_filled
            end
    end.

-spec get_score(atom(), t()) -> {ok, integer()} | score_not_filled | error.
get_score(upper, Sheet) ->
    lists:sum(lists:map(fun(Key) -> get_score(Key, Sheet) end, ?UPPER));
get_score(total, Sheet) ->
    lists:sum(lists:map(fun(Key) -> maps:get(Key, Sheet) end, ?VALID_SCORING));
get_score(bonus, Sheet) ->
    case get_score(upper, Sheet) >= 63 of
        true -> 50;
        false -> 0
    end;
get_score(Category, Sheet) ->
    case lists:member(Category, ?VALID_SCORING) of
        true ->
            maps:get(Category, Sheet, 0);
        false ->
            error
    end.
