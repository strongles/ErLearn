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
-export([new_player/1, record_score/3, final_score/1, print_score/1]). %% roll_dice/1, re_roll/2,

loop(Sheet) ->
    receive
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
            print_score_list([[Hand, Score] || {Hand, Score} <- maps:to_list(Sheet)]),
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
print_score_list([H | T]) ->
    io:format("~p: ~p~n", H),
    print_score_list(T).

-spec new_player(atom()) -> {ok, pid()}.
new_player(Name) ->
    Pid = spawn(fun() -> loop(yatzy_sheet:new()) end),
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