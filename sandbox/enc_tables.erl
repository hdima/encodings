-module(enc_tables).

-export([display_tables/1]).


%%
%% EncTbl = [{UC, C} | [{UC, C}] | badarg]
%% DecTbl = [C | badarg]
%%
display_tables(File) ->
    {ok, Terms} = file:consult(File),
    Chars = lists:sort([{C, UC}
        || {C, UC} <- Terms, is_integer(C), is_integer(UC)]),
    New = fill_holes(Chars, [], 0),
    EncTbl = compact(lists:sort([{UC rem 255, {UC, C}} || {C, UC} <- New])),
    DecTbl = [UC || {_, UC} <- New],
    {EncTbl, DecTbl}.


fill_holes([], New, 256) ->
    lists:reverse(New);
fill_holes([{N, _}=Pair | Tail], New, N) ->
    fill_holes(Tail, [Pair | New], N + 1);
fill_holes(Tail, New, N) ->
    fill_holes(Tail, [{N, badarg} | New], N + 1).


compact([{H, P} | Tail]) ->
    . % TODO
