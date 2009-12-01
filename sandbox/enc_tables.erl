-module(enc_tables).

-compile(export_all).

-export([display_tables/1]).


%%
%% EncTbl = [{UC, C} | [{UC, C}] | badarg]
%% DecTbl = [C | badarg]
%%
display_tables(File) ->
    {ok, Terms} = file:consult(File),
    Chars = lists:keysort(1, [{C, UC}
        || {C, UC} <- Terms, is_integer(C), is_integer(UC)]),
    EncPrep = lists:keysort(1, [{UC rem 255, {UC, C}} || {C, UC} <- Chars]),
    EncTbl = [P || {_, P} <- fill_holes(group(EncPrep))],
    DecTbl = [UC || {_, UC} <- fill_holes(Chars)],
    {EncTbl, DecTbl}.


fill_holes(Chars) ->
    fill_holes(Chars, [], 0).

fill_holes([], New, 256) ->
    lists:reverse(New);
fill_holes([{N, _}=Pair | Tail], New, N) ->
    fill_holes(Tail, [Pair | New], N + 1);
fill_holes(Tail, New, N) ->
    fill_holes(Tail, [{N, badarg} | New], N + 1).


group([{H, _}=I | Tail]) ->
    group(Tail, I, H, []).

group([{H, _}=I | Tail], {H, O}, H, Acc) ->
    group(Tail, [I, O], H, Acc);
group([{H, _}=I | Tail], [{H, OI} | O], H, Acc) ->
    group(Tail, [I, OI | O], H, Acc);
group([{H, _}=I | Tail], A, _, Acc) when is_tuple(A) ->
    group(Tail, I, H, [A | Acc]);
group([{H, _}=I | Tail], [{_, O} | OT], OH, Acc) ->
    group(Tail, I, H, [{OH, [O | OT]} | Acc]);
group([], A, _, Acc) when is_tuple(A) ->
    lists:reverse([A | Acc]);
group([], [{_, O} | OT], _, Acc) ->
    lists:reverse([[O | OT] | Acc]).
