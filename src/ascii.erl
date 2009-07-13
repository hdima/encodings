-module(ascii).

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


aliases() ->
    [ascii, "ascii"].


encode(Unicode) ->
    encode(Unicode, "").

encode("", Result) ->
    lists:reverse(Result);
encode([C | Tail], Result) ->
    encode(Tail, [encode_char(C) | Result]).


decode(String) ->
    decode(String, "").

decode("", Result) ->
    lists:reverse(Result);
decode([C | Tail], Result) ->
    decode(Tail, [decode_char(C) | Result]).


encode_char(C) when C =< 16#ff -> C;
encode_char(C) -> erlang:error(badarg, [C]).


decode_char(C) when C =< 16#ff -> C;
decode_char(C) -> erlang:error(badarg, [C]).
