-module(encodings).
-export([encode/2, decode/2]).


% TODO: We can use ets for store encoders/decoders. And in this case
% registration also will be allowed.

encode(Unicode, cp1251) ->
    cp1251:encode(Unicode);
encode(Unicode, Encoding) ->
    erlang:error(badarg, [Unicode, Encoding]).


decode(String, cp1251) ->
    cp1251:decode(String);
decode(String, Encoding) ->
    erlang:error(badarg, [String, Encoding]).
