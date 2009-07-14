%% Copyright (C) 2009 Dmitry Vasiliev <dima@hlabs.spb.ru>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @doc ASCII encoding
%%
-module(ascii).

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [ascii, "ascii"].


%%
%% @doc Encode Unicode to ASCII string
%%
encode(Unicode) ->
    encode(Unicode, "").

encode("", Result) ->
    lists:reverse(Result);
encode([C | Tail], Result) ->
    encode(Tail, [encode_char(C) | Result]).


%%
%% @doc Decode ASCII string to Unicode
%%
decode(String) ->
    decode(String, "").

decode("", Result) ->
    lists:reverse(Result);
decode([C | Tail], Result) ->
    decode(Tail, [decode_char(C) | Result]).


%%
%% @doc Encode Unicode character to ASCII string
%%
encode_char(C) when C =< 16#ff -> C;
encode_char(C) -> erlang:error(badarg, [C]).


%%
%% @doc Decode ASCII character to Unicode
%%
decode_char(C) when C =< 16#ff -> C;
decode_char(C) -> erlang:error(badarg, [C]).
