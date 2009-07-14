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
encode([C | Tail]=Input, Result) ->
    case encode_char(C) of
        badarg ->
            {error, Result, Input};
        E ->
            encode(Tail, [E | Result])
    end.


%%
%% @doc Decode ASCII string to Unicode
%%
decode(String) ->
    decode(String, "").

decode("", Result) ->
    lists:reverse(Result);
decode([C | Tail]=Input, Result) ->
    case decode_char(C) of
        badarg ->
            {error, Result, Input};
        D ->
            decode(Tail, [D | Result])
    end.


%%
%% @doc Encode Unicode character to ASCII
%%
encode_char(C) when C =< 16#7f -> C;
encode_char(_) -> badarg.


%%
%% @doc Decode ASCII character to Unicode
%%
decode_char(C) when C =< 16#7f -> C;
decode_char(_) -> badarg.
