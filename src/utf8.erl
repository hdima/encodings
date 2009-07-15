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
%% @doc UTF-8 encoding
%%
-module(utf8).

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [utf8, "utf8"].


%%
%% @doc Encode Unicode to UTF-8 string
%%
encode(Unicode) ->
    encode(Unicode, "").

encode("", Result) ->
    lists:reverse(Result);
encode([C | Tail], Result) when C >= 0, C =< 16#7f ->
    encode(Tail, [C | Result]);
encode([C | Tail], Result) when C >= 16#80, C =< 16#7ff ->
    todo;
encode([C | Tail], Result) when C >= 16#800, C =< 16#ffff ->
    todo;
encode([C | Tail], Result) when C >= 16#10000, C =< 16#10ffff ->
    todo;
encode(Input, Result) ->
    {error, Result, Input}.


%%
%% @doc Decode UTF-8 string to Unicode
%%
decode(String) ->
    decode(String, "").

decode("", Result) ->
    lists:reverse(Result);
decode([C | Tail], Result) when C >= 0, C =< 16#7f ->
    decode(Tail, [C | Result]);
decode([C1, C2 | Tail], Rsult)
        when C1 >= 16#c2, C1 =< 16#df, C2 >= 16#80, C2 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Rsult)
        when C1 =:= 16#e0, C2 >= 16#a0, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Rsult)
        when C1 =:= 16#ed, C2 >= 16#80, C2 =< 16#9f,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Rsult)
        when C1 >= 16#e1, C1 =< 16#ec, C2 >= 16#80, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Rsult)
        when C1 >= 16#ee, C1 =< 16#ef, C2 >= 16#80, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3, C4 | Tail], Rsult)
        when C1 =:= 16#f4, C2 >= 16#80, C2 =< 16#8f,
        C3 >= 16#80, C3 =< 16#bf, C4 >= 16#80, C4 =< 16#bf ->
    todo;
decode([C1, C2, C3, C4 | Tail], Rsult)
        when C1 =:= 16#f0, C2 >= 16#90, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf, C4 >= 16#80, C4 =< 16#bf ->
    todo;
decode([C1, C2, C3, C4 | Tail], Rsult)
        when C1 >= 16#f1, C1 =< 16#f3, C2 >= 16#80, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf, C4 >= 16#80, C4 =< 16#bf ->
    todo;
decode(Input, Result) ->
    {error, Result, Input}.
