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
%% @doc ISO 8859-1 encoding
%%
-module(iso8859_1).

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [iso8859_1, "iso88591", latin1, "latin1"].


%%
%% @doc Encode Unicode to ISO 8859-1 string
%%
encode(Unicode) ->
    encode(Unicode, <<>>).

encode("", Result) ->
    Result;
encode([C | Tail], Result) when C >= 0, C =< 16#ff ->
    encode(Tail, <<Result/binary,C>>);
encode(Input, Result) ->
    {error, Result, Input}.


%%
%% @doc Decode ISO 8859-1 string to Unicode
%%
decode(String) ->
    binary_to_list(String).
