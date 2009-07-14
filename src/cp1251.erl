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
%% @doc CP1251 encoding
%%
-module(cp1251).

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [cp1251, windows1251, "cp1251", "windows1251"].


%%
%% @doc Encode Unicode to Windows-1251 string
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
%% @doc Decode Windows-1251 string to Unicode
%%
decode(String) ->
    decode(String, "").

decode("", Result) ->
    lists:reverse(Result);
decode([C | Tail]=Input, Result) ->
    case decode_char(C) of
        badarg ->
            {error, Result, Input};
        E ->
            decode(Tail, [E | Result])
    end.


%%
%% @doc Encode Unicode character to Windows-1251
%%
encode_char(C) when C >= 0, C < 16#80 -> C;
encode_char(16#0402) -> 16#80;
encode_char(16#0403) -> 16#81;
encode_char(16#201a) -> 16#82;
encode_char(16#0453) -> 16#83;
encode_char(16#201e) -> 16#84;
encode_char(16#2026) -> 16#85;
encode_char(16#2020) -> 16#86;
encode_char(16#2021) -> 16#87;
encode_char(16#20ac) -> 16#88;
encode_char(16#2030) -> 16#89;
encode_char(16#0409) -> 16#8a;
encode_char(16#2039) -> 16#8b;
encode_char(16#040a) -> 16#8c;
encode_char(16#040c) -> 16#8d;
encode_char(16#040b) -> 16#8e;
encode_char(16#040f) -> 16#8f;
encode_char(16#0452) -> 16#90;
encode_char(16#2018) -> 16#91;
encode_char(16#2019) -> 16#92;
encode_char(16#201c) -> 16#93;
encode_char(16#201d) -> 16#94;
encode_char(16#2022) -> 16#95;
encode_char(16#2013) -> 16#96;
encode_char(16#2014) -> 16#97;
encode_char(16#fffe) -> 16#98;
encode_char(16#2122) -> 16#99;
encode_char(16#0459) -> 16#9a;
encode_char(16#203a) -> 16#9b;
encode_char(16#045a) -> 16#9c;
encode_char(16#045c) -> 16#9d;
encode_char(16#045b) -> 16#9e;
encode_char(16#045f) -> 16#9f;
encode_char(16#00a0) -> 16#a0;
encode_char(16#040e) -> 16#a1;
encode_char(16#045e) -> 16#a2;
encode_char(16#0408) -> 16#a3;
encode_char(16#00a4) -> 16#a4;
encode_char(16#0490) -> 16#a5;
encode_char(16#00a6) -> 16#a6;
encode_char(16#00a7) -> 16#a7;
encode_char(16#0401) -> 16#a8;
encode_char(16#00a9) -> 16#a9;
encode_char(16#0404) -> 16#aa;
encode_char(16#00ab) -> 16#ab;
encode_char(16#00ac) -> 16#ac;
encode_char(16#00ad) -> 16#ad;
encode_char(16#00ae) -> 16#ae;
encode_char(16#0407) -> 16#af;
encode_char(16#00b0) -> 16#b0;
encode_char(16#00b1) -> 16#b1;
encode_char(16#0406) -> 16#b2;
encode_char(16#0456) -> 16#b3;
encode_char(16#0491) -> 16#b4;
encode_char(16#00b5) -> 16#b5;
encode_char(16#00b6) -> 16#b6;
encode_char(16#00b7) -> 16#b7;
encode_char(16#0451) -> 16#b8;
encode_char(16#2116) -> 16#b9;
encode_char(16#0454) -> 16#ba;
encode_char(16#00bb) -> 16#bb;
encode_char(16#0458) -> 16#bc;
encode_char(16#0405) -> 16#bd;
encode_char(16#0455) -> 16#be;
encode_char(16#0457) -> 16#bf;
encode_char(C) when C >= 16#0410, C =< 16#044f -> C - 16#350;
encode_char(_) -> badarg.


%%
%% @doc Decode Windows-1251 character to Unicode
%%
decode_char(C) when C >= 0, C < 16#80 -> C;
decode_char(16#80) -> 16#0402;
decode_char(16#81) -> 16#0403;
decode_char(16#82) -> 16#201a;
decode_char(16#83) -> 16#0453;
decode_char(16#84) -> 16#201e;
decode_char(16#85) -> 16#2026;
decode_char(16#86) -> 16#2020;
decode_char(16#87) -> 16#2021;
decode_char(16#88) -> 16#20ac;
decode_char(16#89) -> 16#2030;
decode_char(16#8a) -> 16#0409;
decode_char(16#8b) -> 16#2039;
decode_char(16#8c) -> 16#040a;
decode_char(16#8d) -> 16#040c;
decode_char(16#8e) -> 16#040b;
decode_char(16#8f) -> 16#040f;
decode_char(16#90) -> 16#0452;
decode_char(16#91) -> 16#2018;
decode_char(16#92) -> 16#2019;
decode_char(16#93) -> 16#201c;
decode_char(16#94) -> 16#201d;
decode_char(16#95) -> 16#2022;
decode_char(16#96) -> 16#2013;
decode_char(16#97) -> 16#2014;
decode_char(16#98) -> 16#fffe;
decode_char(16#99) -> 16#2122;
decode_char(16#9a) -> 16#0459;
decode_char(16#9b) -> 16#203a;
decode_char(16#9c) -> 16#045a;
decode_char(16#9d) -> 16#045c;
decode_char(16#9e) -> 16#045b;
decode_char(16#9f) -> 16#045f;
decode_char(16#a0) -> 16#00a0;
decode_char(16#a1) -> 16#040e;
decode_char(16#a2) -> 16#045e;
decode_char(16#a3) -> 16#0408;
decode_char(16#a4) -> 16#00a4;
decode_char(16#a5) -> 16#0490;
decode_char(16#a6) -> 16#00a6;
decode_char(16#a7) -> 16#00a7;
decode_char(16#a8) -> 16#0401;
decode_char(16#a9) -> 16#00a9;
decode_char(16#aa) -> 16#0404;
decode_char(16#ab) -> 16#00ab;
decode_char(16#ac) -> 16#00ac;
decode_char(16#ad) -> 16#00ad;
decode_char(16#ae) -> 16#00ae;
decode_char(16#af) -> 16#0407;
decode_char(16#b0) -> 16#00b0;
decode_char(16#b1) -> 16#00b1;
decode_char(16#b2) -> 16#0406;
decode_char(16#b3) -> 16#0456;
decode_char(16#b4) -> 16#0491;
decode_char(16#b5) -> 16#00b5;
decode_char(16#b6) -> 16#00b6;
decode_char(16#b7) -> 16#00b7;
decode_char(16#b8) -> 16#0451;
decode_char(16#b9) -> 16#2116;
decode_char(16#ba) -> 16#0454;
decode_char(16#bb) -> 16#00bb;
decode_char(16#bc) -> 16#0458;
decode_char(16#bd) -> 16#0405;
decode_char(16#be) -> 16#0455;
decode_char(16#bf) -> 16#0457;
decode_char(C) when C >= 16#c0, C =< 16#ff -> C + 16#350;
decode_char(_) -> badarg.
