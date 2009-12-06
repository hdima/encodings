%% Copyright (c) 2009, Dmitry Vasiliev <dima@hlabs.spb.ru>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%% * Neither the name of the copyright holders nor the names of its
%%   contributors may be used to endorse or promote products derived from this
%%   software without specific prior written permission. 
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%
%% @doc CP1251 encoding
%%
-module(enc_cp1251).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-behaviour(encodings).

-compile(inline).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() -> [
    cp1251,
    windows1251,
    "cp1251",
    "1251",
    "windows_1251"
    ].


%%
%% @doc Encode Unicode to Windows-1251 string
%%
encode(Unicode) when is_list(Unicode) ->
    encode(Unicode, <<>>).

encode("", Result) ->
    Result;
encode([C | Tail]=Input, Result) ->
    case encode_char(C) of
        badarg ->
            {error, Result, Input};
        E ->
            encode(Tail, <<Result/binary,E>>)
    end.


%%
%% @doc Decode Windows-1251 string to Unicode
%%
decode(String) when is_binary(String) ->
    decode(String, "").

decode(<<>>, Result) ->
    lists:reverse(Result);
decode(<<C,Tail/binary>>=Input, Result) ->
    case decode_char(C) of
        badarg ->
            {error, lists:reverse(Result), Input};
        D ->
            decode(Tail, [D | Result])
    end.


%%
%% @doc Encode Unicode character to Windows-1251
%%
encode_char(C) when C >= 0, C =< 16#7f -> C;
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
decode_char(C) ->
    element(C + 1, {
        16#0000, 16#0001, 16#0002, 16#0003, 16#0004, 16#0005, 16#0006, 16#0007,
        16#0008, 16#0009, 16#000A, 16#000B, 16#000C, 16#000D, 16#000E, 16#000F,
        16#0010, 16#0011, 16#0012, 16#0013, 16#0014, 16#0015, 16#0016, 16#0017,
        16#0018, 16#0019, 16#001A, 16#001B, 16#001C, 16#001D, 16#001E, 16#001F,
        16#0020, 16#0021, 16#0022, 16#0023, 16#0024, 16#0025, 16#0026, 16#0027,
        16#0028, 16#0029, 16#002A, 16#002B, 16#002C, 16#002D, 16#002E, 16#002F,
        16#0030, 16#0031, 16#0032, 16#0033, 16#0034, 16#0035, 16#0036, 16#0037,
        16#0038, 16#0039, 16#003A, 16#003B, 16#003C, 16#003D, 16#003E, 16#003F,
        16#0040, 16#0041, 16#0042, 16#0043, 16#0044, 16#0045, 16#0046, 16#0047,
        16#0048, 16#0049, 16#004A, 16#004B, 16#004C, 16#004D, 16#004E, 16#004F,
        16#0050, 16#0051, 16#0052, 16#0053, 16#0054, 16#0055, 16#0056, 16#0057,
        16#0058, 16#0059, 16#005A, 16#005B, 16#005C, 16#005D, 16#005E, 16#005F,
        16#0060, 16#0061, 16#0062, 16#0063, 16#0064, 16#0065, 16#0066, 16#0067,
        16#0068, 16#0069, 16#006A, 16#006B, 16#006C, 16#006D, 16#006E, 16#006F,
        16#0070, 16#0071, 16#0072, 16#0073, 16#0074, 16#0075, 16#0076, 16#0077,
        16#0078, 16#0079, 16#007A, 16#007B, 16#007C, 16#007D, 16#007E, 16#007F,
        16#0402, 16#0403, 16#201A, 16#0453, 16#201E, 16#2026, 16#2020, 16#2021,
        16#20AC, 16#2030, 16#0409, 16#2039, 16#040A, 16#040C, 16#040B, 16#040F,
        16#0452, 16#2018, 16#2019, 16#201C, 16#201D, 16#2022, 16#2013, 16#2014,
        badarg, 16#2122, 16#0459, 16#203A, 16#045A, 16#045C, 16#045B, 16#045F,
        16#00A0, 16#040E, 16#045E, 16#0408, 16#00A4, 16#0490, 16#00A6, 16#00A7,
        16#0401, 16#00A9, 16#0404, 16#00AB, 16#00AC, 16#00AD, 16#00AE, 16#0407,
        16#00B0, 16#00B1, 16#0406, 16#0456, 16#0491, 16#00B5, 16#00B6, 16#00B7,
        16#0451, 16#2116, 16#0454, 16#00BB, 16#0458, 16#0405, 16#0455, 16#0457,
        16#0410, 16#0411, 16#0412, 16#0413, 16#0414, 16#0415, 16#0416, 16#0417,
        16#0418, 16#0419, 16#041A, 16#041B, 16#041C, 16#041D, 16#041E, 16#041F,
        16#0420, 16#0421, 16#0422, 16#0423, 16#0424, 16#0425, 16#0426, 16#0427,
        16#0428, 16#0429, 16#042A, 16#042B, 16#042C, 16#042D, 16#042E, 16#042F,
        16#0430, 16#0431, 16#0432, 16#0433, 16#0434, 16#0435, 16#0436, 16#0437,
        16#0438, 16#0439, 16#043A, 16#043B, 16#043C, 16#043D, 16#043E, 16#043F,
        16#0440, 16#0441, 16#0442, 16#0443, 16#0444, 16#0445, 16#0446, 16#0447,
        16#0448, 16#0449, 16#044A, 16#044B, 16#044C, 16#044D, 16#044E, 16#044F
    }).
