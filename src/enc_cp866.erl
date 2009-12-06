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
%% @doc CP866 encoding
%%
-module(enc_cp866).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-behaviour(encodings).

-compile(inline).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() -> [
    cp866,
    "cp866",
    "866",
    "ibm866",
    "csibm866"
    ].


%%
%% @doc Encode Unicode to CP866 string
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
%% @doc Decode CP866 string to Unicode
%%
decode(String) when is_binary(String) ->
    [decode_char(C) || <<C>> <= String].


%%
%% @doc Encode Unicode character to CP866
%%
encode_char(C) when C >= 0, C =< 16#7f -> C;
encode_char(C) when C >= 16#0410, C =< 16#043f -> C - 16#390;
encode_char(16#2591) -> 16#B0;
encode_char(16#2592) -> 16#B1;
encode_char(16#2593) -> 16#B2;
encode_char(16#2502) -> 16#B3;
encode_char(16#2524) -> 16#B4;
encode_char(16#2561) -> 16#B5;
encode_char(16#2562) -> 16#B6;
encode_char(16#2556) -> 16#B7;
encode_char(16#2555) -> 16#B8;
encode_char(16#2563) -> 16#B9;
encode_char(16#2551) -> 16#BA;
encode_char(16#2557) -> 16#BB;
encode_char(16#255D) -> 16#BC;
encode_char(16#255C) -> 16#BD;
encode_char(16#255B) -> 16#BE;
encode_char(16#2510) -> 16#BF;
encode_char(16#2514) -> 16#C0;
encode_char(16#2534) -> 16#C1;
encode_char(16#252C) -> 16#C2;
encode_char(16#251C) -> 16#C3;
encode_char(16#2500) -> 16#C4;
encode_char(16#253C) -> 16#C5;
encode_char(16#255E) -> 16#C6;
encode_char(16#255F) -> 16#C7;
encode_char(16#255A) -> 16#C8;
encode_char(16#2554) -> 16#C9;
encode_char(16#2569) -> 16#CA;
encode_char(16#2566) -> 16#CB;
encode_char(16#2560) -> 16#CC;
encode_char(16#2550) -> 16#CD;
encode_char(16#256C) -> 16#CE;
encode_char(16#2567) -> 16#CF;
encode_char(16#2568) -> 16#D0;
encode_char(16#2564) -> 16#D1;
encode_char(16#2565) -> 16#D2;
encode_char(16#2559) -> 16#D3;
encode_char(16#2558) -> 16#D4;
encode_char(16#2552) -> 16#D5;
encode_char(16#2553) -> 16#D6;
encode_char(16#256B) -> 16#D7;
encode_char(16#256A) -> 16#D8;
encode_char(16#2518) -> 16#D9;
encode_char(16#250C) -> 16#DA;
encode_char(16#2588) -> 16#DB;
encode_char(16#2584) -> 16#DC;
encode_char(16#258C) -> 16#DD;
encode_char(16#2590) -> 16#DE;
encode_char(16#2580) -> 16#DF;
encode_char(C) when C >= 16#0440, C =< 16#044f -> C - 16#360;
encode_char(16#0401) -> 16#F0;
encode_char(16#0451) -> 16#F1;
encode_char(16#0404) -> 16#F2;
encode_char(16#0454) -> 16#F3;
encode_char(16#0407) -> 16#F4;
encode_char(16#0457) -> 16#F5;
encode_char(16#040E) -> 16#F6;
encode_char(16#045E) -> 16#F7;
encode_char(16#00B0) -> 16#F8;
encode_char(16#2219) -> 16#F9;
encode_char(16#00B7) -> 16#FA;
encode_char(16#221A) -> 16#FB;
encode_char(16#2116) -> 16#FC;
encode_char(16#00A4) -> 16#FD;
encode_char(16#25A0) -> 16#FE;
encode_char(16#00A0) -> 16#FF;
encode_char(_) -> badarg.


%%
%% @doc Decode CP866 character to Unicode
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
        16#0410, 16#0411, 16#0412, 16#0413, 16#0414, 16#0415, 16#0416, 16#0417,
        16#0418, 16#0419, 16#041A, 16#041B, 16#041C, 16#041D, 16#041E, 16#041F,
        16#0420, 16#0421, 16#0422, 16#0423, 16#0424, 16#0425, 16#0426, 16#0427,
        16#0428, 16#0429, 16#042A, 16#042B, 16#042C, 16#042D, 16#042E, 16#042F,
        16#0430, 16#0431, 16#0432, 16#0433, 16#0434, 16#0435, 16#0436, 16#0437,
        16#0438, 16#0439, 16#043A, 16#043B, 16#043C, 16#043D, 16#043E, 16#043F,
        16#2591, 16#2592, 16#2593, 16#2502, 16#2524, 16#2561, 16#2562, 16#2556,
        16#2555, 16#2563, 16#2551, 16#2557, 16#255D, 16#255C, 16#255B, 16#2510,
        16#2514, 16#2534, 16#252C, 16#251C, 16#2500, 16#253C, 16#255E, 16#255F,
        16#255A, 16#2554, 16#2569, 16#2566, 16#2560, 16#2550, 16#256C, 16#2567,
        16#2568, 16#2564, 16#2565, 16#2559, 16#2558, 16#2552, 16#2553, 16#256B,
        16#256A, 16#2518, 16#250C, 16#2588, 16#2584, 16#258C, 16#2590, 16#2580,
        16#0440, 16#0441, 16#0442, 16#0443, 16#0444, 16#0445, 16#0446, 16#0447,
        16#0448, 16#0449, 16#044A, 16#044B, 16#044C, 16#044D, 16#044E, 16#044F,
        16#0401, 16#0451, 16#0404, 16#0454, 16#0407, 16#0457, 16#040E, 16#045E,
        16#00B0, 16#2219, 16#00B7, 16#221A, 16#2116, 16#00A4, 16#25A0, 16#00A0
    }).
