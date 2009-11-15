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

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [cp866, "cp866"].


%%
%% @doc Encode Unicode to CP866 string
%%
encode(Unicode) ->
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
decode(String) ->
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
decode_char(C) when C >= 0, C =< 16#7f -> C;
decode_char(C) when C >= 16#80, C =< 16#af -> C + 16#390;
decode_char(16#B0) -> 16#2591;
decode_char(16#B1) -> 16#2592;
decode_char(16#B2) -> 16#2593;
decode_char(16#B3) -> 16#2502;
decode_char(16#B4) -> 16#2524;
decode_char(16#B5) -> 16#2561;
decode_char(16#B6) -> 16#2562;
decode_char(16#B7) -> 16#2556;
decode_char(16#B8) -> 16#2555;
decode_char(16#B9) -> 16#2563;
decode_char(16#BA) -> 16#2551;
decode_char(16#BB) -> 16#2557;
decode_char(16#BC) -> 16#255D;
decode_char(16#BD) -> 16#255C;
decode_char(16#BE) -> 16#255B;
decode_char(16#BF) -> 16#2510;
decode_char(16#C0) -> 16#2514;
decode_char(16#C1) -> 16#2534;
decode_char(16#C2) -> 16#252C;
decode_char(16#C3) -> 16#251C;
decode_char(16#C4) -> 16#2500;
decode_char(16#C5) -> 16#253C;
decode_char(16#C6) -> 16#255E;
decode_char(16#C7) -> 16#255F;
decode_char(16#C8) -> 16#255A;
decode_char(16#C9) -> 16#2554;
decode_char(16#CA) -> 16#2569;
decode_char(16#CB) -> 16#2566;
decode_char(16#CC) -> 16#2560;
decode_char(16#CD) -> 16#2550;
decode_char(16#CE) -> 16#256C;
decode_char(16#CF) -> 16#2567;
decode_char(16#D0) -> 16#2568;
decode_char(16#D1) -> 16#2564;
decode_char(16#D2) -> 16#2565;
decode_char(16#D3) -> 16#2559;
decode_char(16#D4) -> 16#2558;
decode_char(16#D5) -> 16#2552;
decode_char(16#D6) -> 16#2553;
decode_char(16#D7) -> 16#256B;
decode_char(16#D8) -> 16#256A;
decode_char(16#D9) -> 16#2518;
decode_char(16#DA) -> 16#250C;
decode_char(16#DB) -> 16#2588;
decode_char(16#DC) -> 16#2584;
decode_char(16#DD) -> 16#258C;
decode_char(16#DE) -> 16#2590;
decode_char(16#DF) -> 16#2580;
decode_char(C) when C >= 16#e0, C =< 16#ef -> C + 16#360;
decode_char(16#F0) -> 16#0401;
decode_char(16#F1) -> 16#0451;
decode_char(16#F2) -> 16#0404;
decode_char(16#F3) -> 16#0454;
decode_char(16#F4) -> 16#0407;
decode_char(16#F5) -> 16#0457;
decode_char(16#F6) -> 16#040E;
decode_char(16#F7) -> 16#045E;
decode_char(16#F8) -> 16#00B0;
decode_char(16#F9) -> 16#2219;
decode_char(16#FA) -> 16#00B7;
decode_char(16#FB) -> 16#221A;
decode_char(16#FC) -> 16#2116;
decode_char(16#FD) -> 16#00A4;
decode_char(16#FE) -> 16#25A0;
decode_char(16#FF) -> 16#00A0;
decode_char(_) -> badarg.
