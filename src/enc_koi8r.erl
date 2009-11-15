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
%% @doc KOI8-R encoding
%%
-module(enc_koi8r).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [koi8r, "koi8r"].


%%
%% @doc Encode Unicode to KOI8-R string
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
%% @doc Decode KOI8-R string to Unicode
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
%% @doc Encode Unicode character to KOI8-R
%%
encode_char(C) when C >= 0, C =< 16#7f -> C;
encode_char(16#2500) -> 16#80;
encode_char(16#2502) -> 16#81;
encode_char(16#250C) -> 16#82;
encode_char(16#2510) -> 16#83;
encode_char(16#2514) -> 16#84;
encode_char(16#2518) -> 16#85;
encode_char(16#251C) -> 16#86;
encode_char(16#2524) -> 16#87;
encode_char(16#252C) -> 16#88;
encode_char(16#2534) -> 16#89;
encode_char(16#253C) -> 16#8A;
encode_char(16#2580) -> 16#8B;
encode_char(16#2584) -> 16#8C;
encode_char(16#2588) -> 16#8D;
encode_char(16#258C) -> 16#8E;
encode_char(16#2590) -> 16#8F;
encode_char(16#2591) -> 16#90;
encode_char(16#2592) -> 16#91;
encode_char(16#2593) -> 16#92;
encode_char(16#2320) -> 16#93;
encode_char(16#25A0) -> 16#94;
encode_char(16#2219) -> 16#95;
encode_char(16#221A) -> 16#96;
encode_char(16#2248) -> 16#97;
encode_char(16#2264) -> 16#98;
encode_char(16#2265) -> 16#99;
encode_char(16#00A0) -> 16#9A;
encode_char(16#2321) -> 16#9B;
encode_char(16#00B0) -> 16#9C;
encode_char(16#00B2) -> 16#9D;
encode_char(16#00B7) -> 16#9E;
encode_char(16#00F7) -> 16#9F;
encode_char(16#2550) -> 16#A0;
encode_char(16#2551) -> 16#A1;
encode_char(16#2552) -> 16#A2;
encode_char(16#0451) -> 16#A3;
encode_char(16#2553) -> 16#A4;
encode_char(16#2554) -> 16#A5;
encode_char(16#2555) -> 16#A6;
encode_char(16#2556) -> 16#A7;
encode_char(16#2557) -> 16#A8;
encode_char(16#2558) -> 16#A9;
encode_char(16#2559) -> 16#AA;
encode_char(16#255A) -> 16#AB;
encode_char(16#255B) -> 16#AC;
encode_char(16#255C) -> 16#AD;
encode_char(16#255D) -> 16#AE;
encode_char(16#255E) -> 16#AF;
encode_char(16#255F) -> 16#B0;
encode_char(16#2560) -> 16#B1;
encode_char(16#2561) -> 16#B2;
encode_char(16#0401) -> 16#B3;
encode_char(16#2562) -> 16#B4;
encode_char(16#2563) -> 16#B5;
encode_char(16#2564) -> 16#B6;
encode_char(16#2565) -> 16#B7;
encode_char(16#2566) -> 16#B8;
encode_char(16#2567) -> 16#B9;
encode_char(16#2568) -> 16#BA;
encode_char(16#2569) -> 16#BB;
encode_char(16#256A) -> 16#BC;
encode_char(16#256B) -> 16#BD;
encode_char(16#256C) -> 16#BE;
encode_char(16#00A9) -> 16#BF;
encode_char(16#044E) -> 16#C0;
encode_char(16#0430) -> 16#C1;
encode_char(16#0431) -> 16#C2;
encode_char(16#0446) -> 16#C3;
encode_char(16#0434) -> 16#C4;
encode_char(16#0435) -> 16#C5;
encode_char(16#0444) -> 16#C6;
encode_char(16#0433) -> 16#C7;
encode_char(16#0445) -> 16#C8;
encode_char(16#0438) -> 16#C9;
encode_char(16#0439) -> 16#CA;
encode_char(16#043A) -> 16#CB;
encode_char(16#043B) -> 16#CC;
encode_char(16#043C) -> 16#CD;
encode_char(16#043D) -> 16#CE;
encode_char(16#043E) -> 16#CF;
encode_char(16#043F) -> 16#D0;
encode_char(16#044F) -> 16#D1;
encode_char(16#0440) -> 16#D2;
encode_char(16#0441) -> 16#D3;
encode_char(16#0442) -> 16#D4;
encode_char(16#0443) -> 16#D5;
encode_char(16#0436) -> 16#D6;
encode_char(16#0432) -> 16#D7;
encode_char(16#044C) -> 16#D8;
encode_char(16#044B) -> 16#D9;
encode_char(16#0437) -> 16#DA;
encode_char(16#0448) -> 16#DB;
encode_char(16#044D) -> 16#DC;
encode_char(16#0449) -> 16#DD;
encode_char(16#0447) -> 16#DE;
encode_char(16#044A) -> 16#DF;
encode_char(16#042E) -> 16#E0;
encode_char(16#0410) -> 16#E1;
encode_char(16#0411) -> 16#E2;
encode_char(16#0426) -> 16#E3;
encode_char(16#0414) -> 16#E4;
encode_char(16#0415) -> 16#E5;
encode_char(16#0424) -> 16#E6;
encode_char(16#0413) -> 16#E7;
encode_char(16#0425) -> 16#E8;
encode_char(16#0418) -> 16#E9;
encode_char(16#0419) -> 16#EA;
encode_char(16#041A) -> 16#EB;
encode_char(16#041B) -> 16#EC;
encode_char(16#041C) -> 16#ED;
encode_char(16#041D) -> 16#EE;
encode_char(16#041E) -> 16#EF;
encode_char(16#041F) -> 16#F0;
encode_char(16#042F) -> 16#F1;
encode_char(16#0420) -> 16#F2;
encode_char(16#0421) -> 16#F3;
encode_char(16#0422) -> 16#F4;
encode_char(16#0423) -> 16#F5;
encode_char(16#0416) -> 16#F6;
encode_char(16#0412) -> 16#F7;
encode_char(16#042C) -> 16#F8;
encode_char(16#042B) -> 16#F9;
encode_char(16#0417) -> 16#FA;
encode_char(16#0428) -> 16#FB;
encode_char(16#042D) -> 16#FC;
encode_char(16#0429) -> 16#FD;
encode_char(16#0427) -> 16#FE;
encode_char(16#042A) -> 16#FF;
encode_char(_) -> badarg.


%%
%% @doc Decode CP866 character to Unicode
%%
decode_char(C) when C >= 0, C =< 16#7f -> C;
decode_char(16#80) -> 16#2500;
decode_char(16#81) -> 16#2502;
decode_char(16#82) -> 16#250C;
decode_char(16#83) -> 16#2510;
decode_char(16#84) -> 16#2514;
decode_char(16#85) -> 16#2518;
decode_char(16#86) -> 16#251C;
decode_char(16#87) -> 16#2524;
decode_char(16#88) -> 16#252C;
decode_char(16#89) -> 16#2534;
decode_char(16#8A) -> 16#253C;
decode_char(16#8B) -> 16#2580;
decode_char(16#8C) -> 16#2584;
decode_char(16#8D) -> 16#2588;
decode_char(16#8E) -> 16#258C;
decode_char(16#8F) -> 16#2590;
decode_char(16#90) -> 16#2591;
decode_char(16#91) -> 16#2592;
decode_char(16#92) -> 16#2593;
decode_char(16#93) -> 16#2320;
decode_char(16#94) -> 16#25A0;
decode_char(16#95) -> 16#2219;
decode_char(16#96) -> 16#221A;
decode_char(16#97) -> 16#2248;
decode_char(16#98) -> 16#2264;
decode_char(16#99) -> 16#2265;
decode_char(16#9A) -> 16#00A0;
decode_char(16#9B) -> 16#2321;
decode_char(16#9C) -> 16#00B0;
decode_char(16#9D) -> 16#00B2;
decode_char(16#9E) -> 16#00B7;
decode_char(16#9F) -> 16#00F7;
decode_char(16#A0) -> 16#2550;
decode_char(16#A1) -> 16#2551;
decode_char(16#A2) -> 16#2552;
decode_char(16#A3) -> 16#0451;
decode_char(16#A4) -> 16#2553;
decode_char(16#A5) -> 16#2554;
decode_char(16#A6) -> 16#2555;
decode_char(16#A7) -> 16#2556;
decode_char(16#A8) -> 16#2557;
decode_char(16#A9) -> 16#2558;
decode_char(16#AA) -> 16#2559;
decode_char(16#AB) -> 16#255A;
decode_char(16#AC) -> 16#255B;
decode_char(16#AD) -> 16#255C;
decode_char(16#AE) -> 16#255D;
decode_char(16#AF) -> 16#255E;
decode_char(16#B0) -> 16#255F;
decode_char(16#B1) -> 16#2560;
decode_char(16#B2) -> 16#2561;
decode_char(16#B3) -> 16#0401;
decode_char(16#B4) -> 16#2562;
decode_char(16#B5) -> 16#2563;
decode_char(16#B6) -> 16#2564;
decode_char(16#B7) -> 16#2565;
decode_char(16#B8) -> 16#2566;
decode_char(16#B9) -> 16#2567;
decode_char(16#BA) -> 16#2568;
decode_char(16#BB) -> 16#2569;
decode_char(16#BC) -> 16#256A;
decode_char(16#BD) -> 16#256B;
decode_char(16#BE) -> 16#256C;
decode_char(16#BF) -> 16#00A9;
decode_char(16#C0) -> 16#044E;
decode_char(16#C1) -> 16#0430;
decode_char(16#C2) -> 16#0431;
decode_char(16#C3) -> 16#0446;
decode_char(16#C4) -> 16#0434;
decode_char(16#C5) -> 16#0435;
decode_char(16#C6) -> 16#0444;
decode_char(16#C7) -> 16#0433;
decode_char(16#C8) -> 16#0445;
decode_char(16#C9) -> 16#0438;
decode_char(16#CA) -> 16#0439;
decode_char(16#CB) -> 16#043A;
decode_char(16#CC) -> 16#043B;
decode_char(16#CD) -> 16#043C;
decode_char(16#CE) -> 16#043D;
decode_char(16#CF) -> 16#043E;
decode_char(16#D0) -> 16#043F;
decode_char(16#D1) -> 16#044F;
decode_char(16#D2) -> 16#0440;
decode_char(16#D3) -> 16#0441;
decode_char(16#D4) -> 16#0442;
decode_char(16#D5) -> 16#0443;
decode_char(16#D6) -> 16#0436;
decode_char(16#D7) -> 16#0432;
decode_char(16#D8) -> 16#044C;
decode_char(16#D9) -> 16#044B;
decode_char(16#DA) -> 16#0437;
decode_char(16#DB) -> 16#0448;
decode_char(16#DC) -> 16#044D;
decode_char(16#DD) -> 16#0449;
decode_char(16#DE) -> 16#0447;
decode_char(16#DF) -> 16#044A;
decode_char(16#E0) -> 16#042E;
decode_char(16#E1) -> 16#0410;
decode_char(16#E2) -> 16#0411;
decode_char(16#E3) -> 16#0426;
decode_char(16#E4) -> 16#0414;
decode_char(16#E5) -> 16#0415;
decode_char(16#E6) -> 16#0424;
decode_char(16#E7) -> 16#0413;
decode_char(16#E8) -> 16#0425;
decode_char(16#E9) -> 16#0418;
decode_char(16#EA) -> 16#0419;
decode_char(16#EB) -> 16#041A;
decode_char(16#EC) -> 16#041B;
decode_char(16#ED) -> 16#041C;
decode_char(16#EE) -> 16#041D;
decode_char(16#EF) -> 16#041E;
decode_char(16#F0) -> 16#041F;
decode_char(16#F1) -> 16#042F;
decode_char(16#F2) -> 16#0420;
decode_char(16#F3) -> 16#0421;
decode_char(16#F4) -> 16#0422;
decode_char(16#F5) -> 16#0423;
decode_char(16#F6) -> 16#0416;
decode_char(16#F7) -> 16#0412;
decode_char(16#F8) -> 16#042C;
decode_char(16#F9) -> 16#042B;
decode_char(16#FA) -> 16#0417;
decode_char(16#FB) -> 16#0428;
decode_char(16#FC) -> 16#042D;
decode_char(16#FD) -> 16#0429;
decode_char(16#FE) -> 16#0427;
decode_char(16#FF) -> 16#042A;
decode_char(_) -> badarg.
