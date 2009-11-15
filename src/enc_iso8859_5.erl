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
%% @doc ISO8859-5 encoding
%%
-module(enc_iso8859_5).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() ->
    [iso8859_5, latin5, "iso88595", "latin5"].


%%
%% @doc Encode Unicode to ISO8859-5 string
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
%% @doc Decode ISO8859-5 string to Unicode
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
%% @doc Encode Unicode character to ISO8859-5
%%
encode_char(C) when C >= 0, C =< 16#a0 -> C;
encode_char(C) when C >= 16#0401, C =< 16#040c -> C - 16#360;
encode_char(16#00AD) -> 16#AD;
encode_char(C) when C >= 16#040e, C =< 16#044f -> C - 16#360;
encode_char(16#2116) -> 16#F0;
encode_char(16#0451) -> 16#F1;
encode_char(16#0452) -> 16#F2;
encode_char(16#0453) -> 16#F3;
encode_char(16#0454) -> 16#F4;
encode_char(16#0455) -> 16#F5;
encode_char(16#0456) -> 16#F6;
encode_char(16#0457) -> 16#F7;
encode_char(16#0458) -> 16#F8;
encode_char(16#0459) -> 16#F9;
encode_char(16#045A) -> 16#FA;
encode_char(16#045B) -> 16#FB;
encode_char(16#045C) -> 16#FC;
encode_char(16#00A7) -> 16#FD;
encode_char(16#045E) -> 16#FE;
encode_char(16#045F) -> 16#FF;
encode_char(_) -> badarg.


%%
%% @doc Decode ISO8859-5 character to Unicode
%%
decode_char(C) when C >= 0, C =< 16#a0 -> C;
decode_char(C) when C >= 16#a1, C =< 16#ac -> C + 16#360;
decode_char(16#AD) -> 16#00AD;
decode_char(C) when C >= 16#ae, C =< 16#ef -> C + 16#360;
decode_char(16#F0) -> 16#2116;
decode_char(16#F1) -> 16#0451;
decode_char(16#F2) -> 16#0452;
decode_char(16#F3) -> 16#0453;
decode_char(16#F4) -> 16#0454;
decode_char(16#F5) -> 16#0455;
decode_char(16#F6) -> 16#0456;
decode_char(16#F7) -> 16#0457;
decode_char(16#F8) -> 16#0458;
decode_char(16#F9) -> 16#0459;
decode_char(16#FA) -> 16#045A;
decode_char(16#FB) -> 16#045B;
decode_char(16#FC) -> 16#045C;
decode_char(16#FD) -> 16#00A7;
decode_char(16#FE) -> 16#045E;
decode_char(16#FF) -> 16#045F;
decode_char(_) -> badarg.
