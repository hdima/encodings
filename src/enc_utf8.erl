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
%% @doc UTF-8 encoding
%%
-module(enc_utf8).

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
    encode(Tail, [C band 16#bf, C bsr 6 band 16#df | Result]);
encode([C | Tail], Result) when C >= 16#800, C =< 16#ffff ->
    encode(Tail, [C band 16#bf, C bsr 6 band 16#bf, C bsr 12 band 16#ef
        | Result]);
encode([C | Tail], Result) when C >= 16#10000, C =< 16#10ffff ->
    encode(Tail, [C band 16#bf, C bsr 6 band 16#bf, C bsr 12 band 16#bf,
        C bsr 18 band 16#f7 | Result]);
encode(Input, Result) ->
    {error, Result, Input}.


%%
%% @doc Decode UTF-8 string to Unicode
%%
decode(String) ->
    decode(String, "").

% TODO: What about incomplete strings?
decode("", Result) ->
    lists:reverse(Result);
decode([C | Tail], Result) when C >= 0, C =< 16#7f ->
    decode(Tail, [C | Result]);
decode([C1, C2 | Tail], Result)
        when C1 >= 16#c2, C1 =< 16#df, C2 >= 16#80, C2 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Result)
        when C1 =:= 16#e0, C2 >= 16#a0, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Result)
        when C1 =:= 16#ed, C2 >= 16#80, C2 =< 16#9f,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Result)
        when C1 >= 16#e1, C1 =< 16#ec, C2 >= 16#80, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3 | Tail], Result)
        when C1 >= 16#ee, C1 =< 16#ef, C2 >= 16#80, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf ->
    todo;
decode([C1, C2, C3, C4 | Tail], Result)
        when C1 =:= 16#f4, C2 >= 16#80, C2 =< 16#8f,
        C3 >= 16#80, C3 =< 16#bf, C4 >= 16#80, C4 =< 16#bf ->
    todo;
decode([C1, C2, C3, C4 | Tail], Result)
        when C1 =:= 16#f0, C2 >= 16#90, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf, C4 >= 16#80, C4 =< 16#bf ->
    todo;
decode([C1, C2, C3, C4 | Tail], Result)
        when C1 >= 16#f1, C1 =< 16#f3, C2 >= 16#80, C2 =< 16#bf,
        C3 >= 16#80, C3 =< 16#bf, C4 >= 16#80, C4 =< 16#bf ->
    todo;
decode(Input, Result) ->
    {error, Result, Input}.
