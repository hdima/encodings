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
%% @doc ISO 8859-1 encoding
%%
-module(enc_iso8859_1).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-behaviour(encodings).

-export([aliases/0, encode/1, decode/1]).


%%
%% @doc Return encoding aliases
%%
aliases() -> [
    latin1,
    iso8859_1,
    "8859",
    "cp819",
    "csisolatin1",
    "ibm819",
    "iso8859",
    "iso8859_1",
    "iso_8859_1",
    "iso_8859_1_1987",
    "iso_ir_100",
    "l1",
    "latin",
    "latin1",
    "latin_1"
    ].


%%
%% @doc Encode Unicode to ISO 8859-1 string
%%
encode(Unicode) when is_list(Unicode) ->
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
decode(String) when is_binary(String) ->
    binary_to_list(String).
