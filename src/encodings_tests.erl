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
%% @doc Tests
%%
-module(encodings_tests).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-include_lib("eunit/include/eunit.hrl").


%%
%% Auxiliary functions
%%

test_encoding(Aliases, Filename) ->
    {Bytes, Unicode, DecoderErrors, EncoderErrors} = read_tests(Filename),
    test_aliases(Aliases),
    Alias = hd(Aliases),
    test_encode_decode(Alias, Bytes, Unicode),
    test_errors(DecoderErrors, fun (I) -> encodings:decode(I, Alias) end),
    test_errors(EncoderErrors, fun (I) -> encodings:encode(I, Alias) end),
    ok.


test_aliases([Alias | Aliases]) ->
    {ok, Encoder} = encodings:getencoder(Alias),
    {ok, Decoder} = encodings:getdecoder(Alias),
    test_aliases(Aliases, Encoder, Decoder).

test_aliases([], _, _) ->
    true;
test_aliases([Alias | Aliases], Encoder, Decoder) ->
    {ok, Encoder} = encodings:getencoder(Alias),
    {ok, Decoder} = encodings:getdecoder(Alias),
    test_aliases(Aliases, Encoder, Decoder).


test_encode_decode(Alias, Bytes, Unicode) ->
    Unicode = encodings:decode(Bytes, Alias),
    Bytes = encodings:encode(Unicode, Alias),
    {ok, Encoder} = encodings:getencoder(Alias),
    {ok, Decoder} = encodings:getdecoder(Alias),
    Unicode = Decoder(Bytes),
    Bytes = Encoder(Unicode).


test_errors([], _) ->
    ok;
test_errors([{Input, Result} | Errors], Fun) ->
    Result = Fun(Input),
    test_errors(Errors, Fun).


read_tests(Filename) ->
    Path = filename:join([filename:dirname(?FILE), "tests", Filename]),
    {ok, Terms} = file:consult(Path),
    read_tests(Terms, <<>>, [], [], []).

read_tests([], String, Unicode, DecodeErrors, EncodeErrors) ->
    {String, lists:reverse(Unicode), DecodeErrors, EncodeErrors};
read_tests([{decode, Error} | Tail],
        String, Unicode, DecodeErrors, EncodeErrors) ->
    read_tests(Tail, String, Unicode, [Error | DecodeErrors], EncodeErrors);
read_tests([{encode, Error} | Tail],
        String, Unicode, DecodeErrors, EncodeErrors) ->
    read_tests(Tail, String, Unicode, DecodeErrors, [Error | EncodeErrors]);
read_tests([{Byte, Char} | Tail],
        String, Unicode, DecodeErrors, EncodeErrors) ->
    read_tests(Tail, <<String/binary,Byte>>,
        [Char | Unicode], DecodeErrors, EncodeErrors).


test_utf8(Aliases) ->
    test_aliases(Aliases),
    Bytes = <<0,
        16#7f,
        16#c2, 16#80,
        16#df, 16#bf,
        16#e0, 16#a0, 16#80,
        16#ef, 16#bf, 16#bd,
        16#f0, 16#90, 16#80, 16#80,
        16#f4, 16#8f, 16#bf, 16#bf>>,
    Unicode = [0, 16#7f, 16#80, 16#7ff, 16#800, 16#fffd, 16#10000, 16#10ffff],
    test_encode_decode(hd(Aliases), Bytes, Unicode),
    ok.


test_register() ->
    {error, badarg} = encodings:getencoder("encoding"),
    Encoder = fun (U) -> "Encoded " ++ U end,
    Decoder = fun (S) -> "Decoded " ++ S end,
    encodings:register(
        {functions, ["encoding", "an encoding"], Encoder, Decoder}),
    {ok, Encoder} = encodings:getencoder("encoding"),
    {ok, Decoder} = encodings:getdecoder("encoding"),
    {ok, Encoder} = encodings:getencoder("anencoding"),
    {ok, Decoder} = encodings:getdecoder("anencoding"),
    {ok, Encoder} = encodings:getencoder("an encoding"),
    {ok, Decoder} = encodings:getdecoder("an encoding"),
    "Encoded Unicode" = Encoder("Unicode"),
    "Decoded String" = Decoder("String"),
    ok.


test_register_module() ->
    encodings:register({module, enc_ascii}),
    {ok, _} = encodings:getencoder("ascii"),
    {ok, _} = encodings:getdecoder("ascii"),
    ok.


test_registration_override() ->
    Encoder = fun (U) -> "Encoded " ++ U end,
    Decoder = fun (S) -> "Decoded " ++ S end,
    false = encodings:register({functions, ["ascii"], Encoder, Decoder}),
    true = encodings:register(
        {functions, ["ascii"], Encoder, Decoder}, [override]),
    {ok, Encoder} = encodings:getencoder("ascii"),
    {ok, Decoder} = encodings:getdecoder("ascii"),
    "Encoded Unicode" = Encoder("Unicode"),
    "Decoded String" = Decoder("String"),
    ok.


%%
%% Tests
%%

setup() ->
    encodings:start().

cleanup(_) ->
    encodings:stop().


encodings_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertEqual(ok, test_encoding([ascii, "ascii", "ASCII"], "ascii.txt")),
    ?_assertEqual(ok, test_encoding(
        [iso8859_1, "iso-88591", latin1, "latin-1"], "iso8859-1.txt")),
    ?_assertEqual(ok, test_encoding(
        [cp1251, windows1251, "cp_1251", "windows 1251"], "cp1251.txt")),
    ?_assertEqual(ok, test_encoding([cp866, "cp866"], "cp866.txt")),
    ?_assertEqual(ok, test_encoding([koi8r, "koi8r", "KOI8-R"], "koi8-r.txt")),
    ?_assertEqual(ok, test_encoding(
        [iso8859_5, "iso8859-5", latin5, "latin-5"], "iso8859-5.txt"))
    ]}.


registration_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertEqual(ok, test_register()),
    ?_assertEqual(ok, test_register_module()),
    ?_assertEqual(ok, test_registration_override())
    ]}.


utf8_test_() -> {setup, fun setup/0, fun cleanup/1, [
    ?_assertEqual(ok, test_utf8([utf8, "utf8"]))
    ]}.
