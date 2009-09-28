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

-include_lib("eunit/include/eunit.hrl").


%%
%% Auxiliary functions
%%

test_encoding(Aliases, Filename) ->
    encodings:start(),
    {Bytes, Unicode, DecoderErrors, EncoderErrors} = read_tests(Filename),
    test_aliases(Aliases),
    test_encode_decode(hd(Aliases), Bytes, Unicode),
    test_decoder_errors(hd(Aliases), DecoderErrors),
    test_encoder_errors(hd(Aliases), EncoderErrors),
    encodings:stop(),
    true.


test_aliases([Alias | Aliases]) ->
    {Encoder, Decoder} = encodings:get_encoder_decoder(Alias),
    test_aliases(Aliases, Encoder, Decoder).

test_aliases([], _, _) ->
    ok;
test_aliases([Alias | Aliases], Encoder, Decoder) ->
    {Encoder, Decoder} = encodings:get_encoder_decoder(Alias),
    test_aliases(Aliases, Encoder, Decoder).


test_encode_decode(Alias, Bytes, Unicode) ->
    Unicode = encodings:decode(Bytes, Alias),
    Bytes = encodings:encode(Unicode, Alias),
    {Encoder, Decoder} = encodings:get_encoder_decoder(Alias),
    Unicode = Decoder(Bytes),
    Bytes = Encoder(Unicode).


test_decoder_errors(_, []) ->
    ok;
test_decoder_errors(Alias, [{Input, Result} | Errors]) ->
    Result = encodings:decode(Input, Alias),
    test_decoder_errors(Alias, Errors).


test_encoder_errors(_, []) ->
    ok;
test_encoder_errors(Alias, [{Input, Result} | Errors]) ->
    Result = encodings:encode(Input, Alias),
    test_encoder_errors(Alias, Errors).


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
read_tests([{Bytes, Char} | Tail],
        String, Unicode, DecodeErrors, EncodeErrors) ->
    read_tests(Tail, <<String/binary,Bytes/binary>>,
        [Char | Unicode], DecodeErrors, EncodeErrors).


%%
%% Tests
%%

encodings_test_() -> [
    ?_assert(test_encoding([ascii, "ascii"], "ascii.txt")),
    ?_assert(test_encoding([iso8859_1, "iso88591", latin1, "latin1"],
        "iso8859-1.txt")),
    ?_assert(test_encoding([cp1251, windows1251, "cp1251", "windows1251"],
        "cp1251.txt"))
    ].
