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
-module(test_encodings).

-export([test/0, doc/0]).


test_encodings() ->
    encodings:start(),
    %io:format("~p~n", [read_utf8("utf8.txt")]),
    %ok = test_encoding([utf8, "utf8"], read_utf8("utf8.txt")),
    ok = test_encoding([ascii, "ascii"], read_tests("ascii.txt")),
    ok = test_encoding([iso8859_1, "iso88591", latin1, "latin1"],
        read_tests("iso8859-1.txt")),
    ok = test_encoding([cp1251, windows1251, "cp1251", "windows1251"],
        read_tests("cp1251.txt")),
    encodings:stop(),
    ok.


read_utf8(Filename) ->
    {_, _, DecodeErrors, EncodeErrors} = read_tests(Filename),
    {String, Unicode} = gen_utf8(0, <<>>, ""),
    {String, Unicode, DecodeErrors, EncodeErrors}.

%generate_utf8(16#110000, String, Unicode) ->
gen_utf8(16#1000, String, Unicode) ->
    {String, lists:reverse(Unicode)};
gen_utf8(C, String, Unicode) when C >= 0, C =< 16#7f ->
    gen_utf8(C + 1, <<String/binary,C>>, [C | Unicode]);
gen_utf8(C, String, Unicode) when C >= 16#80, C =< 16#7ff ->
    % FIXME: Why 2?
    gen_utf8(C, 2, 0, String, Unicode).
%gen_utf8(C, String, Unicode) when C >= 16#800, C =< 16#ffff ->
%    gen_utf8(C, 0, 0, 0, String, Unicode);
%gen_utf8(C, String, Unicdoe) when C >= 16#10000, C =< 16#10ffff ->
%    gen_utf8(C, 0, 0, 0, 0, String, Unicode).

gen_utf8(_, 31, 63, String, Unicode) ->
    {String, lists:reverse(Unicode)};
gen_utf8(C, C1, 63, String, Unicode) ->
    gen_utf8(C + 1, C1 + 1, 0, <<String/binary,2#110:3,C1:5,2#10:2,0:6>>,
        [C | Unicode]);
gen_utf8(C, C1, C2, String, Unicode) ->
    gen_utf8(C + 1, C1 + 1, 0, <<String/binary,2#110:3,C1:5,2#10:2,C2:6>>,
        [C | Unicode]).


%%
%% @doc Test single encoder/decoder
%%
test_encoding(Aliases, {Bytes, Unicode, DecoderErrors, EncoderErrors}) ->
    % TODO: Extract test for aliases to distinct function?
    test_encode_decode(Aliases, Bytes, Unicode),
    test_decoder_errors(hd(Aliases), DecoderErrors),
    test_encoder_errors(hd(Aliases), EncoderErrors),
    ok.

test_encode_decode([], _Bytes, _Unicode) ->
    ok;
test_encode_decode([Alias | Aliases], Bytes, Unicode) ->
    Unicode = encodings:decode(Bytes, Alias),
    Bytes = encodings:encode(Unicode, Alias),
    {Encoder, Decoder} = encodings:get_encoder_decoder(Alias),
    Unicode = Decoder(Bytes),
    Bytes = Encoder(Unicode),
    test_encode_decode(Aliases, Bytes, Unicode).

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


%%
%% @doc Read test specifications
%%
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


test() ->
    test_encodings().


doc() ->
    edoc:application(encodings, "src", []).
