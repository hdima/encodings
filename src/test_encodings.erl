%% Copyright (C) 2009 Dmitry Vasiliev <dima@hlabs.spb.ru>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @doc Tests
%%
-module(test_encodings).
-export([test/0, doc/0]).


test_encodings() ->
    encodings:start(),
    ok = test_encoding([ascii, "ascii"], read_tests("ascii.txt")),
    ok = test_encoding([iso8859_1, "iso88591", latin1, "latin1"],
        read_tests("iso8859-1.txt")),
    %ok = test_encoding([cp1251, windows1251, "cp1251", "windows1251"],
    %    read_records("cp1251.txt")),
    encodings:stop(),
    ok.


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


read_tests(Filename) ->
    Path = filename:join([filename:dirname(?FILE), "tests", Filename]),
    {ok, Terms} = file:consult(Path),
    read_tests(Terms, <<>>, [], [], []).

read_tests([], String, Unicode, DecodeErrors, EncodeErrors) ->
    {String, lists:reverse(Unicode), DecodeErrors, EncodeErrors};
read_tests([{decode_errors, Errors} | Tail],
        String, Unicode, _DecodeErrors, EncodeErrors) ->
    read_tests(Tail, String, Unicode, Errors, EncodeErrors);
read_tests([{encode_errors, Errors} | Tail],
        String, Unicode, DecodeErrors, _EncodeErrors) ->
    read_tests(Tail, String, Unicode, DecodeErrors, Errors);
read_tests([{Bytes, Char} | Tail],
        String, Unicode, DecodeErrors, EncodeErrors) ->
    read_tests(Tail, <<String/binary,Bytes/binary>>,
        [Char | Unicode], DecodeErrors, EncodeErrors).


test() ->
    % test_utf8(),
    test_encodings().


doc() ->
    edoc:application(encodings, "src", []).
