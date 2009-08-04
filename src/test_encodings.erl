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
    %ok = test_encoding([iso8859_1, "iso88591", latin1, "latin1"],
    %    read_records("iso8859-1.txt")),
    %ok = test_encoding([cp1251, windows1251, "cp1251", "windows1251"],
    %    read_records("cp1251.txt")),
    encodings:stop(),
    ok.


test_encoding([], _Info) ->
    ok;
test_encoding([Encoding | Encodings], {Bytes, Unicode}=Info) ->
    {Encoder, Decoder} = encodings:get_encoder_decoder(Encoding),
    Bytes = encodings:encode(Unicode, Encoding),
    Bytes = Encoder(Unicode),
    Unicode = encodings:decode(Bytes, Encoding),
    Unicode = Decoder(Bytes),
    {error, <<>>, [-1]} = encodings:encode([-1], Encoding),
    test_encoding(Encodings, Info).


read_tests(Filename) ->
    Path = filename:join([filename:dirname(?FILE), "tests", Filename]),
    {ok, Terms} = file:consult(Path),
    read_tests(Terms, <<>>, []).

%% TODO: Add tests for bad cases
read_tests([], String, Unicode) ->
    {String, lists:reverse(Unicode)};
read_tests([{Bytes, Char} | Tail], String, Unicode) ->
    read_tests(Tail, <<String/binary,Bytes/binary>>, [Char | Unicode]).


test() ->
    % test_utf8(),
    test_encodings().


doc() ->
    edoc:application(encodings, "src", []).
