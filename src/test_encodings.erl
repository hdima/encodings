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


test_utf8() ->
    ok = test_encoding([utf8, "utf8"], generate_utf8()),
    ok.


generate_utf8() ->
    {generate_utf8("", 16#10ffff), generate_unicode("", 16#10ffff)}.


generate_utf8(Result, -1) ->
    Result;
generate_utf8(Result, N) when N >= 16#0, N =< 16#7f->
    generate_utf8([N | Result], N - 1);
generate_utf8(Result, N) when N >= 16#80, N =< 16#7ff->
    % TODO
    generate_utf8([N, N | Result], N - 1);
generate_utf8(Result, N) when N >= 16#800, N =< 16#ffff->
    % TODO
    generate_utf8([N, N, N | Result], N - 1);
generate_utf8(Result, N) when N >= 16#10000, N =< 16#10ffff->
    % TODO
    generate_utf8([N, N, N, N | Result], N - 1).


generate_unicode(Result, -1) ->
    Result;
generate_unicode(Result, N) ->
    generate_unicode([N | Result], N - 1).


test_encodings() ->
    encodings:start(),
    ok = test_encoding([ascii, "ascii"], read_records("ascii.txt")),
    ok = test_encoding([iso8859_1, "iso88591", latin1, "latin1"],
        read_records("iso8859-1.txt")),
    ok = test_encoding([cp1251, windows1251, "cp1251", "windows1251"],
        read_records("cp1251.txt")),
    encodings:stop(),
    ok.


test_encoding([], _Info) ->
    ok;
test_encoding([Encoding | Encodings], {String, Unicode}=Info) ->
    String = encodings:encode(Unicode, Encoding),
    Unicode = encodings:decode(String, Encoding),
    BadString = [hd(String) + 1],
    {error, [], BadString} = (catch encodings:decode(BadString, Encoding)),
    {error, [], [-1]} = (catch encodings:encode([-1], Encoding)),
    {error, [], [-1]} = (catch encodings:decode([-1], Encoding)),
    test_encoding(Encodings, Info).


read_records(Filename) ->
    Path = filename:join([filename:dirname(?FILE), "tests", Filename]),
    {ok, F} = file:open(Path, [read, raw]),
    read_records(F, "", "", "").


read_records(F, [$0, $x, S1, S2, $\t, $0, $x, U1, U2, U3, U4, $\n | Tail],
        String, Unicode) ->
    read_records(F, Tail, [erlang:list_to_integer([S1, S2], 16) | String],
        [erlang:list_to_integer([U1, U2, U3, U4], 16) | Unicode]);
read_records(F, Buffer, String, Unicode) ->
    case file:read(F, 1024) of
        eof ->
            file:close(F),
            {String, Unicode};
        {ok, Data} ->
            read_records(F, Buffer ++ Data, String, Unicode)
    end.


test() ->
    % test_utf8(),
    test_encodings().


doc() ->
    edoc:application(encodings, "src", []).
