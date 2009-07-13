-module(test_encodings).
-export([test/0, doc/0]).


test_encodings() ->
    encodings:start_link(),
    ok = test_encoding([cp1251, windows1251, "cp1251", "windows1251"],
        read_records("cp1251.txt")),
    encodings:stop(),
    ok.


test_encoding([], _Info) ->
    ok;
test_encoding([Encoding | Encodings], {String, Unicode}=Info) ->
    String = encodings:encode(Unicode, Encoding),
    Unicode = encodings:decode(String, Encoding),
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
    test_encodings().


doc() ->
    edoc:application(encodings, "src", []).
