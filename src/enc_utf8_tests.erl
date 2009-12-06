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
%% @doc UTF-8 tests
%%
-module(enc_utf8_tests).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-include_lib("eunit/include/eunit.hrl").


%%
%% Auxiliary functions
%%

test_utf8(Aliases) ->
    encodings_tests:test_aliases(Aliases),
    Bytes = <<0,
        16#7f,
        16#c2, 16#80,
        16#df, 16#bf,
        16#e0, 16#a0, 16#80,
        16#ef, 16#bf, 16#bd,
        16#f0, 16#90, 16#80, 16#80,
        16#f4, 16#8f, 16#bf, 16#bf>>,
    Unicode = [0, 16#7f, 16#80, 16#7ff, 16#800, 16#fffd, 16#10000, 16#10ffff],
    encodings_tests:test_encode_decode(hd(Aliases), Bytes, Unicode),
    ok.


%%
%% Tests
%%

utf8_test_() -> {setup,
        fun encodings_tests:setup/0, fun encodings_tests:cleanup/1, [
    ?_assertEqual(ok, test_utf8([utf8, "utf-8"]))
    ]}.
