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
%% @doc Encodings
%%
-module(encodings).

%% Public interface
-export([encode/2, decode/2, get_encoder_decoder/1,
    register_encoding/2, start/0, start_link/0, stop/0]).

%% Behaviour information
-export([behaviour_info/1]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


behaviour_info(callbacks) ->
    [{aliases, 0}, {encode, 1}, {decode, 1}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Encode Unicode to binary string with Encoding
%%
encode(Unicode, Encoding) ->
    {Encoder, _} = get_encoder_decoder(Encoding),
    Encoder(Unicode).

%%
%% @doc Decode binary String to Unicode with Encoding
%%
decode(String, Encoding) ->
    {_, Decoder} = get_encoder_decoder(Encoding),
    Decoder(String).

%%
%% @doc Return encoder and decoder for the encoding
%%
get_encoder_decoder(Encoding) ->
    gen_server:call(?MODULE, {get_encoder_decoder, Encoding}).

%%
%% @doc Register encoder and decoder for encoding
%%
register_encoding(Encoding, Module) ->
    gen_server:call(?MODULE, {register_encoding, Encoding, Module}).


%%
%% @doc Start encoder process
%%
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    register_modules().

%%
%% @doc Start encoder process
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    register_modules().


register_modules() ->
    register_modules([enc_ascii, enc_iso8859_1, enc_cp1251]).

%%
%% @doc Register modules
%%
register_modules([]) ->
    ok;
register_modules([Module | Modules]) ->
    register_module(Module, Module:aliases()),
    register_modules(Modules).

%%
%% @doc Register module
%%
register_module(_Module, []) ->
    ok;
register_module(Module, [Encoding | Encodings]) ->
    register_encoding(Encoding, Module),
    register_module(Module, Encodings).


%%
%% @doc Stop encoder process
%%
stop() ->
    gen_server:cast(?MODULE, stop).


%%
%% @doc Initialise process
%%
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [set, private, named_table]),
    {ok, none}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Message handling
%%

handle_info(_Info, State) ->
    {noreply, State}.


handle_call({get_encoder_decoder, Encoding}, _From, State) ->
    {reply, ets:lookup_element(?MODULE, Encoding, 2), State};
handle_call({register_encoding, Encoding, Module}, _From, State) ->
    Encoder = fun(U) -> Module:encode(U) end,
    Decoder = fun(S) -> Module:decode(S) end,
    {reply, ets:insert(?MODULE, {Encoding, {Encoder, Decoder}}), State};
handle_call(_, _, State) ->
    {reply, badarg, State}.
