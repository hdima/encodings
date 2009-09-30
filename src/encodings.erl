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
    register_module/1, register_encoder_decoder/3,
    start/0, start_link/0, stop/0]).

%% Behaviour information
-export([behaviour_info/1]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


%%
%% @doc Behaviour information
%% @spec behaviour_info(callbacks) -> Callbacks
%%      Callbacks = [{module(), Arity}]
%%      Arity = integer()
%%
behaviour_info(callbacks) ->
    [{aliases, 0}, {encode, 1}, {decode, 1}];
behaviour_info(_Other) ->
    undefined.


%%
%% @doc Encode Unicode to binary string with Encoding
%% @spec encode(Unicode, Encoding) -> String
%%      Unicode = string()
%%      Encoding = string() | atom()
%%      String = binary()
%%
encode(Unicode, Encoding) ->
    {ok, Encoder, _} = get_encoder_decoder(Encoding),
    Encoder(Unicode).

%%
%% @doc Decode binary String to Unicode with Encoding
%% @spec decode(String, Encoding) -> Unicode
%%      String = binary()
%%      Encoding = string() | atom()
%%      Unicode = string()
%%
decode(String, Encoding) ->
    {ok, _, Decoder} = get_encoder_decoder(Encoding),
    Decoder(String).

%%
%% @doc Return encoder and decoder for the encoding
%% @spec get_encoder_decoder(Encoding) -> Result
%%      Encoding = string() | atom()
%%      Encoder = function()
%%      Decoder = function()
%%      Result = {ok, Encoder, Decoder} | {error, badarg}
%%
get_encoder_decoder(Encoding) ->
    gen_server:call(?MODULE, {get_encoder_decoder, Encoding}).

%%
%% @doc Register callback module for encoding
%% @spec register_module(Module) -> ok
%%      Module = module()
%%
register_module(Module) ->
    gen_server:call(?MODULE, {register_module, Module}).


%%
%% @doc Register encoder and decoder as functions
%% @spec register_encoder_decoder(Encodings, Encoder, Decoder) -> ok
%%      Encodings = [Encoding]
%%      Encoding = string() | atom()
%%      Encoder = function()
%%      Decoder = function()
%%
register_encoder_decoder(Encodings, Encoder, Decoder) ->
    gen_server:call(?MODULE, {register_encoder_decoder,
        Encodings, Encoder, Decoder}).


%%
%% @doc Start encoder process
%%
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%
%% @doc Start encoder process
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%% @doc Stop encoder process
%%
stop() ->
    gen_server:cast(?MODULE, stop),
    timer:sleep(10).


%%
%% @doc Initialise process
%%
init([]) ->
    ets:new(?MODULE, [set, private, named_table]),
    gen_server:cast(?MODULE, register_builtin_modules),
    {ok, none}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Message handling
%%

handle_cast(register_builtin_modules, State) ->
    register_builtin_modules(),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


handle_call({get_encoder_decoder, Encoding}, _From, State) ->
    Result = try ets:lookup_element(?MODULE, Encoding, 2) of
        {Encoder, Decoder} ->
            {ok, Encoder, Decoder}
    catch
        error:badarg ->
            {error, badarg}
    end,
    {reply, Result, State};
handle_call({register_module, Module}, _From, State) ->
    {reply, register_module_internally(Module), State};
handle_call({register_encoder_decoder, Encodings, Encoder, Decoder},
        _From, State) ->
    {reply, register_encoding(Encodings, Encoder, Decoder), State};
handle_call(_, _, State) ->
    {reply, badarg, State}.


%%
%% Auxiliary functions
%%

%%
%% @doc Register builtin modules
%%
register_builtin_modules() ->
    Path = filename:dirname(?FILE),
    {ok, Filenames} = file:list_dir(Path),
    register_builtin_modules([list_to_existing_atom(filename:rootname(N))
        || N <- Filenames, string:str(N, "enc_") =:= 1]).

register_builtin_modules([]) ->
    ok;
register_builtin_modules([Module | Modules]) ->
    register_module_internally(Module),
    register_builtin_modules(Modules).


%%
%% @doc Register module
%%
register_module_internally(Module) ->
    Encoder = fun (U) -> Module:encode(U) end,
    Decoder = fun (S) -> Module:decode(S) end,
    register_encoding(Module:aliases(), Encoder, Decoder).


%%
%% @doc Register encoder/decoder
%%
register_encoding([], _, _) ->
    ok;
register_encoding([Encoding | Encodings], Encoder, Decoder) ->
    ets:insert(?MODULE, {Encoding, {Encoder, Decoder}}),
    register_encoding(Encodings, Encoder, Decoder).
