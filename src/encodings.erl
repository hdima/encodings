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
%% Callback module interface:
%%
%% <pre>
%%      aliases() -> [Alias]
%%          Alias = atom() | string()
%%
%%      encode(Unicode) -> Result
%%          Unicode = string()
%%          Result = binary() | {error, Encoded, Rest}
%%              | {incomplete, Encoded, Rest}
%%          Encoded = binary()
%%          Rest = string()
%%
%%      decode(String) -> Result
%%          String = binary()
%%          Result = string() | {error, Decoded, Rest}
%%              | {incomplete, Decoded, Rest}
%%          Decoded = string()
%%          Rest = binary()
%% </pre>
%%
-module(encodings).

%% Public interface
-export([encode/2, decode/2, get_encoder_decoder/1,
    register_module/1, register_module/2,
    register_encoder_decoder/3, register_encoder_decoder/4,
    unregister_module/1, unregister_encoding/1,
    start/0, start_link/0, stop/0]).

%% Behaviour information
-export([behaviour_info/1]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% RE of symbols which need to be ignored in encoding name
-define(IGNORE_IN_NAMES, "[ -_.]+").


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
%% @spec encode(Unicode, Encoding) -> Result
%%      Unicode = string()
%%      Result = binary() | {error, Encoded, Rest}
%%          | {incomplete, Encoded, Rest}
%%      Encoded = binary()
%%      Rest = string()
%%
encode(Unicode, Encoding) ->
    case get_encoder_decoder(Encoding) of
        {ok, Encoder, _Decoder} ->
            Encoder(Unicode);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%
%% @doc Decode binary String to Unicode with Encoding
%% @spec decode(String, Encoding) -> Result
%%      String = binary()
%%      Result = string() | {error, Decoded, Rest}
%%          | {incomplete, Decoded, Rest}
%%      Decoded = string()
%%      Rest = binary()
%%
decode(String, Encoding) ->
    case get_encoder_decoder(Encoding) of
        {ok, _Encoder, Decoder} ->
            Decoder(String);
        {error, Reason} ->
            erlang:error(Reason)
    end.


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
%% @doc Register callback module and return true on success
%% @spec register_module(Module) -> bool()
%%      Module = module()
%%
register_module(Module) ->
    register_module(Module, []).


%%
%% @doc Register callback module with options
%% @spec register_module(Module, Options) -> bool()
%%      Module = module()
%%      Options = [] | [override]
%%
register_module(Module, Options) ->
    Info = {register_module, Module, Options},
    case Options of
        [] ->
            gen_server:call(?MODULE, Info);
        [override] ->
            gen_server:call(?MODULE, Info)
    end.


%%
%% @doc Unregister callback module
%% @spec unregister_module(Module) -> ok
%%      Module = module()
%%
unregister_module(Module) ->
    gen_server:call(?MODULE, {unregister_module, Module}).


%%
%% @doc Register encoder and decoder as functions and return true on success
%% @spec register_encoder_decoder(Encodings, Encoder, Decoder) -> true
%%      Encodings = [Encoding]
%%      Encoding = string() | atom()
%%      Encoder = function()
%%      Decoder = function()
%%
register_encoder_decoder(Encodings, Encoder, Decoder) ->
    register_encoder_decoder(Encodings, Encoder, Decoder, []).


%%
%% @doc Register encoder and decoder with options
%% @spec register_encoder_decoder(Encodings, Encoder, Decoder, Options) -> true
%%      Encodings = [Encoding]
%%      Encoding = string() | atom()
%%      Encoder = function()
%%      Decoder = function()
%%      Options = [] | [override]
%%
register_encoder_decoder(Encodings, Encoder, Decoder, Options) ->
    Info = {register_encoder_decoder, Encodings, Encoder, Decoder, Options},
    case Options of
        [] ->
            gen_server:call(?MODULE, Info);
        [override] ->
            gen_server:call(?MODULE, Info)
    end.


%%
%% @doc Unregister encoding
%% @spec unregister_encoding(Encoding) -> ok
%%      Encoding = string() | atom()
%%
unregister_encoding(Encoding) ->
    gen_server:call(?MODULE, {unregister_encoding, Encoding}).


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
    {ok, RE} = re:compile(?IGNORE_IN_NAMES),
    register_builtin_modules(RE),
    {ok, RE}.

terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Message handling
%%

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


handle_call({get_encoder_decoder, Encoding}, _From, RE) ->
    E = normalize_encoding_name(Encoding, RE),
    Result = try ets:lookup_element(?MODULE, E, 3) of
        {Encoder, Decoder} ->
            {ok, Encoder, Decoder}
    catch
        error:badarg ->
            {error, badarg}
    end,
    {reply, Result, RE};
handle_call({register_module, Module, Options}, _From, RE) ->
    {reply, register_module_internally(Module, Options, RE), RE};
handle_call({unregister_module, Module}, _From, RE) ->
    {reply, unregister_module_internally(Module, RE), RE};
handle_call({register_encoder_decoder, Encodings, Encoder, Decoder, Options},
        _From, RE) ->
    {reply, register_encoding(Encodings, Encoder, Decoder, Options, RE), RE};
handle_call({unregister_encoding, Encoding}, _From, RE) ->
    {reply, unregister_encoding_internally(Encoding, RE), RE};
handle_call(_, _, State) ->
    {reply, badarg, State}.


%%
%% Auxiliary functions
%%

%%
%% @doc Normalize encoding name
%% @spec normalize_encoding_name(Name, RE) -> NewName
%%      Name = string()
%%      RE = mp()
%%      NewName = string()
%%
normalize_encoding_name(Name, RE) when is_list(Name) ->
    re:replace(Name, RE, "", [global, {return, list}]);
normalize_encoding_name(Name, _) ->
    Name.

%%
%% @doc Register builtin modules
%%
register_builtin_modules(RE) ->
    Path = filename:dirname(?FILE),
    {ok, Filenames} = file:list_dir(Path),
    register_builtin_modules([list_to_existing_atom(filename:rootname(N))
        || N <- Filenames, string:str(N, "enc_") =:= 1], RE).

register_builtin_modules([], _) ->
    ok;
register_builtin_modules([Module | Modules], RE) ->
    case register_module_internally(Module, [], RE) of
        true ->
            register_builtin_modules(Modules, RE);
        false ->
            error_logger:format(
                "Duplicate encoding alias in module ~p~n", [Module]),
            erlang:error(badarg)
    end.


%%
%% @doc Register module
%%
register_module_internally(Module, Options, RE) ->
    Encoder = fun (U) -> Module:encode(U) end,
    Decoder = fun (S) -> Module:decode(S) end,
    Aliases = Module:aliases(),
    register_encoding(Aliases, Encoder, Decoder, Options, RE).


%%
%% @doc Unregister module
%%
unregister_module_internally(Module, RE) ->
    unregister_encoding_internally(hd(Module:aliases()), RE).


%%
%% @doc Register encoder/decoder
%%
register_encoding(Aliases, Encoder, Decoder, Options, RE) ->
    Info = [{normalize_encoding_name(E, RE), Aliases, {Encoder, Decoder}} ||
        E <- Aliases],
    case Options of
        [override] ->
            ets:insert(?MODULE, Info);
        [] ->
            ets:insert_new(?MODULE, Info)
    end.


%%
%% @doc Unregister encoder/decoder
%%
unregister_encoding_internally(Encoding, RE) ->
    E = normalize_encoding_name(Encoding, RE),
    case ets:lookup(?MODULE, E) of
        [] ->
            ok;
        [{_, Aliases, _}] ->
            unregister_aliases(Aliases)
    end.

unregister_aliases([]) ->
    ok;
unregister_aliases([Alias | Aliases]) ->
    ets:delete(?MODULE, Alias),
    unregister_aliases(Aliases).
