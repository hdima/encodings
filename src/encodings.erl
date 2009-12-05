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
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

%% Public interface
-export([encode/2, decode/2, getencoder/1, getdecoder/1,
    register/3, register/4, register_module/1, register_module/2,
    start/0, start_link/0, stop/0]).

%% Behaviour information
-export([behaviour_info/1]).

%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% RE of symbols which need to be ignored in encoding name
-define(IGNORE_IN_NAMES, "[- _.]+").


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
    case getencoder(Encoding) of
        {ok, Encoder} ->
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
    case getdecoder(Encoding) of
        {ok, Decoder} ->
            Decoder(String);
        {error, Reason} ->
            erlang:error(Reason)
    end.


%%
%% @doc Return encoder
%% @spec getencoder(Encoding) -> {ok, function()} | {error, badarg}
%%
getencoder(Encoding) ->
    gen_server:call(?MODULE, {getencoder, Encoding}).


%%
%% @doc Return decoder
%% @spec getdecoder(Encoding) -> {ok, function()} | {error, badarg}
%%
getdecoder(Encoding) ->
    gen_server:call(?MODULE, {getdecoder, Encoding}).


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
register_module(Module, Opts) when Opts =:= []; Opts =:= [override] ->
    gen_server:call(?MODULE, {register_module, Module, Opts}).


%%
%% @doc Register encoder and decoder as functions and return true on success
%% @spec register(Encodings, Encoder, Decoder) -> true
%%      Encodings = [Encoding]
%%      Encoding = string() | atom()
%%      Encoder = function()
%%      Decoder = function()
%%
register(Encodings, Encoder, Decoder) ->
    register(Encodings, Encoder, Decoder, []).


%%
%% @doc Register encoder and decoder with options
%% @spec register(Encodings, Encoder, Decoder, Options) -> true
%%      Encodings = [Encoding]
%%      Encoding = string() | atom()
%%      Encoder = function()
%%      Decoder = function()
%%      Options = [] | [override]
%%
register(Encodings, Encoder, Decoder, Opts)
        when Opts =:= []; Opts =:= [override] ->
    gen_server:call(?MODULE, {register, Encodings, Encoder, Decoder, Opts}).


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
    process_flag(trap_exit, true),
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


handle_info({'EXIT', _, shutdown}, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.


handle_call({Cmd, Encoding}, _From, RE)
        when Cmd =:= getencoder; Cmd =:= getdecoder ->
    Pos = case Cmd of
        getencoder ->
            3;
        getdecoder ->
            4
    end,
    E = normalize_encoding_name(Encoding, RE),
    Result = try ets:lookup_element(?MODULE, E, Pos) of
        Func ->
            {ok, Func}
    catch
        error:badarg ->
            {error, badarg}
    end,
    {reply, Result, RE};
handle_call({register_module, Module, Options}, _From, RE) ->
    {reply, register_module_internally(Module, Options, RE), RE};
handle_call({register, Encodings, Encoder, Decoder, Options}, _From, RE) ->
    {reply, register_encoding(Encodings, Encoder, Decoder, Options, RE), RE};
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
    Replaced = re:replace(Name, RE, "", [global, {return, list}]),
    string:to_lower(Replaced);
normalize_encoding_name(Name, _) ->
    Name.

%%
%% @doc Register builtin modules
%%
register_builtin_modules(RE) ->
    application:load(encodings),
    {ok, Modules} = application:get_key(encodings, modules),
    register_builtin_modules([N || N <- Modules,
        string:str(atom_to_list(N), "enc_") =:= 1], RE).

register_builtin_modules([], _) ->
    ok;
register_builtin_modules([Module | Modules], RE) ->
    case register_module_internally(Module, [], RE) of
        true ->
            register_builtin_modules(Modules, RE);
        false ->
            error_logger:format(
                "Duplicate encoding alias in module ~p~n", [Module]),
            register_builtin_modules(Modules, RE)
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
%% @doc Register encoder/decoder
%%
register_encoding(Aliases, Encoder, Decoder, Options, RE) ->
    Info = [{normalize_encoding_name(E, RE), Aliases, Encoder, Decoder} ||
        E <- Aliases],
    case Options of
        [override] ->
            ets:insert(?MODULE, Info);
        [] ->
            ets:insert_new(?MODULE, Info)
    end.
