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
%% Error handler function:
%%
%% <pre>
%%      error_handler({encode, function()}, {error, Encoded, string()}) -> R
%%          Encoded = binary()
%%          R = binary() | {error, Encoded, string()}
%%              | {incomplete, Encoded, string()}
%%      error_handler({decode, function()}, {error, Decoded, binary()}) -> R
%%          Decoded = string()
%%          R = string() | {error, Decoded, binary()}
%%              | {incomplete, Decoded, binary()}
%% </pre>
%%
-module(encodings).
-author("Dmitry Vasiliev <dima@hlabs.spb.ru>").
-vsn("0.1").

-behaviour(gen_server).

%% Public interface
-export([encode/2, decode/2, getencoder/1, getdecoder/1,
    register/1, lookup/1, register_error/2, lookup_error/1,
    start/0, start_link/0, stop/0,
    normalize_encoding/1]).

%% Behaviour information
-export([behaviour_info/1]).

%% gen_server callbacks
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
%% @doc Register encoder and decoder
%% @spec register(Spec) -> true
%%      Spec = {module, module()} | {functions, Aliases, Encoder, Decoder}
%%      Aliases = [string() | atom()]
%%      Encoder = function()
%%      Decoder = function()
%%
register({module, Module}) ->
    gen_server:call(?MODULE, {register_module, Module});
register({functions, Aliases, Encoder, Decoder}) ->
    gen_server:call(?MODULE, {register, Aliases, Encoder, Decoder}).


%%
%% @doc Lookup encoding info
%% @spec lookup(Encoding) -> {ok, Aliases, Encoder, Decoder} | {error, badarg}
%%      Encoding = string() | atom()
%%      Aliases = [Encoding]
%%      Encoder = function()
%%      Decoder = function()
%%
lookup(Encoding) ->
    gen_server:call(?MODULE, {lookup, Encoding}).


%%
%% @doc Register error handler
%% @spec register_error(atom(), function()) -> true
%%
register_error(Name, Handler) when is_atom(Name), is_function(Handler, 2) ->
    gen_server:call(?MODULE, {register_error, Name, Handler}).


%%
%% @doc Lookup error handler
%% @spec lookup_error(atom()) -> function()
%%
lookup_error(Name) when is_atom(Name) ->
    gen_server:call(?MODULE, {lookup_error, Name}).


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
%% @doc Normalize encoding name. All alphanumeric characters are lowercased.
%% All non-alphanumeric characters are collapsed and replaced with a single
%% underscore, e.g. '  -;#' becomes '_'. Leading and trailing underscores are
%% removed.
%% @spec normalize_encoding(string() | atom()) -> string() | atom()
%%
normalize_encoding(Name) when is_list(Name) ->
    normalize_encoding(Name, "", none);
normalize_encoding(Name) when is_atom(Name) ->
    Name.

normalize_encoding([C | Tail], Name, Last)
        when C >= $0 andalso C =< $9; C >= $a andalso C =< $z ->
    case Last of
        skip ->
            normalize_encoding(Tail, [C, $_ | Name], char);
        _ ->
            normalize_encoding(Tail, [C | Name], char)
    end;
normalize_encoding([C | Tail], Name, Last) when C >= $A andalso C =< $Z ->
    case Last of
        skip ->
            normalize_encoding(Tail, [C + 32, $_ | Name], char);
        _ ->
            normalize_encoding(Tail, [C + 32 | Name], char)
    end;
normalize_encoding([_ | Tail], Name, none) ->
    normalize_encoding(Tail, Name, none);
normalize_encoding([_ | Tail], Name, _) ->
    normalize_encoding(Tail, Name, skip);
normalize_encoding("", Name, _) ->
    lists:reverse(Name).


%%
%% @doc Initialise process
%%
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [set, private, named_table]),
    register_builtin_modules(),
    register_builtin_errors(encoding_errors:get_builtin_errors()),
    {ok, none}.

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


handle_call({Cmd, Encoding}, _From, State)
        when Cmd =:= getencoder; Cmd =:= getdecoder; Cmd =:= lookup ->
    Key = {encoding, normalize_encoding(Encoding)},
    Result = case ets:lookup(?MODULE, Key) of
        [{_, Aliases, Encoder, Decoder}] ->
            case Cmd of
                getencoder ->
                    {ok, Encoder};
                getdecoder ->
                    {ok, Decoder};
                lookup ->
                    {ok, Aliases, Encoder, Decoder}
            end;
        [] ->
            {error, badarg}
    end,
    {reply, Result, State};
handle_call({register_error, Name, Handler}, _From, State) ->
    {reply, register_error_internally(Name, Handler), State};
handle_call({lookup_error, Name}, _From, State) ->
    Result = try ets:lookup_element(?MODULE, {error, Name}, 2) of
        Handler ->
            Handler
    catch
        error:badarg ->
            {error, badarg}
    end,
    {reply, Result, State};
handle_call({register_module, Module}, _From, State) ->
    {reply, register_module(Module), State};
handle_call({register, Encodings, Encoder, Decoder}, _From, State) ->
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
    application:load(encodings),
    {ok, Modules} = application:get_key(encodings, modules),
    register_builtin_modules([N || N <- Modules,
        string:str(atom_to_list(N), "enc_") =:= 1]).

register_builtin_modules([]) ->
    ok;
register_builtin_modules([Module | Modules]) ->
    register_module(Module),
    register_builtin_modules(Modules).


%%
%% @doc Register module
%%
register_module(Module) ->
    Encoder = fun (U) -> Module:encode(U) end,
    Decoder = fun (S) -> Module:decode(S) end,
    Aliases = Module:aliases(),
    register_encoding(Aliases, Encoder, Decoder).


%%
%% @doc Register encoder/decoder
%%
register_encoding(Aliases, Encoder, Decoder) ->
    Info = [{{encoding, normalize_encoding(E)}, Aliases, Encoder, Decoder}
        || E <- Aliases],
    ets:insert(?MODULE, Info).


%%
%% @doc Register builtin error handlers
%%
register_builtin_errors([]) ->
    ok;
register_builtin_errors([{Name, Handler} | Tail]) ->
    register_error_internally(Name, Handler),
    register_builtin_errors(Tail).


%%
%% @doc Register error handler internally
%%
register_error_internally(Name, Handler) ->
    ets:insert(?MODULE, {{error, Name}, Handler}).
