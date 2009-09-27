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
%% @doc Encodings
%%
-module(encodings).

%% Public interface
-export([encode/2, decode/2, get_encoder_decoder/1,
    register_encoding/2, start/0, start_link/0, stop/0]).

-export([behaviour_info/1]).

%% gen_server callbacks
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
    [{_, Encoder, Decoder}] = ets:lookup(?MODULE, Encoding),
    {reply, {Encoder, Decoder}, State};
handle_call({register_encoding, Encoding, Module}, _From, State) ->
    Encoder = fun(U) -> Module:encode(U) end,
    Decoder = fun(S) -> Module:decode(S) end,
    {reply, ets:insert(?MODULE, {Encoding, Encoder, Decoder}), State};
handle_call(_, _, State) ->
    {reply, badarg, State}.
