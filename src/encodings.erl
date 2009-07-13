%%
%% @doc Encodings
%%
-module(encodings).
-export([encode/2, decode/2, register/3, start_link/0, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


%%
%% @doc Encode Unicode to string with Encoding
%%
encode(Unicode, Encoding) ->
    gen_server:call(?MODULE, {encode, Unicode, Encoding}).

%%
%% @doc Decode String to Unicode with Encoding
%%
decode(String, Encoding) ->
    gen_server:call(?MODULE, {decode, String, Encoding}).

%%
%% @doc Register encoder and decoder for encoding
%%
register(Encoding, Encoder, Decoder) ->
    gen_server:call(?MODULE, {register, Encoding, Encoder, Decoder}).


%%
%% @doc Start encoder process
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    cp1251:register().


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


handle_call({encode, Unicode, Encoding}, _From, State) ->
    [{_Encoding, Encoder, _Decoder}] = ets:lookup(?MODULE, Encoding),
    {reply, Encoder(Unicode), State};
handle_call({decode, String, Encoding}, _From, State) ->
    [{_Encoding, _Encoder, Decoder}] = ets:lookup(?MODULE, Encoding),
    {reply, Decoder(String), State};
handle_call({register, Encoding, Encoder, Decoder}, _From, State) ->
    ets:insert(?MODULE, {Encoding, Encoder, Decoder}),
    {reply, ok, State}.
