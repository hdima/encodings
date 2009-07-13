%%
%% @doc Encodings
%%
-module(encodings).
-export([encode/2, decode/2, register_encoding/2, start_link/0, stop/0]).

-export([behaviour_info/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


behaviour_info(callbacks) ->
    [{aliases, 0}, {encode, 1}, {decode, 1}];
behaviour_info(_Other) ->
    undefined.


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
register_encoding(Encoding, Module) ->
    gen_server:call(?MODULE, {register_encoding, Encoding, Module}).


%%
%% @doc Start encoder process
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    register_modules([cp1251]).


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


handle_call({encode, Unicode, Encoding}, _From, State) ->
    Module = ets:lookup_element(?MODULE, Encoding, 2),
    {reply, Module:encode(Unicode), State};
handle_call({decode, String, Encoding}, _From, State) ->
    Module = ets:lookup_element(?MODULE, Encoding, 2),
    {reply, Module:decode(String), State};
handle_call({register_encoding, Encoding, Module}, _From, State) ->
    ets:insert(?MODULE, {Encoding, Module}),
    {reply, ok, State}.
