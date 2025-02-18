-module(server_devlog).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    logger:notice(#{call => Msg}),
    {reply, ok, State}.

%% Log a message
handle_cast({log, Msg}, #state{} = State) ->
    logger:notice(Msg),
    {noreply, State};
%% Handle any unknown messages
handle_cast(Msg, State) ->
    logger:notice(#{module => ?MODULE, cast_unknown => Msg}),
    {noreply, State}.
