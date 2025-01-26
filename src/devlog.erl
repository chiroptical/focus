-module(devlog).

-behaviour(gen_server).

-export([
    log/1,
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Msg) ->
    gen_server:cast(?MODULE, {log, Msg}).

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
