-module(devlog).

-behaviour(gen_server).

-export([
    log/1,
    enable/0,
    disable/0,
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(state, {enabled = false :: boolean()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Msg) ->
    gen_server:cast(?MODULE, {log, Msg}).

enable() ->
    gen_server:cast(?MODULE, enable).

disable() ->
    gen_server:cast(?MODULE, disable).

init([]) ->
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    logger:notice(#{call => Msg}),
    {reply, ok, State}.

%% Enable or disable the gen_server from emitting logs
handle_cast(enable, State) ->
    {noreply, State#state{enabled = true}};
handle_cast(disable, State) ->
    {noreply, State#state{enabled = false}};
%% Log a message when enabled is true
handle_cast({log, Msg}, #state{enabled = true} = State) ->
    logger:notice(Msg),
    {noreply, State};
handle_cast({log, _Msg}, #state{enabled = false} = State) ->
    {noreply, State};
%% Handle any unknown messages
handle_cast(Msg, State) ->
    logger:notice(#{unhandled_cast => Msg}),
    {noreply, State}.
