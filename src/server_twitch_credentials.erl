-module(server_twitch_credentials).

-behaviour(gen_server).

-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% TODO: Store credentials with mnesia
% TODO: The focus server supervisor should start this process
% TODO: If our credentials are expired, fetch new ones.
%       Should happen as continue in init/1 and in check_credentials loop.
% TODO: Add a function refresh_credentials/0

start_link(ClientId, Secret) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {ClientId, Secret}, []).

-record(state, {
    client_id,
    secret,
    access_token = none,
    refresh_token = none
}).

-define(THIRTY_MINUTES_MS, 1_800_000).

init({ClientId, Secret}) ->
    erlang:send_after(?THIRTY_MINUTES_MS, self(), check_credentials),
    {ok, #state{client_id = ClientId, secret = Secret}}.

handle_call(
    read,
    _From,
    #state{access_token = AccessToken, refresh_token = RefreshToken} = State
) ->
    case {AccessToken, RefreshToken} of
        {none, _} -> {reply, no_credentials, State};
        {_, none} -> {reply, no_credentials, State};
        {_, _} -> {reply, {ok, AccessToken, RefreshToken}, State}
    end.

handle_cast({update, AccessToken, RefreshToken}, _State) ->
    {noreply, #state{access_token = AccessToken, refresh_token = RefreshToken}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    check_credentials,
    #state{access_token = AccessToken, refresh_token = RefreshToken} = State
) ->
    erlang:send_after(?THIRTY_MINUTES_MS, self(), check_credentials),

    case {AccessToken, RefreshToken} of
        {none, _} ->
            ok;
        {_, none} ->
            ok;
        {_, _} ->
            % TODO: Call twitch:auth with our credentials
            ok
    end,
    {reply, State};
handle_info(_Msg, State) ->
    {noreply, State}.
