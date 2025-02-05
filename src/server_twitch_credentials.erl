-module(server_twitch_credentials).

-behaviour(gen_server).

-export([
    start_link/2,
    setup_mnesia/0,
    create/4,
    read/1
]).
-export([
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

-record(twitch_credentials, {
    client_id,
    secret,
    access_token = none,
    refresh_token = none
}).

setup_mnesia() ->
    Nodes = [node()],
    case mnesia:create_schema(Nodes) of
        ok ->
            logger:notice("Created mnesia schema"),
            ok;
        {error, {_, {already_exists, _}}} ->
            logger:notice("Schema already exists");
        {error, Err} ->
            logger:notice(#{create_schema => failed_with, error => Err})
    end,
    application:start(mnesia),
    case
        mnesia:create_table(
            twitch_credentials,
            [
                {attributes, record_info(fields, twitch_credentials)},
                {disc_copies, Nodes}
            ]
        )
    of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _}} ->
            logger:notice(#{twitch_credentials_table => already_exists}),
            ok
    end,
    application:stop(mnesia).

create(ClientId, Secret, AccessToken, RefreshToken) ->
    Txn = fun() ->
        mnesia:write(
            #twitch_credentials{
                client_id = ClientId,
                secret = Secret,
                access_token = AccessToken,
                refresh_token = RefreshToken
            }
        )
    end,
    mnesia:activity(transaction, Txn).

% TODO: Returns [{twitch_credentials, ClientId, Secret, AccessToken, Refresh}]
read(ClientId) ->
    Txn = fun() ->
        mnesia:read(twitch_credentials, ClientId)
    end,
    mnesia:activity(transaction, Txn).

start_link(ClientId, Secret) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {ClientId, Secret}, []).

-define(THIRTY_MINUTES_MS, 1_800_000).

init({ClientId, Secret}) ->
    erlang:send_after(?THIRTY_MINUTES_MS, self(), check_credentials),
    {ok, #twitch_credentials{client_id = ClientId, secret = Secret}}.

handle_call(
    read,
    _From,
    #twitch_credentials{access_token = AccessToken, refresh_token = RefreshToken} = State
) ->
    case {AccessToken, RefreshToken} of
        {none, _} -> {reply, no_credentials, State};
        {_, none} -> {reply, no_credentials, State};
        {_, _} -> {reply, {ok, AccessToken, RefreshToken}, State}
    end.

handle_cast({update, AccessToken, RefreshToken}, _State) ->
    {noreply, #twitch_credentials{access_token = AccessToken, refresh_token = RefreshToken}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    check_credentials,
    #twitch_credentials{access_token = AccessToken, refresh_token = RefreshToken} = State
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
