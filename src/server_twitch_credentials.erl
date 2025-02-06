-module(server_twitch_credentials).

-behaviour(gen_server).

-export([
    start_link/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% TODO: We need to store the credential expiry
% TODO: The focus server supervisor should start this process
% TODO: If our credentials are expired, fetch new ones.
%       Should happen in continue in init/1 and in check_credentials loop.
% TODO: Add a function refresh_credentials/0

-record(twitch_credentials, {
    client_id,
    secret,
    access_token = none,
    refresh_token = none
}).

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

%% INTERNAL UTILITIES

create(#twitch_credentials{
    client_id = ClientId,
    secret = Secret,
    access_token = none,
    refresh_token = none
}) ->
    Contents = json:encode(#{secret => Secret}),
    create_body(ClientId, Contents);
create(#twitch_credentials{
    client_id = ClientId,
    secret = Secret,
    access_token = AccessToken,
    refresh_token = RefreshToken
}) ->
    Contents = json:encode(#{
        secret => Secret,
        access_token => AccessToken,
        refresh_token => RefreshToken
    }),
    create_body(ClientId, Contents).

read(ClientId) ->
    maybe
        Filename = make_filename(ClientId),
        {ok, Contents} = file:read_file(Filename),
        Decoded = json:decode(Contents),
        {ok, Secret} = maps:find(~"secret", Decoded),
        AccessToken = maps:get(~"access_token", Decoded, none),
        RefreshToken = maps:get(~"refresh_token", Decoded, none),
        {ok, #twitch_credentials{
            client_id = ClientId,
            secret = Secret,
            access_token = AccessToken,
            refresh_token = RefreshToken
        }}
    else
        {error, _} -> {error, not_found}
    end.

make_dir_if_not_exists() ->
    Dir = filename:basedir(user_data, ~"focus"),
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, _} -> error
    end.

make_filename(ClientId) ->
    filename:basedir(
        user_data,
        <<"focus/", ClientId/binary, ".json">>
    ).

create_body(ClientId, Contents) ->
    maybe
        ok = make_dir_if_not_exists(),
        Filename = make_filename(ClientId),
        ok = file:write_file(Filename, Contents),
        ok
    else
        {error, _} ->
            error
    end.
