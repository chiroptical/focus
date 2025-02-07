-module(server_twitch_credentials).

-behaviour(gen_server).

-export([
    start_link/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2
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
    {ok, #twitch_credentials{client_id = ClientId, secret = Secret}, {continue, check_credentials}}.

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

    % TODO: Do twitch:auth(AccessToken),
    % if 200 and expires in >30 minutes, {noreply, State}
    % if 401, run refresh_credentials, update State

    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_continue(check_credentials, State) ->
    case check_credentials(State, initial) of
        ok ->
            {noreply, State};
        {ok, AccessToken, RefreshToken} ->
            {noreply, State#twitch_credentials{
                access_token = AccessToken, refresh_token = RefreshToken
            }};
        {error, needs_oauth_flow} ->
            logger:notice(#{credential_manager => "Run oauth flow!"});
        {error, Error} ->
            devlog:log(#{handle_continue => check_credentials, error => Error})
    end;
handle_continue(Msg, State) ->
    devlog:log(#{got_unknown_msg => Msg}),
    {noreply, State}.

%% INTERNAL UTILITIES

check_credentials(
    #twitch_credentials{
        client_id = ClientId,
        secret = Secret,
        access_token = AccessToken,
        refresh_token = RefreshToken
    },
    Param
) ->
    Refresh =
        case {AccessToken, RefreshToken, Param} of
            {none, none, _} -> needs_oauth_flow;
            {_, _, initial} -> true;
            {_, _, _} -> false
        end,
    case Refresh of
        % No credentials present, need to run oauth flow to seed them
        needs_oauth_flow ->
            {error, needs_oauth_flow};
        % No refresh is needed, credentials are good
        false ->
            ok;
        % Refresh needed, use refresh token to fetch new oauth token
        true ->
            twitch:refresh_token(ClientId, Secret, RefreshToken)
    end.

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
