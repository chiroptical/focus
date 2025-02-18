-module(server_twitch_credentials).

-behaviour(gen_server).

-export([
    start_link/2,
    read_credentials/0,
    update_credentials/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2
]).

-record(twitch_credentials, {
    client_id,
    secret,
    access_token = none,
    refresh_token = none
}).

start_link(ClientId, ClientSecret) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, {ClientId, ClientSecret}, []).

update_credentials(AccessToken, RefreshToken) ->
    case global:whereis_name(?MODULE) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {update, AccessToken, RefreshToken})
    end.

read_credentials() ->
    case global:whereis_name(?MODULE) of
        undefined ->
            {error, no_manager_present};
        Pid ->
            maybe
                {ok, Credentials} ?= gen_server:call(Pid, read),
                {ok, Credentials}
            else
                no_credentials ->
                    {error, no_credentials}
            end
    end.

-define(THIRTY_MINUTES_MS, 1_800_000).

init({ClientId, ClientSecret}) ->
    erlang:send_after(?THIRTY_MINUTES_MS, self(), check_credentials),
    {ok,
        #twitch_credentials{
            client_id = ClientId,
            secret = ClientSecret
        },
        {continue, check_credentials}}.

handle_call(
    read,
    _From,
    #twitch_credentials{
        access_token = AccessToken,
        refresh_token = RefreshToken,
        client_id = ClientId
    } = State
) ->
    case {AccessToken, RefreshToken} of
        {none, _} -> {reply, no_credentials, State};
        {_, none} -> {reply, no_credentials, State};
        {_, _} -> {reply, {ok, {ClientId, AccessToken, RefreshToken}}, State}
    end;
handle_call(
    Msg,
    _From,
    State
) ->
    devlog:log(#{handle_call => Msg}),
    {noreply, State}.

handle_cast(write_credentials, State) ->
    case create(State) of
        ok ->
            {noreply, State};
        error ->
            devlog:log(#{handle_cast => write_credentials, error => unable_to_write_credentials}),
            {noreply, State}
    end;
handle_cast({update, AccessToken, RefreshToken}, State) ->
    NewState = State#twitch_credentials{access_token = AccessToken, refresh_token = RefreshToken},
    %% Write the credentials to the filesystem
    create(NewState),
    {noreply, NewState};
handle_cast(Msg, State) ->
    devlog:log(#{handle_cast => Msg}),
    {noreply, State}.

handle_info(
    check_credentials,
    #twitch_credentials{access_token = AccessToken} = State
) ->
    erlang:send_after(?THIRTY_MINUTES_MS, self(), check_credentials),

    case twitch_auth:validate(AccessToken) of
        {ok, ExpiresIn} when ExpiresIn =< 1800 ->
            maybe
                {ok, NewAccessToken, NewRefreshToken} ?=
                    check_credentials(State, refresh),
                {noreply, State#twitch_credentials{
                    access_token = NewAccessToken, refresh_token = NewRefreshToken
                }}
            else
                {error, _} = Err ->
                    Err
            end;
        {ok, _ExpiresIn} ->
            {noreply, State};
        {error, refresh_token} ->
            maybe
                {ok, NewAccessToken, NewRefreshToken} ?=
                    check_credentials(State, refresh),
                {noreply, State#twitch_credentials{
                    access_token = NewAccessToken, refresh_token = NewRefreshToken
                }}
            else
                {error, _} = Err ->
                    Err
            end;
        {error, Error} ->
            devlog:log(#{handle_info => check_credentials, error => Error}),
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

handle_continue(check_credentials, #twitch_credentials{client_id = ClientId} = State) ->
    case read(ClientId) of
        {ok, AccessToken, RefreshToken} ->
            NewState = State#twitch_credentials{
                access_token = AccessToken,
                refresh_token = RefreshToken
            },
            case check_credentials(NewState, refresh) of
                ok ->
                    {noreply, NewState};
                {ok, NewAccessToken, NewRefreshToken} ->
                    gen_server:cast(self(), write_credentials),
                    {noreply, NewState#twitch_credentials{
                        access_token = NewAccessToken, refresh_token = NewRefreshToken
                    }};
                {error, needs_oauth_flow} ->
                    logger:notice(#{credential_manager => "Run oauth flow!"}),
                    {noreply, NewState};
                {error, Error} ->
                    devlog:log(#{handle_continue => check_credentials, error => Error}),
                    {noreply, NewState}
            end;
        {error, not_found} ->
            logger:notice(#{credential_manager => "Run oauth flow!"}),
            {noreply, State}
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
            {_, _, refresh} -> true;
            {_, _, _} -> false
        end,
    case Refresh of
        % No credentials present, need to run oauth flow to seed them
        needs_oauth_flow ->
            {error, needs_oauth_flow};
        % No refresh is needed, credentials are good
        false ->
            {ok, AccessToken, RefreshToken};
        % Refresh needed, use refresh token to fetch new oauth token
        true ->
            twitch_auth:refresh_token(ClientId, Secret, RefreshToken)
    end.

create(#twitch_credentials{
    client_id = ClientId,
    access_token = none,
    refresh_token = none
}) ->
    Contents = json:encode(#{}),
    create_body(ClientId, Contents);
create(#twitch_credentials{
    client_id = ClientId,
    access_token = AccessToken,
    refresh_token = RefreshToken
}) ->
    Contents = json:encode(#{
        access_token => AccessToken,
        refresh_token => RefreshToken
    }),
    create_body(ClientId, Contents).

read(ClientId) ->
    maybe
        Filename = make_filename(ClientId),
        {ok, Contents} ?= file:read_file(Filename),
        {ok, Decoded} ?= utils_json:decode_object(Contents),
        AccessToken = maps:get(~"access_token", Decoded, none),
        RefreshToken = maps:get(~"refresh_token", Decoded, none),
        {ok, AccessToken, RefreshToken}
    else
        {error, _} ->
            {error, not_found}
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
        ok ?= make_dir_if_not_exists(),
        Filename = make_filename(ClientId),
        ok ?= file:write_file(Filename, Contents),
        ok
    else
        {error, _} ->
            error
    end.
