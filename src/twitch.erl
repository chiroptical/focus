-module(twitch).

-export([
    parse_message_type/1,
    message_action/2,
    auth/0,
    subscribe/1
]).

get_env(Key) ->
    case os:getenv(Key) of
        false ->
            {error, {key_not_set, Key}};
        Value ->
            {ok, list_to_binary(Value)}
    end.

get_twitch_env() ->
    maybe
        {ok, Secret} = get_env("TWITCH_SECRET"),
        {ok, UserAccessToken} = get_env("TWITCH_USER_ACCESS_TOKEN"),
        {ok, ClientId} = get_env("TWITCH_CLIENT_ID"),
        {ok, RefreshToken} = get_env("TWITCH_REFRESH_TOKEN"),
        {ok, UserId} = get_env("TWITCH_USER_ID"),
        {ok, #{
            secret => Secret,
            user_access_token => UserAccessToken,
            client_id => ClientId,
            refresh_token => RefreshToken,
            user_id => UserId
        }}
    else
        Err = {error, {key_not_set, _}} -> Err
    end.

get_key(Key, Map) ->
    case maps:get(Key, Map, key_doesnt_exist) of
        key_doesnt_exist ->
            error;
        Val ->
            {ok, Val}
    end.

message_action(~"session_welcome", TwitchMessage) ->
    maybe
        {ok, Payload} = get_key(~"payload", TwitchMessage),
        {ok, Session} = get_key(~"session", Payload),
        {ok, Id} = get_key(~"id", Session),
        {ok, {subscribe, Id}}
    else
        error ->
            {error, no_websocket_session_id}
    end.

parse_message_type(TwitchMessage) ->
    maybe
        {ok, Metadata} = get_key(~"metadata", TwitchMessage),
        {ok, MessageType} = get_key(~"message_type", Metadata),
        {ok, MessageType}
    else
        error ->
            {error, unable_to_parse}
    end.

%% curl -X POST 'https://api.twitch.tv/helix/eventsub/subscriptions' \
%% -H 'Authorization: Bearer 2gbdx6oar67tqtcmt49t3wpcgycthx' \
%% -H 'Client-Id: wbmytr93xzw8zbg0p1izqyzzc5mbiz' \
%% -H 'Content-Type: application/json' \
%% -d '{"type":"channel.follow","version":"2","condition":{"broadcaster_user_id":"1234", "moderator_user_id": "1234"},"transport":{"method":"webhook","callback":"https://example.com/callback","secret":"s3cre77890ab"}}'
subscribe(WebsocketSessionId) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserAccessToken} = get_key(user_access_token, TwitchEnv),
        {ok, ClientId} = get_key(client_id, TwitchEnv),
        {ok, UserId} = get_key(user_id, TwitchEnv),
        {ok, 202, _Headers, Body} = restc:request(
            post,
            json,
            "https://api.twitch.tv/helix/eventsub/subscriptions",
            [202],
            [
                {~"Authorization", <<"Bearer ", UserAccessToken/binary>>},
                {~"Client-Id", ClientId}
            ],
            #{
                type => ~"channel.chat.message",
                version => ~"1",
                condition => #{
                    broadcaster_user_id => UserId,
                    user_id => UserId
                },
                transport => #{
                    method => websocket,
                    session_id => WebsocketSessionId
                }
            },
            []
        ),
        {ok, Body}
    else
        Err = {error, _} ->
            Err;
        {error, ErrStatus, _ErrHeaders, ErrBody} ->
            {error, ErrStatus, ErrBody}
    end.

auth() ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserAccessToken} = get_key(user_access_token, TwitchEnv),
        {ok, 200, _Headers, Body} =
            restc:request(
                get,
                json,
                "https://id.twitch.tv/oauth2/validate",
                [200],
                [{<<"Authorization">>, <<"OAuth ", UserAccessToken/binary>>}]
            ),
        {ok, Body}
    else
        Err = {error, _} ->
            Err;
        {error, Status, _ErrHeaders, ErrBody} ->
            {error, Status, ErrBody}
    end.
