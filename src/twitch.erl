-module(twitch).

-export([
    parse_message_type/1,
    message_action/2,
    auth/0,
    subscribe/2,
    handle_notification/2
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

% Session welcome https://dev.twitch.tv/docs/eventsub/websocket-reference/#welcome-message
message_action(~"session_welcome", TwitchMessage) ->
    maybe
        {ok, Payload} = maps:find(~"payload", TwitchMessage),
        {ok, Session} = maps:find(~"session", Payload),
        {ok, Id} = maps:find(~"id", Session),
        {ok, {subscribe, Id}}
    else
        error ->
            {error, no_websocket_session_id_from_session_welcome}
    end;
% Session keepalive https://dev.twitch.tv/docs/eventsub/websocket-reference/#keepalive-message
message_action(~"session_keepalive", TwitchMessage) ->
    maybe
        {ok, Metadata} = maps:find(~"metadata", TwitchMessage),
        {ok, Timestamp} = maps:find(~"message_timestamp", Metadata),
        {ok, {keepalive, Timestamp}}
    else
        error ->
            {error, no_timestamp_from_session_keepalive}
    end;
% Notification message https://dev.twitch.tv/docs/eventsub/websocket-reference/#notification-message
message_action(~"notification", TwitchMessage) ->
    maybe
        {ok, Payload} = maps:find(~"payload", TwitchMessage),
        % Get the message type
        {ok, Subscription} = maps:find(~"subscription", Payload),
        {ok, Type} = maps:find(~"type", Subscription),
        % Get the event
        {ok, Event} = maps:find(~"event", Payload),
        {ok, {notification, Type, Event}}
    else
        error ->
            {error, no_event_from_notification}
    end;
message_action(Msg, _TwitchMessage) ->
    {error, {unknown_message, Msg}}.

parse_message_type(TwitchMessage) ->
    maybe
        {ok, Metadata} = maps:find(~"metadata", TwitchMessage),
        {ok, MessageType} = maps:find(~"message_type", Metadata),
        {ok, MessageType}
    else
        error ->
            {error, unable_to_parse}
    end.

post202(TwitchEnv, Payload) ->
    maybe
        {ok, UserAccessToken} = maps:find(user_access_token, TwitchEnv),
        {ok, ClientId} = maps:find(client_id, TwitchEnv),
        {ok, 202, _Headers, Body} = restc:request(
            post,
            json,
            "https://api.twitch.tv/helix/eventsub/subscriptions",
            [202],
            [
                {~"Authorization", <<"Bearer ", UserAccessToken/binary>>},
                {~"Client-Id", ClientId}
            ],
            Payload,
            []
        ),
        {ok, Body}
    else
        Err = {error, _} ->
            Err;
        {error, ErrStatus, _ErrHeaders, ErrBody} ->
            {error, ErrStatus, ErrBody}
    end.

%% Example: https://dev.twitch.tv/docs/api/reference/#create-eventsub-subscription
subscribe(follows, WebsocketSessionId) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserId} = maps:find(user_id, TwitchEnv),
        {ok, Body} = post202(
            TwitchEnv,
            #{
                type => ~"channel.follow",
                version => ~"2",
                condition => #{
                    broadcaster_user_id => UserId,
                    moderator_user_id => UserId
                },
                transport => #{
                    method => websocket,
                    session_id => WebsocketSessionId
                }
            }
        ),
        {ok, Body}
    end;
subscribe(chat, WebsocketSessionId) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserId} = maps:find(user_id, TwitchEnv),
        {ok, Body} = post202(
            TwitchEnv,
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
            }
        ),
        {ok, Body}
    end.

auth() ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserAccessToken} = maps:find(user_access_token, TwitchEnv),
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

handle_notification(~"channel.chat.message", Event) ->
    maybe
        {ok, Chatter} = maps:find(~"chatter_user_name", Event),
        {ok, Color} = maps:find(~"color", Event),
        {_, Hex} = string:take(binary_to_list(Color), "#"),
        {ok, Message} = maps:find(~"message", Event),
        {ok, MessageText} = maps:find(~"text", Message),
        Chat = io_lib:format("[~s] ~s~n", [color:true(Hex, Chatter), MessageText]),
        logger:notice(Chat)
    else
        {error, _} ->
            logger:notice(#{unable_to_handle_notification => Event})
    end;
handle_notification(~"channel.follow", Event) ->
    maybe
        {ok, Chatter} = maps:find(~"user_name", Event),
        Chat = io_lib:format("Thanks for following ~s!~n", [color:true("483248", Chatter)]),
        logger:notice(Chat)
    else
        {error, _} ->
            logger:notice(#{unable_to_handle_notification => Event})
    end;
handle_notification(Type, _Event) ->
    logger:notice(#{handle_notification_type_not_implemented => Type}).
