-module(twitch).

-export([
    parse_message_type/1,
    message_action/2,
    auth/0,
    subscribe/2,
    handle_notification/2,
    msg/1,
    ban/1
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

post(TwitchEnv, Url, Payload, StatusExpected) ->
    maybe
        {ok, UserAccessToken} = maps:find(user_access_token, TwitchEnv),
        {ok, ClientId} = maps:find(client_id, TwitchEnv),
        {ok, StatusExpected, _Headers, Body} = restc:request(
            post,
            json,
            Url,
            [StatusExpected],
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

eventsub_subscription(TwitchEnv, Payload) ->
    post(TwitchEnv, "https://api.twitch.tv/helix/eventsub/subscriptions", Payload, 202).

%% Example: https://dev.twitch.tv/docs/api/reference/#create-eventsub-subscription
subscribe(follows, WebsocketSessionId) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserId} = maps:find(user_id, TwitchEnv),
        {ok, Body} = eventsub_subscription(
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
subscribe(subscribers, WebsocketSessionId) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserId} = maps:find(user_id, TwitchEnv),
        {ok, Body} = eventsub_subscription(
            TwitchEnv,
            #{
                type => ~"channel.subscribe",
                version => ~"1",
                condition => #{
                    broadcaster_user_id => UserId
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
        {ok, Body} = eventsub_subscription(
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

twitch_user_id(TwitchEnv, UserName) ->
    maybe
        {ok, ClientId} = maps:find(client_id, TwitchEnv),
        {ok, UserAccessToken} = maps:find(user_access_token, TwitchEnv),
        {ok, 200, _Headers, Body} =
            restc:request(
                get,
                json,
                restc:construct_url("https://api.twitch.tv/helix/users", [{"login", UserName}]),
                [200],
                [
                    {<<"Authorization">>, <<"Bearer ", UserAccessToken/binary>>},
                    {~"Client-Id", ClientId}
                ]
            ),
        {ok, Body}
    else
        Err = {error, _} ->
            Err;
        {error, Status, _ErrHeaders, ErrBody} ->
            {error, Status, ErrBody}
    end.

ban(UserName) when is_binary(UserName) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, StreamerId} = maps:find(user_id, TwitchEnv),
        {ok, UserResponse} = twitch_user_id(TwitchEnv, UserName),
        {ok, Data} = maps:find(~"data", UserResponse),
        case Data of
            [UserData] ->
                UserId = maps:find(~"id", UserData),
                {ok, _Body} =
                    post(
                        TwitchEnv,
                        restc:construct_url(
                            "https://api.twitch.tv/helix/moderation/bans",
                            [
                                {"broadcaster_id", binary_to_list(StreamerId)},
                                {"moderator_id", binary_to_list(StreamerId)}
                            ]
                        ),
                        #{
                            data => #{
                                user_id => UserId
                            }
                        },
                        200
                    ),
                ok;
            _ ->
                unable_to_find_user_data
        end
    else
        Err = {error, _} ->
            Err;
        {error, Status, _ErrHeaders, ErrBody} ->
            {error, Status, ErrBody}
    end.

msg(Message) when is_binary(Message) ->
    maybe
        {ok, TwitchEnv} = get_twitch_env(),
        {ok, UserId} = maps:find(user_id, TwitchEnv),
        {ok, _Body} =
            post(
                TwitchEnv,
                "https://api.twitch.tv/helix/chat/messages",
                #{
                    broadcaster_id => UserId,
                    sender_id => UserId,
                    message => Message
                },
                200
            ),
        ok
    else
        Err = {error, _} ->
            Err;
        {error, Status, _ErrHeaders, ErrBody} ->
            {error, Status, ErrBody}
    end.

from_tier(~"1000") ->
    ~"T1";
from_tier(~"2000") ->
    ~"T2";
from_tier(~"3000") ->
    ~"T3".

display_subscription(true, Chatter, Tier) ->
    io_lib:format(
        "~s was gifted a ~s sub, I appreciate you!~n",
        [color:true("0096FF", Chatter), from_tier(Tier)]
    );
display_subscription(false, Chatter, Tier) ->
    io_lib:format(
        "~s subscribed at ~s, I appreciate you!~n",
        [color:true("0096FF", Chatter), from_tier(Tier)]
    ).

handle_notification(~"channel.chat.message", Event) ->
    maybe
        {ok, Chatter} = maps:find(~"chatter_user_name", Event),
        {ok, Color} = maps:find(~"color", Event),
        ColorWithDefault =
            case Color of
                <<>> -> "6495ED";
                Color -> Color
            end,
        {_, Hex} = string:take(binary_to_list(ColorWithDefault), "#"),
        {ok, Message} = maps:find(~"message", Event),
        {ok, MessageText} = maps:find(~"text", Message),
        Chat = io_lib:format("[~s] ~s~n", [color:true(Hex, Chatter), MessageText]),
        logger:notice(Chat)
    else
        {error, _} ->
            logger:notice(#{unable_to_handle_notification => Event})
    end;
handle_notification(~"channel.subscribe", Event) ->
    maybe
        {ok, Chatter} = maps:find(~"user_name", Event),
        {ok, Tier} = maps:find(~"tier", Event),
        {ok, IsGifted} = maps:find(~"is_gift", Event),
        Chat = display_subscription(IsGifted, Chatter, Tier),
        logger:notice(Chat)
    else
        {error, _} ->
            logger:notice(#{unable_to_handle_notification => Event})
    end;
handle_notification(~"channel.follow", Event) ->
    maybe
        {ok, Chatter} = maps:find(~"user_name", Event),
        Chat = io_lib:format("Thanks for following ~s!~n", [color:true("C3B1E1", Chatter)]),
        logger:notice(Chat)
    else
        {error, _} ->
            logger:notice(#{unable_to_handle_notification => Event})
    end;
handle_notification(Type, _Event) ->
    logger:notice(#{handle_notification_type_not_implemented => Type}).
