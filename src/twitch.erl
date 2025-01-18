-module(twitch).

-export([
    auth/0,
    parse_message_type/1,
    message_action/2
]).

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

auth() ->
    %%    await fetch('https://id.twitch.tv/oauth2/validate', {
    %% 	method: 'GET',
    %% 	headers: {
    %% 		'Authorization': 'OAuth ' + OAUTH_TOKEN
    %% 	}
    %% });
    case os:getenv("TWITCH_OAUTH_TOKEN") of
        false ->
            {error, twitch_oauth_token_not_set};
        TwitchOAuthToken ->
            Binary = list_to_binary(TwitchOAuthToken),
            case
                restc:request(
                    get,
                    json,
                    "https://id.twitch.tv/oauth2/validate",
                    [200],
                    [{<<"Authorization">>, <<"OAuth ", Binary/binary>>}]
                )
            of
                {ok, Status, _Headers, Body} -> {ok, Status, Body};
                {error, Status, _Headers, Body} -> {error, Status, Body};
                {error, Reason} -> {error, Reason}
            end
    end.
