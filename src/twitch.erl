-module(twitch).

-export([
    auth/0
]).

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
