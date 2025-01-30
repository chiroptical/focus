-module(handler_oauth).

-export([
    init/2
]).

% TODO: Use https://github.com/bunopnu/e2h to generate views
view(~"") ->
    <<"<!doctype html>", "<html>", "<head>", "<title>Authorize with Twitch</title>", "</head>",
        "<body>", "<form action=\"/oauth/begin\" method=\"get\">",
        "<input type=\"submit\" value=\"Begin\">", "</form>", "</body>", "</html>">>;
view(Msg) ->
    <<"<!doctype html>", "<html>", "<head>", "<title>Authorize with Twitch</title>", "</head>",
        "<body>", "<p>", Msg/binary, "</p>", "</body>", "</html>">>.

%% TODO: We don't need to redirect to /oauth/begin first
init(#{path := ~"/", method := ~"GET"} = Req0, State) ->
    #{message := Message} = cowboy_req:match_qs([{message, [], ~""}], Req0),
    Req = cowboy_req:reply(
        200,
        #{~"content-type" => ~"text/html"},
        view(Message),
        Req0
    ),
    {ok, Req, State};
%% TODO: Store 'Secret' in 'State' so we can look it back up
init(#{path := ~"/oauth/begin", method := ~"GET"} = Req0, State) ->
    maybe
        devlog:log(#{state => State}),
        {ok, TwitchEnv} = twitch:env(),
        {ok, ClientId} = maps:find(client_id, TwitchEnv),
        % QUESTION: Is 16 bytes enough?
        Secret = base64:encode(crypto:strong_rand_bytes(16)),
        TwitchUrl = restc:construct_url(
            "https://id.twitch.tv",
            "oauth2/authorize",
            [
                {"client_id", ClientId},
                {"redirect_uri", "http:localhost:8080/oauth/end"},
                {"response_type", "code"},
                {"scope",
                    <<"user:read:chat ",
                        "user:write:chat "
                        "moderator:read:followers "
                        "channel:read:subscriptions">>},
                {"state", Secret}
            ]
        ),
        Req = cowboy_req:reply(
            303,
            #{~"location" => TwitchUrl},
            ~"",
            Req0
        ),
        {ok, Req, State}
    else
        _Err ->
            Req500 = cowboy_req:reply(
                500,
                #{~"content-type" => ~"text/plain"},
                ~"Unable to fetch Twitch environment",
                Req0
            ),
            {ok, Req500, State}
    end;
%% DOCS: https://dev.twitch.tv/docs/authentication/getting-tokens-oauth/#authorization-code-grant-flow
%% TODO: Finish this handler for Twitch's redirect, see 'DOCS'
%% TODO: Store the results in an mnesia table
%% TODO: Use the authorization code to get a token, see 'DOCS'
init(#{path := ~"/oauth/end", method := ~"GET"} = Req0, State) ->
    Req = cowboy_req:reply(
        303,
        #{~"location" => ~"/?message=All done"},
        ~"",
        Req0
    ),
    {ok, Req, State}.
