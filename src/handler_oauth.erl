-module(handler_oauth).

-export([
    init/2
]).

% TODO: Use https://github.com/bunopnu/e2h to generate views
index(~"") ->
    <<"<!doctype html>", "<html>", "<head>", "<title>Authorize with Twitch</title>", "</head>",
        "<body>", "<form action=\"/oauth/begin\" method=\"get\">",
        "<input type=\"submit\" value=\"Begin\">", "</form>", "</body>", "</html>">>;
index(Msg) ->
    <<"<!doctype html>", "<html>", "<head>", "<title>Authorize with Twitch</title>", "</head>",
        "<body>", "<p>", Msg/binary, "</p>", "</body>", "</html>">>.

%% TODO: We don't need to redirect to /oauth/begin first
init(#{path := ~"/", method := ~"GET"} = Req0, State) ->
    #{message := Message} = cowboy_req:match_qs([{message, [], ~""}], Req0),
    Req = cowboy_req:reply(
        200,
        #{~"content-type" => ~"text/html"},
        index(Message),
        Req0
    ),
    {ok, Req, State};
%% TODO: Redirect to initiate Twitch OAuth
%% DOCS: https://dev.twitch.tv/docs/authentication/getting-tokens-oauth/#authorization-code-grant-flow
init(#{path := ~"/oauth/begin", method := ~"GET"} = Req0, State) ->
    _TwitchUrl = restc:construct_url(
        "https://id.twitch.tv/oauth2/authorize",
        [
            {"client_id", "..."},
            {"redirect_uri", "..."},
            {"response_type", "code"},
            {"scope", "..."},
            {"state", "..."}
        ]
    ),
    Req = cowboy_req:reply(
        303,
        #{~"location" => ~"/oauth/end"},
        ~"",
        Req0
    ),
    {ok, Req, State};
%% TODO: Finish this handler for Twitch's redirect, see 'DOCS' above
%% TODO: Store the results in an mnesia table
%% TODO: Use the authorization code to get a token, see 'DOCS' above
init(#{path := ~"/oauth/end", method := ~"GET"} = Req0, State) ->
    Req = cowboy_req:reply(
        303,
        #{~"location" => ~"/?message=All done"},
        ~"",
        Req0
    ),
    {ok, Req, State}.
