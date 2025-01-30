-module(handler_oauth).

-export([
    init/2
]).

index(~"") ->
    <<"<!doctype html>", "<html>", "<head>", "<title>Authorize with Twitch</title>", "</head>",
        "<body>", "<form action=\"/oauth/begin\" method=\"get\">",
        "<input type=\"submit\" value=\"Begin\">", "</form>", "</body>", "</html>">>;
index(Msg) ->
    <<"<!doctype html>", "<html>", "<head>", "<title>Authorize with Twitch</title>", "</head>",
        "<body>", "<p>", Msg/binary, "</p>", "</body>", "</html>">>.

init(#{path := ~"/", method := ~"GET"} = Req0, State) ->
    %% TODO: Add button to POST /oauth/begin
    #{message := Message} = cowboy_req:match_qs([{message, [], ~""}], Req0),
    Req = cowboy_req:reply(
        200,
        #{~"content-type" => ~"text/html"},
        index(Message),
        Req0
    ),
    {ok, Req, State};
init(#{path := ~"/oauth/begin", method := ~"GET"} = Req0, State) ->
    %% TODO: Redirect to initiate Twitch OAuth
    Req = cowboy_req:reply(
        303,
        #{~"location" => ~"/oauth/end"},
        ~"",
        Req0
    ),
    {ok, Req, State};
%% TODO: Finish this handler for Twitch's GET
%% TODO: Store the results in an mnesia table
init(#{path := ~"/oauth/end", method := ~"GET"} = Req0, State) ->
    %% TODO: Handle Twitch request and store information
    Req = cowboy_req:reply(
        303,
        #{~"location" => ~"/?message=All done"},
        ~"",
        Req0
    ),
    {ok, Req, State}.
