-module(handler_oauth).

-export([
    init/2
]).

%% TODO: Need a handler for Twitch's response /oauth/end
%% TODO: Store credentials in mnesia
init(#{path := ~"/", method := ~"GET"} = Req0, State) ->
    %% TODO: Add button to POST /oauth/begin
    Req = cowboy_req:reply(
        200,
        #{~"content-type" => ~"text/plain"},
        ~"Hello Erlang!",
        Req0
    ),
    {ok, Req, State};
init(#{path := ~"/oauth/begin", method := ~"POST"} = Req0, State) ->
    %% TODO: Redirect to initiate Twitch oauth
    Req = cowboy_req:reply(
        303,
        #{~"location" => ~"..."},
        #{},
        Req0
    ),
    {ok, Req, State}.
