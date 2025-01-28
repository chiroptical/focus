-module(server_oauth).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", handler_oauth, []},
            {"/oauth/begin", handler_oauth, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

stop(_State) ->
    ok = cowboy:stop_listener(http).
