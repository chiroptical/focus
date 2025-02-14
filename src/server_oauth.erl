-module(server_oauth).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _Args) ->
    ets:new(csrf_token, [set, named_table, public]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", handler_oauth, []},
            {"/oauth/begin", handler_oauth, []},
            {"/oauth/end", handler_oauth, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

stop(_State) ->
    ok = cowboy:stop_listener(http).
