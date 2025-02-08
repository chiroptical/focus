-module(supervisor_focus).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

get_env(Key) ->
    case os:getenv(Key) of
        false ->
            {error, {key_not_set, Key}};
        Value ->
            {ok, list_to_binary(Value)}
    end.

start_link() ->
    case get_env("TWITCH_CLIENT_ID") of
        {error, _} ->
            logger:notice(#{error => ~"You need to set TWITCH_CLIENT_ID!"});
        {ok, ClientId} ->
            supervisor:start_link({local, ?MODULE}, ?MODULE, ClientId)
    end.

init(ClientId) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => focus,
            start => {server_focus, start_link, []}
        },
        #{
            id => credential_manager,
            start => {server_twitch_credentials, start_link, [ClientId]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
