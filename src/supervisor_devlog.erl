-module(supervisor_devlog).
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
    maybe
        {ok, ClientId} = get_env("TWITCH_CLIENT_ID"),
        {ok, ClientSecret} = get_env("TWITCH_SECRET"),
        ok = supervisor:start_link({local, ?MODULE}, ?MODULE, {ClientId, ClientSecret})
    else
        {error, {key_not_set, Key}} ->
            logger:notice(#{error => ~"Missing env var", env_var => Key});
        {error, Err} ->
            logger:notice(#{start_link => ?MODULE, error => Err})
    end.

init({ClientId, ClientSecret}) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => devlog,
            start => {server_devlog, start_link, []}
        },
        #{
            id => credential_manager,
            start => {server_twitch_credentials, start_link, [ClientId, ClientSecret]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
