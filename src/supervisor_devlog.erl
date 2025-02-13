-module(supervisor_devlog).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => devlog,
            start => {server_devlog, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
