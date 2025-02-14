-module(focus).

-export([
    server/0,
    devlog/0,
    cm/0,
    oauth/1
]).

server() ->
    supervisor_focus:start_link().

devlog() ->
    supervisor_devlog:start_link().

cm() ->
    supervisor_credential_manager:start_link().

oauth(start) ->
    server_oauth:start([], []);
oauth(stop) ->
    server_oauth:stop([]).
