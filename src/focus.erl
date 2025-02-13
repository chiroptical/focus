-module(focus).

-export([
    server/0,
    devlog/0,
    cm/0
]).

server() ->
    supervisor_focus:start_link().

devlog() ->
    supervisor_devlog:start_link().

cm() ->
    supervisor_credential_manager:start_link().
