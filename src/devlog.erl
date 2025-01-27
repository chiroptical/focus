-module(devlog).

-export([
    log/1,
    log_inner/1
]).

-define(DEVLOG, server_devlog).

-doc """
If `devlog` is not running on our current node, broadcast the log message to all
connected nodes once.
""".
log(Msg) ->
    case erlang:whereis(?DEVLOG) of
        undefined ->
            erpc:multicast(nodes(), ?MODULE, log_inner, [Msg]);
        _Pid ->
            gen_server:cast(?DEVLOG, {log, Msg})
    end.

-doc """
DO NOT CALL THIS FUNCTION DIRECTLY
""".
log_inner(Msg) ->
    case erlang:whereis(?DEVLOG) of
        undefined ->
            ok;
        _Pid ->
            gen_server:cast(?DEVLOG, {log, Msg})
    end.
