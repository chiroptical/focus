-module(devlog).

-export([log/1]).

-define(DEVLOG, server_devlog).

log(Msg) ->
    case global:whereis_name(?DEVLOG) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {log, Msg})
    end.
