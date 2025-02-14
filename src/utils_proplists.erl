-module(utils_proplists).

-export([
    find/2
]).

find(Key, List) ->
    case proplists:get_value(Key, List) of
        undefined -> {error, {key_not_found, Key}};
        Value -> {ok, Value}
    end.
