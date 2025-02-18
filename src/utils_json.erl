-module(utils_json).

-export([
    decode_object/1
]).

-spec decode_object(binary()) -> {ok, map()} | {error, _}.
decode_object(Bin) ->
    try json:decode(Bin) of
        Map when is_map(Map) -> {ok, Map}
    catch
        error:Error -> {error, Error}
    end.
