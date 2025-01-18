-module(utils_json).

-export([
    decode/1
]).

%% TODO: technically, this should parse more than just maps
decode(Bin) ->
    try json:decode(Bin) of
        Map when is_map(Map) -> {ok, Map}
    catch
        error:Error -> {error, Error}
    end.
