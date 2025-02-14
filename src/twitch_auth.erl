-module(twitch_auth).

-export([
    validate/0,
    validate/1,
    refresh_token/3,
    token/4
]).

validate_request(AccessToken) ->
    restc:request(
        get,
        json,
        "https://id.twitch.tv/oauth2/validate",
        [200, 401],
        [{<<"Authorization">>, <<"OAuth ", AccessToken/binary>>}]
    ).

validate() ->
    maybe
        {ok, {_ClientId, AccessToken, _RefreshToken}} ?=
            server_twitch_credentials:read_credentials(),
        {ok, 200, _Headers, Body} ?= validate_request(AccessToken),
        devlog:log(#{validate => Body}),
        ok
    else
        {ok, 401, _, _} ->
            {error, refresh_token};
        {error, _} = Err ->
            Err
    end.

validate(AccessToken) ->
    maybe
        {ok, Status, _Headers, Body} ?= validate_request(AccessToken),
        case Status of
            200 ->
                utils_proplists:find(~"expires_in", Body);
            401 ->
                {error, refresh_token}
        end
    else
        Err = {error, _} ->
            Err;
        {error, ErrStatus, _ErrHeaders, ErrBody} ->
            {error, {ErrStatus, ErrBody}}
    end.

refresh_token(ClientId, Secret, RefreshToken) ->
    maybe
        {ok, 200, _Headers, Body} ?=
            restc:request(
                post,
                json,
                "https://id.twitch.tv/oauth2/token",
                [200],
                [],
                #{
                    client_id => ClientId,
                    client_secret => Secret,
                    grant_type => refresh_token,
                    refresh_token => RefreshToken
                },
                []
            ),
        {ok, AccessToken} ?= utils_proplists:find(~"access_token", Body),
        {ok, NextRefreshToken} ?= utils_proplists:find(~"refresh_token", Body),
        {ok, AccessToken, NextRefreshToken}
    else
        Err = {error, _} ->
            Err;
        {error, Status, _ErrHeaders, ErrBody} ->
            {error, {Status, ErrBody}}
    end.

token(ClientId, Secret, RedirectUri, AuthCode) ->
    maybe
        {ok, 200, _Headers, Body} ?=
            restc:request(
                post,
                json,
                "https://id.twitch.tv/oauth2/token",
                [200],
                [],
                #{
                    client_id => ClientId,
                    client_secret => Secret,
                    grant_type => authorization_code,
                    code => AuthCode,
                    redirect_uri => RedirectUri
                },
                []
            ),
        {ok, AccessToken} ?= utils_proplists:find(~"access_token", Body),
        {ok, RefreshToken} ?= utils_proplists:find(~"refresh_token", Body),
        {ok, AccessToken, RefreshToken}
    else
        Err = {error, _} ->
            Err;
        {error, Status, _ErrHeaders, ErrBody} ->
            {error, {Status, ErrBody}}
    end.
