-module(handler_oauth).

-export([
    init/2
]).

layout(Title, Inner) ->
    [
        {~"head", [], [
            {~"title", [], [Title]}
        ]},
        {~"body", [], Inner}
    ].

view(Msg) ->
    layout(~"Authorize with Twitch", [
        case Msg of
            ~"" ->
                {~"form", [{~"action", ~"/oauth/begin"}, {~"method", ~"get"}], [
                    {~"input", [{~"type", ~"submit"}, {~"value", ~"Begin"}]}
                ]};
            _ ->
                {~"p", [], [Msg]}
        end
    ]).

%% TODO: We don't need to redirect to /oauth/begin first
init(#{path := ~"/", method := ~"GET"} = Req0, State) ->
    #{message := Message} = cowboy_req:match_qs([{message, [], ~""}], Req0),
    View = view(Message),
    Req = cowboy_req:reply(
        200,
        #{~"content-type" => ~"text/html"},
        e2h:render_html(View),
        Req0
    ),
    {ok, Req, State};
init(#{path := ~"/oauth/begin", method := ~"GET"} = Req0, State) ->
    maybe
        devlog:log(#{state => State}),
        {ok, TwitchEnv} = twitch:env(),
        {ok, ClientId} = maps:find(client_id, TwitchEnv),
        {ok, RedirectUri} = maps:find(redirect_uri, TwitchEnv),
        % QUESTION: Is 16 bytes enough?
        CsrfToken = base64:encode(crypto:strong_rand_bytes(16)),
        TwitchUrl = restc:construct_url(
            "https://id.twitch.tv",
            "oauth2/authorize",
            [
                {"client_id", ClientId},
                {"redirect_uri", RedirectUri},
                {"response_type", "code"},
                {"scope",
                    <<"user:read:chat ",
                        "user:write:chat "
                        "moderator:read:followers "
                        "channel:read:subscriptions">>},
                {"state", CsrfToken}
            ]
        ),
        Req = cowboy_req:reply(
            303,
            #{~"location" => TwitchUrl},
            ~"",
            Req0
        ),
        {ok, Req, [{csrf_token, CsrfToken} | State]}
    else
        _Err ->
            Req500 = cowboy_req:reply(
                500,
                #{~"content-type" => ~"text/plain"},
                ~"Unable to fetch Twitch environment",
                Req0
            ),
            {ok, Req500, State}
    end;
init(#{path := ~"/oauth/end", method := ~"GET"} = Req0, State) ->
    {ok, CsrfToken} = utils_proplists:find(csrf_token, State),
    #{code := AuthCode, state := GotCsrfToken} = cowboy_req:match_qs(
        [
            {code, nonempty},
            {state, nonempty}
        ],
        Req0
    ),
    case CsrfToken =:= GotCsrfToken of
        true ->
            {ok, TwitchEnv} = twitch:env(),
            {ok, ClientId} = maps:find(client_id, TwitchEnv),
            {ok, Secret} = maps:find(secret, TwitchEnv),
            {ok, RedirectUri} = maps:find(redirect_uri, TwitchEnv),
            {ok, AccessToken, RefreshToken} = twitch_auth:token(
                ClientId,
                Secret,
                RedirectUri,
                AuthCode
            ),
            ok = server_twitch_credentials:update_credentials(AccessToken, RefreshToken),
            View = view(~"All done!"),
            Req = cowboy_req:reply(
                200,
                #{~"content-type" => ~"text/html"},
                e2h:render_html(View),
                Req0
            ),
            {ok, Req, State};
        false ->
            View = view(~"Incorrect CSRF Token!"),
            Req = cowboy_req:reply(
                200,
                #{~"content-type" => ~"text/html"},
                e2h:render_html(View),
                Req0
            ),
            {ok, Req, State}
    end.
