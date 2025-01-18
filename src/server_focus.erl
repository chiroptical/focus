-module(server_focus).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    handle_continue/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ConnPid} = gun:open(
        "eventsub.wss.twitch.tv",
        443,
        #{
            protocols => [http],
            transport => ssl,
            tls_opts => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}]
        }
    ),
    {ok, ConnPid, {continue, upgrade_connection}}.

handle_info({gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], _Headers}, ConnPid) ->
    logger:notice(#{connection => upgraded}),
    {noreply, ConnPid};
handle_info({gun_ws, ConnPid, _StreamRef, {text, Bin}}, ConnPid) ->
    case utils_json:decode(Bin) of
        {ok, TwitchMessage} ->
            maybe
                {ok, MessageType} = twitch:parse_message_type(TwitchMessage),
                logger:notice(#{got_twitch_message => MessageType}),
                {ok, MessageAction} = twitch:message_action(MessageType, TwitchMessage),
                gen_server:cast(self(), MessageAction)
            else
                {error, _} ->
                    logger:notice(#{unable_to_action => TwitchMessage})
            end;
        {error, _} ->
            logger:notice(#{unable_to_parse => Bin})
    end,
    {noreply, ConnPid};
handle_info(Msg, ConnPid) ->
    logger:notice(#{got_unknown_message => Msg}),
    {noreply, ConnPid}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({subscribe, WebsocketSessionId}, State) ->
    {ok, Body} = twitch:subscribe(WebsocketSessionId),
    logger:notice(#{got_subscribe_body => Body}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue(upgrade_connection, ConnPid) ->
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, "/ws"),
    {noreply, ConnPid}.
