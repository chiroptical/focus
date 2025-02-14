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

-record(state, {
    gun_connection_pid, keepalive = none, websocket_session_id = none
}).

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
    {ok, #state{gun_connection_pid = ConnPid}, {continue, upgrade_connection}}.

handle_info(
    {gun_upgrade, ConnPid, _StreamRef, [~"websocket"], _Headers},
    #state{gun_connection_pid = ConnPid} = State
) ->
    devlog:log(#{websocket => upgraded}),
    {noreply, State};
handle_info(
    {gun_ws, ConnPid, _StreamRef, {text, JsonBody}},
    #state{gun_connection_pid = ConnPid} = State
) ->
    case utils_json:decode(JsonBody) of
        {ok, TwitchMessage} ->
            maybe
                devlog:log(#{decoded_twitch_message => TwitchMessage}),
                {ok, MessageType} ?= twitch:parse_message_type(TwitchMessage),
                devlog:log(#{parsed_twitch_message => MessageType}),
                {ok, MessageAction} ?= twitch:message_action(MessageType, TwitchMessage),
                devlog:log(#{twitch_message_action => MessageAction}),
                gen_server:cast(self(), MessageAction)
            else
                {error, GotError} ->
                    devlog:log(#{twitch_message_error => GotError})
            end;
        {error, _} ->
            devlog:log(#{unable_to_parse_twitch_message => JsonBody})
    end,
    {noreply, State};
handle_info(Msg, #state{} = State) ->
    devlog:log(#{handle_info_unknown_message => Msg}),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    devlog:log(#{handle_call_unknown_message => Msg}),
    {noreply, State}.

% Messages from Twitch
handle_cast({subscribe, WebsocketSessionId} = Action, State) ->
    maybe
        devlog:log(#{attempting => Action}),
        {ok, Body} ?= twitch:subscribe(chat, WebsocketSessionId),
        devlog:log(#{completed => Action, got => Body}),
        gen_server:cast(self(), {subscribe, follows, WebsocketSessionId}),
        gen_server:cast(self(), {subscribe, subscribers, WebsocketSessionId}),
        {noreply, State#state{websocket_session_id = WebsocketSessionId}}
    else
        Err ->
            devlog:log(#{errored => Action, error => Err}),
            {noreply, State}
    end;
handle_cast({subscribe, follows, WebsocketSessionId} = Action, State) ->
    maybe
        devlog:log(#{attempting => Action}),
        {ok, Body} ?= twitch:subscribe(follows, WebsocketSessionId),
        devlog:log(#{completed => Action, got => Body})
    else
        Err ->
            devlog:log(#{errored => Action, error => Err})
    end,
    {noreply, State};
handle_cast({subscribe, subscribers, WebsocketSessionId} = Action, State) ->
    maybe
        devlog:log(#{attempting => Action}),
        {ok, Body} ?= twitch:subscribe(subscribers, WebsocketSessionId),
        devlog:log(#{completed => Action, got => Body})
    else
        Err ->
            devlog:log(#{errored => Action, error => Err})
    end,
    {noreply, State};
handle_cast({keepalive, Timestamp}, State) ->
    devlog:log(#{keepalive => Timestamp}),
    {noreply, State#state{keepalive = Timestamp}};
handle_cast({notification, Type, Event} = Notification, State) ->
    devlog:log(#{attempting => Notification}),
    % TODO: handle_notification should return `ok` | `error`
    % so we can use `maybe` here and not let our gen_server explode
    twitch:handle_notification(Type, Event),
    devlog:log(#{completed => Notification}),
    {noreply, State};
handle_cast(Msg, State) ->
    devlog:log(#{handle_cast_unknown_message => Msg}),
    {noreply, State}.

handle_continue(upgrade_connection, #state{gun_connection_pid = ConnPid} = State) ->
    devlog:log(#{start => awaiting_gun_up}),
    {ok, _Protocol} = gun:await_up(ConnPid),
    devlog:log(#{start => ws_upgrade}),
    gun:ws_upgrade(ConnPid, "/ws"),
    {noreply, State}.
