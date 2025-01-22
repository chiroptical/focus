-module(server_focus).
-behaviour(gen_server).

-export([
    start_link/0,
    enable_debug_messages/0,
    disable_debug_messages/0,
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    handle_continue/2
]).

-record(state, {gun_connection_pid, enable_debug_messages = false, keepalive = none}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enable_debug_messages() ->
    gen_server:cast(self(), enable_debug_messages).

disable_debug_messages() ->
    gen_server:cast(self(), disable_debug_messages).

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

debug_log(true, Msg) ->
    logger:notice(Msg);
debug_log(false, _Msg) ->
    ok.

handle_info(
    {gun_upgrade, ConnPid, _StreamRef, [<<"websocket">>], _Headers},
    #state{gun_connection_pid = ConnPid, enable_debug_messages = DebugMessages} = State
) ->
    debug_log(DebugMessages, #{connection => upgraded}),
    {noreply, State};
handle_info(
    {gun_ws, ConnPid, _StreamRef, {text, Bin}},
    #state{gun_connection_pid = ConnPid, enable_debug_messages = DebugMessages} = State
) ->
    case utils_json:decode(Bin) of
        {ok, TwitchMessage} ->
            maybe
                {ok, MessageType} = twitch:parse_message_type(TwitchMessage),
                debug_log(DebugMessages, #{got_twitch_message => MessageType}),
                {ok, MessageAction} = twitch:message_action(MessageType, TwitchMessage),
                gen_server:cast(self(), MessageAction)
            else
                {error, _} ->
                    logger:notice(#{unable_to_action => TwitchMessage})
            end;
        {error, _} ->
            logger:notice(#{unable_to_parse => Bin})
    end,
    {noreply, State};
handle_info(Msg, #state{enable_debug_messages = DebugMessages} = State) ->
    debug_log(DebugMessages, #{got_unknown_message => Msg}),
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

% Control whether or not to print debug messages
handle_cast(enable_debug_messages, State) ->
    {noreply, State#state{enable_debug_messages = true}};
handle_cast(disable_debug_messages, State) ->
    {noreply, State#state{enable_debug_messages = false}};
% Messages from Twitch
handle_cast({subscribe, WebsocketSessionId}, State) ->
    {ok, _Body} = twitch:subscribe(chat, WebsocketSessionId),
    % gen_server:cast(self(), {subscribe, follows, WebsocketSessionId}),
    %% gen_server:cast(self(), {subscribe, subscribers, WebsocketSessionId})
    {noreply, State};
handle_cast({subscribe, follows, WebsocketSessionId}, State) ->
    {ok, _Body} = twitch:subscribe(follows, WebsocketSessionId),
    {noreply, State};
handle_cast({keepalive, Timestamp}, State) ->
    {noreply, State#state{keepalive = Timestamp}};
handle_cast({notification, Type, Event}, State) ->
    twitch:handle_notification(Type, Event),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue(upgrade_connection, #state{gun_connection_pid = ConnPid} = State) ->
    {ok, _Protocol} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, "/ws"),
    {noreply, State}.
