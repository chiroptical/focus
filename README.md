focus
=====

Bring Twitch chat directly into your Erlang development shell for maximum focus.

Developed live on [![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)</br>

TODO
---

- [ ] Add a small cowboy server to handle user auth token https://ninenines.eu/docs/en/cowboy/2.9/guide/static_files/
  - [x] Need an index.html with a single button to start the flow
  - [x] Redirect user to handle OAuth
  - [ ] Store and retrieve credentials
    - [ ] focus:server should create mnesia schema and twitch_credential table
          if they don't exist. 
    - [ ] focus:server should control starting mnesia on focus@hostname
  - [ ] Redirect user to complete.html
  - [ ] Handle request from Twitch
- [ ] refactor `twitch` module, it shouldn't really require `maybe` expressions.
      the maybe expressions are the responsibility of the caller.
- [ ] Break up `twitch:subscribe/2` into more specific functions
- [ ] See https://github.com/chiroptical/focus/issues/2
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#reconnect-message
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#revocation-message
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#close-message
- [ ] Add convenient mechanism to stop `server_focus`
- [ ] Store twitch user name to user id mnesia table
- [ ] Store twitch messages in mnesia table
- [ ] can we automatically get the two nodes `focus` and `devlog` to be connected?
- [ ] Record video to set this up in Erlang
- [ ] Record video to set this up in Elixir
- [ ] Record video to set this up in Gleam

Environment variables and set up
----

You'll need to set the following environment variables to use focus.

```
export TWITCH_CLIENT_ID='...'
export TWITCH_SECRET='...'
export TWITCH_REDIRECT_URI='http://localhost:3000/oauth'
export TWITCH_USER_ACCESS_TOKEN='...'
export TWITCH_REFRESH_TOKEN='...'
export TWITCH_USER_ID='...'
```

You can get your client id and secret by registering an application at
`https://dev.twitch.tv/console`. Once those are set, you currently need to get
`twitch-cli` (https://dev.twitch.tv/docs/cli/). I get it from through my nix
shell.

```
twitch-cli configure -i $TWITCH_CLIENT_ID -s $TWITCH_SECRET # assuming you have already set these
twitch-cli token --user-token --scopes "user:read:chat user:write:chat moderator:read:followers channel:read:subscriptions"
```

The tool will open a webserver at port 3000 and run the oauth flow for you.
You will need to set your "OAuth Redirect URLs"  to `http://localhost:3000`
for this step to work. This will allow you to add your user access token and
refresh token. Once all of that is exported, you can run `rebar3 shell` and
`twitch:auth()` should get you your user id. If that doesn't work, you have
configured something incorrectly.

Running focus
---

Open two terminal windows, the first one I'll refer to as 'focus' and the second
'devlog'. The focus terminal will display events from Twitch, e.g. messages,
followers, subscribers. The devlog terminal will display **all** of the event
data being ingested and processed to make that happen. The devlog terminal is
**not** meant to be seen by your viewers!

In the focus terminal, run the following `rebar3 focus` this will start the
erlang development shell with a name e.g. on my machine,

```
(focus@wilder)1>
```

The value in parentheses is known as the "node name" and this is how you
connect erlang nodes together. We'll do that shortly. In the focus erlang
shell, run `focus:server().`. The server is now running and if you
configured it correctly you should recieve messages, follows, subs.

In the devlog terminal, run the following `rebar3 devlog` this will start
the erlang development shell with a name, e.g. on my machine,

```
(devlog@wilder)1>
```

In the devlog log, run the following commands,

```
(devlog@wilder)1> net_adm:ping('focus@wilder').
pong
(devlog@wilder)2> focus:devlog().
{ok, <0.400.0>}
```

If you see, `pong` and an `ok` tuple you should start seeing keepalive messages
in the devlog erlang shell.
