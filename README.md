focus
=====

Bring Twitch chat directly into your Erlang development shell for maximum focus.

Developed live on [![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)</br>

TODO
---

- [ ] Separate `twitch` module into `twitch_auth`, `twitch_api`, `twitch_websocket`
- [ ] Break up `twitch:subscribe/2` into more specific functions
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#reconnect-message
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#revocation-message
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#close-message
- [ ] Add convenient mechanism to stop `server_focus`, e.g. `focus:server(start)`
- [ ] can we automatically get the two nodes `focus` and `devlog` to be connected?
  - Use `net_adm:localhost()` to get, e.g., `wilder`
  - When starting either server, just try `net_adm:ping(focus@wilder)` and `net_adm:ping(devlog@wilder)`
  - Note: you can use `list_to_atom/1` to convert a string to an atom
- [ ] Try setting up focus in another project as a dev dependency
- [ ] Record video to set this up in Erlang
- [ ] Record video to set this up in Elixir
- [ ] Record video to set this up in Gleam
- [ ] Should we store user metadata? For example,
      - Highlight @username in the user's specified color if we know it
      - Recall a user's chat history to decide if we should ban them
      - Store first time chatter messages and then release them, e.g. no follow bot messages

Environment variables and set up
----

You'll need to set the following environment variables to use the focus server.

```
export TWITCH_CLIENT_ID='...'
export TWITCH_SECRET='...'
export TWITCH_REDIRECT_URI='http://localhost:8080/oauth/end'
export TWITCH_USER_ID='...'
```

You can get your client id and secret by registering an application at
https://dev.twitch.tv/console. Make sure the oauth redirect uri is set to
http://localhost:8080/oauth/end`. Once those are set, you'll need to set up
oauth credentials. Open a shell,

```console
$ rebar3 shell

1> focus:devlog().
{ok, ...}
2> focus:cm().
{ok, ...}
3> focus:oauth(start).
{ok, ...}
```

This starts the devlog service, credential manager service, and oauth service.
DO NO SHOW THIS ON STREAM, it could leak credentials. Navigate to
http://localhost:8080, click `Begin`, fill out Twitch details, it should navigate to
a page that says `All set!`. If it does not, please file a bug report being
careful not to leak any of your secrets please!

This will store your credentials on your filesystem in your `user_data`
directory according to Erlang's
[filename](https://www.erlang.org/doc/apps/stdlib/filename.html#basedir/3) library.

You can stop the oauth server once this is done and continue with the set up,

```console
4> focus:oauth(stop).
ok
```

If you didn't know your `TWITCH_USER_ID` you can now get it with,

```console
5> twitch_auth:validate().
ok
```

Running focus
---

Focus is built up from four services, you've seen three in the previous section

- `focus:devlog()`: prints logs from Twitch websockets, requests, and focus
  services
- `focus:cm()`: the credential manager reads, stores, and refreshes the Twitch
   OAuth token
- `focus:server()`: the server which displays Twitch chat, follower, and
   subscriber notifications which arrive on the websocket
- `focus:oauth(start)`: the oauth server for fetching tokens from Twitch if your
   setting up for the first time or your refresh token is revoked

If you have valid credentials, you'll want to start the `devlog`, `cm`, and `server`.
When I stream, I open two terminal windows. The first is off-stream and runs `devlog`
and `cm`. The second is the one displayed on stream and runs the `server`.

```console
# In off-stream terminal
$ rebar3 shell --sname=devlog
(devlog@Barrys-MacBook-Air)1> focus:devlog().
{ok, ...}
(devlog@Barrys-MacBook-Air)2> focus:cm().
{ok, ...}

# In on-stream terminal
$ rebar3 shell --sname=focus
(focus@Barrys-MacBook-Air)1> net_adm:ping('devlog@Barrys-MacBook-Air').
pong
(focus@Barrys-MacBook-Air)2> focus:server().
{ok, ...}
```

If you don't get `{ok, Pid}` from the services and `pong` from `net_adm:ping`
something has gone wrong. Please check you've followed the directions and submit
an issue otherwise. Be careful not to leak your credentials.
