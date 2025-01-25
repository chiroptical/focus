focus
=====

Bring Twitch chat directly into your Erlang development shell for maximum focus.

Developed live on [![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)</br>

TODO
---

- [ ] Add `twitch:ban(UserName)` for banning users from chat
- [ ] `devlog:log/1` should check if the process is local, if not use `erpc:multicast/4`
- [ ] Refactor logging and error handling to use `devlog:log/1`
- [ ] The `devlog` server doesn't need to have `enable`/`disable`
- [ ] `server_focus` should be supervised
- [ ] `devlog` should be supervised
- [ ] rebar3 command to automatically start shell with name `focus`
- [ ] rebar3 command to automatically start shell with name `devlog` and connect to the `focus` node
- [ ] Break up `twitch:subscribe/2` into more specific functions
- [ ] How do we deal with emojis over the wire? 👋
- [ ] See https://github.com/chiroptical/focus/issues/1
- [ ] See https://github.com/chiroptical/focus/issues/2
- [ ] Mechanism to automatically refresh Twitch credentials
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#reconnect-message
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#revocation-message
- [ ] Handle https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#close-message
- [ ] Add convenient mechanism to stop `server_focus`
- [ ] Add a small cowboy server to handle user auth token https://ninenines.eu/docs/en/cowboy/2.9/guide/static_files/
  - [ ] Need an index.html with a single button to start the flow
  - [ ] Redirect user to handle OAuth
  - [ ] Handle request from Twitch
  - [ ] Store credentials
  - [ ] Redirect user to complete.html
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
