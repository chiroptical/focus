focus
=====

Bring Twitch chat directly into your Erlang development shell for maximum focus.

Developed live on [![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)</br>

TODO
---

- [x] Handle `session_keepalive` in `twitch:message_action/2` https://dev.twitch.tv/docs/eventsub/handling-websocket-events/#keepalive-message
- [x] Handle `channel.chat.message` in `twitch:message_action/2` https://dev.twitch.tv/docs/eventsub/eventsub-subscription-types/#channelchatmessage
- [x] In `handle_cast({notification, ...`, create a function to handle the event based on the type
- [ ] Subscribe to `channel.follow` in `twitch:subscribe/1` https://dev.twitch.tv/docs/eventsub/eventsub-subscription-types/#channelfollow
- [ ] Handle `channel.follow` in `twitch:message_action/2`
- [ ] Subscribe to `channel.subscribe` in `twitch:message_action/2` https://dev.twitch.tv/docs/eventsub/eventsub-subscription-types/#channelsubscribe
- [ ] Handle `channel.subscribe` in `twitch:message_action/2`
- [x] Need to add some logger formatting to twitch chat (https://github.com/julianduque/erlang-color?tab=readme-ov-file#true-24-bit-colors) 
  ```erlang
  io_lib:format("Hello, this is the ~s color~n", [ color:red("red") ])  
  ```
- [ ] Separate logging levels into Twitch and everything else
- [ ] Add a small cowboy server to handle user auth token https://ninenines.eu/docs/en/cowboy/2.9/guide/static_files/
  - [ ] Need an index.html with a single button to start the flow
  - [ ] Redirect user to handle OAuth
  - [ ] Handle request from Twitch
  - [ ] Store credentials
  - [ ] Redirect user to complete.html
- [ ] Mechanism to automatically refresh Twitch credentials

Environment variables required
----

```
export TWITCH_CLIENT_ID='...'
export TWITCH_SECRET='...'
export TWITCH_REDIRECT_URI='http://localhost:3000/oauth'
export TWITCH_USER_ACCESS_TOKEN='...'
export TWITCH_REFRESH_TOKEN='...'
export TWITCH_USER_ID='...'
```
