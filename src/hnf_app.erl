-module(hnf_app).

-behaviour(application).
-export([start/2,stop/1]).

-spec start(any(), any()) -> any().
start(_Type, _StartArgs) ->
  (catch zog_web_error_handler:bob()), % force loading of error handler
  inets:start(),
  ssl:start(),
  HnfSup = hnf_sup:start_link(),
  % Only start serving web traffic after hnf is up and running:
  zog_web:start(),
  HnfSup.

-spec stop(any()) -> any().
stop(_State) ->
  ok.
