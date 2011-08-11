-module(hnf_mochiweb).

-export([start/1, stop/0, loop/1]).
-compile(export_all).

%% External API

start(Options) ->
  mochiweb_http:start([{name, ?MODULE}, {loop, fun loop/1} | Options]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req) ->
  Path = Req:get(path),
  try
    hnf:front_page(Req, Path)
  catch
    throw:Response -> Response;
    Type:What ->
      Report = ["web request failed",
            {path, Path},
            {type, Type}, {what, What},
            {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      %% NOTE: mustache templates need \ because they are not awesome.
      Req:respond({500, [{"Content-Type", "text/plain"}],
             "you broke my tubes :(\n"})
  end.
