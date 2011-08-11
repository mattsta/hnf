-module(hnf_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Erlwg = erlwg(),
  Trees = treestore(),
  Web = web_specs(hnf_mochiweb, 8081),

  Processes = [Erlwg, Trees, Web],
  Strategy = {one_for_one, 10, 10},

  {ok,
   {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Port) ->
  WebConfig = [{ip, {127, 0, 0, 1}}, {port, Port}],
  {Mod,
   {Mod, start, [WebConfig]},
   permanent, 5000, worker, dynamic}.

erlwg() ->
  {erlwg_sup,
   {erlwg_sup, start_link, []},
   permanent, 5000, supervisor, dynamic}.

treestore() ->
  {erlwg_hn_treestore,
   {erlwg_server, start_link, [hn, 60, fun hnf:process_html_body/1]},
   permanent, 5000, worker, dynamic}.
