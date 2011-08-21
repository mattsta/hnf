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

  Processes = [Erlwg, Trees],
  Strategy = {one_for_one, 10, 10},

  {ok,
   {Strategy, lists:flatten(Processes)}}.

erlwg() ->
  {erlwg_sup,
   {erlwg_sup, start_link, []},
   permanent, 5000, supervisor, [erlwg_sup]}.

treestore() ->
  {erlwg_hn_treestore,
   {erlwg_server, start_link, [hn, 60, fun hnf:process_html_body/1]},
   permanent, 5000, worker, [erlwg_hn_treestore]}.
