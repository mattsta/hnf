-module(hnf).

-export([start/0]).

% erlwg callback
-export([process_html_body/1]).

% zog exports for paths
-export([default_page/3, x/3, newest/3, jerbs/3, ask/3]).

% default regex filter
-define(DEFAULT, "(yc w|yc s|yc 2|gamif|yc-|20...yc|yc.20|crunch|onsh|"
                  "37s|twitch)").

%%%----------------------------------------------------------------------
%%% starting
%%%----------------------------------------------------------------------
start() ->
  application:start(hnf).

%%%----------------------------------------------------------------------
%%% zog_web functions for individual page requests
%%%----------------------------------------------------------------------
% default_page is what we use for / and anything else without a page function.
% Path [] represents "/" on default page or the function-name-as-path
% in any of the other functions.
default_page('GET', [], Cxn) -> page(Cxn, "/");
default_page('GET', _, Cxn) -> zog_page:temp_redirect(Cxn, "/").
newest('GET', [], Cxn) -> page(Cxn, "/newest").
jerbs('GET', [], Cxn) -> page(Cxn, "/jobs").
ask('GET', [], Cxn) -> page(Cxn, "/ask").

x('GET', [], Cxn) ->
  Queries = Cxn:parse_qs(),
  case Queries of
    [] -> zog_page:temp_redirect(Cxn, "/");
     _ -> case proplists:get_value("fnid", Queries, []) of
            [] -> zog_page:temp_redirect(Cxn, "/");
             F -> page(Cxn, "/x?fnid=" ++ F)
          end
  end.

%%%----------------------------------------------------------------------
%%% page helpers
%%%----------------------------------------------------------------------
page(Cxn, WhichPage) ->
  Queries = Cxn:parse_qs(),
  case Queries of
    [] -> To = WhichPage ++ "?" ++
               "remove=" ++ plus(?DEFAULT) ++ "&only-show-removed=no",
          zog_page:temp_redirect(Cxn, To);
     _ -> Parsed = parsed_newsyc(WhichPage),
          Filter = proplists:get_value("remove", Queries, ?DEFAULT),
          UseMatches = nice_invert("only-show-removed", Queries),
          Config = [{filter, Filter}, {invert, UseMatches}],
          Filtered = remove_yc(Parsed, Config),
          Body = mochiweb_html:to_html(Filtered),
          zog_page:ok(Cxn, Body)
  end.

%%%----------------------------------------------------------------------
%%% cache helpers
%%%----------------------------------------------------------------------
process_html_body(death) -> [];
process_html_body(Body) ->
  mochiweb_html:parse(unicode:characters_to_binary(Body, utf8)).

% Convert pluses to spaces for putting the regex in URLs
plus(What) when is_list(What) ->
  lists:reverse(lists:foldl(fun(32, A) -> ["+" | A];
                               (E, A) -> [E | A]
                            end, [], What)).

nice_invert(Key, Queries) ->
  case proplists:get_value(Key, Queries, no) of
     "yes" -> false;
    "true" -> false;
      "no" -> true;
   "false" -> true;
         _ -> true
   end.

parsed_newsyc(WhichPage) ->
  erlwg_server:get(hn, WhichPage, "http://news.ycombinator.com" ++ WhichPage).

%%%----------------------------------------------------------------------
%%% ehtml traversal
%%%----------------------------------------------------------------------
remove_yc({<<"html">>, Props, SubTags}, Config) ->
  {<<"html">>, Props, remove_yc(SubTags, Config)};
remove_yc([{<<"head">>, Props, SubTags} | T], Config) ->
  [{<<"head">>, Props,
    [{<<"base">>,
      [{<<"href">>, <<"http://news.ycombinator.com">>}], []} |
       remove_yc(SubTags, Config)]} |
        remove_yc(T, Config)];
remove_yc([{<<"tr">>, Props, SubTags} | T], Config) ->
  case remove_content(SubTags, Config) of
             % remove the comment and padding rows
     true -> Next = try tl(T) catch error:badarg -> T end,
%             NextT = try tl(Next) catch error:badarg -> Next end,
             remove_yc(Next, Config);
    false -> [{<<"tr">>, Props, remove_yc(SubTags, Config)} |
              remove_yc(T, Config)]
  end;
remove_yc([{<<"a">>, [{<<"href">>, <<"newest">>} | Other], _LinkText} | T],
    Config) ->
  Args = create_args(Config),
  [{<<"a">>,
   [{<<"href">>, ["http://diff.biz/newest?", Args]} | Other],
   <<"incoming">>} | remove_yc(T, Config)];
remove_yc([{<<"a">>, [{<<"href">>, <<"jobs">>} | Other], _LinkText} | T],
    Config) ->
  Args = create_args(Config),
  [{<<"a">>,
   [{<<"href">>, ["http://diff.biz/jerbs?", Args]} | Other],
   <<"jerbs">>} | remove_yc(T, Config)];
remove_yc([{<<"a">>, [{<<"href">>, <<"ask">>} | Other], _LinkText} | T],
    Config) ->
  Args = create_args(Config),
  [{<<"a">>,
   [{<<"href">>, ["http://diff.biz/ask?", Args]} | Other],
   <<"selfposts">>} | remove_yc(T, Config)];
remove_yc([{<<"a">>, [{<<"href">>, <<"news">>} | Other], _LinkText} | T],
    Config) ->
  Args = create_args(Config),
  [{<<"a">>,
   [{<<"href">>, ["http://diff.biz/?", Args]} | Other],
   <<"Hacked News">>} | remove_yc(T, Config)];
remove_yc([{<<"title">>, _, _} | T], Config) ->
  [{<<"title">>, [], <<"Hacked News">>} | remove_yc(T, Config)];
remove_yc([{<<"a">>, _, [<<"login">>]} | T], Config) ->
  remove_yc(T, Config);
remove_yc([{<<"a">>, [{<<"href">>, <<"/x?fnid=", F/binary>>} | Other],
    [<<"More">>]} | T], Config) ->
  Args = create_args(Config),
  [{<<"a">>,
   [{<<"href">>, ["http://diff.biz/x?fnid=", F, "&", Args]} | Other],
   <<"more distractions »">>} | remove_yc(T, Config)];
remove_yc([{Tag, Props, SubTags} | T], Config) ->
  [{Tag, Props, remove_yc(SubTags, Config)} | remove_yc(T, Config)];
remove_yc([], _) -> [];
remove_yc([H|T], Config) ->
  [H | remove_yc(T, Config)].

create_args(Config) ->
  {Remove, Filter} = props(Config),
  ShowFiltered = case Remove of true -> "no"; false -> "yes" end,
  "remove=" ++ plus(Filter) ++ "&only-show-removed=" ++ ShowFiltered.

props(Config) ->
  RemoveMatches = proplists:get_value(invert, Config, true),
  Filter = proplists:get_value(filter, Config, ""),
  {RemoveMatches, Filter}.

-spec remove_content(any(), [{atom(), any()}]) -> boolean().
remove_content([{<<"td">>, _, SubTags} | T], Config) ->
  remove_content(SubTags, Config) orelse remove_content(T, Config);
remove_content([{<<"a">>, Props, [Title]} | _], Config) when is_binary(Title) ->
  {RemoveMatch, Filter} = props(Config),
  case re:run(Title, Filter, [caseless]) of
    {match, _} -> RemoveMatch;
       nomatch -> case proplists:get_value(<<"href">>, Props) of
                    <<"/x?", _/binary>> -> false;  % always keep the More link
                    undefined -> not RemoveMatch;
                          URL -> case re:run(URL, Filter, [caseless]) of
                                   {match, _} -> RemoveMatch;
                                      nomatch -> not RemoveMatch
                                 end
                  end
  end;
remove_content([_H|T], Config) -> remove_content(T, Config);
remove_content([], _) -> false. % we found nothing matching. no removal.
