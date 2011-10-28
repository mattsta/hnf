hnf: hacker news filter
=======================

wtfbbq is hnf?
--------------
During the most recent YC "launch season," I made a HN proxy solve two
problems: filtering content based on URL/title *and* caching HN pages
so we don't have to deal with 30s+ page load times.  When I want my pop
tech news tabloid drivel, I want it *now*.

You can use the URL params to customize your regex filter and follow the
results across all the more/next links.  It's super-minimal and effective
at lowering my story-induced annoyance level throughout the day.

Tech tip: Sometimes you have to refresh the page to get a new version.  If
the cached version is too old (default cache time is 5min), the old page
is immediately returned while a new page fetches in the background to maintain
responsiveness at the expense of freshness.

Usage
-----
        rebar get-deps
        rebar compile
        ./start-dev.sh

A `hnf` instance is usually running live at http://diff.biz/

Things to Change
----------------

* You can change the webserver's IP/Port in hnf.config.

Building
--------
Download deps:

        rebar get-deps

Build:

        rebar compile


Testing
-------
We really should have tests, shouldn't we?
