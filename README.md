htmerl
=====

An OTP library for parsing HTML documents.

This library attempts to follow the [HTML 5.2 specification](https://www.w3.org/TR/html52/) 
for tokenizing and parsing the HTML syntax as closely as possible.
This means that common errors that browsers accept are also accepted here and sanitized.

The output from `htmerl:sax/2` is identical to the XML SAX events produced
by `xmerl_sax_parser` except that here all values and names are UTF-8 binary
and not lists.  

Usage
-----
There are two ways to use `htmerl`. 
Firstly, to build a tree directly from the parsed input. Notice here that the missing "head" element was added.

```erlang
1> htmerl:simple(<<"<!DOCTYPE html><html><body>Hello</body></html>">>).
{htmlDocument,<<"html">>,<<>>,<<>>,
    [{htmlElement,<<"html">>,<<"http://www.w3.org/1999/xhtml">>,
         [],
         [{htmlElement,<<"head">>,<<"http://www.w3.org/1999/xhtml">>,
              [],[]},
          {htmlElement,<<"body">>,<<"http://www.w3.org/1999/xhtml">>,
              [],
              [{htmlText,<<"Hello">>,text}]}]}]}
```

Secondly, as a SAX parser. Calling `htmerl:sax/1` returns a list of SAX events.
`htmerl:sax/2` calls a user defined function.

```erlang
2> htmerl:sax(<<"<!DOCTYPE html><html><body>Hello</body></html>">>).
{ok,[startDocument,
     {startDTD,<<"html">>,<<>>,<<>>},
     endDTD,
     {startPrefixMapping,<<>>,<<"http://www.w3.org/1999/xhtml">>},
     {startElement,<<"http://www.w3.org/1999/xhtml">>,<<"html">>,
                   {<<>>,<<"html">>},
                   []},
     {startElement,<<"http://www.w3.org/1999/xhtml">>,<<"head">>,
                   {<<>>,<<"head">>},
                   []},
     {endElement,<<"http://www.w3.org/1999/xhtml">>,<<"head">>,
                 {<<>>,<<"head">>}},
     {startElement,<<"http://www.w3.org/1999/xhtml">>,<<"body">>,
                   {<<>>,<<"body">>},
                   []},
     {characters,<<"Hello">>},
     {endElement,<<"http://www.w3.org/1999/xhtml">>,<<"body">>,
                 {<<>>,<<"body">>}},
     {endElement,<<"http://www.w3.org/1999/xhtml">>,<<"html">>,
                 {<<>>,<<"html">>}},
     {endPrefixMapping,<<>>},
     endDocument],
    []}
```

 or with a user defined function and state
 
```erlang
3> F = fun(E, _, S) -> io:format("Event: ~p~n", [E]), S end,
Opts = [{event_fun, F}, {user_state, []}],
htmerl:sax(<<"<!DOCTYPE html><html><body>Hello</body></html>">>, Opts).
Event: startDocument
Event: {startDTD,<<"html">>,<<>>,<<>>}
Event: endDTD
Event: {startPrefixMapping,<<>>,<<"http://www.w3.org/1999/xhtml">>}
Event: {startElement,<<"http://www.w3.org/1999/xhtml">>,<<"html">>,
                     {<<>>,<<"html">>},
                     []}
Event: {startElement,<<"http://www.w3.org/1999/xhtml">>,<<"head">>,
                     {<<>>,<<"head">>},
                     []}
Event: {endElement,<<"http://www.w3.org/1999/xhtml">>,<<"head">>,
                   {<<>>,<<"head">>}}
Event: {startElement,<<"http://www.w3.org/1999/xhtml">>,<<"body">>,
                     {<<>>,<<"body">>},
                     []}
Event: {characters,<<"Hello">>}
Event: {endElement,<<"http://www.w3.org/1999/xhtml">>,<<"body">>,
                   {<<>>,<<"body">>}}
Event: {endElement,<<"http://www.w3.org/1999/xhtml">>,<<"html">>,
                   {<<>>,<<"html">>}}
Event: {endPrefixMapping,<<>>}
Event: endDocument
{ok,[],[]}
```

Build
-----

    $ rebar3 compile
