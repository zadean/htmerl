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

or extracting values using the SAX events in a module:

```erlang
-module(htmerl_example).

-export([run/0]).

run() ->
    Html =
        <<"<html><body><p>Check</p>nothing here<p>this <b>bold garbage</b></p>g"
          "arbage<p>out!</p></body></html>">>,
    XPath = <<"html/body/p">>,
    Path =
        lists:reverse(
            binary:split(XPath, <<"/">>, [global])),
    Opts = [{event_fun, fun xpath/3}, {user_state, {[], Path, []}}],
    {ok, TextList, []} = htmerl:sax(Html, Opts),
    TextList.

xpath({characters, Text}, _LineNum, {Path, Path, Acc}) ->
    {Path, Path, [Text | Acc]};
xpath({endElement, _Ns, Ln, _}, _LineNum, {[Ln | Path], XPath, Acc}) ->
    {Path, XPath, Acc};
xpath({startElement, _Ns, Ln, _, _Atts}, _LineNum, {Path, XPath, Acc}) ->
    {[Ln | Path], XPath, Acc};
xpath(endDocument, _LineNum, {_Path, _XPath, Acc}) ->
    lists:reverse(Acc);
xpath(_Event, _LineNum, State) ->
    State.
```

```erlang
4> htmerl_example:run().
[<<"Check">>,<<"this">>,<<"out!">>]
```


Build
-----

    $ rebar3 compile
