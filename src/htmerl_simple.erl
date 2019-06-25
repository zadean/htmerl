-module(htmerl_simple).

%% ====================================================================
%% API functions
%% ====================================================================
-include("htmerl.hrl").

-export([string/1]).

string(Bin) ->
   Opts = [{event_fun, fun event/3},
           {user_state, state()}],
   {ok, Val, _} = htmerl_sax_utf8:string(Bin, Opts),
   Val.

state() ->
   #{stack => []}.

event({startElement, Uri, Local, _, AttsList}, _L, #{stack := S} = State) -> 
   E = #htmlElement{name = Local,
                    namespace = Uri,
                    attributes = attributes(AttsList)},
   State#{stack := [E|S]};
event({endElement, Uri, Local, _}, _L, 
      #{stack := [#htmlElement{name = Local,
                               namespace = Uri,
                               content = C} = E, P|Es]} = State) -> 
   E1 = E#htmlElement{content = lists:reverse(C)},
   P1 = add_child(E1, P),
   State#{stack := [P1|Es]};
event({comment, _} = Comment, _L, #{stack := [P|Ps]} = State) ->
   P1 = add_child(Comment, P),
   State#{stack := [P1|Ps]};
event({characters, _} = Text, _L, #{stack := [P|Ps]} = State) ->
   P1 = add_child(Text, P),
   State#{stack := [P1|Ps]};
event({startPrefixMapping, <<>>, _Uri}, _L, State) -> State;
event({endPrefixMapping, <<>>}, _L, State) -> State;
event(startDocument, _L, State) -> 
   State#{stack := [#htmlDocument{}]};
event(endDocument, _L, #{stack := [#htmlDocument{content = C} = D]}) -> 
   D#htmlDocument{content = lists:reverse(C)};
event({startDTD, Name, Public, System}, _L, 
      #{stack := [#htmlDocument{} = D]} = State) -> 
   D1 = D#htmlDocument{name = Name,
                       public = Public,
                       system = System},
   State#{stack := [D1]};
event(endDTD, _L, State) -> State.

%event(startCDATA, _L, State) -> State;
%event(endCDATA, _L, State) -> State;

add_child(#htmlElement{} = E, #htmlElement{content = Content} = P) ->
   P#htmlElement{content = [E|Content]};
add_child(#htmlElement{} = E, #htmlDocument{content = Content} = P) ->
   P#htmlDocument{content = [E|Content]};
add_child({characters, Text}, #htmlElement{content = Content} = P) ->
   P#htmlElement{content = [#htmlText{value = Text}|Content]};
add_child({comment, Comment}, #htmlElement{content = Content} = P) ->
   P#htmlElement{content = [#htmlComment{value = Comment}|Content]};
add_child({comment, Comment}, #htmlDocument{content = Content} = P) ->
   P#htmlDocument{content = [#htmlComment{value = Comment}|Content]}.

attributes(Atts) ->
  [#htmlAttribute{name = L,
                  prefix = P,
                  namespace = U,
                  value = V} || {U, P, L, V} <- Atts].
