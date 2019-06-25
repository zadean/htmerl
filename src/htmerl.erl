-module(htmerl).

%% API exports
-export([simple/1,
         sax/1, sax/2]).

%%====================================================================
%% API functions
%%====================================================================

%% Takes a UTF-8 encoded binary containing an entire HTML document and returns
%% the parsed document as a tree of records found in htmerl.hrl. The root
%% record is htmlDocument. 
simple(Bin) ->
   htmerl_simple:string(Bin).


%% Takes a UTF-8 encoded binary containing an entire HTML document and returns
%% the parsed document as a list of SAX events. The SAX events are identical to 
%% those returned by xmerl except that the values and names are UTF-8 and
%% not codepoint lists.
sax(Bin) ->
   htmerl_sax_utf8:string(Bin).

%% Takes a UTF-8 encoded binary containing an entire HTML document and 
%% [{event_fun,  fun event/3},
%%  {user_state, state()}]
%% as Opts.
%% For each SAX event, the function in event_fun will be called as follows:
%% Fun(Event, LineNum, UserState)
%% Once all events are consumed {ok, UserState, Warnings} is returned.
%% The SAX events are identical to those used by xmerl_sax_parser except that 
%% the values and names are binaries and not codepoint lists.
%%
%% A usage example can be found in htmerl_simple. 
sax(Bin, Opts) ->
   htmerl_sax_utf8:string(Bin, Opts).

