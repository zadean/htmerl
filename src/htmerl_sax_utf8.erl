-module(htmerl_sax_utf8).

%% ====================================================================
%% API functions
%% ====================================================================
-export([string/1, string/2]).

-compile({inline,[emit/2, u/1]}).

%% Token types
-record(doctype, 
        {name   = <<>>, 
         public = undefined,
         system = undefined,
         quirks = false
        }).

-record(start_tag, 
        {name       = <<>>,
         closing    = false,
         attributes = []
        }).

-record(attribute, 
        {name  = <<>>,
         value = <<>>
        }).

-record(end_tag, { name = <<>> }).

-record(comment, { data = <<>> }).

-record(char, { data = <<>> }).
-record(chars, { data = <<>> }).

-define(HTML,  <<"http://www.w3.org/1999/xhtml">>).
-define(MATH,  <<"http://www.w3.org/1998/Math/MathML">>).
-define(SVG,   <<"http://www.w3.org/2000/svg">>).
-define(XLINK, <<"http://www.w3.org/1999/xlink">>).
-define(XML,   <<"http://www.w3.org/XML/1998/namespace">>).
-define(XMLNS, <<"http://www.w3.org/2000/xmlns/">>).

-define(upper_ascii_letter(L),  (L >= $A andalso L =< $Z) ).
-define(lower_ascii_letter(L),  (L >= $a andalso L =< $z) ).
-define(ascii_letter(L), ?lower_ascii_letter(L) orelse ?upper_ascii_letter(L) ).
-define(ascii_digit(L), (L >= $0 andalso L =< $9) ).
-define(ascii_hex_digit(L), ( ?ascii_digit(L) orelse 
                                (L >= $A andalso L =< $F) orelse 
                                (L >= $a andalso L =< $f) ) ).
-define(ws(S), (S =:= $  orelse S =:= $\t orelse S =:= $\n orelse S =:= $\f) ).
-define(i(S), incr_line(S)).

string(Bin) ->
   string(Bin, []).

string(Bin, Options) ->
   M1 = case proplists:get_value(event_fun, Options) of
           F when is_function(F, 3) ->
              #{user_event_fun => F};
           _ ->
              #{}
        end,
   M2 = case proplists:get_value(user_state, Options) of
           undefined ->
              M1;
           V ->
              M1#{user_event_state => V}
        end,
   M3 = maps:merge(default_state(), M2),
   Bin1 = norm_newlines(Bin, <<>>),
   data(Bin1, M3).

norm_whitespaces(Bin) ->
   Splitted = binary:split(Bin, [<<"\n">>, <<" ">>, <<"\t">>], [global, trim_all]),
   IOList = combine(Splitted, <<" ">>),
   iolist_to_binary(IOList).

combine([], _) -> "" ;
combine([Last], _) -> [Last];
combine([Head|Tail], Space) -> 
   [Head, Space, combine(Tail, Space)].

norm_newlines(Bin, _) ->
   binary:replace(Bin, [<<$\r,$\n>>, <<$\r>>], <<$\n>>, [global]).


%% ====================================================================
%% 8.2.3 Parse State
%% ====================================================================

default_state() ->
   #{%% parser
      %% initial
      %%  | before_html | before_head | in_head | in_head_noscript
      %%  | after_head | in_body | text | in_table | in_table_text
      %%  | in_caption | in_column_group | in_table_body | in_row
      %%  | in_cell | in_select | in_select_in_table | in_template
      %%  | after_body | in_frameset | after_frameset
      %%  | after_after_body | after_after_frameset
      insertion_mode => initial,
      orig_insertion_mode => undefined,
      template_ins_modes => [],
      open_elements   => [],
      user_event_fun => fun(endDocument = A, _, C) ->
                              lists:reverse([A|C]);
                           (A, _B, C) -> 
                              [A|C]
                        end,
      user_event_state => [],
      inscope_namespace => [html], % html | svg | mathml
      text_node_buff => undefined,
     %% tokenizer
      current_token  => undefined,
      return_fun     => undefined,
      temp_buffer    => [],
      last_start_tag => undefined,
      line_num       => 1,
      char_ref_code  => 0,
      data_stop      => binary:compile_pattern([<<$&>>, <<$<>>]),
      att_dq_stop    => binary:compile_pattern([<<$\">>, <<$&>>, <<0>>]),
      att_sq_stop    => binary:compile_pattern([<<$'>>, <<$&>>, <<0>>]),
      rawtext_stop   => binary:compile_pattern([<<$<>>, <<0>>]),
      nl_cp          => binary:compile_pattern([<<$\n>>])
    }.

%% ====================================================================
%% 8.2.4 Tokenization
%% ====================================================================

%% 8.2.4.1
data(<<$&, Rest/binary>>, State) ->
   character_reference(Rest, State#{return_fun := ?FUNCTION_NAME});
data(<<$<, Rest/binary>>, State) ->
   tag_open(Rest, State);
data(<<>>, State) ->
   emit(eof, State);
data(Stream, #{line_num := LineNum,
               nl_cp := NlCp,
               data_stop := Stop} = State) ->
   {Len, LineNum1} = find_stop(Stream, Stop, LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = emit(#chars{data = Chars}, State),
   data(Rest, State1#{line_num := LineNum1}).

%% 8.2.4.2
rcdata(<<$&, Rest/binary>>, State) ->
   character_reference(Rest, State#{return_fun := ?FUNCTION_NAME});
rcdata(<<$<, Rest/binary>>, State) ->
   rcdata_less_than_sign(Rest, State);
rcdata(<<0, Rest/binary>>, State) ->
   % parse error
   State1 = emit(#char{data = 16#FFFD}, State),
   rcdata(Rest, State1);
rcdata(<<>>, State) ->
   emit(eof, State);
rcdata(Stream, #{line_num := LineNum,
                 nl_cp := NlCp} = State) ->
   {Len, LineNum1} = find_stop(Stream, [<<$&>>, <<$<>>, <<0>>], LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = emit(#chars{data = Chars}, State),
   rcdata(Rest, State1#{line_num := LineNum1}).

%% 8.2.4.3
rawtext(<<$<, Rest/binary>>, State) ->
   rawtext_less_than_sign(Rest, State);
rawtext(<<0, Rest/binary>>, State) ->
   % parse error
   State1 = emit(#char{data = 16#FFFD}, State),
   rawtext(Rest, State1);
rawtext(<<>>, State) ->
   emit(eof, State);
rawtext(Stream, #{line_num := LineNum,
                  rawtext_stop := Stop,
                  nl_cp := NlCp} = State) ->
   {Len, LineNum1} = find_stop(Stream, Stop, LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = emit(#chars{data = Chars}, State),
   rawtext(Rest, State1#{line_num := LineNum1}).
   
%% 8.2.4.4
script_data(<<$<, Rest/binary>>, State) ->
   script_data_less_than_sign(Rest, State);
script_data(<<0, Rest/binary>>, State) ->
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data(Rest, State1);
script_data(<<>>, State) ->
   emit(eof, State);
script_data(Stream, #{line_num := LineNum,
                      rawtext_stop := Stop,
                      nl_cp := NlCp} = State) ->
   {Len, LineNum1} = find_stop(Stream, Stop, LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = emit(#chars{data = Chars}, State),
   script_data(Rest, State1#{line_num := LineNum1}).

%% 8.2.4.5
plaintext(<<0, Rest/binary>>, State) ->
   % parse error
   State1 = emit(#char{data = 16#FFFD}, State),
   plaintext(Rest, State1);
plaintext(<<>>, State) ->
   emit(eof, State);
plaintext(Stream, #{line_num := LineNum,
                    nl_cp := NlCp} = State) ->
   {Len, LineNum1} = find_stop(Stream, [<<0>>], LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = emit(#chars{data = Chars}, State),
   plaintext(Rest, State1#{line_num := LineNum1}).
   
%% 8.2.4.6
tag_open(<<$!, Rest/binary>>, State) ->
   markup_declaration_open(Rest, State);
tag_open(<<$/, Rest/binary>>, State) ->
   end_tag_open(Rest, State);
tag_open(<<C, _/binary>> = Stream, State) when ?ascii_letter(C) ->
   Token = #start_tag{},
   tag_name(Stream, State#{current_token := Token});
tag_open(<<$?, _/binary>> = Stream, State) ->
   % parse error
   Token = #comment{},
   bogus_comment(Stream, State#{current_token := Token});
tag_open(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   data(Stream, State1).

%% 8.2.4.7
end_tag_open(<<C, _/binary>> = Stream, State) when ?ascii_letter(C) ->
   Token = #end_tag{},
   tag_name(Stream, State#{current_token := Token});
end_tag_open(<<$>, Rest/binary>>, State) ->
   % parse error
   data(Rest, State);
end_tag_open(<<>>, State) ->
   % parse error
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   emit(eof, State2);
end_tag_open(Stream, State) -> 
   % parse error
   Token = #comment{},
   bogus_comment(Stream, State#{current_token := Token}).

%% 8.2.4.8
tag_name(<<$\n, Rest/binary>>, State) ->
   before_attribute_name(Rest, ?i(State));
tag_name(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_attribute_name(Rest, State);
tag_name(<<$/, Rest/binary>>, State) ->
   self_closing_start_tag(Rest, State);
tag_name(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   call_data_function(Curr, Rest, State1);
tag_name(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_name(16#FFFD, Curr)},
   tag_name(Rest, State1);
tag_name(<<>>, State) ->
   emit(eof, State);
tag_name(Stream, #{current_token := Curr} = State) ->
   {Part, Rest} = tag_name_1(Stream, 0),
   State1 = State#{current_token := append_to_name(Part, Curr)},
   tag_name(Rest, State1).


tag_name_1(Stream, Pos) ->
   case Stream of
      <<_:Pos/binary, C1, C2, C3, _/binary>> 
         when ?lower_ascii_letter(C1),
              ?lower_ascii_letter(C2),
              ?lower_ascii_letter(C3) ->
         tag_name_1(Stream, Pos + 3);
      <<_:Pos/binary, C1, C2, _/binary>> 
         when ?lower_ascii_letter(C1),
              ?lower_ascii_letter(C2) ->
         tag_name_1(Stream, Pos + 2);
      <<_:Pos/binary, C, _/binary>> when ?lower_ascii_letter(C) ->
         tag_name_1(Stream, Pos + 1);
      <<_:Pos/binary, C, _/binary>> when ?upper_ascii_letter(C) ->
         tag_name_2(Stream, Pos + 1);
      <<_:Pos/binary, C, _/binary>> when ?ws(C);
                                         C == $/;
                                         C == $>;
                                         C == 0 ->
         <<Part:Pos/binary, Rest/binary>> = Stream,
         {Part, Rest};
      <<_:Pos/binary, _/binary>> ->
         tag_name_1(Stream, Pos + 1);
      <<Part:Pos/binary>> ->
         {Part, <<>>}
   end.

tag_name_2(Stream, Pos) ->
   case Stream of
      <<_:Pos/binary, C, _/binary>> when ?ws(C);
                                         C == $/;
                                         C == $>;
                                         C == 0 ->
         <<Part:Pos/binary, Rest/binary>> = Stream,
         {string:lowercase(Part), Rest};
      <<_:Pos/binary, _/binary>> ->
         tag_name_1(Stream, Pos + 1);
      <<Part:Pos/binary>> ->
         {string:lowercase(Part), <<>>}
   end.


%% 8.2.4.9
rcdata_less_than_sign(<<$/, Rest/binary>>, State) ->
   rcdata_end_tag_open(Rest, State#{temp_buffer := []});
rcdata_less_than_sign(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   rcdata(Stream, State1).

%% 8.2.4.10      
rcdata_end_tag_open(<<C, _/binary>> = Stream, State) when ?ascii_letter(C) ->
   Tok = #end_tag{},
   State1 = State#{current_token := Tok},
   rcdata_end_tag_name(Stream, State1);
rcdata_end_tag_open(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   rcdata(Stream, State2).

%% 8.2.4.11      
rcdata_end_tag_name(<<$\n, Rest/binary>>, 
                    #{current_token := #end_tag{name = Name},
                      last_start_tag := #start_tag{name = Name}} = State) ->
   before_attribute_name(Rest, ?i(State));
rcdata_end_tag_name(<<C, Rest/binary>>, 
                    #{current_token := #end_tag{name = Name},
                      last_start_tag := #start_tag{name = Name}} = State) when ?ws(C) ->
   before_attribute_name(Rest, State);
rcdata_end_tag_name(<<$/, Rest/binary>>, 
                    #{current_token := #end_tag{name = Name},
                      last_start_tag := #start_tag{name = Name}} = State) ->
   self_closing_start_tag(Rest, State);
rcdata_end_tag_name(<<$>, Rest/binary>>, 
                    #{current_token := #end_tag{name = Name} = Curr,
                      last_start_tag := #start_tag{name = Name}} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
rcdata_end_tag_name(<<C, Rest/binary>>, 
                    #{current_token := Curr,
                      temp_buffer := Buff} = State) when ?upper_ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C + 32, Curr)},
   rcdata_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
rcdata_end_tag_name(<<C, Rest/binary>>, 
                    #{current_token := Curr,
                      temp_buffer := Buff} = State) when ?ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C, Curr)},
   rcdata_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
rcdata_end_tag_name(Stream, #{temp_buffer := Buff} = State) -> 
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   F = fun(Ch, S) ->
             emit(#char{data = Ch}, S)
       end,
   State3 = lists:foldr(F, State2, Buff),
   rcdata(Stream, State3).

%% 8.2.4.12      
rawtext_less_than_sign(<<$/, Rest/binary>>, State) ->
   rawtext_end_tag_open(Rest, State#{temp_buffer := []});
rawtext_less_than_sign(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   rawtext(Stream, State1).

%% 8.2.4.13      
rawtext_end_tag_open(<<C, _/binary>> = Stream, State) when ?ascii_letter(C) ->
   Tok = #end_tag{},
   State1 = State#{current_token := Tok},
   rawtext_end_tag_name(Stream, State1);
rawtext_end_tag_open(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   rawtext(Stream, State2).

%% 8.2.4.14      
rawtext_end_tag_name(<<$\n, Rest/binary>>, 
                     #{current_token := #end_tag{name = Name},
                       last_start_tag := #start_tag{name = Name}} = State) ->
   before_attribute_name(Rest, ?i(State));
rawtext_end_tag_name(<<C, Rest/binary>>, 
                     #{current_token := #end_tag{name = Name},
                       last_start_tag := #start_tag{name = Name}} = State) when ?ws(C) ->
   before_attribute_name(Rest, State);
rawtext_end_tag_name(<<$/, Rest/binary>>, 
                     #{current_token := #end_tag{name = Name},
                       last_start_tag := #start_tag{name = Name}} = State) ->
   self_closing_start_tag(Rest, State);
rawtext_end_tag_name(<<$>, Rest/binary>>, 
                     #{current_token := #end_tag{name = Name} = Curr,
                       last_start_tag := #start_tag{name = Name}} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
rawtext_end_tag_name(<<C, Rest/binary>>, 
                     #{current_token := Curr,
                       temp_buffer := Buff} = State) when ?upper_ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C + 32, Curr)},
   rawtext_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
rawtext_end_tag_name(<<C, Rest/binary>>, 
                     #{current_token := Curr,
                       temp_buffer := Buff} = State) when ?ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C, Curr)},
   rawtext_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
rawtext_end_tag_name(Stream, #{temp_buffer := Buff} = State) -> 
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   F = fun(Cf, S) ->
             emit(#char{data = Cf}, S)
       end,
   State3 = lists:foldr(F, State2, Buff),
   rawtext(Stream, State3).

%% 8.2.4.15      
script_data_less_than_sign(<<$/, Rest/binary>>, State0) ->
   script_data_end_tag_open(Rest, State0#{temp_buffer := []});
script_data_less_than_sign(<<$!, Rest/binary>>, State0) ->
   State1 = emit(#char{data = $<}, State0),
   State2 = emit(#char{data = $!}, State1),
   script_data_escape_start(Rest, State2);
script_data_less_than_sign(Stream, State0) -> 
   State1 = emit(#char{data = $<}, State0),
   script_data(Stream, State1).

%% 8.2.4.16      
script_data_end_tag_open(<<C, _/binary>> = Stream, State0) when ?ascii_letter(C) ->
   script_data_end_tag_name(Stream, State0#{current_token := #end_tag{}});
script_data_end_tag_open(Stream, State0) -> 
   State1 = emit(#char{data = $<}, State0),
   State2 = emit(#char{data = $/}, State1),
   script_data(Stream, State2).

%% 8.2.4.17      
script_data_end_tag_name(<<$\n, Rest/binary>>, 
                         #{current_token := #end_tag{name = Name},
                           last_start_tag := #start_tag{name = Name}} = State) ->
   before_attribute_name(Rest, ?i(State));
script_data_end_tag_name(<<C, Rest/binary>>, 
                         #{current_token := #end_tag{name = Name},
                           last_start_tag := #start_tag{name = Name}} = State) when ?ws(C) ->
   before_attribute_name(Rest, State);
script_data_end_tag_name(<<$/, Rest/binary>>, 
                         #{current_token := #end_tag{name = Name},
                           last_start_tag := #start_tag{name = Name}} = State) ->
   self_closing_start_tag(Rest, State);
script_data_end_tag_name(<<$>, Rest/binary>>, 
                         #{current_token := #end_tag{name = Name} = Curr,
                           last_start_tag := #start_tag{name = Name}} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
script_data_end_tag_name(<<C, Rest/binary>>, 
                         #{current_token := Curr,
                           temp_buffer := Buff} = State) when ?upper_ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C + 32, Curr)},
   script_data_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
script_data_end_tag_name(<<C, Rest/binary>>, 
                         #{current_token := Curr,
                           temp_buffer := Buff} = State) when ?ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C, Curr)},
   script_data_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
script_data_end_tag_name(Stream, #{temp_buffer := Buff} = State) -> 
   Buff1 = [$<|[$/|lists:reverse(Buff)]],
   Chars = unicode:characters_to_binary(Buff1),
   State1 = emit(#chars{data = Chars}, State),
   script_data(Stream, State1#{temp_buffer := []}).

%% 8.2.4.18      
script_data_escape_start(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_escape_start_dash(Rest, State1);
script_data_escape_start(Stream, State) -> 
   script_data(Stream, State).

%% 8.2.4.19      
script_data_escape_start_dash(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_escaped_dash_dash(Rest, State1);
script_data_escape_start_dash(Stream, State) ->
   script_data(Stream, State).

%% 8.2.4.20      
script_data_escaped(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_escaped_dash(Rest, State1);
script_data_escaped(<<$<, Rest/binary>>, State) ->
   script_data_escaped_less_than_sign(Rest, State);
script_data_escaped(<<0, Rest/binary>>, State) ->
   % parse error
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data_escaped(Rest, State1);
script_data_escaped(<<>>, State) ->
   emit(eof, State);
script_data_escaped(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_escaped(Rest, State1);
script_data_escaped(<<C/utf8, Rest/binary>>, State) ->
   State1 = emit(#char{data = C}, State),
   script_data_escaped(Rest, State1).

%% 8.2.4.21      
script_data_escaped_dash(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_escaped_dash_dash(Rest, State1);
script_data_escaped_dash(<<$<, Rest/binary>>, State) ->
   script_data_escaped_less_than_sign(Rest, State);
script_data_escaped_dash(<<0, Rest/binary>>, State) ->
   % parse error
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data_escaped(Rest, State1);
script_data_escaped_dash(<<>>, State) ->
   emit(eof, State);
script_data_escaped_dash(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_escaped(Rest, State1);
script_data_escaped_dash(<<C/utf8, Rest/binary>>, State) ->
   State1 = emit(#char{data = C}, State),
   script_data_escaped(Rest, State1).

%% 8.2.4.22      
script_data_escaped_dash_dash(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_escaped_dash_dash(Rest, State1);
script_data_escaped_dash_dash(<<$<, Rest/binary>>, State) ->
   script_data_escaped_less_than_sign(Rest, State);
script_data_escaped_dash_dash(<<$>, Rest/binary>>, State) ->
   State1 = emit(#char{data = $>}, State),
   script_data(Rest, State1);
script_data_escaped_dash_dash(<<0, Rest/binary>>, State) ->
   % parse error
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data_escaped(Rest, State1);
script_data_escaped_dash_dash(<<>>, State) ->
   emit(eof, State);
script_data_escaped_dash_dash(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_escaped(Rest, State1);
script_data_escaped_dash_dash(<<C/utf8, Rest/binary>>, State) ->
   State1 = emit(#char{data = C}, State),
   script_data_escaped(Rest, State1).

%% 8.2.4.23      
script_data_escaped_less_than_sign(<<$/, Rest/binary>>, State) ->
   script_data_escaped_end_tag_open(Rest, State#{temp_buffer := []});
script_data_escaped_less_than_sign(<<C, _/binary>> = Stream, State) when ?ascii_letter(C) ->
   State1 = emit(#char{data = $<}, State),
   script_data_double_escape_start(Stream, State1);
script_data_escaped_less_than_sign(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   script_data_escaped(Stream, State1).

%% 8.2.4.24      
script_data_escaped_end_tag_open(<<C, _/binary>> = Stream, State) when ?ascii_letter(C) ->
   Tok = #end_tag{},
   script_data_escaped_end_tag_name(Stream, State#{current_token := Tok});
script_data_escaped_end_tag_open(Stream, State) -> 
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   script_data_escaped(Stream, State2).

%% 8.2.4.25
script_data_escaped_end_tag_name(<<$\n, Rest/binary>>, 
                                 #{current_token := #end_tag{name = Name},
                                   last_start_tag := #start_tag{name = Name}} = State) ->
   before_attribute_name(Rest, ?i(State));
script_data_escaped_end_tag_name(<<C, Rest/binary>>, 
                                 #{current_token := #end_tag{name = Name},
                                   last_start_tag := #start_tag{name = Name}} = State) when ?ws(C) ->
   before_attribute_name(Rest, State);
script_data_escaped_end_tag_name(<<$/, Rest/binary>>, 
                                 #{current_token := #end_tag{name = Name},
                                   last_start_tag := #start_tag{name = Name}} = State) ->
   self_closing_start_tag(Rest, State);
script_data_escaped_end_tag_name(<<$>, Rest/binary>>, 
                                 #{current_token := #end_tag{name = Name} = Curr,
                                   last_start_tag := #start_tag{name = Name}} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
script_data_escaped_end_tag_name(<<C, Rest/binary>>, 
                                 #{current_token := Curr,
                                   temp_buffer := Buff} = State) when ?upper_ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C + 32, Curr)},
   script_data_escaped_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
script_data_escaped_end_tag_name(<<C, Rest/binary>>, 
                                 #{current_token := Curr,
                                   temp_buffer := Buff} = State) when ?ascii_letter(C) ->
   State1 = State#{current_token := append_to_name(C, Curr)},
   script_data_escaped_end_tag_name(Rest, State1#{temp_buffer := [C|Buff]});
script_data_escaped_end_tag_name(Stream, #{temp_buffer := Buff} = State) ->
   State1 = emit(#char{data = $<}, State),
   State2 = emit(#char{data = $/}, State1),
   F = fun(Ch, S) ->
             emit(#char{data = Ch}, S)
       end,
   State3 = lists:foldr(F, State2, Buff),
   script_data_escaped(Stream, State3).

%% 8.2.4.26      
script_data_double_escape_start(<<$\n, Rest/binary>>, #{temp_buffer := "tpircs"} = State) ->
   script_data_double_escaped(Rest, ?i(State));
script_data_double_escape_start(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_escaped(Rest, State1);
script_data_double_escape_start(<<C, Rest/binary>>, #{temp_buffer := "tpircs"} = State) 
   when ?ws(C);
        C =:= $/;
        C =:= $> ->
   script_data_double_escaped(Rest, State);
script_data_double_escape_start(<<C, Rest/binary>>, State) 
   when ?ws(C);
        C =:= $/;
        C =:= $> ->
   State1 = emit(#char{data = C}, State),
   script_data_escaped(Rest, State1);
script_data_double_escape_start(<<C, Rest/binary>>, #{temp_buffer := Buff} = State) 
   when ?upper_ascii_letter(C) ->
   State1 = emit(#char{data = C}, State),
   State2 = State1#{temp_buffer := [C + 32|Buff]},
   script_data_double_escape_start(Rest, State2);
script_data_double_escape_start(<<C, Rest/binary>>, #{temp_buffer := Buff} = State) 
   when ?ascii_letter(C) ->
   State1 = emit(#char{data = C}, State),
   State2 = State1#{temp_buffer := [C|Buff]},
   script_data_double_escape_start(Rest, State2);
script_data_double_escape_start(Stream, State) ->
   script_data_escaped(Stream, State).

%% 8.2.4.27      
script_data_double_escaped(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_double_escaped_dash(Rest, State1);
script_data_double_escaped(<<$<, Rest/binary>>, State) ->
   State1 = emit(#char{data = $<}, State),
   script_data_double_escaped_less_than_sign(Rest, State1);
script_data_double_escaped(<<0, Rest/binary>>, State) ->
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data_double_escaped(Rest, State1);
script_data_double_escaped(<<>>, State) ->
   emit(eof, State);
script_data_double_escaped(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_double_escaped(Rest, State1);
script_data_double_escaped(<<C/utf8, Rest/binary>>, State) ->
   % XXX look ahead here?
   State1 = emit(#char{data = C}, State),
   script_data_double_escaped(Rest, State1).

%% 8.2.4.28      
script_data_double_escaped_dash(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_double_escaped_dash_dash(Rest, State1);
script_data_double_escaped_dash(<<$<, Rest/binary>>, State) ->
   State1 = emit(#char{data = $<}, State),
   script_data_double_escaped_less_than_sign(Rest, State1);
script_data_double_escaped_dash(<<0, Rest/binary>>, State) ->
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data_double_escaped(Rest, State1);
script_data_double_escaped_dash(<<>>, State) ->
   emit(eof, State);
script_data_double_escaped_dash(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_double_escaped(Rest, State1);
script_data_double_escaped_dash(<<C/utf8, Rest/binary>>, State) ->
   State1 = emit(#char{data = C}, State),
   script_data_double_escaped(Rest, State1).

%% 8.2.4.29      
script_data_double_escaped_dash_dash(<<$-, Rest/binary>>, State) ->
   State1 = emit(#char{data = $-}, State),
   script_data_double_escaped_dash_dash(Rest, State1);
script_data_double_escaped_dash_dash(<<$<, Rest/binary>>, State) ->
   State1 = emit(#char{data = $<}, State),
   script_data_double_escaped_less_than_sign(Rest, State1);
script_data_double_escaped_dash_dash(<<$>, Rest/binary>>, State) ->
   State1 = emit(#char{data = $>}, State),
   script_data(Rest, State1);
script_data_double_escaped_dash_dash(<<0, Rest/binary>>, State) ->
   State1 = emit(#char{data = 16#FFFD}, State),
   script_data_double_escaped(Rest, State1);
script_data_double_escaped_dash_dash(<<>>, State) ->
   emit(eof, State);
script_data_double_escaped_dash_dash(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_double_escaped(Rest, State1);
script_data_double_escaped_dash_dash(<<C/utf8, Rest/binary>>, State) ->
   State1 = emit(#char{data = C}, State),
   script_data_double_escaped(Rest, State1).

%% 8.2.4.30      
script_data_double_escaped_less_than_sign(<<$/, Rest/binary>>, State) ->
   State1 = emit(#char{data = $/}, State),
   script_data_double_escape_end(Rest, State1#{temp_buffer := []});
script_data_double_escaped_less_than_sign(Stream, State) -> 
   script_data_double_escaped(Stream, State).

%% 8.2.4.31      
script_data_double_escape_end(<<$\n, Rest/binary>>, #{temp_buffer := "tpircs"} = State) ->
   script_data_escaped(Rest, ?i(State));
script_data_double_escape_end(<<$\n, Rest/binary>>, State) ->
   State1 = emit(#char{data = $\n}, ?i(State)),
   script_data_double_escaped(Rest, State1);
script_data_double_escape_end(<<C, Rest/binary>>, #{temp_buffer := "tpircs"} = State) 
   when ?ws(C);
        C =:= $/;
        C =:= $> ->
   script_data_escaped(Rest, State);
script_data_double_escape_end(<<C, Rest/binary>>, State) 
   when ?ws(C);
        C =:= $/;
        C =:= $> ->
   State1 = emit(#char{data = C}, State),
   script_data_double_escaped(Rest, State1);
script_data_double_escape_end(<<C, Rest/binary>>, #{temp_buffer := Buff} = State) 
   when ?upper_ascii_letter(C) -> 
   State1 = emit(#char{data = C}, State),
   State2 = State1#{temp_buffer := [C + 32|Buff]},
   script_data_double_escape_end(Rest, State2);
script_data_double_escape_end(<<C, Rest/binary>>, #{temp_buffer := Buff} = State) 
   when ?ascii_letter(C) -> 
   State1 = emit(#char{data = C}, State),
   State2 = State1#{temp_buffer := [C|Buff]},
   script_data_double_escape_end(Rest, State2);
script_data_double_escape_end(Stream, State) -> 
   script_data_double_escaped(Stream, State).

%% 8.2.4.32      
before_attribute_name(<<$\n, Rest/binary>>, State) ->
   before_attribute_name(Rest, ?i(State));
before_attribute_name(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_attribute_name(Rest, State);
before_attribute_name(<<>>, State0) ->
   after_attribute_name(<<>>, State0);
before_attribute_name(<<C, _/binary>> = Stream, State0) when C == $/;
                                                             C == $> ->
   after_attribute_name(Stream, State0);
before_attribute_name(<<$=, Rest/binary>>, #{current_token := Curr} = State0) ->
   % parse error
   Atts = Curr#start_tag.attributes,
   Tok = #attribute{name = <<"INVALID">>},
   attribute_name(Rest, State0#{current_token := Curr#start_tag{attributes = [Tok|Atts]}});
before_attribute_name(Stream, #{current_token := Curr} = State0) ->
   Atts = Curr#start_tag.attributes,
   Tok = #attribute{},
   attribute_name(Stream, State0#{current_token := Curr#start_tag{attributes = [Tok|Atts]}}).

%% 8.2.4.33      
attribute_name(<<>>, State) ->
   after_attribute_name(<<>>, State);
attribute_name(<<C, _/binary>> = Stream, State) when ?ws(C);
                                                     C == $/;
                                                     C == $> ->
   after_attribute_name(Stream, State);
attribute_name(<<$=, Rest/binary>>, State) ->
   before_attribute_value(Rest, State);

attribute_name(Stream, 
               #{current_token :=
                   #start_tag{attributes = [At|Ats]} = Curr} = State) ->
   {Part, Rest} = attribute_name_1(Stream, <<>>),
   Atts = [append_to_name(Part, At)|Ats],
   State1 = State#{current_token := Curr#start_tag{attributes = Atts}},
   attribute_name(Rest, State1).
   %%    when C =:= $\";
   %%         C =:= $';
   %%         C =:= $< ->
   %%    % parse error

attribute_name_1(<<C, Rest/binary>>, Acc) when ?upper_ascii_letter(C) ->
   attribute_name_1(Rest, <<Acc/binary, (C + 32)>>);
attribute_name_1(<<C, Rest/binary>>, Acc) when ?ascii_letter(C) ->
   attribute_name_1(Rest, <<Acc/binary, C>>);
attribute_name_1(<<C, _/binary>> = Stream, Acc) when ?ws(C);
                                                     C == $/;
                                                     C == $>;
                                                     C == $=->
   {Acc, Stream};
attribute_name_1(<<0, Rest/binary>>, Acc) ->
   % parse error
   attribute_name_1(Rest, <<Acc/binary, 16#FFFD/utf8>>);
attribute_name_1(<<>>, Acc) ->
   {Acc, <<>>};
attribute_name_1(<<C/utf8, Rest/binary>>, Acc) ->
   attribute_name_1(Rest, <<Acc/binary, C/utf8>>).

%% 8.2.4.34      
after_attribute_name(<<$\n, Rest/binary>>, State) ->
   after_attribute_name(Rest, ?i(State));
after_attribute_name(<<C, Rest/binary>>, State) when ?ws(C) ->
   after_attribute_name(Rest, State);
after_attribute_name(<<$/, Rest/binary>>, State) ->
   self_closing_start_tag(Rest, State);
after_attribute_name(<<$=, Rest/binary>>, State) ->
   before_attribute_value(Rest, State);
after_attribute_name(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   call_data_function(Curr, Rest, State1);
after_attribute_name(<<>>, State) ->
   % fatal parse error
   emit(eof, State);
after_attribute_name(Stream, #{current_token := Curr} = State) -> 
   [#attribute{name = AName} = Ca|Atts0] = Curr#start_tag.attributes, 
   Atts = [#attribute{}|[Ca#attribute{value = AName}|Atts0]],
   attribute_name(Stream, State#{current_token := Curr#start_tag{attributes = Atts}}).

%% 8.2.4.35      
before_attribute_value(<<$\n, Rest/binary>>, State) ->
   before_attribute_value(Rest, ?i(State));
before_attribute_value(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_attribute_value(Rest, State);
before_attribute_value(<<$\", Rest/binary>>, State) ->
   attribute_value_double_quoted(Rest, State);
before_attribute_value(<<$', Rest/binary>>, State) ->
   attribute_value_single_quoted(Rest, State);
before_attribute_value(<<$>, _/binary>> = Stream, State) ->
   % parse error
   attribute_value_unquoted(Stream, State);
before_attribute_value(Stream, State) -> 
   attribute_value_unquoted(Stream, State).

%% 8.2.4.36      
attribute_value_double_quoted(<<$\", Rest/binary>>, State) ->
   after_attribute_value_quoted(Rest, State);
attribute_value_double_quoted(<<$&, Rest/binary>>, State) ->
   State1 = State#{return_fun := ?FUNCTION_NAME},
   character_reference(Rest, State1);
attribute_value_double_quoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_att_value(<<16#FFFD/utf8>>, Curr)},
   attribute_value_double_quoted(Rest, State1);
attribute_value_double_quoted(<<>>, State) ->
   % fatal parse error
   emit(eof, State);
attribute_value_double_quoted(Stream, #{current_token := Curr,
                                        att_dq_stop := Stop,
                                        nl_cp := NlCp,
                                        line_num := LineNum} = State) ->
   {Len, LineNum1} = find_stop(Stream, Stop, LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = State#{current_token := append_to_att_value(Chars, Curr)},
   attribute_value_double_quoted(Rest, State1#{line_num := LineNum1}).

%% 8.2.4.37      
attribute_value_single_quoted(<<$', Rest/binary>>, State) ->
   after_attribute_value_quoted(Rest, State);
attribute_value_single_quoted(<<$&, Rest/binary>>, State) ->
   State1 = State#{return_fun := ?FUNCTION_NAME},
   character_reference(Rest, State1);
attribute_value_single_quoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_att_value(<<16#FFFD/utf8>>, Curr)},
   attribute_value_single_quoted(Rest, State1);
attribute_value_single_quoted(<<>>, State) ->
   % fatal parse error
   emit(eof, State);
attribute_value_single_quoted(Stream, #{current_token := Curr,
                                        att_sq_stop := Stop,
                                        nl_cp := NlCp,
                                        line_num := LineNum} = State) ->
   {Len, LineNum1} = find_stop(Stream, Stop, LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = State#{current_token := append_to_att_value(Chars, Curr)},
   attribute_value_single_quoted(Rest, State1#{line_num := LineNum1}).

%% 8.2.4.38      
attribute_value_unquoted(<<$\n, Rest/binary>>, State) ->
   before_attribute_name(Rest, ?i(State));
attribute_value_unquoted(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_attribute_name(Rest, State);
attribute_value_unquoted(<<$&, Rest/binary>>, State) ->
   State1 = State#{return_fun := ?FUNCTION_NAME},
   character_reference(Rest, State1);
attribute_value_unquoted(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   call_data_function(Curr, Rest, State1);
attribute_value_unquoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_att_value(16#FFFD, Curr)},
   attribute_value_unquoted(Rest, State1);
attribute_value_unquoted(<<C, Rest/binary>>, #{current_token := Curr} = State)
   when C =:= $\";
        C =:= $';
        C =:= $<;
        C =:= $=;
        C =:= $` ->
   % parse error
   State1 = State#{current_token := append_to_att_value(C, Curr)},
   attribute_value_unquoted(Rest, State1);
attribute_value_unquoted(<<>>, State) ->
   % fatal parse error
   emit(eof, State);
attribute_value_unquoted(<<C/utf8, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_att_value(C, Curr)},
   attribute_value_unquoted(Rest, State1).

%% 8.2.4.39      
after_attribute_value_quoted(<<$\n, Rest/binary>>, State) ->
   before_attribute_name(Rest, ?i(State));
after_attribute_value_quoted(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_attribute_name(Rest, State);
after_attribute_value_quoted(<<$/, Rest/binary>>, State) ->
   self_closing_start_tag(Rest, State);
after_attribute_value_quoted(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   call_data_function(Curr, Rest, State1);
after_attribute_value_quoted(<<>>, State) ->
   % fatal parse error
   emit(eof, State);
after_attribute_value_quoted(Stream, State) -> 
   % parse error
   before_attribute_name(Stream, State).

%% 8.2.4.40      
self_closing_start_tag(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = set_self_closing(Curr),
   State1 = emit(Tok, State),
   call_data_function(Curr, Rest, State1);
self_closing_start_tag(<<>>, State) ->
   % fatal parse error
   emit(eof, State);
self_closing_start_tag(Stream, State) -> 
   % parse error
   before_attribute_name(Stream, State).

%% 8.2.4.41      
bogus_comment(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
bogus_comment(<<>>, #{current_token := Curr} = State) ->
   emit(Curr, State);
bogus_comment(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_value(16#FFFD, Curr)},
   bogus_comment(Rest, State1);
bogus_comment(<<$\n, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_value($\n, Curr)},
   bogus_comment(Rest, ?i(State1));
bogus_comment(<<C, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = State#{current_token := append_to_value(C, Curr)},
   bogus_comment(Rest, State1).

%% 8.2.4.42      
markup_declaration_open(<<$-, $-, Rest/binary>>, State) ->
   Tok = #comment{},
   comment_start(Rest, State#{current_token := Tok});
markup_declaration_open(<<D, O, C, T, Y, P, E, Rest/binary>>, State)
   when D =:= $d orelse D =:= $D,
        O =:= $o orelse O =:= $O,
        C =:= $c orelse C =:= $C,
        T =:= $t orelse T =:= $T,
        Y =:= $y orelse Y =:= $Y,
        P =:= $p orelse P =:= $P,
        E =:= $e orelse E =:= $E ->
   doctype(Rest, State);
markup_declaration_open(<<$[, $C, $D, $A, $T, $A, $[, Rest/binary>>, State) ->
   %State1 = emit(startCDATA, State),
   cdata_section(Rest, State);
markup_declaration_open(Stream, State) ->
   % parse error
   Tok = #comment{},
   bogus_comment(Stream, State#{current_token := Tok}).

%% 8.2.4.43      
comment_start(<<$-, Rest/binary>>, State) ->
   comment_start_dash(Rest, State);
comment_start(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   %parse error
   State1 = emit(Curr, State),
   data(Rest, State1);
comment_start(Stream, State) -> 
   comment(Stream, State).

%% 8.2.4.44      
comment_start_dash(<<$-, Rest/binary>>, State) ->
   comment_end(Rest, State);
comment_start_dash(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   %parse error
   State1 = emit(Curr, State),
   data(Rest, State1);
comment_start_dash(Stream, #{current_token := Curr} = State) -> 
   Tok = append_to_value($-, Curr),
   comment(Stream, State#{current_token := Tok}).

%% 8.2.4.45      
comment(<<$<, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_value($<, Curr),
   comment_less_than_sign(Rest, State#{current_token := Tok});
comment(<<$-, Rest/binary>>, State) ->
   comment_end_dash(Rest, State);
comment(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_value(16#FFFD, Curr),
   comment_less_than_sign(Rest, State#{current_token := Tok});
comment(<<>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   emit(eof, State1);
comment(Stream, #{current_token := Curr,
                  nl_cp := NlCp,
                  line_num := LineNum} = State) ->
   {Len, LineNum1} = find_stop(Stream, [<<$<>>, <<$->>, <<0>>], LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   Tok = append_to_value(Chars, Curr),
   comment(Rest, State#{current_token := Tok,
                        line_num := LineNum1}).

%% 8.2.4.46      
comment_less_than_sign(<<$!, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_value($!, Curr),
   comment_less_than_sign_bang(Rest, State#{current_token := Tok});
comment_less_than_sign(<<$<, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_value($<, Curr),
   comment_less_than_sign(Rest, State#{current_token := Tok});
comment_less_than_sign(Stream, State) -> 
   comment(Stream, State).

%% 8.2.4.47      
comment_less_than_sign_bang(<<$-, Rest/binary>>, State) ->
   comment_less_than_sign_bang_dash(Rest, State);
comment_less_than_sign_bang(Stream, State) ->
   comment(Stream, State).

%% 8.2.4.48      
comment_less_than_sign_bang_dash(<<$-, Rest/binary>>, State) ->
   comment_less_than_sign_bang_dash_dash(Rest, State);
comment_less_than_sign_bang_dash(Stream, State) -> 
   comment_end_dash(Stream, State).

%% 8.2.4.49      
comment_less_than_sign_bang_dash_dash(<<$-, _/binary>> = Stream, State) ->
   comment_end(Stream, State);
comment_less_than_sign_bang_dash_dash(Stream, State) ->
   % parse error
   comment_end(Stream, State).

%% 8.2.4.50      
comment_end_dash(<<$-, Rest/binary>>, State) ->
   comment_end(Rest, State);
comment_end_dash(<<>>, #{current_token := Curr} = State) ->
   % parse error
   State1 = emit(Curr, State),
   emit(eof, State1);
comment_end_dash(Stream, #{current_token := Curr} = State) ->
   Tok = append_to_value($-, Curr),
   comment(Stream, State#{current_token := Tok}).

%% 8.2.4.51      
comment_end(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
comment_end(<<$!, Rest/binary>>, State) ->
   comment_end_bang(Rest, State);
comment_end(<<$-, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_value($-, Curr),
   comment_end(Rest, State#{current_token := Tok});
comment_end(<<>>, #{current_token := Curr} = State) ->
   % parse error
   State1 = emit(Curr, State),
   emit(eof, State1);
comment_end(Stream, #{current_token := Curr} = State) ->
   Tok = append_to_value($-, Curr),
   Tok1 = append_to_value($-, Tok),
   comment(Stream, State#{current_token := Tok1}).

%% 8.2.4.52      
comment_end_bang(<<$-, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_value($-, Curr),
   Tok1 = append_to_value($-, Tok),
   Tok2 = append_to_value($!, Tok1),
   comment_end_dash(Rest, State#{current_token := Tok2});
comment_end_bang(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   State1 = emit(Curr, State),
   data(Rest, State1);
comment_end_bang(<<>>, #{current_token := Curr} = State) ->
   % parse error
   State1 = emit(Curr, State),
   emit(eof, State1);
comment_end_bang(Stream, #{current_token := Curr} = State) ->
   Tok = append_to_value($-, Curr),
   Tok1 = append_to_value($-, Tok),
   Tok2 = append_to_value($!, Tok1),
   comment(Stream, State#{current_token := Tok2}).

%% 8.2.4.53      
doctype(<<$\n, Rest/binary>>, State) ->
   before_doctype_name(Rest, ?i(State));
doctype(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_doctype_name(Rest, State);
doctype(<<>>, State) ->
   % parse error
   Tok = #doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
doctype(Stream, State) ->
   % parse error
   before_doctype_name(Stream, State).

%% 8.2.4.54      
before_doctype_name(<<$\n, Rest/binary>>, State) ->
   before_doctype_name(Rest, ?i(State));
before_doctype_name(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_doctype_name(Rest, State);
before_doctype_name(<<C, Rest/binary>>, State) when ?upper_ascii_letter(C) ->
   Tok = #doctype{name = <<(C + 32)>>},
   doctype_name(Rest, State#{current_token := Tok});
before_doctype_name(<<0, Rest/binary>>, State) ->
   % parse error
   Tok = #doctype{name = <<16#FFFD/utf8>>},
   doctype_name(Rest, State#{current_token := Tok});
before_doctype_name(<<$>, Rest/binary>>, State) ->
   % parse error
   Tok = #doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
before_doctype_name(<<>>, State) ->
   % parse error
   Tok = #doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
before_doctype_name(<<C/utf8, Rest/binary>>, State) ->
   Tok = #doctype{name = <<C/utf8>>},
   doctype_name(Rest, State#{current_token := Tok}).

%% 8.2.4.55      
doctype_name(<<$\n, Rest/binary>>, State) ->
   after_doctype_name(Rest, ?i(State));
doctype_name(<<C, Rest/binary>>, State) when ?ws(C) ->
   after_doctype_name(Rest, State);
doctype_name(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
doctype_name(<<C, Rest/binary>>, #{current_token := Curr} = State) when ?upper_ascii_letter(C) ->
   Tok = append_to_name(C + 32, Curr),
   doctype_name(Rest, State#{current_token := Tok});
doctype_name(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = append_to_name(16#FFFD, Curr),
   doctype_name(Rest, State#{current_token := Tok});
doctype_name(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
doctype_name(<<C, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_name(C, Curr),
   doctype_name(Rest, State#{current_token := Tok}).

%% 8.2.4.56      
after_doctype_name(<<$\n, Rest/binary>>, State) ->
   after_doctype_name(Rest, ?i(State));
after_doctype_name(<<C, Rest/binary>>, State) when ?ws(C) ->
   after_doctype_name(Rest, State);
after_doctype_name(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
after_doctype_name(<<P, U, B, L, I, C, Rest/binary>>, State)
   when P =:= $p orelse P =:= $P,
        U =:= $u orelse U =:= $U,
        B =:= $b orelse B =:= $B,
        L =:= $l orelse L =:= $L,
        I =:= $i orelse I =:= $I,
        C =:= $c orelse C =:= $C ->
      after_doctype_public_keyword(Rest, State);
after_doctype_name(<<S, Y, Z, T, E, M, Rest/binary>>, State)
   when S =:= $s orelse S =:= $S,
        Y =:= $y orelse Y =:= $Y,
        Z =:= $s orelse Z =:= $S,
        T =:= $t orelse T =:= $T,
        E =:= $e orelse E =:= $E,
        M =:= $m orelse M =:= $M ->
      after_doctype_system_keyword(Rest, State);
after_doctype_name(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
after_doctype_name(Stream, #{current_token := Curr} = State) -> 
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Stream, State#{current_token := Tok}).

%% 8.2.4.57      
after_doctype_public_keyword(<<$\n, Rest/binary>>, State) ->
   before_doctype_public_identifier(Rest, ?i(State));
after_doctype_public_keyword(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_doctype_public_identifier(Rest, State);
after_doctype_public_keyword(<<$\", Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{public = <<>>},
   doctype_public_identifier_double_quoted(Rest, State#{current_token := Tok});
after_doctype_public_keyword(<<$', Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{public = <<>>},
   doctype_public_identifier_single_quoted(Rest, State#{current_token := Tok});
after_doctype_public_keyword(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
after_doctype_public_keyword(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
after_doctype_public_keyword(<<_, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Rest, State#{current_token := Tok}).

%% 8.2.4.58      
before_doctype_public_identifier(<<$\n, Rest/binary>>, State) ->
   before_doctype_public_identifier(Rest, ?i(State));
before_doctype_public_identifier(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_doctype_public_identifier(Rest, State);
before_doctype_public_identifier(<<$\", Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = Curr#doctype{public = <<>>},
   doctype_public_identifier_double_quoted(Rest, State#{current_token := Tok});
before_doctype_public_identifier(<<$', Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = Curr#doctype{public = <<>>},
   doctype_public_identifier_single_quoted(Rest, State#{current_token := Tok});
before_doctype_public_identifier(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
before_doctype_public_identifier(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
before_doctype_public_identifier(<<_, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Rest, State#{current_token := Tok}).

%% 8.2.4.59      
doctype_public_identifier_double_quoted(<<$\", Rest/binary>>, State) ->
   after_doctype_public_identifier(Rest, State);
doctype_public_identifier_double_quoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = append_to_public(16#FFFD, Curr),
   doctype_public_identifier_double_quoted(Rest, State#{current_token := Tok});
doctype_public_identifier_double_quoted(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
doctype_public_identifier_double_quoted(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
doctype_public_identifier_double_quoted(<<$\n, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_public($\n, Curr),
   doctype_public_identifier_double_quoted(Rest, ?i(State#{current_token := Tok}));
doctype_public_identifier_double_quoted(<<C/utf8, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_public(C, Curr),
   doctype_public_identifier_double_quoted(Rest, State#{current_token := Tok}).

%% 8.2.4.60      
doctype_public_identifier_single_quoted(<<$', Rest/binary>>, State) ->
   after_doctype_public_identifier(Rest, State);
doctype_public_identifier_single_quoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = append_to_public(16#FFFD, Curr),
   doctype_public_identifier_single_quoted(Rest, State#{current_token := Tok});
doctype_public_identifier_single_quoted(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
doctype_public_identifier_single_quoted(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
doctype_public_identifier_single_quoted(<<$\n, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_public($\n, Curr),
   doctype_public_identifier_single_quoted(Rest, ?i(State#{current_token := Tok}));
doctype_public_identifier_single_quoted(<<C/utf8, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_public(C, Curr),
   doctype_public_identifier_single_quoted(Rest, State#{current_token := Tok}).

%% 8.2.4.61      
after_doctype_public_identifier(<<$\n, Rest/binary>>, State) ->
   between_doctype_public_and_system_identifiers(Rest, ?i(State));
after_doctype_public_identifier(<<C, Rest/binary>>, State) when ?ws(C) ->
   between_doctype_public_and_system_identifiers(Rest, State);
after_doctype_public_identifier(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
after_doctype_public_identifier(<<$\", Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_double_quoted(Rest, State#{current_token := Tok});
after_doctype_public_identifier(<<$', Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_single_quoted(Rest, State#{current_token := Tok});
after_doctype_public_identifier(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
after_doctype_public_identifier(<<_, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Rest, State#{current_token := Tok}).

%% 8.2.4.62      
between_doctype_public_and_system_identifiers(<<$\n, Rest/binary>>, State) ->
   between_doctype_public_and_system_identifiers(Rest, ?i(State));
between_doctype_public_and_system_identifiers(<<C, Rest/binary>>, State) when ?ws(C) ->
   between_doctype_public_and_system_identifiers(Rest, State);
between_doctype_public_and_system_identifiers(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
between_doctype_public_and_system_identifiers(<<$\", Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_double_quoted(Rest, State#{current_token := Tok});
between_doctype_public_and_system_identifiers(<<$', Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_single_quoted(Rest, State#{current_token := Tok});
between_doctype_public_and_system_identifiers(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
between_doctype_public_and_system_identifiers(<<_, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Rest, State#{current_token := Tok}).

%% 8.2.4.63      
after_doctype_system_keyword(<<$\n, Rest/binary>>, State) ->
   before_doctype_system_identifier(Rest, ?i(State));
after_doctype_system_keyword(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_doctype_system_identifier(Rest, State);
after_doctype_system_keyword(<<$\", Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_double_quoted(Rest, State#{current_token := Tok});
after_doctype_system_keyword(<<$', Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_single_quoted(Rest, State#{current_token := Tok});
after_doctype_system_keyword(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
after_doctype_system_keyword(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
after_doctype_system_keyword(<<_, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Rest, State#{current_token := Tok}).

%% 8.2.4.64      
before_doctype_system_identifier(<<$\n, Rest/binary>>, State) ->
   before_doctype_system_identifier(Rest, ?i(State));
before_doctype_system_identifier(<<C, Rest/binary>>, State) when ?ws(C) ->
   before_doctype_system_identifier(Rest, State);
before_doctype_system_identifier(<<$\", Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_double_quoted(Rest, State#{current_token := Tok});
before_doctype_system_identifier(<<$', Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = Curr#doctype{system = <<>>},
   doctype_system_identifier_single_quoted(Rest, State#{current_token := Tok});
before_doctype_system_identifier(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
before_doctype_system_identifier(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
before_doctype_system_identifier(<<_, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   bogus_doctype(Rest, State#{current_token := Tok}).

%% 8.2.4.65      
doctype_system_identifier_double_quoted(<<$\", Rest/binary>>, State) ->
   after_doctype_system_identifier(Rest, State);   
doctype_system_identifier_double_quoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = append_to_system(16#FFFD, Curr),
   doctype_system_identifier_double_quoted(Rest, State#{current_token := Tok});
doctype_system_identifier_double_quoted(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
doctype_system_identifier_double_quoted(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
doctype_system_identifier_double_quoted(<<$\n, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_system($\n, Curr),
   doctype_system_identifier_double_quoted(Rest, ?i(State#{current_token := Tok}));
doctype_system_identifier_double_quoted(<<C/utf8, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_system(C, Curr),
   doctype_system_identifier_double_quoted(Rest, State#{current_token := Tok}).

%% 8.2.4.66      
doctype_system_identifier_single_quoted(<<$', Rest/binary>>, State) ->
   after_doctype_system_identifier(Rest, State);   
doctype_system_identifier_single_quoted(<<0, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = append_to_system(16#FFFD, Curr),
   doctype_system_identifier_single_quoted(Rest, State#{current_token := Tok});
doctype_system_identifier_single_quoted(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   data(Rest, State1);
doctype_system_identifier_single_quoted(<<>>, #{current_token := Curr} = State) ->
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
doctype_system_identifier_single_quoted(<<$\n, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_system($\n, Curr),
   doctype_system_identifier_single_quoted(Rest, ?i(State#{current_token := Tok}));
doctype_system_identifier_single_quoted(<<C/utf8, Rest/binary>>, #{current_token := Curr} = State) ->
   Tok = append_to_system(C, Curr),
   doctype_system_identifier_single_quoted(Rest, State#{current_token := Tok}).

%% 8.2.4.67      
after_doctype_system_identifier(<<$\n, Rest/binary>>, State) ->
   after_doctype_system_identifier(Rest, ?i(State));
after_doctype_system_identifier(<<C, Rest/binary>>, State) when ?ws(C) ->
   after_doctype_system_identifier(Rest, State);
after_doctype_system_identifier(<<$>, Rest/binary>>, #{current_token := Curr} = State) ->
   State1 = emit(Curr, State),
   data(Rest, State1);
after_doctype_system_identifier(<<>>, #{current_token := Curr} = State) -> 
   % parse error
   Tok = Curr#doctype{quirks = true},
   State1 = emit(Tok, State),
   emit(eof, State1);
after_doctype_system_identifier(<<_, Rest/binary>>, State) -> 
   % parse error
   bogus_doctype(Rest, State).

%% 8.2.4.68      
bogus_doctype(<<>>, #{current_token := Curr} = State) ->
   % parse error
   State1 = emit(Curr, State),
   emit(eof, State1);
bogus_doctype(<<$>, Rest/binary>>, #{current_token := Curr} = State) -> 
   State1 = emit(Curr, State),
   data(Rest, State1);
bogus_doctype(<<$\n, Rest/binary>>, State) -> 
   % parse error
   bogus_doctype(Rest, ?i(State));
bogus_doctype(<<_, Rest/binary>>, State) -> 
   % parse error
   bogus_doctype(Rest, State).

%% 8.2.4.69      
cdata_section(<<>>, State) ->
   % parse error
   emit(eof, State);
cdata_section(<<$], Rest/binary>>, State) ->
   cdata_section_bracket(Rest, State);
cdata_section(Stream, #{line_num := LineNum,
                        nl_cp := NlCp} = State) ->
   {Len, LineNum1} = find_stop(Stream, [<<$]>>], LineNum, NlCp),
   <<Chars:Len/binary, Rest/binary>> = Stream,
   State1 = emit(#chars{data = Chars}, State),
   cdata_section(Rest, State1#{line_num := LineNum1}).

%% 8.2.4.70      
cdata_section_bracket(<<$], Rest/binary>>, State0) ->
   cdata_section_end(Rest, State0);
cdata_section_bracket(Stream, State0) -> 
   State1 = emit(#char{data = $]}, State0),
   cdata_section(Stream, State1).

%% 8.2.4.71      
cdata_section_end(<<$], Rest/binary>>, State0) ->
   State1 = emit(#char{data = $]}, State0),
   cdata_section_end(Rest, State1);
cdata_section_end(<<$>, Rest/binary>>, State) ->
   %State1 = emit(endCDATA, State),
   data(Rest, State);
cdata_section_end(Stream, State0) ->
   State1 = emit(#char{data = $]}, State0),
   State2 = emit(#char{data = $]}, State1),
   cdata_section(Stream, State2).

%% 8.2.4.72      
character_reference(<<$\n, _/binary>> = Stream, State) ->
   character_reference_end(Stream, ?i(State));
character_reference(<<C, _/binary>> = Stream, State)
   when ?ws(C);
        C =:= $<;
        C =:= $& ->
   character_reference_end(Stream, State);
character_reference(<<>> = Stream, State) ->
   character_reference_end(Stream, State);
character_reference(<<$#, Rest/binary>>, State) ->
   numeric_character_reference(Rest, State#{temp_buffer := [$#, $&]});
character_reference(Stream, State) -> 
   case htmerl_util:entity(Stream) of
      {[], _, _} ->
         % no match, parse error
         character_reference_end(Stream, State);
      {Cps, Rest1, false} ->
         % parse error, no semicolon
         Temp = lists:reverse(Cps),
         character_reference_end(Rest1, State#{temp_buffer := Temp});
      {Cps, Rest1, true} ->
         Temp = lists:reverse(Cps),
         character_reference_end(Rest1, State#{temp_buffer := Temp})
   end.

%% 8.2.4.73      
numeric_character_reference(<<C, Rest/binary>>, #{temp_buffer := Buff} = State0)
   when C =:= $x;
        C =:= $X ->
   hexadecimal_character_reference_start(Rest, State0#{temp_buffer := [C|Buff],
                                                       char_ref_code := 0});
numeric_character_reference(Stream, State0) ->
   decimal_character_reference_start(Stream, State0#{char_ref_code := 0}).

%% 8.2.4.74      
hexadecimal_character_reference_start(<<C, _/binary>> = Stream, State0)
   when ?ascii_hex_digit(C) ->
   hexadecimal_character_reference(Stream, State0);
hexadecimal_character_reference_start(Stream, State0) ->
   % parse error
   character_reference_end(Stream, State0).

%% 8.2.4.75
decimal_character_reference_start(<<C, _/binary>> = Stream, State0)
   when ?ascii_digit(C) ->
   decimal_character_reference(Stream, State0);
decimal_character_reference_start(Stream, State0) ->
   % parse error
   character_reference_end(Stream, State0).

%% 8.2.4.76
hexadecimal_character_reference(<<C1, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1) ->
   Code1 = list_to_integer([C1], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(<<C1, C2, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1), ?ascii_hex_digit(C2) ->
   Code1 = list_to_integer([C1, C2], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(<<C1, C2, C3, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1), ?ascii_hex_digit(C2),
        ?ascii_hex_digit(C3) ->
   Code1 = list_to_integer([C1, C2, C3], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(<<C1, C2, C3, C4, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1), ?ascii_hex_digit(C2),
        ?ascii_hex_digit(C3), ?ascii_hex_digit(C4) ->
   Code1 = list_to_integer([C1, C2, C3, C4], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(<<C1, C2, C3, C4, C5, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1), ?ascii_hex_digit(C2),
        ?ascii_hex_digit(C3), ?ascii_hex_digit(C4),
        ?ascii_hex_digit(C5) ->
   Code1 = list_to_integer([C1, C2, C3, C4, C5], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(<<C1, C2, C3, C4, C5, C6, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1), ?ascii_hex_digit(C2),
        ?ascii_hex_digit(C3), ?ascii_hex_digit(C4),
        ?ascii_hex_digit(C5), ?ascii_hex_digit(C6) ->
   Code1 = list_to_integer([C1, C2, C3, C4, C5, C6], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(<<C1, C2, C3, C4, C5, C6, C7, $;, Rest/binary>>, State0)
   when ?ascii_hex_digit(C1), ?ascii_hex_digit(C2),
        ?ascii_hex_digit(C3), ?ascii_hex_digit(C4),
        ?ascii_hex_digit(C5), ?ascii_hex_digit(C6),
        ?ascii_hex_digit(C7) ->
   Code1 = list_to_integer([C1, C2, C3, C4, C5, C6, C7], 16),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
hexadecimal_character_reference(Stream, State0) -> 
   % parse error
   numeric_character_reference_end(Stream, State0).

%% 8.2.4.77
decimal_character_reference(<<C1, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1) ->
   Code1 = list_to_integer([C1]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(<<C1, C2, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1), ?ascii_digit(C2) ->
   Code1 = list_to_integer([C1, C2]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(<<C1, C2, C3, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1), ?ascii_digit(C2),
        ?ascii_digit(C3) ->
   Code1 = list_to_integer([C1, C2, C3]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(<<C1, C2, C3, C4, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1), ?ascii_digit(C2),
        ?ascii_digit(C3), ?ascii_digit(C4) ->
   Code1 = list_to_integer([C1, C2, C3, C4]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(<<C1, C2, C3, C4, C5, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1), ?ascii_digit(C2),
        ?ascii_digit(C3), ?ascii_digit(C4),
        ?ascii_digit(C5) ->
   Code1 = list_to_integer([C1, C2, C3, C4, C5]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(<<C1, C2, C3, C4, C5, C6, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1), ?ascii_digit(C2),
        ?ascii_digit(C3), ?ascii_digit(C4),
        ?ascii_digit(C5), ?ascii_digit(C6) ->
   Code1 = list_to_integer([C1, C2, C3, C4, C5, C6]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(<<C1, C2, C3, C4, C5, C6, C7, $;, Rest/binary>>, State0)
   when ?ascii_digit(C1), ?ascii_digit(C2),
        ?ascii_digit(C3), ?ascii_digit(C4),
        ?ascii_digit(C5), ?ascii_digit(C6),
        ?ascii_digit(C7) ->
   Code1 = list_to_integer([C1, C2, C3, C4, C5, C6, C7]),
   numeric_character_reference_end(Rest, State0#{char_ref_code := Code1});
decimal_character_reference(Stream, State0) -> 
   % parse error
   numeric_character_reference_end(Stream, State0).

%% 8.2.4.78
numeric_character_reference_end(Stream, #{char_ref_code := Code} = State0) -> 
   Code1 = htmerl_util:mask_code_ref(Code),
   character_reference_end(Stream, State0#{temp_buffer := [Code1]}).

%% 8.2.4.79
character_reference_end(Stream, #{temp_buffer := Buff,
                                  current_token := Curr,
                                  return_fun := Return} = State) -> 
   State1 = if Return == attribute_value_double_quoted;
               Return == attribute_value_single_quoted;
               Return == attribute_value_unquoted ->
                  Tok = lists:foldr(fun(C, A) ->
                                          append_to_att_value(C, A)
                                    end, Curr, Buff),
                  State#{current_token := Tok};
               true ->
                  lists:foldr(fun(C, A) ->
                                    emit(#char{data = C}, A)
                              end , State, Buff)
            end,
   case Return of
      data ->
         data(Stream, State1);
      rcdata ->
         rcdata(Stream, State1);
      attribute_value_double_quoted ->
         attribute_value_double_quoted(Stream, State1);
      attribute_value_single_quoted ->
         attribute_value_single_quoted(Stream, State1);
      attribute_value_unquoted ->
         attribute_value_unquoted(Stream, State1)      
   end.


%% ====================================================================
%% 8.2.5 Tree Construction
%% ====================================================================

%% inital state
dispatch(#{insertion_mode := initial} = State, Token) ->
   case Token of
      #char{data = C} when ?ws(C) ->
         State;
      #comment{data = Comment} ->
         State1 = send_event(startDocument, State),
         send_event({comment, Comment}, State1);
      #doctype{name = Name,
               public = Pub,
               system = Sys} ->
         State1 = send_event(startDocument, State),
         State2 = send_event({startDTD, Name, u(Pub), u(Sys)}, State1),
         State3 = send_event(endDTD, State2),
         State3#{insertion_mode := before_html};
      _ ->
         % parse error
         State1 = send_event(startDocument, State),
         dispatch(State1#{insertion_mode := before_html}, Token)
   end;
%% before_html state
dispatch(#{insertion_mode := before_html} = State, Token) ->
   case Token of
      #doctype{} ->
         % parse error
         State;
      #comment{data = Comment} ->
         send_event({comment, Comment}, State);
      #char{data = C} when ?ws(C) ->
         State;
      #start_tag{name = <<"html">>} ->
         Ns = {startPrefixMapping, <<>>, ?HTML},
         State1 = send_event(Ns, State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := before_head};
      #end_tag{name = Name} when Name == <<"head">>;
                                 Name == <<"body">>;
                                 Name == <<"html">>;
                                 Name == <<"br">> ->
         Ns = {startPrefixMapping, <<>>, ?HTML},
         Token1 = #start_tag{name = <<"html">>},
         State1 = send_event(Ns, State),
         State2 = add_html_element(Token1, State1),
         dispatch(State2#{insertion_mode := before_head}, Token);
      #end_tag{} ->
         %parse error - ignoring token
         State;
      _ ->
         Ns = {startPrefixMapping, <<>>, ?HTML},
         Token1 = #start_tag{name = <<"html">>},
         State1 = send_event(Ns, State),
         State2 = add_html_element(Token1, State1),
         dispatch(State2#{insertion_mode := before_head}, Token)
   end;
%% before_head state
dispatch(#{insertion_mode := before_head} = State, Token) ->
   case Token of
      #char{data = C} when ?ws(C) ->
         add_text_char(C, State);
      #comment{data = Comment} ->
         send_event({comment, Comment}, State);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"html">>} ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := before_head};
      #start_tag{name = <<"head">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_head};
      #end_tag{name = Name} when Name == <<"head">>;
                                 Name == <<"body">>;
                                 Name == <<"html">>;
                                 Name == <<"br">> ->
         Token1 = #start_tag{name = <<"head">>},
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token1, State1),
         dispatch(State2#{insertion_mode := in_head}, Token);
      #end_tag{} ->
         % parse error
         State;
      _ ->
         Token1 = #start_tag{name = <<"head">>},
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token1, State1),
         dispatch(State2#{insertion_mode := in_head}, Token)
   end;
%% in_head state
dispatch(#{insertion_mode := in_head} = State, Token) -> 
   case Token of
      #char{data = C} when ?ws(C) ->
         add_text_char(C, State);
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"html">>} ->
         State1 = maybe_pop_text(State), 
         State2 = dispatch(State1#{insertion_mode := in_body}, Token),
         State2#{insertion_mode := in_head};
      #start_tag{name = Name}
         when Name == <<"base">>; Name == <<"basefont">>; Name == <<"bgsound">>;
              Name == <<"link">>; Name == <<"meta">> ->
         State1 = maybe_pop_text(State),
         add_html_element(Token, State1);
      #start_tag{name = Name}
         when Name == <<"title">>; Name == <<"noscript">>; Name == <<"noframes">>;
              Name == <<"style">>; Name == <<"script">> ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := text,
                 orig_insertion_mode := in_head};
      #start_tag{name = <<"template">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_template,
                 orig_insertion_mode := in_head};
      #end_tag{name = <<"head">>} ->
         State1 = maybe_pop_text(State),
         State2 = pop_all_to_tag(<<"head">>,State1),
         State2#{insertion_mode := after_head};
      #end_tag{name = Name} 
         when Name == <<"base">>; Name == <<"basefont">>; Name == <<"bgsound">>;
              Name == <<"link">>; Name == <<"meta">>;
              Name == <<"title">>; Name == <<"noscript">>; Name == <<"noframes">>;
              Name == <<"style">>; Name == <<"script">> ->
         State1 = maybe_pop_text(State),
         pop_all_to_tag(Name, State1);
      #end_tag{name = <<"template">>} ->
         case is_open(<<"template">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               pop_all_to_tag(<<"template">>, State1)
         end;
      #end_tag{name = Name} when Name == <<"body">>;
                                 Name == <<"html">>;
                                 Name == <<"br">> ->
         State1 = maybe_pop_text(State),
         State2 = pop_all_to_tag(Name, State1), % should be "head"
         dispatch(State2#{insertion_mode := after_head}, Token);
      #start_tag{name = <<"head">>} ->
         %parse error
         State;
      #end_tag{} ->
         %parse error
         State;
      _ ->
         State1 = maybe_pop_text(State),
         State2 = pop_all_to_tag(<<"head">>, State1), % should be "head"
         dispatch(State2#{insertion_mode := after_head}, Token)
   end;

%% in_head_noscript state
dispatch(#{insertion_mode := in_head_noscript} = State, _Token) -> State;

%% after_head state
dispatch(#{insertion_mode := after_head} = State, Token) -> 
   case Token of
      #char{data = C} when ?ws(C) ->
         add_text_char(C, State);
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"html">>} ->
         State1 = maybe_pop_text(State), 
         State2 = dispatch(State1#{insertion_mode := in_body}, Token),
         State2#{insertion_mode := after_head};
      #start_tag{name = <<"body">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_body};
      #start_tag{name = <<"frameset">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_frameset};
      #start_tag{name = Name}
         when Name == <<"base">>;
              Name == <<"basefont">>;
              Name == <<"bgsound">>;
              Name == <<"link">>;
              Name == <<"meta">>;
              Name == <<"noframes">>;
              Name == <<"script">>;
              Name == <<"style">>;
              Name == <<"template">>;
              Name == <<"title">> -> % head elements after head
         State1 = maybe_pop_text(State), 
         State2 = dispatch(State1#{insertion_mode := in_head}, Token),
         State2#{insertion_mode := after_head};
      #end_tag{name = <<"template">>} ->
         State1 = maybe_pop_text(State), 
         State2 = dispatch(State1#{insertion_mode := in_head}, Token),
         State2#{insertion_mode := after_head};
      #start_tag{name = <<"head">>} ->
         % parse error
         State;
      #end_tag{} ->
         % parse error
         State;
      _ ->
         State1 = maybe_pop_text(State),
         Token1 = #start_tag{name = <<"body">>},
         State2 = add_html_element(Token1, State1),
         State2#{insertion_mode := in_body}
   end;

%% in_body state
dispatch(#{insertion_mode := in_body} = State, Token) -> 
   case Token of
      #char{data = C} ->
         add_text_char(C, State);
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = Name} when Name == <<"html">>;
                                   Name == <<"body">>;
                                   Name == <<"frameset">> ->
         State;
      #start_tag{name = Name}
         when Name == <<"title">>; 
              Name == <<"noscript">>; 
              Name == <<"noframes">>;
              Name == <<"style">>; 
              Name == <<"script">> ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := text,
                 orig_insertion_mode := in_body};
      #end_tag{name = Name} 
         when Name == <<"title">>; 
              Name == <<"noscript">>; 
              Name == <<"noframes">>;
              Name == <<"style">>; 
              Name == <<"script">> ->
         State1 = maybe_pop_text(State),
         pop_all_to_tag(Name, State1);
      #start_tag{name = Name}
         when Name == <<"base">>;
              Name == <<"basefont">>;
              Name == <<"bgsound">>;
              Name == <<"link">>;
              Name == <<"meta">>;
              Name == <<"template">> -> % head elements after head
         State1 = maybe_pop_text(State), 
         State2 = dispatch(State1#{insertion_mode := in_head}, Token),
         State2#{insertion_mode := in_body};
      #end_tag{name = <<"template">>} ->
         State1 = maybe_pop_text(State), 
         State2 = dispatch(State1#{insertion_mode := in_head}, Token),
         State2#{insertion_mode := in_body};
      eof ->
         % Stop parsing
         finish_document(pop_all(State));
      #end_tag{name = <<"body">>} ->
         % if the stack is not empty, parse error
         State1 = maybe_pop_text(State),
         State2 = pop_all_to_tag(<<"body">>, State1),
         State2#{insertion_mode := after_body};
      #end_tag{name = <<"html">>} ->
         % if the stack is not empty, parse error
         State1 = maybe_pop_text(State),
         State2 = pop_all_to_tag(<<"body">>, State1),
         dispatch(State2#{insertion_mode := after_body}, Token);
      #start_tag{name = N} 
         when N == <<"address">>; N == <<"article">>; N == <<"aside">>; N == <<"blockquote">>; 
              N == <<"center">>; N == <<"details">>; N == <<"dialog">>; N == <<"dir">>; 
              N == <<"div">>; N == <<"dl">>; N == <<"fieldset">>; N == <<"figcaption">>; 
              N == <<"figure">>; N == <<"footer">>; N == <<"header">>; N == <<"main">>; 
              N == <<"nav">>; N == <<"ol">>; N == <<"p">>; N == <<"section">>; N == <<"summary">>; 
              N == <<"ul">>;
              %% added, but have other special cases              
              N == <<"pre">>; N == <<"listing">>;
              N == <<"form">>; N == <<"plaintext">>; N == <<"button">>; 
              % formatting
              N == <<"a">>; N == <<"b">>; N == <<"big">>; N == <<"code">>; N == <<"em">>; 
              N == <<"font">>; N == <<"i">>; N == <<"s">>; N == <<"small">>; N == <<"strike">>; 
              N == <<"strong">>; N == <<"tt">>; N == <<"u">>; 
              N == <<"nobr">>; N == <<"applet">>; N == <<"marquee">>; N == <<"object">> ->
         State1 = maybe_pop_text(State),
         add_html_element(Token, State1);
      #start_tag{name = N} 
         when N == <<"h1">>;
              N == <<"h2">>;
              N == <<"h3">>;
              N == <<"h4">>;
              N == <<"h5">>;
              N == <<"h6">> ->
         State1 = maybe_pop_text(State),
         State2 =  case get_current_node(State1) of
                      #start_tag{name = N1} 
                         when N1 == <<"h1">>;
                              N1 == <<"h2">>;
                              N1 == <<"h3">>;
                              N1 == <<"h4">>;
                              N1 == <<"h5">>;
                              N1 == <<"h6">> ->
                         pop_all_to_tag(N1, State1);
                      _ ->
                         State1
                   end,
         add_html_element(Token, State2);
      #start_tag{name = Name} when Name == <<"li">>;
                                   Name == <<"dd">>;
                                   Name == <<"dt">> ->
         State1 = maybe_pop_text(State),
         State2 = case get_current_node(State1) of
                     #start_tag{name = Name} -> % close
                        pop_all_to_tag(Name, State1);
                     _ ->
                        State1
                  end,         
         add_html_element(Token, State2);
      #end_tag{name = N} 
         when N == <<"address">>; N == <<"article">>; N == <<"aside">>; N == <<"blockquote">>; 
              N == <<"button">>; N == <<"center">>; N == <<"details">>; N == <<"dialog">>; 
              N == <<"dir">>; N == <<"div">>; N == <<"dl">>; N == <<"fieldset">>; 
              N == <<"figcaption">>; N == <<"figure">>; N == <<"footer">>; N == <<"header">>; 
              N == <<"listing">>; N == <<"main">>; N == <<"nav">>; N == <<"ol">>; N == <<"pre">>; 
              N == <<"section">>; N == <<"summary">>; N == <<"ul">>;
              %% added
              N == <<"form">> ->
         State1 = maybe_pop_text(State),
         case is_open(N, State1) of
            false -> % ignore orphaned end tag
               State1;
            true ->
               pop_all_to_tag(N, State1)
         end;
      #end_tag{name = <<"p">>} ->
         State1 = maybe_pop_text(State),
         case is_open(<<"p">>, State1) of
            false -> % add open to orphaned end tag
               State2 = add_html_element(#start_tag{name = <<"p">>}, State1),
               pop_all_to_tag(<<"p">>, State2);
            true ->
               pop_all_to_tag(<<"p">>, State1) % maybe just close it
         end;
      #end_tag{name = N} 
         when N == <<"li">>; N == <<"dd">>; N == <<"dt">>;
              N == <<"h1">>; N == <<"h2">>; N == <<"h3">>;
              N == <<"h4">>; N == <<"h5">>; N == <<"h6">>;
              N == <<"b">>; N == <<"big">>; N == <<"code">>; N == <<"em">>; 
              N == <<"font">>; N == <<"i">>; N == <<"s">>; N == <<"small">>; N == <<"strike">>; 
              N == <<"strong">>; N == <<"tt">>; N == <<"u">>; 
              N == <<"nobr">>; N == <<"applet">>; N == <<"marquee">>; N == <<"object">> ->
         State1 = maybe_pop_text(State),
         case is_open(N, State1) of
            false -> % ignore orphaned end tag
               State1;
            true ->
               pop_all_to_tag(N, State1)
         end;
      #start_tag{name = <<"table">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_table};
      #end_tag{name = <<"br">>} ->
         add_html_element(#start_tag{name = <<"br">>,
                                     closing = true}, State);
      #start_tag{name = <<"image">>} ->
         dispatch(State, Token#start_tag{name = <<"img">>});
      #start_tag{name = N} when N == <<"area">>;
                                N == <<"br">>;
                                N == <<"embed">>;
                                N == <<"img">>;
                                N == <<"wbr">>;
                                N == <<"input">>;
                                N == <<"param">>;
                                N == <<"source">>;
                                N == <<"track">>;
                                N == <<"hr">> ->
         State1 = maybe_pop_text(State),
         add_html_element(Token#start_tag{closing = true}, State1);
      #start_tag{name = N}
         when N == <<"textarea">>;
              N == <<"xmp">>;
              N == <<"iframe">>;
              N == <<"noembed">>;
              N == <<"noscript">> ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := text,
                 orig_insertion_mode := in_body};
      #start_tag{name = <<"select">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_select};
      #start_tag{name = Name} when Name == <<"optgroup">>; Name == <<"option">> ->
         State1 = maybe_pop_text(State),
         State2 = case get_current_node(State1) of
                     #start_tag{name = <<"option">>} ->
                        pop_all_to_tag(<<"option">>, State1);
                     _ ->
                        State1
                  end,
         add_html_element(Token, State2);
      #start_tag{name = Name} when Name == <<"rb">>; Name == <<"rtc">> ->
         State1 = maybe_pop_text(State),
         case is_open(<<"ruby">>, State1) of
            false ->
               % parse error
               add_html_element(Token, State1);
            true ->
               State2 = generate_implied_end_tags(State1),
               % parse error if current not "ruby"
               add_html_element(Token, State2)
         end;
      #start_tag{name = Name} when Name == <<"rp">>; Name == <<"rt">> ->
         State1 = maybe_pop_text(State),
         case is_open(<<"ruby">>, State1) of
            false ->
               % parse error
               add_html_element(Token, State1);
            true ->
               State2 = generate_implied_end_tags(State1, <<"rtc">>),
               % parse error if current not "ruby"
               add_html_element(Token, State2)
         end;
      #start_tag{name = <<"math">>} ->
         State1 = maybe_pop_text(State),
         Ns = {startPrefixMapping, <<>>, ?MATH},
         State2 = send_event(Ns, State1),
         add_math_element(Token, State2);
      #start_tag{name = <<"svg">>} ->
         State1 = maybe_pop_text(State),
         Ns = {startPrefixMapping, <<>>, ?SVG},
         State2 = send_event(Ns, State1),
         add_svg_element(Token, State2);
      #start_tag{name = N} 
         when N == <<"caption">>;  N == <<"col">>;
              N == <<"colgroup">>; N == <<"frame">>;
              N == <<"head">>;     N == <<"tbody">>;
              N == <<"td">>;       N == <<"tfoot">>;
              N == <<"th">>;       N == <<"thead">>;
              N == <<"tr">> ->
         State;
      #start_tag{} ->
         State1 = maybe_pop_text(State),
         #{inscope_namespace := [N|_]} = State1,
         case N of
            html ->
               add_html_element(Token, State1);
            mathml ->
               add_math_element(Token, State1);
            svg ->
               add_svg_element(Token, State1)
         end;
      #end_tag{name = Nm} ->
         State1 = maybe_pop_text(State),
         #{inscope_namespace := [N|_]} = State1,
         Name = case N of
                 html -> Nm;
                 mathml -> {mathml, Nm};
                 svg -> {svg, Nm}
              end,
         pop_all_to_tag(Name, State1);
      _ ->
         State
   end;

%% text state
dispatch(#{insertion_mode := text} = State, Token) ->
   case Token of
      #char{data = C} ->
         add_text_char(C, State);
      _ ->
         #{orig_insertion_mode := Orig} = State,
         dispatch(State#{insertion_mode := Orig,
                         orig_insertion_mode := undefined}, Token)
   end;

%% in_table state
dispatch(#{insertion_mode := in_table} = State, Token) ->
   #start_tag{name = Curr} = get_current_node(State),
   case Token of
      #char{} when Curr == <<"table">>;
                   Curr == <<"tbody">>;
                   Curr == <<"tfoot">>;
                   Curr == <<"thead">>;
                   Curr == <<"tr">> ->
         dispatch(State#{insertion_mode := in_table_text,
                         orig_insertion_mode := in_table}, Token);
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"caption">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_caption};
      #start_tag{name = <<"colgroup">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_column_group};
      #start_tag{name = <<"col">>} -> % missing group
         State1 = maybe_pop_text(State),
         Grp = #start_tag{name = <<"colgroup">>},
         State2 = add_html_element(Grp, State1),
         dispatch(State2#{insertion_mode := in_column_group}, Token);
      #start_tag{name = N} when N == <<"tbody">>;
                                N == <<"tfoot">>;
                                N == <<"thead">> ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_table_body};
      #start_tag{name = N} when N == <<"td">>;
                                N == <<"th">>;
                                N == <<"tr">> -> % missing body
         State1 = maybe_pop_text(State),
         Bdy = #start_tag{name = <<"tbody">>},
         State2 = add_html_element(Bdy, State1),
         dispatch(State2#{insertion_mode := in_table_body}, Token);
      #start_tag{name = <<"table">>} ->
         case is_open(<<"table">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               pop_all_to_tag(<<"table">>, State1)
         end;
      #end_tag{name = <<"table">>} ->
         case is_open(<<"table">>, State) of
            false ->
               State#{insertion_mode := in_body};
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"table">>, State1),
               State2#{insertion_mode := in_body}
         end;
      #end_tag{name = N} when N == <<"body">>; N == <<"caption">>; N == <<"col">>;
                              N == <<"colgroup">>; N == <<"html">>; N == <<"tbody">>; 
                              N == <<"td">>; N == <<"tfoot">>; N == <<"th">>;
                              N == <<"thead">>; N == <<"tr">> ->
         % parse error
         State;
      #start_tag{name = N} when N == <<"style">>;
                                N == <<"script">>;
                                N == <<"template">> ->
         State1 = dispatch(State#{insertion_mode := in_head}, Token),
         State1#{insertion_mode := in_table};
      #end_tag{name = <<"template">>} ->
         State1 = dispatch(State#{insertion_mode := in_head}, Token),
         State1#{insertion_mode := in_table};
      #start_tag{name = N} when N == <<"input">>;
                                N == <<"form">> ->
         % parse error
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         pop_all_to_tag(N, State2);
      eof ->
         dispatch(State#{insertion_mode := in_body}, Token);
      _ ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := in_table}
   end;

%% in_table_text state
dispatch(#{insertion_mode := in_table_text} = State, Token) -> 
   case Token of 
      #char{data = C} ->
         add_text_char(C, State);
      _ ->
         #{orig_insertion_mode := Orig} = State1 = maybe_pop_text(State),
         dispatch(State1#{insertion_mode := Orig,
                          orig_insertion_mode := undefined}, Token)
   end;

%% in_caption state
dispatch(#{insertion_mode := in_caption} = State, Token) ->
   case Token of
      #end_tag{name = <<"caption">>} ->
         case is_open(<<"caption">>, State) of
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"caption">>, State1),
               State2#{insertion_mode := in_table};
            false ->
               % parse error
               State
         end;
      #start_tag{name = N} when N == <<"caption">>;
                                N == <<"col">>;
                                N == <<"colgroup">>;
                                N == <<"tbody">>;
                                N == <<"td">>;
                                N == <<"tfoot">>;
                                N == <<"th">>;
                                N == <<"thead">>;
                                N == <<"tr">> ->
         case is_open(<<"caption">>, State) of
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"caption">>, State1),
               dispatch(State2#{insertion_mode := in_table}, Token);
            false ->
               % parse error
               State
         end;
      #end_tag{name = <<"table">>} ->
         case is_open(<<"caption">>, State) of
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"caption">>, State1),
               dispatch(State2#{insertion_mode := in_table}, Token);
            false ->
               % parse error
               State
         end;
      #end_tag{name = N} when N == <<"caption">>;
                              N == <<"col">>;
                              N == <<"colgroup">>;
                              N == <<"tbody">>;
                              N == <<"td">>;
                              N == <<"tfoot">>;
                              N == <<"th">>;
                              N == <<"thead">>;
                              N == <<"tr">> ->
         State;
      _ ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := in_caption}
   end;

%% in_column_group state
dispatch(#{insertion_mode := in_column_group} = State, Token) -> 
   case Token of
      #char{data = C} when ?ws(C) ->
         add_text_char(C, State);
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"html">>} ->
         State1 = maybe_pop_text(State),
         State2 = dispatch(State1#{insertion_mode := in_body}, Token),
         State2#{insertion_mode := in_column_group};
      #start_tag{name = <<"col">>} ->
         State1 = maybe_pop_text(State),
         add_html_element(Token#start_tag{closing = true}, State1);
      #end_tag{name = <<"colgroup">>} ->
         State1 = maybe_pop_text(State),
         case get_current_node(State) of
            #start_tag{name = <<"colgroup">>} ->
               pop_all_to_tag(<<"colgroup">>, State1);
            _ ->
               State1#{insertion_mode := in_table}
         end;
      #end_tag{name = <<"col">>} ->
         % parse error
         State;
      #start_tag{name = <<"template">>} ->
         State1 = maybe_pop_text(State),
         State2 = dispatch(State1#{insertion_mode := in_head}, Token),
         State2#{insertion_mode := in_column_group};
      #end_tag{name = <<"template">>} ->
         State1 = maybe_pop_text(State),
         State2 = dispatch(State1#{insertion_mode := in_head}, Token),
         State2#{insertion_mode := in_column_group};
      eof ->
         State1 = maybe_pop_text(State),
         State2 = dispatch(State1#{insertion_mode := in_body}, Token),
         State2#{insertion_mode := in_column_group};
      _ ->
         State1 = maybe_pop_text(State),
         case get_current_node(State) of
            #start_tag{name = <<"colgroup">>} ->
               State2 = pop_all_to_tag(<<"colgroup">>, State1),
               State2#{insertion_mode := in_table};
            _ ->
               State1
         end
   end;

%% in_table_body state
dispatch(#{insertion_mode := in_table_body} = State, Token) -> 
   case Token of
      #start_tag{name = <<"tr">>} ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_row};
      #start_tag{name = N} when N == <<"th">>;
                                N == <<"td">> ->
         State1 = maybe_pop_text(State),
         Tok = #start_tag{name = <<"tr">>},
         State2 = add_html_element(Tok, State1),
         dispatch(State2#{insertion_mode := in_row}, Token);
      #end_tag{name = N} when N == <<"tbody">>;
                              N == <<"tfoot">>;
                              N == <<"thead">> ->
         case is_open(N, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(N, State1),
               State2#{insertion_mode := in_table}
         end;
      #start_tag{name = N} when N == <<"caption">>;
                                N == <<"col">>;
                                N == <<"colgroup">>;
                                N == <<"tbody">>;
                                N == <<"tfoot">>;
                                N == <<"thead">> ->
         case get_current_node(State) of
            #start_tag{name = Curr} when Curr == <<"tbody">>;
                                         Curr == <<"thead">>;
                                         Curr == <<"tfoot">> ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(Curr, State1),
               dispatch(State2#{insertion_mode := in_table}, Token);
            _ ->
               State
         end;
      #end_tag{name = <<"table">>} ->
         case get_current_node(State) of
            #start_tag{name = Curr} when Curr == <<"tbody">>;
                                         Curr == <<"thead">>;
                                         Curr == <<"tfoot">> ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(Curr, State1),
               dispatch(State2#{insertion_mode := in_table}, Token);
            _ ->
               State
         end;
      #end_tag{name = N} when N == <<"body">>; N == <<"caption">>;
                              N == <<"col">>; N == <<"colgroup">>;
                              N == <<"html">>; N == <<"td">>;
                              N == <<"th">>; N == <<"tr">> ->
         State;
      _ ->
         State1 = dispatch(State#{insertion_mode := in_table}, Token),
         State1#{insertion_mode := in_table_body}
   end;

%% in_row state
dispatch(#{insertion_mode := in_row} = State, Token) ->
   case Token of
      #start_tag{name = N} when N == <<"th">>;
                                N == <<"td">> ->
         State1 = maybe_pop_text(State),
         State2 = add_html_element(Token, State1),
         State2#{insertion_mode := in_cell};
      #end_tag{name = <<"tr">>} ->
         case is_open(<<"tr">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"tr">>, State1),
               State2#{insertion_mode := in_table_body}
         end;
      #start_tag{name = N} when N == <<"caption">>; N == <<"col">>;
                                N == <<"colgroup">>; N == <<"tbody">>;
                                N == <<"tfoot">>; N == <<"thead">>;
                                N == <<"tr">> ->
         case is_open(<<"tr">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"tr">>, State1),
               dispatch(State2#{insertion_mode := in_table_body}, Token)
         end;
      #end_tag{name = <<"table">>} ->
         case is_open(<<"tr">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"tr">>, State1),
               dispatch(State2#{insertion_mode := in_table_body}, Token)
         end;
      #end_tag{name = N} when N == <<"tbody">>;
                              N == <<"tfoot">>;
                              N == <<"thead">> ->
         case is_open(N, State) orelse is_open(<<"tr">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"tr">>, State1),
               dispatch(State2#{insertion_mode := in_table_body}, Token)
         end;
      #end_tag{name = N} when N == <<"body">>; N == <<"caption">>; N == <<"col">>;
                              N == <<"colgroup">>; N == <<"html">>; N == <<"td">>;
                              N == <<"th">> ->
         % parse error
         State;
      _ ->
         %State1 = 
           dispatch(State#{insertion_mode := in_table}, Token)%,
         %State1#{insertion_mode := in_row}
   end;

%% in_cell state
dispatch(#{insertion_mode := in_cell} = State, Token) -> 
   case Token of
      #end_tag{name = N} when N == <<"td">>;
                              N == <<"th">> ->
         case is_open(N, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(N, State1),
               State2#{insertion_mode := in_row}
         end;
      #start_tag{name = N} when N == <<"caption">>;
                                N == <<"col">>;
                                N == <<"colgroup">>;
                                N == <<"tbody">>;
                                N == <<"td">>;
                                N == <<"tfoot">>;
                                N == <<"th">>;
                                N == <<"thead">>;
                                N == <<"tr">> ->
         TdOpen = is_open(<<"td">>, State),
         case TdOpen orelse is_open(<<"th">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               if TdOpen ->
                     State2 = pop_all_to_tag(<<"td">>, State1),
                     State2#{insertion_mode := in_row};
                  true ->
                     State2 = pop_all_to_tag(<<"th">>, State1),
                     State2#{insertion_mode := in_row}
               end
         end;
      #end_tag{name = N} when N == <<"body">>;
                              N == <<"caption">>;
                              N == <<"col">>;
                              N == <<"colgroup">>;
                              N == <<"html">> ->
         % parse error
         State;
      #end_tag{name = N} when N == <<"table">>;
                              N == <<"tbody">>;
                              N == <<"tfoot">>;
                              N == <<"thead">>;
                              N == <<"tr">> ->
         case is_open(N, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               case is_open(<<"td">>, State) of
                  true ->
                     State2 = pop_all_to_tag(<<"td">>, State1),
                     State2#{insertion_mode := in_row};
                  false ->
                     State2 = pop_all_to_tag(<<"th">>, State1),
                     State2#{insertion_mode := in_row}
               end
         end;
      _ ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := in_cell}
   end;

%% in_select state
dispatch(#{insertion_mode := in_select} = State, Token) ->
   case Token of
      #char{data = 0} ->
         State;
      #char{data = C} ->
         add_text_char(C, State);
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"html">>} ->
         State1 = maybe_pop_text(State),
         State2 = dispatch(State1#{insertion_mode := in_body}, Token),
         State2#{insertion_mode := in_select};
      #start_tag{name = <<"option">>} ->
         State1 = maybe_pop_text(State),
         State2 = case get_current_node(State1) of
                     #start_tag{name = <<"option">>} ->
                        pop_all_to_tag(<<"option">>, State1);
                     _ ->
                        State1
                  end,
         add_html_element(Token, State2);
      #start_tag{name = <<"optgroup">>} ->
         State1 = maybe_pop_text(State),
         State2 = case get_current_node(State1) of
                     #start_tag{name = <<"option">>} ->
                        pop_all_to_tag(<<"option">>, State1);
                     #start_tag{name = <<"optgroup">>} ->
                        pop_all_to_tag(<<"optgroup">>, State1);
                     _ ->
                        State1
                  end,
         add_html_element(Token, State2);
      #end_tag{name = <<"optgroup">>} ->
         State1 = maybe_pop_text(State),
         State2 = case get_current_node(State1) of
                     #start_tag{name = <<"option">>} ->
                        pop_all_to_tag(<<"option">>, State1);
                     _ ->
                        State1
                  end,
         case get_current_node(State2) of
            #start_tag{name = <<"optgroup">>} ->
               pop_all_to_tag(<<"optgroup">>, State2);
            _ ->
               % parse error
               State2
         end;
      #end_tag{name = <<"option">>} ->
         case get_current_node(State) of
            #start_tag{name = <<"option">>} ->
               State1 = maybe_pop_text(State),
               pop_all_to_tag(<<"option">>, State1);
            _ ->
               % parse error
               State
         end;
      #end_tag{name = <<"select">>} ->
         case is_open(<<"select">>, State) of
            false ->
               % parse error
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"select">>, State1),
               %% XXX Reset the insertion mode appropriately.
               State2#{insertion_mode := in_body}
         end;
      #start_tag{name = <<"select">>} -> % treated as end tag
         case is_open(<<"select">>, State) of
            false ->
               % parse error
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"select">>, State1),
               %% XXX Reset the insertion mode appropriately.
               State2#{insertion_mode := in_body}
         end;
      #start_tag{name = N} when N == <<"input">>;
                                N == <<"textarea">> ->
         % parse error
         case is_open(<<"select">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"select">>, State1),
               %% XXX Reset the insertion mode appropriately.
               State2#{insertion_mode := in_body}
         end;
      #start_tag{name = N} when N == <<"script">>;
                                N == <<"template">> ->
         State1 = dispatch(State#{insertion_mode := in_head}, Token),
         State1#{insertion_mode := in_select};
      #end_tag{name = N} when N == <<"template">> ->
         State1 = dispatch(State#{insertion_mode := in_head}, Token),
         State1#{insertion_mode := in_select};
      eof ->
         dispatch(State#{insertion_mode := in_body}, Token);
      _ ->
         % parse error
         State
   end;

%% in_select_in_table state
dispatch(#{insertion_mode := in_select_in_table} = State, Token) ->
   case Token of
      #start_tag{name = N} when N == <<"caption">>;
                                N == <<"table">>;
                                N == <<"tbody">>;
                                N == <<"tfoot">>;
                                N == <<"thead">>;
                                N == <<"tr">>;
                                N == <<"td">>;
                                N == <<"th">> ->
         %parse error
         State1 = maybe_pop_text(State),
         State2 = pop_all_to_tag(<<"select">>, State1),
         %% XXX Reset the insertion mode appropriately.
         dispatch(State2#{insertion_mode := in_body}, Token);
      #end_tag{name = N} when N == <<"caption">>;
                              N == <<"table">>;
                              N == <<"tbody">>;
                              N == <<"tfoot">>;
                              N == <<"thead">>;
                              N == <<"tr">>;
                              N == <<"td">>;
                              N == <<"th">> ->
         %parse error
         case is_open(N, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"select">>, State1),
               %% XXX Reset the insertion mode appropriately.
               dispatch(State2#{insertion_mode := in_body}, Token)
         end;
      _ ->
         State1 = dispatch(State#{insertion_mode := in_select}, Token),
         State1#{insertion_mode := in_select_in_table}
   end;

%% in_template state
dispatch(#{insertion_mode := in_template} = State, Token) -> 
   case Token of
      #char{} ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := in_template};
      #comment{} ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := in_template};
      #doctype{} ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := in_template};
      #start_tag{name = N} when N == <<"base">>;
                                N == <<"basefont">>;
                                N == <<"bgsound">>;
                                N == <<"link">>;
                                N == <<"meta">>;
                                N == <<"noframes">>;
                                N == <<"script">>;
                                N == <<"style">>;
                                N == <<"template">>;
                                N == <<"title">> ->
         State1 = dispatch(State#{insertion_mode := in_head}, Token),
         State1#{insertion_mode := in_template};
      #end_tag{name = N} when N == <<"template">> ->
         State1 = dispatch(State#{insertion_mode := in_head}, Token),
         State1#{insertion_mode := in_template};
      #start_tag{name = N} when N == <<"caption">>;
                                N == <<"colgroup">>;
                                N == <<"tbody">>;
                                N == <<"tfoot">>;
                                N == <<"thead">> ->
         dispatch(State#{insertion_mode := in_table}, Token);
      #start_tag{name = N} when N == <<"col">> ->
         dispatch(State#{insertion_mode := in_column_group}, Token);
      #start_tag{name = N} when N == <<"tr">> ->
         dispatch(State#{insertion_mode := in_table_body}, Token);
      #start_tag{name = N} when N == <<"td">>;
                                N == <<"th">> ->
         dispatch(State#{insertion_mode := in_row}, Token);
      #start_tag{} ->
         dispatch(State#{insertion_mode := in_body}, Token);
      #end_tag{} ->
         % parse error
         State;
      eof ->
         case is_open(<<"template">>, State) of
            false ->
               State;
            true ->
               State1 = maybe_pop_text(State),
               State2 = pop_all_to_tag(<<"template">>, State1),
               %% XXX Reset the insertion mode appropriately.
               dispatch(State2#{insertion_mode := in_body}, Token)
         end
   end;

%% after_body state
dispatch(#{insertion_mode := after_body} = State, Token) -> 
   case Token of
      #char{data = C} when ?ws(C) ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := after_body};
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      #doctype{} ->
         % parse error
         State;
      #start_tag{name = <<"html">>} ->
         State1 = dispatch(State#{insertion_mode := in_body}, Token),
         State1#{insertion_mode := after_body};
      #end_tag{name = <<"html">>} ->
         State1 = maybe_pop_text(State),
         State1#{insertion_mode := after_after_body};
      eof ->
         % stop parsing
         finish_document(pop_all(State));
      _ ->
         dispatch(State#{insertion_mode := in_body}, Token)
   end;

%% in_frameset state
dispatch(#{insertion_mode := in_frameset} = State, _Token) -> State;
%% after_frameset state
dispatch(#{insertion_mode := after_frameset} = State, _Token) -> State;
%% after_after_body state
dispatch(#{insertion_mode := after_after_body} = State, Token) ->
   case Token of
      #comment{data = Comment} ->
         State1 = maybe_pop_text(State),
         send_event({comment, Comment}, State1);
      eof ->
         % stop parsing
         finish_document(pop_all(State));
      _ ->
         dispatch(State#{insertion_mode := in_body}, Token)
   end;

%% after_after_frameset state
dispatch(#{insertion_mode := after_after_frameset} = State, _Token) -> State.




emit(#start_tag{} = Tok, State) ->
   dispatch(State#{last_start_tag := Tok}, norm_tok(Tok));
emit(#chars{data = Data}, State) ->
   add_text_chars(Data, State);
emit(Tok, State) ->
   dispatch(State, Tok).
   
norm_tok(#start_tag{name = Name,
                    closing = Closing,
                    attributes = Ats} = Tok) ->
   Closing1 = (Closing orelse Name == <<"area">> orelse Name == <<"base">> orelse
                 Name == <<"br">> orelse Name == <<"col">> orelse Name == <<"embed">> orelse
                 Name == <<"hr">> orelse Name == <<"img">> orelse Name == <<"input">> orelse
                 Name == <<"link">> orelse Name == <<"meta">> orelse
                 Name == <<"param">> orelse Name == <<"source">> orelse
                 Name == <<"track">> orelse Name == <<"wbr">>),
   Tok#start_tag{closing = Closing1, attributes = lists:reverse(Ats)}.


%% ====================================================================
%%   Helpers
%% ====================================================================

incr_line(#{line_num := LineNum} = S) ->
   S#{line_num := LineNum + 1}.

append_to_name(Char, #start_tag{name = N} = Curr) when is_integer(Char) ->
   Curr#start_tag{name = <<N/binary, Char/utf8>>};
append_to_name(Char, #end_tag{name = N} = Curr) when is_integer(Char) ->
   Curr#end_tag{name = <<N/binary, Char/utf8>>};
append_to_name(Char, #attribute{name = N} = Curr) when is_integer(Char) ->
   Curr#attribute{name = <<N/binary, Char/utf8>>};
append_to_name(Char, #doctype{name = N} = Curr) when is_integer(Char) ->
   Curr#doctype{name = <<N/binary, Char/utf8>>};
append_to_name(Char, #start_tag{name = N} = Curr) ->
   Curr#start_tag{name = <<N/binary, Char/binary>>};
append_to_name(Char, #end_tag{name = N} = Curr) ->
   Curr#end_tag{name = <<N/binary, Char/binary>>};
append_to_name(Char, #attribute{name = N} = Curr) ->
   Curr#attribute{name = <<N/binary, Char/binary>>};
append_to_name(Char, #doctype{name = N} = Curr) ->
   Curr#doctype{name = <<N/binary, Char/binary>>}.

append_to_value(C, #comment{data = V} = A) when is_integer(C) ->
   A#comment{data = <<V/binary, C/utf8>>};
append_to_value(C, #comment{data = V} = A) when is_binary(C) ->
   A#comment{data = <<V/binary, C/binary>>}.

append_to_att_value(#chars{data = C}, #start_tag{attributes = [A|As]} = Curr) ->
   Old = A#attribute.value,
   New = <<Old/binary, C/binary>>,
   Curr#start_tag{attributes = [A#attribute{value = New}|As]};
append_to_att_value(C, #start_tag{attributes = [A|As]} = Curr) when is_integer(C) ->
   Old = A#attribute.value,
   New = <<Old/binary, C/utf8>>,
   Curr#start_tag{attributes = [A#attribute{value = New}|As]};
append_to_att_value(C, #start_tag{attributes = [A|As]} = Curr) ->
   Old = A#attribute.value,
   New = <<Old/binary, C/binary>>,
   Curr#start_tag{attributes = [A#attribute{value = New}|As]}.

append_to_public(C, #doctype{public = P} = Curr) ->
   Curr#doctype{public = <<P/binary, C/utf8>>}.

append_to_system(C, #doctype{system = P} = Curr) ->
   Curr#doctype{system = <<P/binary, C/utf8>>}.
   
set_self_closing(Curr) ->
   Curr#start_tag{closing = true}.


%% add_template_insertion_mode(Mode, #{template_ins_modes := Modes} = State) ->
%%    State#{template_ins_modes := [Mode|Modes]}.
%% 
%% get_template_insertion_mode(#{template_ins_modes := [M|Modes]}) -> M;
%% get_template_insertion_mode(#{template_ins_modes := []}) -> undefined.
%% 
%% pop_template_insertion_mode(#{template_ins_modes := [_|Ms]} = State) -> 
%%    State#{template_ins_modes := Ms};
%% pop_template_insertion_mode(State) ->
%%    State.

add_open_element(Element, #{open_elements := Els} = State) ->
   State#{open_elements := [Element|Els]}.

get_current_node(#{open_elements := [E|_]}) -> E.

is_open(Name, #{open_elements := Els}) ->
   Pred = fun(#start_tag{name = N}) ->
                N == Name;
             (_) -> false
          end,                
   lists:any(Pred, Els).

pop_all(State) ->
   State1 = maybe_pop_text(State),
   pop_all_to_tag_(none, State1).

pop_all_to_tag(Name, State) ->
   case is_open(Name, State) of
      false ->
         % parse error
         State;
      true ->
         pop_all_to_tag_(Name, State)
   end.

pop_all_to_tag_(Name, #{open_elements := [#start_tag{name = Name}|Els]} = State) ->
   State1 = do_pop_tag(Name, State),
   State1#{open_elements := Els};
pop_all_to_tag_(OName, #{open_elements := [#start_tag{name = Name}|Els]} = State) ->
   State1 = do_pop_tag(Name, State),
   pop_all_to_tag_(OName, State1#{open_elements := Els});
pop_all_to_tag_(_, #{open_elements := []} = State) ->
   State.

do_pop_tag({svg, <<"svg">>}, #{inscope_namespace := [_|Ns]} = State) ->
   State1 = send_event({endElement, ?SVG, <<"svg">>, {<<>>, <<"svg">>}}, State),
   send_event({endPrefixMapping, <<>>}, State1#{inscope_namespace := Ns});
do_pop_tag({mathml, <<"math">>}, #{inscope_namespace := [_|Ns]} = State) ->
   State1 = send_event({endElement, ?MATH, <<"math">>, {<<>>, <<"math">>}}, State),
   send_event({endPrefixMapping, <<>>}, State1#{inscope_namespace := Ns});
do_pop_tag(<<"html">>, #{inscope_namespace := [html]} = State) ->
   State1 = send_event({endElement, ?HTML, <<"html">>, {<<>>, <<"html">>}}, State),
   send_event({endPrefixMapping, <<>>}, State1#{inscope_namespace := []});
do_pop_tag({svg, L}, #{inscope_namespace := [_|Ns]} = State) ->
   S = u(L),
   send_event({endElement, ?SVG, S, {<<>>, S}}, State#{inscope_namespace := Ns});
do_pop_tag({mathml, L}, #{inscope_namespace := [_|Ns]} = State) ->
   S = u(L),
   send_event({endElement, ?MATH, S, {<<>>, S}}, State#{inscope_namespace := Ns});
do_pop_tag(L, State) ->
   S = u(L),
   send_event({endElement, ?HTML, S, {<<>>, S}}, State).
   
u(undefined) -> <<>>;
u(Bin) -> Bin.

%% u(undefined) -> [];
%% u(Bin) -> unicode:characters_to_list(Bin).

generate_implied_end_tags(#{open_elements := [#start_tag{name = Nm}|Els]} = State) 
   when Nm == <<"dd">>; Nm == <<"dt">>; Nm == <<"li">>;
        Nm == <<"optgroup">>; Nm == <<"option">>;
        Nm == <<"p">>; Nm == <<"rb">>; Nm == <<"rp">>;
        Nm == <<"rt">>; Nm == <<"rtc">> ->
   SNm = u(Nm),
   End = {endElement, ?HTML, SNm, {<<>>, SNm}},
   State1 = send_event(End, State#{open_elements := Els}),
   generate_implied_end_tags(State1).

generate_implied_end_tags(#{open_elements := [#start_tag{name = Nm}|Els]} = State, Except) 
   when Nm =/= Except andalso 
          (Nm == <<"dd">> orelse Nm == <<"dt">> orelse Nm == <<"li">> orelse 
             Nm == <<"optgroup">> orelse Nm == <<"option">> orelse Nm == <<"p">> orelse 
             Nm == <<"rb">> orelse Nm == <<"rp">> orelse Nm == <<"rt">> orelse Nm == <<"rtc">>) ->
   SNm = u(Nm),
   End = {endElement, ?HTML, SNm, {<<>>, SNm}},
   State1 = send_event(End, State#{open_elements := Els}),
   generate_implied_end_tags(State1).


send_event(Event, #{%insertion_mode := M,
                    user_event_fun := UFun,
                    user_event_state := UState,
                    line_num := LineNum} = State) ->
   %io:format("~p~n", [M]),
   UState1 = UFun(Event, LineNum, UState),
   State#{user_event_state := UState1}.

adjust_att_name(Name) ->
   case htmerl_util:adjusted_attribute_name(Name) of
      {xlink, L} ->
         {?XLINK, <<"xlink">>, L};
      {xml, L} ->
         {?XML, <<"xml">>, L};
      {xmlns, <<>>} ->
         {?XMLNS, <<>>, <<"xmlns">>};
      {xmlns, L} ->
         {?XMLNS, <<"xmlns">>, L};
      {mathml, L} ->
         {?MATH, <<>>, L};
      {svg, L} ->
         {?SVG, <<>>, L};
      {_, L} ->
         {<<>>, <<>>, L}
   end.

add_text_char(C, #{text_node_buff := undefined} = State) ->
   State#{text_node_buff := <<C/utf8>>};
add_text_char(C, #{text_node_buff := Buff} = State) ->
   State#{text_node_buff := <<Buff/binary, C/utf8>>}.

add_text_chars(C, #{text_node_buff := undefined} = State) ->
   State#{text_node_buff := C};
add_text_chars(C, #{text_node_buff := Buff} = State) ->
   State#{text_node_buff := <<Buff/binary, C/binary>>}.


maybe_pop_text(#{text_node_buff := undefined} = State) ->
   State;
maybe_pop_text(#{text_node_buff := Buff} = State) ->
   TestFun = fun(X) -> {start_tag, Y, _, _} = X, Y =:= <<"pre">> end,
   HasPre = lists:any(TestFun, maps:get(open_elements, State)),
   if
      HasPre -> 
         Event = {characters, u(Buff)},
         State1 = send_event(Event, State),
         State1#{text_node_buff := undefined};
      true ->
        Buff1 = norm_whitespaces(Buff),
        Event = {characters, u(Buff1)},
        State1 = send_event(Event, State),
        State1#{text_node_buff := undefined}
   end.

add_html_element(#start_tag{name = N, 
                            attributes = Atts,
                            closing = Closing} = Token, State1) ->
   Atts1 = [begin
               {U, P, L} = adjust_att_name({html, AName}),
               {U, P, u(L), AValue}
            end 
           || #attribute{name = AName, value = AValue} <- Atts,
              AName =/= <<"xmlns">>],
   U = u(N),
   Elem = {startElement, ?HTML, U, {<<>>, U}, Atts1},
   State2 = send_event(Elem, State1),
   case Closing of
      false ->
         add_open_element(Token,  State2);
      true ->
         EndElem = {endElement, ?HTML, U, {<<>>, U}},
         send_event(EndElem, State2)
   end.

add_math_element(#start_tag{name = N,
                            closing = Closing, 
                            attributes = Atts} = Token, 
                 #{inscope_namespace := Ns} = State1) ->
   Atts1 = [begin
               {U, P, L} = adjust_att_name({mathml, AName}),
               {U, P, u(L), AValue}
            end 
           || #attribute{name = AName, value = AValue} <- Atts,
              AName =/= <<"xmlns">>],
   U = u(N),
   Elem = {startElement, ?MATH, U, {<<>>, U}, Atts1},
   State2 = send_event(Elem, State1),
   case Closing of
      false ->
         add_open_element(Token#start_tag{name = {mathml, N}},  State2#{inscope_namespace := [mathml|Ns]});
      true ->
         EndElem = {endElement, ?MATH, U, {<<>>, U}},
         send_event(EndElem, State2)
   end.

add_svg_element(#start_tag{name = N,
                           closing = Closing, 
                           attributes = Atts} = Token, 
                #{inscope_namespace := Ns} = State1) ->
   Atts1 = [begin
               {U, P, L} = adjust_att_name({svg, AName}),
               {U, P, L, AValue}
            end 
           || #attribute{name = AName, value = AValue} <- Atts,
              AName =/= <<"xmlns">>],
   Elem = {startElement, ?SVG, N, {<<>>, N}, Atts1},
   State2 = send_event(Elem, State1),
   case Closing of
      false ->
         add_open_element(Token#start_tag{name = {svg, N}},  State2#{inscope_namespace := [svg|Ns]});
      true ->
         EndElem = {endElement, ?SVG, N, {<<>>, N}},
         send_event(EndElem, State2)
   end.
   

finish_document(State) ->
   %% TODO send warnings out
   #{user_event_state := UState} = send_event(endDocument, State),
   {ok, UState, []}.

%% TODO here add parse error and fatal error formatters that add to state.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


count_newlines(Bin, Counter, NlCp) ->
   case length(binary:matches(Bin, NlCp,[])) of
      0 -> Counter;
      Cnt -> Counter + Cnt
   end.

count_newlines(Bin, Len, Counter, NlCp) ->
   case length(binary:matches(Bin, NlCp,[{scope, {0, Len}}])) of
      0 -> Counter;
      Cnt -> Counter + Cnt
   end.

find_stop(Bin, Stops, Counter, NlCp) ->
   case binary:match(Bin, Stops, []) of
      nomatch ->
         {byte_size(Bin), count_newlines(Bin, Counter, NlCp)};
      {Pos, _} ->
         {Pos, count_newlines(Bin, Pos, Counter, NlCp)}
   end.

call_data_function(#start_tag{name = <<"textarea">>}, Rest, State) ->
   rcdata(Rest, State);
call_data_function(#start_tag{name = <<"noframes">>}, Rest, State) ->
   rawtext(Rest, State);
call_data_function(#start_tag{name = <<"style">>}, Rest, State) ->
   rawtext(Rest, State);
call_data_function(#start_tag{name = <<"script">>}, Rest, State) ->
   script_data(Rest, State);
call_data_function(#start_tag{name = <<"plaintext">>}, Rest, State) ->
   plaintext(Rest, State);
call_data_function(_, Rest, State) ->
   data(Rest, State).
   
