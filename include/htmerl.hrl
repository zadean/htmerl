%% Attribute
-record(htmlAttribute,
   {
    name             :: binary(),
    prefix    = <<>> :: binary(),
    namespace = <<>> :: binary(),
    value     = <<>> :: binary()
   }).

%% Tag
-record(htmlElement,
   {
    name            :: binary(),
    namespace       :: binary(),
    attributes = [] :: [#htmlAttribute{}],
    content    = []
   }).

%% Text
-record(htmlText,
   {
    value = <<>> :: binary(),
    type  = text :: text | cdata
   }).

%% Comment
-record(htmlComment,
   {
    value = <<>> :: binary()
   }).

-record(htmlDocument, 
   {
    name    = <<"html">>,
    public,
    system,
    content = []
   }).