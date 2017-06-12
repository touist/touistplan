type token =
  | DEFINE
  | DOMAIN
  | REQUIREMENTS
  | CONSTANTS
  | TYPES
  | FUNCTIONS
  | ACTION
  | DURATIVE_ACTION
  | PARAM
  | DURATION
  | AT
  | BEFORE
  | AFTER
  | START
  | END
  | OVER
  | ALL
  | SOMEWHERE
  | ANYWHERE
  | MIN_DUR
  | ASSIGN
  | INCREASE
  | DECREASE
  | CONSUME
  | PRODUCE
  | QUALITY
  | PREC
  | EFFECT
  | NOT
  | AND
  | TYPE
  | LP
  | RP
  | EQUAL
  | ADD
  | MULTIPLY
  | DIVIDE
  | LH
  | RH
  | INF
  | SUP
  | PROBLEM
  | PDOMAIN
  | OBJECTS
  | INIT
  | GOAL
  | METRIC
  | MINIMIZE
  | TOTALTIME
  | TOTALCOST
  | VAR of (string)
  | IDENT of (string)
  | REQUIREMENT of (string)
  | INTEGER of (int)
  | RATIONAL of (float)
  | CONSTRAINTS
  | CDOMAIN
  | NECESSARLYBEFORE
  | POSSIBLYBEFORE
  | EVENTUALLYLEADSTO
  | IMMEDIATLYLEADSTO
  | FILL
  | CHOICE
  | PARALLEL

open Parsing;;
let _ = parse_error;;
# 2 "Sources/parser.mly"
  let action_relative_end_time = ref (FunctionFormula.Number 1.0)

  let current_time_set = ref ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))

  let current_time_bound = ref (true,true)

  let ti_code = ref 0

  let ti_min_dur = ref 0.0

  let functions_value_list = ref []

  (*let parse_error s = Utils.eprint " Syntax error at line %i\n\n" !Lexer.line*)

  let symb_set = new SymbSet.t
  let attribute_spaces = new Typeset.attribute_space_set symb_set

  let domain = ref Domain.domain_void

  let constants_in_domain = ref []

  let create_domain name requirements operators =
    (*Lexer.line := 1 ;*)
    Utils.eprint "%s..... " name ;
    domain := new Domain.domain name requirements (Array.of_list operators) ;
    !domain

  let create_operator name params descr =
    symb_set#reset_var_table ;
    let (duration, quality, prec, eff) = descr in
    let parameters =  new Typeset.parameters params attribute_spaces in
      new Domain.operator (String.uppercase name) parameters duration quality prec eff

  let create_problem name domain_name objects init goal =
    Utils.eprint "%s..... " name ;
    if domain_name <> !domain#name then begin
      Utils.eprint "\n\nProblem %s not for domain %s !\n\n" name !domain#name ;
      exit 0 
    end ;
    new Domain.problem name !domain objects init goal symb_set attribute_spaces !functions_value_list
  
  
  let create_constraints name domain_name constraints_t =
    Utils.eprint "%s..... " name ;
    if domain_name <> !domain#name then begin
      Utils.eprint "\n\Constraints %s not for domain %s !\n\n" name !domain#name ;
      exit 0 
    end ;
    new Domain.constraints name !domain constraints_t


  let create_atom pred terms timeset =
    let pred = symb_set#create_predicate pred in
      if !Lexer.is_effect then pred#untype ;
      let timedata = ref (new Timedata.t timeset !ti_code) in
      (!timedata)#set_closed_left (fst !current_time_bound);
      (!timedata)#set_closed_right (snd !current_time_bound);
      (!timedata)#set_min_dur !ti_min_dur;
      new Atom.t pred (Array.of_list terms) !timedata timeset

  let create_preatom pred terms timeset =
    let pred = symb_set#create_predicate pred in
      if !Lexer.is_effect then pred#untype ;
      let timedata = ref (new Timedata.t timeset !ti_code) in
      (!timedata)#set_closed_left (fst !current_time_bound);
      (!timedata)#set_closed_right (snd !current_time_bound);
      (!timedata)#set_min_dur !ti_min_dur;
      (pred,(Array.of_list terms),!timedata)

  let create_typed_term_list variables pred =
    List.iter (fun t -> t#set_builtin_type (symb_set#create_predicate pred)) variables ;
    variables

  let set_action_relative_end_time dur =
    action_relative_end_time := dur ;
    dur

  let set_current_time_set timeset =
    current_time_set := timeset ;
    timeset

  let formula_time formulas time_set =
   List.map (fun p -> (p, time_set)) formulas

  let create_function name var_list =
   (name,var_list)

# 158 "Sources/parser.ml"
let yytransl_const = [|
  257 (* DEFINE *);
  258 (* DOMAIN *);
  259 (* REQUIREMENTS *);
  260 (* CONSTANTS *);
  261 (* TYPES *);
  262 (* FUNCTIONS *);
  263 (* ACTION *);
  264 (* DURATIVE_ACTION *);
  265 (* PARAM *);
  266 (* DURATION *);
  267 (* AT *);
  268 (* BEFORE *);
  269 (* AFTER *);
  270 (* START *);
  271 (* END *);
  272 (* OVER *);
  273 (* ALL *);
  274 (* SOMEWHERE *);
  275 (* ANYWHERE *);
  276 (* MIN_DUR *);
  277 (* ASSIGN *);
  278 (* INCREASE *);
  279 (* DECREASE *);
  280 (* CONSUME *);
  281 (* PRODUCE *);
  282 (* QUALITY *);
  283 (* PREC *);
  284 (* EFFECT *);
  285 (* NOT *);
  286 (* AND *);
  287 (* TYPE *);
  288 (* LP *);
  289 (* RP *);
  290 (* EQUAL *);
  291 (* ADD *);
  292 (* MULTIPLY *);
  293 (* DIVIDE *);
  294 (* LH *);
  295 (* RH *);
  296 (* INF *);
  297 (* SUP *);
  298 (* PROBLEM *);
  299 (* PDOMAIN *);
  300 (* OBJECTS *);
  301 (* INIT *);
  302 (* GOAL *);
  303 (* METRIC *);
  304 (* MINIMIZE *);
  305 (* TOTALTIME *);
  306 (* TOTALCOST *);
  312 (* CONSTRAINTS *);
  313 (* CDOMAIN *);
  314 (* NECESSARLYBEFORE *);
  315 (* POSSIBLYBEFORE *);
  316 (* EVENTUALLYLEADSTO *);
  317 (* IMMEDIATLYLEADSTO *);
  318 (* FILL *);
  319 (* CHOICE *);
  320 (* PARALLEL *);
    0|]

let yytransl_block = [|
  307 (* VAR *);
  308 (* IDENT *);
  309 (* REQUIREMENT *);
  310 (* INTEGER *);
  311 (* RATIONAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\004\000\
\004\000\007\000\007\000\007\000\009\000\009\000\009\000\011\000\
\011\000\011\000\011\000\015\000\015\000\016\000\018\000\013\000\
\013\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\014\000\002\000\002\000\002\000\021\000\021\000\022\000\022\000\
\003\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\026\000\026\000\026\000\017\000\017\000\017\000\017\000\
\017\000\017\000\027\000\027\000\027\000\038\000\038\000\040\000\
\040\000\024\000\024\000\041\000\041\000\025\000\025\000\025\000\
\025\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\042\000\042\000\042\000\045\000\046\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\047\000\047\000\047\000\047\000\047\000\048\000\048\000\048\000\
\048\000\048\000\005\000\005\000\050\000\050\000\049\000\049\000\
\006\000\006\000\051\000\051\000\010\000\010\000\053\000\053\000\
\008\000\008\000\044\000\044\000\055\000\055\000\043\000\043\000\
\012\000\012\000\023\000\023\000\056\000\056\000\056\000\054\000\
\052\000\052\000\020\000\020\000\000\000\000\000"

let yylen = "\002\000\
\016\000\019\000\013\000\016\000\010\000\010\000\007\000\001\000\
\002\000\001\000\004\000\004\000\006\000\006\000\003\000\003\000\
\002\000\002\000\001\000\004\000\002\000\001\000\001\000\002\000\
\006\000\001\000\005\000\005\000\005\000\005\000\003\000\003\000\
\002\000\015\000\015\000\012\000\003\000\003\000\005\000\005\000\
\011\000\001\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\005\000\005\000\006\000\005\000\005\000\005\000\
\005\000\001\000\007\000\007\000\003\000\006\000\008\000\005\000\
\005\000\002\000\003\000\005\000\002\000\001\000\002\000\001\000\
\002\000\001\000\003\000\001\000\002\000\001\000\003\000\002\000\
\007\000\002\000\002\000\002\000\006\000\006\000\005\000\005\000\
\005\000\005\000\005\000\005\000\004\000\004\000\005\000\005\000\
\002\000\002\000\002\000\002\000\002\000\002\000\004\000\004\000\
\001\000\001\000\001\000\005\000\005\000\001\000\004\000\004\000\
\004\000\004\000\001\000\003\000\001\000\002\000\001\000\002\000\
\003\000\003\000\001\000\002\000\003\000\003\000\001\000\002\000\
\001\000\003\000\002\000\002\000\002\000\002\000\001\000\002\000\
\001\000\002\000\001\000\002\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\149\000\000\000\150\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\117\000\119\000\000\000\
\000\000\115\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\005\000\120\000\118\000\000\000\006\000\000\000\000\000\
\000\000\000\000\022\000\023\000\000\000\000\000\000\000\019\000\
\000\000\000\000\011\000\012\000\000\000\000\000\116\000\000\000\
\000\000\147\000\148\000\024\000\026\000\033\000\015\000\000\000\
\017\000\018\000\000\000\000\000\021\000\000\000\000\000\000\000\
\127\000\137\000\144\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\066\000\
\000\000\000\000\000\000\000\000\000\000\036\000\000\000\003\000\
\000\000\000\000\000\000\138\000\128\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\032\000\105\000\106\000\000\000\
\135\000\141\000\142\000\000\000\107\000\084\000\097\000\000\000\
\098\000\099\000\110\000\000\000\000\000\100\000\101\000\102\000\
\000\000\000\000\000\000\000\000\000\000\000\000\070\000\000\000\
\061\000\000\000\143\000\083\000\000\000\000\000\000\000\082\000\
\000\000\020\000\123\000\139\000\145\000\146\000\000\000\000\000\
\000\000\000\000\000\000\074\000\037\000\038\000\000\000\000\000\
\000\000\000\000\013\000\014\000\125\000\126\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\136\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\071\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\140\000\124\000\000\000\000\000\
\000\000\000\000\078\000\080\000\000\000\000\000\000\000\000\000\
\000\000\000\000\129\000\000\000\028\000\025\000\027\000\029\000\
\030\000\000\000\000\000\000\000\000\000\103\000\131\000\132\000\
\000\000\000\000\000\000\064\000\000\000\072\000\000\000\000\000\
\104\000\000\000\000\000\000\000\000\000\065\000\034\000\035\000\
\121\000\122\000\000\000\075\000\079\000\000\000\000\000\000\000\
\000\000\000\000\000\000\066\000\000\000\000\000\000\000\001\000\
\000\000\004\000\000\000\000\000\112\000\111\000\114\000\113\000\
\089\000\090\000\091\000\000\000\000\000\000\000\000\000\073\000\
\062\000\087\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\093\000\094\000\000\000\000\000\070\000\067\000\000\000\
\058\000\039\000\040\000\000\000\130\000\109\000\108\000\000\000\
\000\000\069\000\085\000\086\000\000\000\000\000\000\000\000\000\
\092\000\000\000\000\000\000\000\000\000\000\000\063\000\000\000\
\000\000\000\000\064\000\000\000\002\000\000\000\000\000\081\000\
\096\000\095\000\000\000\068\000\000\000\000\000\000\000\000\000\
\059\000\060\000"

let yydgoto = "\003\000\
\005\000\007\000\000\000\029\000\033\000\175\000\020\000\236\000\
\037\000\084\000\053\000\085\000\054\000\055\000\056\000\057\000\
\160\000\058\000\068\000\141\000\079\000\118\000\176\000\181\000\
\013\001\058\001\255\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\112\000\161\000\113\000\055\001\
\000\000\183\000\142\000\204\000\015\001\016\001\143\000\150\000\
\034\000\035\000\177\000\233\000\086\000\087\000\000\000\144\000"

let yysindex = "\161\000\
\003\255\017\255\000\000\070\255\000\000\075\255\000\000\080\255\
\099\255\120\255\061\255\083\255\092\255\104\255\118\255\245\255\
\125\255\081\000\000\000\000\000\126\255\006\255\182\255\123\255\
\123\255\130\255\000\000\006\255\058\000\000\000\000\000\182\255\
\063\000\000\000\145\255\055\000\063\000\063\000\139\255\000\000\
\238\255\000\000\000\000\000\000\085\000\000\000\182\255\172\255\
\073\255\154\255\000\000\000\000\178\255\229\255\070\000\000\000\
\184\255\184\255\000\000\000\000\192\255\182\255\000\000\001\255\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\070\000\
\000\000\000\000\165\255\186\255\000\000\056\000\203\255\071\000\
\000\000\000\000\000\000\026\000\026\000\197\255\001\255\110\255\
\191\255\110\255\110\255\110\255\001\255\000\000\138\255\046\255\
\046\255\085\255\085\255\085\255\051\000\220\255\226\255\228\255\
\249\255\075\000\233\255\158\255\004\255\235\255\158\255\000\000\
\009\000\184\255\056\255\077\000\011\000\000\000\194\255\000\000\
\038\000\047\000\237\255\000\000\000\000\010\255\110\255\110\255\
\110\255\110\255\110\255\000\000\000\000\000\000\000\000\101\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\158\255\
\000\000\000\000\000\000\046\255\046\255\000\000\000\000\000\000\
\036\000\023\000\023\000\023\000\022\000\165\255\000\000\079\000\
\000\000\069\000\000\000\000\000\023\000\067\000\092\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\107\000\107\000\
\035\000\056\255\127\255\000\000\000\000\000\000\082\000\110\000\
\067\255\084\000\000\000\000\000\000\000\000\000\053\255\080\000\
\111\000\112\000\113\000\114\000\046\255\046\255\000\000\046\255\
\046\255\085\255\001\255\048\000\094\000\095\000\158\255\117\000\
\086\000\000\000\085\255\098\000\023\000\023\000\119\000\120\000\
\109\000\203\255\203\255\057\255\000\000\000\000\007\000\077\000\
\123\000\127\255\000\000\000\000\207\255\124\000\125\000\088\000\
\067\255\023\000\000\000\063\000\000\000\000\000\000\000\000\000\
\000\000\046\255\046\255\087\000\089\000\000\000\000\000\000\000\
\126\000\127\000\128\000\000\000\210\255\000\000\090\000\129\000\
\000\000\130\000\102\000\115\000\022\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\242\255\131\000\132\000\
\082\000\134\000\097\000\000\000\099\000\099\000\060\000\000\000\
\084\000\000\000\135\000\137\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\139\000\090\000\120\000\250\255\000\000\
\000\000\000\000\140\000\141\000\142\000\051\000\144\000\145\000\
\146\000\000\000\000\000\022\000\207\255\000\000\000\000\096\000\
\000\000\000\000\000\000\084\000\000\000\000\000\000\000\022\000\
\148\000\000\000\000\000\000\000\149\000\150\000\022\000\022\000\
\000\000\151\000\133\000\063\000\152\000\022\000\000\000\082\000\
\153\000\154\000\000\000\156\000\000\000\149\000\157\000\000\000\
\000\000\000\000\091\000\000\000\158\000\159\000\160\000\162\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\164\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\165\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\139\001\030\000\100\255\227\255\008\255\
\147\001\183\255\049\001\179\255\000\000\124\001\232\255\000\000\
\205\255\113\001\222\255\208\255\216\000\175\000\016\001\230\000\
\142\255\177\000\017\001\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\102\255\040\001\103\255\071\255\
\000\000\000\000\167\255\128\255\000\000\000\000\173\255\174\255\
\170\001\171\001\090\255\152\255\190\255\156\255\000\000\000\000"

let yytablesize = 459
let yytable = "\042\000\
\069\000\182\000\208\000\046\000\209\000\076\000\077\000\059\000\
\060\000\124\000\178\000\222\000\145\000\146\000\216\000\133\000\
\151\000\152\000\164\000\132\000\125\000\168\000\191\000\000\001\
\224\000\225\000\205\000\206\000\232\000\073\000\074\000\081\000\
\061\001\082\000\004\000\165\000\212\000\166\000\027\000\069\000\
\088\000\069\000\069\000\069\000\090\000\091\000\092\000\094\000\
\006\000\190\000\120\000\083\000\153\000\127\000\199\000\129\000\
\130\000\131\000\028\000\134\000\135\000\093\000\170\000\010\001\
\200\000\201\000\222\000\076\001\228\000\040\001\008\000\017\001\
\225\000\178\000\020\001\009\000\063\000\136\000\069\000\069\000\
\069\000\069\000\069\000\081\000\003\001\004\001\171\000\171\000\
\172\000\009\001\191\000\080\000\192\000\193\000\194\000\195\000\
\196\000\171\000\038\001\066\000\067\000\147\000\013\000\083\000\
\065\000\025\001\045\001\173\000\173\000\174\000\174\000\010\000\
\046\001\242\000\243\000\049\001\244\000\245\000\173\000\246\000\
\174\000\012\000\148\000\149\000\125\000\248\000\066\000\067\000\
\001\001\247\000\011\000\197\000\230\000\164\000\014\000\198\000\
\016\000\095\000\096\000\097\000\066\001\126\000\098\000\015\000\
\099\000\100\000\101\000\102\000\103\000\104\000\017\000\134\000\
\135\000\074\001\020\001\209\000\021\000\107\000\027\001\028\001\
\223\000\001\000\002\000\066\000\067\000\077\001\109\000\110\000\
\026\000\136\000\137\000\061\000\081\001\082\001\036\000\095\000\
\096\000\097\000\111\000\087\001\098\000\039\000\099\000\100\000\
\101\000\102\000\103\000\104\000\138\000\139\000\137\000\140\000\
\067\000\105\000\106\000\107\000\047\000\185\000\108\000\186\000\
\024\000\025\000\024\001\064\000\109\000\110\000\026\001\070\000\
\138\000\139\000\071\000\163\000\030\000\052\000\031\000\075\000\
\111\000\095\000\096\000\097\000\207\000\088\001\098\000\078\000\
\099\000\100\000\101\000\102\000\103\000\104\000\102\000\103\000\
\104\000\032\000\117\000\018\001\019\001\107\000\036\001\037\001\
\108\000\128\000\062\000\108\000\024\000\025\000\109\000\110\000\
\123\000\109\000\110\000\154\000\207\000\070\001\050\000\051\000\
\052\000\155\000\111\000\156\000\207\000\111\000\102\000\103\000\
\104\000\047\001\048\001\081\000\167\000\189\000\102\000\103\000\
\104\000\162\000\207\000\108\000\018\000\019\000\065\001\037\001\
\157\000\109\000\110\000\108\000\102\000\103\000\104\000\083\000\
\207\000\109\000\110\000\049\000\215\000\111\000\011\001\137\000\
\169\000\108\000\102\000\103\000\104\000\111\000\085\001\109\000\
\110\000\147\000\143\000\050\000\051\000\052\000\202\000\108\000\
\184\000\138\000\139\000\111\000\163\000\109\000\110\000\048\000\
\049\000\060\001\024\000\025\000\143\000\143\000\187\000\143\000\
\088\000\111\000\203\000\089\000\090\000\091\000\092\000\188\000\
\050\000\051\000\052\000\022\000\211\000\023\000\220\000\024\000\
\025\000\041\000\019\000\024\000\025\000\093\000\045\000\019\000\
\051\000\052\000\213\000\115\000\116\000\249\000\119\000\019\000\
\066\000\067\000\158\000\159\000\179\000\180\000\075\000\159\000\
\237\000\226\000\227\000\234\000\235\000\253\000\254\000\023\001\
\019\000\039\001\254\000\214\000\029\001\030\001\031\001\032\001\
\053\001\054\001\056\001\057\001\121\000\122\000\218\000\219\000\
\007\001\008\001\217\000\093\001\094\001\229\000\075\001\238\000\
\239\000\240\000\241\000\250\000\251\000\252\000\005\001\002\001\
\006\001\116\000\014\001\043\001\021\001\022\001\033\001\034\001\
\035\001\041\001\042\001\050\001\051\001\052\001\040\000\062\001\
\044\001\063\001\064\001\038\000\067\001\068\001\069\001\071\001\
\072\001\072\000\073\001\078\001\084\001\079\001\080\001\083\001\
\086\001\089\001\090\001\091\001\114\000\092\001\095\001\096\001\
\097\001\221\000\098\001\069\000\068\000\012\001\059\001\210\000\
\231\000\043\000\044\000"

let yycheck = "\029\000\
\049\000\116\000\157\000\033\000\158\000\057\000\058\000\037\000\
\038\000\087\000\115\000\178\000\096\000\097\000\169\000\093\000\
\099\000\100\000\108\000\093\000\087\000\111\000\123\000\209\000\
\179\000\179\000\155\000\156\000\185\000\054\000\055\000\031\001\
\025\001\033\001\032\001\032\001\165\000\034\001\033\001\088\000\
\031\001\090\000\091\000\092\000\035\001\036\001\037\001\072\000\
\032\001\123\000\080\000\051\001\101\000\088\000\144\000\090\000\
\091\000\092\000\053\001\014\001\015\001\052\001\114\000\220\000\
\148\000\149\000\233\000\060\001\183\000\255\000\001\001\226\000\
\226\000\178\000\229\000\001\001\047\000\032\001\127\000\128\000\
\129\000\130\000\131\000\031\001\213\000\214\000\031\001\031\001\
\033\001\033\001\191\000\062\000\127\000\128\000\129\000\130\000\
\131\000\031\001\253\000\054\001\055\001\017\001\042\001\051\001\
\032\001\234\000\005\001\052\001\052\001\054\001\054\001\032\001\
\011\001\197\000\198\000\014\001\200\000\201\000\052\001\202\000\
\054\001\002\001\038\001\039\001\191\000\203\000\054\001\055\001\
\211\000\203\000\032\001\031\001\184\000\223\000\052\001\035\001\
\033\001\011\001\012\001\013\001\039\001\032\001\016\001\052\001\
\018\001\019\001\020\001\021\001\022\001\023\001\033\001\014\001\
\015\001\052\001\053\001\053\001\032\001\031\001\242\000\243\000\
\034\001\001\000\002\000\054\001\055\001\064\001\040\001\041\001\
\043\001\032\001\033\001\033\001\071\001\072\001\052\001\011\001\
\012\001\013\001\052\001\078\001\016\001\052\001\018\001\019\001\
\020\001\021\001\022\001\023\001\051\001\052\001\033\001\054\001\
\055\001\029\001\030\001\031\001\052\001\004\001\034\001\006\001\
\007\001\008\001\232\000\032\001\040\001\041\001\236\000\054\001\
\051\001\052\001\033\001\054\001\031\001\028\001\033\001\032\001\
\052\001\011\001\012\001\013\001\011\001\080\001\016\001\032\001\
\018\001\019\001\020\001\021\001\022\001\023\001\021\001\022\001\
\023\001\052\001\032\001\029\001\030\001\031\001\029\001\030\001\
\034\001\051\001\005\001\034\001\007\001\008\001\040\001\041\001\
\052\001\040\001\041\001\032\001\011\001\046\001\026\001\027\001\
\028\001\032\001\052\001\032\001\011\001\052\001\021\001\022\001\
\023\001\024\001\025\001\031\001\034\001\033\001\021\001\022\001\
\023\001\041\001\011\001\034\001\032\001\033\001\029\001\030\001\
\032\001\040\001\041\001\034\001\021\001\022\001\023\001\051\001\
\011\001\040\001\041\001\010\001\029\001\052\001\032\001\033\001\
\032\001\034\001\021\001\022\001\023\001\052\001\076\001\040\001\
\041\001\032\001\033\001\026\001\027\001\028\001\019\001\034\001\
\046\001\051\001\052\001\052\001\054\001\040\001\041\001\009\001\
\010\001\006\001\007\001\008\001\051\001\052\001\033\001\054\001\
\031\001\052\001\052\001\034\001\035\001\036\001\037\001\033\001\
\026\001\027\001\028\001\003\001\016\001\005\001\052\001\007\001\
\008\001\032\001\033\001\007\001\008\001\052\001\032\001\033\001\
\027\001\028\001\032\001\044\001\045\001\054\001\032\001\033\001\
\054\001\055\001\032\001\033\001\032\001\033\001\032\001\033\001\
\033\001\032\001\033\001\032\001\033\001\032\001\033\001\032\001\
\033\001\032\001\033\001\032\001\038\001\039\001\038\001\039\001\
\032\001\033\001\032\001\033\001\084\000\085\000\175\000\176\000\
\218\000\219\000\032\001\049\001\050\001\032\001\047\001\033\001\
\033\001\033\001\033\001\054\001\054\001\033\001\032\001\054\001\
\033\001\045\001\032\001\054\001\033\001\033\001\033\001\033\001\
\033\001\033\001\033\001\033\001\033\001\032\001\028\000\033\001\
\054\001\033\001\032\001\025\000\033\001\033\001\033\001\032\001\
\032\001\054\000\033\001\032\001\048\001\033\001\033\001\033\001\
\033\001\033\001\033\001\032\001\076\000\033\001\033\001\033\001\
\033\001\178\000\033\001\032\001\032\001\224\000\022\001\160\000\
\184\000\032\000\032\000"

let yynames_const = "\
  DEFINE\000\
  DOMAIN\000\
  REQUIREMENTS\000\
  CONSTANTS\000\
  TYPES\000\
  FUNCTIONS\000\
  ACTION\000\
  DURATIVE_ACTION\000\
  PARAM\000\
  DURATION\000\
  AT\000\
  BEFORE\000\
  AFTER\000\
  START\000\
  END\000\
  OVER\000\
  ALL\000\
  SOMEWHERE\000\
  ANYWHERE\000\
  MIN_DUR\000\
  ASSIGN\000\
  INCREASE\000\
  DECREASE\000\
  CONSUME\000\
  PRODUCE\000\
  QUALITY\000\
  PREC\000\
  EFFECT\000\
  NOT\000\
  AND\000\
  TYPE\000\
  LP\000\
  RP\000\
  EQUAL\000\
  ADD\000\
  MULTIPLY\000\
  DIVIDE\000\
  LH\000\
  RH\000\
  INF\000\
  SUP\000\
  PROBLEM\000\
  PDOMAIN\000\
  OBJECTS\000\
  INIT\000\
  GOAL\000\
  METRIC\000\
  MINIMIZE\000\
  TOTALTIME\000\
  TOTALCOST\000\
  CONSTRAINTS\000\
  CDOMAIN\000\
  NECESSARLYBEFORE\000\
  POSSIBLYBEFORE\000\
  EVENTUALLYLEADSTO\000\
  IMMEDIATLYLEADSTO\000\
  FILL\000\
  CHOICE\000\
  PARALLEL\000\
  "

let yynames_block = "\
  VAR\000\
  IDENT\000\
  REQUIREMENT\000\
  INTEGER\000\
  RATIONAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 11 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 7 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 4 : 'types) in
    let _15 = (Parsing.peek_val __caml_parser_env 1 : 'typed_constant_list) in
    let _16 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 106 "Sources/parser.mly"
                                                                                                                          ( constants_in_domain := _15; create_domain _5 _9 _16 )
# 632 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 14 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 10 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 7 : 'types) in
    let _15 = (Parsing.peek_val __caml_parser_env 4 : 'typed_constant_list) in
    let _18 = (Parsing.peek_val __caml_parser_env 1 : 'functions_list) in
    let _19 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 107 "Sources/parser.mly"
                                                                                                                                                        ( constants_in_domain := _15; create_domain _5 _9 _19 )
# 644 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 4 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    let _13 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 109 "Sources/parser.mly"
                                                                                         ( create_domain _5 _9 _13 )
# 654 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 11 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 7 : 'requirements) in
    let _12 = (Parsing.peek_val __caml_parser_env 4 : 'types) in
    let _15 = (Parsing.peek_val __caml_parser_env 1 : 'functions_list) in
    let _16 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 110 "Sources/parser.mly"
                                                                                                                       ( create_domain _5 _9 _16 )
# 665 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'requirements) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 112 "Sources/parser.mly"
                                                                          ( create_domain _5 _9 _10 )
# 674 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'types) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 113 "Sources/parser.mly"
                                                            ( create_domain _5 [] _10 )
# 683 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 114 "Sources/parser.mly"
                                             ( create_domain _5 [] _7 )
# 691 "Sources/parser.ml"
               : Domain.domain))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "Sources/parser.mly"
     ( [] )
# 697 "Sources/parser.ml"
               : 'requirements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'requirements) in
    Obj.repr(
# 118 "Sources/parser.mly"
                           ( _1 :: _2 )
# 705 "Sources/parser.ml"
               : 'requirements))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "Sources/parser.mly"
     ( [] )
# 711 "Sources/parser.ml"
               : 'operator_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operator) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 122 "Sources/parser.mly"
                                   ( _3 :: _4 )
# 719 "Sources/parser.ml"
               : 'operator_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operator) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'operator_list) in
    Obj.repr(
# 123 "Sources/parser.mly"
                                            ( _3 :: _4 )
# 727 "Sources/parser.ml"
               : 'operator_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'typed_variable_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips) in
    Obj.repr(
# 127 "Sources/parser.mly"
                                                        ( set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator _1 _4 _5 )
# 736 "Sources/parser.ml"
               : 'operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'variable_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips) in
    Obj.repr(
# 128 "Sources/parser.mly"
                                                  ( set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator _1 _4 _5 )
# 745 "Sources/parser.ml"
               : 'operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips) in
    Obj.repr(
# 129 "Sources/parser.mly"
                           ( set_action_relative_end_time (FunctionFormula.Number 1.0); set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0)); create_operator _1 [] _2 )
# 753 "Sources/parser.ml"
               : 'operator))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'duration) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'quality) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 132 "Sources/parser.mly"
                                        ( (_1, _2, fst _3, snd _3) )
# 762 "Sources/parser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'duration) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 133 "Sources/parser.mly"
                                ( (_1, 0, fst _2, snd _2) )
# 770 "Sources/parser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'quality) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 134 "Sources/parser.mly"
                               ( ((FunctionFormula.Number 1.0), _1, fst _2, snd _2) )
# 778 "Sources/parser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'operator_strips_cont) in
    Obj.repr(
# 135 "Sources/parser.mly"
                       ( ((FunctionFormula.Number 1.0), 0, fst _1, snd _1) )
# 785 "Sources/parser.ml"
               : 'operator_strips))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'operator_strips_cont_prec) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips_cont_eff) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 138 "Sources/parser.mly"
                                                                     ( (_2, _4) )
# 795 "Sources/parser.ml"
               : 'operator_strips_cont))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'operator_strips_cont_eff) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 139 "Sources/parser.mly"
                                   ( (Formula.Top, _2) )
# 803 "Sources/parser.ml"
               : 'operator_strips_cont))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "Sources/parser.mly"
       ( current_time_bound := (true,false); ti_code := 0; set_current_time_set ((FunctionFormula.Number 0.0),(FunctionFormula.Number 1.0)) )
# 809 "Sources/parser.ml"
               : 'operator_strips_cont_prec))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "Sources/parser.mly"
         ( current_time_bound := (true,true); ti_code := 0; set_current_time_set ((FunctionFormula.Number 1.0),(FunctionFormula.Number 1.0)) )
# 815 "Sources/parser.ml"
               : 'operator_strips_cont_eff))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'function_formula) in
    Obj.repr(
# 148 "Sources/parser.mly"
                            ( set_action_relative_end_time _2 )
# 822 "Sources/parser.ml"
               : 'duration))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 149 "Sources/parser.mly"
                                            ( set_action_relative_end_time _5 )
# 830 "Sources/parser.ml"
               : 'duration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 152 "Sources/parser.mly"
         ( FunctionFormula.Number _1 )
# 837 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 153 "Sources/parser.mly"
                                              ( FunctionFormula.Add (_3,_4) )
# 845 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 154 "Sources/parser.mly"
                                               ( FunctionFormula.Sub (_3,_4) )
# 853 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 155 "Sources/parser.mly"
                                                   ( FunctionFormula.Multiply (_3,_4) )
# 861 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'function_formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'function_formula) in
    Obj.repr(
# 156 "Sources/parser.mly"
                                                 ( FunctionFormula.Divide (_3,_4) )
# 869 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list) in
    Obj.repr(
# 157 "Sources/parser.mly"
                               ( FunctionFormula.Funct (create_preatom _2 _3 ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))) )
# 877 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 158 "Sources/parser.mly"
                         ( FunctionFormula.Funct (create_preatom _2 _3 ((FunctionFormula.Number 0.0),(FunctionFormula.Number 0.0))) )
# 885 "Sources/parser.ml"
               : 'function_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 162 "Sources/parser.mly"
                  ( _2 )
# 892 "Sources/parser.ml"
               : 'quality))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _13 = (Parsing.peek_val __caml_parser_env 2 : 'typed_constant_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 1 : 'init_definition) in
    let _15 = (Parsing.peek_val __caml_parser_env 0 : 'goal_definition) in
    Obj.repr(
# 169 "Sources/parser.mly"
                      ( create_problem _5 _9 (!constants_in_domain @ _13) _14 _15 )
# 903 "Sources/parser.ml"
               : Domain.problem))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 10 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _13 = (Parsing.peek_val __caml_parser_env 2 : 'constant_list) in
    let _14 = (Parsing.peek_val __caml_parser_env 1 : 'init_definition) in
    let _15 = (Parsing.peek_val __caml_parser_env 0 : 'goal_definition) in
    Obj.repr(
# 174 "Sources/parser.mly"
                      ( create_problem _5 _9 (!constants_in_domain @ _13) _14 _15 )
# 914 "Sources/parser.ml"
               : Domain.problem))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'init_definition) in
    let _12 = (Parsing.peek_val __caml_parser_env 0 : 'goal_definition) in
    Obj.repr(
# 178 "Sources/parser.mly"
                      ( create_problem _5 _9 [] _11 _12 )
# 924 "Sources/parser.ml"
               : Domain.problem))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom_list) in
    Obj.repr(
# 181 "Sources/parser.mly"
                    ( _3 )
# 931 "Sources/parser.ml"
               : 'init_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 182 "Sources/parser.mly"
                          ( _3 )
# 938 "Sources/parser.ml"
               : 'init_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'metric) in
    Obj.repr(
# 185 "Sources/parser.mly"
                            ( _3 )
# 946 "Sources/parser.ml"
               : 'goal_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'timed_formula) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'metric) in
    Obj.repr(
# 186 "Sources/parser.mly"
                                   ( _3 )
# 954 "Sources/parser.ml"
               : 'goal_definition))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _11 = (Parsing.peek_val __caml_parser_env 0 : 'constraints_list) in
    Obj.repr(
# 192 "Sources/parser.mly"
                                           (create_constraints _5 _9 _11)
# 963 "Sources/parser.ml"
               : Domain.constraints))
; (fun __caml_parser_env ->
    Obj.repr(
# 195 "Sources/parser.mly"
     ( [] )
# 969 "Sources/parser.ml"
               : 'constraints_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constraints_definition) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constraints_list) in
    Obj.repr(
# 196 "Sources/parser.mly"
                                          ( _1::_2 )
# 977 "Sources/parser.ml"
               : 'constraints_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'necessarlyBefore_definition) in
    Obj.repr(
# 199 "Sources/parser.mly"
                              ((ConstraintsType.NecessarlyBefore , _1))
# 984 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'possiblyBefore_definition) in
    Obj.repr(
# 200 "Sources/parser.mly"
                             ((ConstraintsType.PossiblyBefore, _1))
# 991 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fill_definition) in
    Obj.repr(
# 201 "Sources/parser.mly"
                    ((ConstraintsType.Fill, _1))
# 998 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'choice_definition) in
    Obj.repr(
# 202 "Sources/parser.mly"
                     ((ConstraintsType.Choice, _1))
# 1005 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'immediatlyLeadsTo_definition) in
    Obj.repr(
# 203 "Sources/parser.mly"
                                 ((ConstraintsType.ImmediatlyLeadsTo, _1))
# 1012 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eventualyLeadsTo_definition) in
    Obj.repr(
# 204 "Sources/parser.mly"
                              ((ConstraintsType.EventuallyLeadsTo, _1))
# 1019 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parallel_definition) in
    Obj.repr(
# 205 "Sources/parser.mly"
                       ((ConstraintsType.Parallel, _1))
# 1026 "Sources/parser.ml"
               : 'constraints_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 209 "Sources/parser.mly"
                                    (_3::_4)
# 1034 "Sources/parser.ml"
               : 'necessarlyBefore_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 213 "Sources/parser.mly"
                                 (_3::_4)
# 1042 "Sources/parser.ml"
               : 'possiblyBefore_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 216 "Sources/parser.mly"
                            (_3::_4::_5)
# 1051 "Sources/parser.ml"
               : 'fill_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 219 "Sources/parser.mly"
                         (_3::_4)
# 1059 "Sources/parser.ml"
               : 'choice_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 222 "Sources/parser.mly"
                                    (_3::_4)
# 1067 "Sources/parser.ml"
               : 'immediatlyLeadsTo_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 225 "Sources/parser.mly"
                                    (_3::_4)
# 1075 "Sources/parser.ml"
               : 'eventualyLeadsTo_definition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 228 "Sources/parser.mly"
                           (_3::_4)
# 1083 "Sources/parser.ml"
               : 'parallel_definition))
; (fun __caml_parser_env ->
    Obj.repr(
# 232 "Sources/parser.mly"
     ( )
# 1089 "Sources/parser.ml"
               : 'metric))
; (fun __caml_parser_env ->
    Obj.repr(
# 233 "Sources/parser.mly"
                                        ( )
# 1095 "Sources/parser.ml"
               : 'metric))
; (fun __caml_parser_env ->
    Obj.repr(
# 234 "Sources/parser.mly"
                                        ( )
# 1101 "Sources/parser.ml"
               : 'metric))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula_list) in
    Obj.repr(
# 237 "Sources/parser.mly"
                      ( Formula.Conjunct (Array.of_list _3) )
# 1108 "Sources/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'time_set) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'timed_formula_list) in
    Obj.repr(
# 238 "Sources/parser.mly"
                                           ( Formula.Conjunct (Array.of_list _5) )
# 1116 "Sources/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'time_set) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'atom) in
    Obj.repr(
# 239 "Sources/parser.mly"
                                   ( Formula.NegLit _6 )
# 1124 "Sources/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 240 "Sources/parser.mly"
                    ( Formula.NegLit _4 )
# 1131 "Sources/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'time_set) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 241 "Sources/parser.mly"
                         ( Formula.PosLit _4 )
# 1139 "Sources/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 242 "Sources/parser.mly"
          ( Formula.PosLit _2 )
# 1146 "Sources/parser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timed_formula_list) in
    Obj.repr(
# 246 "Sources/parser.mly"
                            ( Formula.Conjunct (Array.of_list _3) )
# 1153 "Sources/parser.ml"
               : 'timed_formula))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 247 "Sources/parser.mly"
                    ( Formula.NegLit _4 )
# 1160 "Sources/parser.ml"
               : 'timed_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 248 "Sources/parser.mly"
          ( Formula.PosLit _2 )
# 1167 "Sources/parser.ml"
               : 'timed_formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 252 "Sources/parser.mly"
     ( [] )
# 1173 "Sources/parser.ml"
               : 'formula_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula_list) in
    Obj.repr(
# 253 "Sources/parser.mly"
                       ( _1 :: _2 )
# 1181 "Sources/parser.ml"
               : 'formula_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 257 "Sources/parser.mly"
     ( [] )
# 1187 "Sources/parser.ml"
               : 'timed_formula_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'timed_formula) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timed_formula_list) in
    Obj.repr(
# 258 "Sources/parser.mly"
                                   ( _1 :: _2 )
# 1195 "Sources/parser.ml"
               : 'timed_formula_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 262 "Sources/parser.mly"
     ( [] )
# 1201 "Sources/parser.ml"
               : 'atom_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom_list) in
    Obj.repr(
# 263 "Sources/parser.mly"
                    ( _2 :: _3 )
# 1209 "Sources/parser.ml"
               : 'atom_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 267 "Sources/parser.mly"
     ( [] )
# 1215 "Sources/parser.ml"
               : 'timed_atom_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'timed_atom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timed_atom_list) in
    Obj.repr(
# 268 "Sources/parser.mly"
                             ( _1 :: _2 )
# 1223 "Sources/parser.ml"
               : 'timed_atom_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 272 "Sources/parser.mly"
     ( [] )
# 1229 "Sources/parser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 273 "Sources/parser.mly"
                          ( _2 :: _3 )
# 1237 "Sources/parser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'timed_atom) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 274 "Sources/parser.mly"
                             ( _1 :: _2 )
# 1245 "Sources/parser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'atom) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'mixed_atom_list) in
    Obj.repr(
# 275 "Sources/parser.mly"
                                             ( functions_value_list := (_4,_5) :: !functions_value_list; _7 )
# 1254 "Sources/parser.ml"
               : 'mixed_atom_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 279 "Sources/parser.mly"
                   ( create_atom _1 _2 !current_time_set )
# 1262 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 280 "Sources/parser.mly"
                   ( create_atom "=" _2 !current_time_set )
# 1269 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 281 "Sources/parser.mly"
               ( create_atom "at" _2 !current_time_set )
# 1276 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 282 "Sources/parser.mly"
                                ( create_atom (fst _4) (snd _4) !current_time_set )
# 1284 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 283 "Sources/parser.mly"
                                ( create_atom (fst _4) (snd _4) !current_time_set )
# 1292 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 284 "Sources/parser.mly"
                          ( create_atom (fst _3) (snd _3) !current_time_set )
# 1300 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 285 "Sources/parser.mly"
                          ( create_atom (fst _3) (snd _3) !current_time_set )
# 1308 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 286 "Sources/parser.mly"
                             ( create_atom (fst _3) (snd _3) !current_time_set )
# 1316 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 287 "Sources/parser.mly"
                               ( create_atom (fst _3) (snd _3) !current_time_set )
# 1324 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'funct) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 288 "Sources/parser.mly"
                               ( create_atom (fst _3) (snd _3) !current_time_set )
# 1332 "Sources/parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'time_set) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 292 "Sources/parser.mly"
                         ( _4 )
# 1340 "Sources/parser.ml"
               : 'timed_atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'time_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'produced_timed_atom) in
    Obj.repr(
# 293 "Sources/parser.mly"
                                     ( _3 )
# 1348 "Sources/parser.ml"
               : 'timed_atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'time_set) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'consumed_timed_atom) in
    Obj.repr(
# 294 "Sources/parser.mly"
                                     ( _3 )
# 1356 "Sources/parser.ml"
               : 'timed_atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 298 "Sources/parser.mly"
                        ( ti_code := 10; _4 )
# 1363 "Sources/parser.ml"
               : 'produced_timed_atom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'atom) in
    Obj.repr(
# 301 "Sources/parser.mly"
                        ( ti_code := 10; _4 )
# 1370 "Sources/parser.ml"
               : 'consumed_timed_atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timepoint) in
    Obj.repr(
# 304 "Sources/parser.mly"
               ( current_time_bound := (true,true); ti_code := 0; set_current_time_set (_2,_2) )
# 1377 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timepoint) in
    Obj.repr(
# 305 "Sources/parser.mly"
                   ( current_time_bound := (true,false); ti_code := 0; set_current_time_set (_2,_2) )
# 1384 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'timepoint) in
    Obj.repr(
# 306 "Sources/parser.mly"
                  ( current_time_bound := (false,true); ti_code := 0; set_current_time_set (_2,_2) )
# 1391 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 307 "Sources/parser.mly"
                     ( ti_code := 0; set_current_time_set _2 )
# 1398 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 308 "Sources/parser.mly"
                          ( ti_code := 1; set_current_time_set _2 )
# 1405 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 309 "Sources/parser.mly"
                         ( ti_code := 2; ti_min_dur := 0.0; set_current_time_set _2 )
# 1412 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'number) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 310 "Sources/parser.mly"
                                        ( ti_code := 2; ti_min_dur := _2; set_current_time_set _4 )
# 1420 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'time_interval) in
    Obj.repr(
# 311 "Sources/parser.mly"
                              ( ti_code := 10; set_current_time_set _4 )
# 1427 "Sources/parser.ml"
               : 'time_set))
; (fun __caml_parser_env ->
    Obj.repr(
# 314 "Sources/parser.mly"
        ( (FunctionFormula.Number 0.0) )
# 1433 "Sources/parser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    Obj.repr(
# 315 "Sources/parser.mly"
      ( !action_relative_end_time )
# 1439 "Sources/parser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'number) in
    Obj.repr(
# 316 "Sources/parser.mly"
         ( (FunctionFormula.Number _1) )
# 1446 "Sources/parser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 317 "Sources/parser.mly"
                                ( FunctionFormula.Add (_3,_4) )
# 1454 "Sources/parser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 318 "Sources/parser.mly"
                                 ( FunctionFormula.Sub (_3,_4) )
# 1462 "Sources/parser.ml"
               : 'timepoint))
; (fun __caml_parser_env ->
    Obj.repr(
# 321 "Sources/parser.mly"
      ( current_time_bound := (true,false); ((FunctionFormula.Number 0.0),!action_relative_end_time) )
# 1468 "Sources/parser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 322 "Sources/parser.mly"
                            ( current_time_bound := (true,true); (_2,_3) )
# 1476 "Sources/parser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 323 "Sources/parser.mly"
                            ( current_time_bound := (true,false); (_2,_3) )
# 1484 "Sources/parser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 324 "Sources/parser.mly"
                            ( current_time_bound := (false,true); (_2,_3) )
# 1492 "Sources/parser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'timepoint) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'timepoint) in
    Obj.repr(
# 325 "Sources/parser.mly"
                            ( current_time_bound := (false,false); (_2,_3) )
# 1500 "Sources/parser.ml"
               : 'time_interval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'types_aux2) in
    Obj.repr(
# 329 "Sources/parser.mly"
             ( )
# 1507 "Sources/parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'types_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 330 "Sources/parser.mly"
                        (
    let ptype = symb_set#create_predicate _2 in
      List.iter (fun p -> p#add_builtin_type ptype) _1 )
# 1518 "Sources/parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    Obj.repr(
# 335 "Sources/parser.mly"
       ( [] )
# 1524 "Sources/parser.ml"
               : 'types_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'types_aux) in
    Obj.repr(
# 336 "Sources/parser.mly"
                  ( symb_set#create_predicate _1 :: _2 )
# 1532 "Sources/parser.ml"
               : 'types_aux))
; (fun __caml_parser_env ->
    Obj.repr(
# 339 "Sources/parser.mly"
     ( )
# 1538 "Sources/parser.ml"
               : 'types_aux2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'types_aux2) in
    Obj.repr(
# 340 "Sources/parser.mly"
                   ( )
# 1546 "Sources/parser.ml"
               : 'types_aux2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_constant_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 344 "Sources/parser.mly"
                                    ( create_typed_term_list _1 _2 )
# 1554 "Sources/parser.ml"
               : 'typed_constant_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_constant_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_constant_list) in
    Obj.repr(
# 345 "Sources/parser.mly"
                                                     ( create_typed_term_list _1 _2 @ _3 )
# 1563 "Sources/parser.ml"
               : 'typed_constant_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 348 "Sources/parser.mly"
       ( [] )
# 1569 "Sources/parser.ml"
               : 'typed_constant_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constant) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_constant_list_aux) in
    Obj.repr(
# 349 "Sources/parser.mly"
                                   ( _1 :: _2 )
# 1577 "Sources/parser.ml"
               : 'typed_constant_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_variable_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 353 "Sources/parser.mly"
                                   ( create_typed_term_list _1 _2 )
# 1585 "Sources/parser.ml"
               : 'typed_variable_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typed_variable_list_aux) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list) in
    Obj.repr(
# 354 "Sources/parser.mly"
                                                    ( create_typed_term_list _1 _2 @ _3 )
# 1594 "Sources/parser.ml"
               : 'typed_variable_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 357 "Sources/parser.mly"
       ( [] )
# 1600 "Sources/parser.ml"
               : 'typed_variable_list_aux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list_aux) in
    Obj.repr(
# 358 "Sources/parser.mly"
                                   ( _1 :: _2 )
# 1608 "Sources/parser.ml"
               : 'typed_variable_list_aux))
; (fun __caml_parser_env ->
    Obj.repr(
# 362 "Sources/parser.mly"
     ( [] )
# 1614 "Sources/parser.ml"
               : 'functions_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'funct) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'functions_list) in
    Obj.repr(
# 363 "Sources/parser.mly"
                          ( _2 :: _3 )
# 1622 "Sources/parser.ml"
               : 'functions_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_variable_list) in
    Obj.repr(
# 367 "Sources/parser.mly"
                            ( create_function _1 _2 )
# 1630 "Sources/parser.ml"
               : 'funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 368 "Sources/parser.mly"
                      ( create_function _1 _2 )
# 1638 "Sources/parser.ml"
               : 'funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typed_constant_list) in
    Obj.repr(
# 372 "Sources/parser.mly"
                            ( create_function _1 _2 )
# 1646 "Sources/parser.ml"
               : 'instanciated_funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant_list) in
    Obj.repr(
# 373 "Sources/parser.mly"
                      ( create_function _1 _2 )
# 1654 "Sources/parser.ml"
               : 'instanciated_funct))
; (fun __caml_parser_env ->
    Obj.repr(
# 377 "Sources/parser.mly"
     ( [] )
# 1660 "Sources/parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 378 "Sources/parser.mly"
                 ( _1 :: _2 )
# 1668 "Sources/parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 381 "Sources/parser.mly"
     ( [] )
# 1674 "Sources/parser.ml"
               : 'variable_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variable_list) in
    Obj.repr(
# 382 "Sources/parser.mly"
                         ( _1 :: _2 )
# 1682 "Sources/parser.ml"
               : 'variable_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 385 "Sources/parser.mly"
     ( [] )
# 1688 "Sources/parser.ml"
               : 'constant_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constant) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constant_list) in
    Obj.repr(
# 386 "Sources/parser.mly"
                         ( _1 :: _2 )
# 1696 "Sources/parser.ml"
               : 'constant_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 389 "Sources/parser.mly"
      ( symb_set#create_variable _1 )
# 1703 "Sources/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 390 "Sources/parser.mly"
        ( symb_set#create_constant _1 )
# 1710 "Sources/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 391 "Sources/parser.mly"
          ( symb_set#create_constant (string_of_int _1) )
# 1717 "Sources/parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 394 "Sources/parser.mly"
      ( symb_set#create_variable _1 )
# 1724 "Sources/parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 397 "Sources/parser.mly"
        ( symb_set#create_constant _1 )
# 1731 "Sources/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 398 "Sources/parser.mly"
          ( symb_set#create_constant (string_of_int _1) )
# 1738 "Sources/parser.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 401 "Sources/parser.mly"
          ( float_of_int _1 )
# 1745 "Sources/parser.ml"
               : 'number))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 402 "Sources/parser.mly"
           ( _1 )
# 1752 "Sources/parser.ml"
               : 'number))
(* Entry domain *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry problem *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let domain (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Domain.domain)
let problem (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Domain.problem)
