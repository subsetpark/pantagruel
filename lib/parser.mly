%{
  open Ast

  let make_loc startpos _endpos = {
    file = startpos.Lexing.pos_fname;
    line = startpos.Lexing.pos_lnum;
    col = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol;
  }

  let located startpos endpos value = {
    loc = make_loc startpos endpos;
    value;
    doc = [];
    doc_adjacent = true;
  }

  let located_with_doc startpos endpos value =
    let (doc, doc_adjacent) = Doc_comments.get_at_pos startpos in
    {
      loc = make_loc startpos endpos;
      value;
      doc;
      doc_adjacent;
    }

  (* Split a list of guards into params and expression guards *)
  let split_params_guards (guards : guard list) : param list * guard list =
    let rec go params others = function
      | [] -> (List.rev params, List.rev others)
      | GParam p :: rest -> go (p :: params) others rest
      | g :: rest -> go params (g :: others) rest
    in
    go [] [] guards
%}

(* Tokens from lexer *)
%token MODULE IMPORT WHERE TRUE FALSE
%token <string> UPPER_IDENT LOWER_IDENT
%token <int> NAT
%token <float> REAL
%token <string> STRING
%token SQUIG_ARROW DARROW ARROW IFF
%token EQ NEQ LT GT LE GE
%token PLUS MINUS TIMES DIVIDE CARD
%token PRIME MAPSTO
%token <int> PROJ
%token AND OR NOT
%token FORALL EXISTS IN SUBSET
%token DOT COMMA COLON DCOLON PIPE SEPARATOR
%token LPAREN RPAREN LBRACKET RBRACKET
%token LBRACE RBRACE CONTEXT
%token <string> ACTION_LABEL
%token EOF

%start <Ast.document> document

%%

(* Document structure *)
document:
  | MODULE name=UPPER_IDENT DOT
    imports=list(import_decl)
    contexts=list(context_decl)
    chapters=separated_nonempty_list(WHERE, chapter)
    EOF
    { { module_name = Some name; imports; contexts; chapters } }
  | chapters=separated_nonempty_list(WHERE, chapter)
    EOF
    { { module_name = None; imports = []; contexts = []; chapters } }

import_decl:
  | IMPORT name=UPPER_IDENT DOT
    { located $startpos $endpos name }

context_decl:
  | CONTEXT name=UPPER_IDENT DOT
    { located $startpos $endpos name }

chapter:
  | head=nonempty_list(declaration) SEPARATOR body=list(proposition)
    { { head; body } }

(* Declarations - doc comments are looked up by start position *)
declaration:
  | name=UPPER_IDENT DOT
    { located_with_doc $startpos $endpos (DeclDomain name) }
  | name=UPPER_IDENT EQ t=type_expr DOT
    { located_with_doc $startpos $endpos (DeclAlias (name, t)) }
  | LBRACE ctxs=separated_nonempty_list(COMMA, UPPER_IDENT) RBRACE
    name=LOWER_IDENT pg=rule_params_guards DARROW ret=type_expr DOT
    { let (params, guards) = pg in
      located_with_doc $startpos $endpos (DeclRule {
        name;
        params;
        guards;
        return_type = ret;
        contexts = ctxs;
      }) }
  | name=LOWER_IDENT pg=rule_params_guards DARROW ret=type_expr DOT
    { let (params, guards) = pg in
      located_with_doc $startpos $endpos (DeclRule {
        name;
        params;
        guards;
        return_type = ret;
        contexts = [];
      }) }
  | ctx=UPPER_IDENT SQUIG_ARROW label=ACTION_LABEL PIPE pg=action_params_guards DOT
    { let (params, guards) = pg in
      located_with_doc $startpos $endpos (DeclAction {
        label;
        params;
        guards;
        context = Some ctx;
      }) }
  | ctx=UPPER_IDENT SQUIG_ARROW label=ACTION_LABEL DOT
    { located_with_doc $startpos $endpos (DeclAction {
        label;
        params = [];
        guards = [];
        context = Some ctx;
      }) }
  | SQUIG_ARROW label=ACTION_LABEL PIPE pg=action_params_guards DOT
    { let (params, guards) = pg in
      located_with_doc $startpos $endpos (DeclAction {
        label;
        params;
        guards;
        context = None;
      }) }
  | SQUIG_ARROW label=ACTION_LABEL DOT
    { located_with_doc $startpos $endpos (DeclAction {
        label;
        params = [];
        guards = [];
        context = None;
      }) }

(* Parameters and guards for actions (after |, must start with param) *)
action_params_guards:
  | p=param rest=list(preceded(COMMA, rule_param_or_guard))
    { split_params_guards (GParam p :: rest) }

(* Parameters and guards for rules - parsed together then split *)
rule_params_guards:
  | (* empty *) { ([], []) }
  | p=param rest=list(preceded(COMMA, rule_param_or_guard))
    { split_params_guards (GParam p :: rest) }

rule_param_or_guard:
  | p=param { GParam p }
  | e=rule_guard_expr { GExpr e }

(* Guard expressions for rules: use disjunction level *)
rule_guard_expr:
  | e=disjunction { e }

param:
  | name=LOWER_IDENT COLON t=type_expr { { param_name = name; param_type = t } }

(* Type expressions *)
type_expr:
  | t=type_sum { t }

type_sum:
  | ts=separated_nonempty_list(PLUS, type_product)
    { match ts with [t] -> t | _ -> TSum ts }

type_product:
  | ts=separated_nonempty_list(TIMES, type_term)
    { match ts with [t] -> t | _ -> TProduct ts }

type_term:
  | name=UPPER_IDENT { TName name }
  | m=UPPER_IDENT DCOLON name=UPPER_IDENT { TQName (m, name) }
  | LBRACKET t=type_expr RBRACKET { TList t }
  | LPAREN t=type_expr RPAREN { t }

(* Propositions in chapter body - doc comments are looked up by start position *)
proposition:
  | e=expr DOT
    { located_with_doc $startpos $endpos e }

(* Expressions *)
expr:
  | e=quantified { e }
  | e=biconditional { e }

biconditional:
  | e1=implication IFF e2=implication { EBinop (OpIff, e1, e2) }
  | e=implication { e }

quantified:
  | FORALL pg=quant_params_guards PIPE e=expr
    { let (params, guards) = pg in EForall (params, guards, e) }
  | EXISTS pg=quant_params_guards PIPE e=expr
    { let (params, guards) = pg in EExists (params, guards, e) }

(* Parameters and guards in quantifiers *)
quant_params_guards:
  | first=quant_first_binding rest=list(preceded(COMMA, quant_guard_or_param))
    { split_params_guards (first :: rest) }

quant_first_binding:
  | p=param { GParam p }
  | name=LOWER_IDENT IN e=term { GIn (name, e) }

quant_guard_or_param:
  | p=param { GParam p }
  | name=LOWER_IDENT IN e=term { GIn (name, e) }  (* x in xs - binds x to element type *)
  | e=conjunction { GExpr e }  (* Use conjunction to avoid ambiguity with | *)

(* The RHS of -> allows quantifiers (P -> all x: T | Q) and
   nested implications (right-associative), but not <->. *)
impl_rhs:
  | e=quantified { e }
  | e=implication { e }

implication:
  | e1=disjunction ARROW e2=impl_rhs { EBinop (OpImpl, e1, e2) }
  | e=disjunction { e }

disjunction:
  | e1=disjunction OR e2=conjunction { EBinop (OpOr, e1, e2) }
  | e=conjunction { e }

conjunction:
  | e1=conjunction AND e2=negation { EBinop (OpAnd, e1, e2) }
  | e=negation { e }

negation:
  | NOT e=negation { EUnop (OpNot, e) }
  | e=comparison { e }

comparison:
  | e1=term op=comp_op e2=term { EBinop (op, e1, e2) }
  | e=term { e }

%inline comp_op:
  | EQ { OpEq }
  | NEQ { OpNeq }
  | LT { OpLt }
  | GT { OpGt }
  | LE { OpLe }
  | GE { OpGe }
  | IN { OpIn }
  | SUBSET { OpSubset }

term:
  | e1=term PLUS e2=factor { EBinop (OpAdd, e1, e2) }
  | e1=term MINUS e2=factor { EBinop (OpSub, e1, e2) }
  | e=factor { e }

factor:
  | e1=factor TIMES e2=unary { EBinop (OpMul, e1, e2) }
  | e1=factor DIVIDE e2=unary { EBinop (OpDiv, e1, e2) }
  | e=unary { e }

unary:
  | CARD e=unary { EUnop (OpCard, e) }
  | MINUS e=unary { EUnop (OpNeg, e) }
  | e=primary { e }

(* Application by juxtaposition *)
primary:
  | atoms=nonempty_list(postfix)
    { match atoms with
      | [e] -> e
      | f :: args -> EApp (f, args)
      | [] -> assert false }

(* Postfix operations: projections *)
postfix:
  | e=atom projs=list(projection)
    { List.fold_left (fun e i -> EProj (e, i)) e projs }

projection:
  | n=PROJ { n }

atom:
  | name=LOWER_IDENT { EVar name }
  | name=UPPER_IDENT { EDomain name }
  | m=UPPER_IDENT DCOLON name=LOWER_IDENT { EQualified (m, name) }
  | m=UPPER_IDENT DCOLON name=UPPER_IDENT { EQualified (m, name) }
  | n=NAT { ELitNat n }
  | r=REAL { ELitReal r }
  | s=STRING { ELitString s }
  | TRUE { ELitBool true }
  | FALSE { ELitBool false }
  | name=LOWER_IDENT PRIME { EPrimed name }
  | name=LOWER_IDENT LBRACKET ovs=override_list RBRACKET
    { EOverride (name, ovs) }
  | LPAREN e=expr RPAREN { e }
  | LPAREN e1=expr COMMA e2=expr rest=list(preceded(COMMA, expr)) RPAREN
    { ETuple (e1 :: e2 :: rest) }

override_list:
  | ovs=separated_nonempty_list(COMMA, override_pair) { ovs }

override_pair:
  | k=expr MAPSTO v=expr { (k, v) }
