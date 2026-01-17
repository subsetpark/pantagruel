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
%token DARROW ARROW
%token EQ NEQ LT GT LE GE
%token PLUS MINUS TIMES DIVIDE CARD
%token PRIME MAPSTO
%token <int> PROJ
%token AND OR NOT
%token FORALL EXISTS IN SUBSET
%token DOT COMMA COLON DCOLON PIPE SEPARATOR
%token LPAREN RPAREN LBRACKET RBRACKET
%token EOF

(* Precedence - lowest to highest *)
%right ARROW                (* implication *)
%left OR                    (* disjunction *)
%left AND                   (* conjunction *)
%nonassoc NOT               (* negation *)
%nonassoc EQ NEQ LT GT LE GE IN SUBSET  (* comparison/membership *)
%left PLUS MINUS            (* addition *)
%left TIMES DIVIDE          (* multiplication *)
%nonassoc CARD              (* cardinality *)
%nonassoc UMINUS            (* unary minus *)

%start <Ast.document> document

%%

(* Document structure *)
document:
  | MODULE name=UPPER_IDENT DOT
    imports=list(import_decl)
    chapters=separated_nonempty_list(WHERE, chapter)
    EOF
    { { module_name = name; imports; chapters } }

import_decl:
  | IMPORT name=UPPER_IDENT DOT
    { located $startpos $endpos name }

chapter:
  | head=list(declaration) SEPARATOR body=list(proposition)
    { { head; body } }

(* Declarations *)
declaration:
  | name=UPPER_IDENT DOT
    { located $startpos $endpos (DeclDomain name) }
  | name=UPPER_IDENT EQ t=type_expr DOT
    { located $startpos $endpos (DeclAlias (name, t)) }
  | name=LOWER_IDENT ps=params_opt gs=guards_opt ret=return_type_opt DOT
    { located $startpos $endpos (DeclProc {
        name;
        params = ps;
        guards = gs;
        return_type = ret;
      }) }

params_opt:
  | (* empty *) { [] }
  | p=param ps=list(preceded(COMMA, param_only)) { p :: ps }

param_only:
  | p=param { p }

param:
  | name=LOWER_IDENT COLON t=type_expr { { param_name = name; param_type = t } }

guards_opt:
  | (* empty *) { [] }
  | COMMA g=guard_expr gs=list(preceded(COMMA, guard_item)) { GExpr g :: gs }

guard_item:
  | p=param { GParam p }
  | e=guard_expr { GExpr e }

(* Guard expressions: restricted to avoid parsing ambiguity with params *)
guard_expr:
  | e=disjunction { e }

return_type_opt:
  | (* empty *) { None }
  | DARROW t=type_expr { Some t }

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
  | LBRACKET t=type_expr RBRACKET { TList t }
  | LPAREN t=type_expr RPAREN { t }

(* Propositions in chapter body *)
proposition:
  | e=expr DOT { located $startpos $endpos e }

(* Expressions *)
expr:
  | e=quantified { e }
  | e=implication { e }

quantified:
  | FORALL pg=quant_params_guards PIPE e=expr
    { let (params, guards) = pg in EForall (params, guards, e) }
  | EXISTS pg=quant_params_guards PIPE e=expr
    { let (params, guards) = pg in EExists (params, guards, e) }

(* Parameters and guards in quantifiers *)
quant_params_guards:
  | p=param rest=list(preceded(COMMA, quant_guard_or_param))
    { split_params_guards (GParam p :: rest) }

quant_guard_or_param:
  | p=param { GParam p }
  | e=conjunction { GExpr e }  (* Use conjunction to avoid ambiguity with | *)

implication:
  | e1=disjunction ARROW e2=expr { EBinop (OpImpl, e1, e2) }
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
  | MINUS e=unary %prec UMINUS { EUnop (OpNeg, e) }
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
