import strutils, sequtils, algorithm

type
  Number = int
  Relation = enum
    rEquals = "="
    rImplies = "->"
    rIsIn = ":"
  DomainType = enum
    dtContainer
    dtRange
    dtValue
  Domain = ref object
    case domType: DomainType
    of dtValue:
      name: string
    of dtContainer:
      containerType: ContainerType
      contents: Domain
    of dtRange:
      low, high: Number
  ExpressionType = enum
    etNull
    etRelation
    etValue
    etApplication
    etSize
  Expression = ref object
    case exprType: ExpressionType
    of etRelation:
      left: Expression
      right: Expression
      exprRelation: Relation
    of etValue:
      exprName: string
    of etSize:
      exprUnary: Expression
    of etApplication:
      exprFun: Expression
      exprArg: Expression
    of etNull:
      discard
  ContainerType = enum
    ctList,
    ctString,
    ctSet,
    ctBunch
  FunctionDeclaration = object
    name: string
    clauses: seq[seq[FunctionArg]]
  FunctionArg = tuple
    name: string
    domain: Domain

converter varName*(name: string): Expression = Expression(
  exprType: etValue,
  exprName: name
)

# Domains    

proc value*(name: string): Domain = Domain(domType: dtValue, name: name)
proc list*(e: Domain): Domain = Domain(domType: dtContainer, containerType: ctList, contents: e)
proc str*(valueName: string): Domain =
  Domain(domType: dtContainer, containerType: ctString, contents: value(valueName))
proc set*(e: Domain): Domain = Domain(domType: dtContainer, containerType: ctSet, contents: e)
proc bunch*(e: Domain): Domain = Domain(domType: dtContainer, containerType: ctBunch, contents: e)
proc range*(low, high: Number): Domain = Domain(domType: dtRange, high: high, low: low)

# Function Declaration

proc declare*(name: string, clauses: varargs[seq[FunctionArg]]): FunctionDeclaration =
  FunctionDeclaration(name: name, clauses: @clauses)

# Expression Relations  

proc relation(left, right: Expression, relation: Relation): Expression =
  Expression(exprType: etRelation, exprRelation: relation, left: left, right: right)
proc `->`*(left, right: Expression): Expression = relation(left, right, rImplies)
proc `==`*(left, right: Expression): Expression = relation(left, right, rEquals)
proc `in`*(left, right: Expression): Expression = relation(left, right, rIsIn)

# Unary Functions

proc size*(e: Expression): Expression =
  Expression(exprType: etSize, exprUnary: e)

let
  N* = range(1, int.high)
  Z* = range(0, int.high)
  B* = range(0, 1)

proc `$`*(d: Domain): string =
  case d.domType
  of dtValue:
    result = d.name
  of dtContainer:
    let wrapper = case d.containerType
    of ctList: "[$#]"
    of ctString: "\"$#\""
    of ctSet: "{$#}"
    of ctBunch: "($#)"
    result = wrapper % $d.contents
  of dtRange:
    if d == N:
      return "ℕ"
    if d == Z:    
      return "ℤ"
    if d == B:
      return "𝔹"
    result = "$#..$#" % [$d.low, $d.high]

proc `$`*(e: Expression): string =
  case e.exprType:
  of etNull:
    result = ""  
  of etRelation:
    result = "$1 $2 $3" % [$e.left, $e.exprRelation, $e.right]
  of etValue:
    result = e.exprName
  of etSize:
    result = "#" & $e.exprUnary  
  of etApplication:
    if not e.exprFun.isNil:
      result = $e.exprFun
      if not e.exprArg.isNil:
        result &=  " " & $e.exprArg

proc `$`*(f: FunctionDeclaration): string =
  var clausesStrings = newSeq[string]()
  for clause in f.clauses:
    let
      argString = clause.mapIt(it.name).join(", ")
      domainString = clause.mapIt($it.domain).join(", ")
    clausesStrings.add "$1: $2" % [argString, domainString]
  "$1<$2>" % [f.name, clausesStrings.join(", ")]

proc applyFun*(args: varargs[Expression, varName]): Expression =
  result = Expression(exprType: etApplication)
  var
    tail = result
    toApply = args.reversed()
  while toApply.len > 0:
    let arg = toApply.pop()
    tail.exprFun = arg
    if toApply.len > 0:
      tail.exprArg = Expression(exprType: etApplication)
      tail = tail.exprArg
