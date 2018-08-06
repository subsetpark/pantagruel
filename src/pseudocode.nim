import strutils, sequtils, algorithm

type
  Number = int
  Relation = enum
    rEquals = "="
    rImplies = "->"
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
  Expression = ref object
    case exprType: ExpressionType
    of etRelation:
      left: Expression
      right: Expression
      exprRelation: Relation
    of etValue:
      exprName: string
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

proc value(name: string): Domain = Domain(domType: dtValue, name: name)
proc list(e: Domain): Domain = Domain(domType: dtContainer, containerType: ctList, contents: e)
proc str(valueName: string): Domain =
  Domain(domType: dtContainer, containerType: ctString, contents: value(valueName))
proc set(e: Domain): Domain = Domain(domType: dtContainer, containerType: ctSet, contents: e)
proc bunch(e: Domain): Domain = Domain(domType: dtContainer, containerType: ctBunch, contents: e)
proc range(low, high: Number): Domain = Domain(domType: dtRange, high: high, low: low)
proc declare(name: string, clauses: varargs[seq[FunctionArg]]): FunctionDeclaration =
  FunctionDeclaration(name: name, clauses: @clauses)

let
  N = range(1, int.high)
  Z = range(0, int.high)
  B = range(0, 1)
  
proc `$`(d: Domain): string =
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

proc varName(name: string): Expression = Expression(
  exprType: etValue,
  exprName: name
)  

proc `$`(e: Expression): string =
  case e.exprType:
  of etNull:
    result = ""  
  of etRelation:
    result = "$1 $2 $3" % [$e.left, $e.exprRelation, $e.right]
  of etValue:
    result = e.exprName
  of etApplication:
    if not e.exprFun.isNil:
      result = $e.exprFun
      if not e.exprArg.isNil:
        result &=  " " & $e.exprArg

proc `$`(f: FunctionDeclaration): string =
  var clausesStrings = newSeq[string]()
  for clause in f.clauses:
    let
      argString = clause.mapIt(it.name).join(", ")
      domainString = clause.mapIt($it.domain).join(", ")
    clausesStrings.add "$1: $2" % [argString, domainString]
  "$1<$2>" % [f.name, clausesStrings.join(", ")]

proc maybeApplyFun(e: Expression, args: seq[Expression]): Expression =
  if args.len == 0:
    result = e
  else:
    let
      arg = args[0]
      e2 = Expression(exprType: etApplication, exprFun: e, exprArg: arg)
    result = maybeApplyFun(e2, args[1..args.high])

proc applyFun(args: varargs[Expression, varName]): Expression =
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

when isMainModule:
  let decl = declare(
    "f",
    @[("x", str("y"))],
    @[("y", B)]
  )  
  echo decl
  let decl2 = declare("g", @[("x", str("y")), ("i", Z)])
  echo decl2
  let application = applyFun("a", "b", "c")
  echo application
