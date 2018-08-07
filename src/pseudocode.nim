import strutils, sequtils, algorithm

type
  NumberKind = enum
    nkInt
    nkSymbol
  Number = object
    case numberKind: NumberKind
    of nkInt:
      numberInt: int
    of nkSymbol:
      numberSymbol: string
  Relation = enum
    rEquals = "="
    rImplies = "->"
    rIsIn = ":"
  ContainerType = enum
    ctList,
    ctString,
    ctSet,
    ctBunch
  DomainType = enum
    dtContainer
    dtRange
    dtValue
  Domain = ref object
    case domType: DomainType
    of dtValue:
      domainName: string
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
    etComprehension
    etContainer
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
    of etComprehension:
      exprCompDomain: Domain
      exprCompElement: string
      exprCompExpr: Expression
    of etContainer:
      exprContainerType: ContainerType
      exprContents: Expression
    of etNull:
      discard
  FunctionDeclaration = object
    name: string
    clauses: seq[seq[FunctionArg]]
  FunctionArg = tuple
    name: string
    domain: Domain

converter e*(name: string): Expression = Expression(
  exprType: etValue,
  exprName: name
)

converter d*(name: string): Domain = Domain(
  domType: dtValue,
  domainName: name
)

converter intToNumber*(n: int): Number = Number(
  numberKind: nkInt,
  numberInt: n
)

converter stringToNumber*(s: string): Number = Number(
  numberKind: nkSymbol,
  numberSymbol: s
)

proc `...`*(low, high: Number): Domain

let
  N* = 1...int.high
  Z* = 0...int.high
  B* = 0...1

proc `$`*(n: Number): string =
  case n.numberKind
  of nkInt: $n.numberInt
  of nkSymbol: $n.numberSymbol

proc containerWrapper(ct: ContainerType): string =
    case ct
    of ctList: "[$#]"
    of ctString: "\"$#\""
    of ctSet: "{$#}"
    of ctBunch: "($#)"

proc `$`*(d: Domain): string =
  case d.domType
  of dtValue:
    result = d.domainName
  of dtContainer:
    result = containerWrapper(d.containerType) % $d.contents
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
  of etComprehension:
    result = "$3 : $2 ∈ $1" % [
      $e.exprCompDomain,
      $e.exprCompElement,
      $e.exprCompExpr
    ]
  of etContainer:
    result = containerWrapper(e.exprContainerType) % $e.exprContents

# Domains    

proc domainContainer(d: Domain, containerType: ContainerType): Domain =
  Domain(domType: dtContainer, containerType: containerType, contents: d)
proc listOf*(d: Domain): Domain = domainContainer(d, ctList)
proc strOf*(d: Domain): Domain = domainContainer(d, ctString)
proc setOf*(d: Domain): Domain = domainContainer(d, ctSet)
proc bunchOf*(d: Domain): Domain = domainContainer(d, ctBunch)
proc `...`*(low, high: Number): Domain = Domain(domType: dtRange, high: high, low: low)

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
proc listOf*(e: Expression): Expression= Expression(exprType: etContainer, exprContainerType: ctList, exprContents: e)
proc strOf*(e: Expression): Expression =
  Expression(exprType: etContainer, exprContainerType: ctString, exprContents: e)
proc setOf*(e: Expression): Expression = Expression(exprType: etContainer, exprContainerType: ctSet, exprContents: e)
proc bunchOf*(e: Expression): Expression = Expression(exprType: etContainer, exprContainerType: ctBunch, exprContents: e)

proc sizeDomain*(e: Expression): Domain = Domain(domType: dtRange, low: 0, high: $size(e))

# Map / Reduce

proc map*(domain: Domain, element: string, expr: Expression): Expression =
  Expression(
    exprType: etComprehension,
    exprCompDomain: domain,
    exprCompElement: element,
    exprCompExpr: expr
  )  

proc `$`*(f: FunctionDeclaration): string =
  var clausesStrings = newSeq[string]()
  for clause in f.clauses:
    let
      argString = clause.mapIt(it.name).join(", ")
      domainString = clause.mapIt($it.domain).join(", ")
    clausesStrings.add "$1: $2" % [argString, domainString]
  "$1<$2>" % [f.name, clausesStrings.join(", ")]

proc applyFun*(args: varargs[Expression, e]): Expression =
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
