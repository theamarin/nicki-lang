import strutils, strformat, tables
import parser, lexer, dtype, diagnostics

type
   Identifier* = ref object
      name*: string
      dtype*: Dtype
   BoundScope* = ref object
      identifiers*: Table[string, Identifier]
      parent*: BoundScope

func tryDeclare*(self: BoundScope, identifier: Identifier): bool =
   if identifier.name in self.identifiers: return false
   self.identifiers[identifier.name] = identifier
   return true

func tryLookup*(self: BoundScope, name: string): Identifier =
   if name in self.identifiers: return self.identifiers[name]
   if self.parent != nil: return self.parent.tryLookup(name)
   return nil


type
   BoundUnaryOperatorKind* = enum
      boundUnaryPlus
      boundUnaryMinus
      boundUnaryNot

   BoundUnaryOperatorMatch = tuple
      tokenKind: TokenKind
      operandDtype: Dtype
   BoundUnaryOperatorResult = tuple
      operatorKind: BoundUnaryOperatorKind
      resultDtype: Dtype
   BoundUnaryOperator = tuple
      match: BoundUnaryOperatorMatch
      result: BoundUnaryOperatorResult
   BoundUnaryOperators = Table[BoundUnaryOperatorMatch, BoundUnaryOperatorResult]

   BoundBinaryOperatorKind* = enum
      boundBinaryAddition,
      boundBinarySubtraction,
      boundBinaryMultiplication,
      boundBinaryDivision,
      boundBinaryEquals,
      boundBinaryNotEquals,
      boundBinaryGreaterThan,
      boundBinaryGreaterEquals,
      boundBinaryLessThan,
      boundBinaryLessEquals,
      boundBinaryLogicalAnd,
      boundBinaryLogicalOr
      boundBinaryLogicalXor

   BoundBinaryOperatorMatch = tuple
      leftDtype: Dtype
      tokenKind: TokenKind
      rightDtype: Dtype
   BoundBinaryOperatorResult = tuple
      operatorKind: BoundBinaryOperatorKind
      resultDtype: Dtype
   BoundBinaryOperator = tuple
      match: BoundBinaryOperatorMatch
      result: BoundBinaryOperatorResult
   BoundBinaryOperators = Table[BoundBinaryOperatorMatch, BoundBinaryOperatorResult]


   BoundKind* = enum
      boundLiteralExpression = "literal expression"
      boundIdentifierExpression = "identifier expression"
      boundUnaryExpression = "unary expression"
      boundBinaryExpression = "binary expression"
      boundAssignmentExpression = "assignment expression"

   Bound* = ref object
      dtype*: Dtype
      case kind*: BoundKind
      of boundLiteralExpression:
         value*: Value
      of boundIdentifierExpression:
         identifier*: Identifier
      of boundUnaryExpression:
         unaryOperator*: BoundUnaryOperatorKind
         unaryOperand*: Bound
      of boundBinaryExpression:
         binaryOperator*: BoundBinaryOperatorKind
         binaryLeft*: Bound
         binaryRight*: Bound
      of boundAssignmentExpression:
         lvalue*: Token
         assignment*: Token
         rvalue*: Bound

   Binder* = ref object
      root*: Bound
      diagnostics*: Diagnostics
      scope*: BoundScope

const
   boundUnaryOperatorList: seq[BoundUnaryOperator] = @[
      ((tokenPlus, tint), (boundUnaryPlus, tint)),
      ((tokenMinus, tint), (boundUnaryMinus, tint)),
      ((tokenBang, tbool), (boundUnaryNot, tbool)),
   ]
   boundUnaryOperators: BoundUnaryOperators = boundUnaryOperatorList.toTable

   boundBinaryOperatorList: seq[BoundBinaryOperator] = @[
      ((tint, tokenPlus, tint), (boundBinaryaddition, tint)),
      ((tint, tokenMinus, tint), (boundBinarySubtraction, tint)),
      ((tint, tokenStar, tint), (boundBinaryMultiplication, tint)),
      ((tint, tokenSlash, tint), (boundBinaryDivision, tint)),

      ((tbool, tokenEqualsEquals, tbool), (boundBinaryEquals, tbool)),
      ((tbool, tokenBangEquals, tbool), (boundBinaryNotEquals, tbool)),
      ((tbool, tokenGreater, tbool), (boundBinaryGreaterThan, tbool)),
      ((tbool, tokenGreaterEquals, tbool), (boundBinaryGreaterEquals, tbool)),
      ((tbool, tokenLess, tbool), (boundBinaryLessThan, tbool)),
      ((tbool, tokenLessEquals, tbool), (boundBinaryLessEquals, tbool)),

      ((tint, tokenEqualsEquals, tint), (boundBinaryEquals, tbool)),
      ((tint, tokenBangEquals, tint), (boundBinaryNotEquals, tbool)),
      ((tint, tokenGreater, tint), (boundBinaryGreaterThan, tbool)),
      ((tint, tokenGreaterEquals, tint), (boundBinaryGreaterEquals, tbool)),
      ((tint, tokenLess, tint), (boundBinaryLessThan, tbool)),
      ((tint, tokenLessEquals, tint), (boundBinaryLessEquals, tbool)),

      ((tbool, tokenAmpAmp, tbool), (boundBinaryLogicalAnd, tbool)),
      ((tbool, tokenPipePipe, tbool), (boundBinaryLogicalOr, tbool)),
      ((tbool, tokenCaret, tbool), (boundBinaryLogicalXor, tbool)),
   ]
   boundBinaryOperators: BoundBinaryOperators = boundBinaryOperatorList.toTable


func getUnaryOperator(binder: Binder, token: Token,
      dtype: Dtype): BoundUnaryOperatorResult =
   if (token.kind, dtype) in boundUnaryOperators:
      return boundUnaryOperators[(token.kind, dtype)]
   elif dtype == terror: discard
   else:
      binder.diagnostics.report("Error: Unary operator " & escape($token.kind) &
            " not defined for dtype " & escape($dtype), token.pos)

func getBinaryOperator(binder: Binder, leftDtype: Dtype, token: Token,
      rightDtype: Dtype): BoundBinaryOperatorResult =
   if (leftDtype, token.kind, rightDtype) in boundBinaryOperators:
      return boundBinaryOperators[(leftDtype, token.kind, rightDtype)]
   elif terror in [leftDtype, rightDtype]: discard
   else:
      binder.diagnostics.report("Binary operator " & escape(
            $token.kind) & " not defined for dtypes " & escape($leftDtype) & " and " &
            escape($rightDtype), token.pos)



func `$`*(bound: Bound): string =
   result = $bound.kind & ": "
   case bound.kind
   of boundLiteralExpression: result &= $bound.value
   of boundIdentifierExpression:
      result &= bound.identifier.name & " of " & $bound.identifier.dtype
   of boundUnaryExpression:
      result &= "\p"
      result &= indent($bound.unaryOperator, 3) & "\p"
      result &= indent($bound.unaryOperand, 3)
   of boundBinaryExpression:
      result &= "\p"
      result &= indent($bound.binaryLeft, 3) & "\p"
      result &= indent($bound.binaryOperator, 3) & "\p"
      result &= indent($bound.binaryRight, 3)
   of boundAssignmentExpression:
      result &= "\p"
      result &= indent($bound.lvalue, 3) & "\p"
      result &= indent($bound.assignment, 3) & "\p"
      result &= indent($bound.rvalue, 3)


func bindExpression*(binder: Binder, node: Node): Bound

func bindLiteralExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeLiteral
   case node.literal.kind
   of tokenNumber:
      let value = node.literal.value
      return Bound(kind: boundLiteralExpression, value: value, dtype: value.dtype)
   of tokenTrue, tokenFalse:
      let value = Value(dtype: tbool, valBool: node.literal.kind == tokenTrue)
      return Bound(kind: boundLiteralExpression, value: value, dtype: value.dtype)
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literal.kind))

func bindIdentifierExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeIdentifier
   assert node.identifier.kind == tokenIdentifier
   let name = node.identifier.text
   let identifier = binder.scope.tryLookup(name)
   var dtype: Dtype
   if identifier == nil:
      binder.diagnostics.report("Unknown identifier " & escape(name),
            node.identifier.pos)
      dtype = terror
   else:
      dtype = identifier.dtype
   return Bound(kind: boundIdentifierExpression, dtype: dtype, identifier: identifier)

func bindUnaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeUnaryExpression
   let operand = binder.bindExpression(node.unaryOperand)
   let (operatorKind, resultDtype) = binder.getUnaryOperator(node.unaryOperator, operand.dtype)
   return Bound(kind: boundUnaryExpression, unaryOperator: operatorKind,
         unaryOperand: operand, dtype: resultDtype)

func bindBinaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeBinaryExpression
   let boundLeft = binder.bindExpression(node.left)
   let boundRight = binder.bindExpression(node.right)
   let (operatorKind, resultDtype) = binder.getBinaryOperator(boundLeft.dtype,
         node.binaryOperator, boundRight.dtype)
   return Bound(kind: boundBinaryExpression, binaryLeft: boundLeft,
         binaryRight: boundRight, binaryOperator: operatorKind, dtype: resultDtype)

func bindAssignmentExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeAssignmentExpression
   assert node.lvalue.kind == tokenIdentifier
   let rvalue = binder.bindExpression(node.rvalue)
   if not binder.scope.tryDeclare(Identifier(name: node.lvalue.text,
         dtype: rvalue.dtype)):
      binder.diagnostics.report(&"Identifier {escape(node.lvalue.text)} already declared",
            node.lvalue.pos)
   return Bound(kind: boundAssignmentExpression, lvalue: node.lvalue,
         assignment: node.assignment, rvalue: rvalue)

func bindExpression*(binder: Binder, node: Node): Bound =
   case node.kind
   of nodeLiteral:
      return binder.bindLiteralExpression(node)
   of nodeIdentifier:
      return binder.bindIdentifierExpression(node)
   of nodeUnaryExpression:
      return binder.bindUnaryExpression(node)
   of nodeBinaryExpression:
      return binder.bindBinaryExpression(node)
   of nodeParanthesisExpression:
      return binder.bindExpression(node.expression)
   of nodeAssignmentExpression:
      return binder.bindAssignmentExpression(node)
   of nodeCompilationUnit:
      return

func newBinder*(parent: BoundScope = nil): Binder =
   result = Binder()
   result.scope = BoundScope(parent: parent)
