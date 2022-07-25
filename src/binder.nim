import strutils, tables
import parser, lexer, dtype, diagnostics

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
      boundLiteralExpression = "literal expression",
      boundUnaryExpression = "unary expression",
      boundBinaryExpression = "binary expression",

   Bound* = ref object
      dtype*: Dtype
      case kind*: BoundKind
      of boundLiteralExpression:
         value*: Value
      of boundUnaryExpression:
         unaryOperator*: BoundUnaryOperatorKind
         unaryOperand*: Bound
      of boundBinaryExpression:
         binaryOperator*: BoundBinaryOperatorKind
         binaryLeft*: Bound
         binaryRight*: Bound

   Binder* = ref object
      root*: Bound
      diagnostics*: Diagnostics

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

func bindExpression(binder: Binder, node: Node): Bound

func bindLiteralExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeLiteral
   case node.literalToken.kind
   of tokenNumber:
      let value = node.literalToken.value
      return Bound(kind: boundLiteralExpression, value: value, dtype: value.dtype)
   of tokenTrue, tokenFalse:
      let value = Value(dtype: tbool, valBool: node.literalToken.kind == tokenTrue)
      return Bound(kind: boundLiteralExpression, value: value, dtype: value.dtype)
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literalToken.kind))


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

func bindExpression(binder: Binder, node: Node): Bound =
   case node.kind
   of nodeLiteral:
      return binder.bindLiteralExpression(node)
   of nodeUnaryExpression:
      return binder.bindUnaryExpression(node)
   of nodeBinaryExpression:
      return binder.bindBinaryExpression(node)
   of nodeParanthesisExpression:
      return binder.bindExpression(node.expression)

func newBinder*(node: Node): Binder =
   result = Binder()
   result.root = result.bindExpression(node)
