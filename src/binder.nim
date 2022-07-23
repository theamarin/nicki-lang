import strutils, tables
import parser, lexer, dtype

type
   BoundUnaryOperatorKind* = enum
      bound_unary_identity,
      bound_unary_negation,

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
      bound_binary_addition,
      bound_binary_subtraction,
      bound_binary_multiplication,
      bound_binary_division

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
      bound_literal_expression = "literal expression",
      bound_unary_expression = "unary expression",
      bound_binary_expression = "binary expression",

   Bound* = ref object
      dtype: Dtype
      case kind: BoundKind
      of bound_literal_expression:
         value: Value
      of bound_unary_expression:
         unaryOperator: BoundUnaryOperatorKind
         unaryOperand: Bound
      of bound_binary_expression:
         binaryOperator: BoundBinaryOperatorKind
         binaryLeft: Bound
         binaryRight: Bound

   Binder* = ref object
      root: Bound
      diagnostics: seq[string]

const
   boundUnaryOperatorList: seq[BoundUnaryOperator] = @[
      ((tokenPlus, tint), (bound_unary_identity, tint)),
      ((tokenMinus, tint), (bound_unary_negation, tint)),
   ]
   boundUnaryOperators: BoundUnaryOperators = boundUnaryOperatorList.toTable

   boundBinaryOperatorList: seq[BoundBinaryOperator] = @[
      ((tint, tokenPlus, tint), (bound_binary_addition, tint)),
      ((tint, tokenMinus, tint), (bound_binary_subtraction, tint)),
   ]
   boundBinaryOperators: BoundBinaryOperators = boundBinaryOperatorList.toTable


func getUnaryOperator(kind: TokenKind, dtype: Dtype): BoundUnaryOperatorResult =
   if (kind, dtype) in boundUnaryOperators: return boundUnaryOperators[(kind, dtype)]
   else: raise (ref Exception)(msg: "Unary operator " & escape($kind) &
         " not defined for dtype " & escape($dtype))


func getBinaryOperator(leftDtype: Dtype, kind: TokenKind,
      rightDtype: Dtype): BoundBinaryOperatorResult =
   if (leftDtype, kind, rightDtype) in boundBinaryOperators: return boundBinaryOperators[
         (leftDtype, kind, rightDtype)]
   else: raise (ref Exception)(msg: "Binary operator " & escape($kind) &
         " not defined for dtypes " & escape($leftDtype) & " and " & escape($rightDtype))

func bindExpression(binder: Binder, node: Node): Bound

func bindLiteralExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeLiteral
   case node.literalToken.kind
   of tokenNumber:
      let value = Value(dtype: tint, valInt: node.literalToken.valInt)
      return Bound(kind: bound_literal_expression, value: value, dtype: tint)
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literalToken.kind))


func bindUnaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeUnaryExpression
   let operand = binder.bindExpression(node.unaryOperand)
   let (operatorKind, resultDtype) = getUnaryOperator(node.unaryOperator.kind, operand.dtype)
   return Bound(kind: bound_unary_expression, unaryOperator: operatorKind,
         unaryOperand: operand, dtype: resultDtype)

func bindBinaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == nodeBinaryExpression
   let boundLeft = binder.bindExpression(node.left)
   let boundRight = binder.bindExpression(node.right)
   let (operatorKind, resultDtype) = getBinaryOperator(boundLeft.dtype,
         node.binaryOperator.kind, boundRight.dtype)
   return Bound(kind: bound_binary_expression, binaryLeft: boundLeft,
         binaryRight: boundRight, binaryOperator: operatorKind, dtype: resultDtype)

func bindExpression(binder: Binder, node: Node): Bound =
   case node.kind
   of nodeLiteral:
      return binder.bindLiteralExpression(node)
   of nodeUnaryExpression:
      return binder.bindUnaryExpression(node)
   of nodeBinaryExpression:
      return binder.bindBinaryExpression(node)
   else: raise (ref Exception)(msg: "Unexpected syntax node " & escape($node))

func newBinder*(node: Node): Binder =
   let root = result.bindExpression(node)
