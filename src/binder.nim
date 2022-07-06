import strutils
import parser, lexer, dtype

type
   BoundUnaryOperatorKind* = enum
      bound_unary_identity,
      bound_unary_negation,

   BoundBinaryOperatorKind* = enum
      bound_binary_addition,
      bound_binary_subtraction,
      bound_binary_multiplication,
      bound_binary_division

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

func getUnaryOperatorKind(kind: TokenKind): BoundUnaryOperatorKind =
   case kind
   of token_plus: return bound_unary_identity
   of token_minus: return bound_unary_negation
   else: raise (ref Exception)(msg: "Unexpected unary operator " & escape($kind))


func getBinaryOperatorKind(kind: TokenKind): BoundBinaryOperatorKind =
   case kind
   of token_plus: return bound_binary_addition
   of token_minus: return bound_binary_subtraction
   of token_star: return bound_binary_multiplication
   of token_slash: return bound_binary_division
   else: raise (ref Exception)(msg: "Unexpected binary operator " & escape($kind))

func bindExpression(binder: Binder, node: Node): Bound

func bindLiteralExpression(binder: Binder, node: Node): Bound =
   assert node.kind == node_literal
   case node.literalToken.kind
   of token_number:
      let value = Value(dtype: tint, valInt: node.literalToken.valInt)
      return Bound(kind: bound_literal_expression, value: value)
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literalToken.kind))


func bindUnaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == node_unary_expression
   let operand = binder.bindExpression(node.unaryOperand)
   let operatorKind = getUnaryOperatorKind(node.unaryOperator.kind)
   return Bound(kind: bound_unary_expression, unaryOperator: operatorKind,
         unaryOperand: operand)

func bindBinaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == node_binary_expression
   let boundLeft = binder.bindExpression(node.left)
   let operatorKind = getBinaryOperatorKind(node.binaryOperator.kind)
   let boundRight = binder.bindExpression(node.right)
   return Bound(kind: bound_binary_expression, binaryLeft: boundLeft,
         binaryRight: boundRight, binaryOperator: operatorKind)

func bindExpression(binder: Binder, node: Node): Bound =
   case node.kind
   of node_literal:
      return binder.bindLiteralExpression(node)
   of node_unary_expression:
      return binder.bindUnaryExpression(node)
   of node_binary_expression:
      return binder.bindBinaryExpression(node)
   else: raise (ref Exception)(msg: "Unexpected syntax node " & escape($node))

func newBinder*(node: Node): Binder =
   let root = result.bindExpression(node)
