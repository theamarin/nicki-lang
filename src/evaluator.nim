import strutils

import parser, lexer



func evaluate*(node: Node): int =
   case node.kind
   of nodeLiteral: return node.literalToken.valInt
   of nodeUnaryExpression:
      case node.unaryOperator.kind
      of tokenPlus: return node.unaryOperand.evaluate
      of tokenMinus: return -node.unaryOperand.evaluate
      else: raise newException(ValueError, "Unexpected unary operator " & escape(
            $node.unaryOperator.kind))
   of nodeBinaryExpression:
      let left = node.left.evaluate
      let right = node.right.evaluate
      case node.binaryOperator.kind
      of tokenPlus: return left + right
      of tokenMinus: return left - right
      of tokenStar: return left * right
      of tokenSlash: return left div right
      else: raise newException(ValueError, "Unexpected binary operator " & escape(
            $node.binaryOperator.kind))
   of nodeParanthesisExpression:
      return node.expression.evaluate
   # else: raise newException(ValueError, "Unexpected node " & escape($node.kind))
