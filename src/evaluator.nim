import strutils

import parser, lexer



func evaluate*(node: Node): int =
   case node.kind
   of node_literal: return node.literalToken.valInt
   of node_unary_expression:
      case node.unaryOperator.kind
      of token_plus: return node.unaryOperand.evaluate
      of token_minus: return -node.unaryOperand.evaluate
      else: raise newException(ValueError, "Unexpected unary operator " & escape(
            $node.unaryOperator.kind))
   of node_binary_expression:
      let left = node.left.evaluate
      let right = node.right.evaluate
      case node.binaryOperator.kind
      of token_plus: return left + right
      of token_minus: return left - right
      of token_star: return left * right
      of token_slash: return left div right
      else: raise newException(ValueError, "Unexpected binary operator " & escape(
            $node.binaryOperator.kind))
   of node_paranthesis_expression:
      return node.expression.evaluate
   # else: raise newException(ValueError, "Unexpected node " & escape($node.kind))
