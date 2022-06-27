import strutils

import parser, lexer



func evaluate*(node: Node): int =
   case node.kind
   of node_number: return node.numberToken.value
   of node_binary_expression:
      let left = node.left.evaluate
      let right = node.right.evaluate
      case node.operatorToken.kind
      of token_plus: return left + right
      of token_minus: return left - right
      of token_star: return left * right
      of token_slash: return left div right
      else: raise newException(ValueError, "Unexpected binary operator " & escape(
            $node.operatorToken.kind))
   of node_paranthesis_expression:
      return node.expression.evaluate
   # else: raise newException(ValueError, "Unexpected node " & escape($node.kind))
