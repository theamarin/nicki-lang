import dtype, binder

func evaluate*(node: Bound): Value =
   case node.kind
   of boundLiteralExpression: return node.value
   of boundUnaryExpression:
      case node.unaryOperator
      of boundUnaryPlus: return node.unaryOperand.evaluate
      of boundUnaryMinus: return node.unaryOperand.evaluate.negative
      of boundUnaryNot: return node.unaryOperand.evaluate.logicalNot
   of boundBinaryExpression:
      let left = node.binaryLeft.evaluate
      let right = node.binaryRight.evaluate
      case node.binaryOperator
      of boundBinaryAddition: return left + right
      of boundBinarySubtraction: return left - right
      of boundBinaryMultiplication: return left * right
      of boundBinaryDivision: return left div right
      of boundBinaryLogicalAnd: return left and right
      of boundBinaryLogicalOr: return left or right
