import tables
import identifiers, evaluator_ops, binder

type
   Evaluator* = ref object
      variables*: Table[string, Value]

func evaluate*(self: var Evaluator, node: Bound): Value =
   case node.kind
   of boundError: return Value(dtype: terror)
   of boundLiteralExpression: return node.value
   of boundIdentifierExpression: return self.variables[node.identifier.name]
   of boundUnaryExpression:
      case node.unaryOperator
      of boundUnaryPlus: return self.evaluate(node.unaryOperand)
      of boundUnaryMinus: return self.evaluate(node.unaryOperand).negative
      of boundUnaryNot: return self.evaluate(node.unaryOperand).logicalNot
   of boundBinaryExpression:
      let left = self.evaluate(node.binaryLeft)
      let right = self.evaluate(node.binaryRight)
      case node.binaryOperator
      of boundBinaryAddition: return left + right
      of boundBinarySubtraction: return left - right
      of boundBinaryMultiplication: return left * right
      of boundBinaryDivision: return left div right
      of boundBinaryEquals: return left == right
      of boundBinaryNotEquals: return left != right
      of boundBinaryGreaterThan: return left > right
      of boundBinaryGreaterEquals: return left >= right
      of boundBinaryLessThan: return left < right
      of boundBinaryLessEquals: return left <= right
      of boundBinaryCompinedComparison:
         let res: int =
            if (left > right).valBool: 1
            elif (left < right).valBool: -1
            else: 0
         return Value(dtype: tint, valInt: res)
      of boundBinaryLogicalAnd: return left and right
      of boundBinaryLogicalOr: return left or right
      of boundBinaryLogicalXor: return left xor right
   of boundAssignmentExpression:
      let rvalue = self.evaluate(node.rvalue)
      let lvalue = node.lvalue
      self.variables[lvalue.text] = rvalue
      return rvalue
   of boundDefinitionExpression:
      var value = Value(dtype: node.defDtype)
      if node.defValue != nil:
         case node.defValue.dtype:
         of tint, tbool: value = self.evaluate(node.defValue)
         else: discard
      self.variables[node.defIdentifier.text] = value
      return Value(dtype: tvoid)
   of boundConditionalExpression:
      if node.condition == nil or self.evaluate(node.condition).valBool:
         return self.evaluate(node.conditional)
      elif node.otherwise != nil:
         return self.evaluate(node.otherwise)
      else: return Value(dtype: terror)
   of boundWhileExpression:
      while self.evaluate(node.whileCondition).valBool:
         discard self.evaluate(node.whileBody)
      return Value(dtype: tvoid)
   of boundBlockExpression:
      for expression in node.blockExpressions:
         discard self.evaluate(expression)
      return Value(dtype: tvoid)
