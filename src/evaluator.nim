import tables
import identifiers, evaluator_ops, binder

type
   VariableKind* = enum vkValue, vkImplementation

   Variable* = ref object
      case kind: VariableKind
      of vkValue: value*: Value
      of vkImplementation: implementation*: Bound

   Evaluator* = ref object
      variables*: Table[string, Variable]
      parent*: Evaluator

func `$`*(self: Variable): string =
   case self.kind
   of vkValue: return $self.value
   of vkImplementation:
      if self.implementation.isNil: return "[func decl]"
      else: return "[func impl]"

func tryLookup*(self: Evaluator, name: string): Variable =
   if name in self.variables: return self.variables[name]
   elif not self.parent.isNil: return self.parent.tryLookup(name)
   else: return nil

func evaluate*(self: var Evaluator, node: Bound): Value =
   case node.kind
   of boundError: return Value(dtype: Dtype(base: terror))
   of boundRoot: return Value(dtype: Dtype(base: terror))
   of boundLiteralExpression: return node.value
   of boundIdentifierExpression: return self.tryLookup(node.identifier.name).value
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
      of boundBinaryCombinedComparison:
         let res: int =
            if (left > right).valBool: 1
            elif (left < right).valBool: -1
            else: 0
         return Value(dtype: Dtype(base: tint), valInt: res)
      of boundBinaryLogicalAnd: return left and right
      of boundBinaryLogicalOr: return left or right
      of boundBinaryLogicalXor: return left xor right
   of boundAssignmentExpression:
      let rvalue = self.evaluate(node.rvalue)
      let lvalue = node.lvalue
      self.variables[lvalue.text] = Variable(kind: vkValue, value: rvalue)
      return rvalue
   of boundDefinitionExpression:
      var value = Value(dtype: node.defDtype)
      if node.defDtype.base == tfunc:
         let variable = Variable(kind: vkImplementation, implementation: node.defBound)
         self.variables[node.defIdentifier.text] = variable
      else:
         if node.defBound != nil:
            value = self.evaluate(node.defBound)
         self.variables[node.defIdentifier.text] = Variable(kind: vkValue, value: value)
      return Value(dtype: Dtype(base: tvoid))
   of boundCallExpression:
      case node.callIdentifier.name
      of "print":
         let val = self.evaluate(node.callArguments[0])
         debugEcho $val
         return Value(dtype: Dtype(base: tvoid))
      else:
         let impl = self.tryLookup(node.callIdentifier.name).implementation
         return self.evaluate(impl)
   of boundConditionalExpression:
      if node.condition == nil or self.evaluate(node.condition).valBool:
         return self.evaluate(node.conditional)
      elif node.otherwise != nil:
         return self.evaluate(node.otherwise)
      else: return Value(dtype: Dtype(base: terror))
   of boundWhileExpression:
      while self.evaluate(node.whileCondition).valBool:
         discard self.evaluate(node.whileBody)
      return Value(dtype: Dtype(base: tvoid))
   of boundBlockExpression:
      var scope = Evaluator(parent: self)
      var lastValue = Value(dtype: Dtype(base: tvoid))
      for expression in node.blockExpressions:
         lastValue = scope.evaluate(expression)
      return lastValue
