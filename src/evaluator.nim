import tables, strutils
import identifiers, lexer, evaluator_ops, binder
export evaluator_ops

type
   Variable* = ref object
      value*: Value
      implementation*: Bound

   Evaluator* = ref object
      variables*: OrderedTable[string, Variable]
      parent*: Evaluator

func `$`*(self: Variable): string =
   result = $self.value

func typeStr*(self: Variable): string =
   result = $self.value.dtype

func tryLookup*(self: Evaluator, name: string): Variable =
   if name in self.variables: return self.variables[name]
   elif not self.parent.isNil: return self.parent.tryLookup(name)
   else: raise (ref KeyError)(msg: "Undefined identifier " & escape(name))

func typeVariable(base: DtypeBase): Variable =
   Variable(value: Value(dtype: Dtype(base: ttype), valDtype: Dtype(base: base)))

func addBaseDtypes*(self: Evaluator) =
   for dtypeBase in DtypeBase:
      self.variables[$dtypeBase] = typeVariable(dtypeBase)

func toValue*(self: ValueBase): Value =
   case self.dtypeBase
   of tbool: return Value(dtype: Dtype(base: tbool), valBool: self.valBool)
   of tint: return Value(dtype: Dtype(base: tint), valInt: self.valInt)
   of tstr: return Value(dtype: Dtype(base: tstr), valStr: self.valStr)
   else: raiseUnexpectedDtypeException($self.dtypeBase, "conversion to value")

func evaluate*(self: var Evaluator, node: Bound): Value =
   case node.kind
   of boundError: return Value(dtype: Dtype(base: terror))
   of boundRoot: return Value(dtype: Dtype(base: terror))
   of boundLiteral: return node.value.toValue
   of boundIdentifier: return self.tryLookup(node.identifier.name).value
   of boundUnaryOperator:
      case node.unaryOperator
      of boundUnaryPlus: return self.evaluate(node.unaryOperand)
      of boundUnaryMinus: return self.evaluate(node.unaryOperand).negative
      of boundUnaryNot: return self.evaluate(node.unaryOperand).logicalNot
   of boundBinaryOperator:
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
   of boundAssignment:
      let rvalue = self.evaluate(node.rvalue)
      let variable = self.tryLookup(node.lvalue.text)
      variable.value = rvalue
      return rvalue
   of boundDefinition:
      var value = Value(dtype: node.defDtype)
      if node.defDtype.base == tfunc:
         let variable = Variable(value: Value(dtype: node.defDtype), implementation: node.defBound)
         self.variables[node.defIdentifier.text] = variable
      else:
         if node.defBound != nil:
            value = self.evaluate(node.defBound)
         self.variables[node.defIdentifier.text] = Variable(value: value)
      return Value(dtype: Dtype(base: tvoid))
   of boundFunctionCall:
      case node.callIdentifier.name
      of "print":
         let val = self.evaluate(node.callArguments[0])
         debugEcho $val
         return Value(dtype: Dtype(base: tvoid))
      else:
         let impl = self.tryLookup(node.callIdentifier.name).implementation
         var scope = Evaluator(parent: self)
         for idx, p in node.callIdentifier.dtype.parameters:
            let arg = self.evaluate(node.callArguments[idx])
            scope.variables[p.name] = Variable(value: arg)
         assert not impl.isNil
         return scope.evaluate(impl)
   of boundConditional:
      if node.condition == nil or self.evaluate(node.condition).valBool:
         var scope = Evaluator(parent: self)
         result = scope.evaluate(node.conditional)
      elif node.otherwise != nil:
         var scope = Evaluator(parent: self)
         result = scope.evaluate(node.otherwise)
      else: return Value(dtype: Dtype(base: tvoid))
      if node.dtype.base == tvoid:
         return Value(dtype: Dtype(base: tvoid))
   of boundWhileLoop:
      while self.evaluate(node.whileCondition).valBool:
         discard self.evaluate(node.whileBody)
      return Value(dtype: Dtype(base: tvoid))
   of boundBlock:
      var scope = Evaluator(parent: self)
      var lastValue = Value(dtype: Dtype(base: tvoid))
      for expression in node.blockExpressions:
         lastValue = scope.evaluate(expression)
      return lastValue

func newEvaluator*(): Evaluator =
   result = Evaluator()
   result.addBaseDtypes()
