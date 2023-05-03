import tables, strutils
import identifiers, evaluator_ops, binder
export evaluator_ops

type
   Variable* = ref object
      value*: Value
      implementation*: Bound

   Evaluator* = ref object
      variables*: OrderedTable[Identifier, Variable]
      parent*: Evaluator
      context*: Evaluator

func `$`*(self: Variable): string =
   result = $self.value

func typeStr*(self: Variable): string =
   result = $self.value.base

func tryLookup*(self: Evaluator, identifier: Identifier): Variable =
   if identifier in self.variables: return self.variables[identifier]
   elif not self.parent.isNil: return self.parent.tryLookup(identifier)
   else: raise (ref KeyError)(msg: "Undefined identifier " & escape(identifier.name))

func typeVariable(base: DtypeBase): Variable =
   Variable(value: Value(base: tcomposed, composed: ComposedValue(kind: ttype, dtype: newDtype(base))))

func addBaseDtypes*(self: Evaluator, bound: Bound) =
   var ids: seq[string]
   for id, ident in bound.scope.identifiers: ids.add($id)
   for dtypeBase in DtypeBase:
      if dtypeBase == tcomposed: continue
      let identifier = bound.scope.identifiers[$dtypeBase]
      self.variables[identifier] = typeVariable(dtypeBase)

func evaluate*(self: Evaluator, node: Bound): Value

func evaluateBinaryOperator(self: Evaluator, node: Bound): Value =
   assert node.kind == boundBinaryOperator
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
      return Value(base: tint, valInt: res)
   of boundBinaryLogicalAnd: return left and right
   of boundBinaryLogicalOr: return left or right
   of boundBinaryLogicalXor: return left xor right


func evaluateBlock*(self: Evaluator, node: Bound): Value =
   assert node.kind == boundBlock
   var scope = Evaluator(parent: self)
   var labelToIndex = newTable[BoundLabel, int]()
   for index, expression in node.blockExpressions:
      if expression.kind == boundLabel:
         labelToIndex[expression.label] = index

   var lastValue = Value(base: tvoid)
   var index = 0
   while index < node.blockExpressions.len:
      var expression = node.blockExpressions[index]
      case expression.kind
      of boundGoto:
         index = labelToIndex[expression.label]
      of boundConditionalGoto:
         let condition = scope.evaluate(expression.gotoCondition)
         if condition.valBool == expression.gotoIfTrue:
            index = labelToIndex[expression.gotoLabel]
      of boundLabel:
         discard
      of boundReturn:
         if expression.returnExpr.isNil: return lastValue
         else: return self.evaluate(expression.returnExpr)
      else:
         if node.dtype.base != tvoid:
            lastValue = self.evaluate(expression)
         else: discard self.evaluate(expression)
      index.inc()
   return lastValue

func evaluate*(self: Evaluator, node: Bound): Value =
   case node.kind
   of boundError: return Value(base: terror)
   of boundRoot: return Value(base: terror)
   of boundLiteral: return node.value
   of boundIdentifier: return self.tryLookup(node.identifier).value
   of boundUnaryOperator:
      case node.unaryOperator
      of boundUnaryPlus: return self.evaluate(node.unaryOperand)
      of boundUnaryMinus: return self.evaluate(node.unaryOperand).negative
      of boundUnaryNot: return self.evaluate(node.unaryOperand).logicalNot
   of boundBinaryOperator:
      return self.evaluateBinaryOperator(node)
   of boundStruct:
      doAssert node.dtype.isComposedType(ttype)
      let s = ComposedValue(kind: ttype, dtype: node.dtype)
      return Value(pos: node.pos, base: tcomposed, composed: s)
   of boundAssignment:
      let rvalue = self.evaluate(node.rvalue)
      let variable = self.tryLookup(node.lvalue)
      variable.value = rvalue
      return rvalue
   of boundDefinition:
      if node.defDtype.isComposedType(tfunc):
         let variable = Variable(value: Value(base: node.defDtype.base),
               implementation: node.defInitialization)
         self.variables[node.defIdentifier] = variable
      else:
         var value: Value
         if node.defInitialization.isNil:
            value = node.defDtype.toValue()
         else:
            value = self.evaluate(node.defInitialization)
         self.variables[node.defIdentifier] = Variable(value: value)
      return Value(base: tvoid)
   of boundFunctionCall:
      case node.callIdentifier.name
      of "print":
         let val = self.evaluate(node.callArguments[0])
         debugEcho $val
         return Value(base: tvoid)
      else:
         let impl = self.tryLookup(node.callIdentifier).implementation
         var scope = Evaluator(parent: self)
         for idx, p in node.callIdentifier.dtype.composed.parameters:
            let arg = self.evaluate(node.callArguments[idx])
            scope.variables[p] = Variable(value: arg)
         assert not impl.isNil
         result = scope.evaluate(impl)
         return result
   of boundConditional:
      if node.condition == nil or self.evaluate(node.condition).valBool:
         var scope = Evaluator(parent: self)
         result = scope.evaluate(node.conditional)
      elif node.otherwise != nil:
         var scope = Evaluator(parent: self)
         result = scope.evaluate(node.otherwise)
      else: return Value(base: tvoid)
      if node.dtype.base == tvoid:
         return Value(base: tvoid)
   of boundWhileLoop:
      while self.evaluate(node.whileCondition).valBool:
         discard self.evaluate(node.whileBody)
      return Value(base: tvoid)
   of boundBlock:
      return self.evaluateBlock(node)
   else:
      raise (ref KeyError)(msg: "Cannot evaluate node " & escape($node.kind))

func newEvaluator*(binder: Binder): Evaluator =
   result = Evaluator()
   result.addBaseDtypes(binder.root)
