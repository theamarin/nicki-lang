import binder

func lower*(bound: Bound): Bound

func lowerRoot(bound: Bound): Bound =
   return bound.main.lower()

func lowerUnaryOperator(bound: Bound): Bound =
   let operand = bound.unaryOperand.lower()
   if operand == bound.unaryOperand: return bound
   return Bound(kind: boundUnaryOperator, unaryOperator: bound.unaryOperator, unaryOperand: operand)

func lowerBinaryOperator(bound: Bound): Bound =
   let left = bound.binaryLeft.lower()
   let right = bound.binaryRight.lower()
   if left == bound.binaryLeft and right == bound.binaryRight: return bound
   return Bound(kind: boundBinaryOperator, binaryLeft: left, binaryOperator: bound.binaryOperator,
         binaryRight: right)

func lowerAssignment(bound: Bound): Bound =
   let rvalue = bound.rvalue.lower()
   if rvalue == bound.rvalue: return bound
   return Bound(kind: boundAssignment, lvalue: bound.lvalue, rvalue: rvalue)

func lowerDefinition(bound: Bound): Bound =
   let initialization = bound.defInitialization.lower()
   if initialization == bound.defInitialization: return bound
   return Bound(kind: boundDefinition, defIdentifier: bound.defIdentifier, defDtype: bound.defDtype,
         defInitialization: initialization)

func lowerFunctionCall(bound: Bound): Bound =
   var newArguments = newSeq[Bound](bound.callArguments.len())
   var dirty = false
   for arg in bound.callArguments:
      let newArg = arg.lower()
      if newArg != arg: dirty = true
      newArguments.add(newArg)
   if not dirty: return bound
   return Bound(kind: boundFunctionCall, callIdentifier: bound.callIdentifier,
         callArguments: newArguments)

func lowerConditional(bound: Bound): Bound =
   let condition = bound.condition.lower()
   let conditional = bound.conditional.lower()
   let otherwise = bound.otherwise.lower()
   if condition == bound.condition and conditional == bound.conditional and
         otherwise == bound.otherwise: return bound
   return Bound(kind: boundConditional, conditionToken: bound.conditionToken, condition: condition,
         conditional: conditional, otherwise: otherwise)

func lowerWhileLoop(bound: Bound): Bound =
   let condition = bound.whileCondition.lower()
   let body = bound.whileBody.lower()
   if condition == bound.whileCondition and body == bound.whileBody: return bound
   return Bound(kind: boundWhileLoop, whileCondition: condition, whileBody: body)

func lowerReturn(bound: Bound): Bound =
   let returnExpr = bound.returnExpr.lower()
   if returnExpr == bound.returnExpr: return bound
   return Bound(kind: boundReturn, returnExpr: returnExpr)

func flattenBlock(bound: Bound): Bound =
   var newExpressions: seq[Bound]
   var stack: seq[Bound]
   stack.add(bound)
   while stack.len > 0:
      var current = stack.pop()
      let currentLen = stack.len()
      if current.kind == boundBlock:
         for s in current.blockExpressions:
            stack.insert(s, currentLen)
      else:
         newExpressions.add(current)
   result = Bound(kind: boundBlock, blockExpressions: newExpressions)

func lowerBlock(bound: Bound): Bound =
   var newExpressions = newSeq[Bound](bound.blockExpressions.len())
   var dirty = false
   for expression in bound.blockExpressions:
      let newExpression = expression.lower()
      if newExpression != expression: dirty = true
      newExpressions.add(newExpression)
   if not dirty: return bound
   result = Bound(kind: boundBlock, blockExpressions: newExpressions)
   return flattenBlock(result)

func lowerConditionalGoto(bound: Bound): Bound =
   let condition = bound.gotoCondition.lower()
   if condition == bound.condition: return bound
   return Bound(kind: boundConditionalGoto, gotoCondition: condition)

func lower*(bound: Bound): Bound =
   if bound.isNil: return nil
   case bound.kind
   of boundError, boundLiteral, boundIdentifier, boundLabel, boundGoto:
      return bound
   of boundRoot:
      result = lowerRoot(bound)
   of boundUnaryOperator:
      result = lowerUnaryOperator(bound)
   of boundBinaryOperator:
      result = lowerBinaryOperator(bound)
   of boundAssignment:
      result = lowerAssignment(bound)
   of boundDefinition:
      result = lowerDefinition(bound)
   of boundFunctionCall:
      result = lowerFunctionCall(bound)
   of boundConditional:
      result = lowerConditional(bound)
   of boundWhileLoop:
      result = lowerWhileLoop(bound)
   of boundReturn:
      result = lowerReturn(bound)
   of boundBlock:
      result = lowerBlock(bound)
   of boundConditionalGoto:
      result = lowerConditionalGoto(bound)

   if result != bound:
      result.binder = bound.binder
      result.scope = bound.scope
      result.dtype = bound.dtype
