import binder

func lower*(bound: Bound): Bound

func lowerBoundError(bound: Bound): Bound =
   return bound

func lowerBoundRoot(bound: Bound): Bound =
   return bound.main.lower()

func lowerBoundLiteral(bound: Bound): Bound =
   return bound

func lowerBoundIdentifier(bound: Bound): Bound =
   return bound

func lowerBoundUnaryOperator(bound: Bound): Bound =
   let operand = bound.unaryOperand.lower()
   if operand == bound.unaryOperand: return bound
   return Bound(kind: boundUnaryOperator, unaryOperator: bound.unaryOperator, unaryOperand: operand)

func lowerBoundBinaryOperator(bound: Bound): Bound =
   let left = bound.binaryLeft.lower()
   let right = bound.binaryRight.lower()
   if left == bound.binaryLeft and right == bound.binaryRight: return bound
   return Bound(kind: boundBinaryOperator, binaryLeft: left, binaryOperator: bound.binaryOperator,
         binaryRight: right)

func lowerBoundAssignment(bound: Bound): Bound =
   let rvalue = bound.rvalue.lower()
   if rvalue == bound.rvalue: return bound
   return Bound(kind: boundAssignment, lvalue: bound.lvalue, rvalue: rvalue)

func lowerBoundDefinition(bound: Bound): Bound =
   let initialization = bound.defInitialization.lower()
   if initialization == bound.defInitialization: return bound
   return Bound(kind: boundDefinition, defIdentifier: bound.defIdentifier, defDtype: bound.defDtype,
         defInitialization: initialization)

func lowerBoundFunctionCall(bound: Bound): Bound =
   var newArguments = newSeq[Bound](bound.callArguments.len())
   var dirty = false
   for arg in bound.callArguments:
      let newArg = arg.lower()
      if newArg != arg: dirty = true
      newArguments.add(newArg)
   if not dirty: return bound
   return Bound(kind: boundFunctionCall, callIdentifier: bound.callIdentifier,
         callArguments: newArguments)

func lowerBoundConditional(bound: Bound): Bound =
   let condition = bound.condition.lower()
   let conditional = bound.conditional.lower()
   let otherwise = bound.otherwise.lower()
   if condition == bound.condition and conditional == bound.conditional and
         otherwise == bound.otherwise: return bound
   return Bound(kind: boundConditional, conditionToken: bound.conditionToken, condition: condition,
         conditional: conditional, otherwise: otherwise)

func lowerBoundWhileLoop(bound: Bound): Bound =
   let condition = bound.whileCondition.lower()
   let body = bound.whileBody.lower()
   if condition == bound.whileCondition and body == bound.whileBody: return bound
   return Bound(kind: boundWhileLoop, whileCondition: condition, whileBody: body)

func lowerBoundReturn(bound: Bound): Bound =
   let returnExpr = bound.returnExpr.lower()
   if returnExpr == bound.returnExpr: return bound
   return Bound(kind: boundReturn, returnExpr: returnExpr)

func lowerBoundBlock(bound: Bound): Bound =
   var newExpressions = newSeq[Bound](bound.blockExpressions.len())
   var dirty = false
   for expression in bound.blockExpressions:
      let newExpression = expression.lower()
      if newExpression != expression: dirty = true
      newExpressions.add(newExpression)
   if not dirty: return bound
   return Bound(kind: boundBlock, blockExpressions: newExpressions)

func lower*(bound: Bound): Bound =
   if bound.isNil: return nil
   case bound.kind
   of boundError:
      result = lowerBoundError(bound)
   of boundRoot:
      result = lowerBoundRoot(bound)
   of boundLiteral:
      result = lowerBoundLiteral(bound)
   of boundIdentifier:
      result = lowerBoundIdentifier(bound)
   of boundUnaryOperator:
      result = lowerBoundUnaryOperator(bound)
   of boundBinaryOperator:
      result = lowerBoundBinaryOperator(bound)
   of boundAssignment:
      result = lowerBoundAssignment(bound)
   of boundDefinition:
      result = lowerBoundDefinition(bound)
   of boundFunctionCall:
      result = lowerBoundFunctionCall(bound)
   of boundConditional:
      result = lowerBoundConditional(bound)
   of boundWhileLoop:
      result = lowerBoundWhileLoop(bound)
   of boundReturn:
      result = lowerBoundReturn(bound)
   of boundBlock:
      result = lowerBoundBlock(bound)

   if result != bound:
      result.scope = bound.scope
      result.dtype = bound.dtype
