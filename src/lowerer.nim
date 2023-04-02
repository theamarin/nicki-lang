import binder

func generateLabel(bound: Bound, suffix = ""): BoundLabel =
   result = BoundLabel(name: "label_" & $bound.binder.nextLabel)
   if suffix.len > 0: result.name &= "_" & suffix
   bound.binder.nextLabel.inc()

func lower*(bound: Bound): Bound

func lowerRoot(bound: Bound): Bound =
   return bound.main.lower()

func lowerUnaryOperator(bound: Bound): Bound =
   let operand = bound.unaryOperand.lower()
   if operand == bound.unaryOperand: return bound
   result = Bound(kind: boundUnaryOperator, unaryOperator: bound.unaryOperator,
         unaryOperand: operand)
   result.inherit(bound)

func lowerBinaryOperator(bound: Bound): Bound =
   let left = bound.binaryLeft.lower()
   let right = bound.binaryRight.lower()
   if left == bound.binaryLeft and right == bound.binaryRight: return bound
   result = Bound(kind: boundBinaryOperator, binaryLeft: left, binaryOperator: bound.binaryOperator,
         binaryRight: right)
   result.inherit(bound)

func lowerAssignment(bound: Bound): Bound =
   let rvalue = bound.rvalue.lower()
   if rvalue == bound.rvalue: return bound
   result = Bound(kind: boundAssignment, lvalue: bound.lvalue, rvalue: rvalue)
   result.inherit(bound)

func lowerDefinition(bound: Bound): Bound =
   let initialization = bound.defInitialization.lower()
   if initialization == bound.defInitialization: return bound
   result = Bound(kind: boundDefinition, defIdentifier: bound.defIdentifier,
         defDtype: bound.defDtype, defInitialization: initialization)
   result.inherit(bound)

func lowerFunctionCall(bound: Bound): Bound =
   var newArguments = newSeq[Bound]()
   var dirty = false
   for arg in bound.callArguments:
      let newArg = arg.lower()
      if newArg != arg: dirty = true
      newArguments.add(newArg)
   if not dirty: return bound
   result = Bound(kind: boundFunctionCall, callIdentifier: bound.callIdentifier,
         callArguments: newArguments)
   result.inherit(bound)

func lowerConditional(bound: Bound): Bound =
   # if <condition>
   #      <conditional>
   # else
   #      <otherwise>
   #
   # ---->
   #
   # gotoIfFalse <condition> else
   # <conditional>
   # goto endif
   # else:
   # <otherwise>
   # endif:

   result = Bound(kind: boundBlock)
   result.inherit(bound)
   if not bound.condition.isNil:
      let elseLabel = bound.generateLabel("else")
      let endLabel = bound.generateLabel("endif")

      result.blockExpressions.add(Bound(kind: boundConditionalGoto, gotoLabel: elseLabel,
            gotoCondition: bound.condition, gotoIfTrue: false))
      result.blockExpressions.add(bound.conditional)
      result.blockExpressions.add(Bound(kind: boundGoto, label: endLabel))
      result.blockExpressions.add(Bound(kind: boundLabel, label: elseLabel))
      if not bound.otherwise.isNil:
         result.blockExpressions.add(bound.otherwise)
      result.blockExpressions.add(Bound(kind: boundLabel, label: endLabel))
   else:
      assert not bound.conditional.isNil
      result.blockExpressions.add(bound.conditional)

   for expression in result.blockExpressions: expression.inherit(bound)
   return result.lower()


func lowerWhileLoop(bound: Bound): Bound =
   # while <condition>:
   #      <body>
   #
   # ----->
   #
   # continue:
   # gotoIfNot <condition> break
   # <body>
   # goto continue
   # break:
   result = Bound(kind: boundBlock)
   result.inherit(bound)

   let continueLabel = bound.generateLabel("continue")
   let breakLabel = bound.generateLabel("break")

   result.blockExpressions.add(Bound(kind: boundLabel, label: continueLabel))
   result.blockExpressions.add(Bound(kind: boundConditionalGoto, gotoLabel: breakLabel,
      gotoCondition: bound.whileCondition, gotoIfTrue: false))
   result.blockExpressions.add(bound.whileBody)
   result.blockExpressions.add(Bound(kind: boundGoto, label: continueLabel))
   result.blockExpressions.add(Bound(kind: boundLabel, label: breakLabel))
   for expression in result.blockExpressions: expression.inherit(bound)
   return result.lower()

func lowerReturn(bound: Bound): Bound =
   let returnExpr = bound.returnExpr.lower()
   if returnExpr == bound.returnExpr: return bound
   result = Bound(kind: boundReturn, returnExpr: returnExpr)
   result.inherit(bound)

func flattenBlock(bound: Bound): Bound =
   assert not bound.isNil
   var newExpressions: seq[Bound]
   var stack: seq[Bound]
   stack.add(bound)
   while stack.len > 0:
      var current = stack.pop()
      assert not current.isNil
      let currentLen = stack.len()
      if current.kind == boundBlock:
         for s in current.blockExpressions:
            assert not s.isNil
            stack.insert(s, currentLen)
      else:
         newExpressions.add(current)
   result = Bound(kind: boundBlock, blockExpressions: newExpressions)
   result.inherit(bound)

func lowerBlock(bound: Bound): Bound =
   var newExpressions = newSeq[Bound]()
   var dirty = false
   for expression in bound.blockExpressions:
      let newExpression = expression.lower()
      if newExpression != expression: dirty = true
      newExpressions.add(newExpression)
   if not dirty: return bound
   result = Bound(kind: boundBlock, blockExpressions: newExpressions)
   result.inherit(bound)
   return flattenBlock(result)

func lowerConditionalGoto(bound: Bound): Bound =
   let condition = bound.gotoCondition.lower()
   if condition == bound.gotoCondition: return bound
   result = Bound(kind: boundConditionalGoto, gotoCondition: condition)
   result.inherit(bound)

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
