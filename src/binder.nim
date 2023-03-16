import strutils, tables
import parser, lexer, binder_ops, identifiers, diagnostics


type
   BoundKind* = enum
      boundError = "error bound"
      boundRoot = "root bound"
      boundLiteralExpression = "literal bound"
      boundIdentifierExpression = "identifier bound"
      boundUnaryExpression = "unary bound"
      boundBinaryExpression = "binary bound"
      boundAssignmentExpression = "assignment bound"
      boundDefinitionExpression = "definition bound"
      boundCallExpression = "call bound"
      boundConditionalExpression = "conditional bound"
      boundWhileExpression = "while bound"
      boundBlockExpression = "block bound"

   BoundScope* = ref Table[string, Identifier]

   Bound* = ref object
      scope*: BoundScope
      parent*: Bound
      binder*: Binder
      dtype*: Dtype
      case kind*: BoundKind
      of boundError:
         errorToken*: Token
      of boundRoot: discard
      of boundLiteralExpression:
         value*: Value
         valueNode*: Node
      of boundIdentifierExpression:
         identifier*: Identifier
      of boundUnaryExpression:
         unaryOperator*: BoundUnaryOperatorKind
         unaryOperand*: Bound
      of boundBinaryExpression:
         binaryOperator*: BoundBinaryOperatorKind
         binaryLeft*: Bound
         binaryRight*: Bound
      of boundAssignmentExpression:
         lvalue*: Token
         rvalue*: Bound
      of boundDefinitionExpression:
         defIdentifier*: Token
         defDtype*: Dtype
         defBound*: Bound
      of boundCallExpression:
         callIdentifier*: Identifier
         callArguments*: seq[Bound]
      of boundConditionalExpression:
         conditionToken*: Token
         condition*: Bound # nil for "else"
         conditional*: Bound
         otherwise*: Bound # if "elif" or "else" is present
      of boundWhileExpression:
         whileCondition*: Bound
         whileBody*: Bound
      of boundBlockExpression:
         blockExpressions*: seq[Bound]

   Binder* = ref object
      root*: Bound
      diagnostics*: Diagnostics
      scope*: BoundScope

func getScope*(self: Bound): Bound =
   if self == nil: raise (ref Exception)(msg: "bound is nil!")
   elif self.scope != nil: return self
   elif self.parent != nil: return self.parent.getScope()
   else: raise (ref Exception)(msg: "No root bound exists!")

func tryDeclare*(self: Bound, identifier: Identifier): bool =
   let bound = self.getScope()
   if identifier.name in bound.scope: return false
   bound.scope[identifier.name] = identifier
   return true

func tryLookup*(self: Bound, name: string): Identifier =
   let bound = self.getScope()
   if name in bound.scope: return bound.scope[name]
   elif bound.parent != nil: return bound.parent.tryLookup(name)
   else: return nil

func addBaseDtypes*(self: Bound) =
   discard self.tryDeclare(Identifier(kind: dtypeIdentifier, name: $terror, dtype: Dtype(base: terror)))
   discard self.tryDeclare(Identifier(kind: dtypeIdentifier, name: $tvoid, dtype: Dtype(base: tvoid)))
   discard self.tryDeclare(Identifier(kind: dtypeIdentifier, name: $tbool, dtype: Dtype(base: tbool)))
   discard self.tryDeclare(Identifier(kind: dtypeIdentifier, name: $tint, dtype: Dtype(base: tint)))
   discard self.tryDeclare(Identifier(kind: dtypeIdentifier, name: $tstr, dtype: Dtype(base: tstr)))

func pos*(node: Bound): Position =
   for key, value in fieldPairs(node[]):
      when key == "kind": discard
      elif value is seq: return value[0].pos
      elif value is Token or value is Node or value is Bound: return value.pos
      else: discard
   raise (ref Exception)(msg: "No position for bound " & escape($node.kind))

func `$`*(bound: Bound): string =
   if bound.isNil: return ""
   let intro = $bound.kind & ": "
   var children: seq[string]
   for key, value in fieldPairs(bound[]):
      when key in ["kind", "scope", "parent", "binder"]: discard
      elif value is seq:
         children.add(key)
         for x in value:
            children.add(indent($x, 3))
      else:
         children.add(prettyPrint(key, $value))
   return intro & "\p" & children.join("\p").indent(3)

func toDtype*(bound: Bound, dtypeToken: Token): Dtype =
   if dtypeToken.text in basicDtypeStrings:
      return Dtype(base: basicDtypeStrings[dtypeToken.text])
   let id = bound.tryLookup(dtypeToken.text)
   if id == nil: return Dtype(base: terror)
   elif id.kind != dtypeIdentifier:
      bound.binder.diagnostics.reportWrongIdentifier(dtypeToken.pos, $id.kind, $dtypeIdentifier)

func bindExpression*(bound: Bound, node: Node): Bound

func bindErrorExpression(parent: Bound, node: Node): Bound =
   assert node.kind == errorExpression
   return Bound(kind: boundError, parent: parent, binder: parent.binder,
         errorToken: node.errorToken)

func bindLiteralExpression(parent: Bound, node: Node): Bound =
   assert node.kind == literalExpression
   result = Bound(kind: boundLiteralExpression, parent: parent, binder: parent.binder)
   case node.literal.kind
   of tokenNumber:
      result.value = node.literal.value
   of tokenTrue, tokenFalse:
      result.value = Value(dtype: Dtype(base: tbool), valBool: node.literal.kind == tokenTrue)
   of tokenString:
      result.value = node.literal.value
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literal.kind))
   result.dtype = result.value.dtype
   result.valueNode = node

func bindIdentifierExpression(parent: Bound, node: Node): Bound =
   assert node.kind == identifierExpression
   assert node.identifier.kind == tokenIdentifier
   result = Bound(kind: boundIdentifierExpression, parent: parent, binder: parent.binder)
   let name = node.identifier.text
   result.identifier = result.tryLookup(name)
   if result.identifier != nil:
      result.dtype = result.identifier.dtype
   else:
      result.binder.diagnostics.reportUndefinedIdentifier(node.identifier.pos, name)
      result.dtype = Dtype(base: terror)

func bindUnaryExpression(parent: Bound, node: Node): Bound =
   assert node.kind == unaryExpression
   result = Bound(kind: boundUnaryExpression, parent: parent, binder: parent.binder)
   result.unaryOperand = result.bindExpression(node.unaryOperand)
   let (operatorKind, resultDtype) = getUnaryOperator(result.binder.diagnostics,
         node.unaryOperator, result.unaryOperand.dtype)
   result.unaryOperator = operatorKind
   result.dtype = Dtype(base: resultDtype)

func bindBinaryExpression(parent: Bound, node: Node): Bound =
   assert node.kind == binaryExpression
   result = Bound(kind: boundBinaryExpression, parent: parent, binder: parent.binder)
   result.binaryLeft = result.bindExpression(node.left)
   result.binaryRight = result.bindExpression(node.right)
   let (operatorKind, resultDtype) = getBinaryOperator(result.binder.diagnostics,
         result.binaryLeft.dtype, node.binaryOperator, result.binaryRight.dtype)
   result.binaryOperator = operatorKind
   result.dtype = Dtype(base: resultDtype)

func bindAssignmentExpression(parent: Bound, node: Node): Bound =
   assert node.kind == assignmentExpression
   assert node.lvalue.kind == tokenIdentifier
   result = Bound(kind: boundAssignmentExpression, parent: parent, binder: parent.binder)
   result.lvalue = node.lvalue
   result.rvalue = result.bindExpression(node.rvalue)
   let identifier = result.tryLookup(node.lvalue.text)
   if identifier == nil:
      result.binder.diagnostics.reportUndefinedIdentifier(node.lvalue.pos, node.lvalue.text)
   elif identifier.dtype != result.rvalue.dtype:
      result.binder.diagnostics.reportCannotCast(node.assignment.pos, $result.rvalue.dtype,
            $identifier.dtype)
   result.dtype = result.rvalue.dtype

func bindParameter(bound: Bound, node: Node): Parameter =
   assert node.kind == parameterExpression
   result = Parameter()
   result.name = node.parameterName.text
   result.dtype = bound.toDtype(node.parameterDtype)
   result.pos = node.parameterName.pos
   if result.dtype.base == terror:
      bound.binder.diagnostics.reportUndefinedIdentifier(node.parameterDtype.pos,
            node.parameterDtype.text)

func bindDefinitionExpression(parent: Bound, node: Node): Bound =
   assert node.kind == definitionExpression
   result = Bound(kind: boundDefinitionExpression, parent: parent, binder: parent.binder)
   result.defIdentifier = node.defIdentifier
   result.dtype = Dtype(base: tvoid)
   let isFunc = node.defParameterOpen != nil
   if isFunc:
      result.defDtype = Dtype(base: tfunc)
      for parameter in node.defParameters:
         let p = result.bindParameter(parameter)
         result.defDtype.parameters.add(p)
      if node.defDtype != nil:
         result.defDtype.retDtype = result.toDtype(node.defDtype)
         if result.defDtype.retDtype.base == terror:
            result.binder.diagnostics.reportUndefinedIdentifier(node.defDtype.pos,
                  node.defDtype.text)
      else: result.defDtype.retDtype = Dtype(base: tvoid)
      if node.defAssignExpression != nil:
         result.defBound = result.bindExpression(node.defAssignExpression)
         if result.defDtype.retDtype != result.defBound.dtype:
            result.binder.diagnostics.reportCannotCast(node.defAssignToken.pos,
                  $result.defBound.dtype, $result.defDtype)
   else:
      if node.defDtype != nil:
         result.defDtype = result.toDtype(node.defDtype)
         if result.defDtype.base == terror:
            result.binder.diagnostics.reportUndefinedIdentifier(node.defDtype.pos,
                  node.defDtype.text)
      if node.defAssignExpression != nil:
         result.defBound = result.bindExpression(node.defAssignExpression)
         if result.defDtype.isNil:
            result.defDtype = result.defBound.dtype
         elif result.defDtype != result.defBound.dtype:
            result.binder.diagnostics.reportCannotCast(node.defAssignToken.pos,
                  $result.defBound.dtype, $result.defDtype)
   let identifier = newVariableIdentifier(node.defIdentifier.text, result.defDtype,
         node.defToken.pos)
   if not result.tryDeclare(identifier):
      result.binder.diagnostics.reportAlreadyDefinedIdentifier(node.defToken.pos,
            node.defToken.text)

func bindCallExpression(parent: Bound, node: Node): Bound =
   assert node.kind == callExpression
   result = Bound(kind: boundCallExpression, parent: parent, binder: parent.binder)
   result.callIdentifier = result.tryLookup(node.callIdentifier.text)
   if result.callIdentifier == nil:
      result.binder.diagnostics.reportUndefinedIdentifier(node.callIdentifier.pos,
            node.callIdentifier.text)
      result.dtype = Dtype(base: terror)
   elif result.callIdentifier.dtype.base != tfunc:
      result.binder.diagnostics.reportCannotCast(result.callIdentifier.pos,
            $result.callIdentifier.dtype, "func")
   else:
      result.dtype = result.callIdentifier.dtype.retDtype
      let parameters = result.callIdentifier.dtype.parameters
      if node.callArguments.len != parameters.len:
         result.binder.diagnostics.reportWrongNumberOfArguments(result.callIdentifier.pos,
               node.callArguments.len, parameters.len)
      else:
         for idx, arg in node.callArguments:
            let argExpression = result.bindExpression(arg.callArgExpression)
            if argExpression.dtype != parameters[idx].dtype:
               result.binder.diagnostics.reportCannotCast(argExpression.pos, $argExpression.dtype,
                     $parameters[idx].dtype)
               result.binder.diagnostics.reportDefinitionHint(parameters[idx].pos, parameters[idx].name)
            result.callArguments.add(argExpression)

func bindConditionalExpression(parent: Bound, node: Node): Bound =
   assert node.kind == conditionalExpression
   let condition =
      if node.condition != nil:
         result.bindExpression(node.condition)
      else: nil
   if condition != nil and condition.dtype.base != tbool:
      result.binder.diagnostics.reportConditionNotBoolean(node.conditionToken.pos)
   let conditional = result.bindExpression(node.conditional)
   let otherwise =
      if node.otherwise != nil: result.bindConditionalExpression(node.otherwise)
      else: nil
   if conditional.dtype.base != tvoid and node.conditionToken.kind != tokenElse:
      if otherwise == nil:
         result.binder.diagnostics.reportMissingElse(node.conditionToken.pos,
               $conditional.dtype)
      elif otherwise.dtype != conditional.dtype:
         result.binder.diagnostics.reportInconsistentConditionals(node.conditionToken.pos,
               $node.conditionToken.text, $conditional.dtype,
               $otherwise.conditionToken.text, $otherwise.dtype)
   return Bound(kind: boundConditionalExpression, parent: parent, binder: parent.binder,
         dtype: conditional.dtype, conditionToken: node.conditionToken, condition: condition,
         conditional: conditional, otherwise: otherwise)

func bindWhileExpression(parent: Bound, node: Node): Bound =
   assert node.kind == whileExpression
   result = Bound(kind: boundWhileExpression, parent: parent, binder: parent.binder)
   result.whileCondition = result.bindExpression(node.whileCondition)
   result.whileBody = result.bindExpression(node.whileBody)
   result.dtype = Dtype(base: tvoid)

func bindBlockExpression(parent: Bound, node: Node): Bound =
   assert node.kind == blockExpression
   result = Bound(kind: boundBlockExpression, parent: parent, binder: parent.binder)
   result.scope = BoundScope()
   for expression in node.blockExpressions:
      result.blockExpressions.add(result.bindExpression(expression))
   result.dtype = Dtype(base: tvoid)
   if result.blockExpressions.len > 0:
      result.dtype = result.blockExpressions[^1].dtype


func bindExpression*(bound: Bound, node: Node): Bound =
   case node.kind
   of errorExpression:
      return bound.bindErrorExpression(node)
   of literalExpression:
      return bound.bindLiteralExpression(node)
   of identifierExpression:
      return bound.bindIdentifierExpression(node)
   of unaryExpression:
      return bound.bindUnaryExpression(node)
   of binaryExpression:
      return bound.bindBinaryExpression(node)
   of paranthesisExpression:
      return bound.bindExpression(node.expression)
   of assignmentExpression:
      return bound.bindAssignmentExpression(node)
   of parameterExpression:
      raise (ref Exception)(msg: "Unexpected parameter expression")
   of definitionExpression:
      return bound.bindDefinitionExpression(node)
   of callArgumentExpression:
      raise (ref Exception)(msg: "Unexpected call argument expression")
   of callExpression:
      return bound.bindCallExpression(node)
   of conditionalExpression:
      return bound.bindConditionalExpression(node)
   of whileExpression:
      return bound.bindWhileExpression(node)
   of blockExpression:
      return bound.bindBlockExpression(node)
   of compilationUnit:
      return

func bindExpression*(binder: Binder, node: Node): Bound =
   return binder.root.bindExpression(node)

func newBinder*(parent: BoundScope = nil): Binder =
   result = Binder()
   result.root = Bound(kind: boundRoot, scope: BoundScope(), binder: result)
