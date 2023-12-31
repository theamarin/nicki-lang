import strutils, tables, hashes
import parser, lexer, binder_ops, identifiers, diagnostics
export binder_ops


type
   BoundKind* = enum
      boundError = "error"
      boundRoot = "root"
      boundLiteral = "literal"
      boundIdentifier = "identifier"
      boundUnaryOperator = "unary"
      boundBinaryOperator = "binary"
      boundStruct = "struct"
      boundAssignment = "assignment"
      boundDefinition = "definition"
      boundFunctionCall = "function call"
      boundConditional = "conditional"
      boundWhileLoop = "while"
      boundReturn = "return"
      boundBlock = "block"
      boundLabel = "label"
      boundGoto = "goto"
      boundConditionalGoto = "conditional goto"

   BoundScope* = ref object
      functionContext*: Bound
      loopContext*: Bound
      identifiers*: OrderedTable[string, Identifier]

   BoundLabel* = ref object
      name*: string

   Bound* = ref object
      scope*: BoundScope
      parent*: Bound
      binder*: Binder
      dtype*: Dtype
      node*: Node
      case kind*: BoundKind
      of boundError:
         errorToken*: Token
      of boundRoot:
         main*: Bound
      of boundLiteral:
         value*: Value
         valueNode*: Node
      of boundIdentifier:
         identifier*: Identifier
      of boundUnaryOperator:
         unaryOperator*: BoundUnaryOperatorKind
         unaryOperand*: Bound
      of boundBinaryOperator:
         binaryLeft*: Bound
         binaryOperator*: BoundBinaryOperatorKind
         binaryRight*: Bound
      of boundStruct:
         structToken*: Token
         structMembers*: seq[Bound] # similar to dtype.dtype.members
      of boundAssignment:
         lvalue*: Identifier
         assignmentToken*: Token
         rvalue*: Bound
      of boundDefinition:
         defIdentifier*: Identifier
         defDtype*: Dtype           # same as defIdentifier.dtype
         defInitialization*: Bound
      of boundFunctionCall:
         callIdentifier*: Identifier
         callArguments*: seq[Bound]
      of boundConditional:
         conditionToken*: Token
         condition*: Bound          # nil for "else"
         conditional*: Bound
         otherwise*: Bound          # if "elif" or "else" is present
      of boundWhileLoop:
         whileCondition*: Bound
         whileBody*: Bound
      of boundReturn:
         returnExpr*: Bound         # may be nil
      of boundBlock:
         blockExpressions*: seq[Bound]
      of boundLabel, boundGoto:
         label*: BoundLabel
      of boundConditionalGoto:
         gotoCondition*: Bound
         gotoLabel*: BoundLabel
         gotoIfTrue*: bool

   Binder* = ref object
      root*: Bound
      diagnostics*: Diagnostics
      scope*: BoundScope
      nextLabel*: int

func `$`*(self: BoundLabel): string = return self.name
func asTree*(self: BoundLabel): string = return self.name
func asCode*(self: BoundLabel): string = return self.name

func asTree*(x: bool): string = $x
func asCode*(x: bool): string = $x

func hash*(self: BoundLabel): Hash = return cast[pointer](self).hash

func inherit*(self: Bound, inheriter: Bound) =
   self.scope = inheriter.scope
   self.parent = inheriter.parent
   self.binder = inheriter.binder
   self.dtype = inheriter.dtype

func getScope*(self: Bound): Bound =
   if self == nil: raise (ref Exception)(msg: "bound is nil!")
   var bound = self
   while bound != nil:
      if bound.scope != nil: return bound
      bound = bound.parent
   raise (ref Exception)(msg: "No root bound exists!")

func getFunctionContext*(self: Bound): Bound =
   if self == nil: raise (ref Exception)(msg: "bound is nil!")
   var bound = self
   while bound != nil:
      if bound.scope != nil and bound.scope.functionContext != nil:
         return bound.scope.functionContext
      bound = bound.parent
   return nil

func getLoopContext*(self: Bound): Bound =
   if self == nil: raise (ref Exception)(msg: "bound is nil!")
   var bound = self
   while bound != nil:
      if bound.scope != nil and bound.scope.loopContext != nil:
         return bound.scope.loopContext
      bound = bound.parent
   return nil

func tryDeclare*(self: Bound, identifier: Identifier): bool {.discardable.} =
   let bound = self.getScope()
   if identifier.name in bound.scope.identifiers:
      let existing = bound.scope.identifiers[identifier.name]
      if identifier.isComposedType(tfunc):
         if identifier.dtype != existing.dtype:
            self.binder.diagnostics.reportConflictingTypes(identifier.pos, identifier.name)
            self.binder.diagnostics.reportDefinitionHint(existing.pos, existing.name)
            return false
         elif identifier.dtype.composed.hasImplementation:
            if existing.dtype.composed.hasImplementation:
               self.binder.diagnostics.reportMultipleImplementations(identifier.pos,
                     identifier.name)
               self.binder.diagnostics.reportDefinitionHint(existing.pos, existing.name)
               return false
            else:
               bound.scope.identifiers[identifier.name] = identifier
               return true
         else:
            # Allow repeated pre-declaration
            return true
      else:
         self.binder.diagnostics.reportRedefinition(identifier.pos, identifier.name)
         self.binder.diagnostics.reportDefinitionHint(existing.pos, existing.name)
      return false
   bound.scope.identifiers[identifier.name] = identifier
   return true

func tryLookup*(self: Bound, name: string): Identifier =
   let bound = self.getScope()
   if name in bound.scope.identifiers: return bound.scope.identifiers[name]
   elif bound.parent != nil: return bound.parent.tryLookup(name)
   else: return nil

func toIdentifier(dtype: Dtype, name: string): Identifier =
   return Identifier(name: name, dtype: dtype)

func newTypeDtype(dtype: Dtype, name = ""): Dtype =
   return Dtype(base: tcomposed, composed: ComposedDtype(name: name, kind: ttype, dtype: dtype))


func addBaseDtypes*(self: Bound) =
   for dtypeBase in DtypeBase:
      if dtypeBase == tcomposed: continue
      let dtype = newTypeDtype(Dtype(base: dtypeBase), $dtypeBase)
      doAssert self.tryDeclare(toIdentifier(dtype, $dtypeBase))
   var ids: seq[string]
   for id, ident in self.scope.identifiers: ids.add($id)

func pos*(node: Bound): Position =
   if not node.node.isNil: return node.node.pos
   for key, value in fieldPairs(node[]):
      when key in ["kind", "scope", "parent", "binder"]: discard
      elif value is seq: return value[0].pos
      elif value is Token or value is Node or value is Bound:
         if not value.isNil: return value.pos
      elif value is Identifier:
         if not value.isNil: return value.pos
      else: discard
   raise (ref Exception)(msg: "No position for bound " & escape($node.kind))

func asCode*(bound: Bound): string =
   if bound.isNil: return ""
   case bound.kind
   of boundError:
      return "error"
   of boundRoot:
      return bound.main.asCode
   of boundLiteral:
      return bound.value.asCode
   of boundIdentifier:
      return bound.identifier.asCode
   of boundUnaryOperator:
      return bound.unaryOperator.asCode & " " & bound.unaryOperand.asCode
   of boundBinaryOperator:
      return bound.binaryLeft.asCode & bound.binaryOperator.asCode & bound.binaryRight.asCode
   of boundStruct:
      return bound.structToken.asCode & "{" & "}"
   of boundAssignment:
      return bound.lvalue.asCode & bound.assignmentToken.asCode & bound.rvalue.asCode
   of boundDefinition:
      result = $tokenDef & " " & bound.defIdentifier.asCode & ": " & bound.defDtype.asCode
      if not bound.defInitialization.isNil: result &= " = " & bound.defInitialization.asCode
   of boundFunctionCall:
      var args: seq[string]
      for arg in bound.callArguments: args.add(arg.asCode)
      return bound.callIdentifier.asCode & "(" & args.join(", ") & ")"
   of boundConditional:
      result = bound.conditionToken.asCode
      if not bound.condition.isNil: result &= bound.condition.asCode
      result &= ": " & bound.conditional.asCode
      if not bound.otherwise.isNil: result &= bound.otherwise.asCode
   of boundWhileLoop:
      return "while " & bound.whileCondition.asCode & ": " & bound.whileBody.asCode
   of boundReturn:
      return "return " & bound.returnExpr.asCode
   of boundBlock:
      var exprs: seq[string]
      for expression in bound.blockExpressions: exprs.add(expression.asCode)
      return "{" & exprs.join("\p") & "}"
   of boundLabel:
      return bound.label.asCode & ":"
   of boundGoto:
      return "goto " & bound.label.asCode
   of boundConditionalGoto:
      return "if " & bound.gotoCondition.asCode & " == " & bound.gotoIfTrue.asCode & ": goto " &
            bound.gotoLabel.asCode


func asTree*(bound: Bound): string =
   if bound.isNil: return ""
   let intro = $bound.kind & ": "
   var children: seq[string]
   for key, value in fieldPairs(bound[]):
      when key in ["kind", "scope", "parent", "binder"]: discard
      elif value is seq:
         children.add(key)
         for x in value:
            children.add(indent(x.asTree, 3))
      else:
         children.add(prettyPrint(key, value.asTree))
   return intro & "\p" & children.join("\p").indent(3)

func toDtype*(bound: Bound, dtypeToken: Token): Dtype =
   let id = bound.tryLookup(dtypeToken.text)
   if id == nil:
      bound.binder.diagnostics.reportUndefinedIdentifier(dtypeToken.pos, dtypeToken.text)
      return Dtype(base: terror)
   elif id.isComposedType(ttype):
      return id.dtype.composed.dtype
   else:
      bound.binder.diagnostics.reportWrongIdentifier(dtypeToken.pos, $id.dtype.base, $ttype)


func bindExpression*(bound: Bound, node: Node, requireValue = true): Bound
func bindBlockExpression(parent: Bound, node: Node): Bound
func bindDefinitionExpression(parent: Bound, node: Node): Bound

func bindErrorExpression(parent: Bound, node: Node): Bound =
   assert node.kind == errorExpression
   return Bound(kind: boundError, parent: parent, binder: parent.binder, node: node,
         errorToken: node.errorToken)

func bindLiteralExpression(parent: Bound, node: Node): Bound =
   assert node.kind == literalExpression
   result = Bound(kind: boundLiteral, parent: parent, binder: parent.binder, node: node)
   case node.literal.kind
   of tokenNumber:
      result.value = Value(pos: node.pos, base: tint, valInt: node.literal.valInt)
   of tokenTrue, tokenFalse:
      result.value = Value(pos: node.pos, base: tbool, valBool: node.literal.kind == tokenTrue)
   of tokenString:
      result.value = Value(pos: node.pos, base: tstr, valStr: node.literal.valStr)
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literal.kind))
   result.dtype = Dtype(base: result.value.base)
   result.valueNode = node

func bindIdentifierExpression(parent: Bound, node: Node): Bound =
   assert node.kind == identifierExpression
   assert node.identifier.kind == tokenIdentifier
   result = Bound(kind: boundIdentifier, parent: parent, binder: parent.binder, node: node)
   let name = node.identifier.text
   result.identifier = result.tryLookup(name)
   if result.identifier != nil:
      result.dtype = result.identifier.dtype
   else:
      result.binder.diagnostics.reportUndefinedIdentifier(node.identifier.pos, name)
      result.dtype = newDtype(terror)

func bindUnaryExpression(parent: Bound, node: Node): Bound =
   assert node.kind == unaryExpression
   result = Bound(kind: boundUnaryOperator, parent: parent, binder: parent.binder, node: node)
   result.unaryOperand = result.bindExpression(node.unaryOperand)
   let (operatorKind, resultDtype) = getUnaryOperator(result.binder.diagnostics,
         node.unaryOperator, result.unaryOperand.dtype)
   result.unaryOperator = operatorKind
   result.dtype = newDtype(resultDtype)

func bindBinaryExpression(parent: Bound, node: Node): Bound =
   assert node.kind == binaryExpression
   result = Bound(kind: boundBinaryOperator, parent: parent, binder: parent.binder, node: node)
   result.binaryLeft = result.bindExpression(node.left)
   result.binaryRight = result.bindExpression(node.right)
   let (operatorKind, resultDtype) = getBinaryOperator(result.binder.diagnostics,
         result.binaryLeft.dtype, node.binaryOperator, result.binaryRight.dtype)
   result.binaryOperator = operatorKind
   result.dtype = newDtype(resultDtype)

func bindAssignmentExpression(parent: Bound, node: Node): Bound =
   assert node.kind == assignmentExpression
   assert node.lvalue.kind == tokenIdentifier
   result = Bound(kind: boundAssignment, parent: parent, binder: parent.binder, node: node)
   result.lvalue = result.tryLookup(node.lvalue.text)
   result.assignmentToken = node.assignmentToken
   result.rvalue = result.bindExpression(node.rvalue)
   if result.lvalue == nil:
      result.binder.diagnostics.reportUndefinedIdentifier(node.lvalue.pos, node.lvalue.text)
   elif result.lvalue.dtype != result.rvalue.dtype:
      result.binder.diagnostics.reportCannotCast(node.assignmentToken.pos, $result.rvalue.dtype,
            $result.lvalue.dtype)
   result.dtype = result.rvalue.dtype

func bindParameter(bound: Bound, node: Node): Identifier =
   assert node.kind == parameterExpression
   result = Identifier()
   result.name = node.parameterName.text
   result.dtype = bound.toDtype(node.parameterDtype)
   result.pos = node.parameterName.pos
   if result.dtype.base == terror:
      bound.binder.diagnostics.reportUndefinedIdentifier(node.parameterDtype.pos,
            node.parameterDtype.text)

func bindStructExpression(parent: Bound, node: Node): Bound =
   assert node.kind == structExpression
   result = Bound(kind: boundStruct, parent: parent, binder: parent.binder, node: node)
   let composed = ComposedDtype(pos: Position(), kind: tstruct)
   result.scope = BoundScope()
   for member in node.structDefinitions:
      let boundMember = result.bindDefinitionExpression(member)
      composed.members.add(boundMember.defIdentifier)
   let dtype = Dtype(base: tcomposed, composed: composed)
   let dtypeType = newTypeDtype(dtype)
   result.dtype = dtypeType

func bindVariableDefinitonExpression(parent: Bound, node: Node): Bound =
   assert node.kind == definitionExpression
   assert node.defParameterOpen == nil
   result = Bound(kind: boundDefinition, parent: parent, binder: parent.binder, node: node)
   result.dtype = newDtype(tvoid)
   if node.defDtype != nil:
      result.defDtype = result.toDtype(node.defDtype)
   if node.defAssignExpression != nil:
      result.defInitialization = result.bindExpression(node.defAssignExpression)
      if result.defDtype.isNil:
         result.defDtype = result.defInitialization.dtype
      elif result.defDtype != result.defInitialization.dtype:
         result.binder.diagnostics.reportCannotCast(node.defAssignToken.pos,
               $result.defInitialization.dtype, $result.defDtype)

func bindFunctionDefinitionExpression(parent: Bound, node: Node): Bound =
   assert node.kind == definitionExpression
   assert node.defParameterOpen != nil
   result = Bound(kind: boundDefinition, parent: parent, binder: parent.binder, node: node)
   result.dtype = newDtype(tvoid)
   let composed = ComposedDtype(name: node.defIdentifier.text, pos: node.defIdentifier.pos, kind: tfunc)
   result.defDtype = Dtype(base: tcomposed, composed: composed)
   result.scope = BoundScope(functionContext: result)
   for parameter in node.defParameters:
      let p = result.bindParameter(parameter)
      composed.parameters.add(p)
      result.tryDeclare(p)
   if node.defDtype != nil:
      composed.retDtype = result.toDtype(node.defDtype)
   else: composed.retDtype = newDtype(tvoid)
   if node.defAssignExpression != nil:
      result.defInitialization = result.bindBlockExpression(node.defAssignExpression)
      composed.hasImplementation = true

func bindDefinitionExpression(parent: Bound, node: Node): Bound =
   assert node.kind == definitionExpression
   if node.defParameterOpen == nil:
      result = bindVariableDefinitonExpression(parent, node)
   else:
      result = bindFunctionDefinitionExpression(parent, node)
   result.defIdentifier = Identifier(name: node.defIdentifier.text, dtype: result.defDtype,
         pos: node.defIdentifier.pos)
   parent.tryDeclare(result.defIdentifier)

func bindCallExpression(parent: Bound, node: Node): Bound =
   assert node.kind == callExpression
   result = Bound(kind: boundFunctionCall, parent: parent, binder: parent.binder, node: node)
   result.callIdentifier = result.tryLookup(node.callIdentifier.text)
   if result.callIdentifier == nil:
      result.binder.diagnostics.reportUndefinedIdentifier(node.callIdentifier.pos,
            node.callIdentifier.text)
      result.dtype = newDtype(terror)
   elif not result.callIdentifier.isComposedType(tfunc):
      result.binder.diagnostics.reportCannotCast(result.callIdentifier.pos,
            $result.callIdentifier.dtype, "func")
   else:
      let dtype = result.callIdentifier.dtype.composed
      result.dtype = dtype.retDtype
      let parameters = dtype.parameters
      if node.callArguments.len != parameters.len:
         result.binder.diagnostics.reportWrongNumberOfArguments(result.callIdentifier.pos,
               node.callArguments.len, parameters.len)
      elif not dtype.hasImplementation:
         result.binder.diagnostics.reportMissingImplementation(node.callIdentifier.pos,
               node.callIdentifier.text)
         result.binder.diagnostics.reportDefinitionHint(result.callIdentifier.pos,
               node.callIdentifier.text)
      else:
         for idx, arg in node.callArguments:
            let argExpression = result.bindExpression(arg.callArgExpression)
            if argExpression.dtype != parameters[idx].dtype:
               result.binder.diagnostics.reportCannotCast(arg.callArgExpression.pos,
                     $argExpression.dtype, $parameters[idx].dtype)
               result.binder.diagnostics.reportDefinitionHint(parameters[idx].pos, parameters[idx].name)
            result.callArguments.add(argExpression)

func bindConditionalExpression(parent: Bound, node: Node, requireValue: bool): Bound =
   assert node.kind == conditionalExpression
   result = Bound(kind: boundConditional, parent: parent, binder: parent.binder, node: node)
   result.conditionToken = node.conditionToken
   if node.condition != nil:
      result.condition = result.bindExpression(node.condition)
      if result.condition.dtype.base != tbool:
         result.binder.diagnostics.reportCannotCast(node.condition.pos, $result.condition.dtype, "bool")
   result.scope = BoundScope() # Scope for conditional
   result.conditional = result.bindExpression(node.conditional, requireValue)
   if node.otherwise != nil:
      result.scope = BoundScope() # Scope for otherwise
      result.otherwise = result.bindConditionalExpression(node.otherwise, requireValue)

   if requireValue:
      result.dtype = result.conditional.dtype
      if node.conditionToken.kind == tokenElse:
         discard # dtype consistency check is only done in if, elif
      elif result.conditional.dtype.base != tvoid and result.otherwise == nil:
         result.binder.diagnostics.reportMissingElse(node.conditionToken.pos,
               $result.conditional.dtype)
      elif result.otherwise != nil and result.otherwise.dtype != result.conditional.dtype:
         result.binder.diagnostics.reportInconsistentConditionals(node.conditionToken.pos,
               $node.conditionToken.text, $result.conditional.dtype,
               $result.otherwise.conditionToken.text, $result.otherwise.dtype)
   else:
      result.dtype = newDtype(tvoid)

func bindWhileExpression(parent: Bound, node: Node): Bound =
   assert node.kind == whileExpression
   result = Bound(kind: boundWhileLoop, parent: parent, binder: parent.binder, node: node)
   result.whileCondition = result.bindExpression(node.whileCondition)
   result.scope = BoundScope(loopContext: result)
   result.whileBody = result.bindExpression(node.whileBody)
   result.dtype = newDtype(tvoid)

func bindReturnExpression(parent: Bound, node: Node): Bound =
   assert node.kind == returnExpression
   result = Bound(kind: boundReturn, parent: parent, binder: parent.binder, node: node)
   let functionContext = parent.getFunctionContext()
   if functionContext.isNil:
      parent.binder.diagnostics.reportReturnOutsideFunction(node.returnToken.pos)
      result.dtype = newDtype(terror)
      return
   if node.returnExpr != nil:
      result.returnExpr = result.bindExpression(node.returnExpr)
      result.dtype = result.returnExpr.dtype
   else:
      result.dtype = newDtype(tvoid)
   if result.dtype != functionContext.defDtype.composed.retDtype:
      result.binder.diagnostics.reportCannotCast(node.returnToken.pos, $result.dtype,
            $functionContext.defDtype.composed.retDtype)

func bindBlockExpression(parent: Bound, node: Node): Bound =
   assert node.kind == blockExpression
   result = Bound(kind: boundBlock, parent: parent, binder: parent.binder, node: node)
   result.scope = BoundScope()
   for expression in node.blockExpressions:
      result.blockExpressions.add(result.bindExpression(expression, requireValue = false))
   result.dtype = newDtype(tvoid)


func bindExpression*(bound: Bound, node: Node, requireValue = true): Bound =
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
      return bound.bindExpression(node.expression, requireValue)
   of assignmentExpression:
      return bound.bindAssignmentExpression(node)
   of parameterExpression:
      raise (ref Exception)(msg: "Unexpected parameter expression")
   of structExpression:
      return bound.bindStructExpression(node)
   of definitionExpression:
      if requireValue: bound.binder.diagnostics.reportRequireValue(node.pos)
      return bound.bindDefinitionExpression(node)
   of callArgumentExpression:
      raise (ref Exception)(msg: "Unexpected call argument expression")
   of callExpression:
      return bound.bindCallExpression(node)
   of conditionalExpression:
      return bound.bindConditionalExpression(node, requireValue)
   of whileExpression:
      return bound.bindWhileExpression(node)
   of returnExpression:
      return bound.bindReturnExpression(node)
   of blockExpression:
      return bound.bindBlockExpression(node)
   of compilationUnit:
      return

func bindExpression*(binder: Binder, node: Node): Bound =
   binder.root.main = binder.root.bindExpression(node, false)
   return binder.root.main

func newBinder*(parent: BoundScope = nil): Binder =
   result = Binder()
   result.root = Bound(kind: boundRoot, scope: BoundScope(), binder: result, node: nil)
   result.root.addBaseDtypes()
