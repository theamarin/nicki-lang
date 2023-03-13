import strutils, tables
import parser, lexer, binder_ops, identifiers, diagnostics

type
   BoundScope* = ref object
      identifiers*: Table[string, Identifier]
      parent*: BoundScope

func tryDeclare*(self: BoundScope, identifier: Identifier): bool =
   if identifier.name in self.identifiers: return false
   self.identifiers[identifier.name] = identifier
   return true

func tryLookup*(self: BoundScope, name: string): Identifier =
   if name in self.identifiers: return self.identifiers[name]
   elif self.parent != nil: return self.parent.tryLookup(name)
   else: return nil

type
   BoundKind* = enum
      boundError = "error expression",
      boundLiteralExpression = "literal expression"
      boundIdentifierExpression = "identifier expression"
      boundUnaryExpression = "unary expression"
      boundBinaryExpression = "binary expression"
      boundAssignmentExpression = "assignment expression"
      boundDefinitionExpression = "definition expression"
      boundConditionalExpression = "conditional expression"
      boundWhileExpression = "while expression"
      boundBlockExpression = "block expression"

   Bound* = ref object
      dtype*: Dtype
      case kind*: BoundKind
      of boundError:
         errorToken*: Token
      of boundLiteralExpression:
         value*: Value
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
         defParameters*: seq[Parameter]
         defDtype*: Dtype
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


func `$`*(bound: Bound): string =
   if bound.isNil: return ""
   let intro = $bound.kind & ": "
   var children: seq[string]
   for key, value in fieldPairs(bound[]):
      when key == "kind": discard
      elif value is seq:
         children.add(key)
         for x in value:
            children.add(indent($x, 3))
      else:
         children.add(prettyPrint(key, $value))
   return intro & "\p" & children.join("\p").indent(3)


func bindExpression*(binder: Binder, node: Node): Bound

func bindErrorExpression(binder: Binder, node: Node): Bound =
   assert node.kind == errorExpression
   return Bound(kind: boundError, errorToken: node.errorToken)

func bindLiteralExpression(binder: Binder, node: Node): Bound =
   assert node.kind == literalExpression
   case node.literal.kind
   of tokenNumber:
      let value = node.literal.value
      return Bound(kind: boundLiteralExpression, value: value, dtype: value.dtype)
   of tokenTrue, tokenFalse:
      let value = Value(dtype: tbool, valBool: node.literal.kind == tokenTrue)
      return Bound(kind: boundLiteralExpression, value: value,
            dtype: value.dtype)
   of tokenString:
      let value = Value(dtype: tstring, valString: node.literal.text)
      return Bound(kind: boundLiteralExpression, value: value, dtype: value.dtype)
   else: raise (ref Exception)(msg: "Unexpected literal " & escape(
         $node.literal.kind))

func bindIdentifierExpression(binder: Binder, node: Node): Bound =
   assert node.kind == identifierExpression
   assert node.identifier.kind == tokenIdentifier
   result = Bound(kind: boundIdentifierExpression)
   let name = node.identifier.text
   result.identifier = binder.scope.tryLookup(name)
   if result.identifier != nil:
      result.dtype = result.identifier.dtype
   else:
      binder.diagnostics.reportUndefinedIdentifier(node.identifier.pos, name)
      result.dtype = terror

func bindUnaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == unaryExpression
   let operand = binder.bindExpression(node.unaryOperand)
   let (operatorKind, resultDtype) = getUnaryOperator(binder.diagnostics,
         node.unaryOperator, operand.dtype)
   return Bound(kind: boundUnaryExpression, unaryOperator: operatorKind,
         unaryOperand: operand, dtype: resultDtype)

func bindBinaryExpression(binder: Binder, node: Node): Bound =
   assert node.kind == binaryExpression
   let boundLeft = binder.bindExpression(node.left)
   let boundRight = binder.bindExpression(node.right)
   let (operatorKind, resultDtype) = getBinaryOperator(binder.diagnostics,
         boundLeft.dtype, node.binaryOperator, boundRight.dtype)
   return Bound(kind: boundBinaryExpression, binaryLeft: boundLeft,
         binaryRight: boundRight, binaryOperator: operatorKind,
         dtype: resultDtype)

func bindAssignmentExpression(binder: Binder, node: Node): Bound =
   assert node.kind == assignmentExpression
   assert node.lvalue.kind == tokenIdentifier
   let rvalue = binder.bindExpression(node.rvalue)
   let identifier = binder.scope.tryLookup(node.lvalue.text)
   if identifier == nil:
      binder.diagnostics.reportUndefinedIdentifier(node.lvalue.pos, node.lvalue.text)
   elif identifier.dtype != rvalue.dtype:
      binder.diagnostics.reportCannotCast(node.assignment.pos, $rvalue.dtype,
            $identifier.dtype)
   return Bound(kind: boundAssignmentExpression, dtype: rvalue.dtype,
         lvalue: node.lvalue, rvalue: rvalue)

func bindParameter(binder: Binder, node: Node): Parameter =
   assert node.kind == parameterExpression
   result.name = node.parameterName.text
   result.dtype = node.parameterDtype.text.toDtype
   if result.dtype == terror:
      binder.diagnostics.reportUndefinedIdentifier(node.parameterDtype.pos,
            node.parameterDtype.text)

func bindDefinitionExpression(binder: Binder, node: Node): Bound =
   assert node.kind == definitionExpression
   result = Bound(kind: boundDefinitionExpression)
   result.dtype = tvoid
   result.defIdentifier = node.defIdentifier
   for parameter in node.defParameters:
      result.defParameters.add(binder.bindParameter(parameter))
   result.defDtype = node.defDtype.text.toDtype
   if result.defDtype == terror:
      binder.diagnostics.reportUndefinedIdentifier(node.defDtype.pos, node.defDtype.text)
   let identifier =
      if node.defParameterOpen != nil:
         newFunctionIdentifier(node.defIdentifier.text, result.defDtype, result.defParameters,
            node.defToken.pos)
      else:
         newVariableIdentifier(node.defIdentifier.text, result.defDtype, node.defToken.pos)
   if not binder.scope.tryDeclare(identifier):
      binder.diagnostics.reportAlreadyDefinedIdentifier(node.defToken.pos, node.defToken.text)

func bindConditionalExpression(binder: Binder, node: Node): Bound =
   assert node.kind == conditionalExpression
   let condition =
      if node.condition != nil:
         binder.bindExpression(node.condition)
      else: nil
   if condition != nil and condition.dtype != tbool:
      binder.diagnostics.reportConditionNotBoolean(node.conditionToken.pos)
   let conditional = binder.bindExpression(node.conditional)
   let otherwise =
      if node.otherwise != nil: binder.bindConditionalExpression(node.otherwise)
      else: nil
   if conditional.dtype != tvoid and node.conditionToken.kind != tokenElse:
      if otherwise == nil:
         binder.diagnostics.reportMissingElse(node.conditionToken.pos,
               $conditional.dtype)
      elif otherwise.dtype != conditional.dtype:
         binder.diagnostics.reportInconsistentConditionals(node.conditionToken.pos,
               $node.conditionToken.text, $conditional.dtype,
               $otherwise.conditionToken.text, $otherwise.dtype)
   return Bound(kind: boundConditionalExpression, dtype: conditional.dtype,
         conditionToken: node.conditionToken, condition: condition,
         conditional: conditional, otherwise: otherwise)

func bindWhileExpression(binder: Binder, node: Node): Bound =
   assert node.kind == whileExpression
   let whileCondition = binder.bindExpression(node.whileCondition)
   let whileBody = binder.bindExpression(node.whileBody)
   return Bound(kind: boundWhileExpression, dtype: tvoid, whileCondition: whileCondition,
         whileBody: whileBody)

func bindBlockExpression(binder: Binder, node: Node): Bound =
   assert node.kind == blockExpression
   var blockExpressions: seq[Bound]
   for expression in node.blockExpressions:
      blockExpressions.add(binder.bindExpression(expression))
   return Bound(kind: boundBlockExpression, dtype: tvoid, blockExpressions: blockExpressions)


func bindExpression*(binder: Binder, node: Node): Bound =
   case node.kind
   of errorExpression:
      return binder.bindErrorExpression(node)
   of literalExpression:
      return binder.bindLiteralExpression(node)
   of identifierExpression:
      return binder.bindIdentifierExpression(node)
   of unaryExpression:
      return binder.bindUnaryExpression(node)
   of binaryExpression:
      return binder.bindBinaryExpression(node)
   of paranthesisExpression:
      return binder.bindExpression(node.expression)
   of assignmentExpression:
      return binder.bindAssignmentExpression(node)
   of parameterExpression:
      raise (ref Exception)(msg: "Unexpected parameter expression")
   of definitionExpression:
      return binder.bindDefinitionExpression(node)
   of conditionalExpression:
      return binder.bindConditionalExpression(node)
   of whileExpression:
      return binder.bindWhileExpression(node)
   of blockExpression:
      return binder.bindBlockExpression(node)
   of compilationUnit:
      return

func newBinder*(parent: BoundScope = nil): Binder =
   result = Binder()
   result.scope = BoundScope(parent: parent)
