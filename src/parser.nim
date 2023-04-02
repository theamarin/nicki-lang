import strutils
import lexer, syntaxfacts, diagnostics

{.experimental: "notnil".}

type
   NodeKind* = enum
      errorExpression = "error expression"
      literalExpression = "literal expression"
      identifierExpression = "identifier expression"
      unaryExpression = "unary expression"
      binaryExpression = "binary expression"
      paranthesisExpression = "paranthesis expression"
      parameterExpression = "parameter expression"
      definitionExpression = "definition expression"
      assignmentExpression = "assignment expression"
      callArgumentExpression = "call argument expression"
      callExpression = "call expression"
      conditionalExpression = "conditional expression"
      whileExpression = "while expression"
      returnExpression = "return expression"
      blockExpression = "block expression"
      compilationUnit = "compilation unit"

   Node* = ref object
      case kind*: NodeKind
      of errorExpression: errorToken*: Token
      of literalExpression: literal*: Token
      of identifierExpression: identifier*: Token
      of unaryExpression:
         unaryOperator*: Token
         unaryOperand*: Node
      of binaryExpression:
         left*: Node
         binaryOperator*: Token
         right*: Node
      of paranthesisExpression:
         open*: Token
         expression*: Node
         close*: Token
      of parameterExpression:
         parameterSeparator*: Token
         parameterName*: Token
         parameterColon*: Token
         parameterDtype*: Token
      of definitionExpression:
         defToken*: Token
         defIdentifier*: Token
         defParameterOpen*: Token
         defParameters*: seq[Node]
         defParameterClose*: Token
         defColon*: Token
         defDtype*: Token
         defAssignToken*: Token
         defAssignExpression*: Node
      of assignmentExpression:
         lvalue*: Token
         assignmentToken*: Token
         rvalue*: Node
      of callArgumentExpression:
         callArgSeparator*: Token
         callArgIdentifier*: Token
         callArgEquals*: Token
         callArgExpression*: Node
      of callExpression:
         callIdentifier*: Token
         callParanthesisOpen*: Token
         callArguments*: seq[Node]
         callParenthesisClose*: Token
      of conditionalExpression:
         conditionToken*: Token
         condition*: Node # nil for "else"
         colonToken*: Token
         conditional*: Node
         otherwise*: Node # if "elif" or "else" is present
      of whileExpression:
         whileToken*: Token
         whileCondition*: Node
         whileColon*: Token
         whileBody*: Node
      of returnExpression:
         returnToken*: Token
         returnExpr*: Node
      of blockExpression:
         blockStart*: Token
         blockExpressions*: seq[Node]
         blockEnd*: Token
      of compilationUnit:
         root*: Node
         eofToken*: Token


   Parser = ref object
      lexer: Lexer
      pos: int
      root*: Node
      diagnostics*: Diagnostics

func pos*(node: Node): Position =
   for key, value in fieldPairs(node[]):
      when key == "kind": discard
      elif value is seq: return value[0].pos
      else: return value.pos
   return Position()

func asTree*(token: Token): string = $token
func asTree*(val: ValueBase): string = $val

func asTree*(node: Node): string =
   if node.isNil: return ""
   let intro = $node.kind & ": "
   var children: seq[string]
   for key, value in fieldPairs(node[]):
      when key == "kind": discard
      elif value is seq:
         children.add(key)
         for x in value:
            children.add(indent(x.asTree, 3))
      else:
         children.add(prettyPrint(key, value.asTree))
   return intro & "\p" & children.join("\p").indent(3)

func asCode*(token: Token): string = $token
func asCode*(node: Node): string =
   if node.isNil: return ""
   var children: seq[string]
   for key, value in fieldPairs(node[]):
      when key == "kind": discard
      elif value is seq:
         for x in value:
            children.add(x.asCode)
      else:
         children.add(value.asCode)
   return children.join(" ")

func peek(parser: Parser, offset: int = 1): Token =
   return parser.lexer.get(parser.pos + offset)

func current(parser: Parser): Token = parser.peek(0)

func nextToken(parser: var Parser): Token =
   result = parser.current
   parser.pos.inc

func matchToken(parser: var Parser, kind: TokenKind,
      expression: NodeKind|string): Token {.discardable.} =
   if parser.current.kind == kind:
      return parser.nextToken
   else:
      parser.diagnostics.reportUnexpectedToken(parser.current.pos, $parser.current.kind, $kind, $expression)
      return Token()

func matchToken(parser: var Parser, kinds: set[TokenKind], expression: NodeKind): Token =
   if parser.current.kind in kinds:
      return parser.nextToken
   else:
      parser.diagnostics.reportUnexpectedToken(parser.current.pos, $parser.current.kind, $kinds, $expression)
      return Token()


func parseAssignmentExpression(parser: var Parser): Node
func parseOperatorExpression(parser: var Parser, parentPrecedence = 0): Node
func parsePrimaryExpression(parser: var Parser): Node

func parseExpression(parser: var Parser): Node = parser.parseAssignmentExpression

func parseConditionalExpression(parser: var Parser): Node =
   result = Node(kind: conditionalExpression)
   result.conditionToken = parser.matchToken({tokenIf, tokenElif, tokenElse}, result.kind)
   if result.conditionToken.kind in [tokenIf, tokenElif]:
      result.condition = parser.parseExpression()
   result.colonToken = parser.matchToken(tokenColon, result.kind)
   result.conditional = parser.parseExpression
   if parser.current.kind in [tokenElif, tokenElse]:
      result.otherwise = parser.parseConditionalExpression

func parseWhileExpression(parser: var Parser): Node =
   result = Node(kind: whileExpression)
   result.whileToken = parser.matchToken(tokenWhile, result.kind)
   result.whileCondition = parser.parseExpression()
   result.whileColon = parser.matchToken(tokenColon, result.kind)
   result.whileBody = parser.parseExpression()

func parseReturnExpression(parser: var Parser): Node =
   result = Node(kind: returnExpression)
   result.returnToken = parser.matchToken(tokenReturn, result.kind)
   let hasExpr = parser.current.kind != tokenEof and
      (parser.current.pos.line == result.returnToken.pos.line)
   if hasExpr: result.returnExpr = parser.parseExpression()

func parseBlockExpression(parser: var Parser): Node =
   result = Node(kind: blockExpression)
   result.blockStart = parser.matchToken(tokenBraceOpen, result.kind)
   while parser.current.kind notin {tokenBraceClose, tokenEof}:
      result.blockExpressions.add(parser.parseExpression())
   result.blockEnd = parser.matchToken(tokenBraceClose, result.kind)

func parseParameterExpression(parser: var Parser, isFirst: bool): Node =
   result = Node(kind: parameterExpression)
   if not isFirst:
      result.parameterSeparator = parser.matchToken(tokenComma, result.kind)
   result.parameterName = parser.matchToken(tokenIdentifier, result.kind)
   result.parameterColon = parser.matchToken(tokenColon, result.kind)
   result.parameterDtype = parser.matchToken(tokenIdentifier, result.kind)

func parseDefinitionExpression(parser: var Parser): Node =
   result = Node(kind: definitionExpression)
   result.defToken = parser.matchToken(tokenDef, result.kind)
   result.defIdentifier = parser.matchToken(tokenIdentifier, result.kind)
   if parser.current.kind == tokenParanthesisOpen:
      # Function definition
      result.defParameterOpen = parser.matchToken(tokenParanthesisOpen, result.kind)
      if parser.current.kind == tokenIdentifier:
         result.defParameters.add(parser.parseParameterExpression(isFirst = true))
         while parser.current.kind == tokenComma:
            result.defParameters.add(parser.parseParameterExpression(isFirst = false))
      result.defParameterClose = parser.matchToken(tokenParanthesisClose, result.kind)
   if parser.current.kind == tokenColon:
      # Optional return type specification
      result.defColon = parser.matchToken(tokenColon, result.kind)
      result.defDtype = parser.matchToken(tokenIdentifier, result.kind)
   if parser.current.kind == tokenEquals:
      # Initialization
      result.defAssignToken = parser.matchToken(tokenEquals, result.kind)
      if result.defParameterClose != nil:
         result.defAssignExpression = parser.parseBlockExpression()
      else:
         result.defAssignExpression = parser.parseExpression()
   if result.defColon == nil and result.defAssignToken == nil:
      parser.diagnostics.reportIncompleteDefinition(result.defToken.pos, result.defIdentifier.text)

func parseAssignmentExpression(parser: var Parser): Node =
   if parser.current.kind == tokenIdentifier and parser.peek.kind == tokenEquals:
      let lvalue = parser.nextToken
      let assignmentToken = parser.nextToken()
      let rvalue = parser.parseAssignmentExpression()
      return Node(kind: assignmentExpression, lvalue: lvalue,
            assignmentToken: assignmentToken, rvalue: rvalue)
   return parser.parseOperatorExpression()

func parseCallArgumentExpression(parser: var Parser, isFirst: bool): Node =
   result = Node(kind: callArgumentExpression)
   if not isFirst:
      result.callArgSeparator = parser.matchToken(tokenComma, result.kind)
   # result.callArgIdentifier = parser.matchToken(tokenIdentifier)
   # result.callArgEquals = parser.matchToken(tokenEquals)
   result.callArgExpression = parser.parseExpression()

func parseCallExpression(parser: var Parser): Node =
   result = Node(kind: callExpression)
   result.callIdentifier = parser.matchToken(tokenIdentifier, result.kind)
   result.callParanthesisOpen = parser.matchToken(tokenParanthesisOpen, result.kind)
   if parser.current.kind notin {tokenParanthesisClose, tokenEof}:
      result.callArguments.add(parser.parseCallArgumentExpression(isFirst = true))
      while parser.current.kind == tokenComma:
         result.callArguments.add(parser.parseCallArgumentExpression(isFirst = false))
   result.callParenthesisClose = parser.matchToken(tokenParanthesisClose, result.kind)

func parsePrimaryExpression(parser: var Parser): Node =
   case parser.current.kind
   of tokenParanthesisOpen:
      result = Node(kind: paranthesisExpression)
      result.open = parser.nextToken()
      result.expression = parser.parseOperatorExpression()
      result.close = parser.matchToken(tokenParanthesisClose, result.kind)
   of literalTokens:
      result = Node(kind: literalExpression)
      result.literal = parser.nextToken()
   of tokenIdentifier:
      if parser.peek.kind == tokenParanthesisOpen:
         return parser.parseCallExpression()
      else:
         let token = parser.nextToken()
         return Node(kind: identifierExpression, identifier: token)
   of tokenDef:
      return parser.parseDefinitionExpression()
   of tokenIf:
      return parser.parseConditionalExpression()
   of tokenWhile:
      return parser.parseWhileExpression()
   of tokenReturn:
      return parser.parseReturnExpression()
   of tokenBraceOpen:
      return parser.parseBlockExpression()
   else:
      let expectedKinds = {tokenParanthesisOpen, tokenTrue, tokenFalse, tokenNumber,
            tokenIdentifier, tokenIf, tokenWhile, tokenBraceOpen}
      parser.diagnostics.reportUnexpectedToken(parser.current.pos, $parser.current.kind,
            $expectedKinds, "primary expression")
      return Node()

func parseOperatorExpression(parser: var Parser, parentPrecedence = 0): Node =
   let unaryOperatorPrecedence = getUnaryOperatorPrecedence(parser.current.kind)
   if unaryOperatorPrecedence != 0 and unaryOperatorPrecedence >= parentPrecedence:
      result = Node(kind: unaryExpression)
      result.unaryOperator = parser.nextToken
      result.unaryOperand = parser.parseOperatorExpression(unaryOperatorPrecedence)
   else:
      result = parser.parsePrimaryExpression()

   while true:
      var precedence = getBinaryOperatorPrecedence(parser.current.kind)
      if precedence == 0 or precedence <= parentPrecedence:
         break
      let previous = result
      result = Node(kind: binaryExpression)
      result.left = previous
      result.binaryOperator = parser.nextToken
      result.right = parser.parseOperatorExpression(precedence)


func parse*(text: string): Parser =
   result = Parser(root: Node())
   result.lexer = text.lex
   for r in result.lexer.diagnostics:
      result.diagnostics.add(r)

   let left = result.parseExpression
   result.matchToken(tokenEof, "parser")

   result.root = left
