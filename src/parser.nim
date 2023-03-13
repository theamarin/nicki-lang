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
      conditionalExpression = "conditional expression"
      whileExpression = "while expression"
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
         left*, right*: Node
         binaryOperator*: Token
      of paranthesisExpression:
         open*, close*: Token
         expression*: Node
      of parameterExpression:
         parameterOpenParan*: Token
         parameterName*: Node
         parameterColon*: Token
         parameterDtype*: Node
         parameterCommaOrClose*: Token
      of definitionExpression:
         defToken*: Token
         defIdentifier*: Token
         defParameters*: seq[Node] # parameterExpressions
         defColon*: Token
         defDtype*: Token
      of assignmentExpression:
         lvalue*, assignment*: Token
         rvalue*: Node
      of conditionalExpression:
         conditionToken*: Token
         condition*: Node          # nil for "else"
         colonToken*: Token
         conditional*: Node
         otherwise*: Node          # if "elif" or "else" is present
      of whileExpression:
         whileToken*: Token
         whileCondition*: Node
         whileColon*: Token
         whileBody*: Node
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


func `$`*(node: Node): string =
   let intro = $node.kind & ": "
   var children: seq[string]
   case node.kind
   of errorExpression: return intro & $node.errorToken
   of literalExpression: return intro & $node.literal
   of identifierExpression: return intro & $node.identifier
   of unaryExpression:
      children.add($node.unaryOperator)
      children.add($node.unaryOperand)
   of binaryExpression:
      children.add($node.left)
      children.add($node.binaryOperator)
      children.add($node.right)
   of paranthesisExpression:
      children.add($node.open)
      children.add($node.expression)
      children.add($node.close)
   of parameterExpression:
      children.add($node.parameterOpenParan)
      children.add($node.parameterName)
      children.add($node.parameterColon)
      children.add($node.parameterDtype)
      children.add($node.parameterCommaOrClose)
   of definitionExpression:
      children.add($node.defToken)
      children.add($node.defIdentifier)
      for parameter in node.defParameters:
         children.add($parameter)
      children.add($node.defColon)
      children.add($node.defDtype)
   of assignmentExpression:
      children.add($node.lvalue)
      children.add($node.assignment)
      children.add($node.rvalue)
   of conditionalExpression:
      children.add($node.conditionToken)
      if node.condition != nil:
         children.add($node.condition)
      children.add($node.conditional)
      if node.otherwise != nil:
         children.add($node.otherwise)
   of whileExpression:
      children.add($node.whileToken)
      children.add($node.whileCondition)
      children.add($node.whileColon)
      children.add($node.whileBody)
   of blockExpression:
      for expression in node.blockExpressions:
         children.add($expression)
   of compilationUnit:
      children.add($node.root)
      children.add($node.eofToken)
   return intro & "\p" & children.join("\p").indent(3)

func peek(parser: Parser, offset: int = 0): Token =
   return parser.lexer.get(parser.pos + offset)

func current(parser: Parser): Token = parser.peek()

func nextToken(parser: var Parser): Token =
   result = parser.current
   parser.pos.inc

func matchToken(parser: var Parser, kind: TokenKind): Token {.discardable.} =
   if parser.current.kind == kind:
      return parser.nextToken
   else:
      parser.diagnostics.reportUnexpectedToken(parser.current.pos, $parser.current.kind, $kind)
      return Token()

func matchToken(parser: var Parser, kinds: set[TokenKind]): Token =
   if parser.current.kind in kinds:
      return parser.nextToken
   else:
      parser.diagnostics.reportUnexpectedToken(parser.current.pos, $parser.current.kind, $kinds)
      return Token()


func parseAssignmentExpression(parser: var Parser): Node
func parseOperatorExpression(parser: var Parser, parentPrecedence = 0): Node
func parsePrimaryExpression(parser: var Parser): Node

func parseExpression(parser: var Parser): Node = parser.parseAssignmentExpression

func parseConditionalExpression(parser: var Parser): Node =
   let conditionToken = parser.matchToken({tokenIf, tokenElif, tokenElse})
   let condition =
      if conditionToken.kind in [tokenIf, tokenElif]:
         parser.parseExpression
      else: nil
   let colonToken = parser.matchToken(tokenColon)
   let conditional = parser.parseExpression
   let otherwise: Node =
      if parser.peek.kind in [tokenElif, tokenElse]:
         parser.parseConditionalExpression
      else: nil

   return Node(kind: conditionalExpression, conditionToken: conditionToken,
         condition: condition, colonToken: colonToken, conditional: conditional,
         otherwise: otherwise)

func parseWhileExpression(parser: var Parser): Node =
   let whileToken = parser.matchToken(tokenWhile)
   let condition = parser.parseExpression()
   let colonToken = parser.matchToken(tokenColon)
   let body = parser.parseExpression()
   return Node(kind: whileExpression, whileToken: whileToken, whileCondition: condition,
         whileColon: colonToken, whileBody: body)

func parseBlockExpression(parser: var Parser): Node =
   let blockStart = parser.matchToken(tokenBraceOpen)
   var blockExpressions: seq[Node]
   while parser.current.kind notin {tokenBraceClose, tokenEof}:
      blockExpressions.add(parser.parseExpression())
   let blockEnd = parser.matchToken(tokenBraceClose)

   return Node(kind: blockExpression, blockStart: blockStart,
         blockExpressions: blockExpressions, blockEnd: blockEnd)

func parseDefinitionExpression(parser: var Parser): Node =
   let defToken = parser.matchToken(tokenDef)
   let defIdentifier = parser.matchToken(tokenIdentifier)
   let defColon = parser.matchToken(tokenColon)
   let defDtype = parser.matchToken(tokenIdentifier)
   return Node(kind: definitionExpression, defToken: defToken, defIdentifier: defIdentifier,
         defColon: defColon, defDtype: defDtype)

func parseAssignmentExpression(parser: var Parser): Node =
   if parser.current.kind == tokenIdentifier and parser.peek(1).kind == tokenEquals:
      let lvalue = parser.nextToken
      let assignment = parser.nextToken()
      let rvalue = parser.parseAssignmentExpression()
      return Node(kind: assignmentExpression, lvalue: lvalue,
            assignment: assignment, rvalue: rvalue)
   return parser.parseOperatorExpression()

func parsePrimaryExpression(parser: var Parser): Node =
   if parser.current.kind == tokenParanthesisOpen:
      let open = parser.nextToken()
      let expression = parser.parseOperatorExpression
      let close = parser.matchToken(tokenParanthesisClose)
      return Node(kind: paranthesisExpression, open: open,
            expression: expression, close: close)
   elif parser.current.kind in literalTokens:
      let token = parser.nextToken()
      return Node(kind: literalExpression, literal: token)
   elif parser.current.kind == tokenIdentifier:
      let token = parser.nextToken()
      return Node(kind: identifierExpression, identifier: token)
   elif parser.current.kind == tokenDef:
      return parser.parseDefinitionExpression()
   elif parser.current.kind == tokenIf:
      return parser.parseConditionalExpression()
   elif parser.current.kind == tokenWhile:
      return parser.parseWhileExpression()
   elif parser.current.kind == tokenBraceOpen:
      return parser.parseBlockExpression()
   else:
      let expectedKinds = {tokenParanthesisOpen, tokenTrue, tokenFalse, tokenNumber,
            tokenIdentifier, tokenIf, tokenWhile, tokenBraceOpen}
      parser.diagnostics.reportUnexpectedToken(parser.current.pos, $parser.current.kind,
            $expectedKinds)
      return Node()

func parseOperatorExpression(parser: var Parser, parentPrecedence = 0): Node =
   let unaryOperatorPrecedence = getUnaryOperatorPrecedence(parser.current.kind)
   if unaryOperatorPrecedence != 0 and unaryOperatorPrecedence >= parentPrecedence:
      let operatorToken = parser.nextToken
      let operand = parser.parseOperatorExpression(unaryOperatorPrecedence)
      result = Node(kind: unaryExpression, unaryOperator: operatorToken,
            unaryOperand: operand)
   else:
      result = parser.parsePrimaryExpression

   while true:
      var precedence = getBinaryOperatorPrecedence(parser.current.kind)
      if precedence == 0 or precedence <= parentPrecedence:
         break
      let binaryOperator = parser.nextToken
      let right = parser.parseOperatorExpression(precedence)
      result = Node(kind: binaryExpression, left: result,
            binaryOperator: binaryOperator, right: right)


func parse*(text: string): Parser =
   result = Parser(root: Node())
   result.lexer = text.lex
   for r in result.lexer.diagnostics:
      result.diagnostics.add(r)

   let left = result.parseExpression
   result.matchToken(tokenEof)

   result.root = left
