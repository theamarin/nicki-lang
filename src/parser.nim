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
      assignmentExpression = "assignment expression"
      conditionalExpression = "conditional expression"
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
      of assignmentExpression:
         lvalue*, assignment*: Token
         rvalue*: Node
      of conditionalExpression:
         conditionToken*: Token
         condition*: Node # nil for "else"
         colonToken*: Token
         conditional*: Node
         otherwise*: Node # if "elif" or "else" is present
      of compilationUnit:
         root*: Node
         eofToken*: Token


   Parser = ref object
      lexer: Lexer
      pos: int
      root*: Node
      diagnostics*: Diagnostics


func `$`*(node: Node): string =
   result = $node.kind & ": "
   case node.kind
   of errorExpression: result &= $node.errorToken
   of literalExpression: result &= $node.literal
   of identifierExpression: result &= $node.identifier
   of unaryExpression:
      result &= "\p"
      result &= indent($node.unaryOperator, 3) & "\p"
      result &= indent($node.unaryOperand, 3)
   of binaryExpression:
      result &= "\p"
      result &= indent($node.left, 3) & "\p"
      result &= indent($node.binaryOperator, 3) & "\p"
      result &= indent($node.right, 3)
   of paranthesisExpression:
      result &= "\p"
      result &= indent($node.open, 3) & "\p"
      result &= indent($node.expression, 3) & "\p"
      result &= indent($node.close, 3)
   of assignmentExpression:
      result &= "\p"
      result &= indent($node.lvalue, 3) & "\p"
      result &= indent($node.assignment, 3) & "\p"
      result &= indent($node.rvalue, 3)
   of conditionalExpression:
      result &= "\p"
      result &= indent($node.conditionToken, 3) & "\p"
      if node.condition != nil:
         result &= indent($node.condition, 3) & "\p"
      result &= indent($node.conditional, 3) & "\p"
      if node.otherwise != nil:
         result &= indent($node.otherwise, 3) & "\p"
   of compilationUnit:
      result &= "\p"
      result &= indent($node.root, 3) & "\p"
      result &= indent($node.eofToken, 3)

func peek(parser: Parser, offset: int = 0): Token =
   return parser.lexer.get(parser.pos + offset)

func current(parser: Parser): Token = parser.peek()

func nextToken(parser: var Parser): Token =
   result = parser.current
   parser.pos.inc

func matchToken(parser: var Parser, kind: TokenKind): Token {.discardable.} =
   if parser.current.kind == kind:
      return parser.nextToken
   parser.diagnostics.report("Parser: Unexpected token " & escape(
         $parser.current.kind) & ", expected " & escape($kind),
               parser.current.pos)
   return Token()

func matchToken(parser: var Parser, kinds: set[TokenKind]): Token =
   if parser.current.kind in kinds:
      return parser.nextToken
   parser.diagnostics.report("Parser: Unexpected token " & escape(
         $parser.current.kind) & ", expected " & escape($kinds),
               parser.current.pos)
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


func parseAssignmentExpression(parser: var Parser): Node =
   if parser.current.kind == tokenIdentifier and parser.peek(1).kind == tokenEquals:
      let lvalue = parser.nextToken
      let assignment = parser.nextToken
      let rvalue = parser.parseAssignmentExpression
      return Node(kind: assignmentExpression, lvalue: lvalue,
            assignment: assignment, rvalue: rvalue)
   return parser.parseOperatorExpression

func parsePrimaryExpression(parser: var Parser): Node =
   if parser.current.kind == tokenParanthesisOpen:
      let open = parser.nextToken
      let expression = parser.parseOperatorExpression
      let close = parser.matchToken(tokenParanthesisClose)
      return Node(kind: paranthesisExpression, open: open,
            expression: expression, close: close)
   elif parser.current.kind in [tokenTrue, tokenFalse, tokenNumber]:
      let token = parser.nextToken
      return Node(kind: literalExpression, literal: token)
   elif parser.current.kind == tokenIdentifier:
      let token = parser.nextToken
      return Node(kind: identifierExpression, identifier: token)
   elif parser.current.kind == tokenIf:
      return parser.parseConditionalExpression
   else:
      parser.diagnostics.report("Parser: Unexpected token " & escape(
            $parser.current.kind) & " for primary expression",
            parser.current.pos)
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
