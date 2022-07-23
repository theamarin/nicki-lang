import strutils
import lexer, syntaxfacts

type
   NodeKind* = enum
      nodeLiteral = "literal"
      nodeUnaryExpression = "unary expression"
      nodeBinaryExpression = "binary expression"
      nodeParanthesisExpression = "paranthesis expression"

   Node* = ref object
      case kind*: NodeKind
      of nodeLiteral: literalToken*: Token
      of nodeUnaryExpression:
         unaryOperator*: Token
         unaryOperand*: Node
      of nodeBinaryExpression:
         left*, right*: Node
         binaryOperator*: Token
      of nodeParanthesisExpression:
         open*, close*: Token
         expression*: Node

   Parser = ref object
      lexer: Lexer
      position: int
      root*: Node
      diagnostics*: seq[string]


func `$`*(node: Node): string =
   result = $node.kind & ": "
   case node.kind
   of nodeLiteral: result &= $node.literalToken
   of nodeUnaryExpression:
      result &= "\p"
      result &= indent($node.unaryOperator, 3) & "\p"
      result &= indent($node.unaryOperand, 3)
   of nodeBinaryExpression:
      result &= "\p"
      result &= indent($node.left, 3) & "\p"
      result &= indent($node.binaryOperator, 3) & "\p"
      result &= indent($node.right, 3)
   of nodeParanthesisExpression:
      result &= "\p"
      result &= indent($node.open, 3) & "\p"
      result &= indent($node.expression, 3) & "\p"
      result &= indent($node.close, 3)

func peek(parser: Parser, offset: int = 0): Token =
   return parser.lexer.get(parser.position + offset)

func current(parser: Parser): Token = parser.peek()

func nextToken(parser: var Parser): Token =
   result = parser.current
   parser.position.inc

func matchToken(parser: var Parser, kind: TokenKind): Token {.discardable.} =
   if parser.current.kind == kind:
      return parser.nextToken
   parser.diagnostics.add("Error: Unexpected token " & escape($parser.current.kind) &
         ", expected " & escape($kind))
   return Token()

func parseExpression(parser: var Parser, parentPrecedence = 0): Node

func parsePrimaryExpression(parser: var Parser): Node =
   if parser.current.kind == tokenParanthesisOpen:
      let open = parser.nextToken
      let expression = parser.parseExpression
      let close = parser.matchToken(tokenParanthesisClose)
      return Node(kind: nodeParanthesisExpression, open: open, expression: expression, close: close)
   else:
      let token = parser.matchToken(tokenNumber)
      return Node(kind: nodeLiteral, literalToken: token)

func parseExpression(parser: var Parser, parentPrecedence = 0): Node =
   let unaryOperatorPrecedence = getUnaryOperatorPrecedence(parser.current.kind)
   if unaryOperatorPrecedence != 0 and unaryOperatorPrecedence >= parentPrecedence:
      let operatorToken = parser.nextToken
      let operand = parser.parseExpression(unaryOperatorPrecedence)
      result = Node(kind: nodeUnaryExpression, unaryOperator: operatorToken,
            unaryOperand: operand)
   else:
      result = parser.parsePrimaryExpression

   while true:
      var precedence = getBinaryOperatorPrecedence(parser.current.kind)
      if precedence == 0 or precedence <= parentPrecedence:
         break
      let binaryOperator = parser.nextToken
      let right = parser.parseExpression(precedence)
      result = Node(kind: nodeBinaryExpression, left: result,
            binaryOperator: binaryOperator, right: right)


func parse*(text: string): Parser =
   var parser = Parser()
   parser.lexer = text.lex
   parser.diagnostics = parser.lexer.getDiagnostics

   let left = parser.parseExpression
   parser.matchToken(tokenEof)

   parser.root = left
   return parser
