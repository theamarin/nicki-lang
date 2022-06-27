import strutils
import lexer, syntaxfacts

type
   NodeKind* = enum
      node_literal = "literal"
      node_binary_expression = "binary expression"
      node_paranthesis_expression = "paranthesis expression"

   Node* = ref object
      case kind*: NodeKind
      of node_literal: literalToken*: Token
      of node_binary_expression:
         left*, right*: Node
         operatorToken*: Token
      of node_paranthesis_expression:
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
   of node_literal: result &= $node.literalToken
   of node_binary_expression:
      result &= "\p"
      result &= indent($node.left, 3) & "\p"
      result &= indent($node.operatorToken, 3) & "\p"
      result &= indent($node.right, 3)
   of node_paranthesis_expression:
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


func parseExpression(parser: var Parser): Node

func parsePrimaryExpression(parser: var Parser): Node =
   if parser.current.kind == token_paranthesis_open:
      let open = parser.nextToken
      let expression = parser.parseExpression
      let close = parser.matchToken(token_paranthesis_close)
      return Node(kind: node_paranthesis_expression, open: open, expression: expression, close: close)
   else:
      let token = parser.matchToken(token_number)
      return Node(kind: node_literal, literalToken: token)

func parseBinaryExpression(parser: var Parser, parentPrecedence = 0): Node =
   result = parser.parsePrimaryExpression
   while true:
      var precedence = getBinaryOperatorPrecedence(parser.current.kind)
      if precedence == 0 or precedence <= parentPrecedence:
         break
      let operatorToken = parser.nextToken
      let right = parser.parseBinaryExpression(precedence)
      result = Node(kind: node_binary_expression, left: result,
            operatorToken: operatorToken, right: right)

func parseExpression(parser: var Parser): Node = parser.parseBinaryExpression

func parse*(text: string): Parser =
   var parser = Parser()
   parser.lexer = text.lex
   parser.diagnostics = parser.lexer.getDiagnostics

   let left = parser.parseExpression
   parser.matchToken(token_eof)

   parser.root = left
   return parser
