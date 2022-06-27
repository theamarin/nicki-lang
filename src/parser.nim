import strutils
import lexer

type
   NodeKind = enum
      node_number = "number"
      node_binary_expression = "binary expression"
      node_paranthesis_expression = "paranthesis expression"
   Node* = ref object
      case kind: NodeKind
      of node_number: numberToken: Token
      of node_binary_expression:
         left, right: Node
         operatorToken: Token
      of node_paranthesis_expression:
         open, close: Token
         expression: Node

   Parser = ref object
      lexer: Lexer
      position: int
      root*: Node
      diagnostics*: seq[string]


func `$`*(node: Node): string =
   result = $node.kind & ": "
   case node.kind
   of node_number: result &= $node.numberToken
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

func next(parser: var Parser): Token =
   result = parser.current
   parser.position.inc

func match(parser: var Parser, kind: TokenKind): Token {.discardable.} =
   if parser.current.kind == kind:
      return parser.next
   parser.diagnostics.add("Error: Unexpected token " & escape($parser.current.kind) &
         ", expected " & escape($kind))
   return Token()

func parseExpression(parser: var Parser): Node


func parsePrimaryExpression(parser: var Parser): Node =
   if parser.current.kind == token_paranthesis_open:
      let open = parser.next
      let expression = parser.parseExpression
      let close = parser.match(token_paranthesis_close)
      return Node(kind: node_paranthesis_expression, open: open, expression: expression, close: close)
   else:
      let token = parser.match(token_number)
      return Node(kind: node_number, numberToken: token)

func parseFactor(parser: var Parser): Node =
   var left = parser.parsePrimaryExpression

   while parser.current.kind in [token_star, token_slash]:
      let operatorToken = parser.next
      let right = parser.parsePrimaryExpression
      left = Node(kind: node_binary_expression, left: left,
            operatorToken: operatorToken, right: right)
   return left

func parseTerm(parser: var Parser): Node =
   var left = parser.parseFactor

   while parser.current.kind in [token_plus, token_minus]:
      let operatorToken = parser.next
      let right = parser.parseFactor
      left = Node(kind: node_binary_expression, left: left,
            operatorToken: operatorToken, right: right)
   return left

func parseExpression(parser: var Parser): Node = parser.parseTerm

func parse*(text: string): Parser =
   var parser = Parser()
   parser.lexer = text.lex
   parser.diagnostics = parser.lexer.getDiagnostics

   let left = parser.parseExpression
   parser.match(token_eof)

   parser.root = left
   return parser


func evaluate*(node: Node): int =
   case node.kind
   of node_number: return node.numberToken.value
   of node_binary_expression:
      let left = node.left.evaluate
      let right = node.right.evaluate
      case node.operatorToken.kind
      of token_plus: return left + right
      of token_minus: return left - right
      of token_star: return left * right
      of token_slash: return left div right
      else: raise newException(ValueError, "Unexpected binary operator " & escape(
            $node.operatorToken.kind))
   of node_paranthesis_expression:
      return node.expression.evaluate
   else: raise newException(ValueError, "Unexpected node " & escape($node.kind))
