import strutils
import lexer

type
   NodeKind = enum
      node_number = "number"
      node_binary_expression = "binary expression"
   Node = ref object
      case kind: NodeKind
      of node_number: numberToken: Token
      of node_binary_expression:
         left, right: Node
         operatorToken: Token

   Parser = ref object
      lexer: Lexer
      position: int
      root*: Node
      diagnostics*: seq[string]


func `$`(node: Node): string =
   result = $node.kind & ": "
   case node.kind
   of node_number: result &= $node.numberToken
   of node_binary_expression:
      result &= "\p"
      result &= indent($node.left, 3) & "\p"
      result &= indent($node.operatorToken, 3) & "\p"
      result &= indent($node.right, 3)

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

func parseNumber(parser: var Parser): Node =
   let token = parser.match(token_number)
   return Node(kind: node_number, numberToken: token)

func parseBinaryExpression(parser: var Parser): Node =
   let token = parser.match(token_number)
   return Node(kind: node_number, numberToken: token)

func parseExpression(parser: var Parser): Node =
   var left = parser.parseNumber

   while parser.current.kind in [token_plus, token_minus]:
      let operatorToken = parser.next
      let right = parser.parseNumber
      left = Node(kind: node_binary_expression, left: left,
            operatorToken: operatorToken, right: right)
   return left


func parse*(text: string): Parser =
   var parser = Parser()
   parser.lexer = text.lex
   parser.diagnostics = parser.lexer.getDiagnostics

   let left = parser.parseExpression
   parser.match(token_eof)

   parser.root = left
   debugEcho $parser.root
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
   else: raise newException(ValueError, "Unexpected node " & escape($node.kind))
