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
         op: Token

   Parser = ref object
      lexer: Lexer
      position: int
      root: Node

func `$`(node: Node): string =
   result = $node.kind & ": "
   case node.kind
   of node_number: result &= $node.numberToken
   of node_binary_expression:
      result &= "\p"
      result &= indent($node.left, 3) & "\p"
      result &= indent($node.op, 3) & "\p"
      result &= indent($node.right, 3)

func peek(parser: Parser, offset: int = 0): Token =
   return parser.lexer.get(parser.position + offset)

func current(parser: Parser): Token = parser.peek()

func next(parser: var Parser): Token =
   result = parser.current
   parser.position.inc

func match(parser: var Parser, kind: TokenKind): Token =
   if parser.current.kind == kind:
      return parser.next
   return Token()

func parseNumber(parser: var Parser): Node =
   let token = parser.match(token_number)
   return Node(kind: node_number, numberToken: token)

func parseBinaryExpression(parser: var Parser): Node =
   let token = parser.match(token_number)
   return Node(kind: node_number, numberToken: token)


func parse*(text: string): Parser =
   var parser = Parser()
   parser.lexer = text.lex

   var left = parser.parseNumber

   while parser.current.kind in [token_plus, token_minus]:
      let operatorToken = parser.next
      let right = parser.parseNumber
      left = Node(kind: node_binary_expression, left: left, op: operatorToken, right: right)

   parser.root = left
   debugEcho $parser.root
   return parser
