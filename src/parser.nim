import strutils
import lexer, syntaxfacts, diagnostics

{.experimental: "notnil".}

type
   NodeKind* = enum
      nodeLiteral = "literal"
      nodeIdentifier = "identifier"
      nodeUnaryExpression = "unary expression"
      nodeBinaryExpression = "binary expression"
      nodeParanthesisExpression = "paranthesis expression"
      nodeAssignmentExpression = "assignment expression"
      nodeCompilationUnit = "compilation unit"

   Node* = ref NodeObj not nil
   NodeObj = object
      case kind*: NodeKind
      of nodeLiteral: literal*: Token
      of nodeIdentifier: identifier*: Token
      of nodeUnaryExpression:
         unaryOperator*: Token
         unaryOperand*: Node
      of nodeBinaryExpression:
         left*, right*: Node
         binaryOperator*: Token
      of nodeParanthesisExpression:
         open*, close*: Token
         expression*: Node
      of nodeAssignmentExpression:
         lvalue*, assignment*: Token
         rvalue*: Node
      of nodeCompilationUnit:
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
   of nodeLiteral: result &= $node.literal
   of nodeIdentifier: result &= $node.identifier
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
   of nodeAssignmentExpression:
      result &= "\p"
      result &= indent($node.lvalue, 3) & "\p"
      result &= indent($node.assignment, 3) & "\p"
      result &= indent($node.rvalue, 3)
   of nodeCompilationUnit:
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
         $parser.current.kind) & ", expected " & escape($kind), parser.current.pos)
   return Token()

func parseAssignmentExpression(parser: var Parser): Node
func parseOperatorExpression(parser: var Parser, parentPrecedence = 0): Node
func parsePrimaryExpression(parser: var Parser): Node

func parseExpression(parser: var Parser): Node = parser.parseAssignmentExpression

func parseAssignmentExpression(parser: var Parser): Node =
   if parser.current.kind == tokenIdentifier and parser.peek(1).kind == tokenEquals:
      let lvalue = parser.nextToken
      let assignment = parser.nextToken
      let rvalue = parser.parseAssignmentExpression
      return Node(kind: nodeAssignmentExpression, lvalue: lvalue,
            assignment: assignment, rvalue: rvalue)
   return parser.parseOperatorExpression

func parsePrimaryExpression(parser: var Parser): Node =
   if parser.current.kind == tokenParanthesisOpen:
      let open = parser.nextToken
      let expression = parser.parseOperatorExpression
      let close = parser.matchToken(tokenParanthesisClose)
      return Node(kind: nodeParanthesisExpression, open: open, expression: expression, close: close)
   elif parser.current.kind in [tokenTrue, tokenFalse, tokenNumber]:
      let token = parser.nextToken
      return Node(kind: nodeLiteral, literal: token)
   elif parser.current.kind == tokenIdentifier:
      let token = parser.nextToken
      return Node(kind: nodeIdentifier, identifier: token)
   else:
      parser.diagnostics.report("Parser: Unexpected token " & escape(
            $parser.current.kind) & " for primary expression", parser.current.pos)
      return Node()

func parseOperatorExpression(parser: var Parser, parentPrecedence = 0): Node =
   let unaryOperatorPrecedence = getUnaryOperatorPrecedence(parser.current.kind)
   if unaryOperatorPrecedence != 0 and unaryOperatorPrecedence >= parentPrecedence:
      let operatorToken = parser.nextToken
      let operand = parser.parseOperatorExpression(unaryOperatorPrecedence)
      result = Node(kind: nodeUnaryExpression, unaryOperator: operatorToken,
            unaryOperand: operand)
   else:
      result = parser.parsePrimaryExpression

   while true:
      var precedence = getBinaryOperatorPrecedence(parser.current.kind)
      if precedence == 0 or precedence <= parentPrecedence:
         break
      let binaryOperator = parser.nextToken
      let right = parser.parseOperatorExpression(precedence)
      result = Node(kind: nodeBinaryExpression, left: result,
            binaryOperator: binaryOperator, right: right)


func parse*(text: string): Parser =
   result = Parser(root: Node())
   result.lexer = text.lex
   for r in result.lexer.diagnostics:
      result.diagnostics.add(r)

   let left = result.parseExpression
   result.matchToken(tokenEof)

   result.root = left
