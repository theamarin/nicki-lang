import strutils

type
   TokenKind* = enum
      token_bad
      token_whitespace
      token_number
      token_plus
      token_minus
      token_star
      token_slash
      token_paranthesis_open
      token_paranthesis_close
      token_eof
   Token* = ref object
      case kind: TokenKind
      of token_number: value: int
      else: discard
      position: int
      text: string

func kind*(token: Token): TokenKind = token.kind
func `$`*(token: Token): string =
   result = $token.kind

type
   Lexer* = ref object
      text: string
      position: int
      tokens: seq[Token]


func current(l: Lexer): char =
   if l.position >= l.text.len: return '\0'
   return l.text[l.position]

func next(l: var Lexer): int {.discardable.} =
   l.position.inc
   return l.position


func nextToken(l: var Lexer): Token =
   # numbers
   # operators: + - * / ( )
   # whitespace
   if l.position >= l.text.len:
      return Token(kind: token_eof, position: l.position, text: "\0")
   if l.current() in Digits:
      let start = l.position
      while l.current() in Digits: l.next
      let text = l.text.substr(start, l.position - 1)
      let value = parseInt(text)
      return Token(kind: token_number, position: l.position, text: text, value: value)
   elif l.current() in Whitespace:
      let start = l.position
      while l.current() in Whitespace: l.next
      let text = l.text.substr(start, l.position - 1)
      return Token(kind: token_whitespace, position: l.position, text: text)
   elif l.current() == '+':
      return Token(kind: token_plus, position: l.next, text: "+")
   elif l.current() == '-':
      return Token(kind: token_minus, position: l.next, text: "-")
   elif l.current() == '*':
      return Token(kind: token_star, position: l.next, text: "*")
   elif l.current() == '/':
      return Token(kind: token_slash, position: l.next, text: "/")
   elif l.current() == '(':
      return Token(kind: token_paranthesis_open, position: l.next, text: "(")
   elif l.current() == ')':
      return Token(kind: token_paranthesis_close, position: l.next, text: ")")
   else:
      let text: string = $l.text[l.position]
      return Token(kind: token_bad, position: l.next, text: text)

func lex*(text: string): Lexer =
   result = Lexer(text: text)

   while true:
      let token = result.nextToken()
      if token.kind notin [token_whitespace, token_bad]:
         result.tokens.add(token)
      if token.kind == token_eof:
         break

func get*(lexer: Lexer, index: int): Token =
   if index >= lexer.tokens.len: return lexer.tokens[^1]
   else: return lexer.tokens[index]
