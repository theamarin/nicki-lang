import strutils

type
   TokenKind* = enum
      tokenBad
      tokenEof

      tokenWhitespace
      tokenNumber
      tokenPlus
      tokenMinus
      tokenStar
      tokenSlash
      tokenParanthesisOpen
      tokenParanthesisClose

   Token* = ref object
      case kind*: TokenKind
      of tokenNumber: valInt*: int
      else: discard
      position: int
      text: string

# func kind*(token: Token): TokenKind = token.kind
func `$`*(token: Token): string =
   result = $token.kind

type
   Lexer* = ref object
      text: string
      position: int
      tokens: seq[Token]
      diagnostics: seq[string]

func getDiagnostics*(lexer: Lexer): seq[string] = lexer.diagnostics


func current(l: Lexer): char =
   if l.position >= l.text.len: return '\0'
   return l.text[l.position]

func next(l: var Lexer): int {.discardable.} =
   l.position.inc
   return l.position

func lexNumber(l: var Lexer): Token =
   let start = l.position
   while l.current() in Digits: l.next
   let text = l.text.substr(start, l.position - 1)
   let valInt =
      try: parseInt(text)
      except ValueError:
         l.diagnostics.add("Cannot parse number: " & escape(text))
         0
   return Token(kind: tokenNumber, position: l.position, text: text, valInt: valInt)

func lexWhitespace(l: var Lexer): Token =
   let start = l.position
   while l.current() in Whitespace: l.next
   let text = l.text.substr(start, l.position - 1)
   return Token(kind: tokenWhitespace, position: l.position, text: text)

func nextToken(l: var Lexer): Token =
   # numbers
   # operators: + - * / ( )
   # whitespace
   if l.position >= l.text.len:
      return Token(kind: tokenEof, position: l.position, text: "\0")
   case l.current()
   of Digits:
      return l.lexNumber
   of Whitespace:
      return l.lexWhitespace
   of '+':
      return Token(kind: tokenPlus, position: l.next, text: "+")
   of '-':
      return Token(kind: tokenMinus, position: l.next, text: "-")
   of '*':
      return Token(kind: tokenStar, position: l.next, text: "*")
   of '/':
      return Token(kind: tokenSlash, position: l.next, text: "/")
   of '(':
      return Token(kind: tokenParanthesisOpen, position: l.next, text: "(")
   of ')':
      return Token(kind: tokenParanthesisClose, position: l.next, text: ")")
   else:
      l.diagnostics.add("Error: Bad character input: " & escape($l.current()))
      let text: string = $l.text[l.position]
      return Token(kind: tokenBad, position: l.next, text: text)

func lex*(text: string): Lexer =
   result = Lexer(text: text)

   while true:
      let token = result.nextToken()
      if token.kind notin [tokenWhitespace, tokenBad]:
         result.tokens.add(token)
      if token.kind == tokenEof:
         break

func get*(lexer: Lexer, index: int): Token =
   if index >= lexer.tokens.len: return lexer.tokens[^1]
   else: return lexer.tokens[index]
