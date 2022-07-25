import strutils, tables
import dtype

type
   TokenKind* = enum
      tokenBad
      tokenEof

      tokenWhitespace
      tokenNumber
      tokenIdentifier

      # Keywords
      tokenTrue
      tokenFalse

      # Operators
      tokenComma
      tokenCaret
      tokenEquals
      tokenEqualsEquals
      tokenBang
      tokenBangEquals
      tokenPercent
      tokenAmp
      tokenAmpAmp
      tokenPipe
      tokenPipePipe
      tokenPlus
      tokenMinus
      tokenStar
      tokenSlash

      # Parentheses
      tokenParanthesisOpen
      tokenParanthesisClose

   Token* = ref object
      case kind*: TokenKind
      of tokenNumber: value*: Value
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

type
   KeywordMatch = tuple
      keyword: string
      token: TokenKind
   KeywordList = seq[KeywordMatch]
   Keywords = Table[string, TokenKind]

const
   keywordList: KeywordList = @[
      ("true", tokenTrue),
      ("false", tokenFalse),
   ]
   keywords: Keywords = keywordList.toTable


func current(l: Lexer): char =
   if l.position >= l.text.len: return '\0'
   return l.text[l.position]

func next(l: var Lexer): int {.discardable.} =
   result = l.position
   l.position.inc


func peek(l: Lexer, n = 1): char =
   let pos = l.position + n
   if pos >= l.text.len: return '\0'
   return l.text[pos]


func lexNumber(l: var Lexer): Token =
   let start = l.position
   while l.current() in Digits: l.next
   let text = l.text.substr(start, l.position - 1)
   let valInt =
      try: parseInt(text)
      except ValueError:
         l.diagnostics.add("Cannot parse number: " & escape(text))
         0
   let value = Value(dtype: tint, valInt: valInt)
   return Token(kind: tokenNumber, position: l.position, text: text, value: value)

func lexWhitespace(l: var Lexer): Token =
   let start = l.position
   while l.current() in Whitespace: l.next
   let text = l.text.substr(start, l.position - 1)
   return Token(kind: tokenWhitespace, position: l.position, text: text)

func lexWord(l: var Lexer): Token =
   let start = l.position
   while l.current() in Letters: l.next
   let text = l.text.substr(start, l.position - 1)
   let token = if text in keywords: keywords[text] else: tokenIdentifier
   return Token(kind: token, position: l.position, text: text)

func newToken(l: Lexer, kind: TokenKind, text: string): Token =
   result = Token(kind: kind, position: l.position, text: text)
   l.position += text.len

func nextToken(l: var Lexer): Token =
   if l.position >= l.text.len:
      return Token(kind: tokenEof, position: l.position, text: "\0")
   case l.current()
   of Letters:
      return l.lexWord
   of Digits:
      return l.lexNumber
   of Whitespace:
      return l.lexWhitespace
   of '+':
      return l.newToken(tokenPlus, "+")
   of '-':
      return l.newToken(tokenMinus, "-")
   of '*':
      return l.newToken(tokenStar, "*")
   of '/':
      return l.newToken(tokenSlash, "/")
   of '(':
      return l.newToken(tokenParanthesisOpen, "(")
   of ')':
      return l.newToken(tokenParanthesisClose, ")")
   of '&':
      if l.peek == '&': return l.newToken(tokenAmpAmp, "&&")
      else: return l.newToken(tokenAmp, "&")
   of '|':
      if l.peek == '|': return l.newToken(tokenPipePipe, "||")
      else: return l.newToken(tokenPipe, "|")
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
