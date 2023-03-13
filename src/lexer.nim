import strutils, tables
import identifiers, diagnostics

type
   TokenKind* = enum
      tokenBad = "[BAD]"
      tokenEof = "[EOF]"

      tokenWhitespace = "[whitespace]"
      tokenIdentifier = "[identifier]"
      tokenNumber = "[number]"
      tokenString = "[string]"

      # Keywords (need to be added to keyword list below)
      tokenTrue = "true"
      tokenFalse = "false"
      tokenAnd = "and"
      tokenOr = "or"
      tokenXor = "xor"
      tokenIf = "if"
      tokenElif = "elif"
      tokenElse = "else"
      tokenFor = "for"
      tokenWhile = "while"
      tokenDef = "def"
      tokenType = "type"
      tokenConst = "const"
      tokenPublic = "public"
      tokenObject = "object"
      tokenEnum = "enum"
      tokenContinue = "continue"
      tokenBreak = "break"
      tokenReturn = "return"
      tokenYield = "yield"
      tokenDiscard = "discard"

      # Operators
      tokenColon = ":"
      tokenComma = ","
      tokenCaret = "^"
      tokenEquals = "="
      tokenEqualsEquals = "=="
      tokenGreater = ">"
      tokenGreaterEquals = ">="
      tokenLess = "<"
      tokenLessEquals = "<="
      tokenCombinedComparison = "<=>"
      tokenBang = "!"
      tokenBangEquals = "!="
      tokenPercent = "%"
      tokenAmp = "&"
      tokenAmpAmp = "&&"
      tokenPipe = "|"
      tokenPipePipe = "||"
      tokenPlus = "+"
      tokenMinus = "-"
      tokenStar = "*"
      tokenSlash = "/"

      # Parentheses
      tokenParanthesisOpen = "("
      tokenParanthesisClose = ")"
      tokenBraceOpen = "{"
      tokenBraceClose = "}"

   Token* = ref object
      case kind*: TokenKind
      of tokenNumber: value*: Value
      else: discard
      pos*: Position
      text*: string

func prettyPrint*(key, value: string): string =
   const align = 20
   result = value
   if value.len < align:
      result &= " ".repeat(align - value.len)
   result &= " " & key

func `$`*(token: Token): string =
   if token.isNil: return ""
   result &= token.text
   if $token.kind != token.text: result &= " " & $token.kind

type
   Lexer* = ref object
      text: string
      pos: Position
      tokens: seq[Token]
      diagnostics*: Diagnostics

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
      ("and", tokenAnd),
      ("or", tokenOr),
      ("xor", tokenXor),
      ("if", tokenIf),
      ("elif", tokenElif),
      ("else", tokenElse),
      ("for", tokenFor),
      ("while", tokenWhile),
      ("def", tokenDef),
      ("type", tokenType),
      ("const", tokenConst),
      ("public", tokenPublic),
      ("object", tokenObject),
      ("enum", tokenEnum),
      ("continue", tokenContinue),
      ("break", tokenBreak),
      ("return", tokenReturn),
      ("yield", tokenYield),
      ("discard", tokenDiscard),
   ]
   keywords: Keywords = keywordList.toTable

   literalTokens* = {tokenNumber, tokenString, tokenTrue, tokenFalse}


func current(l: Lexer): char =
   if l.pos.abs >= l.text.len: return '\0'
   return l.text[l.pos.abs]

func advance(l: Lexer, n = 1) =
   for i in 0..n-1:
      if l.current() in Newlines:
         l.pos.line.inc
         l.pos.column = 0
      else: l.pos.column.inc
      l.pos.abs.inc

func next(l: Lexer): Position {.discardable.} =
   result = l.pos
   l.advance()

func peek(l: Lexer, n = 1): char =
   let pos = l.pos.abs + n
   if pos >= l.text.len: return '\0'
   return l.text[pos]

func lexNumber(l: Lexer): Token =
   let start = l.pos
   while l.current() in Digits: l.next
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   let valInt =
      try: parseInt(text)
      except ValueError:
         l.diagnostics.reportCannotParseNumber(start, text); 0
   let value = Value(dtype: tint, valInt: valInt)
   return Token(kind: tokenNumber, pos: start, text: text, value: value)

func lexWhitespace(l: Lexer): Token =
   let start = l.pos
   while l.current() in Whitespace: l.next
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   return Token(kind: tokenWhitespace, pos: start, text: text)

func lexWord(l: Lexer): Token =
   let start = l.pos
   doAssert l.current() in IdentStartChars
   while l.current() in IdentChars: l.next
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   let token = if text in keywords: keywords[text] else: tokenIdentifier
   return Token(kind: token, pos: start, text: text)

func lexString(l: Lexer): Token =
   let start = l.pos
   doAssert l.current() == '"'
   l.next
   while l.current() notin {'"', '\0', '\n', '\r'}:
      l.next
   if l.current() == '"':
      l.next
   else:
      l.diagnostics.reportUnterminatedString(start)
   let text = l.text.substr(start.abs+1, l.pos.abs - 2)
   return Token(kind: tokenString, pos: start, text: text)

func newToken(l: Lexer, kind: TokenKind, text = ""): Token =
   let myText = if text.len > 0: text else: $kind
   result = Token(kind: kind, pos: l.pos, text: myText)
   l.advance(myText.len)

func nextToken(l: Lexer): Token =
   case l.current()
   of '\0':
      return l.newToken(tokenEof, "\0")
   of Letters:
      return l.lexWord
   of Digits:
      return l.lexNumber
   of Whitespace:
      return l.lexWhitespace
   of '+':
      return l.newToken(tokenPlus)
   of '-':
      return l.newToken(tokenMinus)
   of '*':
      return l.newToken(tokenStar)
   of '/':
      return l.newToken(tokenSlash)
   of ',':
      return l.newToken(tokenComma)
   of ':':
      return l.newToken(tokenColon)
   of '^':
      return l.newToken(tokenCaret)
   of '&':
      if l.peek == '&': return l.newToken(tokenAmpAmp)
      else: return l.newToken(tokenAmp)
   of '|':
      if l.peek == '|': return l.newToken(tokenPipePipe)
      else: return l.newToken(tokenPipe)
   of '=':
      if l.peek == '=': return l.newToken(tokenEqualsEquals)
      else: return l.newToken(tokenEquals)
   of '!':
      if l.peek == '=': return l.newToken(tokenBangEquals)
      else: return l.newToken(tokenBang)
   of '>':
      if l.peek == '=': return l.newToken(tokenGreaterEquals)
      else: return l.newToken(tokenGreater)
   of '<':
      if l.peek == '=':
         if l.peek(2) == '>':
            return l.newToken(tokenCombinedComparison)
         return l.newToken(tokenLessEquals)
      else: return l.newToken(tokenLess)
   of '(':
      return l.newToken(tokenParanthesisOpen)
   of ')':
      return l.newToken(tokenParanthesisClose)
   of '{':
      return l.newToken(tokenBraceOpen)
   of '}':
      return l.newToken(tokenBraceClose)
   of '"':
      return l.lexString()
   else:
      l.diagnostics.reportBadCharacter(l.pos, $l.current())
      let text: string = $l.text[l.pos.abs]
      return Token(kind: tokenBad, pos: l.next, text: text)

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
