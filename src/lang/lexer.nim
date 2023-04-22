import strutils, tables
import identifiers, diagnostics

type
   ValueBase* = ref object
      case dtypeBase*: DtypeBase
      of tbool: valBool*: bool
      of tint: valInt*: int
      of tstr: valStr*: string
      else: discard

   TokenKind* = enum
      tokenBad = "[BAD]"
      tokenEof = "[EOF]"

      tokenWhitespace = "[whitespace]"
      tokenNewline = "[newline]"
      tokenIdentifier = "[identifier]"
      tokenNumber = "[number]"
      tokenString = "[string]"
      tokenComment = "[#]"

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
      tokenConst = "const"
      tokenPublic = "public"
      tokenObject = "object"
      tokenEnum = "enum"
      tokenStruct = "struct"
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
      of tokenNumber, tokenString, tokenComment:
         value*: ValueBase
      else: discard
      pos*: Position
      text*: string
      leadingTrivia*, trailingTrivia*: seq[Token]

func prettyPrint*(key, value: string): string =
   const align = 16
   result = key
   if key.len < align:
      result &= " ".repeat(align - key.len)
   result &= " " & value

func `$`*(val: ValueBase): string =
   case val.dtypeBase
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstr: return $val.valStr
   else: return "[" & $val.dtypeBase & "]"

func asCode*(val: ValueBase): string =
   case val.dtypeBase
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstr: return escape(val.valStr)
   else: return "[" & $val.dtypeBase & "]"

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
      ("const", tokenConst),
      ("public", tokenPublic),
      ("object", tokenObject),
      ("enum", tokenEnum),
      ("struct", tokenStruct),
      ("continue", tokenContinue),
      ("break", tokenBreak),
      ("return", tokenReturn),
      ("yield", tokenYield),
      ("discard", tokenDiscard),
   ]
   keywords: Keywords = keywordList.toTable

   whitespaceChars = {' ', '\t'}
   newlineChars = {'\v', '\r', '\l', '\f'}

   literalTokens* = {tokenNumber, tokenString, tokenTrue, tokenFalse}
   triviaTokens* = {tokenBad, tokenWhitespace, tokenNewline, tokenComment}


func current(l: Lexer): char =
   if l.pos.abs >= l.text.len: return '\0'
   return l.text[l.pos.abs]

func peek(l: Lexer, n = 1): char =
   let pos = l.pos.abs + n
   if pos >= l.text.len: return '\0'
   return l.text[pos]

func advance(l: Lexer, n = 1) =
   for i in 0..n-1:
      if l.current() == '\r' and l.peek() == '\n':
         l.pos.column.inc
      elif l.current() in Newlines:
         l.pos.line.inc
         l.pos.column = 0
      else: l.pos.column.inc
      l.pos.abs.inc

func next(l: Lexer): Position {.discardable.} =
   result = l.pos
   l.advance()

func lexNumber(l: Lexer): Token =
   let start = l.pos
   while l.current() in Digits: l.next
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   let valInt =
      try: parseInt(text)
      except ValueError:
         l.diagnostics.reportCannotParseNumber(start, text); 0
   let value = ValueBase(dtypeBase: tint, valInt: valInt)
   return Token(kind: tokenNumber, pos: start, text: text, value: value)

func lexWhitespaces(l: Lexer): Token =
   let start = l.pos
   while l.current() in whitespaceChars: l.next
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   return Token(kind: tokenWhitespace, pos: start, text: text)

func lexNewlines(l: Lexer): Token =
   let start = l.pos
   while l.current() in newlineChars: l.next
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   return Token(kind: tokenNewline, pos: start, text: text)

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
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   let valStr = l.text.substr(start.abs+1, l.pos.abs - 2)
   let value = ValueBase(dtypeBase: tstr, valStr: valStr)
   return Token(kind: tokenString, pos: start, text: text, value: value)

func lexComment(l: Lexer): Token =
   let start = l.pos
   doAssert l.current() == '#'
   l.next
   while l.current() in whitespaceChars: l.next()
   let commentTextStart = l.pos
   while l.current() notin {'\0', '\n', '\r'}: l.next()
   let text = l.text.substr(start.abs, l.pos.abs - 1)
   let valStr = l.text.substr(commentTextStart.abs, l.pos.abs - 1)
   let value = ValueBase(dtypeBase: tstr, valStr: valStr)
   return Token(kind: tokenComment, pos: start, text: text, value: value)

func newToken(l: Lexer, kind: TokenKind, text: string = $kind): Token =
   result = Token(kind: kind, pos: l.pos, text: text)
   l.advance(text.len)

func nextToken(l: Lexer): Token =
   case l.current()
   of '\0':
      return l.newToken(tokenEof, "")
   of Letters:
      return l.lexWord
   of Digits:
      return l.lexNumber
   of whitespaceChars:
      return l.lexWhitespaces
   of newlineChars:
      return l.lexNewlines
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
   of '%':
      return l.newToken(tokenPercent)
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
   of '#':
      return l.lexComment()
   of '"':
      return l.lexString()
   else:
      l.diagnostics.reportBadCharacter(l.pos, $l.current())
      let text: string = $l.text[l.pos.abs]
      return Token(kind: tokenBad, pos: l.next, text: text)

func lex*(text: string, lineIdx = 0): Lexer =
   result = Lexer(text: text, pos: Position(line: lineIdx))

   var prevToken: Token = nil
   var isLeading = true
   var leadingTrivia: seq[Token]
   while true:
      let token = result.nextToken()
      if token.kind notin triviaTokens:
         result.tokens.add(token)
         token.leadingTrivia = leadingTrivia
         leadingTrivia = newSeq[Token]()
         isLeading = false
         prevToken = token
      elif token.kind == tokenNewline:
         isLeading = true
         leadingTrivia.add(token)
      elif isLeading:
         leadingTrivia.add(token)
      else:
         prevToken.trailingTrivia.add(token)
      if token.kind == tokenEof:
         break

func len*(lexer: Lexer): int = return lexer.tokens.len

func get*(lexer: Lexer, index: int): Token =
   if lexer.tokens.len == 0: return Token(kind: tokenEof)
   elif index >= lexer.tokens.len: return lexer.tokens[^1]
   else: return lexer.tokens[index]
