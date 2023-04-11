import unittest, strutils
import ../src/lang/lexer

proc assertEquals(input: string, expected: seq[Token]) =
   var l = lex(input)
   doAssert expected.len == l.len,
      "Expected " & $expected.len & " tokens, but got " & $l.len & " for input string " & escape(input)
   for i, expectedToken in expected:
      let actualToken = l.get(i)
      doAssert actualToken.kind == expectedToken.kind,
         "Expected " & escape($expectedToken.kind) & " but got " & escape($actualToken.kind) &
               " at position " & $i & " for input string " & escape(input)
      doAssert actualToken.text == expectedToken.text,
         "Expected " & escape($expectedToken.text) & " but got " & escape($actualToken.text) &
               " at position " & $i & " for input string " & escape(input)

iterator validTokens(): Token =
   for kind in TokenKind:
      var text = $kind
      case kind
      of tokenEof: continue
      of triviaTokens: continue
      of tokenIdentifier: text = "abc"
      of tokenNumber: text = "123"
      of tokenString: text = "\"text\""
      else: discard
      yield Token(kind: kind, text: text)

let eof = Token(kind: tokenEof)

func requiresSeparator(a, b: TokenKind): bool =
   if ($a)[0] in IdentChars + {'['} and ($b)[0] in IdentChars + {'['}: return true
   if [a, b] == [tokenEquals, tokenEquals]: return true
   if [a, b] == [tokenEquals, tokenEqualsEquals]: return true
   if [a, b] == [tokenGreater, tokenEquals]: return true
   if [a, b] == [tokenGreater, tokenEqualsEquals]: return true
   if [a, b] == [tokenLess, tokenEquals]: return true
   if [a, b] == [tokenLess, tokenEqualsEquals]: return true

   if [a, b] == [tokenLessEquals, tokenGreater]: return true
   if [a, b] == [tokenLessEquals, tokenGreaterEquals]: return true

   if [a, b] == [tokenBang, tokenEquals]: return true
   if [a, b] == [tokenBang, tokenEqualsEquals]: return true

   if [a, b] == [tokenAmp, tokenAmp]: return true
   if [a, b] == [tokenAmp, tokenAmpAmp]: return true

   if [a, b] == [tokenPipe, tokenPipe]: return true
   if [a, b] == [tokenPipe, tokenPipePipe]: return true

   return false

suite "Lexer":
   test "Empty document":
      assertEquals("", @[eof])

   test "Single token":
      for token in validTokens():
         assertEquals(token.text, @[token, eof])

   test "Two tokens without separator":
      for token1 in validTokens():
         for token2 in validTokens():
            if requiresSeparator(token1.kind, token2.kind): continue
            assertEquals(token1.text & token2.text, @[token1, token2, eof])

   test "Two tokens with separator":
      let separator = Token(kind: tokenWhitespace, text: " ")
      for token1 in validTokens():
         for token2 in validTokens():
            assertEquals(token1.text & separator.text & token2.text, @[token1, token2, eof])
