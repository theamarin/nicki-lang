import lexer

func getUnaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of tokenBang, tokenMinus, tokenPlus: return 14
   else: return 0

func getBinaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of tokenComma: return 1
   of tokenEquals: return 2 # Assignments
   # 3: Ternary conditional
   of tokenPipePipe: return 4 # Logical OR
   of tokenAmpAmp: return 5 # Logical AND
   of tokenPipe: return 6 # Bitwise OR
   of tokenCaret: return 7 # Bitwise XOR
   of tokenAmp: return 8 # Bitwise AND
   of tokenEqualsEquals, tokenBangEquals, tokenLess, tokenLessEquals, tokenGreater,
         tokenGreaterEquals: return 9 # relational: equals
   # 10: relational: other
   # 11: Bitwise shift
   of tokenMinus, tokenPlus: return 12 # Addition and subtraction
   of tokenStar, tokenSlash, tokenPercent: return 13 # Multiplication, division, remainder
   # 14: Unaries (not, identity, negation)
   # 15: Function calls etc.
   else: return 0
