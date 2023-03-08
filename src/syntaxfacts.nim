import lexer

# Operator precedence is close to C,
# but uses additional <=> operator
# see: https://en.cppreference.com/w/c/language/operator_precedence

func getUnaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of tokenBang, tokenMinus, tokenPlus: return 15
   else: return 0

func getBinaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of tokenComma: return 1
   of tokenEquals: return 2 # Assignments
   # 3: Ternary conditional
   of tokenOr, tokenPipePipe: return 4 # Logical OR
   of tokenXor: return 5 # Logical AND
   of tokenAnd, tokenAmpAmp: return 6 # Logical AND
   of tokenPipe: return 7 # Bitwise OR
   of tokenCaret: return 8 # Bitwise XOR
   of tokenAmp: return 9 # Bitwise AND
   of tokenEqualsEquals, tokenBangEquals, tokenLess, tokenLessEquals,
      tokenGreater, tokenGreaterEquals, tokenCombinedComparison:
      return 10 # relational: equals
   # 11: relational: other
   # 12: Bitwise shift
   of tokenMinus, tokenPlus: return 13 # Addition and subtraction
   of tokenStar, tokenSlash, tokenPercent: return 14 # Multiplication, division, remainder
   # 15: Unaries (not, identity, negation) -- see getUnaryOperatorPrecedence()
   # 16: Function calls etc.
   else: return 0
