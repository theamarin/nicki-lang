import lexer

func getUnaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of tokenMinus, tokenPlus: return 3
   else: return 0

func getBinaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of tokenMinus, tokenPlus: return 1
   of tokenStar, tokenSlash: return 2
   else: return 0
