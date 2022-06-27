import lexer

func getUnaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of token_minus, token_plus: return 3
   else: return 0

func getBinaryOperatorPrecedence*(tokenKind: TokenKind): int =
   case tokenKind
   of token_minus, token_plus: return 1
   of token_star, token_slash: return 2
   else: return 0
