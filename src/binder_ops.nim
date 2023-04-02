import tables
import lexer, identifiers, diagnostics

type
   BoundUnaryOperatorKind* = enum
      boundUnaryPlus = "identity"
      boundUnaryMinus = "negative"
      boundUnaryNot = "negation"

   BoundUnaryOperatorMatch = tuple
      tokenKind: TokenKind
      operandDtype: DtypeBase
   BoundUnaryOperatorResult = tuple
      operatorKind: BoundUnaryOperatorKind
      resultDtype: DtypeBase
   BoundUnaryOperator = tuple
      match: BoundUnaryOperatorMatch
      result: BoundUnaryOperatorResult
   BoundUnaryOperators = Table[BoundUnaryOperatorMatch, BoundUnaryOperatorResult]

   BoundBinaryOperatorKind* = enum
      boundBinaryAddition = "addition",
      boundBinarySubtraction = "subtraction",
      boundBinaryMultiplication = "multiplication",
      boundBinaryDivision = "division",
      boundBinaryEquals = "equals",
      boundBinaryNotEquals = "not equals",
      boundBinaryGreaterThan = "greater than",
      boundBinaryGreaterEquals = "greater equals",
      boundBinaryLessThan = "less than",
      boundBinaryLessEquals = "less or equals",
      boundBinaryCombinedComparison = "combined comparison",
      boundBinaryLogicalAnd = "logical and",
      boundBinaryLogicalOr = "logical or"
      boundBinaryLogicalXor = "logical xor"

   BoundBinaryOperatorMatch = tuple
      leftDtype: DtypeBase
      tokenKind: TokenKind
      rightDtype: DtypeBase
   BoundBinaryOperatorResult = tuple
      operatorKind: BoundBinaryOperatorKind
      resultDtype: DtypeBase
   BoundBinaryOperator = tuple
      match: BoundBinaryOperatorMatch
      result: BoundBinaryOperatorResult
   BoundBinaryOperators = Table[BoundBinaryOperatorMatch, BoundBinaryOperatorResult]


const
   boundUnaryOperatorList: seq[BoundUnaryOperator] = @[
      ((tokenPlus, tint), (boundUnaryPlus, tint)),
      ((tokenMinus, tint), (boundUnaryMinus, tint)),
      ((tokenBang, tbool), (boundUnaryNot, tbool)),
   ]
   boundUnaryOperators: BoundUnaryOperators = boundUnaryOperatorList.toTable

   boundBinaryOperatorList: seq[BoundBinaryOperator] = @[
      ((tint, tokenPlus, tint), (boundBinaryaddition, tint)),
      ((tint, tokenMinus, tint), (boundBinarySubtraction, tint)),
      ((tint, tokenStar, tint), (boundBinaryMultiplication, tint)),
      ((tint, tokenSlash, tint), (boundBinaryDivision, tint)),

      ((tbool, tokenEqualsEquals, tbool), (boundBinaryEquals, tbool)),
      ((tbool, tokenBangEquals, tbool), (boundBinaryNotEquals, tbool)),
      ((tbool, tokenGreater, tbool), (boundBinaryGreaterThan, tbool)),
      ((tbool, tokenGreaterEquals, tbool), (boundBinaryGreaterEquals, tbool)),
      ((tbool, tokenLess, tbool), (boundBinaryLessThan, tbool)),
      ((tbool, tokenLessEquals, tbool), (boundBinaryLessEquals, tbool)),

      ((tint, tokenEqualsEquals, tint), (boundBinaryEquals, tbool)),
      ((tint, tokenBangEquals, tint), (boundBinaryNotEquals, tbool)),
      ((tint, tokenGreater, tint), (boundBinaryGreaterThan, tbool)),
      ((tint, tokenGreaterEquals, tint), (boundBinaryGreaterEquals, tbool)),
      ((tint, tokenLess, tint), (boundBinaryLessThan, tbool)),
      ((tint, tokenLessEquals, tint), (boundBinaryLessEquals, tbool)),
      ((tint, tokenCombinedComparison, tint), (boundBinaryCombinedComparison,
            tint)),

      ((tbool, tokenAmpAmp, tbool), (boundBinaryLogicalAnd, tbool)),
      ((tbool, tokenAnd, tbool), (boundBinaryLogicalAnd, tbool)),
      ((tbool, tokenPipePipe, tbool), (boundBinaryLogicalOr, tbool)),
      ((tbool, tokenOr, tbool), (boundBinaryLogicalOr, tbool)),
      ((tbool, tokenCaret, tbool), (boundBinaryLogicalXor, tbool)),
      ((tbool, tokenXor, tbool), (boundBinaryLogicalXor, tbool)),
   ]
   boundBinaryOperators: BoundBinaryOperators = boundBinaryOperatorList.toTable


func getUnaryOperator*(diagnostics: var Diagnostics, token: Token,
      dtype: Dtype): BoundUnaryOperatorResult =
   if (token.kind, dtype.base) in boundUnaryOperators:
      return boundUnaryOperators[(token.kind, dtype.base)]
   elif dtype.base == terror: discard
   else: diagnostics.reportUndefinedUnaryOperator(token.pos, $token.kind, $dtype)

func getBinaryOperator*(diagnostics: var Diagnostics, leftDtype: Dtype, token: Token,
      rightDtype: Dtype): BoundBinaryOperatorResult =
   if (leftDtype.base, token.kind, rightDtype.base) in boundBinaryOperators:
      return boundBinaryOperators[(leftDtype.base, token.kind, rightDtype.base)]
   elif terror in [leftDtype.base, rightDtype.base]: discard
   else:
      diagnostics.reportUndefinedBinaryOperator(token.pos, $token.kind, $leftDtype, $rightDtype)


func asTree*(x: BoundUnaryOperatorKind): string = $x
func asTree*(x: BoundBinaryOperatorKind): string = $x

func asCode*(x: BoundUnaryOperatorKind): string =
   case x
   of boundUnaryPlus: return "+"
   of boundUnaryMinus: return "-"
   of boundUnaryNot: return "not"

func asCode*(x: BoundBinaryOperatorKind): string =
   case x
   of boundBinaryAddition: return "+"
   of boundBinarySubtraction: return "-"
   of boundBinaryMultiplication: return "*"
   of boundBinaryDivision: return "/"
   of boundBinaryEquals: return "=="
   of boundBinaryNotEquals: return "!="
   of boundBinaryGreaterThan: return ">"
   of boundBinaryGreaterEquals: return ">="
   of boundBinaryLessThan: return "<"
   of boundBinaryLessEquals: return "<="
   of boundBinaryCombinedComparison: return "<=>"
   of boundBinaryLogicalAnd: return "and"
   of boundBinaryLogicalOr: return "or"
   of boundBinaryLogicalXor: return "xor"
