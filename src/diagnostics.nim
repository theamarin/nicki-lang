import strutils

type
   Position* = object
      line*, column*, abs*: int
   Span* = ref object
      start*, `end`*: Position
   Report* = ref object
      msg*: string
      pos*: Position
   Diagnostics* = object
      reports: seq[Report]

func `$`*(pos: Position): string =
   return $(pos.line+1) & ":" & $(pos.column+1)

func add*(d: var Diagnostics, report: Report) =
   d.reports.add(report)

func len*(d: Diagnostics): int = d.reports.len

func clear*(d: var Diagnostics) = d.reports = newSeq[Report]()

iterator items*(d: Diagnostics): Report =
   for r in d.reports:
      yield r

func report(d: var Diagnostics, msg: string, pos: Position) =
   d.reports.add(Report(msg: msg, pos: pos))

# Lexer
func reportCannotParseNumber*(self: var Diagnostics, pos: Position, text: string) =
   self.report("Cannot parse number: " & escape(text), pos)

func reportBadCharacter*(self: var Diagnostics, pos: Position, text: string) =
   self.report("Bad character input " & escape(text), pos)

func reportUnterminatedString*(self: var Diagnostics, pos: Position) =
   self.report("Unterminated string", pos)

# Parser
func reportUnexpectedToken*(self: var Diagnostics, pos: Position, actual: string,
      expected: string, expressionKind: string) =
   self.report("Unexpected token " & escape(actual) & ", expected " & escape(expected) & " in " &
         escape(expressionKind), pos)

func reportIncompleteDefinition*(self: var Diagnostics, pos: Position, name: string) =
   self.report("Incomplete definition of " & escape(name) &
         ", requires either data type or initial value", pos)

# Binder
func reportUndefinedUnaryOperator*(self: var Diagnostics, pos: Position, opKind: string,
      dtype: string) =
   self.report("Unary operator " & escape(opKind) & " not defined for dtype " &
         escape(dtype), pos)

func reportUndefinedBinaryOperator*(self: var Diagnostics, pos: Position,
      opKind: string, leftDtype: string, rightDtype: string) =
   self.report("Binary operator " & escape(opKind) & " not defined for dtypes " &
         escape(leftDtype) & " and " & escape(rightDtype), pos)

func reportUndefinedIdentifier*(self: var Diagnostics, pos: Position, name: string) =
   self.report("Undefined identifier " & escape(name), pos)

func reportAlreadyDefinedIdentifier*(self: var Diagnostics, pos: Position, name: string) =
   self.report("Identifier " & escape(name) & " already defined", pos)

func reportCannotCast*(self: var Diagnostics, pos: Position, dtypeFrom: string, dtypeTo: string) =
   self.report("Cannot cast " & escape(dtypeFrom) & " to data type " & escape(dtypeTo), pos)

func reportDefinitionHint*(self: var Diagnostics, pos: Position, name: string) =
   self.report("Hint: Definition of " & escape(name) & " was here", pos)

func reportConditionNotBoolean*(self: var Diagnostics, pos: Position) =
   self.report("Condition is not boolean", pos)

func reportMissingElse*(self: var Diagnostics, pos: Position, dtype: string) =
   self.report("Missing else to return data type " & escape(dtype), pos)

func reportInconsistentConditionals*(self: var Diagnostics, pos: Position,
      conditionToken: string, conditionalDtype: string,
      otherwiseToken: string, otherwiseDtype: string) =
   self.report("Inconsistent data type in conditional expression: " &
         escape(conditionToken) & " returns type " & escape(conditionalDtype) &
         ", but " & escape(otherwiseToken) & " returns type " & escape(otherwiseDtype),
         pos)
