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

func report*(d: var Diagnostics, msg: string, pos: Position) =
   d.reports.add(Report(msg: msg, pos: pos))

func add*(d: var Diagnostics, report: Report) =
   d.reports.add(report)

func len*(d: Diagnostics): int = d.reports.len

func clear*(d: var Diagnostics) = d.reports = newSeq[Report]()

iterator items*(d: Diagnostics): Report =
   for r in d.reports:
      yield r
