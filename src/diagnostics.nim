type
   Report = ref object
      msg*: string
      pos*: int
   Diagnostics* = object
      reports: seq[Report]



func report*(d: var Diagnostics, msg: string, pos: int) =
   d.reports.add(Report(msg: msg, pos: pos))

func add*(d: var Diagnostics, report: Report) =
   d.reports.add(report)

func len*(d: Diagnostics): int = d.reports.len

func clear*(d: var Diagnostics) = d.reports = newSeq[Report]()

iterator items*(d: Diagnostics): Report =
   for r in d.reports:
      yield r
