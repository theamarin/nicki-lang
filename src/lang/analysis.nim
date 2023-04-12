import strutils, tables
import parser, binder, diagnostics, lowerer, control_flow, identifiers, evaluator

export BoundKind

type AnalysisSettings* = object
   filename*: string
   showParseTree*: bool
   showBoundTree*: bool
   showLoweredTree*: bool
   showVars*: bool
   doLower*: bool

type AnalysisContext* = object
   binder: Binder
   evaluator*: Evaluator
   settings: AnalysisSettings
   lines: seq[string]

func newSettings*(): AnalysisSettings =
   return AnalysisSettings(doLower: true)

func newContext*(settings: AnalysisSettings): AnalysisContext =
   let b = newBinder()
   let e = newEvaluator(b)
   return AnalysisContext(settings: settings, binder: b, evaluator: e)

proc showVars*(context: AnalysisContext) =
   echo "Identifiers: "
   for name, identifier in context.binder.root.scope.identifiers:
      echo " " & $identifier

   echo "Variables: "
   for name, variable in context.evaluator.variables:
      echo " " & $name & ": " & variable.typeStr & " = " & $variable

proc showReport(context: AnalysisContext, report: Report) =
   if context.settings.filename.len > 0:
      writeLine(stdout, context.lines[report.pos.line])
      writeLine(stdout, " ".repeat(report.pos.column) & "^  " & report.msg & " in " &
            context.settings.filename & ":" & $report.pos & " [" & $report.kind & "]")
   if report.pos.line == context.lines.len - 1:
      writeLine(stdout, "  " & " ".repeat(report.pos.column) & "^  " & report.msg & " [" &
            $report.kind & "]")
   else:
      writeLine(stdout, "  " & context.lines[report.pos.line])
      writeLine(stdout, "  " & " ".repeat(report.pos.column) & "^  " & report.msg & " in line " &
            $report.pos.line & " [" & $report.kind & "]")

proc showDiagnostics(context: AnalysisContext, diagnostics: var Diagnostics): bool =
   for report in diagnostics: context.showReport(report)
   if diagnostics.len > 0:
      diagnostics.clear()
      return true
   else: return false

proc analyze*(context: var AnalysisContext, data: string): Bound =
   let settings = context.settings
   let lines = data.splitLines()
   var myParser = data.parse(context.lines.len)
   context.lines.add(lines)
   if settings.showParseTree: echo myParser.root.asTree()
   if context.showDiagnostics(myParser.diagnostics):
      return Bound(kind: boundError)

   let identifiersBackup = context.binder.root.scope.identifiers
   let bound = context.binder.bindExpression(myParser.root)
   if settings.showBoundTree: echo bound.asTree()
   if context.showDiagnostics(context.binder.diagnostics):
      context.binder.root.scope.identifiers = identifiersBackup # Reset scope on error
      return Bound(kind: boundError)

   if not settings.doLower: return bound

   let lowered = bound.lower()
   if settings.showLoweredTree: echo lowered.asTree()
   lowered.checkControlFlows()
   if context.showDiagnostics(context.binder.diagnostics):
      return Bound(kind: boundError)

   return lowered
