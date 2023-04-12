import strutils, tables, os
import parser, binder, diagnostics, lowerer, control_flow, identifiers, evaluator

export BoundKind

type SourceText* = object
   filename*: string
   lines: seq[string]

type AnalysisSettings* = object
   showParseTree*: bool
   showBoundTree*: bool
   showLoweredTree*: bool
   showVars*: bool
   doLower*: bool

type AnalysisContext* = object
   binder: Binder
   evaluator*: Evaluator
   settings*: AnalysisSettings
   sourceText*: SourceText

func newSettings(): AnalysisSettings =
   return AnalysisSettings(doLower: true)

func newContext*(): AnalysisContext =
   let s = newSettings()
   let b = newBinder()
   let e = newEvaluator(b)
   return AnalysisContext(settings: s, binder: b, evaluator: e)

proc showVars*(context: AnalysisContext) =
   echo "Identifiers: "
   for name, identifier in context.binder.root.scope.identifiers:
      echo " " & $identifier

   echo "Variables: "
   for name, variable in context.evaluator.variables:
      echo " " & $name & ": " & variable.typeStr & " = " & $variable

proc showReport(context: AnalysisContext, report: Report) =
   if context.sourceText.filename.len > 0:
      writeLine(stdout, context.sourceText.filename.absolutePath, ":" & $report.pos & ": error: " &
            report.msg & " [" & $report.kind & "]")
      writeLine(stdout, "   " & context.sourceText.lines[report.pos.line])
      writeLine(stdout, "   " & " ".repeat(report.pos.column) & "^")
   elif report.pos.line == context.sourceText.lines.len - 1:
      writeLine(stdout, "  " & " ".repeat(report.pos.column) & "^  " & report.msg & " [" &
            $report.kind & "]")
   else:
      writeLine(stdout, "  " & context.sourceText.lines[report.pos.line])
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
   var myParser = data.parse(context.sourceText.lines.len)
   context.sourceText.lines.add(lines)
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
