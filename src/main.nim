import strutils, tables, parseopt, os
import parser, binder, evaluator, diagnostics, identifiers

var showTree = false
var showBind = false
var showVars = false

var myEvaluator = Evaluator()
var myBinder = newBinder()

var filename = ""
var p = initOptParser(commandLineParams())
for kind, key, val in p.getopt():
   case kind
   of cmdArgument: filename = key
   of cmdLongOption, cmdShortOption:
      echo "Error: Unknown option " & escape(key)
      quit(QuitFailure)
   of cmdEnd: assert(false) # cannot happen
if filename != "":
   let f = open(filename, fmRead)
   let data = f.readAll()
   f.close()
   let lines = data.splitLines()
   var parser = data.parse()
   if parser.diagnostics.len > 0:
      for report in parser.diagnostics:
         writeLine(stdout, lines[report.pos.line])
         writeLine(stdout, " ".repeat(report.pos.column) & "^  " & report.msg & " in " &
               filename & ":" & $report.pos)
      quit(QuitFailure)
   let bound = myBinder.bindExpression(parser.root)
   if myBinder.diagnostics.len > 0:
      for report in myBinder.diagnostics:
         writeLine(stdout, lines[report.pos.line])
         writeLine(stdout, " ".repeat(report.pos.column) & "^  " & report.msg & " in " &
               filename & ":" & $report.pos)
      quit(QuitFailure)
   let result = myEvaluator.evaluate(bound)
   writeline(stdout, $result)
   quit(QuitSuccess)


const prompt = "> "

while true:
   if showVars:
      echo "Identifiers: "
      for name, identifier in myBinder.scope.identifiers:
         echo " " & $identifier

      echo "Variables: "
      for variable, value in myEvaluator.variables:
         echo " " & $variable & ": " & $value

   write(stdout, prompt)
   var line: string
   discard readLine(stdin, line)
   case line
   of "": echo "Quit"; break
   of "#showTree": showTree = not showTree; echo("showTree: " & $showTree); continue
   of "#showBind": showBind = not showBind; echo("showBind: " & $showBind); continue
   of "#showVars": showVars = not showVars; echo("showVars: " & $showVars); continue
   var parser = line.parse()
   if showTree: echo $parser.root
   if parser.diagnostics.len > 0:
      for report in parser.diagnostics:
         writeLine(stdout, " ".repeat(report.pos.column+prompt.len) & "^  " & report.msg)
      parser.diagnostics.clear
      continue

   let identifiersBackup = myBinder.scope.identifiers
   let bound = myBinder.bindExpression(parser.root)
   if showBind: echo $bound
   if myBinder.diagnostics.len > 0:
      for report in myBinder.diagnostics:
         writeLine(stdout, " ".repeat(report.pos.column+prompt.len) & "^  " & report.msg)
      myBinder.diagnostics.clear
      myBinder.scope.identifiers = identifiersBackup # Reset scope on error
      continue

   let result = myEvaluator.evaluate(bound)
   if result.dtype != tvoid: writeline(stdout, $result)
