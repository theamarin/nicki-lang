import strutils, tables
import parser, binder, evaluator, diagnostics


var showTree = false
var showBind = false
var showVars = false

var myEvaluator = Evaluator()
var myBinder = newBinder()

while true:
   if showVars:
      echo "Identifiers: "
      for name, identifier in myBinder.scope.identifiers:
         echo " " & $name & ": " & $identifier.dtype

      echo "Variables: "
      for variable, value in myEvaluator.variables:
         echo " " & $variable & ": " & $value

   write(stdout, "> ")
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
         writeLine(stdout, " ".repeat(report.pos+2) & "^  " & report.msg)
      parser.diagnostics.clear
      continue

   let bound = myBinder.bindExpression(parser.root)
   if showBind: echo $bound
   if myBinder.diagnostics.len > 0:
      for report in myBinder.diagnostics:
         writeLine(stdout, " ".repeat(report.pos+2) & "^  " & report.msg)
      myBinder.diagnostics.clear
      continue

   let result = myEvaluator.evaluate(bound)
   writeline(stdout, $result)
