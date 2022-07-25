import strutils
import parser, binder, evaluator, diagnostics


var showTree = false

while true:
   write(stdout, "> ")
   var line: string
   discard readLine(stdin, line)
   case line
   of "": echo "Quit"; break
   of "#showTree": showTree = not showTree; echo("showTree: " & $showTree); continue
   var parser = line.parse()
   if showTree: echo $parser.root
   if parser.diagnostics.len > 0:
      for report in parser.diagnostics:
         writeLine(stdout, " ".repeat(report.pos+2) & "^  " & report.msg)
      continue

   var binder = newBinder(parser.root)
   if binder.diagnostics.len > 0:
      for report in binder.diagnostics:
         writeLine(stdout, " ".repeat(report.pos+2) & "^  " & report.msg)
      continue

   writeline(stdout, $binder.root.evaluate)
