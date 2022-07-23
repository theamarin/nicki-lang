import parser, binder, evaluator


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
      for d in parser.diagnostics:
         writeLine(stdout, d)
      continue

   var binder = newBinder(parser.root)
   if binder.diagnostics.len > 0:
      for d in binder.diagnostics:
         writeLine(stdout, d)
      continue

   writeline(stdout, $binder.root.evaluate)
