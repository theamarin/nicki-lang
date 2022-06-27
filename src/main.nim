import parser



while true:
   write(stdout, "> ")
   var line: string
   discard readLine(stdin, line)
   if line == "":
      echo "Quit"
      break
   var parser = line.parse()
   if parser.diagnostics.len > 0:
      for d in parser.diagnostics:
         writeLine(stdout, d)
   else:
      writeline(stdout, $parser.root.evaluate)
