import parser



while true:
   write(stdout, "> ")
   var line: string
   discard readLine(stdin, line)
   if line == "":
      echo "Quit"
      break
   var parser = line.parse()
