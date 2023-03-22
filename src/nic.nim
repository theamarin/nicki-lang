import strutils, parseopt, os
import parser, binder, diagnostics
import cwriter

var showTree = false
var showBind = false
var showVars = false
var filename = ""

var myBinder = newBinder()

const helpStr = dedent """
   nicki-lang compiler
   Usage: nim r src/nic.nim [OPTIONS] filename
   Options:
      -h, --help     Show this help"""


var p = initOptParser(commandLineParams())
for kind, key, val in p.getopt():
   case kind
   of cmdArgument: filename = key
   of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
         echo helpStr
         quit(QuitSuccess)
      else:
         echo "Error: Unknown option " & escape(key)
         quit(QuitFailure)
   of cmdEnd: assert(false) # cannot happen

if filename == "":
   echo "Error: Missing filename"
   quit(QuitFailure)

var filenameOut = filename.changeFileExt(".c")

let f = open(filename, fmRead)
let data = f.readAll()
f.close()
let lines = data.splitLines()
var myParser = data.parse()
if showTree: echo $myParser.root
if myParser.diagnostics.len > 0:
   for report in myParser.diagnostics:
      writeLine(stdout, lines[report.pos.line])
      writeLine(stdout, " ".repeat(report.pos.column) & "^  " & report.msg & " in " &
            filename & ":" & $report.pos)
   quit(QuitFailure)
let bound = myBinder.bindExpression(myParser.root)
if showBind: echo $bound
if myBinder.diagnostics.len > 0:
   for report in myBinder.diagnostics:
      writeLine(stdout, lines[report.pos.line])
      writeLine(stdout, " ".repeat(report.pos.column) & "^  " & report.msg & " in " &
            filename & ":" & $report.pos)
   quit(QuitFailure)
let result = compile(myBinder.root)
let fOut = open(filenameOut, fmWrite)
fOut.write(result.join("\p"))

quit(QuitSuccess)
