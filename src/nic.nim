import strutils, parseopt, os
import lang / [analysis, cwriter]

const helpStr = dedent """
   nicki-lang compiler
   Usage: nic [OPTIONS] filename
   Options:
      -h, --help           Show this help and exit
      --show-parse-tree    Show parse tree
      --show-bound-tree    Show bound tree
      --show-lowered-tree  Show lowered tree
      --show-vars          Show global-scope identifiers"""

var settings = newSettings()
var context = newContext(settings)

var p = initOptParser(commandLineParams())
for kind, key, val in p.getopt():
   case kind
   of cmdArgument: settings.filename = key
   of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
         echo helpStr
         quit(QuitSuccess)
      of "show-parse-tree": settings.showParseTree = true
      of "show-bound-tree": settings.showBoundTree = true
      of "show-vars": settings.showVars = true
      else:
         echo "Error: Unknown option " & escape(key)
         quit(QuitFailure)
   of cmdEnd: assert(false) # cannot happen

if settings.filename == "":
   echo "Error: Missing filename"
   quit(QuitFailure)

let f = open(settings.filename, fmRead)
let data = f.readAll()
f.close()

let bound = context.analyze(data)
if bound.kind == boundError:
   quit(QuitFailure)

let result = compile(bound)
var filenameOut = settings.filename.changeFileExt(".c")
let fOut = open(filenameOut, fmWrite)
fOut.writeLine("int main()")
fOut.write(result.join("\p"))

quit(QuitSuccess)
