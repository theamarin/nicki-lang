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

var context = newContext()

var p = initOptParser(commandLineParams())
for kind, key, val in p.getopt():
   case kind
   of cmdArgument: context.sourceText.filename = key
   of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
         echo helpStr
         quit(QuitSuccess)
      of "show-parse-tree": context.settings.showParseTree = true
      of "show-bound-tree": context.settings.showBoundTree = true
      of "show-vars": context.settings.showVars = true
      else:
         echo "Error: Unknown option " & escape(key)
         quit(QuitFailure)
   of cmdEnd: assert(false) # cannot happen

if context.sourceText.filename == "":
   echo "Error: Missing filename"
   quit(QuitFailure)

let f = open(context.sourceText.filename, fmRead)
let data = f.readAll()
f.close()

let bound = context.analyze(data)
if bound.kind == boundError:
   quit(QuitFailure)

let result = compile(bound)
var filenameOut = context.sourceText.filename.changeFileExt(".c")
let fOut = open(filenameOut, fmWrite)
fOut.writeLine("int main()")
fOut.write(result.join("\p"))

quit(QuitSuccess)
