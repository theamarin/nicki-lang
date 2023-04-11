import strutils, parseopt, os
import lang/[analysis, evaluator, identifiers]

const helpStr = dedent """
   nicki-lang REPL
   Usage: nii [OPTIONS] [filename]
   Options:
      -h, --help           Show this help and exit
      --show-parse-tree    Show parse tree
      --show-bound-tree    Show bound tree
      --show-lowered-tree  Show lowered tree
      --show-vars          Show global-scope identifiers"""

var settings: AnalysisSettings
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
      of "show-lowered-tree": settings.showLoweredTree = true
      of "show-vars": settings.showVars = true
      else:
         echo "Error: Unknown option " & escape(key)
         quit(QuitFailure)
   of cmdEnd: assert(false) # cannot happen


const prompt = "> "

while true:
   if settings.showVars: context.showVars()

   write(stdout, prompt)
   var line: string
   discard readLine(stdin, line)
   case line
   of "": echo "Quit"; break
   of "#parseTree":
      settings.showParseTree = not settings.showParseTree
      echo("showParseTree: " & $settings.showParseTree)
      continue
   of "#boundTree":
      settings.showBoundTree = not settings.showBoundTree
      echo("showBoundTree: " & $settings.showBoundTree)
      continue
   of "#loweredTree":
      settings.showLoweredTree = not settings.showLoweredTree
      echo("showLoweredTree: " & $settings.showLoweredTree)
      continue
   of "#vars":
      settings.showVars = not settings.showVars
      echo("settings.showVars: " & $settings.showVars)
      continue

   let bound = context.analyze(line)

   let result = evaluate(context.evaluator, bound)
   if result.dtype.base notin {tvoid, terror}: writeline(stdout, $result)
