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
      of "show-lowered-tree": context.settings.showLoweredTree = true
      of "show-vars": context.settings.showVars = true
      else:
         echo "Error: Unknown option " & escape(key)
         quit(QuitFailure)
   of cmdEnd: assert(false) # cannot happen



if context.sourceText.filename.len > 0:
   let f = open(context.sourceText.filename, fmRead)
   let data = f.readAll()
   f.close()

   let bound = context.analyze(data)
   if bound.kind == boundError:
      quit(QuitFailure)

   let result = evaluate(context.evaluator, bound)
   if result.base notin {tvoid, terror}: writeline(stdout, $result)

   quit(QuitSuccess)


const prompt = "> "

while true:
   if context.settings.showVars: context.showVars()

   write(stdout, prompt)
   var line: string
   discard readLine(stdin, line)
   case line
   of "": echo "Quit"; break
   of "#parseTree":
      context.settings.showParseTree = not context.settings.showParseTree
      echo("showParseTree: " & $context.settings.showParseTree)
      continue
   of "#boundTree":
      context.settings.showBoundTree = not context.settings.showBoundTree
      echo("showBoundTree: " & $context.settings.showBoundTree)
      continue
   of "#loweredTree":
      context.settings.showLoweredTree = not context.settings.showLoweredTree
      echo("showLoweredTree: " & $context.settings.showLoweredTree)
      continue
   of "#vars":
      context.settings.showVars = not context.settings.showVars
      echo("context.settings.showVars: " & $context.settings.showVars)
      continue

   let bound = context.analyze(line)
   if bound.kind == boundError: continue

   let result = evaluate(context.evaluator, bound)
   if result.base notin {tvoid, terror}: writeline(stdout, $result)
