import unittest
import ../src/lang/[analysis, evaluator, identifiers]

proc run(filename: string): bool =
   let f = open(filename, fmRead)
   let data = f.readAll()
   f.close()

   var context = newContext()
   let bound = context.analyze(data)
   if bound.kind == boundError: return false

   let evalResult = evaluate(context.evaluator, bound)
   if evalResult.base == terror: return false

   return true


suite "Examples":
   test "Call":
      doAssert run("examples/call.nic")

   test "loop":
      doAssert run("examples/loop.nic")

   test "nofunc":
      doAssert run("examples/nofunc.nic")

   test "shadow":
      doAssert run("examples/shadow.nic")

   test "simple":
      doAssert run("examples/simple.nic")

   test "sum":
      doAssert run("examples/sum.nic")

   test "whiletrue":
      doAssert run("examples/whiletrue.nic")
