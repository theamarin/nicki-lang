import strutils, strformat, hashes, tables
import binder, identifiers, diagnostics

type
   Edge = ref object
      `from`, to: Block
      condition: Bound

   Block = ref object
      bounds: seq[Bound]
      edges: seq[Edge]
      visited: bool

   ControlFlowGraph = seq[Block]


# Control flow
# -------------
# Cannot split in sparate file because Nim is broken

func hash*(self: Block): Hash = return cast[pointer](self).hash

func connect(`from`, `to`: Block, condition: Bound = nil) =
   var prunedCondition = condition
   if not condition.isNil and condition.kind == boundLiteral:
      if condition.value.valBool == true: prunedCondition = nil
      else: return
   let edge = Edge(`from`: `from`, to: to, condition: prunedCondition)
   `from`.edges.add(edge)

func negate(condition: Bound): Bound =
   if condition.kind == boundLiteral:
      let newValue = Value(base: tbool, valBool: not condition.value.valBool)
      result = Bound(kind: boundLiteral, value: newValue)
      result.inherit(condition)
   else:
      result = Bound(kind: boundUnaryOperator, unaryOperator: boundUnaryNot,
            unaryOperand: condition)
      result.inherit(condition)


func finalizeBlock(blocks: var ControlFlowGraph, current: var Block) =
   if current.bounds.len > 0:
      blocks.add(current)
      current = Block()

func createBlocks(bound: Bound): ControlFlowGraph =
   assert bound.kind == boundBlock
   result.add(Block()) # start block
   var currentBlock = Block()
   for expression in bound.blockExpressions:
      case expression.kind
      of boundLabel:
         result.finalizeBlock(currentBlock)
         currentBlock.bounds.add(expression)
      of boundGoto, boundConditionalGoto, boundReturn:
         currentBlock.bounds.add(expression)
         result.finalizeBlock(currentBlock)
      else:
         currentBlock.bounds.add(expression)
   result.finalizeBlock(currentBlock)
   result.add(Block()) # end block

func createEdges(blocks: ControlFlowGraph) =
   let endBlock = blocks[^1]
   var labelToBlock = newTable[BoundLabel, Block]()
   for current in blocks:
      for expression in current.bounds:
         if expression.kind == boundLabel:
            labelToBlock[expression.label] = current

   for currentIdx, current in blocks:
      let nextBlock = if currentIdx + 1 < blocks.len: blocks[
            currentIdx+1] else: endBlock
      for exprIdx, expression in current.bounds:
         case expression.kind
         of boundGoto:
            let toBlock = labelToBlock[expression.label]
            current.connect(toBlock)
         of boundConditionalGoto:
            let thenBlock = labelToBlock[expression.gotoLabel]
            let elseBlock = nextBlock
            let negatedCondition = expression.gotoCondition.negate()
            let thenCondition = if expression.gotoIfTrue: expression.gotoCondition else: negatedCondition
            let elseCondition = if expression.gotoIfTrue: negatedCondition else: expression.gotoCondition
            current.connect(thenBlock, thenCondition)
            current.connect(elseBlock, elseCondition)
         of boundReturn:
            current.connect(endBlock)
         else:
            if exprIdx == current.bounds.len - 1:
               current.connect(nextBlock)
      if current.bounds.len == 0 and current != endBlock:
         current.connect(nextBlock)

func createGraph*(bound: Bound): ControlFlowGraph =
   result = createBlocks(bound)
   result.createEdges()

func allPathsReturnValue*(blocks: ControlFlowGraph): bool =
   let endBlock = blocks[^1]

   # Collect edges to endBlock
   var edges: seq[Edge]
   for b in blocks:
      if b.visited:
         for edge in b.edges:
            if edge.`to` == endBlock: edges.add(edge)

   for edge in edges:
      let srcBlock = edge.`from`
      if srcBlock.bounds.len == 0:
         return false
      let lastExpression = srcBlock.bounds[^1]
      if lastExpression.kind != boundReturn:
         return false
   return true

func visitBlocks(b: Block) =
   if b.visited: return
   b.visited = true
   for edge in b.edges: visitBlocks(edge.`to`)

func collectUnreachable*(blocks: ControlFlowGraph): seq[Bound] =
   for b in blocks:
      if not b.visited:
         for b in b.bounds:
            if b.kind notin {boundLabel, boundGoto}:
               result.add(b)
               break

func toStr(b: Block, i: int = -1): string =
   var strs: seq[string]
   # if i >= 0: strs.add("block " & $i)
   if i == 0: strs.add("<start>")
   elif b.bounds.len == 0: strs.add("<end>")
   for bound in b.bounds:
      strs.add(bound.asCode)
   return strs.join("\\n")

proc writeTo*(blocks: ControlFlowGraph, filename: string) =
   let f = open(filename, fmWrite)
   f.writeLine("digraph G {");
   var blockIds: Table[Block, string]
   var edges: seq[Edge]
   for i, b in blocks:
      let id = &"N{i}"
      blockIds[b] = id
      for edge in b.edges:
         edges.add(edge)
   for i, b in blocks:
      var id = blockIds[b];
      var label = "" & escape(b.toStr(i)).replace("\\\\n", "\\n")
      f.writeLine(&"    {id} [label = {label}, shape = box]");
   for edge in edges:
      let fromId = blockIds[edge.`from`]
      let toId = blockIds[edge.`to`]
      let label = if edge.condition.isNil: "" else: $edge.condition.asCode
      f.writeLine(&"    {fromId} -> {toId} [label = {label.escape}]");
   f.writeLine("}");
   f.close()

proc checkControlFlow(node: Bound) =
   let controlFlow = createGraph(node.defInitialization)
   visitBlocks(controlFlow[0])

   if not controlFlow[^1].visited:
      node.binder.diagnostics.reportCannotReturn(node.defIdentifier.pos)

   if node.defDtype.composed.retDtype.base != tvoid:
      if not controlFlow.allPathsReturnValue():
         node.binder.diagnostics.reportNotAllPathsReturnValue(node.defIdentifier.pos)

   if false:
      let unreachableBounds = controlFlow.collectUnreachable()
      for unreachable in unreachableBounds:
         node.binder.diagnostics.reportUnreachableCode(unreachable.pos)

   controlFlow.writeTo("graphs/" & node.defIdentifier.name & ".dot")

proc checkControlFlows*(node: Bound) =
   case node.kind
   of boundDefinition:
      if node.defDtype.isComposedType(tfunc):
         node.checkControlFlow()
   of boundBlock:
      for e in node.blockExpressions: e.checkControlFlows()
   of boundRoot:
      node.main.checkControlFlows()
   else: return
