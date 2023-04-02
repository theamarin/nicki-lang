import tables, strformat, strutils, hashes
import binder, lexer, identifiers

type
   Edge = ref object
      `from`, to: Block
      condition: Bound

   Block = ref object
      bounds: seq[Bound]
      edges: seq[Edge]

   ControlFlowGraph = seq[Block]


func hash*(self: Block): Hash = return cast[pointer](self).hash

func connect(`from`, `to`: Block, condition: Bound = nil) =
   let edge = Edge(`from`: `from`, to: to, condition: condition)
   `from`.edges.add(edge)

func negate(condition: Bound): Bound =
   if condition.kind == boundLiteral:
      let newValue = ValueBase(dtypeBase: tbool, valBool: not condition.value.valBool)
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
      let nextBlock = if currentIdx + 1 < blocks.len: blocks[currentIdx+1] else: endBlock
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

func toStr(b: Block, i: int = -1): string =
   var strs: seq[string]
   if i >= 0: strs.add("block " & $i)
   for bound in b.bounds:
      strs.add($bound.kind)
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
      var label = "\"" & b.toStr(i) & "\""
      f.writeLine(&"    {id} [label = {label}, shape = box]");
   for edge in edges:
      let fromId = blockIds[edge.`from`]
      let toId = blockIds[edge.`to`]
      let label = if edge.condition.isNil: "" else: $edge.condition.kind
      f.writeLine(&"    {fromId} -> {toId} [label = {label.escape}]");
   f.writeLine("}");
   f.close()
