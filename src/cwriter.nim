import strutils
import identifiers, lexer, evaluator_ops, binder

func toC*(self: ValueBase): string =
   case self.dtypeBase
   of tbool:
      if self.valBool: return "1" else:  return "0"
   of tint: return $self.valInt
   of tstr: return escape(self.valStr)
   else: raiseUnexpectedDtypeException($self.dtypeBase, "conversion to C")

func compile*(bound: Bound): seq[string] =
   case bound.kind
   of boundError: return @[]
   of boundRoot:
      result.add("int main()")
      result.add(bound.main.compile.join("\p"))
   of boundLiteral:
      result.add(bound.value.toC())
   of boundIdentifier:
      result.add(bound.identifier.name)
   of boundUnaryOperator:
      case bound.unaryOperator
      of boundUnaryPlus: result.add("+")
      of boundUnaryMinus: result.add("-")
      of boundUnaryNot: result.add("!")
      result.add(bound.unaryOperand.compile().join(" "))
   of boundBinaryOperator:
      result.add(bound.binaryLeft.compile().join(" "))
      case bound.binaryOperator
      of boundBinaryAddition: result.add("+")
      of boundBinarySubtraction: result.add("-")
      of boundBinaryMultiplication: result.add("*")
      of boundBinaryDivision: result.add("div")
      of boundBinaryEquals: result.add("==")
      of boundBinaryNotEquals: result.add("!=")
      of boundBinaryGreaterThan: result.add(">")
      of boundBinaryGreaterEquals: result.add(">=")
      of boundBinaryLessThan: result.add("<")
      of boundBinaryLessEquals: result.add("<=")
      of boundBinaryCombinedComparison:
         raise (ref ValueError)(msg: "Not yet implemented: <=>")
      of boundBinaryLogicalAnd: result.add("&&")
      of boundBinaryLogicalOr: result.add("||")
      of boundBinaryLogicalXor: result.add("^")
      result.add(bound.binaryRight.compile().join(" "))
   of boundAssignment:
      result.add(bound.lvalue.text)
      result.add("=")
      result.add(bound.rvalue.compile)
   of boundDefinition:
      raise (ref ValueError)(msg: "Not yet implemented: definition")
   of boundFunctionCall:
      raise (ref ValueError)(msg: "Not yet implemented: function call")
   of boundConditional:
      if bound.condition != nil:
         result.add("if (")
         result.add(bound.condition.compile())
         result.add(")")
      result.add(bound.conditional.compile())
      if bound.otherwise != nil:
         result.add("else")
         result.add(bound.otherwise.compile())
   of boundWhileLoop:
      raise (ref ValueError)(msg: "Not yet implemented: while loop")
   of boundBlock:
      result.add("{")
      var last = false
      for idx, expression in bound.blockExpressions:
         if idx == bound.blockExpressions.len()-1: last = true
         var exp: string
         if last: exp = "return "
         exp &= expression.compile.join(" ") & ";"
         result.add(exp)
      result.add("}")
