import strutils
import diagnostics

type
   Dtype* = enum
      terror = "[error]"
      tvoid = "void"
      tbool = "bool"
      tint = "int"
      tstring = "string"

   Value* = object
      case dtype*: Dtype
      of terror: discard
      of tvoid: discard
      of tbool: valBool*: bool
      of tint: valInt*: int
      of tstring: valString*: string

   Parameter* = object
      name*: string
      dtype*: Dtype
      pos*: Position

   IdentifierKind* = enum
      dtypeIdentifier,
      variableIdentifier,
      functionIdentifier

   Identifier* = ref object
      name*: string
      dtype*: Dtype
      pos*: Position
      case kind*: IdentifierKind
      of functionIdentifier:
         retDtype*: Dtype
         parameters*: seq[Parameter]
      else: discard

func toDtype*(dtype: string): Dtype =
   try:
      return parseEnum[Dtype](dtype)
   except ValueError:
      return terror

func `$`*(val: Value): string =
   case val.dtype
   of terror: return "[error]"
   of tvoid: return "[void]"
   of tbool: return $val.valBool
   of tint: return $val.valInt
   of tstring: return $val.valString

func `$`*(parameter: Parameter): string =
   return parameter.name & ": " & $parameter.dtype

func `$`*(identifier: Identifier): string =
   case identifier.kind
   of variableIdentifier: return identifier.name & ": " & $identifier.dtype
   of functionIdentifier:
      result = identifier.name & "("
      result &= $(identifier.parameters).join(", ")
      result &= "): " & $identifier.retDtype
   of dtypeIdentifier: discard

func newDtypeIdentifier*(name: string, pos: Position): Identifier =
   return Identifier(kind: dtypeIdentifier, pos: pos)

func newVariableIdentifier*(name: string, dtype: Dtype, pos: Position): Identifier =
   return Identifier(kind: variableIdentifier, name: name, dtype: dtype, pos: pos)

func newFunctionIdentifier*(name: string, retDtype: Dtype, parameters: seq[Parameter],
      pos: Position): Identifier =
   return Identifier(kind: functionIdentifier, name: name, dtype: tvoid, retDtype: retDtype,
         parameters: parameters, pos: pos)
