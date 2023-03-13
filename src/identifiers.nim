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
      name: string
      dtype: Dtype

   IdentifierKind* = enum
      dtypeIdentifier,
      variableIdentifier,
      functionIdentifier

   Identifier* = ref object
      name*: string
      declarationPos*: Position
      case kind*: IdentifierKind
      of variableIdentifier:
         dtype*: Dtype
      of functionIdentifier:
         retDtype*: Dtype
         parameters*: seq[Parameter]
      else: discard

func toDtype*(dtype: string): Dtype =
   return parseEnum[Dtype](dtype)

func `$`*(parameter: Parameter): string =
   return parameter.name & ": " & $parameter.dtype

func `$`*(identifier: Identifier): string =
   case identifier.kind
   of variableIdentifier: return identifier.name & ": " & $identifier.dtype
   of functionIdentifier:
      result = identifier.name & "("
      result &= $(identifier.parameters).join(", ")
      result = "): " & $identifier.retDtype
   of dtypeIdentifier: discard

func newDtypeIdentifier*(name: string, pos: Position): Identifier =
   return Identifier(kind: dtypeIdentifier, declarationPos: pos)

func newVariableIdentifier*(name: string, dtype: Dtype, pos: Position): Identifier =
   return Identifier(kind: variableIdentifier, name: name, dtype: dtype,
         declarationPos: pos)

func newFunctionIdentifier*(name: string, retDtype: Dtype, pos: Position): Identifier =
   return Identifier(kind: functionIdentifier, name: name, retDtype: retDtype,
         declarationPos: pos)
