Begin["`Constant`Private`"]

DeclareIterator["Constant", <|"Element"->Nothing|>]

ImplementIterator["Constant", "Any", {
  "Next"[] :> $IteratorData@"Element",
  "Setup"[args___] :> constantSetup[$IteratorData, args],
  "SizeHint"[] :> Infinity
}]
ImplementIterator["Constant", "Copyable"]

SetAttributes[constantSetup, HoldFirst];
constantSetup[data_, c_]:=data@"Element"=c
constantSetup[_, args___]:=IteratorSetupArgumentsCheck["Constant", Length@{args}, 1]

End[]
