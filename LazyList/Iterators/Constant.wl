Begin["`Constant`Private`"]

DeclareIterator["Constant", <|"Element"->Nothing|>]

ImplementIterator["Constant", "Any", {
  "Next"[] :> $IteratorData@"Element",
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Element: " -> $IteratorData@"Element"
  |>,
  "Setup"[args___] :> setup[$IteratorData, args],
  "SizeHint"[] :> Infinity
}]
ImplementIterator["Constant", "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, c_]:=data@"Element"=c
setup[_, args___]:=IteratorSetupArgumentsCheck["Constant", Length@{args}, 1]

End[]
