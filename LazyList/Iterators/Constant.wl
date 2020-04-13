Begin["`Constant`Private`"]

DeclareIterator["Constant", <|"Element"->Nothing|>]

ImplementIterator["Constant", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Element: " -> $IteratorData@"Element"
  |>
}]

ImplementIterator["Constant", "Forward", {
  "Next"[] :> $IteratorData@"Element",
  "SizeHint"[] :> Infinity
}]

ImplementIterator["Constant", "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, c_]:=data@"Element"=c
setup[_, args___]:=IteratorSetupArgumentsCheck["Constant", Length@{args}, 1]

End[]
