Begin["`Once`Private`"]

DeclareIterator["Once", <|"Element"->Nothing|>]

ImplementIterator["Once", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Element: " -> Dynamic@If[$IteratorData@"Element"===Nothing,
      Missing["Exhausted"],
      $IteratorData@"Element"
    ]
  |>
}]

ImplementIterator["Once", "Forward", {
  "Next"[] :> move[$IteratorData@"Element"],
  "SizeHint"[] :> If[$IteratorData@"Element"===Nothing, 0, 1]
}]

ImplementIterator["Empty", "Peekable", {
  "Peek"[] :> $IteratorData@"Element"
}]

ImplementIterator["Empty", "Copyable"]
ImplementIterator["Empty", "ExactSize"]

SetAttributes[move, HoldFirst];
move[val_]:=Block[{tmp=val},
  val=Nothing;
  tmp
]

SetAttributes[setup, HoldFirst];
setup[data_, val_]:=data@"Element"=val
setup[_, args___]:=IteratorSetupArgumentsCheck["Once", Length@{args}, 1]

End[]
