Begin["`Successors`Private`"]

DeclareIterator["Successors", <|
  "Succ"->Nothing,
  "Next"->Nothing
|>]

ImplementIterator["Successors", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Succ: " -> $IteratorData[["Succ"]],
    "Next: " -> DynamicIteratorItem@Dynamic@If[$IteratorData[["Next"]]===Nothing,
      Missing["Exhausted"],
      $IteratorData[["Next"]]
    ]
  |>
}]

ImplementIterator["Successors", "Forward", {
  "Next"[] :> Block[{item=$IteratorData[["Next"]]},
    If[item===Nothing,
      Nothing,
      $IteratorData[["Next"]]=$IteratorData[["Succ"]][item];
      item
    ]
  ],
  "SizeHint"[] :> If[$IteratorData[["Next"]]===Nothing, 0, Interval[{1,Infinity}]]
}]

ImplementIterator["Successors", "Peekable", {
  "Peek"[] :> $IteratorData[["Next"]]
}]

ImplementIterator["Successors", "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, succ_, init_]:=(
  data@"Succ"=succ;
  data@"Next"=init;
)
setup[_, args___]:=IteratorSetupArgumentsCheck["Successors", Length@{args}, 2]

End[]
