Begin["`Take`Private`"]

DeclareIterator["Take"["Any"], <|"Iter"->Nothing, "Count"->0|>]

ImplementIterator["Take"["Any"], "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Count: " -> DynamicIteratorItem@Dynamic[$IteratorData[["Count"]]],
    "From: " -> DynamicIteratorItem@$IteratorData[["Iter"]]
  |>,
  "Dispose"[] :> $IteratorData[["Iter"]]@"Dispose"[]
}]

ImplementIterator["Take"["Forward"], "Forward", {
  "Next"[] :> If[$IteratorData[["Count"]] > 0,
    --$IteratorData[["Count"]];
    $IteratorData[["Iter"]]@"Next"[],
    Nothing
  ],
  "SizeHint"[] :> Interval@{0, $IteratorData[["Count"]]}
}]

ImplementIterator["Take"["Peekable"], "Peekable", {
  "Peek"[] :> $IteratorData[["Iter"]]@"Peek"[]
}]

ImplementIterator["Take"["Copyable"], "Copyable"]

ImplementIterator["Take"["ExactSize"], "Forward", {
  "SizeHint"[] :> Min[$IteratorData[["Count"]], $IteratorData[["Iter"]]@"SizeHint"[]]
}]
ImplementIterator["Take"["ExactSize"], "ExactSize"]

SetAttributes[setup, HoldFirst];
setup[data_, iter_, n_]:=(
  data@"Iter"=iter;
  data@"Count"=n;
)
setup[_, args___]:=IteratorSetupArgumentsCheck["Take", Length@{args}, 2]

End[]
