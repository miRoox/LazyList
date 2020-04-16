Begin["`Take`Private`"]

DeclareIterator["Take"["Any"], <|"Delegate"->Nothing, "Count"->0|>]

ImplementIterator["Take"["Any"], "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Count: " -> DynamicIteratorItem@Dynamic[$IteratorData[["Count"]]]
  |>
}]

ImplementIterator["Take"["Forward"], "Forward", {
  "Next"[] :> If[$IteratorData[["Count"]] > 0,
    --$IteratorData[["Count"]];
    $IteratorData[["Delegate"]]@"Next"[],
    Nothing
  ],
  "SizeHint"[] :> Interval@{0, $IteratorData[["Count"]]},
  "Take"[n_Integer?NonNegative] :> Block[
    {new=CreateIterator[$IteratorType, $IteratorData[["Delegate"]], Min[$IteratorData[["Count"]], n]]},
    $IteratorSelf@"Dispose"[];
    new
  ]
}]

ImplementIterator["Take"["Peekable"], "Peekable", {
  "Peek"[] :> $IteratorData[["Delegate"]]@"Peek"[]
}]

ImplementIterator["Take"["Copyable"], "Copyable"]

ImplementIterator["Take"["ExactSize"], "Forward", {
  "SizeHint"[] :> Min[$IteratorData[["Count"]], $IteratorData[["Delegate"]]@"SizeHint"[]]
}]
ImplementIterator["Take"["ExactSize"], "ExactSize"]

SetAttributes[setup, HoldFirst];
setup[data_, delegate_, n_]:=(
  data@"Delegate"=delegate;
  data@"Count"=n;
)
setup[_, args___]:=IteratorSetupArgumentsCheck["Take", Length@{args}, 2]

End[]
