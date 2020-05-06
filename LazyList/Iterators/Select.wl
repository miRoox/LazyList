Begin["`Select`Private`"]

DeclareIterator["Select"["Any"], <|
  "Iter"->Nothing,
  "Predicate"->(True&)
|>]

ImplementIterator["Select"["Any"], "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Predicate: " -> $IteratorData[["Predicate"]],
    "From: " -> DynamicIteratorItem@$IteratorData[["Iter"]]
  |>,
  "Dispose"[] :> $IteratorData[["Iter"]]@"Dispose"[]
}]

ImplementIterator["Select"["Forward"], "Forward", {
  "Next"[] :> $IteratorData[["Iter"]]@"FindNext"[$IteratorData[["Predicate"]]],
  "SizeHint"[] :> Interval@{0, Max[$IteratorData[["Iter"]]@"SizeHint"[]]}
}]

ImplementIterator["Select"["DoubleEnded"], "DoubleEnded", {
  "NextBack"[] :> $IteratorData[["Iter"]]@"FindNextBack"[$IteratorData[["Predicate"]]]
}]

ImplementIterator["Select"["Copyable"], "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, iter_, pred_]:=(
  data@"Iter"=iter;
  data@"Predicate"=pred;
)

End[]
