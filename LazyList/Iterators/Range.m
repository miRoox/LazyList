Begin["`Range`Private`"]

DeclareIterator["Range", <|
  "Start"->1,
  "Stop"->Infinity,
  "Step"->1
|>]

ImplementIterator["Range", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "From: " -> DynamicIteratorItem@Dynamic[$IteratorSelf[["Start"]]],
    "To: " -> DynamicIteratorItem@Dynamic[$IteratorSelf[["Stop"]]],
    "Step: " -> $IteratorSelf[["Step"]]
  |>
}]

ImplementIterator["Range", "Forward", {
  "Next"[] :> Block[{item=$IteratorSelf[["Start"]]},
    If[item <= $IteratorSelf[["Stop"]],
      $IteratorSelf[["Start"]] += $IteratorSelf[["Step"]];
      item,
      Nothing
    ]
  ],
  "SizeHint"[] :> Quotient[$IteratorSelf[["Stop"]]-$IteratorSelf[["Start"]], $IteratorSelf[["Step"]]]+1,
  "Collect"[] :> Range[$IteratorSelf[["Start"]], $IteratorSelf[["Stop"]], $IteratorSelf[["Step"]]]
}]

ImplementIterator["Range", "Peekable", {
  "Peek"[] :> If[$IteratorSelf[["Start"]] <= $IteratorSelf[["Stop"]],
    $IteratorSelf[["Start"]],
    Nothing
  ]
}]

ImplementIterator["Range", "DoubleEnded", {
  "NextBack"[] :> Block[{item=$IteratorSelf[["Stop"]]},
    If[$IteratorSelf[["Start"]] <= item,
      $IteratorSelf[["Stop"]] -= $IteratorSelf[["Step"]];
      item,
      Nothing
    ]
  ]
}]

ImplementIterator["Range", "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, start_|PatternSequence[], stop_|PatternSequence[], step_|PatternSequence[]]:=ResourceFunction["WithMessageHandler"][
  isetup[data, LazyRange[start, stop, step]],
  Throw
]
setup[_, args__]:=IteratorSetupArgumentsCheck["Range", Length@{args}, {0, 3}]
SetAttributes[isetup, HoldFirst];
isetup[data_, HoldPattern@LazyRange[start_, stop_, step_]]:=(
  data[["Start"]]=start;
  data[["Stop"]]=stop;
  data[["Step"]]=step;
)

End[]
