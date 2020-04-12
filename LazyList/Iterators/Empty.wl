Begin["`Empty`Private`"]

DeclareIterator["Empty", <||>]

ImplementIterator["Empty", "Any", {
  "Next"[] :> Nothing,
  "SizeHint"[] :> 0,
  "Collect"[] :> {}
}]

ImplementIterator["Empty", "Peekable", {
  "Peek"[] :> Nothing
}]

ImplementIterator["Empty", "Bidirectional", {
  "Previous"[] :> Nothing
}]

ImplementIterator["Empty", "Copyable"]
ImplementIterator["Empty", "ExactSize"]

End[]
