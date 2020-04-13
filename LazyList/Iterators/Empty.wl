Begin["`Empty`Private`"]

DeclareIterator["Empty", <||>]

ImplementIterator["Empty", "Any"]

ImplementIterator["Empty", "Forward", {
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
