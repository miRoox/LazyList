(* Mathematica Package *)

BeginPackage["LazyList`Iterators`", {"LazyList`", "LazyList`Developer`"}];

Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Take.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "TakeWhile.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Select.wl"};

Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Empty.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Constant.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Once.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Range.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "List.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Stream.wl"};
Get@FileNameJoin@{DirectoryName@$InputFileName, "Iterators", "Successors.wl"};

EndPackage[]