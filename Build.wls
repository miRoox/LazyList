#!/usr/bin/env wolframscript
(* ::Package:: *)

$pacletDir=FileNameJoin@{DirectoryName[$InputFileName], "LazyList"}

paclet=If[$VersionNumber>=12.1,
  System`CreatePacletArchive[$pacletDir],
  PacletManager`PackPaclet[$pacletDir]
];

If[FailureQ@paclet,
  Exit[1],
  Print["::set-output name=path::", paclet];
  Print["::set-output name=name::", FileNameTake@paclet];
]

Exit[]
