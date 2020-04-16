#!/usr/bin/env wolframscript
(* ::Package:: *)

$pacletDir=FileNameJoin@{DirectoryName[$InputFileName], "LazyList"}

If[$VersionNumber>=12.1,
  System`CreatePacletArchive[$pacletDir],
  PacletManager`PackPaclet[$pacletDir]
]
