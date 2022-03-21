(* ::Package:: *)

(* ::Section:: *)
(*Assemble the paclet, and build a new .paclet file*)


date = DateString[{"Year", "Month", "Day"}];
time = DateString[{"Hour24", "Minute", "Second"}];


$scriptsDirectory = Which[
	Environment["WORKSPACE"] =!= $Failed,
		FileNameJoin[{Environment["WORKSPACE"],"Scripts"}],
	$InputFileName =!= "",
		DirectoryName[$InputFileName],
	True,
		NotebookDirectory[]
];

$buildDirectory = ToFileName[{ParentDirectory[$scriptsDirectory], "Build"}];

$source = ToFileName[{ParentDirectory[$scriptsDirectory], "BioFormatsLink"}];
$pacletinfo = FileNameJoin[{$source, "PacletInfo.wl"}];
$java = ToFileName[{ParentDirectory[$scriptsDirectory], "Java"}];
$assembled = ToFileName[{$buildDirectory, date <> "-" <> time, "BioFormatsLink"}];

CreateDirectory[$assembled, CreateIntermediateDirectories -> True];

$sourceFolderSet = {"Kernel"};

$builtDocs = FileNameJoin[{
	$buildDirectory,
	"BioFormatsLink",
	"Documentation"
}
];
Print[$assembled]
Print[$pacletinfo]
CopyDirectory[$builtDocs, FileNameJoin[{$assembled, "Documentation"}]]
CopyDirectory[ToFileName[{$source, #}], ToFileName[{$assembled, #}]]& /@ $sourceFolderSet;
CopyDirectory[$java,  ToFileName[{$assembled, "Java"}]];
CopyFile[$pacletinfo, FileNameJoin[{$assembled, "PacletInfo.wl"}]]

(* get rid of any .DS* files or other hidden files *)
DeleteFile /@ FileNames[".*", $assembled, Infinity];


CreatePacletArchive[$assembled]
