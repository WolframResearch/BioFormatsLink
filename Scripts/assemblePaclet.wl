(* ::Package:: *)

(* ::Section:: *)
(*Assemble the paclet, and build a new .paclet file*)


date = DateString[{"Year", "Month", "Day"}];
time = DateString[{"Hour24", "Minute", "Second"}];


$scriptsDirectory = Which[
	Environment["WORKSPACE"] =!= $Failed,
		FileNameJoin[{Environment["WORKSPACE"],"scripts"}],
	$InputFileName =!= "",
		DirectoryName[$InputFileName],
	True,
		NotebookDirectory[]
];

$buildDirectory = ToFileName[{ParentDirectory[$scriptsDirectory], "build"}];

$source = ToFileName[{ParentDirectory[$scriptsDirectory], "BioFormatsLink"}];
$pacletinfo = FileNameJoin[{$source, "PacletInfo.m"}];
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
CopyFile[$pacletinfo, FileNameJoin[{$assembled, "PacletInfo.m"}]]

(* get rid of any .DS* files or other hidden files *)
DeleteFile /@ FileNames[".*", $assembled, Infinity];


PackPaclet[$assembled]


(* ::Section:: *)
(*notes*)


(*
Re version numbering:

The code above which builds the .paclet file starts from a PacletInfo.m file,
which currently has the version number set to '2100.0'.  This should be changed
by the developers to whatever version number is appropriate.

Due to the nature of the build system, the RE script will replace the version 
number with one including the build number.  This prevents issues with similar
version-ed files confusing the build system.  This change occurs in
re_build_BioFormatsLink.xml, in the 'Paclet.BioFormatsLink.prebuild' target.
*)
