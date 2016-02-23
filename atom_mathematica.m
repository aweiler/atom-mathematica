(* ::Package:: *)

(*************************)
(*    ATOM MATHEMATICA ROUTINES       *)
(*    v1.0                            *)
(*    author: andreas.weiler@cern.ch *)


(*

1) should be able to parse all the available information in the JSON (produced from Yaml via the yaml2json tool)
2) should be able to read yoda histogram files (done for HISTO1D)
3) should be user-friendly (done)

*)


(*
List of changes:

  02/11/15: AW: further cut methods
  01/19/15: AW: first commit
  01/21/15: AW: added yoda HIST1D plotting

TODO: 

  - check if value exists before printing, otherwise show nothing
  - import the rest of the (legacy) matehamtica atom package
  - underflow/overflow HIST1D example needed
  - get more yoda histogramming examples

*)

(*************************)



OV := OptionValue; (*save space*)

(* all the contents of an analysis*)
PickAnalysis[json_, anaName_] := OV[OV[{json}, "Analyses"], anaName];

GetVal[json_, entrName_] := OV[OV[json, entrName], "Value"];
GetVal[json_, entrName_, valName_] := 
 OV[OV[json, valName], valName]; (*e.g. for "Cut Value"*)

PrintValue[json_, valName_] := 
 Print[ Style[valName <> " : " , Blue], 
  ToString[OV[ana, valName]]]; (* for simple Value/Error pairs *)

(*************************)
(*       PRINT OUTPUT    *)
(*************************)


ShowAnalysisInfo[json_, anaName_] :=
 (
  ana = PickAnalysis[json, anaName];
  Print[ Style[anaName, Bold,Blue]];
  PrintValue[ana, "Summary"];
  Print[ Style["Int. luminosity : ", Blue]  , 
   ToString[GetVal[ana, "Luminosity"]/1000.] <> " 1/fb "];
  PrintValue[ana, "References"];
  );



ShowFullAnalysisInfo[json_, anaName_] := 
  (
   ana = PickAnalysis[json, anaName];
   Print[ Style[anaName, Bold, Blue]];
   PrintValue[ana, "Summary"];
   PrintValue[ana, "Experiment"];
   PrintValue[ana, "Year"];
   PrintValue[ana, "Description"];
   PrintValue[ana, "RunInfo"];
   PrintValue[ana, "Status"];
   PrintValue[ana, "Authors"];
   PrintValue[ana, "References"];
   PrintValue[ana, "SpiresID"];
   PrintValue[ana, "InspireID"];
   
   PrintValue[ana, "BibKey"];
   Print[ Style["Int. luminosity : ", Blue]  , 
    ToString[GetVal[ana, "Luminosity"]/1000.] <> " \[PlusMinus] " <>  
     ToString[OV[OV[ana, "Luminosity"], "Error"]] <> " 1/fb "];
   PrintValue[ana, "References"];
   
   );
(*************************)
(*   EXTRACT EFFIENCIES   *)
(*************************)


ListEfficiencies[json_, anaName_, ShowSubprocesses] := (
   ana = PickAnalysis[json, anaName];
   {OV[#, "Efficiency Name"], OV[#, "Description"], 
      OV[#, "IsControlRegion"], ({OV[#, "Sub-process ID"], 
          OV[#, "Efficiency Value"], 
          OV[#, "Efficiency Stat Error"]} & /@ OV[#, "Data"])} &  /@ 
    OV[ana, "Efficiencies"]
   
   (*{OV[#,"Name"],OV[#,"Value"],OV[OV[#,"Error"],"Stat"] ,OV[#,
   "Sub-process ID"] }& /@{OV[#,"Efficiency Name"],OV[#,
   "Description"],OV[OV[#,"Error"],"Stat"] ,OV[#,
   "Sub-process ID"] }& *)
   );

ShowEfficiencies[json_, anaName_] := (
   Print[Style["Efficiencies for " <> anaName, Blue, Bold]];
   Join[{{"Efficiency Name", "Description", "IsControlRegion", 
       "Efficiency Value", "Stat Error -", "Stat Error +"}}, 
     ListEfficiencies[json, anaName]] // Grid
   );

GetTotEfficiency[
   jsonEffiency_] := (Select[({OV[#, "Sub-process ID"], 
         OV[#, "Efficiency Value"], 
         OV[#, "Efficiency Stat Error"]} & /@ 
       OV[jsonEffiency, "Data"]), #[[1]] == 0 &][[1, 2]]);

GetTotStatError[
   jsonEffiency_] := (Select[({OV[#, "Sub-process ID"], 
         OV[#, "Efficiency Value"], 
         OV[#, "Efficiency Stat Error"]} & /@ 
       OV[jsonEffiency, "Data"]), #[[1]] == 0 &][[1, 3]]);

ListEfficiencies[json_, anaName_] := (
   ana = PickAnalysis[json, anaName];
   temp = {OV[#, "Efficiency Name"], OV[#, "Description"], 
       OV[#, "IsControlRegion"], GetTotEfficiency[#], 
       GetTotStatError[#][[1]], GetTotStatError[#][[2]]} &  /@ 
     OV[ana, "Efficiencies"]
   );

GetEfficiency[json_, anaName_, effName_] := (
   ana = PickAnalysis[json, anaName];
   Select[{OV[#, "Efficiency Name"], OV[#, "Description"], 
        OV[#, "IsControlRegion"], GetTotEfficiency[#], 
        GetTotStatError[#][[1]], GetTotStatError[#][[2]]} &  /@ 
      OV[ana, "Efficiencies"], #[[1]] == effName &][[1]]
   );

GetEfficiencyValue[json_, anaName_, effName_] := (
   GetEfficiency[json, anaName, effName][[4]]
   );

GetEfficiencyStatError[json_, anaName_, effName_] := (
   GetEfficiency[json, anaName, effName][[5 ;; 6]]
   );

(*************************)
(*   EXTRACT CUTS   *)
(*************************)


GetSubProc0[jsonSub_] := 
 Select[jsonSub, #[[1]] == 
     0 &][[1]]; (* assumes that procID is first entry! *)

ListCuts[json_, anaName_, ShowSubprocesses] := (
  ana = PickAnalysis[json, anaName];
  temp = {
      OV[#, "Cut Name"],
      OV[#, "Description"],
      OV[#, "Parent Cut"], 
      ({OV[#, "Sub-process ID"],
          OV[#, "Cut Value"],
          OV[#, "Cut Value Flags"],
          OV[#, "Cut Log. Derivative"],
          OV[#, "Cut Log. Der. Stat Error"],
          OV[#, "Cut Log. Der. Flags"]
          } &
        /@ OV[#, "Data"])} &  
    /@ OV[ana, "Cuts"]
  
  );

ListCuts[json_, anaName_] := (
   {#[[1]], #[[2]], #[[3]], Select[#[[4]], #[[1]] == 0 &]} & /@ 
    ListCuts[json, anaName, ShowSubprocesses]
   );


ShowCuts[json_, anaName_] := (
   Print[Style[ " Cutflow for " <> anaName, Bold, Blue]]
   (* todo *)
   );


GetCut[json_, anaName_, cutName_, ShowSubprocesses] := (
   Select[
     ListCuts[json, anaName, 
      ShowSubprocesses], #[[1]] == cutName &][[1]]
   );

GetCut[json_, anaName_, cutName_] := (
   Select[ListCuts[json, anaName], #[[1]] == cutName &][[1]]
   );

GetCutValue[json_, anaName_, effName_] := (
   GetCut[json, anaName, effName][[4, 1, 2]]
   );

GetCutStatError[json_, anaName_, effName_] := (
   GetCut[json, anaName, effName][[4, 1, 5 ;; 6]][[1]]
   );

(*************************)
(*   Cross Sections   *)
(*************************)

ListCrossSection[json_, ShowSubprocesses] := 
  (
   {OV[#, "Process ID"], OV[#, "Cross Section"], 
      OV[#, "Cross Section Error"][[1]], 
      OV[#, "Cross Section Error"][[2]]} & /@ 
    OV[json, "Cross Sections"]
   );

GetTotalCrossSection[json_] :=
  (
   (Select[
      ListCrossSection[json, ShowSubprocesses], #[[1]] == 0 &])[[1, 
     2 ;; 4]]
   );

ShowCrossSection[json_, ShowSubprocesses] := 
  (
   Print[Style["Cross Sections in fb", Blue, Bold] , 
    " (Process ID = 0 is total xsec)" ];
   Join[{{"Process ID", "Cross Section", "Cross Section Error-", 
       "Cross Section Error+"}}, 
     ListCrossSection[json, ShowSubprocesses]] // Grid
   );



(*************************)
(*   YODA histogram plotting   *)
(*************************)

(*

from: https://yoda.hepforge.org/trac/wiki/DataFormat  

----  
\[Bullet] Each data object is wrapped with type-specific BEGIN...END lines, to trigger the processing of the contained data in the correct way.
\[Bullet] The hash sign (#) is to be interpreted as a comment character in the manner of Python. Need some way to declare that a comment is actually a header?
\[Bullet] An arbitrary number of headers may be present on each object. We recommend that these headers are escaped by a prefix # character, for compatibility with gnuplot
\[Bullet] For compatibility with gnuplot, the first 6 ??? data columns of records must be compatible with the gnuplot "plot with xyerrors" format, namely: x, y, xlow, xhigh, ylow, yhigh


HISTO1D 
The main data type.
xcenter, ycenter, xlow, xhigh, ylow, yhigh, sumw, sumw2, sumwx, sumwx2

# xlow   xhigh   sumw  sumw2   sumwx   sumwx2  numEntries
----

*)

GetYodaRaw[fileName_] := (tempYoda = Import[fileName, "Table"];
   posL = Position[tempYoda, {"#", "BEGIN", ___}|{"BEGIN", ___}] // Flatten;
   nHistos = Length[posL];
   outHistos = {};
   
   (*

    Collect histograms into list,
   with position of beginning of histos'posL',split output into lists,
   be careful with last histo which goes from'BEGIN' to the end of \
the file

*)


   Do[
    AppendTo[outHistos,(*cleanup comments*)
      DeleteCases[
       tempYoda[[posL[[i]] ;; 
          If[i < nHistos, posL[[i + 1]] - 1, 
           Length[tempYoda]]]], {"#", ___} | {"###", ___} | {}]];, {i,
      nHistos}];


  (* Print some info *)

   Do[
    histo = outHistos[[n]];
    
    HistTitle = 
     Select[histo, 
      If[Head[#[[1]]] == String, StringMatchQ[#[[1]], "Path=" ~~ ___],
         False] &];

      HistTitleString = StringSplit[HistTitle[[1, 1]], "="][[2]];
    HistType = 
     Select[histo, 
      If[Head[#[[1]]] == String, StringMatchQ[#[[1]], "Type=" ~~ ___],
         False] &];
    
    HistType = StringSplit[HistType[[1, 1]], "="][[2]];
    
    Print[n, " Imported ..", HistType, "..  ", HistTitleString];
    
    , {n, Length[outHistos]}];
   
   return = outHistos
   
   );

HistogramPlot[histo_, HistogramOptions___] := 
  Module[{high, low, plotInput, HistData, HistTitle, overFlow, 
    underFlow, Nbins, HistTitleString}, 
   HistTitle = 
    Select[histo, 
     If[Head[#[[1]]] == String, StringMatchQ[#[[1]], "Path=" ~~ ___], 
       False] &];
   HistType = 
    Select[histo, 
     If[Head[#[[1]]] == String, StringMatchQ[#[[1]], "Type=" ~~ ___], 
       False] &];
   HistType = StringSplit[HistType[[1, 1]], "="][[2]];
   
   If [HistType !=  "Histo1D", 
    Print[ "Not a histogram. ", HistType , 
     " needs a different plot function"];
    Return[];
    ];
   
   
   HistTitleString = StringSplit[HistTitle[[1, 1]], "="][[2]];
   underFlow = 
    Select[histo, 
     If[Head[#[[1]]] == String, 
       StringMatchQ[#[[1]], "Underflow" ~~ ___], False] &];(*TODO:
   Implement,need example*)
   overFlow = 
    Select[histo, 
     If[Head[#[[1]]] == String, 
       StringMatchQ[#[[1]], "Overflow" ~~ ___], False] &];(*TODO:
   Implement,need example*)
   HistData = Select[histo, NumericQ[#[[1]]] &];
   plotInput = {#[[3]], #[[1]] <= x < #[[2]]} & /@ HistData;
   low = First[HistData][[1]];
   high = Last[HistData][[1]];
   Nbins = Length[HistData];
   Plot[Piecewise[plotInput], {x, low, high}, HistogramOptions, 
    PlotRange -> All, Exclusions -> None, PlotPoints -> 3 (Nbins), 
    Frame -> True, PlotLabel -> HistTitleString]
   ];

Needs["ErrorBarPlots`"];

ScatterPlot[histo_, plotOptions___] := 
  Module[{HistData, HistTitle, PlotData,
    HistTitleString}, 
   HistTitle = 
    Select[histo, 
     If[Head[#[[1]]] == String, StringMatchQ[#[[1]], "Path=" ~~ ___], 
       False] &];
   HistType = 
    Select[histo, 
     If[Head[#[[1]]] == String, StringMatchQ[#[[1]], "Type=" ~~ ___], 
       False] &];
   HistType = StringSplit[HistType[[1, 1]], "="][[2]];
   
   If [HistType !=  "Scatter2D", 
    Print[ "Not a scatter plot. ", HistType , 
     " needs a different plot function"];
    Return[];
    ];
   
   HistTitleString = StringSplit[HistTitle[[1, 1]], "="][[2]];
   
   HistData = Select[histo, NumericQ[#[[1]]] &];
   
   PlotData = {{#[[1]], #[[4]]}, 
       ErrorBar[{-#[[2]], #[[3]]}, {-#[[5]], #[[6]]}]} & /@ HistData;
   ErrorListPlot[PlotData, PlotRange -> All, Frame -> True, 
    plotOptions]
   
   ];
