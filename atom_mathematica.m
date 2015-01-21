
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

ShowListEfficiencies[json_, anaName_] := (
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


ShowListCuts[json_, anaName_] := (
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
