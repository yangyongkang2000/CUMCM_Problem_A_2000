(* ::Package:: *)

(* ::Title:: *)
(*                         \:6570\:5b66\:5efa\:6a21A\:9898*)


(* ::Text:: *)
(*\:6570\:636e\:5bfc\:5165*)


data=Import["/Users/yangyongkang/Downloads/CUMCM2020Probelms/A/\:9644\:4ef6.xlsx","Data"][[1]][[2;;]];


(* ::Text:: *)
(*\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf.png",
ListPlot[data,PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}]];


(* ::Text:: *)
(*\:7ea6\:675f\:6761\:4ef6*)
(**)


constranint=(Or@@Take[Table[150/7+213/7 (-1+k)<=Indexed[x,1]<=333/7+213/7 (-1+k),{k,1,11}],#]&/@{{1,5},{6,6},{7,
7},{8,9}})~Append~(Indexed[x,1]>291+30/7);


(* ::Text:: *)
(*\:975e\:95f4\:9699\:533a\:6570\:636e*)


data1=Select[data,Function[{x},#]]&/@constranint;


(* ::Text:: *)
(*\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:62df\:5408\:6a21\:62df*)


np=With[{tmp={175,195,235,255,25}},
NonlinearModelFit[data1[[#]],tmp[[#]]-E^(-k*t)*(tmp[[#]]-data1[[#,1,2]])/E^(-k*data1[[#,1,1]]),k,t]&/@Range@5];


(* ::Text:: *)
(*\:5168\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:516c\:5f0f*)


function2[v_Real,tmperature_List,
k_Real:Mean[Normal@np[[;;-2]]/.a_+b_*E^(c_+d_*t):>-d],
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1]]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,fittingfunctionlist},
fittingfunction[0][x_]:=32.411632653061226`;
Flatten[Table@@@Table[fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,9}],1]]


(* ::Text:: *)
(*\:5168\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:62df\:5408\:56fe\:5f62*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf\:548c\:5168\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:62df\:5408\:5bf9\:6bd4.png",
ListPlot[{function2[7/6.,{175,195,235,255,25}],data},
PlotLegends->{"\:5168\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:62df\:5408\:66f2\:7ebf","\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf"},PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}]];


(* ::Text:: *)
(*\:70ed\:8f90\:5c04\:52a0\:5bf9\:6d41\:5dee\:5206\:65b9\:7a0b\:62df\:5408*)


npfunction[tmp_List]:=Table[NonlinearModelFit[MapThread[List,
{(#-tmp[[k]])&/@data1[[k]][[All,2]][[;;-2]],(#^4-tmp[[k]]^4)&/@data1[[k]][[All,2]][[;;-2]],#
2/#1&@@@Differences@data1[[k]]}],a*x+b*y,{a,b},{x,y}],{k,1,Length@tmp}]


(* ::Text:: *)
(*\:62df\:5408\:7cfb\:6570\:56e0\:5b50*)


data3=Normal@npfunction[{175,195,235,255,25}]/.a_*x+b_*y:>{a,b};


(* ::Text:: *)
(*\:5168\:5bf9\:6d41\:52a0\:70ed\:8f90\:5c04\:6a21\:62df*)


npfunction[tmp_List]:=
Table[NonlinearModelFit[MapThread[List,{(#-tmp[[k]])&/@data1[[k]][[All,2]][[;;-2]],
(#^4-tmp[[k]]^4)&/@data1[[k]][[All,2]][[;;-2]],
#2/#1&@@@Differences@data1[[k]]}],a*x+b*y,{a,b},{x,y}],{k,1,Length@tmp}]


(* ::Text:: *)
(*\:5404\:6e29\:533a\:7cfb\:6570*)


data3=Normal@npfunction[{175,195,235,255,25}]/.a_*x+b_*y:>{a,b};


(* ::Text:: *)
(*\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:52a0\:70ed\:8f90\:5c04*)


function[v_Real,tmperature_List,k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:{{0.023984019158063287`,-8.011724895980368`*^-10},
{0.023984019158063287`,-8.011724895980368`*^-10},
{-0.01566960505173086`,8.194905445225778`*^-10}}]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,fittingfunctionlist,pointdata},
fittingfunction[0][t_]:=32.411632653061226`;
fittingfunctionlist=Table[
fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,6}];
pointdata[0]={{time[[7]],fittingfunction[6][time[[7]]]}};
Table[pointdata[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[6+i]]-ab [[i,2]]tmp[[6+i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata[i-1][[-1,1]],pointdata[i-1][[-1,2]]},#1[[1]]<=timelist[[-4+i,-1]]&],{i,1,3}];
Flatten[Join[(Table@@@fittingfunctionlist),{pointdata[1],pointdata[2],pointdata[3]}],1]]


(* ::Text:: *)
(*\:5168\:5bf9\:6d41\:52a0\:70ed\:8f90\:5c04*)


function1[v_Real,tmperature_List,k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:Riffle[data3,data3[[;;-2]]]]:=Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,
timelist=(distancelist/v),time=(distancelist/v)[[All,1]],pointdata1,pointdata1List},
pointdata1[0]={{150/7,32.411632653061226`}};
pointdata1List=Table[
pointdata1[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[i]]-ab [[i,2]]tmp[[i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata1[i-1][[-1,1]],pointdata1[i-1][[-1,2]]},
#1[[1]]<=timelist[[i,-1]]&],{i,1,9}];Flatten[pointdata1List,1]]


(* ::Text:: *)
(*\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf\:548c\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:52a0\:70ed\:8f90\:5c04\:62df\:5408\:66f2\:7ebf\:5bf9\:6bd4*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf\:548c\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:52a0\:70ed\:8f90\:5c04\:62df\:5408\:66f2\:7ebf\:5bf9\:6bd4.png",
ListPlot[{function[7/6.,{175,195,235,255,25}],data},
PlotLegends->{"\:725b\:987f\:51b7\:5374\:5b9a\:5f8b\:52a0\:70ed\:8f90\:5c04\:62df\:5408\:66f2\:7ebf","\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf"},
PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}]];


(* ::Text:: *)
(*\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf\:548c\:70ed\:8f90\:5c04\:52a0\:70ed\:5bf9\:6d41\:62df\:5408\:66f2\:7ebf\:62df\:5408\:66f2\:7ebf\:5bf9\:6bd4*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf\:548c\:70ed\:8f90\:5c04\:52a0\:70ed\:5bf9\:6d41\:62df\:5408\:66f2\:7ebf\:62df\:5408\:66f2\:7ebf\:5bf9\:6bd4.png",
ListPlot[{function1[7/6.,{175,195,235,255,25}],data},PlotLegends->{"\:70ed\:8f90\:5c04\:52a0\:70ed\:5bf9\:6d41\:62df\:5408\:66f2\:7ebf","\:9644\:4ef6\:7089\:6e29\:66f2\:7ebf"},
PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}]];


(* ::Text:: *)
(*\:95ee\:9898\:4e00\:7089\:6e29\:66f2\:7ebf*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:95ee\:9898\:4e00\:62df\:5408\:66f2\:7ebf.png",
ListPlot[function[78/60.,{173,198,230,257,25}],
PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}]];


(* ::Text:: *)
(*\:95ee\:9898\:4e00\:7089\:6e29\:66f2\:7ebf\:63d2\:503c*)


ipf=Interpolation[Union@function[78/60.,{173,198,230,257,25}]]


(* ::Text:: *)
(*\:5bfc\:51fa\:8868\:683c*)


Export["/Users/yangyongkang/Downloads/CUMCM2020Probelms/A/result.csv",
{{"time/s","temperature\!\(\*SuperscriptBox[\(/\), \(0\)]\)C"}}~Join~Table[{x,ipf[x]},{x,18.4,315,0.5}]];


(* ::Text:: *)
(*\:6e29\:533a\:4e2d\:5fc3\:6e29\:5ea6*)


temperature[n_Integer]:=ipf[(25+35.5*(n-1)+30.5/2)/(78/60)]


(* ::Text:: *)
(*\:95ee\:9898\:4e8c\:904d\:5386\:4ee3\:7801*)


Do[
With[{numdata=Union@function[v/60.,{182,203,237,254,25}]},
If[And[AllTrue[#2/#1&@@@Differences@numdata,-3<=#<=3&],
240<=Max[numdata[[All,2]]]<=250,60<=(SelectFirst[numdata,#[[2]]>=190&][[1]]-SelectFirst[numdata,#[[2]]>=150&][[1]])<=120,
40<=((#[[-1,1]]-#[[1,1]])&@Select[numdata,#[[2]]>=217&])<=90]==True,Print[v];Break[]]],{v,100,65,-1}]


(* ::Text:: *)
(*\:95ee\:9898\:4e8c\:7089\:6e29\:66f2\:7ebf*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:95ee\:9898\:4e8c\:62df\:5408\:66f2\:7ebf.png",
ListPlot[function[72/60.,{182,203,237,254,25}],
PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}]];


(* ::Text:: *)
(*\:95ee\:9898\:4e09*)


area[v_Real,tmperature_List,k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:{{0.023984019158063287`,-8.011724895980368`*^-10},
{0.023984019158063287`,-8.011724895980368`*^-10},
{-0.01566960505173086`,8.194905445225778`*^-10}}]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,
fittingfunctionlist,pointdata,numdata,pos,pos1,bool,benchdata},
fittingfunction[0][t_]:=32.411632653061226`;
fittingfunctionlist=Table[
fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,6}];
pointdata[0]={{time[[7]],fittingfunction[6][time[[7]]]}};
Table[pointdata[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[6+i]]-ab [[i,2]]tmp[[6+i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata[i-1][[-1,1]],pointdata[i-1][[-1,2]]},#1[[1]]<=timelist[[-4+i,-1]]&],{i,1,3}];
numdata=Flatten[Join[(Table@@@fittingfunctionlist),{pointdata[1],pointdata[2]}],1];
benchdata=Select[numdata,#[[2]]>=217&];
If[And[240<=numdata[[-1,2]]<=250,60<=(#[[-1,1]]-#[[1,1]]&@Select[numdata,150<=#[[2]]<=190&])<=120,
40<=Select[pointdata[3],#[[2]]>=217&][[-1,1]]-benchdata[[1,1]]<=90,
(#2/#1&@@(numdata[[2]]-numdata[[1]]))<=3&&(#2/#1&@@(pointdata[3][[-1]]-pointdata[3][[-2]]))>=-3],
{v,tmperature,Total[(#2+#4)*(#3-#1)/2&@@@Partition[Flatten@benchdata,4,2]]},Nothing[]]
]


area1[v_Real,tmperature_List,k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:{{0.023984019158063287`,-8.011724895980368`*^-10},
{0.023984019158063287`,-8.011724895980368`*^-10},
{-0.01566960505173086`,8.194905445225778`*^-10}}]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,fittingfunctionlist,pointdata,numdata,pos,pos1,bool,benchdata},
fittingfunction[0][t_]:=32.411632653061226`;
fittingfunctionlist=Table[
fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,6}];
pointdata[0]={{time[[7]],fittingfunction[6][time[[7]]]}};
Table[pointdata[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[6+i]]-ab [[i,2]]tmp[[6+i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata[i-1][[-1,1]],pointdata[i-1][[-1,2]]},#1[[1]]<=timelist[[-4+i,-1]]&],{i,1,3}];
numdata=Flatten[Join[(Table@@@fittingfunctionlist),{pointdata[1]}],1];
benchdata=Select[numdata,#[[2]]>=217&];If[And[240<=numdata[[-1,2]]<=250,60<=(#[[-1,1]]-#[[1,1]]&@Select[numdata,150<=#[[2]]<=190&])<=120,
40<=Select[pointdata[3],#[[2]]>=217&][[-1,1]]-benchdata[[1,1]]<=90,
(#2/#1&@@(numdata[[2]]-numdata[[1]]))<=3&&(#2/#1&@@(pointdata[3][[-1]]-pointdata[3][[-2]]))>=-3],
{v,tmperature,
Total[(#2+#4)*(#3-#1)/2&@@@Partition[Flatten@benchdata,4,2]]-benchdata[[1,2]]*(benchdata[[-1,1]]-benchdata[[1,1]])},{}]
]


(* ::Text:: *)
(*\:95ee\:9898\:4e09\:904d\:5386\:6700\:4f18\:7ec4\:5408*)


min=1000000;Do[Do[If[Length@(list=area1[v/60.,{i,j,k,l,25}])!=0,
If[list[[-1]]<=min,minlist=list;min=list[[-1]]],Break[]],
{v,65,100}],{i,165,185},{j,185,205},{k,225,245},{l,245,265}]//AbsoluteTiming


{1.3166666666666667`,{165,185,225,264,25},6308.356923660918`}


(* ::Text:: *)
(*\:95ee\:9898\:4e09\:6700\:4f18\:7089\:6e29\:66f2\:7ebf*)


Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/\:95ee\:9898\:4e09\:6700\:4f18\:7089\:6e29\:66f2\:7ebf.png",Show[ListPlot[#[[1;;1935]]~Join~#[[1992;;]],
PlotTheme->"Scientific",FrameLabel->{"\:65f6\:95f4/\:79d2","\:6e29\:5ea6/\:6444\:6c0f\:5ea6"}],
ListPlot[#[[1936;;1991]],Filling->Bottom,
PlotTheme->"Scientific",FillingStyle->Black]]&@(function@@minlist[[;;-2]])];


(* ::Text:: *)
(*\:95ee\:9898\:56db\:7ec8\:6781\:6700\:4f18\:5316\:7089\:6e29\:66f2\:7ebf*)


optimize[v_Real,tmperature_List,k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:{{0.023984019158063287`,-8.011724895980368`*^-10},
{0.023984019158063287`,-8.011724895980368`*^-10},
{-0.01566960505173086`,8.194905445225778`*^-10}}]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,fittingfunctionlist,pointdata,numdata,pos,pos1,bool,benchdata},
fittingfunction[0][t_]:=32.411632653061226`;
fittingfunctionlist=Table[
fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,6}];
pointdata[0]={{time[[7]],fittingfunction[6][time[[7]]]}};
Table[pointdata[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[6+i]]-ab [[i,2]]tmp[[6+i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata[i-1][[-1,1]],pointdata[i-1][[-1,2]]},#1[[1]]<=timelist[[-4+i,-1]]&],{i,1,3}];
numdata=Flatten[Join[(Table@@@fittingfunctionlist),{pointdata[1]}],1];
benchdata=Select[numdata,#[[2]]>=217&];If[And[240<=numdata[[-1,2]]<=250,60<=(#[[-1,1]]-#[[1,1]]&@Select[numdata,150<=#[[2]]<=190&])<=120,
40<=Select[pointdata[3],#[[2]]>=217&][[-1,1]]-benchdata[[1,1]]<=90,
(#2/#1&@@(numdata[[2]]-numdata[[1]]))<=3&&(#2/#1&@@(pointdata[3][[-1]]-pointdata[3][[-2]]))>=-3],
{v,tmperature,
(Total[(#2+#4)*(#3-#1)/2&@@@Partition[Flatten@benchdata,4,2]]-benchdata[[1,2]]*(benchdata[[-1,1]]-benchdata[[1,1]])+RealAbs@Total[#2/#1&@@@Differences@Union@Join[benchdata,Select[Join[pointdata[2],pointdata[3]],#[[2]]>=217&]]])/2},{}]
]


min=1000000;Do[Do[If[Length@(list=optimize[v/60.,{i,j,k,l,25}])!=0,
If[list[[-1]]<=min,minlist=list;
min=list[[-1]]],Break[]],{v,65,100}],
{i,165,185},{j,185,205},{k,225,245},{l,245,265}]//AbsoluteTiming


productdata[v_Real,tmperature_List,k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:{{0.023984019158063287`,-8.011724895980368`*^-10},
{0.023984019158063287`,-8.011724895980368`*^-10},
{-0.01566960505173086`,8.194905445225778`*^-10}}]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,fittingfunctionlist,pointdata,numdata,pos,pos1,bool,benchdata},
fittingfunction[0][t_]:=32.411632653061226`;
fittingfunctionlist=Table[
fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,6}];
pointdata[0]={{time[[7]],fittingfunction[6][time[[7]]]}};
Table[pointdata[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[6+i]]-ab [[i,2]]tmp[[6+i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata[i-1][[-1,1]],pointdata[i-1][[-1,2]]},#1[[1]]<=timelist[[-4+i,-1]]&],{i,1,3}];
numdata=Flatten[Join[(Table@@@fittingfunctionlist),{pointdata[1]}],1];
benchdata=Select[numdata,#[[2]]>=217&];If[And[240<=numdata[[-1,2]]<=250,60<=(#[[-1,1]]-#[[1,1]]&@Select[numdata,150<=#[[2]]<=190&])<=120,
40<=Select[pointdata[3],#[[2]]>=217&][[-1,1]]-benchdata[[1,1]]<=90,
(#2/#1&@@(numdata[[2]]-numdata[[1]]))<=3&&(#2/#1&@@(pointdata[3][[-1]]-pointdata[3][[-2]]))>=-3],
{v,tmperature,
Total[(#2+#4)*(#3-#1)/2&@@@Partition[Flatten@benchdata,4,2]]-benchdata[[1,2]]*(benchdata[[-1,1]]-benchdata[[1,1]]),
RealAbs@Total[#2/#1&@@@Differences@Union@Join[benchdata,Select[Join[pointdata[2],pointdata[3]],#[[2]]>=217&]]]},{}]
];


(* ::Text:: *)
(*\:7075\:654f\:5ea6\:5206\:6790*)


sensitivity[T_,v_Real:7/6.,tmperature_List:{175,195,235,255,25},k_Real:0.01918189383347094`,
distancelist_List:Partition[FoldList[Plus,25,{172.5`,5,30.5`,5,30.5`,5,66.`,5,66.`}],2,1],
ab_List:{{0.023984019158063287`,-8.011724895980368`*^-10},
{0.023984019158063287`,-8.011724895980368`*^-10},
{-0.01566960505173086`,8.194905445225778`*^-10}}]:=
Block[{tmp=Riffle[#,Mean/@Partition[#,2,1]]&@tmperature,timelist=(distancelist/v),
time=(distancelist/v)[[All,1]],fittingfunction,fittingfunctionlist,pointdata},
fittingfunction[0][t_]:=T;
fittingfunctionlist=Table[
fittingfunction[i][t_]=tmp[[i]]-E^(-k*t)*(tmp[[i]]-fittingfunction[i-1][time[[i]]])/E^(-k*time[[i]]);
{{t,fittingfunction[i][t]},Join[{t},timelist[[i]],{0.1}]},{i,1,6}];
pointdata[0]={{time[[7]],fittingfunction[6][time[[7]]]}};
Table[pointdata[i]=NestWhileList[{#1[[1]]+0.5,1/2 (-ab[[i,1]] tmp[[6+i]]-ab [[i,2]]tmp[[6+i]]^4+2 #1[[2]]+ab[[i,1]]#1[[2]]+ab [[i,2]]#1[[2]]^4)}&,
{pointdata[i-1][[-1,1]],pointdata[i-1][[-1,2]]},#1[[1]]<=timelist[[-4+i,-1]]&],{i,1,3}];
Flatten[Join[(Table@@@fittingfunctionlist),{pointdata[1],pointdata[2],pointdata[3]}],1]]


(* ::Text:: *)
(*\:95ee\:9898\:56db\:6570\:636e\:96c6*)


Data={};Do[Do[If[Length@(list=productdata[v/60.,{i,j,k,l,25}])!=0,
AppendTo[Data,list],Break[]],{v,65,100}],{i,165,185},{j,185,205},{k,225,245},{l,245,265}];
Export["/Users/yangyongkang/Desktop/2020\:6570\:5b66\:5efa\:6a21/Data.wl",Data];//AbsoluteTiming


With[{max1=MaximalBy[Data,Last][[1,-1]],
max2=MaximalBy[Data,#[[-2]]&][[1,-1]]},
MinimalBy[{#1,#2,#3/max2+#4/max1}&@@@Data,Last]]


(* ::Text:: *)
(**)
