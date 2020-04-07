(* ::Package:: *)

BeginPackage["Axis`"];


Clear[axis];


axis[mima_:{0,1},direction:(Top|Bottom|Left|Right):Bottom,size:_?NumberQ:1,position:{_?NumberQ,_?NumberQ}:{0,0},OptionsPattern[{Ticks->Automatic,AxesStyle->{},LabelStyle->{},AxesLabel->None,FrameLabel->None,GridLines->None,GridLinesStyle->{}}]]:=Module[{min,max,majorticks,minorticks,majortick,minortick,label,label2,move,rot,sgn,grid,gridline},

{min,max}=mima;

rot=Switch[direction,Top|Bottom,IdentityMatrix[2],Left|Right,RotationMatrix[Pi/2]];
sgn=Switch[direction,Bottom|Right,1,Top|Left,-1];
{majorticks,minorticks}=Switch[OptionValue[Ticks]
,Automatic,{Cases[#,{_,_}],Cases[#,{c_,_,{_,_},_}:>c]}&[Charting`FindTicks[{min,max},{min,max}][min,max]]
,None,{{},{}}
];

majortick[{c_,l_}]:=If[c<min||c>max,{},{OptionValue[LabelStyle],Text[l,Offset[rot.{0,-5 sgn},rot.{(c-min)/(max-min),0}],sgn rot.{0,1}]}];
minortick[c_]:=If[c<min||c>max,{},Line[rot.#&/@{{(c-min)/(max-min),sgn 0.005},{(c-min)/(max-min),sgn 0}}]];

grid=Switch[OptionValue[GridLines]
,None,{}
,Automatic,First/@majorticks
,_List,OptionValue[GridLines]
];

gridline[c_]:=HalfLine[rot.{(c-min)/(max-min),0},rot.{0,sgn}];

label=Switch[OptionValue[AxesLabel]
,None,{}
,_,Text[OptionValue[AxesLabel],Offset[rot.{15,0},rot.{1.,0}],rot.{-1,0}]
];
label2=Switch[OptionValue[FrameLabel]
,None,{}
,_,Text[Rotate[OptionValue[FrameLabel],Switch[direction,Left,Pi/2,Right,-Pi/2,Top|Bottom,0]],Offset[rot.{0,-20sgn},rot.{0.5,0}],rot.{0,sgn}]
];

move[r_]:=r//Scale[#,size,{0,0}]&//Translate[#,position]&;

{
{LightGray,Dashed,OptionValue[GridLinesStyle],gridline/@grid}
,{Darker@Gray,OptionValue[AxesStyle],Line[rot.#&/@{{0,0},{1,0}}]}
,{Darker@Gray,OptionValue[AxesStyle],majortick/@majorticks}
,{Darker@Gray,OptionValue[LabelStyle],minortick/@minorticks}
,{OptionValue[LabelStyle],label2}
}//move
];

axis[{2,3},Bottom,GridLines->Automatic]//Graphics


EndPackage[];
