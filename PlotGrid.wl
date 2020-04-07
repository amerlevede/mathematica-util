(* ::Package:: *)

Quiet@Remove["PlotGrid`*"];


BeginPackage["PlotGrid`", {"Util`"}];


PlotGrid::usage = StringTrim@"
PlotGrid[plots] produces a grid of plots.
PlotGrid[plots, {labelsx,labelsy}] uses the given labels to head the rows and columns. None specifies no labels; Automatic uses the relevant keys if the plots are given in the form of associations.
PlotGrid[plots, {labelsx, labelsy}, {freex, freey}] allows giving separate x-axes to rows and y-axes to columns. The default behaviour is {False,False}.
PlotGrid[options] represents an operator form that can be applied to a matrix of plots.

Options:
AxesLabel, BaseStyle, PlotLabel, GridLines, Padding, Spacings, Frame, FrameTicks, ImageSize, AspectRatio, PlotLegends, OtherFrameTicks

FrameTicks can be either None, Automatic, or a function of the form f[{axismin, axismax}, {columnlabel, rowlabel}].
";

OtherFrameTicks::usage = StringTrim@"
Option for PlotGrid that sets ticks on opposite ends of frame axes.
";


Begin["Private`"];


PlotGrid[
    argplots:{{__Graphics}..},
    arglabels:{_, _}:{Automatic, Automatic},
    argfree:{_, _}:{False, False},
    OptionsPattern[{
        AxesLabel   -> {None, None},
        BaseStyle   -> {},
        LabelStyle  -> {},
        PlotLabel   -> None,
        GridLines   -> None,
        GridLinesStyle -> Automatic,
        Padding     -> 30,
        Spacings    -> {0, 0},
        Frame       -> True,
        FrameTicks  -> {Automatic, Automatic},
        ImageSize   -> 300,
        AspectRatio -> 1/GoldenRatio,
        PlotRangeClipping -> True,
        PlotLegends -> None,
        OtherFrameTicks -> {None, None},
        Background  -> None,
        PlotRangePadding -> Automatic
        }]
    ] := Module[{xranges, yranges, xlabels, ylabels, xlabel, ylabel, plots, grid, title, spacexleft, spacexright, spaceybottom, spaceytop, padleft, padright, padtop, padbottom, xticks, yticks, xticks2, yticks2},
    
    (*List of x and y axes ranges*)
    xranges = MinMax /@ Transpose @ Map[Map[PlotRange]][argplots][[;;,;;,1,;;]];
    yranges = MinMax /@             Map[Map[PlotRange]][argplots][[;;,;;,2,;;]];
    
    (*List of column and row labels*)
    xlabels = If[Head[arglabels[[1]]] === List, arglabels[[1]], ConstantArray[arglabels[[1]], Length[xranges]]] /. None | Automatic -> ""; 
    ylabels = If[Head[arglabels[[2]]] === List, arglabels[[2]], ConstantArray[arglabels[[2]], Length[yranges]]] /. None | Automatic -> ""; 
    
    (*Labels for the x and y axes*)
    xlabel =        Style[#, OptionValue[LabelStyle]]        &@ Replace[OptionValue[AxesLabel][[1]], None -> ""]; 
    ylabel = Rotate[Style[#, OptionValue[LabelStyle]], Pi/2] &@ Replace[OptionValue[AxesLabel][[2]], None -> ""];
    
    (*Plot title*)
    title = "";(*Style[#, {FontWeight -> Bold, FontSize -> 18} ~Join~ OptionValue[BaseStyle]] &@ Replace[OptionValue[PlotLabel], None -> ""]; *)
    
    (*Padding between rows and columns*)
    spacexleft   = Switch[OptionValue[Spacings], {{_, _}, _}, OptionValue[Spacings][[1,1]], {_, _}, OptionValue[Spacings][[1]], _, OptionValue[Spacings]];
    spacexright  = Switch[OptionValue[Spacings], {{_, _}, _}, OptionValue[Spacings][[1,2]], {_, _}, OptionValue[Spacings][[1]], _, OptionValue[Spacings]];
    spaceybottom = Switch[OptionValue[Spacings], {_, {_, _}}, OptionValue[Spacings][[2,1]], {_, _}, OptionValue[Spacings][[2]], _, OptionValue[Spacings]];
    spaceytop    = Switch[OptionValue[Spacings], {_, {_, _}}, OptionValue[Spacings][[2,2]], {_, _}, OptionValue[Spacings][[2]], _, OptionValue[Spacings]];
    
    (*Padding on the sides of the grid (space for labels and titles)*)
    padleft   = Switch[OptionValue[Padding], {{_, _}, _}, OptionValue[Padding][[1,1]], {_, _}, OptionValue[Padding][[1]], _, OptionValue[Padding]];
    padright  = Switch[OptionValue[Padding], {{_, _}, _}, OptionValue[Padding][[1,2]], {_, _}, OptionValue[Padding][[1]], _, OptionValue[Padding]];
    padbottom = Switch[OptionValue[Padding], {_, {_, _}}, OptionValue[Padding][[2,1]], {_, _}, OptionValue[Padding][[2]], _, OptionValue[Padding]];
    padtop    = Switch[OptionValue[Padding], {_, {_, _}}, OptionValue[Padding][[2,2]], {_, _}, OptionValue[Padding][[2]], _, OptionValue[Padding]];
        
    (*Ticks on the x and y axes, and top and right frame sides*)
    xticks = Switch[OptionValue[FrameTicks][[1]], None, None &, Automatic, Automatic &, _, OptionValue[FrameTicks][[1]]]; 
    yticks = Switch[OptionValue[FrameTicks][[2]], None, None &, Automatic, Automatic &, _, OptionValue[FrameTicks][[2]]];
    xticks2 = Switch[OptionValue[OtherFrameTicks][[1]], None, None &, Automatic, Automatic &, _, OptionValue[OtherFrameTicks][[1]]]; 
    yticks2 = Switch[OptionValue[OtherFrameTicks][[2]], None, None &, Automatic, Automatic &, _, OptionValue[OtherFrameTicks][[1]]]; 
    
    (*Transform graphics to fit grid and titles*)
    plots = MapIndexed[SetOpt[
          Frame -> OptionValue[Frame]
        , BaseStyle -> OptionValue[BaseStyle]
        , FrameLabel -> {{None, If[#2[[2]] == Length[xranges], Rotate[Style[#, OptionValue[LabelStyle], OptionValue[BaseStyle]], Pi]&@ylabels[[#2[[1]]]], None]}, {None, If[#2[[1]] == 1, Style[#, OptionValue[LabelStyle], OptionValue[BaseStyle]]&@xlabels[[#2[[2]]]], None]}}
        , Axes -> False
        , LabelStyle -> OptionValue[BaseStyle]
        , GridLines -> OptionValue[GridLines]
        , GridLinesStyle -> OptionValue[GridLinesStyle]
        , ImageSize -> {OptionValue[ImageSize] + If[#2[[2]] == 1, padleft, spacexleft] + If[#2[[2]] == Length[xranges], padright, spacexright], OptionValue[ImageSize]*OptionValue[AspectRatio] + If[#2[[1]] == Length[yranges], padbottom, spaceybottom] + If[#2[[1]] == 1, padtop, spaceytop]}
        , AspectRatio -> OptionValue[AspectRatio]
        , ImagePadding -> {
            {If[#2[[2]] == 1, padleft, spacexleft], If[#2[[2]] == Length[xranges], padright, spacexright]},
            {If[#2[[1]] == Length[yranges], padbottom, spaceybottom], If[#2[[1]] == 1, padtop, spaceytop]}}
        , FrameTicks -> {
            {If[#2[[2]] == 1 || argfree[[2]], yticks[PlotRange[argplots[[#2[[1]],#2[[2]]]]][[2]], {xlabels[[#2[[2]]]], ylabels[[#2[[1]]]]}], None],
             yticks2[PlotRange[argplots[[#2[[1]],#2[[2]]]]][[2]], {xlabels[[#2[[2]]]], ylabels[[#2[[1]]]]}]}, 
            {If[#2[[1]] == Length[yranges] || argfree[[1]], xticks[PlotRange[argplots[[#2[[1]], #2[[2]]]]][[1]], {xlabels[[#2[[2]]]], ylabels[[#2[[1]]]]}], None], 
             xticks2[PlotRange[argplots[[#2[[1]],#2[[2]]]]][[1]], {xlabels[[#2[[2]]]], ylabels[[#2[[1]]]]}]}}
        , PlotRange -> {
            If[argfree[[1]], Switch[OldValue, None, Automatic, {_,_}, OldValue[[1]], _, OldValue], xranges[[#2[[2]]]]],
            If[argfree[[2]], Switch[OldValue, None, Automatic, {_,_}, OldValue[[2]], _, OldValue], yranges[[#2[[1]]]]]}
        , AxesOrigin -> {xranges[[#2[[2]],1]], yranges[[#2[[1]],1]]}
        , Background -> OptionValue[Background]
        , PlotRangeClipping -> OptionValue[PlotRangeClipping]
        , PlotRangePadding -> OptionValue[PlotRangePadding]
        ][#]&
    , argplots, {2}];
    
    (*Add labels and title*)
    grid = plots;
    grid = Prepend[Prepend[title][ConstantArray[SpanFromLeft, Length[xranges] - 1]]][grid]; 
    grid = Append[Prepend[xlabel][ConstantArray[SpanFromLeft, Length[xranges] - 1]]][grid]; 
    grid = Join[Prepend[""] /@ grid[[{1}]], Prepend[ylabel] /@ grid[[{2}]], Prepend[SpanFromAbove] /@ grid[[3 ;; All]]];
    
    (*Add legend maybe*)
    grid = Grid[grid, Spacings -> {0, 0}, Alignment -> {Center, Center}, Background -> OptionValue[Background]];
    grid = If[OptionValue[PlotLegends] === None, Identity, Legended[#, OptionValue[PlotLegends]] &][grid]
]; 


PlotGrid[ass_Association, labels:{_, _}:{Automatic, Automatic}, opts___] := 
   PlotGrid[Values[ass], {labels[[1]], If[labels[[2]] === Automatic, Keys[ass], labels[[2]]]}, 
    opts]; 
PlotGrid[asss:{__Association}, labels:{_, _}:{Automatic, Automatic}, opts___] := 
   Module[{xlabels}, xlabels = DeleteDuplicates[Flatten[Keys /@ asss, 1]]; 
     PlotGrid[Values /@ asss[[1 ;; All,Key /@ xlabels]] /. _Missing -> Graphics[{},{}], 
      {If[labels[[1]] === Automatic, xlabels, labels[[1]]], labels[[2]]}, opts]];


PlotGrid[args:{{Legended[_Graphics, _]..}..}, labels:{_, _}:{Automatic, Automatic}, 
    opts___] := Module[{legends}, 
    legends = DeleteDuplicates[Flatten[args[[1 ;; All,1 ;; All,2]]]]; Print[legends]; 
     If[Length[legends] == 1, Legended[PlotGrid[args[[1 ;; All,1 ;; All,1]], labels, opts], 
       Placed[SetOpt[LegendLayout -> "Row"][legends[[1]] /. {Placed[x_, __] :> x}], Bottom]], 
      PlotGrid[Map[SetOpt[Epilog, Inset[#1[[2]] /. Placed[x_, __] :> x, Scaled[{1, 0}], 
            Scaled[{1, 0}], Background -> White]][#1[[1]]] & , args, {2}], labels, opts]]]; 


PlotGrid[args___][plts_] := PlotGrid[plts, args]; 


PlotGrid[{{args__Graphics}, rows_Integer}, opts___] := PlotGrid`reshape[PlotGrid[{{args}}, opts], rows];
PlotGrid[{ass_Association, rows_Integer}, opts___] := PlotGrid`reshape[PlotGrid[{ass}, opts], rows];

Clear[reshape];
reshape[Grid[{{"", title_, spans:SpanFromLeft...}, {ylabel_, gs__Graphics}, {SpanFromAbove, xlabel_, SpanFromLeft...}}, opts___], rows_] := Module[{grid, cols},
    cols = Ceiling[Length[{gs}]/rows];
    grid = ArrayReshape[{gs}, {rows, cols}, Graphics[]];
    grid = Prepend[Prepend[title][ConstantArray[SpanFromLeft, cols - 1]]][grid]; 
    grid = Append[Prepend[xlabel][ConstantArray[SpanFromLeft, cols - 1]]][grid]; 
    grid = Join[Prepend[""] /@ grid[[{1}]], Prepend[ylabel] /@ grid[[{2}]], Prepend[SpanFromAbove] /@ grid[[3 ;; All]]];
    Grid[grid, opts]
    ];
    
reshape[Legended[grid_, legend_], rows_] := Legended[reshape[grid, rows], legend];


PlotGrid[a_Dataset, args___] := PlotGrid[Normal@a, args];
PlotGrid[a:{__Dataset}, args___] := PlotGrid[Normal/@a, args];
PlotGrid[{a_Dataset, rows_Integer}, args___] := PlotGrid[{Normal@a, rows}, args];


End[];


EndPackage[];
