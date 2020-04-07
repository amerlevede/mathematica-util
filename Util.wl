(* ::Package:: *)

Quiet@Remove["Util`"];


BeginPackage["Util`"];


ReadCLI::usage = StringTrim@"
Read a value supplied by the command line, in the form name=value. If multiple such arguments are found, the last one is used.
The second argument is the default value, in case there was no command line argument supplied in that form.
The third (optional) argument is a function to transform the supplied value if it is found (will not be used on default vaule).
";

SetOpt::usage = StringTrim@"
SetOpt[option -> value][obj] returns a copy of the given object (e.g. Graphics), changing the given option value.
SetOpt[{option1->value1, option2->value2}] or SetOpt[opt1,opt2,...] applies the supplied options successively.

The value fields can use the symbol OldValue to represent the original, unreplaced value (or None if it was not present).
";
OldValue::usage = StringTrim@"
Symbol for SetOpt values that will be replaced by original value of the option.
";

OnValues::usage = StringTrim@"
OnValues[f][x] applies the function f to the list of values in the association, and returns the result with the association keys.
This assumes that f does not change the order or number of elements.
";

OnKeys::usage = StringTrim@"
OnKeys[f][x] applies the function f to the list of keys in the association, and returns the result with the associated values.
This assumes that f does not change the order or number of elements.
";

Ranks::usage = StringTrim@"
Ranks[x] replaces the elements in x by their ranks in the list or association.
";

PrintAndExport::usage = StringTrim@"
PrintAndExport[file][obj] exports obj to a file, and also prints obj to the notebook.
PrintAndExport[file, opts][obj] applies opts to the Export call.
";

FunctionQ::usage = StringTrim@"
Check if the supplied argument is a function.
This function is highly experimental and ad-hoc.
";

$RunningAsMain::usage = StringTrim@"
Returns True if the current code is being evaluated as a 'main' file, and False if it is being imported using Get or Needs.
";

$RunningAsScript::usage = StringTrim@"
Returns True if the current code is being evaluated through a text-based script-like interface, and False if it is being run in an interactive notebook.
";

When::usage = StringTrim@"
Functional version of If. When[cond][expr] evaluates expr when cond is True. When[cond, expr2][expr] evaluates expr2 otherwise.
";


Begin["Private`"];


ReadCLI[name_, default_, transform_:Identity] := If[#=={},default,transform@Last@#]&@Map[StringReplace[{StartOfString~~name~~"="->""}]]@Select[$ScriptCommandLine, StringStartsQ[name<>"="]];


SetAttributes[SetOpt, HoldAll];
SetOpt[o_->v_][obj_Graphics] := Graphics[obj[[1]], SetOpt[o->v]@obj[[2]]];
SetOpt[option_ -> value_][obj_]:=If[
    MemberQ[obj,(option->_)|(option:>_),1],
        ReleaseHold@Replace[Hold@obj,(r:Rule|RuleDelayed)[option, x_]:>r[option, ReleaseHold[Hold[value] /. OldValue -> x]],{2}],
        Append[obj,option->ReleaseHold[Hold[value]/.OldValue->None]]
    ];
SetOpt[r__Rule, o_ -> v_][obj_] := SetOpt[o->v][SetOpt[r][obj]];
SetOpt[{r___Rule}][obj_] := SetOpt[r][obj];
SetOpt[][obj_] := obj;


OnValues[f_][x_]:=AssociationThread[Keys@x,f@Values@x];


OnKeys[f_][x_]:=AssociationThread[f@Keys@x,Values@x];


Ranks[x_List] := Ordering@Ordering@x;
Ranks[x_Association] := OnValues[Ranks][x];


PrintAndExport[file_,opts___][plt_]:=(Print[Export[file,plt,opts]];plt);


FunctionQ[_Function | _InterpolatingFunction | _CompiledFunction] = True;
FunctionQ[f_Symbol] := Or[DownValues[f] =!= {}, MemberQ[ Attributes[f], NumericFunction ]];
FunctionQ[f_[a_]] := Select[Map[First@*Head][(First/@SubValues[multiply]/.multiply->dummy)[[;;,1]]], MatchQ[a, #]&] =!= {};
FunctionQ[Identity] = True;
FunctionQ[_] = False;


$RunningAsMain := $Input == "";


$RunningAsScript := Not@$Notebooks;


When[cond_, alt_][expr_] := If[cond, expr, alt];
When[cond_][expr_] := When[cond, Null][expr];


End[];


EndPackage[];
