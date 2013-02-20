%%% file type_check_bopl.ecl

:- module(type_check_bopl).

:- use_module(library(pretty_print)).
:- use_module(lexer_bopl).
:- use_module(parser_bopl).

%%% the struct of the typed tree; the type field holds the type
%%% every struct has a type field, even if it is trivial or constant

%%$% program ::= program(class*, var*, inst)
:- export struct(tProgram(clas,vars,insts,type,file,start,end)).

%%$% class   ::= class(id, cexp, var*, method*)
:- export struct(tClass(id,cexp,vars,methods,type,file,start,end)).


%%$% cexp    ::= cexp(id)
:- export struct(tCexp(id,type,file,line)).

%%$% var     ::= var(cexp, id)
:- export struct(tVar(cexp,id,type,file,line)).

%%$% method  ::= method(id, var*, cexp, var*, inst)
:- export struct(tMethod(id,formals,cexp,locals,inst,type,file,start,end)).

%%$% inst    ::= seq(inst, inst) | assign(id, exp) | writeField(exp, id, exp) |
%%$%             if(exp, inst, inst) | while(exp, inst) | return(exp) |
%%$%             writeln(Exp)
%% we don't have tSeq, we represent sequences as lists of instructions
:- export struct(tAssign(id,exp,type,file,line)).
:- export struct(tWriteField(obj,id,val,type,file,line)).
:- export struct(tIf(exp,seqthen,seqelse,type,file,start,end)).
:- export struct(tWhile(exp,seqdo,type,file,start,end)).
:- export struct(tReturn(exp,type,file,line)).
:- export struct(tWriteln(exp,type,file,line)).

%%% we only keep the starting line of expressions, they usually sit on
%%% a single line.
%$% exp     ::= int(N) | boolean(true) | boolean(false) | not(exp) | nil | self |
:- export struct(tInt(num,type,file,line)).
:- export struct(tBoolean(bool,type,file,line)).
:- export struct(tNot(exp,type,file,line)).
:- export struct(tNil(type,file,line)).
:- export struct(tSelf(type,file,line)).
%$%             super | new(cexp) | instanceof(exp, cexp) | id |
:- export struct(tSuper(type,file,line)).
:- export struct(tNew(cexp,type,file,line)).
:- export struct(tInstanceOf(exp,cexp,type,file,line)).
:- export struct(tIdent(id,type,file,line)).
%$%             methodcall(exp, id, exp*) | readField(exp, id) |id(Atom) |
:- export struct(tMethodCall(recv,id,args,type,file,line)).
:- export struct(tReadField(obj,id,type,file,line)).
:- export struct(tAtom(id,type,file,line)).
%$%             plus(exp, exp) | minus(exp, exp) | times(exp, exp) |
:- export struct(tPlus(left,right,type,file,line)).
:- export struct(tMinus(left,right,type,file,line)).
:- export struct(tTimes(left,right,type,file,line)).
%$%             equal(exp, exp) | and(exp, exp) | or(exp, exp) | less(exp, exp)
:- export struct(tEqual(left,right,type,file,line)).
:- export struct(tAnd(left,right,type,file,line)).
:- export struct(tOr(left,right,type,file,line)).
:- export struct(tLess(left,right,type,file,line)).


%% predicate atFile to give the file name
atFile(tProgram{file:FileName},FileName).
atFile(tClass{file:FileName},FileName).
atFile(tCexp{file:FileName},FileName).
atFile(tVar{file:FileName},FileName).
atFile(tMethod{file:FileName},FileName).
atFile(tWriteField{file:FileName},FileName).
atFile(tIf{file:FileName},FileName).
atFile(tWhile{file:FileName},FileName).
atFile(tWriteln{file:FileName},FileName).
atFile(tInt{file:FileName},FileName).
atFile(tBoolean{file:FileName},FileName).
atFile(tNot{file:FileName},FileName).
atFile(tNil{file:FileName},FileName).
atFile(tSelf{file:FileName},FileName).
atFile(tSuper{file:FileName},FileName).
atFile(tNew{file:FileName},FileName).
atFile(tInstanceOf{file:FileName},FileName).
atFile(tIdent{file:FileName},FileName).
atFile(tMethodCall{file:FileName},FileName).
atFile(tReadField{file:FileName},FileName).
atFile(tAtom{file:FileName},FileName).
atFile(tPlus{file:FileName},FileName).
atFile(tMinus{file:FileName},FileName).
atFile(tTimes{file:FileName},FileName).
atFile(tEqual{file:FileName},FileName).
atFile(tAnd{file:FileName},FileName).
atFile(tOr{file:FileName},FileName).
atFile(tLess{file:FileName},FileName).
%% atFile also works on a list (e.g. of classes), we got that rule in parser
%% atFile([Head|_],FileName) :- atFile(Head,FileName).

%% predicate atStart to give the start line number
atStart(tProgram{start:StartLine},StartLine).
atStart(tClass{start:StartLine},StartLine).
atStart(tMethod{start:StartLine},StartLine).
atStart(tIf{start:StartLine},StartLine).
atStart(tWhile{start:StartLine},StartLine).
%: atStart([Head|_],StartLine) :- atStart(Head,StartLine).
%: atStart(X,StartLine) :- atLine(X,StartLine).

%% predicate atStart to give the end line number
atEnd(tProgram{end:EndLine},EndLine).
atEnd(tClass{end:EndLine},EndLine).
atEnd(tMethod{end:EndLine},EndLine).
atEnd(tIf{end:EndLine},EndLine).
atEnd(tWhile{end:EndLine},EndLine).
atEnd([E],EndLine) :- atEnd(E,EndLine).
atEnd([_|E],EndLine) :- atEnd(E,EndLine).
atEnd(E,EndLine) :- atLine(E,EndLine).

%% predicate atLine to give the line 
atLine(pCexp{line:LineNo},LineNo).
atLine(pVar{line:LineNo},LineNo).
atLine(pAssign{line:LineNo},LineNo).
atLine(pWriteField{line:LineNo},LineNo).
atLine(pReturn{line:LineNo},LineNo).
atLine(pWriteln{line:LineNo},LineNo).
atLine(pInt{line:LineNo},LineNo).
atLine(pBoolean{line:LineNo},LineNo).
atLine(pNot{line:LineNo},LineNo).
atLine(pNil{line:LineNo},LineNo).
atLine(pSelf{line:LineNo},LineNo).
atLine(pSuper{line:LineNo},LineNo).
atLine(pNew{line:LineNo},LineNo).
atLine(pInstanceOf{line:LineNo},LineNo).
atLine(pMethodCall{line:LineNo},LineNo).
atLine(pReadField{line:LineNo},LineNo).
atLine(pAtom{line:LineNo},LineNo).
atLine(pPlus{line:LineNo},LineNo).
atLine(pMinus{line:LineNo},LineNo).
atLine(pEqual{line:LineNo},LineNo).
atLine(pAnd{line:LineNo},LineNo).
atLine(pOr{line:LineNo},LineNo).
atLine(pLess{line:LineNo},LineNo).
atLine([Head|_],StartLine) :- atLine(Head,StartLine).

%%%% predicate to test if a Tree has a given Type (or to retrieve the Type)
hasType(tProgram{type:Ty},Ty).
hasType(tClass{type:Ty},Ty).
hasType(tCexp{type:Ty},Ty).
hasType(tVar{type:Ty},Ty).
hasType(tMethod{type:Ty},Ty).
hasType(tAssign{type:Ty},Ty).
hasType(tWriteField{type:Ty},Ty).
hasType(tIf{type:Ty},Ty).
hasType(tWhile{type:Ty},Ty).
hasType(tReturn{type:Ty},Ty).
hasType(tWriteln{type:Ty},Ty).
hasType(tInt{type:Ty},Ty).
hasType(tBoolean{type:Ty},Ty).
hasType(tNot{type:Ty},Ty).
hasType(tNil{type:Ty},Ty).
hasType(tSelf{type:Ty},Ty).
hasType(tSuper{type:Ty},Ty).
hasType(tNew{type:Ty},Ty).
hasType(tInstanceOf{type:Ty},Ty).
hasType(tIdent{type:Ty},Ty).
hasType(tMethodCall{type:Ty},Ty).
hasType(tReadField{type:Ty},Ty).
hasType(tAtom{type:Ty},Ty).
hasType(tPlus{type:Ty},Ty).
hasType(tMinus{type:Ty},Ty).
hasType(tTimes{type:Ty},Ty).
hasType(tEqual{type:Ty},Ty).
hasType(tAnd{type:Ty},Ty).
hasType(tOr{type:Ty},Ty).
hasType(tLess{type:Ty},Ty).

%% to be completed

%% for a list
lastHasType([L],Typ) :- hasType(L,Typ).
lastHasType([_Hd|Ta],Typ) :- lastHasType(Ta,Typ).
