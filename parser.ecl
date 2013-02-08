%% -*- prolog -*-   %%% to make Emacs happy
%%%%%%%%%%%%%%%%%%%%%%%%%%% The BOPL parser
%%%%% for APS lecture MI030 "Analyse des programmes et sémantique"
%%%%% course by http://pagesperso-systeme.lip6.fr/Jacques.Malenfant/

%% (C) 2013, Basile Starynkevitch    <basile@starynkevitch.net>

%%  This program is free software; you can redistribute it
%%  and/or modify it under the terms of the GNU Lesser General Public
%%  License as published by the Free Software Foundation; either
%%  version 3 of the License, or (at your option) any later version.
%% 
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%  Lesser General Public License for more details.  You should have
%%  received a copy of the GNU Lesser General Public License along
%%  with this program. If not, see <http://www.gnu.org/licenses/>.

%%%%%%%%%%%%%%%%

%% Inspired by the parser.ecl file from Jacques.Malenfant@lip6.fr
%% [Re-]written by Basile Starynkevitch   basile@starynkevitch.net
%% To be compiled by ECLiPSe-CLP Prolog, see http://eclipseclp.org/

:- use_module(library(pretty_print)).
:- use_module(lexer).

:- module(parser).
:- export parseFile/2, parse/3.

%*****************************************************************************
% Concrete grammar
%
% Program    ::= program ClassList Locals Seq
% Locals     ::= <epsilon> | let Vars in
% ClassList  ::= <epsilon> | Classes
% Classes    ::= Class | Classes Class
% Class      ::= class id Extends is VarList MethodList end
% Extends    ::= <epsilon> | extends Classexp
% Classexp   ::= Int | Bool | Void | Object | id
% VarList    ::= <epsilon> | vars Vars
% Vars       ::= Var | Vars Var
% Var        ::= Classexp Ids ;
% Ids        ::= id | Ids , id
% MethodList ::= <epsilon> | methods Methods
% Methods    ::= Method | Methods Method
% Method     ::= Classexp id ( FormalList ) Locals Seq
% FormalList ::= epsilon | Formals
% Formals    ::= Formal | Formals , Formal
% Formal     ::= Classexp id
% Seq        ::= begin Insts end
% Insts      ::= Inst | Insts ; Inst
% Inst       ::= id := Exp | Exp . id := Exp | return Exp |
%                if Exp then Seq else Seq | while Exp do Seq |
%                writeln ( Exp )
% Exp        ::= Exp . id | Exp . id ( ActualList ) | Exp instanceof Classexp |
%                Exp + Term | Exp - Term | Exp or Term | Term
% Term       ::= Term * Fact | Term and Fact | Fact
% Fact       ::= Fact = Basic | Fact < Basic | Basic
% Basic      ::= not Exp | int | id | true | false | nil | self | super |
%                new Classexp | ( Exp )
% ActualList ::= epsilon | Actuals
% Actuals    ::= Exp | Actuals , Exp
%*****************************************************************************


%*****************************************************************************
%% abstract syntax: each tree knows its filename, start and end line numbers

%%$% program ::= program(class*, var*, inst)

:- export struct(pProgram(clas,vars,insts,file,start,end)).

%%$% class   ::= class(id, cexp, var*, method*)
:- export struct(pClass(id,cexp,vars,methods,file,start,end)).

%%$% cexp    ::= cexp(id)
:- export struct(pCexp(id,file,line)).

%%$% var     ::= var(cexp, id)
:- export struct(pVar(cexp,id,file,line)).

%%$% method  ::= method(id, var*, cexp, var*, inst)
:- export struct(pMethod(id,formals,cexp,locals,inst,file,start,end)).

%%$% inst    ::= seq(inst, inst) | assign(id, exp) | writeField(exp, id, exp) |
%%$%             if(exp, inst, inst) | while(exp, inst) | return(exp) |
%%$%             writeln(Exp)
:- export struct(pSeq(lefti,righti,file,start,end)).
:- export struct(pAssign(id,exp,file,line)).
:- export struct(pWriteField(obj,id,val,file,line)).
:- export struct(pIf(exp,ithen,ielse,file,start,end)).
:- export struct(pWhile(exp,inst,file,start,end)).
:- export struct(pReturn(exp,file,line)).
:- export struct(pWriteln(exp,file,line)).

%$% exp     ::= int(N) | boolean(true) | boolean(false) | not(exp) | nil | self |
%$%             super | new(cexp) | instanceof(exp, cexp) | id |
%$%             methodcall(exp, id, exp*) | readField(exp, id) |id(Atom) |
%$%             plus(exp, exp) | minus(exp, exp) | times(exp, exp) |
%$%             equal(exp, exp) | and(exp, exp) | or(exp, exp) | less(exp, exp)

%%% we only keep the starting line of expressions, they usually sit on
%%% a single line.
%$% exp     ::= int(N) | boolean(true) | boolean(false) | not(exp) | nil | self |
:- export struct(pInt(num,file,line)).
:- export struct(pBoolean(bool,file,line)).
:- export struct(pNot(exp,file,line)).
:- export struct(pNil(file,line)).
:- export struct(pSelf(file,line)).
%$%             super | new(cexp) | instanceof(exp, cexp) | id |
:- export struct(pSuper(file,line)).
:- export struct(pNew(cexp,file,line)).
:- export struct(pInstanceOf(exp,cexp,file,line)).
:- export struct(pId(id,file,line)).
%$%             methodcall(exp, id, exp*) | readField(exp, id) |id(Atom) |
:- export struct(pMethodCall(recv,id,args,file,line)).
:- export struct(pReadField(obj,id,file,line)).
:- export struct(pAtom(id,file,line)).
%$%             plus(exp, exp) | minus(exp, exp) | times(exp, exp) |
:- export struct(pPlus(left,right,file,line)).
:- export struct(pMinus(left,right,file,line)).
:- export struct(pTimes(left,right,file,line)).
%$%             equal(exp, exp) | and(exp, exp) | or(exp, exp) | less(exp, exp)
:- export struct(pEqual(left,right,file,line)).
:- export struct(pAnd(left,right,file,line)).
:- export struct(pOr(left,right,file,line)).
:- export struct(pLess(left,right,file,line)).


%% predicate atFile to give the file name
atFile(pProgram{file:FileName},FileName).
atFile(pClass{file:FileName},FileName).
atFile(pCexp{file:FileName},FileName).
atFile(pVar{file:FileName},FileName).
atFile(pMethod{file:FileName},FileName).
atFile(pSeq{file:FileName},FileName).
atFile(pWriteField{file:FileName},FileName).
atFile(pIf{file:FileName},FileName).
atFile(pWhile{file:FileName},FileName).
atFile(pWriteln{file:FileName},FileName).
atFile(pInt{file:FileName},FileName).
atFile(pBoolean{file:FileName},FileName).
atFile(pNot{file:FileName},FileName).
atFile(pNil{file:FileName},FileName).
atFile(pSelf{file:FileName},FileName).
atFile(pSuper{file:FileName},FileName).
atFile(pNew{file:FileName},FileName).
atFile(pInstanceOf{file:FileName},FileName).
atFile(pId{file:FileName},FileName).
atFile(pMethodCall{file:FileName},FileName).
atFile(pReadField{file:FileName},FileName).
atFile(pAtom{file:FileName},FileName).
atFile(pPlus{file:FileName},FileName).
atFile(pMinus{file:FileName},FileName).
atFile(pTimes{file:FileName},FileName).
atFile(pEqual{file:FileName},FileName).
atFile(pAnd{file:FileName},FileName).
atFile(pOr{file:FileName},FileName).
atFile(pLess{file:FileName},FileName).
%% atFile also works on a list (e.g. of classes)
atFile([Head|_],FileName) :- atFile(Head,FileName).

%% predicate atStart to give the start line number
atStart(pProgram{start:StartLine},StartLine).
atStart(pClass{start:StartLine},StartLine).
atStart(pMethod{start:StartLine},StartLine).
atStart(pSeq{start:StartLine},StartLine).
atStart(pIf{start:StartLine},StartLine).
atStart(pWhile{start:StartLine},StartLine).
atStart([Head|_],StartLine) :- atStart(Head,StartLine).
atStart(X,StartLine) :- atLine(X,StartLine).

%% predicate atStart to give the end line number
atEnd(pProgram{end:EndLine},EndLine).
atEnd(pClass{end:EndLine},EndLine).
atEnd(pMethod{end:EndLine},EndLine).
atEnd(pSeq{end:EndLine},EndLine).
atEnd(pIf{end:EndLine},EndLine).
atEnd(pWhile{end:EndLine},EndLine).
atEnd([E],EndLine) :- atEnd(E,EndLine).
atEnd([H|E],EndLine) :- atEnd(E,EndLine).
atEnd(X,EndLine) :- atLine(E,EndLine).

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

% parseFile(+FileName, -AST)
parseFile(FileName,AST) :-
    scanner:scan(FileName,Tokens),
    parse(FileName,Tokens,AST),
    printf(output,"BOPL parsed file %s\n", [FileName]),
    pretty_print:pretty_print(stdout, AST, 80).

% parse(+FileName,+Tokens,-AST)
parse(FileName,Tokens,AST) :- 
    parseProgram(FileName,Tokens,AST,[]), !.

%parseToken(+Read, ?Expected)
parseToken(Read, Expected) :-
  nonvar(Read),
  Read = Expected.

    
     
parseProgram(FileName,[Tprog|TokensAfterProg], 
	     pProgram(Clas,Vars,SeqInsts,FileName,StartLine,EndLine), 
	     [])
:-
    parseToken(Tprog,tKeyw{loc:StartLine,word:program}),
    parseClassList(FileName, TokensAfterProg, Classes, TokensAfterClassList),
    parseLocals(FileName,TokensAfterClassList, Vars, TokensAfterLocals), 
    parseSeq(FileName,TokensAfterLocals,SeqInsts,[])
.

