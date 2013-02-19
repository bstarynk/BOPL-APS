%%%%%%%%%%%%%%%%%%%%%%%%%%% The BOPL parser; file parser_bopl.ecl
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

:- module(parser_bopl).


:- use_module(library(pretty_print)).
:- use_module(lexer_bopl).


:- dynamic debugWanted/1.
:- export parseFile/2, parse/2, dbgprintf/3, debugName/1, debugWanted/1, enableDebug/1, disableDebug/1.

dbgprintf(Name,Fmt,Args) :- 
        debugWanted(Name), atom(Name) 
	-> printf(output, "*| %a ",[Name]), printf(output, Fmt, Args), nl, !
        ; true, !
.

debugName(parseProgram).
debugName(parseClassList).
debugName(parseClass).
debugName(parseVarsList).
debugName(parseLocals).
debugName(parseMethodsList).
debugName(parseMethods).
debugName(parseMethod).
debugName(parseFormals).
debugName(parseFormal).
debugName(parseSeq).
debugName(parseInst).
debugName(parseExp).
debugName(parseRestExp).
debugName(parseTerm).
debugName(parseRestTerm).
debugName(parseFact).
debugName(parseRestFact).


enableDebug(X) :- debugName(X), assert(debugWanted(X)).
enableDebug(all) :- 
    findall(X,debugName(X),Z), !, (foreach(F,Z) do printf(output,"debugWanted %w\n",F),assert(debugWanted(F))).

disableDebug(X) :- debugName(X), retract(debugWanted(X)).
disableDebug(all) :- 
    findall(X,debugName(X),Z), !, (foreach(F,Z) do retract(debugWanted(F))).

%**1***************************************************************************
% Concrete grammar [lowercase are terminals]
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
:- export struct(pIf(exp,seqthen,seqelse,file,start,end)).
:- export struct(pWhile(exp,seqdo,file,start,end)).
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

% parseFile(+FileName, -AST)
parseFile(FileName,AST) :-
    scan(FileName,Tokens),
    parse(Tokens,AST),
    !,
    printf(output,"BOPL parsed file %s\n", [FileName]),
    pretty_print(stdout, AST, 80).



% parse(+FileName,+Tokens,-AST)
parse(Tokens,AST) :- 
    parseProgram(Tokens,AST,[]), !.

% parseToken(+Read, ?Expected)
parseToken(Read, Expected) :-
  nonvar(Read),
  Read = Expected.

    
     
%%%%%%%%%%%%%%%%
%!% Program    ::= program ClassList Locals Seq
parseProgram([Tprog|TokensAfterProg], 
             Program, 
	     []) :-
    parseToken(Tprog,tKeyw{line:StartLine,file:FileName,word:program}),
    !,
    dbgprintf(parseProgram,"TokensAfterProg: %w",[TokensAfterProg]),
    parseClassList(TokensAfterProg,Classes,TokensAfterClassList),
    dbgprintf(parseProgram,"got Classes: %w\n..parseProgram TokensAfterClassList= %w",[Classes,TokensAfterClassList]),
    !,
    parseLocals(TokensAfterClassList,Vars,TokensAfterLocals), 
    dbgprintf(parseProgram,"parseProgram local Vars: %w\n..parseProgram  TokensAfterLocals: %w",[Vars,TokensAfterLocals]),
    !,
    parseSeq(TokensAfterLocals,SeqInsts,[]),
    dbgprintf(parseProgram," SeqInsts %w",[SeqInsts]),
    atEnd(SeqInsts,EndLine),
    !,
    Program = pProgram{clas:Classes,vars:Vars,insts:SeqInsts,
                       file:FileName,start:StartLine,end:EndLine},
    !,
    printf(stdout, "parsed Program:%w\n", [Program]),nl
.

%%% for debugging, parse a string as a program
:- export parStrProgram/2.
parStrProgram(String,AST) :-
    scanString(String,Tokens),
    parseProgram(Tokens,AST,[]), !.

%%%%%%%%%%%%%%%%
%!% ClassList  ::= <epsilon> | Classes
%!% Classes    ::= Class | Classes Class

parseClassList(Tokens,Classes,RestTokens)
:- 
    dbgprintf(parseClassList,"Tokens=%w", [Tokens]),
    %% a lookahead
    ( Tokens = [tKeyw{word:class}|_]
      ->
	  parseClass(Tokens,Class1,TokensAfterClass1),
	  dbgprintf(parseClassList," Class1=%w, TokensAfterClass1=%w",
		    [Class1,TokensAfterClass1]),
	  !,
	  parseClassList(TokensAfterClass1,RestClasses,TokensAfterClassList),
	  Classes = [Class1|RestClasses],
	  RestTokens = TokensAfterClassList,
	  !
      ; 
      Classes = [],
      RestTokens = Tokens,
      !
    ),
    dbgprintf(parseClassList," Classes=%w\n.. parseClassList RestTokens=%w",
	   [Classes,RestTokens])
.

%%% for debugging, parse a string as a class list
:- export parStrClassList/2.
parStrClassList(String,AST) :-
    scanString(String,Tokens),
    parseClassList(Tokens,AST,[]), !.



%%%%%%%%%%%%%%%%
%!% Class      ::= class id Extends is VarList MethodList end
parseClass([tKeyw{word:class,line:StartLine,file:FileName},tId{name:ClId} |TokensAfterClassId],
	   Class,RestTokens)
:- 
    number(StartLine),
    ( 
	dbgprintf(parseClass," start StartLine=%w ClId=%w"
				 " TokensAfterClassId=%w",[StartLine,ClId,
							   TokensAfterClassId]),
	parseExtends(TokensAfterClassId,SuperClass,
                     TokensAfterExtends),
	dbgprintf(parseClass,"SuperClass=%w  TokensAfterExtends=%w", [SuperClass,TokensAfterExtends]),
	TokensAfterExtends = [tKeyw{word:is}|TokensAfterIs],
	dbgprintf(parseClass,"TokensAfterIs=%w", [TokensAfterIs]),
	parseVarsList(TokensAfterIs,VarList,TokensAfterVarsList),
	dbgprintf(parseClass, "VarList=%w\n ..parseClass.. TokensAfterVarsList=%w",
               [VarList,TokensAfterVarsList]),
	parseMethodsList(TokensAfterVarsList,MethodsList,TokensAfterMethodsList),
	dbgprintf(parseClass, "MethodsList=%w\n ..parseClass.. TokensAfterMethodsList=%w",
	       [MethodsList, TokensAfterMethodsList]),
	TokensAfterMethodsList = [tKeyw{word:end,line:EndLine}
                                       |RestTokens],
	Class = pClass{id:ClId,cexp:SuperClass,vars:VarList,methods:MethodsList,
                       file:FileName,start:StartLine,end:EndLine},
	dbgprintf(parseClass,"parsed Class %w", [Class])
    ) ; ( !,
          printf(warning_output,"BOPL failed to parse class at file %s line %w\n",
                 [FileName,StartLine]),
          flush(warning_output),
          fail )
.

%%% for debugging, parse a string as a class
:- export parStrClass/2.
parStrClass(String,AST) :-
    scanString(String,Tokens),
    parseClass(Tokens,AST,[]), !.


%%%%%%%%%%%%%%%%
%!% Extends    ::= <epsilon> | extends Classexp
parseExtends(Tokens,SuperClass,RestTokens) 
:- Tokens = [tKeyw{word:extends}|TokensAfterExtends],
   !, 
   parseClassExp(TokensAfterExtends,SuperClass,RestTokens).

parseExtends(Tokens,SuperClass,Tokens) :- 
    !,
    SuperClass = pClass(id('Object'),nil,[],[],"*builtin*",0,0)
.


%%%%%%%%%%%%%%%%
%!% Classexp   ::= Int | Bool | Void | Object | id
%% parsing predefined classes
parseClassExp([tKeyw{word:'Int',line:StartLine,file:FileName} | RestTokens],
	      pCexp(id:int,file:FileName,line:StartLine), RestTokens).
parseClassExp([tKeyw{word:'Bool',line:StartLine,file:FileName} | RestTokens],
	      pCexp(id:bool,file:FileName,line:StartLine,file:FileName), RestTokens).
parseClassExp([tKeyw{word:'Void',line:StartLine,file:FileName} | RestTokens],
	      pCexp(id:void,file:FileName,line:StartLine), RestTokens).
parseClassExp([tKeyw{word:'Object',line:StartLine,file:FileName} | RestTokens],
	      pCexp(id:object,file:FileName,line:StartLine), RestTokens).

%% parse user defined classes
parseClassExp([tId{name:Id,line:StartLine,file:FileName} | RestTokens],
	      pCexp(id:Id,file:FileName,line:StartLine), RestTokens).

%%%%%%%%%%%%%%%%
%!% VarList    ::= <epsilon> | vars Vars
%!% Vars       ::= Var | Vars Var
%% parse the optional var list, starting with 'vars'
parseVarsList(Tokens,Vars,RestTokens)
:- 
        Tokens = [tKeyw{word:vars,line:StartLine,file:FileName}|TokensAfterKwVars],
	dbgprintf(parseVarsList," TokensAfterKwVars=%w", [TokensAfterKwVars]),
        ( parseVars(TokensAfterKwVars,Vars,TokensAfterVars),
	  dbgprintf(parseVarsList,"parseVarsList Vars=%w TokensAfterVars=%w",
		 [Vars,TokensAfterVars]),
	  RestTokens = TokensAfterVars,
	  !
	  ;
          printf(warning_output,"BOPL failed to parse vars list at file %s line %w\n",
                 [FileName,StartLine]),
          flush(warning_output),
          !, fail )
.

parseVarsList(Tokens,[],Tokens) :- !.

%%% for debugging, parse a string as a list of vars
:- export parStrVarsList/2.
parStrVarsList(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrVarsList Tokens=%w\n",[Tokens]), !,
    parseVarsList(Tokens,AST,[]), !.

parseVars(Tokens,AllVars,RestTokens) :-
        parseVar(Tokens,VarList,TokensAfterVar1),
        !,
        ( parseVars(TokensAfterVar1,RestVars,TokensAfterVars),
	  append(VarList,RestVars,AllVars),
	  RestTokens = TokensAfterVars;
          AllVars = VarList,
	  RestTokens = TokensAfterVar1)
.


%!% Var        ::= Classexp Ids ;
parseVar(Tokens,VarList,RestTokens) :-
        nonvar(FileName),
        parseClassExp(Tokens,Cexp,TokensAfterCexp),
        atLine(Cexp,Line),
        !,
        (
            parseIds(TokensAfterCexp,IdList,TokensAfterIds),
            TokensAfterIds = [tDelim{cont:semicolon} | RestTokens],
%% we distribute the Cexp to each Id
            (param(Cexp),param(FileName),param(Line),
	     foreach(Id,IdList),foreach(Var,VarList) do
                Var = pVar{cexp:Cexp,id:Id,file:FileName,line:Line})
        ;
            !, 
	printf(warning_output,"BOPL failed to parse vars  at file %s line %w\n",
	       [FileName,Line]),
	flush(warning_output),
	fail 
        )
.

parseLocals(Tokens,Locals,RestTokens) :-
        Tokens = [tKeyw{word:let}|TokensAfterLet],
        parseVars(TokensAfterLet,Locals,TokensAfterVars),
	!,
	dbgprintf(parseLocals,"Locals=%w\n..parseLocals TokensAfterVars=%w",
	       [Locals,TokensAfterVars]),
	TokensAfterVars = [tKeyw{word:in}|RestTokens],
	!
        .

parseLocals(_,Tokens,[],Tokens).

 
% Ids        ::= id | Ids , id
parseIds(Tokens,IdList,RestTokens) :-
        Tokens = [tId{name:Id}|TokensAfterId],
        !,
        ( TokensAfterId = [tDelim{cont:comma}|TokensAfterComma],
          !,
          nonvar(TokensAfterComma),
          parseIds(TokensAfterComma,RestIds,RestTokens),
          IdList = [Id|RestIds]
        ; !, IdList = [Id], TokensAfterId = RestTokens )
        .

%!% MethodList ::= <epsilon> | methods Methods
%!% Methods    ::= Method | Methods Method

parseMethodsList( Tokens, MethodList, RestTokens)
        :-
	    Tokens = [tKeyw{word:methods,line:StartLine,file:FileName}|TokensAfterKMethods],
	    !,
	    (
		dbgprintf(parseMethodsList,"TokensAfterKMethods=%w",[TokensAfterKMethods]),
		parseMethods(TokensAfterKMethods,MethodList,TokensAfterMethods),
		!,
		dbgprintf(parseMethodsList," MethodList=%w\n ..parseMethodsList.. TokensAfterMethods=%w",
		       [MethodList,TokensAfterMethods]),
		RestTokens = TokensAfterMethods,
		!
		;
		(
		  printf(warning_output,"BOPL failed to parse methods at file %s line %w\n",
			 [FileName,StartLine]),
		  flush(warning_output)
		),
		fail 
            )
          . 

parseMethodsList(Tokens,[],Tokens).

%%% notice that the last Method of a MethodsList or of Methods is
%%% followed by the 'end' of the containing class
parseMethods(Tokens,Methods,RestTokens) :-
    dbgprintf(parseMethods,"Tokens=%w", [Tokens]),
    parseMethod(Tokens,Method1,TokensAfterMethod1),
    dbgprintf(parseMethods,"Method1=%w\n ..parseMethods.. TokensAfterMethod1=%w", 
	   [Method1,TokensAfterMethod1]),
    !,
        (
	    TokensAfterMethod1 = [tKeyw{word:end}|_],
            Methods = [Method1], RestTokens = TokensAfterMethod1,
	    !
	    ;
            parseMethods(TokensAfterMethod1,RestMethods,
                         TokensAfterMethods),
	    Methods = [Method1|RestMethods],
	    RestTokens = TokensAfterMethods,
	    !
            % ;
            % Methods = [Method1], RestTokens = TokensAfterMethod1
        )
        .

%!% Method     ::= Classexp id ( FormalList ) Locals Seq
parseMethod(Tokens,Method,RestTokens) :-
    dbgprintf(parseMethod," start Tokens=%w", [Tokens]),
        parseClassExp(Tokens,Cexp,TokensAfterCexp),
        atLine(Cexp,StartLine),
	dbgprintf(parseMethod," Cexp=%w StartLine=%w\n ..parseMethod.. TokensAfterCexp=%w", 
	       [Cexp,StartLine,TokensAfterCexp]),
        TokensAfterCexp = [tId{name:Id,file:FileName}|TokensAfterId],
        TokensAfterId = [tDelim{cont:lparen}|TokensAfterLparen],
	dbgprintf(parseMethod,"Id=%w TokensAfterLparen %w", 
	       [Id,TokensAfterLparen]),
        parseFormalList(TokensAfterLparen,Formals,
                        TokensAfterFormalList),
	dbgprintf(parseMethod," Formals %w\n ..parseMethod TokensAfterFormalList=%w",
	       [Formals,TokensAfterFormalList]),
        TokensAfterFormalList = [tDelim{line:RparenLine,cont:rparen}|TokensAfterRparen],
	dbgprintf(parseMethod,"TokensAfterRparen=%w", [TokensAfterRparen]),
	!,
        ( parseLocals(TokensAfterRparen,Locals,
                      TokensAfterLocals),
          dbgprintf(parseMethod,"Locals=%w TokensAfterLocals=%w",
		 [Locals,TokensAfterLocals]),
          parseSeq(TokensAfterLocals,Seq,RestTokens),
          dbgprintf(parseMethod, "Seq=%w RestTokens=%w",
		 [Seq,RestTokens]),
          atEnd(Seq,EndLine),
	  atFile(Seq,FileName),
          Method = pMethod{id:Id,formals:Formals,cexp:Cexp,
                           locals:Locals,inst:Seq,
                           file:FileName,start:StartLine,end:EndLine},
	  dbgprintf(parseMethod, "Method=%w", [Method])
        ;
          !, 
	printf(warning_output,"BOPL failed to parse method %s at file %s line %w\n",
	       [Id,FileName,RparenLine]),
	flush(warning_output),
	fail 
          )
        .

%%% for debugging, parse a string as a method
:- export parStrMethod/2.
parStrMethod(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrMethod Tokens=%w\n",[Tokens]), !,
    parseMethod(Tokens,AST,[]), !.

%!% FormalList ::= epsilon | Formals
%!% Formals    ::= Formal | Formals , Formal
%!% Formal     ::= Classexp id

parseFormalList(Tokens,Formals,RestTokens) :-
        parseFormals(Tokens,Formals,RestTokens)
        .

parseFormalList(Tokens,[],Tokens).

:- export parStrFormalList/2.
parStrFormalList(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrFormalList Tokens=%w\n",[Tokens]), !,
    parseFormalList(Tokens,AST,[]), !.


parseFormals(Tokens,Formals,RestTokens) :-
    dbgprintf(parseFormals, "start Tokens=%w", [Tokens]),
    parseFormal(Tokens,Formal1,TokensAfterFormal1),
    dbgprintf(parseFormals, "Formal1=%w\n..parseFormals TokensAfterFormal1=%w",
	   [Formal1,TokensAfterFormal1]),
    (
	TokensAfterFormal1 = [tDelim{cont:comma}|TokensAfterComma],
	!,
	dbgprintf(parseFormals, "TokensAfterComma=%w", [TokensAfterComma]),
        parseFormals(TokensAfterComma,RestFormals,
                     TokensAfterFormals),
	!,
        Formals = [Formal1|RestFormals],
	dbgprintf(parseFormals, "bigger Formals=%w\n ..parseFormals TokensAfterFormals=%w", 
	       [Formals,TokensAfterFormals]),
	!,
	RestTokens = TokensAfterFormals
        ;
	Formals = [Formal1],
	dbgprintf(parseFormals,"single Formals=%w\n",  [Formals]),
	!,
	RestTokens = TokensAfterFormal1
    )
        .

parseFormal(Tokens,Formal,RestTokens)
        :-
	    dbgprintf(parseFormal, "start Tokens=%w",[Tokens]),
            parseClassExp(Tokens,Cexp,TokensAfterCexp),
	    dbgprintf(parseFormal, "Cexp=%w\n..parseFormal TokensAfterCexp=%w",[Cexp,TokensAfterCexp]),
            TokensAfterCexp = [tId{line:LineId,name:Id,file:FileName}|RestTokens],
            Formal = pVar{cexp:Cexp,id:Id,file:FileName,line:LineId},
	    dbgprintf(parseFormal,"Formal=%w", [Formal])
        .

%!% Seq        ::= begin Insts end
parseSeq(Tokens,Seq,RestTokens)
        :-
	dbgprintf(parseSeq," start Tokens=%w",[Tokens]),
        Tokens = [FirstToken|NextTokens],
        FirstToken = tKeyw{word:begin,line:FirstLine,file:FileName},
        ( 
          parseInsts(NextTokens,Insts,TokensAfterInst),
	  dbgprintf(parseSeq,"Insts=%w TokensAfterInst=%w",[Insts,TokensAfterInst]),
          TokensAfterInst = [tKeyw{word:end}|RestTokens],
          Seq = Insts,
	  dbgprintf(parseSeq," Seq=%w RestTokens=%w",[Seq,RestTokens])
          ;
          ( !, lexAtLine(FirstToken,FirstLine),
            printf(warning_output,
                   "BOPL failed to parse instruction sequence at file %s line %w\n",
                   [FileName,FirstLine]),
            flush(warning_output),
            fail )
        )
        .


%%% for debugging, parse a string as a Seq
:- export parStrSeq/2.
parStrSeq(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrSeq Tokens=%w\n",[Tokens]), !,
    parseSeq(Tokens,AST,[]), !
.


parseInsts(Tokens,Insts,RestTokens) :-
        parseInst(Tokens,InstLeft,TokensAfterLeft),
        (
            TokensAfterLeft = [tDelim{cont:semicolon} |
                               TokensAfterSemicol],
            !,
            parseInsts(TokensAfterSemicol,RightInsts,
                       RestTokens),
            Insts = [InstLeft|RightInsts]
        ;
            TokensAfterLeft = RestTokens,
            Insts = [InstLeft]
        )
.




%%% for debugging, parse a string as a Inst
:- export parStrInst/2.
parStrInst(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrInst Tokens=%w\n",[Tokens]), !,
    parseInst(Tokens,AST,[]), !
.

%!% Inst       ::= id := Exp | Exp . id := Exp | return Exp |
%!%                if Exp then Seq else Seq | while Exp do Seq |
%!%                writeln ( Exp )

%%% assignment instruction: id := Exp
parseInst(Tokens,Inst,RestTokens)
        :- 
        Tokens = [tId{line:FirstLine,file:FileName,name:Id},
                  tDelim{cont:assign}
                 |TokensAfterAssign],
        ( parseExp(TokensAfterAssign,Exp,TokensAfterAssExp),
          Inst = pAssign{id:Id,exp:Exp,file:FileName,line:FirstLine},
	  dbgprintf(parseInst,"assign Inst=%w TokensAfterAssExp=%w", [Inst,TokensAfterAssExp]),
	  RestTokens = TokensAfterAssExp
        ; !, 
          printf(warning_output,
                 "BOPL failed to parse assignment instruction at"
                 " file %s line %d\n", [FileName,FirstLine]),
          flush(warning_output),
          fail)
        .

%%% writefield instruction: Exp . id := Exp
parseInst(Tokens,Inst,RestTokens)
        :-
	    %% trick, the parsed expression is Exp.id and we recognize it as a field access
        parseExp(Tokens,ExpField,TokensAfterExp1),
	TokensAfterExp1 = [tDelim{cont:assign,line:AssignLine,file:FileName}|TokensAfterAssign],
	dbgprintf(parseInst,"writeField ExpField=%w TokensAfterExp1=%w",[ExpField,TokensAfterExp1]),
	ExpField = pReadField{obj:ExpObj,id:IdField},
	!,
	dbgprintf(parseInst,"writeField ExpObj=%w TokensAfterAssign=%w", [ExpObj,TokensAfterAssign]),
        ( parseExp(TokensAfterAssign,ExpData,TokensAfterExpData),
	  dbgprintf(parseInst,"writeField ExpData=%w TokensAfterExpData=%w",[ExpData,TokensAfterExpData]),
          Inst = pWriteField{obj:ExpObj,id:IdField,val:ExpData,
                             file:FileName,line:AssignLine},
	  dbgprintf(parseInst,"writeField Inst=%w TokensAfterExpData=%w", [Inst,TokensAfterExpData]),
	  RestTokens = TokensAfterExpData
        ; !,
          printf(warning_output,
                 "BOPL failed to parse write field instruction at"
                 " file %s line %d\n", [FileName,AssignLine]),
          flush(warning_output),
          fail)
        .


%%% return instruction: return Exp 
parseInst(Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{line:StartLine,word:return,file:FileName}
                 |TokensAfterReturn],
        ( 
	    dbgprintf(parseInst," return ins. Tokens=%w", [Tokens]),
	    parseExp(TokensAfterReturn,Expr,TokensAfterRetExp),
	    dbgprintf(parseInst,"parsing return ins. Expr=%w TokensAfterRetExp=%w",
		   [Expr,TokensAfterRetExp]),
            Inst = pReturn{exp:Expr,file:FileName,line:StartLine},
	    dbgprintf(parseInst,"parsed return Inst=%w",[Inst]),
	    RestTokens = TokensAfterRetExp
        ;
          !,
          printf(warning_output,
                 "BOPL failed to parse return instruction at"
                 " file %s line %d", [FileName,StartLine]),
          flush(warning_output),
          fail)
        .

%%%% if instruction:  if Exp then Seq else Seq
parseInst(Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{word:if,line:StartLine,file:FileName}
                 |TokensAfterIf],
        ( parseExp(TokensAfterIf,ExprCond,
                    TokensAfterCond),
	  dbgprintf(parseInst,"parse if ExprCond=%w TokensAfterCond=%w",[ExprCond,TokensAfterCond]),
          TokensAfterCond = [tKeyw{line:ThenLine,word:then}
                            |TokensAfterThen],
          (
              parseSeq(TokensAfterThen,SeqThen,
                       TokensAfterThenSeq),
	      !,
	      dbgprintf(parseInst,"parse if SeqThen=%w TokensAfterThenSeq=%w", [SeqThen,TokensAfterThenSeq])
          ; !,
            printf(warning_output,
                   "BOPL failed to parse then sequence at"
                   " file %s line %d", [FileName,ThenLine]),
            flush(warning_output),
            fail),
          TokensAfterThenSeq = [tKeyw{line:ElseLine,word:else}
                               |TokensAfterElse],
          (
              parseSeq(TokensAfterElse,SeqElse,
                       TokensAfterElseSeq),
	      !,
	      dbgprintf(parseInst,"parse if SeqElse=%w TokensAfterElseSeq=%w",[SeqElse,TokensAfterElseSeq])
          ; !,
            printf(warning_output,
                   "BOPL failed to parse else sequence at"
                   " file %s line %d", [FileName|ElseLine]),
            flush(warning_output),
            fail),
          atEnd(SeqElse,EndLine),
          Inst = pIf{exp:ExprCond,seqthen:SeqThen,seqelse:SeqElse,
                     file:FileName,start:StartLine,end:EndLine},
	  dbgprintf(parseInst,"parse if Inst=%w",[Inst]),
          TokensAfterElseSeq = RestTokens
        ; !,
          printf(warning_output, 
                 "BOPL failed to parse if instruction at file %s line"
                 " %d",
                 [FileName,StartLine]),
          flush(warning_output),
          fail)
        .

%%%% while instruction: while Exp do Seq
parseInst(Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{word:while,line:StartLine,file:FileName}
                 |TokensAfterWhile],
        (
            (parseExp(TokensAfterWhile,ExprCond,
                       TokensAfterCond),
	     dbgprintf(parseInst,"parse while ExprCond=%w TokensAfterCond=%w",[ExprCond,TokensAfterCond])
            ; !, 
              printf(warning_output, 
                     "BOPL failed to parse while expression at file %s line"
                     " %d",
                     [FileName,StartLine]),
              flush(warning_output),
              fail),
            TokensAfterCond = [tKeyw{line:DoLine,word:do}
                              | TokensAfterDo],
            (parseSeq(TokensAfterDo,SeqDo,TokensAfterSeq),
	     dbgprintf(parseInst,"parse while SeqDo=%w TokensAfterSeq=%w", [SeqDo,TokensAfterSeq]),
	     !
            ; !,
              printf(warning_output, 
                     "BOPL failed to parse do sequence at file %s line"
                     " %d",
                     [FileName,DoLine]),
              flush(warning_output),
              fail),
            atEnd(SeqDo,EndLine),
            Inst = pWhile{exp:ExprCond,seqdo:SeqDo,
                          file:FileName,start:StartLine,end:EndLine},
	    dbgprintf(parseInst,"parse while Inst=%w", [Inst]),
	    RestTokens = TokensAfterSeq
        )    
        .


%%%% writeln instruction writeln (Exp)

parseInst(Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{word:writeln,line:StartLine,file:FileName}|TokensAfterWriteln],
        (
            TokensAfterWriteln = [tDelim{cont:lparen}|TokensAfterLparen],
            parseExp(TokensAfterLparen,ExprWri,
                       TokensAfterExpr),
            TokensAfterExpr = [tDelim{cont:rparen}|RestTokens],
            Inst = pWriteln{exp:ExprWri,file:FileName,line:StartLine}
            ; !, 
          printf(warning_output, 
                 "BOPL failed to parse writeln instruction at file %s line"
                 " %d",
                 [FileName,StartLine]),
          flush(warning_output),
          fail        
        )
        .

%%%% the original syntax rules
%!% Exp        ::= Exp . id | Exp . id ( ActualList ) | Exp instanceof Classexp |
%!%                Exp + Term | Exp - Term | Exp or Term | Term
%%%% are rewritten as
%!% Exp        ::= Term RestExpr
%!% RestExpr   ::=  . id | . id ( ActualList ) | instanceof Classexp 
%!%                + Expr | - Expr | or Expr | <epsilon>

parseExp(Tokens,Exp,RestTokens) :-
	dbgprintf(parseExp, "start Tokens=%w\n",[Tokens]),
        parseTerm(Tokens,Term,TokensAfterTerm),
	dbgprintf(parseExp, "Term=%w TokensAfterTerm=%w",[Term,TokensAfterTerm]),
        parseRestExp(Term,TokensAfterTerm,Exp,RestTokens)
.

%%% for debugging, parse a string as a Exp
:- export parStrExp/2.
parStrExp(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrExp Tokens=%w\n",[Tokens]), !,
    parseExp(Tokens,AST,[]), !.

parseRestExp(ParExp,[tDelim{cont:period,line:StartLine,file:FileName},
                              tId{name:Id},tDelim{cont:lparen}
                             |TokensAfterLparen],
             Exp,RestTokens) :-
        !,
	dbgprintf(parseRestExp, "methodcall ParExp=%w TokensAfterLparen=%w",[ParExp,TokensAfterLparen]),
        parseActualList(TokensAfterLparen,Args,
                        TokensAfterArgs),
	dbgprintf(parseRestExp, "methodcall Args=%w TokensAfterArgs=%w",[Args,TokensAfterArgs]),
        TokensAfterArgs=[tDelim{cont:rparen}|TokensAfterRparen],
        Exp = pMethodCall{recv:ParExp,id:Id,args:Args,file:FileName,
                          line:StartLine},
	dbgprintf(parseRestExp, "methodcall Exp=%w TokensAfterRparen=%w",[Exp,TokensAfterRparen]),
	RestTokens = TokensAfterRparen
        .

parseRestExp(ParExp,[tDelim{cont:period,line:StartLine,file:FileName},
                              tId{name:Id}|TokensAfterId],
             Exp,RestTokens) :-
    TokensAfterId \= [tDelim(cont:lparen)|_],
    !,
    Exp = pReadField{obj:ParExp,id:Id,file:FileName,line:StartLine},
    RestTokens = TokensAfterId
        .

parseRestExp(ParExp,[tKeyw{word:instanceof,line:Line,file:FileName}
                             |TokensAfterInstanceof],
             Exp,RestTokens) :-
        !,
        parseClassExp(TokensAfterInstanceof,Cexp,RestTokens),
        Exp = pInstanceOf(exp:ParExp,cexp:Cexp,
                          file:FileName,line:Line)
        .

parseRestExp(ParExp,[tDelim{cont:plus,line:StartLine,file:FileName}
                             |TokensAfterPlus],
             Exp,RestTokens) :-
        !,
	dbgprintf(parseRestExp," plus ParExp=%w TokensAfterPlus=%w",
	       [ParExp, TokensAfterPlus]),
        parseExp(TokensAfterPlus,Term,TokensAfterTerm),
        Exp = pPlus(left:ParExp,right:Term,
                    file:FileName,line:StartLine),
	dbgprintf(parseRestExp, "plus Exp=%w TokensAfterTerm=%w",
	       [ParExp, TokensAfterPlus]),
	RestTokens = TokensAfterTerm
        .

parseRestExp(ParExp,[tDelim{cont:minus,line:StartLine,file:FileName}
                             |TokensAfterMinus],
             Exp,RestTokens) :-
        !,
        parseExp(TokensAfterMinus,Term,RestTokens),
        Exp = pMinus(left:ParExp,right:Term,
                    file:FileName,line:StartLine)
        .


parseRestExp(ParExp,[tKeyw{word:or,line:StartLine,file:FileName}
                             |TokensAfterOr],
             Exp,RestTokens) :-
        !,
        parseExp(TokensAfterOr,Term,RestTokens),
        Exp = pOr(left:ParExp,right:Term,
                  file:FileName,line:StartLine)
        .


parseRestExp(_,ParentExp,Tokens,ParentExp,Tokens).

%!% Term       ::= Fact RestTerm
%!% RestTerm   ::= * Fact | and Fact | <epsilon>

parseTerm(Tokens,Term,RestTokens) :-
	dbgprintf(parseTerm, "Tokens=%w\n", [Tokens]),
        parseFact(Tokens,Fact,TokensAfterFact),
	dbgprintf(parseTerm, "Fact=%w TokensAfterFact=%w", 
               [Fact,TokensAfterFact]),
        parseRestTerm(Fact,TokensAfterFact,Term,RestTokens)
.

parseRestTerm(ParTerm,
              [tDelim{cont:times,line:StartLine,file:FileName}
              |TokensAfterMult],
              Term,RestTokens)
        :-
        !,
        parseFact(TokensAfterMult,Fact,RestTokens),
        Term = pTimes{left:ParTerm,right:Fact,
                      file:FileName,line:StartLine}
        .

parseRestTerm(ParTerm,
              [tKeyw{word:and,line:StartLine:file:FileName}
              |TokensAfterAnd],
              Term,RestTokens)
        :-
        !,
        parseFact(TokensAfterAnd,Fact,RestTokens),
        Term = pAnd{left:ParTerm,right:Fact,
                    file:FileName,line:StartLine}
        .

parseRestTerm(_,ParTerm,RestTokens,ParTerm,RestTokens).

% Fact       ::= Fact = Basic | Fact < Basic | Basic

%!%     Fact ::= Basic RestFact
%!%     RestFact := = Fact | < Fact | <epsilon>

parseFact(Tokens,Fact,RestTokens) :-
	dbgprintf(parseFact,"parseFact Tokens=%w\n", [Tokens]),
        parseBasic(Tokens,Basic,TokensAfterBasic),
        parseRestFact(Basic,TokensAfterBasic,Fact,RestTokens)
.

parseRestFact(ParBasic,
              [tDelim{cont:less,line:StartLine,file:FileName}
              |TokensAfterLess],
               Fact,RestTokens)
        :-
        !,
        parseFact(TokensAfterLess,FactRight,RestTokens),
        Fact = pLess(left:ParBasic,right:FactRight,
                     file:FileName,line:StartLine)
        .

parseRestFact(ParBasic,
              [tDelim{cont:equal,line:StartLine:file:FileName}
              |TokensAfterEqual],
               Fact,RestTokens)
        :-
        !,
        parseFact(TokensAfterEqual,FactRight,RestTokens),
        Fact = pEqual(left:ParBasic,right:FactRight,
                     file:FileName,line:StartLine)
        .

parseRestFact(_,ParBasic,RestTokens,ParBasic,RestTokens).

%!% Basic      ::= not Exp | int | id | true | false | nil | self | super |
%!%                new Classexp | ( Exp )

parseBasic(Tokens,Basic,RestTokens)
        :- Tokens = [tKeyw{word:not,line:StartLine,file:FileName}|TokensAfterNot],
        parseExp(TokensAfterNot,ExpNeg,RestTokens),
        Basic = pNot{exp:ExpNeg, file:FileName, line:StartLine}
           .

parseBasic(Tokens,Basic,RestTokens)
        :- Tokens = [tKeyw{word:new,line:StartLine,file:FileName}|TokensAfterNew],
        parseClassExp(TokensAfterNew,Cexp,RestTokens),
        Basic = pNew{cexp:Cexp, file:FileName, line:StartLine}
           .

parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tNum{line:Line,file:FileName,num:N}|RestTokens],
        Basic = pInt{num:N, file:FileName, line:Line}
        .
           
parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tId{line:Line,file:FileName,name:N}|RestTokens],
        Basic = pId{id:N, file:FileName, line:Line}
        .
parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{line:Line,word:true,file:FileName}|RestTokens],
        Basic = pBoolean{bool:true, file:FileName, line:Line}
        .

parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{line:Line,word:nil,file:FileName}|RestTokens],
        Basic = pNil{file:FileName, line:Line}
        .

parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{line:Line,file:FileName,word:self}|RestTokens],
        Basic = pSelf{file:FileName, line:Line}
        .

parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{line:Line,word:super,file:FileName}|RestTokens],
        Basic = pSuper{file:FileName, line:Line}
        .


parseBasic(Tokens,Basic,RestTokens)
        :- 
        Tokens = [tDelim{cont:lparen}|TokensAfterLparen],
	parseExp(TokensAfterLparen,Exp,TokensAfterExp),
	TokensAfterExp = [tDelim{cont:rparen}|RestTokens],
	Basic = Exp
	.
           
%!% ActualList ::= epsilon | Actuals
%!% Actuals    ::= Exp | Actuals , Exp

parseActualList(Tokens,ActList,RestTokens) :-
        parseActuals(Tokens,ActList,RestTokens),
        !.

parseActualList(_,Tokens,[],Tokens).

parseActuals(Tokens,Actuals,RestTokens) :-
        parseExp(Tokens,Exp,TokensAfterExp),
        ( TokensAfterExp = [tDelim{cont:comma},TokensAfterComma],
          parseActuals(TokensAfterComma,OtherActuals,
                       RestTokens),
          !,
          Actuals = [Exp|OtherActuals]
        ;
          TokensAfterExp = RestTokens,
          !,
          Actuals = [Exp]
        )
        .
