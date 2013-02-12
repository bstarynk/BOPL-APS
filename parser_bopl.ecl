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

:- export parseFile/2, parse/3.



%*****************************************************************************
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
    parse(FileName,Tokens,AST),
    !,
    printf(output,"BOPL parsed file %s\n", [FileName]),
    pretty_print(stdout, AST, 80).



% parse(+FileName,+Tokens,-AST)
parse(FileName,Tokens,AST) :- 
    parseProgram(FileName,Tokens,AST,[]), !.

% parseToken(+Read, ?Expected)
parseToken(Read, Expected) :-
  nonvar(Read),
  Read = Expected.

    
     
%%%%%%%%%%%%%%%%
%!% Program    ::= program ClassList Locals Seq
parseProgram(FileName,[Tprog|TokensAfterProg], 
             Program, 
	     []) :-
    parseToken(Tprog,tKeyw{loc:StartLine,word:program}),
    !,
    printf(stdout,"parseProgram TokensAfterProg: %w\n",[TokensAfterProg]),nl,
    parseClassList(FileName,TokensAfterProg,Classes,TokensAfterClassList),
    printf(stdout,"parseProgram got Classes: %w\n..parseProgram TokensAfterClassList= %w\n",[Classes,TokensAfterClassList]),
    !,
    parseLocals(FileName,TokensAfterClassList,Vars,TokensAfterLocals), 
    printf(stdout,"parseProgram local Vars: %w\n..parseProgram  TokensAfterLocals: %w\n",[Vars,TokensAfterLocals]),
    !,
    parseSeq(FileName,TokensAfterLocals,SeqInsts,[]),
    printf(stdout,"parseProgram SeqInsts %w\n",[SeqInsts]),
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
    parseProgram("*string*",Tokens,AST,[]), !.

%%%%%%%%%%%%%%%%
%!% ClassList  ::= <epsilon> | Classes
%!% Classes    ::= Class | Classes Class
parseClassList(FileName,Tokens,Classes,RestTokens)
:- 
    printf(output,"parseClassList Tokens=%w\n", [Tokens]),
    %% a lookahead
    ( Tokens = [tKeyw{word:class}|_]
      ->
	  parseClass(FileName,Tokens,Class1,TokensAfterClass1),
	  printf(output,"parseClassList Class1=%w, TokensAfterClass1=%w\n",
		 [Class1,TokensAfterClass1]),
	  !,
	  parseClassList(FileName,TokensAfterClass1,RestClasses,TokensAfterClassList),
	  Classes = [Class1|RestClasses],
	  RestTokens = TokensAfterClassList,
	  !
      ; 
      Classes = [],
      RestTokens = Tokens,
      !
    ),
    printf(output,"parseClassList Classes=%w\n.. parseClassList RestTokens=%w\n",
	   [Classes,RestTokens])
.

%%% for debugging, parse a string as a class list
:- export parStrClassList/2.
parStrClassList(String,AST) :-
    scanString(String,Tokens),
    parseClassList("*string*",Tokens,AST,[]), !.



%%%%%%%%%%%%%%%%
%!% Class      ::= class id Extends is VarList MethodList end
parseClass(FileName,[tKeyw{word:class,loc:StartLine},tId{name:ClId,loc:_} |TokensAfterClassId],
	   Class,RestTokens)
:- 
        number(StartLine),
        ( 
       printf("parseClass start StartLine=%w ClId=%w"
              " TokensAfterClassId=%w\n",[StartLine,ClId,
                                          TokensAfterClassId]),
       parseExtends(FileName,TokensAfterClassId,SuperClass,
                    TokensAfterExtends),
       printf("parseClass SuperClass=%w  TokensAfterExtends=%w\n", [SuperClass,TokensAfterExtends]),
       TokensAfterExtends = [tKeyw{word:is}|TokensAfterIs],
       printf("parseClass TokensAfterIs=%w\n", [TokensAfterIs]),
       parseVarsList(FileName,TokensAfterIs,VarList,TokensAfterVarsList),
       printf("parseClass VarList=%w\n ..parseClass.. TokensAfterVarsList=%w\n",
              [VarList,TokensAfterVarsList]),nl,
       parseMethodsList(FileName,TokensAfterVarsList,MethodsList,TokensAfterMethodsList),
       printf("parseClass MethodsList=%w\n ..parseClass.. TokensAfterMethodsList=%w\n",
	      [MethodsList, TokensAfterMethodsList]),nl,
       TokensAfterMethodsList = [tKeyw{word:end,loc:EndLine}
                                |RestTokens],
       Class = pClass{id:ClId,cexp:SuperClass,vars:VarList,methods:MethodsList,
                    file:FileName,start:StartLine,end:EndLine},
       printf(stdout,"parsed Class %w\n", [Class]),
       flush(stdout)
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
    parseClass("*string*",Tokens,AST,[]), !.


%%%%%%%%%%%%%%%%
%!% Extends    ::= <epsilon> | extends Classexp
parseExtends(FileName,Tokens,SuperClass,RestTokens) 
:- Tokens = [tKeyw{word:extends}|TokensAfterExtends],
   !, 
   parseClassExp(FileName,TokensAfterExtends,SuperClass,RestTokens).

parseExtends(_FileName,Tokens,SuperClass,Tokens) :- 
    !,
    SuperClass = pClass(id('Object'),nil,[],[],"*builtin*",0,0)
.


%%%%%%%%%%%%%%%%
%!% Classexp   ::= Int | Bool | Void | Object | id
%% parsing predefined classes
parseClassExp(FileName,[tKeyw{word:'Int',loc:StartLine} | RestTokens],
	      pCexp(id:int,file:FileName,line:StartLine), RestTokens).
parseClassExp(FileName,[tKeyw{word:'Bool',loc:StartLine} | RestTokens],
	      pCexp(id:bool,file:FileName,line:StartLine), RestTokens).
parseClassExp(FileName,[tKeyw{word:'Void',loc:StartLine} | RestTokens],
	      pCexp(id:void,file:FileName,line:StartLine), RestTokens).
parseClassExp(FileName,[tKeyw{word:'Object',loc:StartLine} | RestTokens],
	      pCexp(id:object,file:FileName,line:StartLine), RestTokens).

%% parse user defined classes
parseClassExp(FileName,[tId{name:Id,loc:StartLine} | RestTokens],
	      pCexp(id:Id,file:FileName,line:StartLine), RestTokens).

%%%%%%%%%%%%%%%%
%!% VarList    ::= <epsilon> | vars Vars
%!% Vars       ::= Var | Vars Var
%% parse the optional var list, starting with 'vars'
parseVarsList(FileName,Tokens,Vars,RestTokens)
:- 
        Tokens = [tKeyw{word:vars,loc:StartLine}|TokensAfterKwVars],
	printf(output,"parseVarsList TokensAfterKwVars=%w\n", [TokensAfterKwVars]),
        ( parseVars(FileName,TokensAfterKwVars,Vars,TokensAfterVars),
	  printf(output,"parseVarsList Vars=%w TokensAfterVars=%w\n",
		 [Vars,TokensAfterVars]),
	  RestTokens = TokensAfterVars,
	  !
	  ;
          printf(warning_output,"BOPL failed to parse vars list at file %s line %w\n",
                 [FileName,StartLine]),
          flush(warning_output),
          !, fail )
.

parseVarsList(_FileName,Tokens,[],Tokens) :- !.

%%% for debugging, parse a string as a list of vars
:- export parStrVarsList/2.
parStrVarsList(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrVarsList Tokens=%w\n",[Tokens]), !,
    parseVarsList("*string*",Tokens,AST,[]), !.

parseVars(FileName,Tokens,AllVars,RestTokens) :-
        parseVar(FileName,Tokens,VarList,TokensAfterVar1),
        !,
        ( parseVars(FileName,TokensAfterVar1,RestVars,TokensAfterVars),
	  append(VarList,RestVars,AllVars),
	  RestTokens = TokensAfterVars;
          AllVars = VarList,
	  RestTokens = TokensAfterVar1)
.


%!% Var        ::= Classexp Ids ;
parseVar(FileName,Tokens,VarList,RestTokens) :-
        nonvar(FileName),
        parseClassExp(FileName,Tokens,Cexp,TokensAfterCexp),
        atLine(Cexp,Line),
        !,
        (
            parseIds(FileName,TokensAfterCexp,IdList,TokensAfterIds),
            TokensAfterIds = [tDelim{loc:_,cont:semicolon} | RestTokens],
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

parseLocals(FileName,Tokens,Locals,RestTokens) :-
        Tokens = [tKeyw{loc:_,word:let}|TokensAfterLet],
        parseVars(FileName,TokensAfterLet,Locals,TokensAfterVars),
	!,
	printf(output,"parseLocals Locals=%w\n..parseLocals TokensAfterVars=%w\n",
	       [Locals,TokensAfterVars]),
	TokensAfterVars = [tKeyw{word:in}|RestTokens],
	!
        .

parseLocals(_,Tokens,[],Tokens).

 
% Ids        ::= id | Ids , id
parseIds(FileName,Tokens,IdList,RestTokens) :-
        Tokens = [tId{loc:_,name:Id}|TokensAfterId],
        !,
        ( TokensAfterId = [tDelim{loc:_,cont:comma}|TokensAfterComma],
          !,
          nonvar(TokensAfterComma),
          parseIds(FileName,TokensAfterComma,RestIds,RestTokens),
          IdList = [Id|RestIds]
        ; !, IdList = [Id], TokensAfterId = RestTokens )
        .

%!% MethodList ::= <epsilon> | methods Methods
%!% Methods    ::= Method | Methods Method

parseMethodsList(FileName, Tokens, MethodList, RestTokens)
        :-
	    Tokens = [tKeyw{word:methods,loc:StartLine}|TokensAfterKMethods],
	    !,
	    (
		printf(output,"parseMethodsList TokensAfterKMethods=%w\n",[TokensAfterKMethods]),
		parseMethods(FileName,TokensAfterKMethods,MethodList,TokensAfterMethods),
		!,
		printf(output,"parseMethodsList MethodList=%w\n ..parseMethodsList.. TokensAfterMethods=%w\n",
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

parseMethodsList(_FileName,Tokens,[],Tokens).

%%% notice that the last Method of a MethodsList or of Methods is
%%% followed by the 'end' of the containing class
parseMethods(FileName,Tokens,Methods,RestTokens) :-
    printf(output,"parseMethods Tokens=%w\n", [Tokens]),
    parseMethod(FileName,Tokens,Method1,TokensAfterMethod1),
    printf(output,"parseMethods Method1=%w\n ..parseMethods.. TokensAfterMethod1=%w\n", 
	   [Method1,TokensAfterMethod1]),
    !,
        (
	    TokensAfterMethod1 = [tKeyw{loc:_,word:end}|_],
            Methods = [Method1], RestTokens = TokensAfterMethod1,
	    !
	    ;
            parseMethods(FileName,TokensAfterMethod1,RestMethods,
                         TokensAfterMethods),
	    Methods = [Method1|RestMethods],
	    RestTokens = TokensAfterMethods,
	    !
            % ;
            % Methods = [Method1], RestTokens = TokensAfterMethod1
        )
        .

%!% Method     ::= Classexp id ( FormalList ) Locals Seq
parseMethod(FileName,Tokens,Method,RestTokens) :-
    printf(output,"parseMethod start Tokens=%w\n", [Tokens]),
        parseClassExp(FileName,Tokens,Cexp,TokensAfterCexp),
        atLine(Cexp,StartLine),
	printf(output,"parseMethod Cexp=%w StartLine=%w\n ..parseMethod.. TokensAfterCexp=%w\n", 
	       [Cexp,StartLine,TokensAfterCexp]),
        TokensAfterCexp = [tId{name:Id}|TokensAfterId],
        TokensAfterId = [tDelim{cont:lparen}|TokensAfterLparen],
	printf(output,"parseMethod Id=%w TokensAfterLparen %w\n", 
	       [Id,TokensAfterLparen]),
        parseFormalList(FileName,TokensAfterLparen,Formals,
                        TokensAfterFormalList),
	printf(output,"parseMethod Formals %w\n ..parseMethod TokensAfterFormalList=%w\n",
	       [Formals,TokensAfterFormalList]),
        TokensAfterFormalList = [tDelim{loc:RparenLine,cont:rparen}|TokensAfterRparen],
	printf(output,"parseMethod TokensAfterRparen=%w\n", [TokensAfterRparen]),
	!,
        ( parseLocals(FileName,TokensAfterRparen,Locals,
                      TokensAfterLocals),
          printf(output,"parseMethod Locals=%w TokensAfterLocals=%w\n",
		 [Locals,TokensAfterLocals]),
          parseSeq(FileName,TokensAfterLocals,Seq,RestTokens),
          printf(output,"parseMethod Seq=%w RestTokens=%w\n",
		 [Seq,RestTokens]),
          atEnd(Seq,EndLine),
          Method = pMethod{id:Id,formals:Formals,cexp:Cexp,
                           locals:Locals,inst:Seq,
                           file:FileName,start:StartLine,end:EndLine},
	  printf(output,"parseMethod Method=%w\n", [Method])
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
    parseMethod("*string*",Tokens,AST,[]), !.

%!% FormalList ::= epsilon | Formals
%!% Formals    ::= Formal | Formals , Formal
%!% Formal     ::= Classexp id

parseFormalList(FileName,Tokens,Formals,RestTokens) :-
        parseFormals(FileName,Tokens,Formals,RestTokens)
        .

parseFormalList(_FileName,Tokens,[],Tokens).

:- export parStrFormalList/2.
parStrFormalList(String,AST) :-
    scanString(String,Tokens),
    printf(output,"parStrFormalList Tokens=%w\n",[Tokens]), !,
    parseFormalList("*string*",Tokens,AST,[]), !.


parseFormals(FileName,Tokens,Formals,RestTokens) :-
    printf(output,"parseFormals start Tokens=%w\n", [Tokens]),
    parseFormal(FileName,Tokens,Formal1,TokensAfterFormal1),
    printf(output,"parseFormals Formal1=%w\n..parseFormals TokensAfterFormal1=%w\n",
	   [Formal1,TokensAfterFormal1]),
    (
	TokensAfterFormal1 = [tDelim{cont:comma}|TokensAfterComma],
	!,
	printf(output,"parseFormals TokensAfterComma=%w\n", [TokensAfterComma]),
        parseFormals(FileName,TokensAfterComma,RestFormals,
                     TokensAfterFormals),
	!,
        Formals = [Formal1|RestFormals],
	printf(output,"parseFormals bigger Formals=%w\n ..parseFormals TokensAfterFormals=%w", 
	       [Formals,TokensAfterFormals]),
	!,
	RestTokens = TokensAfterFormals
        ;
	Formals = [Formal1],
	printf(output,"parseFormals single Formals=%w\n",  [Formals]),
	!,
	RestTokens = TokensAfterFormal1
    )
        .

parseFormal(FileName,Tokens,Formal,RestTokens)
        :-
	    printf(output,"parseFormal start Tokens=%w\n",[Tokens]),
            parseClassExp(FileName,Tokens,Cexp,TokensAfterCexp),
	    printf(output,"parseFormal Cexp=%w\n..parseFormal TokensAfterCexp=%w\n",[Cexp,TokensAfterCexp]),
            TokensAfterCexp = [tId{loc:LineId,name:Id}|RestTokens],
            Formal = pVar{cexp:Cexp,id:Id,file:FileName,line:LineId},
	    printf(output,"parseFormal Formal=%w\n", [Formal])
        .

%!% Seq        ::= begin Insts end
parseSeq(FileName,Tokens,Seq,RestTokens)
        :-
	printf(output,"parseSeq start Tokens=%w\n",[Tokens]),
        Tokens = [FirstToken|NextTokens],
        FirstToken = tKeyw{word:begin,loc:FirstLine},
        ( 
          parseInsts(FileName,NextTokens,Insts,TokensAfterInst),
          TokensAfterInst = [tKeyw{word:end,loc:_EndLine}|RestTokens],
          Seq = Insts,
          print(stdout,"parsed Seq");
          pretty_print(stdout,Seq,80),
          flush(stdout)
          ;
          ( !, lexAt(FirstToken,FirstLine),
            printf(warning_output,
                   "BOPL failed to parse instruction sequence at file %s line %w\n",
                   [FileName,FirstLine]),
            flush(warning_output),
            fail )
        )
        .

parseInsts(FileName,Tokens,Insts,RestTokens) :-
        parseInst(FileName,Tokens,InstLeft,TokensAfterLeft),
        (
            TokensAfterLeft = [tDelim{loc:_,cont:semicolon} |
                               TokensAfterSemicol],
            !,
            parseInsts(FileName,TokensAfterSemicol,RightInsts,
                       RestTokens),
            Insts = [InstLeft|RightInsts]
        ;
            TokensAfterLeft = RestTokens,
            Insts = [InstLeft]
        )
.

%!% Inst       ::= id := Exp | Exp . id := Exp | return Exp |
%!%                if Exp then Seq else Seq | while Exp do Seq |
%!%                writeln ( Exp )

%%% assignment instruction: id := Exp
parseInst(FileName,Tokens,Inst,RestToken)
        :- 
        Tokens = [tId{loc:FirstLine,name:Id},
                  tDelim{cont:assign}
                 |TokensAfterAssign],
        ( parseExp(FileName,TokensAfterAssign,Exp,RestToken),
          Inst = pAssign{id:Id,exp:Exp,file:FileName,line:FirstLine}
        ; !, 
          printf(warning_output,
                 "BOPL failed to parse assignment instruction at"
                 " file %s line %d", [FileName,FirstLine]),
          flush(warning_output),
          fail)
        .

%%% writefield instruction: Exp . id := Exp
parseInst(FileName,Tokens,Inst,RestTokens)
        :-
        parseExp(FileName,Tokens,ExpObj,TokensAfterExp1),
        TokensAfterExp1 = [tDelim{loc:DotLine,cont:dot},
                           tId{name:IdField},
                           tDelim{cont:assign}
                          |TokensAfterAssign],
        ( parseExp(FileName,TokensAfterAssign,ExpField,RestTokens),
          Inst = pWriteField{obj:ExpObj,id:IdField,val:ExpField,
                             file:FileName,line:DotLine}
        ; !,
          printf(warning_output,
                 "BOPL failed to parse write field instruction at"
                 " file %s line %d", [FileName,DotLine]),
          flush(warning_output),
          fail)
        .


%%% return instruction: return Exp 
parseInst(FileName,Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{loc:StartLine,word:return}
                 |TokensAfterReturn],
        ( parseExp(FileName,TokensAfterReturn,Expr,RestTokens),
          Inst = pReturn{exp:Expr,file:FileName,line:StartLine}
        ;
          !,
          printf(warning_output,
                 "BOPL failed to parse return instruction at"
                 " file %s line %d", [FileName,StartLine]),
          flush(warning_output),
          fail)
        .

%%%% if instruction:  if Exp then Seq else Seq
parseInst(FileName,Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{loc:StartLine,word:if}
                 |TokensAfterIf],
        ( parseExp(FileName,TokensAfterIf,ExprCond,
                    TokensAfterCond),
          TokensAfterCond = [tKeyw{loc:ThenLine,word:then}
                            |TokensAfterThen],
          (
              parseSeq(FileName,TokensAfterThen,SeqThen,
                       TokensAfterThenSeq)
          ; !,
            printf(warning_output,
                   "BOPL failed to parse then sequence at"
                   " file %s line %d", [FileName,ThenLine]),
            flush(warning_output),
            fail),
          TokensAfterThenSeq = [tKeyw{loc:ElseLine,word:else}
                               |TokensAfterElse],
          (
              parseSeq(FileName,TokensAfterElse,SeqElse,
                       TokensAfterElseSeq)
          ; !,
            printf(warning_output,
                   "BOPL failed to parse else sequence at"
                   " file %s line %d", [FileName|ElseLine]),
            flush(warning_output),
            fail),
          atEnd(SeqElse,EndLine),
          TokensAfterElseSeq = RestTokens,
          Inst = pIf{exp:ExprCond,seqthen:SeqThen,seqelse:SeqElse,
                     file:FileName,start:StartLine,end:EndLine}
        ; !,
          printf(warning_output, 
                 "BOPL failed to parse if instruction at file %s line"
                 " %d",
                 [FileName,StartLine]),
          flush(warning_output),
          fail)
        .

%%%% while instruction: while Exp do Seq
parseInst(FileName,Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{loc:StartLine,word:while}
                 |TokensAfterWhile],
        (
            (parseExp(FileName,TokensAfterWhile,ExprCond,
                       TokensAfterCond)
            ; !, 
              printf(warning_output, 
                     "BOPL failed to parse while expression at file %s line"
                     " %d",
                     [FileName,StartLine]),
              flush(warning_output),
              fail),
            TokensAfterCond = [tKeyw{loc:DoLine,word:do}
                              | TokensAfterDo],
            (parseSeq(FileName,TokensAfterDo,SeqDo,
                      RestTokens)
            ; !,
              printf(warning_output, 
                     "BOPL failed to parse do sequence at file %s line"
                     " %d",
                     [FileName,DoLine]),
              flush(warning_output),
              fail),
            atEnd(SeqDo,EndLine),
            Inst = pWhile{exp:ExprCond,seqdo:SeqDo,
                          file:FileName,start:StartLine,end:EndLine}
        )    
        .


%%%% writeln instruction writeln (Exp)

parseInst(FileName,Tokens,Inst,RestTokens)
        :- 
        Tokens = [tKeyw{loc:StartLine,word:writeln}|TokensAfterWriteln],
        (
            TokensAfterWriteln = [tDelim{cont:lparen}|TokensAfterLparen],
            parseExp(FileName,TokensAfterLparen,ExprWri,
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

parseExp(FileName,Tokens,Exp,RestTokens) :-
        parseTerm(FileName,Tokens,Term,TokensAfterTerm),
        parseRestExp(FileName,Term,TokensAfterTerm,Exp,RestTokens)
.

parseRestExp(FileName,ParExp,[tDelim{cont:dot,loc:StartLine},
                              tId{name:Id},tDelim(cont:lparen)
                             |TokensAfterLparen],
             Exp,RestTokens) :-
        !,
        parseActualList(FileName,TokensAfterLparen,Args,
                        TokensAfterArgs),
        TokensAfterArgs=[tDelim(cont:rparen)|RestTokens],
        Exp = pMethodCall{recv:ParExp,id:Id,args:Args,file:FileName,
                          line:StartLine}
        .

parseRestExp(FileName,ParExp,[tDelim{cont:dot,loc:StartLine},
                              tId{name:Id}|RestTokens],
             Exp,RestTokens) :-
        !,
        Exp = pReadField{obj:ParExp,id:Id,file:FileName,line:StartLine}
        .

parseRestExp(FileName,ParExp,[tKeyw{loc:Line,word:instanceof}
                             |TokensAfterInstanceof],
             Exp,RestTokens) :-
        !,
        parseClassExp(FileName,TokensAfterInstanceof,Cexp,RestTokens),
        Exp = pInstanceOf(exp:ParExp,cexp:Cexp,
                          file:FileName,line:Line)
        .

parseRestExp(FileName,ParExp,[tDelim{cont:plus,loc:StartLine}
                             |TokensAfterPlus],
             Exp,RestTokens) :-
        !,
        parseExp(FileName,TokensAfterPlus,Term,RestTokens),
        Exp = pPlus(left:ParExp,right:Term,
                    file:FileName,line:StartLine)
        .

parseRestExp(FileName,ParExp,[tDelim{cont:minus,loc:StartLine}
                             |TokensAfterMinus],
             Exp,RestTokens) :-
        !,
        parseExp(FileName,TokensAfterMinus,Term,RestTokens),
        Exp = pMinus(left:ParExp,right:Term,
                    file:FileName,line:StartLine)
        .


parseRestExp(FileName,ParExp,[tKeyw{word:or,loc:StartLine}
                             |TokensAfterOr],
             Exp,RestTokens) :-
        !,
        parseExp(FileName,TokensAfterOr,Term,RestTokens),
        Exp = pOr(left:ParExp,right:Term,
                  file:FileName,line:StartLine)
        .


parseRestExp(_,ParentExp,Tokens,ParentExp,Tokens).

%!% Term       ::= Fact RestTerm
%!% RestTerm   ::= * Fact | and Fact | <epsilon>

parseTerm(FileName,Tokens,Term,RestTokens) :-
        parseFact(FileName,Tokens,Fact,TokensAfterFact),
        parseRestTerm(FileName,Fact,TokensAfterFact,Term,RestTokens)
.

parseRestTerm(FileName,ParTerm,
              [tDelim{cont:mult,loc:StartLine}
              |TokensAfterMult],
              Term,RestTokens)
        :-
        !,
        parseFact(FileName,TokensAfterMult,Fact,RestTokens),
        Term = pTimes{left:ParTerm,right:Fact,
                      file:FileName,line:StartLine}
        .

parseRestTerm(FileName,ParTerm,
              [tKeyw{word:and,loc:StartLine}
              |TokensAfterAnd],
              Term,RestTokens)
        :-
        !,
        parseFact(FileName,TokensAfterAnd,Fact,RestTokens),
        Term = pAnd{left:ParTerm,right:Fact,
                    file:FileName,line:StartLine}
        .

parseRestTerm(_,ParTerm,RestTokens,ParTerm,RestTokens).

% Fact       ::= Fact = Basic | Fact < Basic | Basic

%!%     Fact ::= Basic RestFact
%!%     RestFact := = Fact | < Fact | <epsilon>

parseFact(FileName,Tokens,Fact,RestTokens) :-
        parseBasic(FileName,Tokens,Basic,TokensAfterBasic),
        parseRestFact(FileName,Basic,TokensAfterBasic,Fact,RestTokens)
.

parseRestFact(FileName,ParBasic,
              [tDelim{cont:less,loc:StartLine}
              |TokensAfterLess],
               Fact,RestTokens)
        :-
        !,
        parseFact(FileName,TokensAfterLess,FactRight,RestTokens),
        Fact = pLess(left:ParBasic,right:FactRight,
                     file:FileName,line:StartLine)
        .

parseRestFact(FileName,ParBasic,
              [tDelim{cont:equal,loc:StartLine}
              |TokensAfterEqual],
               Fact,RestTokens)
        :-
        !,
        parseFact(FileName,TokensAfterEqual,FactRight,RestTokens),
        Fact = pEqual(left:ParBasic,right:FactRight,
                     file:FileName,line:StartLine)
        .

parseRestFact(_,ParBasic,RestTokens,ParBasic,RestTokens).

%!% Basic      ::= not Exp | int | id | true | false | nil | self | super |
%!%                new Classexp | ( Exp )

parseBasic(FileName,Tokens,Basic,RestTokens)
        :- Tokens = [tKeyw{word:not,loc:StartLine}|TokensAfterNot],
        parseExp(FileName,TokensAfterNot,ExpNeg,RestTokens),
        Basic = pNot{exp:ExpNeg, file:FileName, line:StartLine}
           .

parseBasic(FileName,Tokens,Basic,RestTokens)
        :- Tokens = [tKeyw{word:new,loc:StartLine}|TokensAfterNew],
        parseClassExp(FileName,TokensAfterNew,Cexp,RestTokens),
        Basic = pNew{cexp:Cexp, file:FileName, line:StartLine}
           .

parseBasic(FileName,Tokens,Basic,RestTokens)
        :- 
        Tokens = [tNum{loc:Line,num:N}|RestTokens],
        Basic = pInt{num:N, file:FileName, line:Line}
        .
           
parseBasic(FileName,Tokens,Basic,RestTokens)
        :- 
        Tokens = [tId{loc:Line,name:N}|RestTokens],
        Basic = pId{id:N, file:FileName, line:Line}
        .
parseBasic(FileName,Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{loc:Line,word:true}|RestTokens],
        Basic = pBoolean{bool:true, file:FileName, line:Line}
        .

parseBasic(FileName,Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{loc:Line,word:nil}|RestTokens],
        Basic = pNil{file:FileName, line:Line}
        .

parseBasic(FileName,Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{loc:Line,word:self}|RestTokens],
        Basic = pSelf{file:FileName, line:Line}
        .

parseBasic(FileName,Tokens,Basic,RestTokens)
        :- 
        Tokens = [tKeyw{loc:Line,word:super}|RestTokens],
        Basic = pSuper{file:FileName, line:Line}
        .

           
%!% ActualList ::= epsilon | Actuals
%!% Actuals    ::= Exp | Actuals , Exp

parseActualList(FileName,Tokens,ActList,RestTokens) :-
        parseActuals(FileName,Tokens,ActList,RestTokens),
        !.

parseActualList(_,Tokens,[],Tokens).

parseActuals(FileName,Tokens,Actuals,RestTokens) :-
        parseExp(FileName,Tokens,Exp,TokensAfterExp),
        ( TokensAfterExp = [tDelim{cont:comma},TokensAfterComma],
          parseActuals(FileName,TokensAfterComma,OtherActuals,
                       RestTokens),
          !,
          Actuals = [Exp|OtherActuals]
        ;
          TokensAfterExp = RestTokens,
          !,
          Actuals = [Exp]
        )
        .
%% incomplete
