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
	     pProgram(Class,_Vars,SeqInsts,FileName,StartLine,EndLine), 
	     [])
:-
    parseToken(Tprog,tKeyw{loc:StartLine,word:program}),
    parseClassList(FileName, TokensAfterProg, Classes, TokensAfterClassList),
    parseLocals(FileName,TokensAfterClassList, Vars, TokensAfterLocals), 
    parseSeq(FileName,TokensAfterLocals,SeqInsts,[])
.


%%%%%%%%%%%%%%%%
%!% ClassList  ::= <epsilon> | Classes
%!% Classes    ::= Class | Classes Class
parseClassList(FileName,Tokens,[Class|RestClasses],TokenAfterClassList)
:- Tokens = [tKeyw{word:class}|_],
   parseClass(FileName,Tokens,Class,TokenAfterClass),
   parseClassList(FileName,TokenAfterClass,RestClasses,TokenAfterClassList)
.

parseClassList(FileName,Tokens,[],Tokens).



%%%%%%%%%%%%%%%%
%!% Class      ::= class id Extends is VarList MethodList end
parseClass(FileName,[tKeyw{word:class,loc:StartLine} | RestTokens],
	   Class,TokensAfterClass)
:- ( RestTokens = [tId{name:ClId,loc:_}|TokensAfterClassId],
     parseExtends(FileName,TokensAfterClassId, SuperClass, TokensAfterExtends),
     TokensAfterExtends = [tKeyw{word:id}|TokensAfterIs],
     parseVarsList(FileName,TokensAfterIs,VarList,TokensAfterVarList),
     parseMethodsList(FileName,TokensAfterVarList,MethodsList,TokensAfterMethodsList),
     TokensAfterMethodsList = [tKeyw{word:end,loc:EndLine}|TokensAfterClass]
    ) ; ( !,
	  printf(warning_output,"BOPL failed to parse class at file %s line %d\n",
		 [FileName,StartLine]),
	  flush(warning_output),
	  fail )
.

%%%%%%%%%%%%%%%%
%!% Extends    ::= <epsilon> | extends Classexp
parseExtends(FileName,[tKeyw{word:extends}|TokensAfterExtends],SuperClass,RestTokens) 
:- !, parseClassExp(FileName,TokensAfterExtends,SuperClass,RestTokens).

parseExtends(FileName,Tokens,pClass(id('Object'),nil,[],[],"*builtin*",0,0),
	     Tokens).


%%%%%%%%%%%%%%%%
%!% Classexp   ::= Int | Bool | Void | Object | id
%% parsing predefined classes
parseClassExp(FileName,[tId{name:'Int',loc:StartLine} | RestTokens],
	      pCexp(id:int,file:FileName,line:StartLine), RestTokens).
parseClassExp(FileName,[tId{name:'Bool',loc:StartLine} | RestTokens],
	      pCexp(id:bool,file:FileName,line:StartLine), RestTokens).
parseClassExp(FileName,[tId{name:'Void',loc:StartLine} | RestTokens],
	      pCexp(id:void,file:FileName,line:StartLine), RestTokens).
parseClassExp(FileName,[tId{name:'Object',loc:StartLine} | RestTokens],
	      pCexp(id:object,file:FileName,line:StartLine), RestTokens).

%% parse user defined classes
parseClassExp(FileName,[tId{name:Id,loc:StartLine} | RestTokens],
	      pCexp(id:Id,file:FileName,line:StartLine), RestTokens).

%%%%%%%%%%%%%%%%
%!% VarList    ::= <epsilon> | vars Vars
%!% Vars       ::= Var | Vars Var
%% parse the optional var list, starting with 'vars'
parseVarsList(FileName,Tokens,VarList,RestTokens)
:- 
    Tokens = [tKeyw{word:vars,loc:StartLine}|TokensAfterVars],
    ( parseVars(FileName,TokensAfterVars,Vars,RestTokens) ;
      ( !, 
	printf(warning_output,"BOPL failed to parse vars list at file %s line %d\n",
	       [FileName,StartLine]),
	flush(warning_output),
	fail ))
.

parseVarsList(_FileName,Tokens,[],Tokens).

parseVars(FileName,Tokens,[Var|RestVars],RestTokens) :-
        parseVar(FileName,Tokens,Var,TokensAfterVar),
        !,
        ( parseVars(FileName,TokensAfterVar,RestVars,RestTokens);
          RestVars = [] )
.


%!% Var        ::= Classexp Ids ;
parseVar(FileName,Tokens,VarList,RestTokens) :-
        parseClassExp(FileName,Tokens,Cexp,TokensAfterCexp),
        atLine(Cexp,Line),
        nonvar(FileName),
        (
            parseIds(FileName,TokensAfterCexp,IdList,TokensAfterIds),
            TokensAfterIds = [tDelim{loc:_,cont:semicolon} | RestTokens],
%% we distribute the Cexp to each Id
            (foreach(Id,IdList),foreach(Var,VarList),param(Cexp),param(FileName),param(Line) do
                Var = pVar{cexp:Cexp,id:Id,file:FileName,line:Line})
        ;
            !, 
	printf(warning_output,"BOPL failed to parse vars  at file %s line %d\n",
	       [FileName,Line]),
	flush(warning_output),
	fail 
        )
.

% Ids        ::= id | Ids , id
parseIds(FileName,Tokens,IdList,RestTokens) :-
        Tokens = [tId{loc:_,name:Id}|TokensAfterId],
        IdList = [Id|RestIds],
        ( TokensAfterId = [tDelim{loc:_,cont:comma}|TokensAfterComma],
          parseIds(FileName,TokensAfterComma,RestIds,RestTokens)
        ; !, RestIds = [], TokensAfterId = RestTokens )
        .

%!% MethodList ::= <epsilon> | methods Methods
%!% Methods    ::= Method | Methods Method

parseMethodList(FileName,Tokens,[],Tokens).
parseMethodList(FileName, 
                [tKeyw{word:methods,loc:StartLine}|TokensAfterMethods],
                MethodList,RestTokens)
        :-
        parseMethods(FileName,TokensAfterMethods,MethodList,RestToken)
        ;
        (!, 
	printf(warning_output,"BOPL failed to parse methods at file %s line %d\n",
	       [FileName,StartLine]),
	flush(warning_output),
	fail 
         )
          . 

parseMethods(FileName,Tokens,[Method|RestMethods],RestTokens) :-
        parseMethod(FileName,Tokens,Method,TokensAfterMethod),
        (
            parseMethods(FileName,TokensAfterMethod,RestMethod,
                         RestToken)
        ;
            RestMethods = [], TokensAfterMethod = RestTokens
        )
        .

%!% Method     ::= Classexp id ( FormalList ) Locals Seq
parseMethod(FileName,Tokens,Method,RestTokens) :-
        parseClassExp(FileName,Tokens,Cexp,TokensAfterCexp),
        TokensAfterCexp = [tId(loc:_,name:Id)|TokensAfterId],
        TokensAfterId = [tDelim(loc:_,cont:lparen)|TokensAfterComma],
        parseFormalList(FileName,TokensAfterComma,Formals,
                        TokensAfterFormalList),
        TokensAfterFormalList = [tDelim(loc:RparenLine,cont:rparen)|TokensAfterRparen],
        ( parseLocals(FileName,TokensAfterRparen,Locals,
                      TokensAfterLocals),
          parseSeq(FileName,TokensAfterLocals,Seq,RestTokens)
        ;
          !, 
	printf(warning_output,"BOPL failed to parse method %s at file %s line %d\n",
	       [Id,FileName,RparenLine]),
	flush(warning_output),
	fail 
          )
        .


%!% FormalList ::= epsilon | Formals
%!% Formals    ::= Formal | Formals , Formal
%!% Formal     ::= Classexp id

parseFormalList(FileName,Tokens,Formals,RestTokens) :-
        parseFormals(FileName,Tokens,Formals,RestTokens)
        .

parseFormalList(FileName,Tokens,[],Tokens).

parseFormals(FileName,Tokens,Formals,RestTokens) :-
        parseFormal(FileName,Tokens,Formal,TokenAfterFormals),
        (TokenAfterFomals = [tDelim{loc:_,cont:comma}|TokensAfterComma],
         parseFormals(FileName,TokensAfterComma,RestFormals,
                      RestTokens),
         Formals=[Formal|RestFormals]
        ;
         !, Formals = [Formal], RestTokens = TokenAfterFormals
        )
        .

parseFormal(FileName,Tokens,Formal,RestTokens)
        :-
        parseClassExp(FileName,Tokens,Cexp,TokensAfterCexp),
        TokensAfterExp = [tId{loc:LineId,name:Id}|RestTokens],
        Formal = pVar{cexp:Cexp,id:Id,file:FileName,line:LineId}
        .

%!% Seq        ::= begin Insts end
parseSeq(FileName,Tokens,Seq,RestTokens)
        :-
        Tokens = [FirstToken|NextTokens],
        ( FirstToken = tKeyw{word:begin,loc:StartLine},
          parseInsts(FileName,NextTokens,Insts,TokensAfterInst),
          TokensAfterInst = [tKeyw{word:end,loc:EndLine}],
          Seq = Insts
          ;
          ( !, lexAt(FirstToken,FirstLine),
            printf(warning_output,
                   "BOPL failed to parse instruction sequence at file %s line %d\n",
                   [FileName,FirstLine]),
            flush(warning_output),
            fail )
        )
        .
%% incomplete
