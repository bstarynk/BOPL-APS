%%%%%%%%%%%%%%%%%%%%%%%%%%% The BOPL lexer; file lexer_bopl.ecl
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

%% Inspired by the scanner.ecl file from Jacques.Malenfant@lip6.fr
%% [Re-]written by Basile Starynkevitch   basile@starynkevitch.net
%% To be compiled by ECLiPSe-CLP Prolog, see http://eclipseclp.org/

:- module(lexer_bopl).

:- use_module(library(pretty_print)).
:- export scanFile/2,scan/2,scanString/2,lexWith/2,lexDelim/2,lexKeyw/2,lexId/2,lexNum/2,lexAtLine/2,lexInFile/2.


%% scanFile(+FileName, -Tokens)
scanFile(FileName, Tokens) :-
  scan(FileName, Tokens),
  pretty_print(stdout, Tokens, 80).


scan(FileName, Tokens) :-
  open(FileName, read, IS),
  get(IS, Char),
  (scanTokens(FileName, IS, Char, 1, Tokens, NbLines);
   (!,printf(warning_output, "BOPL failed to lex file %s\n",[FileName]),fail)),
  length(Tokens, NbTokens),
  !,
  printf(output,"BOPL lexed file %s of %d lines and %d tokens\n", [FileName, NbLines, NbTokens]),
  close(IS).


scanString(String,Tokens) :-
  open(string(String), read, IS),
  get(IS, Char),
  (scanTokens("*string*", IS, Char, 1, Tokens, NbLines);
   (!,printf(warning_output, "BOPL failed to lex string %w\n",[String]),fail)),
  length(Tokens, NbTokens),
  !,
  printf(output,"BOPL lexed string %s of %d lines and %d tokens\n", [String, NbLines, NbTokens]),
  close(IS).

%% we use the structure notation for tokens, because we want to carry
%% the line location of each lexical token.  See chapter 4 of the
%% ECLiPSe tutorial.

%%% tokens with location for delimiters (like period, lparen, ...)
:- export struct(tDelim(line,file,cont)).
%% so code tDelim{cont:C} or tDelim{line:L,file:F,cont:C} or tDelim(L,F,C)

%%% tokens with location for key words or reserved words
:- export struct(tKeyw(line,file,word)).

%%% tokens with location for identifiers
:- export struct(tId(line,file,name)).

%%% token with location for numbers
:- export struct(tNum(line,file,num)).

lexAtLine(tDelim{line:L},L).
lexAtLine(tId{line:L},L).
lexAtLine(tKeyw{line:L},L).
lexAtLine(tNum{line:L},L).

lexInFile(tDelim{file:F},F).
lexInFile(tId{file:F},F).
lexInFile(tKeyw{file:F},F).
lexInFile(tNum{file:F},F).

lexDelim(tDelim{cont:C},C).
lexId(tId{name:N},N).
lexKeyw(tKeyw{word:C},C).
lexNum(tNum{num:N},N).
lexWith(tDelim{cont:C},C).
lexWith(tId{name:N},N).
lexWith(tKeyw{word:C},C).
lexWith(tNum{num:N},N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% scan and acumulate a sequence of tokens
%% scanTokens(+InputStream,+Char,+Lineno,-Tokens,-NextLineno)
scanTokens(_,_, C, Lineno, [], Lineno) :-
  isEos(C),
  !.
scanTokens(FileName, IS, C, Lineno, Tokens, NextLineno) :-
  scanToken(FileName, IS, C, Lineno, Token, NextC, SuccLineno),
  (Token = eof ->
     Tokens = RestTokens, NextLineno = SuccLineno
  ;  Tokens = [Token | RestTokens]
  ),
  scanTokens(FileName, IS, NextC, SuccLineno, RestTokens, NextLineno).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Character codes and test. Assume an ASCII input file, don't give any UTF-8!
isDigit(C) :- C >= 48, C =< 57.
isLower(C) :- C >= 97, C =< 122.
isUpper(C) :- C >= 65, C =< 90.
isEos(C) :- endfile(C).

endfile(26).
endfile(-1).
endline(10).
space(32).
tab(9).
period(46).
lparen(40).
rparen(41).
times(42).
plus(43).
comma(44).
minus(45).
colon(58).
semicolon(59).
hashcomment(35). %% the ascii # for comments up to end of line
less(60).
equal(61).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% main lexing predicate scanToken(+IS, +C, +Lineno, -Token, -NextC, -NextLineno) 
%% +IS is the input stream
%% +C is the current character
%% +Lineno is the current line number
%% -Token is the lexed token
%% -NextC is the next character
%% -NextLineno is the next line number.

scanToken(FileName,IS, C, Lineno, Token, NextC, NextLineno) :-
    %%% digits are numbers
    ( isDigit(C), !, 
      getInt(IS, C, Num, NextC), 
      Token=tNum{line:Lineno,file:FileName,num:Num}, NextLineno = Lineno 
      %%% scan letters as keywords or identifiers
      ; (isLower(C) ; isUpper(C)),
	!,
	getId(IS, C, Id, NextC),
	( reservedWord(Id)
	  -> 
              Token = tKeyw{line:Lineno,file:FileName,word:Id},  NextLineno = Lineno 
	  ; Token = tId{line:Lineno,file:FileName,name:Id}, NextLineno = Lineno 
	)
      ; (space(C); tab(C)), 
	!,
	get(IS,Char), scanToken(FileName,IS,Char,Lineno,Token,NextC,NextLineno)
      ; (endline(C)),
	!,
	%%% for a newline we need to increment the line number
	get(IS,Char), 
	(SuccLineno is Lineno + 1, 
         scanToken(FileName,IS,Char,SuccLineno,Token,NextC,NextLineno))
      %%% skip comments starting by #
      ; (hashcomment(C),
	 !,
	 skipline(IS,C,AfterC),
	 (SuccLineno is Lineno + 1, 
          scanToken(FileName,IS,AfterC,SuccLineno,Token,NextC,NextLineno))
	)
      %%% end of file
      ; isEos(C), !, Token = eof, NextC = C, NextLineno = Lineno
      %%% single character delimiters
      ; period(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:period}, get(IS, NextC), NextLineno = Lineno
      ; lparen(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:lparen}, get(IS, NextC), NextLineno = Lineno
      ; rparen(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:rparen}, get(IS, NextC), NextLineno = Lineno
      ; times(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:times}, get(IS, NextC), NextLineno = Lineno
      ; plus(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:plus}, get(IS, NextC), NextLineno = Lineno
      ; comma(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:comma}, get(IS, NextC), NextLineno = Lineno
      ; minus(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:minus}, get(IS, NextC), NextLineno = Lineno
      ; semicolon(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:semicolon}, get(IS, NextC), NextLineno = Lineno
      ; less(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:less}, get(IS, NextC), NextLineno = Lineno
      ; equal(C), !, 
	Token = tDelim{line:Lineno,file:FileName,cont:equal}, get(IS, NextC), NextLineno = Lineno
      %%% two characters := is the assign delimiter
      ; colon(C), !, get(IS, Eq), equal(Eq), !,
	Token = tDelim{line:Lineno,file:FileName,cont:assign},
	get(IS, NextC), NextLineno = Lineno
      ; !, printf(warning_output, "BOPL lexical error line %d char %c\n", [Lineno, C]), fail
    ).					    


skipline(IS,C,NextC) :- 
    var(NextC),
    ( endline(C) -> get(IS,NextC), !
      ;
      isEos(C) -> NextC = C, !
      ;
      !,
      get(IS,SuccC), !, 
      skipline(IS,SuccC,NextC)
    ) .



%%% the reserved key words of BOPL
reservedWord(program).
reservedWord(begin).
reservedWord(let).
reservedWord(in).
reservedWord(end).
reservedWord(class).
reservedWord(is).
reservedWord(extends).
reservedWord(vars).
reservedWord(methods).
reservedWord(if).
reservedWord(then).
reservedWord(else).
reservedWord(while).
reservedWord(do).
reservedWord(return).
reservedWord(writeln).
reservedWord(nil).
reservedWord(self).
reservedWord(super).
reservedWord(new).
reservedWord(or).
reservedWord(and).
reservedWord(not).
reservedWord(instanceof).
reservedWord(true).
reservedWord(false).
%% since Int ... starts with an uppercase letter we use string constants, not atoms, here:
reservedWord('Int').
reservedWord('Bool').
reservedWord('Void').
reservedWord('Object').


%%%%%%%%%%%%%%%%
%% scanning integers and identifiers

getId(IS, C, Id, NextC) :-
  (isUpper(C) ; isLower(C)),
  get(IS, Char),
  restId(IS, Char, Lc, NextC),
  string_list(Idstring, [C|Lc]),
  atom_string(Id, Idstring).

restId(IS, C, [C|L], NextC) :-
  (isUpper(C) ; isLower(C) ; isDigit(C) ; C = '_'),
  !,
  get(IS, Char),
  restId(IS, Char, L, NextC).
restId(_, NextC, [], NextC).

getInt(IS, C, N, NextC) :-
  isDigit(C),
  get(IS, Char),
  restInt(IS, Char, Lc, NextC),
  string_list(Numstring, [C|Lc]),
  number_string(N, Numstring).

restInt(IS, C, [C|L], NextC) :-
  isDigit(C),
  !,
  get(IS, Char),
  restInt(IS, Char, L, NextC).
restInt(_, NextC, [], NextC).


%%% eof lexer.ecl
