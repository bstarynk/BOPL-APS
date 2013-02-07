%% -*- prolog -*-   %%% to make Emacs happy
%%%%%%%%%%%%%%%%%%%%%%%%%%% The BOPL lexer
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


:- use_module(library(pretty_print)).
:- module(lexer).
:- export scanFile/2,scan/2,lexWith/2,lexDelim/2,lexKeyw/2,lexId/2,lexNum/2,lexAt/2.


%% scanFile(+FileName, -Tokens)
scanFile(FileName, Tokens) :-
  scan(FileName, Tokens),
  pretty_print:pretty_print(stdout, Tokens, 80).


scan(FileName, Tokens) :-
  open(FileName, read, IS),
  get(IS, Char),
  (scanTokens(IS, Char, 1, Tokens, NbLines);(!,printf(warning_output, "BOPL failed to lex file %s\n",[FileName]),fail)),
  length(Tokens, NbTokens),
  !,
  printf(output,"BOPL lexed file %s of %d lines and %d tokens\n", [FileName, NbLines, NbTokens]),
  pretty_print:pretty_print(stdout, "\n", 80),
  close(IS).


%% we use the structure notation for tokens, because we want to carry
%% the line location of each lexical token.  See chapter 4 of the
%% ECLiPSe tutorial.

%%% tokens with location for delimiters (like period, lparen, ...)
:- local struct(tDelim(loc,cont)).
%% so code tDelim{cont:C} or tDelim{loc:L,cont:C} or tDelim(L,C)

%%% tokens with location for key words or reserved words
:- local struct(tKeyw(loc,word)).

%%% tokens with location for identifiers
:- local struct(tId(loc,name)).

%%% token with location for numbers
:- local struct(tNum(loc,num)).

lexAt(tDelim{loc:L},L).
lexAt(tId{loc:L},L).
lexAt(tKeyw{loc:L},L).
lexAt(tNum{loc:L},L).
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
scanTokens(_, C, Lineno, [], Lineno) :-
  isEos(C),
  !.
scanTokens(IS, C, Lineno, Tokens, NextLineno) :-
  scanToken(IS, C, Lineno, Token, NextC, SuccLineno),
  (Token = eof ->
     Tokens = RestTokens, NextLineno = SuccLineno
  ;  Tokens = [Token | RestTokens]
  ),
  scanTokens(IS, NextC, SuccLineno, RestTokens, NextLineno).


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

scanToken(IS, C, Lineno, Token, NextC, NextLineno) :-
%%% digits are numbers
  ( isDigit(C), !, getInt(IS, C, Num, NextC), Token=tNum{loc:Lineno,num:Num}, NextLineno = Lineno 
%%% scan letters as keywords or identifiers
    ; (isLower(C) ; isUpper(C)),
      !,
      getId(IS, C, Id, NextC),
    ( reservedWord(Id), !, ( Token=tKeyw{loc:Lineno,word:Id},  NextLineno = Lineno )
			       ; ( Token=tId{loc:Lineno,name:Id}, NextLineno = Lineno )
    )
    ; (space(C); tab(C)), 
      !,
      get(IS,Char), scanToken(IS,Char,Lineno,Token,NextC,NextLineno)
    ; (endline(C)),
       !,
%%% for a newline we need to increment the line number
       get(IS,Char), 
       (SuccLineno is Lineno + 1, scanToken(IS,Char,SuccLineno,Token,NextC,NextLineno))
%%% skip comments starting by #
    ; (hashcomment(C),
       !,
       skipline(IS,C,NextC),
       (SuccLineno is Lineno + 1, scanToken(IS,Char,SuccLineno,Token,NextC,NextLineno))
      )
%%% end of file
    ; isEos(C), !, Token = eof, NextC = C, NextLineno = Lineno
%%% single character delimiters
    ; period(C), !, Token = tDelim{loc:Lineno,cont:period}, get(IS, NextC), NextLineno = Lineno
    ; lparen(C), !, Token = tDelim{loc:Lineno,cont:lparen}, get(IS, NextC), NextLineno = Lineno
    ; rparen(C), !, Token = tDelim{loc:Lineno,cont:rparen}, get(IS, NextC), NextLineno = Lineno
    ; times(C), !, Token = tDelim{loc:Lineno,cont:times}, get(IS, NextC), NextLineno = Lineno
    ; plus(C), !, Token = tDelim{loc:Lineno,cont:times}, get(IS, NextC), NextLineno = Lineno
    ; comma(C), !, Token = tDelim{loc:Lineno,cont:comma}, get(IS, NextC), NextLineno = Lineno
    ; minus(C), !, Token = tDelim{loc:Lineno,cont:minus}, get(IS, NextC), NextLineno = Lineno
    ; semicolon(C), !, Token = tDelim{loc:Lineno,cont:semicolon}, get(IS, NextC), NextLineno = Lineno
    ; less(C), !, Token = tDelim{loc:Lineno,cont:less}, get(IS, NextC), NextLineno = Lineno
    ; equal(C), !, Token = tDelim{loc:Lineno,cont:equal}, get(IS, NextC), NextLineno = Lineno
%%% two characters := is the assign delimiter
  ; colon(C), !, get(IS, Eq), equal(Eq), Token = tDelim{loc:Lineno,cont:assign},
    get(IS, NextC), NextLineno = Lineno
  ; !, printf(warning_output, "BOPL lexical error line %d char %c\n", [Lineno, C]), fail
  ).					    



skipline(IS,C,NextC) :- not(endline(C)), !, get(IS,SuccC), skipline(IS,SuccC,NextC).
skipline(IS,C,NextC) :- isEos(C), NextC = C.
skipline(IS,C,NextC) :- endline(C), !, get(IS,NextC).



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
