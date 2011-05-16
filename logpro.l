;; Copyright 2007-2010, Matthew Welland.
;;
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;;
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; herc

digit            [0-9]
letter           [a-zA-Z]
identchars       [_%\/\.:\*\+\-\(\)\[\]\#]
escape           \\ 
newline          \n
whitespace       [ \9]

plainidentifier  ({letter}|{digit}|{identchars})+

period           \.
comma            ,
semicolon        \;
gt               >
lt               <
eq               =
gteq             >=
lteq             <=
slash            /

%%

{whitespace}                  (list 'whitespace #f ) ;; yyline)

{newline}                     (list 'newline    #f) 

#                             (list 'comment    #f ) ;; yyline)

trigger                       (list 'trigger    #f ) ;; yyline)

section                       (list 'section     #f ) ;; yyline)

expect                        (list 'expect      #f ) ;; yyline)

{gt}                          (list 'greater-than  #f ) ;; yyline)

{lt}                          (list 'less-than  #f ) ;; yyline)

{eq}                          (list 'equal       #f ) ;; yyline)

{plainidentifier}             (list 'identifier yytext ) ;; yyline)

<<EOF>>                       (list 'end-of-input #f ) ;; yyline)

<<ERROR>>                     (lex-error (conc yyline " : illegal character ") (yygetc))
