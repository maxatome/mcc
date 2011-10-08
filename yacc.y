
/*
 * Copyright (c) 1997,98,99,2000,2001,2002 David Stes.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published 
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: yacc.ym,v 1.13 2003/09/29 16:52:05 stes Exp $
 */

%{
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include <stdarg.h>

#define YYERROR_VERBOSE

#if defined(__DEBUG)
# define MCC_DEBUG(action)	(ps_last_token = (action),		\
				 printf("yacc.y:%d <<<%s>>>\n",		\
					__LINE__, SvPV_nolen(ps_last_token)), \
				 ps_last_token)
#else
# define MCC_DEBUG(action)	(ps_last_token = (action))
#endif

SV *ps_last_token = NULL;

SV *ps_last_lex_token = NULL;

extern SV *perlsubf(char *pa_func, char *pa_format, ...);
extern SV *perlsubn(char *pa_func, ...);
#define F(perl_func, format, args...)				\
  MCC_DEBUG(perlsubf("main::" #perl_func, format , ## args))
#define N(perl_func, args...)					\
  MCC_DEBUG(perlsubn("main::" #perl_func , ## args, NULL))

#define J(first, args...) N(mcc_join, first , ## args)

#define NEW_EMPTY		newSVpvn("", 0)

#define YYSTYPE SV*
%}

%start translunit

%token identifier constant stringcomp builtinfun typeword
%token storageclass typequal externlang classname
%token mccclass
%token cppdirect
 
%token enumkeyw structkeyw ifkeyw elsekeyw whilekeyw dokeyw forkeyw
%token switchkeyw casekeyw
%token defaultkeyw breakkeyw continuekeyw returnkeyw gotokeyw
%token asmkeyw sizeofop typeofop
%token ellipsis
%token assignop equalop relop shift plusplus logand logor arrow

/* Objective C */

%token atdefs atselector atinterface atimplementation atend atencode atrequires 

/* compiler specific */
%token gnuextension attributekeyw

/* precedence (lowest to highest) */
%left ','  /* a,b,c -> (a,b),c */
%right '=' /* a=b=c -> a=(b=c) */
%right ifkeyw elsekeyw
%right assignop
%right '?' ':'
%left logor 
%left logand 
%left '|'
%left '^'
%left '&'
%left equalop
%left relop
%left shift
%left '+' '-'
%left '*' '%' '/'
%right '!' '~' sizeofop typeofop unary plusplus 
%left arrow '.'
%left hyperunary
%left '(' '[' ')' ']'

%%

abstrdecl : pointer
  | abstrdeclx
  | pointer abstrdeclx 
     { $$ = J($1, $2); }
  | gnuattribdecl abstrdecl
     { $$ = J($1, $2); }
  | pointer gnuattribdecl abstrdecl
     { $$ = J($1, $2, $3); }
  ;

msabstrdecl : abstrdecl
  | typequallist abstrdecl 
     { $$ = J($1, $2); }
  ;

abstrdeclx : '(' msabstrdecl ')'
     { $$ = J($1, $2, $3); }
  | '[' optconstantexpr ']'
     { $$ = J($1, $2, $3); }
  | abstrdeclx '[' optconstantexpr ']'
     { $$ = J($1, $2, $3, $4); }
  | abstrdeclx '(' optparmdeflist ')' 
     { $$ = J($1, $2, $3, $4); }
  | abstrdeclx gnuattribdecl
     { $$ = J($1, $2); }
  | abstrdeclx asmstring
     { $$ = J($1, $2); }
  ;

andexpr : equalopexpr
  | andexpr '&' equalopexpr
     { $$ = J($1, $2, $3); }
  ;

anyword : identifier | storageclass | typespec | typequal
  ;

asmstring : asmkeyw '(' stringchain ')'
     { $$ = J($1, $2, $3, $4); }
  ;

asmops : /* empty */
     { $$ = NEW_EMPTY; }
  | asmop
  | asmops ',' asmop
     { $$ = J($1, $2, $3); }
  ;

asmop : stringchain '(' expr ')'
     { $$ = J($1, $2, $3, $4); }
  ;

asmclobbers : stringchain
  | asmclobbers ',' stringchain
     { $$ = J($1, $2, $3); }
  ;

assignexpr : condexpr
  | castexpr assignop assignexpr
     { $$ = J($1, $2, $3); }
  | castexpr '='
     { }
    assignexpr
     { $$ = J($1, $2, $4); }
  ;

gnuattriblist : gnuattrib
  | gnuattriblist ',' gnuattrib
     { $$ = J($1, $2, $3); }
  ;

builtinfunarg : typename /* as in builtin_isfloat(type) */
  | assignexpr
  ;

builtinfunargs : builtinfunarg
  | builtinfunargs ',' builtinfunarg
     { $$ = J($1, $2, $3); }
  ;

caretexpr : andexpr
  | caretexpr '^' andexpr
     { $$ = J($1, $2, $3); }
  ;

castexpr : unaryexpr
  | '(' typename ')' castexpr %prec unary
     { $$ = J($1, $2, $3, $4); }
  | '(' typename ')' '{' initializerlist '}' %prec unary
     { $$ = J($1, $2, $3, $4, $5, $6); }
  | '(' typename ')' '{' initializerlist ',' '}' %prec unary
     { $$ = J($1, $2, $3, $4, $5, $6, $7); }
  ;

classnamezz : atinterface identifier
     {
       N(add_class_type, $2);
       $$ = $2;
     }
  ;

classdef : classnamezz ivardef
     { $$ = N(atinterface, $1, &PL_sv_undef, $2); }
  | classnamezz ivardef ':' ivardef
     { $$ = N(atinterface, $1, &PL_sv_undef, $2, $4); }
  | classnamezz
     { $$ = N(atinterface, $1); }
  | classnamezz ':' mccclass
     { $$ = N(atinterface, $1, $3); }
  | classnamezz ':' mccclass ivardef 
     { $$ = N(atinterface, $1, $3, $4); }
  | classnamezz ':' mccclass ivardef ':' ivardef
     { $$ = N(atinterface, $1, $3, $4, $6); }
  ;

classimpl : atimplementation mccclass
     { $$ = N(atimplementation, $1, $2); }
  | atimplementation identifier
     {
       yyerror("@implementation without preceding @interface keyword");
       YYERROR;
     }
  ;

compoundstmt : '{' '}'
     { $$ = J($1, $2); }
  | '{' stmtlist '}'
     { $$ = J($1, $2, $3); }
  | '{' datadefcompoundlist '}'
     { $$ = J($1, $2, $3); }
  | '{' datadefcompoundlist stmtlist '}'
     { $$ = J($1, $2, $3, $4); }
  | '{' datadefcompoundlist stmtlist mixeddatastmtlist '}'
     { $$ = J($1, $2, $3, $4, $5); }
  ;

condexpr : logorexpr
  | logorexpr '?' expr ':' condexpr
     { $$ = J($1, $2, $3, $4, $5); }
  | logorexpr '?' ':' condexpr
     { $$ = J($1, $2, $3, $4); }
  ;

constantexpr : condexpr
  ;

datadef : datadefspecs ';'  /* id a,b; or id foo(id z); */
     { $$ = J($1, $2); }
  | datadefx ';'
     { $$ = J($1, $2); }
  | cppdirect
     { $$ = $1; }
  | requiresclause /* Objective C 3.3 */
  ;

datadeflist : datadef
  | datadeflist datadef
     { $$ = J($1, $2); }
  ;

datadefcompound : nestedfundef
  | datadef
  ;

datadefcompoundlist : datadefcompound
  | datadefcompoundlist datadefcompound
     { $$ = J($1, $2); }
  ;

datadefspecs : storageclass
  | datadefspecs storageclass 
     { $$ = J($1, $2); }
  | typespec
  | datadefspecs typespec
     { $$ = J($1, $2); }
  | datadefspecs gnuattribdecl typespec
     { $$ = J($1, $2, $3); }
  | externlang
  ;

datadefx : datadefspecs decl
     { }
    optinitializer
     {
       N(add_type, $1, $2);	/* Add new type if $1=typedef */
       $$ = J($1, $2, $4);
     }
  | datadefx ',' decl
     { }
    optinitializer
     {
       N(add_type, $1, $3);	/* Add new type if $1=typedef */
       $$ = J($1, $2, $3, $5);
     }
  ;

decl : declx
  | pointer declx
     { $$ = J($1, $2); }
  | gnuattribdecl decl
     { $$ = J($1, $2); }
  | pointer gnuattribdecl decl
     { $$ = J($1, $2, $3); }
  ;

declx : identifier
  | '(' decl ')'
     { $$ = J($1, $2, $3); }
  | '(' typequallist decl ')'
     { $$ = J($1, $2, $3, $4); }
  | declx '[' optconstantexpr ']'
     { $$ = J($1, $2, $3, $4); }
  | declx '(' optparmdeflist ')'
     { $$ = J($1, $2, $3, $4); }
  | declx gnuattribdecl
     { $$ = J($1, $2); }
  | declx asmstring
     { $$ = J($1, $2); }
  ;

encodeexpr : atencode '(' typename ')'
     { $$ = J($1, $2, $3, $4); }
  ;

equalopexpr : relopexpr
  | relopexpr equalop relopexpr
     { $$ = J($1, $2, $3); }
  ;

enumspec : enumkeyw identifier '{' enumlist '}'
     { $$ = J($1, $2, $3, $4, $5); } 
  | enumkeyw identifier '{' enumlist ',' '}'
     { $$ = J($1, $2, $3, $4, $5, $6); }
  | enumkeyw '{' enumlist '}'
     { $$ = J($1, $2, $3, $4); }
  | enumkeyw '{' enumlist ',' '}'
     { $$ = J($1, $2, $3, $4, $5); }
  | enumkeyw identifier
     { $$ = J($1, $2); }
  ;

enumlist : enumerator
  | enumlist enumerator
     { $$ = J($1, $2); }
  | enumlist ',' enumerator
     { $$ = J($1, $2, $3); }
  ;

enumerator : identifier
  | identifier '=' constantexpr
     { $$ = J($1, $2, $3); }
  | cppdirect
     { $$ = $1; }
  ;

expr : assignexpr
  | expr ',' assignexpr
     { $$ = J($1, $2, $3); }
  ;

exprlist : assignexpr
  | exprlist ',' assignexpr
     { $$ = J($1, $2, $3); }
  ;

extdef : fundef
  | datadef
  | classdef
  | classimpl
  | methoddef
  | atend
     { $$ = N(atend, $1); }
  | ';'
  | gnuasmstmt /* gcc allows asm() at toplevel */
  | externlangblock  
  ;

externlangblock : externlang '{' '}'
     { $$ = J($1, $2, $3); }
  | externlang '{' 
     { }
    translunit '}' 
     { $$ = J($1, $2, $4, $5); }
  ;

funbody : /* id a,b; { } */
    compoundstmt  
  | datadeflist compoundstmt  
     { $$ = J($1, $2); }
  ;

fundef : decl /* f(id z) { } */
     { }
    funbody 
     { $$ = J($1, $3); }
  | datadefspecs decl /* id f(id z) { } */
     { }
    funbody
     { $$ = J($1, $2, $4); }
  ;

nestedfundef : datadefspecs decl /* id f(id z) { } */
     { }
    compoundstmt
     { $$ = J($1, $2, $4); }
  ;

/* gcc stuff like : asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x)) */

gnuasmstmt : asmkeyw opttypequal '(' expr ')' ';'
     { $$ = J($1, $2, $3, $4, $5, $6); } 
  | asmkeyw opttypequal '(' expr ':' asmops ')' ';'
     { $$ = J($1, $2, $3, $4, $5, $6, $7, $8); } 
  | asmkeyw opttypequal '(' expr ':' asmops ':' asmops ')' ';'
     { $$ = J($1, $2, $3, $4, $5, $6, $7, $8, $9, $10); } 
  | asmkeyw opttypequal '(' expr ':' asmops ':' asmops ':' asmclobbers ')' ';'
     { $$ = J($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12); }
  ;

gnuattribdecl : attributekeyw '(' '(' gnuattriblist ')' ')'
    { $$ = J($1, $2, $3, $4, $5, $6); } 
  ;

gnuattrib : /* empty */
    { $$ = NEW_EMPTY; }
  | anyword
  | anyword '(' exprlist ')'
    { $$ = J($1, $2, $3, $4); }
  ;

initializer : assignexpr
  | identifier ':' assignexpr
    { $$ = J($1, $2, $3); }
  | '.' identifier assignexpr
    { $$ = J($1, $2, $3); }
  | '{' initializerlist '}'
    { $$ = J($1, $2, $3); }
  | '{' initializerlist ',' '}'
    { $$ = J($1, $2, $3, $4); }
  ;

initializerlist : initializer
  | initializerlist ',' initializer
     { $$ = J($1, $2, $3); }
  ;

identifiers : identifier
  | identifiers ',' identifier
     { $$ = J($1, $2, $3); }
  ;

ivardef : '{' '}'
     { $$ = &PL_sv_undef; }
  | '{' componentdeflist '}'
     { $$ = J($1, $2, $3); }
  ;

keywarglist : keywarg
     { $$ = N(keywarg_add, $1); }
  | keywarglist keywarg
     { $$ = N(keywarg_add, $2, $1); }
  ;

keywarg : unaryselector ':' { } expr
    { $$ = N(keywarg, $4, $1); }
  | ':' { } expr
    { $$ = N(keywarg, $3); }
  ;

keywdecl : unaryselector ':' identifier
    { $$ = N(keywdecl, $1, &PL_sv_undef, $3); }
  | unaryselector ':' '(' typename ')' identifier
    { $$ = N(keywdecl, $1, $4, $6); }
  | ':' identifier
    { $$ = N(keywdecl, &PL_sv_undef, &PL_sv_undef, $2); }
  | ':' '(' typename ')' identifier
    { $$ = N(keywdecl, &PL_sv_undef, $3, $5); }
  ;

keywselector : keywdecl
     { $$ = N(keywdecl_add, $1); }
  | keywselector keywdecl
     { $$ = N(keywdecl_add, $2, $1); }
  ;

logandexpr : orexpr
  | logandexpr logand orexpr
     { $$ = J($1, $2, $3); }
  ;

logorexpr : logandexpr
  | logorexpr logor logandexpr
     { $$ = J($1, $2, $3); }
  ;

methoddef : '+' methodproto ';'
     { $$ = N(methoddef, $1, $2, $3); }
  | '-' methodproto ';'
     { $$ = N(methoddef, $1, $2, $3); }
  | '+' methodproto
     { }
    compoundstmt
     { $$ = N(methoddef, $1, $2, $4); }
  | '-' methodproto
     { }
    compoundstmt
     { $$ = N(methoddef, $1, $2, $4); }
  ;

methodproto : '(' typename ')' unaryselector
     { $$ = N(methodproto, $2, $4); }
  | '(' typename ')' keywselector
     { $$ = N(methodproto, $2, $4); }
  | '(' typename ')' keywselector ',' ellipsis
     { $$ = N(methodproto, $2, $4, $6); }
  | unaryselector
     { $$ = N(methodproto, &PL_sv_undef, $1); }
  | keywselector
     { $$ = N(methodproto, &PL_sv_undef, $1); }
  | keywselector ',' ellipsis /* +sprintf:(char*)format,... */
     { $$ = N(methodproto, &PL_sv_undef, $1, $3); }
  ;

msgargs : unaryselector
  | keywarglist
  ;

msgexpr : '[' { } expr { } msgargs ']'
     { $$ = N(msgexpr, $3, $5); } /* XXX faut virer ] et laisser le reste */
  ;

mixeddatastmtlist : datadeflist
  | datadeflist stmtlist
     { $$ = J($1, $2); }
  | datadeflist stmtlist mixeddatastmtlist
     { $$ = J($1, $2, $3); }
  ;

optbuiltinfunargs : /* empty */
     { $$ = NEW_EMPTY; }
  | builtinfunargs
  ;

optexpr : /* empty */
     { $$ = NEW_EMPTY; }
  | expr
  ;

optexprlist : /* empty */
     { $$ = NEW_EMPTY; }
  | exprlist
  ;

optconstantexpr : /* empty */
     { $$ = NEW_EMPTY; }
  | constantexpr
  ;

optinitializer : /* empty */
     { $$ = NEW_EMPTY; }
  | '=' initializer
     { $$ = J($1, $2); }
  ;

opttypequal : /* empty */
     { $$ = NEW_EMPTY; }
  | typequal
  ;

optparmdeflist : /* empty */
     { $$ = NEW_EMPTY; }
  | ellipsis
  | identifiers
  | parmdeflist
  | parmdeflist ',' ellipsis
     { $$ = J($1, $2, $3); }
  ;

orexpr : caretexpr
  | orexpr '|' caretexpr
     { $$ = J($1, $2, $3); }
  ;

plusexpr : timesexpr
  | plusexpr '+' timesexpr
     { $$ = J($1, $2, $3); }
  | plusexpr '-' timesexpr
     { $$ = J($1, $2, $3); }
  ;

ppiname : identifier
  ;

ppinamelist : ppiname
  | ppinamelist ',' ppiname
     { $$ = J($1, $2, $3); }
  ;

primaryexpr : identifier
  | constant
  | stringchain
  | ellipsis
  | '(' expr ')'
     { $$ = J($1, $2, $3); }
  | '(' compoundstmt ')' /* gnu braced group ({ ... }) */
     { $$ = J($1, $2, $3); }
  | primaryexpr '[' expr ']' %prec hyperunary
     { $$ = J($1, $2, $3, $4); }
  | primaryexpr '(' optexprlist ')' %prec hyperunary
     { $$ = J($1, $2, $3, $4); }
  | primaryexpr '(' compoundstmt ')' %prec hyperunary
     { $$ = J($1, $2, $3, $4); }
  | primaryexpr '.' identifier
     { $$ = J($1, $2, $3); }
  | primaryexpr '.' typeword
     { $$ = J($1, $2, $3); }
  | primaryexpr arrow identifier
     { $$ = J($1, $2, $3); }
  | primaryexpr arrow typeword
     { $$ = J($1, $2, $3); }
  | mccclass arrow identifier
     { $$ = J(N(class2mcc, $1), $2, $3); }
  | mccclass arrow typeword
     { $$ = J(N(class2mcc, $1), $2, $3); }
  | primaryexpr plusplus
     { $$ = J($1, $2); }
  | msgexpr
  | selectorexpr 
  | encodeexpr
  ;

parmdeflist : parmdef
  | parmdeflist ',' parmdef
     { $$ = J($1, $2, $3); }
  ;

parmdef : typespeclist
  | storageclass typespeclist
     { $$ = J($1, $2); }
  | typespeclist abstrdecl  /* unsigned char*  */
     { $$ = J($1, $2); }
  | storageclass typespeclist abstrdecl
     { $$ = J($1, $2, $3); }
  | typespeclist decl   /* unsigned char*c */
     { $$ = J($1, $2); }
  | storageclass typespeclist decl
     { $$ = J($1, $2, $3); }
  ;

pointer : '*'
  | '*' typespeclist
     { $$ = J($1, $2); }
  | '*' pointer
     { $$ = J($1, $2); }
  | '*' typespeclist pointer
     { $$ = J($1, $2); }
  ;

relopexpr : shiftexpr
  | relopexpr relop shiftexpr
     { $$ = J($1, $2, $3); }
  ;

requiresclause : atrequires ppinamelist ';'
     { warn("ignoring @requires"); }
  ;

reservedword : enumkeyw  | structkeyw | ifkeyw | elsekeyw | whilekeyw
  | dokeyw | forkeyw | switchkeyw | casekeyw | defaultkeyw | breakkeyw
  | continuekeyw | returnkeyw | gotokeyw | asmkeyw | sizeofop
  | typequal
  ;

selectorarg : unaryselector
  | selectorarg ':' /* add:: */ 
     { $$ = J($1, $2); }
  | selectorarg unaryselector ':' /* add:modulo: */
     { $$ = J($1, $2, $3); }
  ;

selectorexpr : atselector '(' selectorarg ')'
     { $$ = J($1, $2, $3, $4); }
  ;

shiftexpr : plusexpr
  | plusexpr shift plusexpr
     { $$ = J($1, $2, $3); }
  ;

structspec : structkeyw identifier '{' optcomponentdeflist '}'
     { $$ = J($1, $2, $3, $4, $5); }
  | structkeyw typeword '{' optcomponentdeflist '}'
     { $$ = J($1, $2, $3, $4, $5); }
  | structkeyw '{' optcomponentdeflist '}'
     { $$ = J($1, $2, $3, $4); }
  | structkeyw identifier
     { $$ = J($1, $2); }
  | structkeyw typeword
     { $$ = J($1, $2); }
  | structspec gnuattribdecl
     { $$ = J($1, $2); }
  ;

optcomponentdeflist : /* empty */
     { $$ = NEW_EMPTY; }
  |  componentdeflist
  ;

componentdeflist : componentdef
  | componentdeflist componentdef
     { $$ = J($1, $2); }
  | atdefs '(' identifier ')'
     { $$ = J($1, $2, $3, $4); }
  | componentdeflist atdefs '(' identifier ')'
     { $$ = J($1, $2, $3, $4, $5); }
  ;

componentdef : componentdefx ';'
     { $$ = J($1, $2); }
  | typespeclist ';'
     { $$ = J($1, $2); }
  | cppdirect
     { $$ = $1; }
  ;

componentdefx : typespeclist bitfielddecl
     { $$ = J($1, $2); }
  | componentdefx ',' bitfielddecl
     { $$ = J($1, $2, $3); }
  ;

bitfielddecl : decl
  | ':' constantexpr
     { $$ = J($1, $2); }
  | decl ':' constantexpr
     { $$ = J($1, $2, $3); }
  ;

stmt : stmtrc
   ;

stmtrc : optexpr ';'
     { $$ = J($1, $2); }
  | compoundstmt 
  | identifier ':' stmt
     { $$ = J($1, $2, $3); }
  | casekeyw constantexpr ':' stmt
     { $$ = J($1, $2, $3, $4); }
  | casekeyw constantexpr ellipsis constantexpr ':' stmt
     { $$ = J($1, $2, $3, $4, $5, $6); }
  | defaultkeyw ':' stmt
     { $$ = J($1, $2, $3); }
  | ifkeyw '(' expr ')' stmt %prec ifkeyw
     { $$ = J($1, $2, $3, $4, $5); }
  | ifkeyw '(' expr ')' stmt elsekeyw stmt 
     { $$ = J($1, $2, $3, $4, $5, $6, $7); }
  | ifkeyw '(' expr ')' stmt cppdirect elsekeyw stmt 
     { $$ = J($1, $2, $3, $4, $5, $6, $7, $8); }
  | switchkeyw '(' expr ')' stmt 
     { $$ = J($1, $2, $3, $4, $5); }
  | whilekeyw '(' expr ')' stmt
     { $$ = J($1, $2, $3, $4, $5); }
  | dokeyw stmt whilekeyw '(' expr ')' ';'
     { $$ = J($1, $2, $3, $4, $5, $6, $7); }
  | forkeyw '(' optexpr ';' optexpr ';' optexpr ')' stmt
     { $$ = J($1, $2, $3, $4, $5, $6, $7, $8, $9); }
  | gotokeyw identifier ';'
     { $$ = J($1, $2, $3); }
  | gotokeyw '*' expr ';'
     { $$ = J($1, $2, $3, $4); }
  | continuekeyw ';'
     { $$ = J($1, $2); }
  | breakkeyw ';'
     { $$ = J($1, $2); }
  | returnkeyw ';'
     { $$ = J($1, $2); }
  | returnkeyw expr ';'
     { $$ = J($1, $2, $3); }
  | gnuasmstmt
  | cppdirect
     { $$ = $1; }
  ;

stmtlist : stmt
  | stmtlist stmt
     { $$ = J($1, $2); }
  ;

stringchain : stringcomp /* string continuation */
  | stringchain stringcomp
     { $$ = J($1, $2); }
  ;

timesexpr : castexpr
  | timesexpr '*' castexpr
     { $$ = J($1, $2, $3); }
  | timesexpr '/' castexpr
     { $$ = J($1, $2, $3); }
  | timesexpr '%' castexpr
     { $$ = J($1, $2, $3); }
  ;

translunit : extdef
     {
       N(output_top_level, $1);
       if (yychar == -1)
	 ps_last_lex_token = NULL;
       ps_last_token = $$ = NULL;
     }
  | translunit extdef
     {
       N(output_top_level, $2);
       if (yychar == -1)
	 ps_last_lex_token = NULL;
       ps_last_token = $$ = NULL;
     }
  ;

typename : typespeclist /* unsigned char */
  | typespeclist abstrdecl /* unsigned char* */
     { $$ = J($1, $2); }
  ;

typespec : typeword
  | mccclass
  | typequal
  | structspec cppdirect
     { $$ = J($1, $2); }
  | structspec
  | enumspec
  | gnuextension
  | typeofop '(' expr ')'
     { $$ = J($1, $2, $3, $4); }
  ;

typespeclist : typespec
  | typespeclist typespec
     { $$ = J($1, $2); }
  ;

typequallist : typequal
  | typequallist typequal
     { $$ = J($1, $2); }
  ;

unaryexpr : primaryexpr
  | mccclass
     { $$ = N(class2mcc, $1); }
  | plusplus castexpr %prec unary
     { $$ = J($1, $2); }
  | unary castexpr %prec unary
     { $$ = J($1, $2); }
  | '!' castexpr %prec unary
     { $$ = J($1, $2); }
  | '~' castexpr %prec unary
     { $$ = J($1, $2); }
  | '&' castexpr %prec unary
     { $$ = J($1, $2); }
  | '*' castexpr %prec unary
     { $$ = J($1, $2); }
  | '+' castexpr %prec unary
     { $$ = J($1, $2); }
  | '-' castexpr %prec unary
     { $$ = J($1, $2); }
  | sizeofop unaryexpr %prec unary
     { $$ = J($1, $2); }
  | sizeofop '(' typename ')' %prec hyperunary
     { $$ = J($1, $2, $3, $4); }
  | builtinfun '(' optbuiltinfunargs ')' %prec hyperunary
     { $$ = J($1, $2, $3, $4); }
  | gnuextension castexpr %prec unary
     { $$ = J($1, $2); }
  | logand identifier
     { $$ = J($1, $2); }
  ;

unaryselector : identifier | reservedword | typeword | builtinfun | mccclass
  ;

%%
