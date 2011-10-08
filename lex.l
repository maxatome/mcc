/* -*- c -*-
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
 * $Id: lex.lm,v 1.7 2003/09/27 09:32:44 stes Exp $
 *
 * ps_sv = SvPV_nolen(SV *);
 */

%{
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include <sys/types.h>
#include "y.tab.h"

#define YY_NEVER_INTERACTIVE 1
  //#define YY_DECL int yylex_raw()

extern SV *perlsubf(char *pa_func, char *pa_format, ...);
extern SV *perlsubn(char *pa_func, ...);
#define F(perl_func, format, args...)				\
  perlsubf("main::" #perl_func, format , ## args)
#define N(perl_func, args...)					\
  perlsubn("main::" #perl_func , ## args, NULL)

extern SV *ps_last_token;	/* Affecté dans lex et réinitialisé dans yacc */
extern SV *ps_last_lex_token;	/* Affecté dans lex */
extern SV *yylval;

#define SET_YYLVAL	(yylval = ps_last_token = ps_last_lex_token	\
			 = newSVpvn(yytext, yyleng))

#define keyw(x)		(SET_YYLVAL, x)
#define gnukeyw(x)	keyw(x)
#define tkeyw(x, t)	(SET_YYLVAL, x)
#define simple_char(c)	(SET_YYLVAL, c)


int inlineno = 0;
char *gpa_global_buffer;
int  gi_global_buffer_len;
char gra_global_filename[256];

#define YY_INPUT(buf, result, max_size)				\
  {								\
    if (gi_global_buffer_len == 0)				\
      result = YY_NULL;						\
    else							\
    {								\
      int i_copy_len = (max_size > gi_global_buffer_len)	\
	? gi_global_buffer_len : max_size;			\
      bcopy(gpa_global_buffer, buf, i_copy_len);		\
      gpa_global_buffer += i_copy_len;				\
      gi_global_buffer_len -= i_copy_len;			\
      result = i_copy_len;					\
    }								\
  }

%}

/* a decimal digit / exponent */
DIGIT ([0-9])
EXP ([Ee][+-]?[0-9]+)

/* octal and hexademical constants */
OCTDIGIT ([0-7])
HEXDIGIT ([0-9a-fA-F])

/* escape seq such as \' or \\ or \n */
/* WATCOM does things like '\0x0d' instead of '\x0d' (as it should be) */
ESCAPEOCT (\\{OCTDIGIT}{1,3})
ESCAPEHEX (\\0?[xX]({HEXDIGIT}{1,3}))
ESCAPECHAR (\\[0\'\"?abfnrtv\\])
ESCAPESEQ ({ESCAPECHAR}|{ESCAPEOCT}|{ESCAPEHEX})

/* stuff for being able to scan files Unix/Mac */
RETURN ((\n)|(\r)|(\r\n))

SPACES ([ \t]+)

/* an escape seq or any char other than backslash, double quote or newline */
CCHAR ({ESCAPESEQ}|[^\"\\\n\r])
  
/* SGI has extensions such as LUL as int.suffix */
INTSUFFIX ([uUlL]{1,3})
DBLSUFFIX ([fFlL]{1,3})

%%

"auto" 			{ return keyw(storageclass); }
"_Bool"			{ return keyw(typeword); }
"break" 		{ return keyw(breakkeyw); }
"case"			{ return keyw(casekeyw); }
"_Complex"		{ return keyw(typeword); }
"char"			{ return keyw(typeword); }
"continue"		{ return keyw(continuekeyw); }
"default"		{ return keyw(defaultkeyw); }
"do"			{ return keyw(dokeyw); }
"double"		{ return keyw(typeword); }
"else"			{ return keyw(elsekeyw); }
"enum"			{ return keyw(enumkeyw); }
"extern"		{ return keyw(storageclass); }
"float"			{ return keyw(typeword); }
"for"			{ return keyw(forkeyw); }
"goto"			{ return keyw(gotokeyw); }
"if"			{ return keyw(ifkeyw); }
"_Imaginary"		{ return keyw(typeword); }
"int"			{ return keyw(typeword); }
"long"			{ return keyw(typeword); }
"register"		{ return keyw(storageclass); }
"return"		{ return keyw(returnkeyw); }
"short"			{ return keyw(typeword); }
"sizeof"		{ return keyw(sizeofop); }
"static"		{ return keyw(storageclass); }
"struct"		{ return keyw(structkeyw); }
"switch"		{ return keyw(switchkeyw); }
"typedef"		{ return keyw(storageclass); }
"union"			{ return keyw(structkeyw); }
"unsigned"		{ return keyw(typeword); }
"void"			{ return keyw(typeword); }
"while"			{ return keyw(whilekeyw); }

"const"			{ return keyw(typequal); }
"restrict"		{ return keyw(typequal); }
"signed"		{ return keyw(typeword); }
"volatile"		{ return keyw(typequal); }

"@defs"			{ return keyw(atdefs); }
"@selector"		{ return keyw(atselector); }
"@interface"		{ return keyw(atinterface); }
"@implementation"	{ return keyw(atimplementation); }
"@end"			{ return keyw(atend); }
"@encode"		{ return keyw(atencode); }
"id"			{ return keyw(typeword); }
"@requires"		{ return keyw(atrequires); }
"@public"		{ warn("ignoring @public."); }
"@protected"		{ warn("ignoring @protected."); }
"@private"		{ warn("ignoring @private."); }

"inline"		{ return keyw(storageclass); }

"asm"			{ return keyw(asmkeyw); }


"__const__"		{ return gnukeyw(typequal); }
"__volatile"		{ return gnukeyw(typequal); }
"__volatile__"		{ return gnukeyw(typequal); }
"__const"		{ return gnukeyw(typequal); }
"__signed"		{ return gnukeyw(typeword); }
"__signed__"		{ return gnukeyw(typeword); }
"__unsigned"		{ return gnukeyw(typeword); }
"__unsigned__"		{ return gnukeyw(typeword); }
"__attribute__"		{ return gnukeyw(attributekeyw); }
"__asm"			{ return keyw(asmkeyw); }
"__asm__"		{ return keyw(asmkeyw); }
"_inline"		{ return keyw(storageclass); }
"__inline__"		{ return keyw(storageclass); }
"__inline"		{ return keyw(storageclass); }
"__extension__"		{ return gnukeyw(gnuextension); }
"__typeof"		{ return gnukeyw(typeofop); }
"typeof"		{ return gnukeyw(typeofop); }
"__restrict"		{ return gnukeyw(typequal); }
"__restrict__"		{ return gnukeyw(typequal); }
"__label__"		{ return gnukeyw(typeword); }


"("			{ return simple_char('('); }
")"			{ return simple_char(')'); }
"["			{ return simple_char('['); }
"]"			{ return simple_char(']'); }
"->"			{ return keyw(arrow); }
"."			{ return simple_char('.'); }
"!"			{ return simple_char('!'); }
"~"			{ return simple_char('~'); }
"++"			{ return keyw(plusplus); }
"--"			{ return keyw(plusplus); }
"*"			{ return simple_char('*'); }
"/"			{ return simple_char('/'); }
"%"			{ return simple_char('%'); }
"+"			{ return simple_char('+'); }
"-"			{ return simple_char('-'); }
"<<"			{ return keyw(shift); }
">>"			{ return keyw(shift); }
"<"			{ return keyw(relop); }
"<="			{ return keyw(relop); }
">"			{ return keyw(relop); }
">="			{ return keyw(relop); }
"=="			{ return keyw(equalop); }
"!="			{ return keyw(relop); }
"&"			{ return simple_char('&'); }
"^"			{ return simple_char('^'); }
"|"			{ return simple_char('|'); }
"&&"			{ return keyw(logand); }
"||"			{ return keyw(logor); }
"?"			{ return simple_char('?'); }
":"			{ return simple_char(':'); }
"="			{ return simple_char('='); }
"+="			{ return keyw(assignop); }
"-="			{ return keyw(assignop); }
"*="			{ return keyw(assignop); }
"/="			{ return keyw(assignop); }
"%="			{ return keyw(assignop); }
"&="			{ return keyw(assignop); }
"|="			{ return keyw(assignop); }
"^="			{ return keyw(assignop); }
"<<="			{ return keyw(assignop); }
">>="			{ return keyw(assignop); }
","			{ return simple_char(','); }
";"			{ return simple_char(';'); }
"{"			{ return keyw('{'); }
"}"			{ return keyw('}'); }

"..."			{ return keyw(ellipsis); }

{SPACES}		{
spaces:
  if (ps_last_lex_token != NULL)
    sv_catpvn(ps_last_lex_token, yytext, yyleng);
  else if (ps_last_token != NULL)
    sv_catpvn(ps_last_token, yytext, yyleng);
  else
    N(output_top_level, sv_2mortal(newSVpvn(yytext, yyleng)));
}

{RETURN}			{
  inlineno++;
  goto spaces;
}

"#"[^\n\r]*{RETURN}	{
  /* line directives, such as #, #line, #pragma, #ident */
  u_int ui_line;
  char ra_filename[yyleng + 1];

  SET_YYLVAL;

  if (sscanf(yytext, "# %u \"%255[^\"]\"", &ui_line, ra_filename) == 2)
  {
    strcpy(gra_global_filename, ra_filename);
    inlineno = ui_line;
  }
  else if (sscanf(yytext, "# line %u", &ui_line) == 1)
    inlineno = ui_line;
  else
    inlineno++;

  return cppdirect;
}

\"{CCHAR}*\"			{ return tkeyw(stringcomp,t_str); }

"__FUNCTION__"			{ return tkeyw(stringcomp,t_str); }

'({ESCAPESEQ}|[^'\\\n\r])*' {
  /* note that we also allow '"' (" is not CCHAR) */
  /* support Macintosh usage 'TEXT' or 'icm#' etc. */
  return tkeyw(constant,t_char);
}

(0{OCTDIGIT}+){INTSUFFIX}? {
  return tkeyw(constant,t_int);
}

(0[xX]{HEXDIGIT}+){INTSUFFIX}? {
  return tkeyw(constant,t_int);
}

([0-9]+){INTSUFFIX}? {
  return tkeyw(constant,t_int);
}

({DIGIT}+"."{DIGIT}*{EXP}?){DBLSUFFIX}? {
  return tkeyw(constant,t_double);
}

({DIGIT}+{EXP}){DBLSUFFIX}? {
  return tkeyw(constant,t_double);
}

({DIGIT}*"."{DIGIT}+{EXP}?){DBLSUFFIX}? {
  return tkeyw(constant,t_double);
}

[A-Za-z_$][0-9A-Za-z_$]* {
  SET_YYLVAL;
  return (long int)perlsubf("main::identifier", "IP", yylval);
}

"//"[^\n\r]* {
  /* accept and ignore bcpl style comment */;
}

. {
    if (1) {
        char c = (yytext)[0];
	warn("lex ignoring '0x%x' (%c)",(int)c,c);
    }
}

%%

/*
 * Put this in the routines section rather than the definitions section.
 * Old flex versions don't define 'yytext' before rules section.
 */ 

/* some flex defines it as 1 */

#ifndef yywrap
int yywrap() { return 1; }
#endif
