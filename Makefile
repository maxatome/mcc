#
# Makefile -- 
# 
# Author          : Maxime Soule
# Created On      : Mon Sep 18 14:25:30 2006
# Last Modified By: Maximum Solo
# Last Modified On: Sat Oct  8 07:16:36 2011
# Update Count    : 80
# Status          : Very good Makefile !!!
#

#
# CC=/usr/local/pilot/bin/m68k-palmos-gcc ./mcc.pl XXX.m
#

TARGET=		parser
TARGETLIB=	libmcc$(TARGET).so

SRCS=		main.c

LIBSRC=		yacc.c lex.c

PERLINC=	/usr/local/lib/perl5/5.14.1/mach/CORE

CFLAGS+=	-I${PERLINC} -fPIC #-D__DEBUG

all: $(TARGETLIB)

$(TARGETLIB): $(LIBSRC:.c=.o) tokens.pl
	cc -o $@ -shared $(LIBSRC:.c=.o)
	rm -rf _Inline

lex.c: yacc.c

tokens.pl: yacc.c
	@perl -e '\
		open(NEW, ">", "$@") or die "open $@\n"; \
		print NEW "\%TOKENS = (\n"; \
		while (<>) \
		{ \
		    if (/^#define\s+(\w+)\s+(\d+)/) \
		    { \
			print NEW "\t$$1 => $$2,\n"; \
	 	    } \
		} \
		print NEW "\t);\n"; \
		close NEW; \
		' < y.tab.h

clean:
	rm -rf $(SRCS:.c=.o) $(LIBSRC:.c=.o) \
	       lex.c yacc.h yacc.c y.tab.h tokens.pl \
	       _Inline
