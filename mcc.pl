#!/usr/local/bin/perl -w
# 
# mcc.pl -- 
# 
# Author          : Maxime Soule
# Created On      : Mon Sep 18 14:46:22 2006
# Last Modified By: Maxime Soule
# Last Modified On: Tue Dec  4 16:45:09 2007
# Update Count    : 634
# Status          : Unknown, Use with caution!
#


use strict;
use Data::Alias;

use Inline C => DATA => LIBS => '-ly -ll -L/home/dotcom/max/Projet/mcc -lmccparser';

die <<END_OF_DIE if @ARGV == 0 or $ARGV[0] =~ /^-?-h(?:elp)\z/i;
usage: $0 file.m
       $0 -extract-classes file1.h [file2.h ...]
END_OF_DIE


my $CLASSES_FILE = $ENV{CLASSES} || 'classes';

#
# Extract class information
if ($ARGV[0] eq '-extract-classes')
{
    shift;
    local $/ = undef;

    my %classes;
    foreach my $hfile (@ARGV)
    {
	open(H, '<', $hfile) or die "Can't open $hfile: $!\n";
	my $contents = <H>;
	close H;

	while ($contents =~ /^\@interface\s+(\w+)(?:\s+:\s+(\w+))?/msgc)
	{
	    my($self, $super) = ($1, $2);

	    $classes{$self} = $super; # $self must be initialized after $super
	}
    }

    sub ordered ($$);
    sub ordered ($$)
    {
	my($c1, $c2) = @_;

	return -1 if not defined $classes{$c1};
	return 1 if not defined $classes{$c2};

	return 1 if $classes{$c1} eq $c2;
	return -1 if $classes{$c2} eq $c1;

	return ordered($classes{$c1}, $classes{$c2});
    }

    # Le fichier récapitulant l'ordre de toutes les classes
    open(CLASS_LST, '>', "$CLASSES_FILE.lst")
	or die "Can't open `$CLASSES_FILE.lst': $!\n";

    print CLASS_LST "%DEF_CLASSES = (\n";
    my $index = 0;
    foreach my $class (sort ordered keys %classes)
    {
	print CLASS_LST "\t$class => $index,\n";
	$classes{$class} = $index;

	$index++;
    }
    print CLASS_LST "\t);\n";

    close CLASS_LST;

    # Le fichier définissant les fonctions d'initialisation et de
    # libération des classes
    open(CLASS_H, '>', "$CLASSES_FILE.h")
        or die "Can't open `$CLASSES_FILE.h': $!\n";

    print CLASS_H <<END_OF_PART;
/* *** Automaticaly generated *** DO NOT EDIT *** */
#ifndef __CLASSES_H__
#define __CLASSES_H__

END_OF_PART

    my @classes = sort { $classes{$a} <=> $classes{$b} } keys %classes;

    #
    # Les index des classes
    print CLASS_H <<END_OF_PART;
enum
{
END_OF_PART

    foreach my $class (@classes)
    {
	print CLASS_H "  __$class\__ = $classes{$class},\n";
    }

    print CLASS_H <<END_OF_PART;
  NUM_CLASSES = @{[ scalar @classes ]}
};

END_OF_PART

    #
    # Déclaration des fonctions d'initialisation
    foreach my $class (@classes)
    {
	print CLASS_H "extern void *initialize_$class(void **, Boolean);\n";
    }

    #
    # La macro d'initialisation de toutes les classes
    print CLASS_H <<END_OF_PART;

#define initOneClass(class, poClasses, b_globals) \\
	poClasses[__ ## class ## __]		  \\
	  = initialize_ ## class((void**)poClasses, b_globals)

#define initClasses(poClasses, b_globals) \\
END_OF_PART

    print CLASS_H "\t", join("; \\\n\t",
			     map { "initOneClass($_, poClasses, b_globals)" }
			     @classes);

    print CLASS_H <<END_OF_PART;


#endif  /* __CLASSES_H__ */
END_OF_PART

    close CLASS_H;

    exit 0;
}


my @PATHS = ('./');
push(@PATHS, $ENV{MCC_PATH}) if exists $ENV{MCC_PATH};
push(@PATHS, $1) if $0 =~ m,^(.*)/,;

#
# Load YACC tokens
my %TOKENS;
foreach my $path (@PATHS)
{
    if (open(TOKENS, '<', "$path/tokens.pl"))
    {
	eval do { local $/ = undef; <TOKENS> };
	die "$@" if $@;
	close TOKENS;

	last;
    }
}
die "Can't open tokens.pl: $!\n(path contains @PATHS)\n" if keys %TOKENS == 0;


#
# Load classes list
my %DEF_CLASSES;
foreach my $path (@PATHS)
{
    if (open(CLASSES, '<', "$path/$CLASSES_FILE.lst"))
    {
	eval do { local $/ = undef; <CLASSES> };
	die "$@" if $@;
	close CLASSES;

	last;
    }
}
die "Can't open classes file `$CLASSES_FILE.lst': $!\n(path contains @PATHS)\n"
    if keys %DEF_CLASSES == 0;


my $CC = $ENV{CC} || '/usr/bin/gcc';

my @CPP_OPTS = qw(-E -x c);
my @CC_OPTS = qw(-x cpp-output);
my $M_FILE;
my $NO_UNLINK = $ENV{NOUNLINK} || $ENV{NO_UNLINK};

for (my $index = 0; $index < @ARGV; )
{
    my $arg = $ARGV[$index++];

    if ($arg =~ /^-(?:[ACDHIPU].*|d[DIMN]|M[MFGPQT]
		 |nostdinc|fworking-directory|remap|trigraphs|undef
		 |Wp,.*)\z/x)
    {
	push(@CPP_OPTS, $arg);
    }
    elsif ($arg =~ /^-(?:idirafter|include|imacros|iprefix|iwithprefix
			 |iwithprefixbefore|isystem|Xpreprocessor)\z/x)
    {
	push(@CPP_OPTS, $arg);
	push(@CPP_OPTS, $arg) if defined($arg = $ARGV[$index++]);
    }
    elsif ($arg eq '-c')
    {
	# skip
    }
    elsif ($arg =~ /\.([^.]+)\z/)
    {
	die "Only one .m file allowed at a time...\n" if defined $M_FILE;
	die "Only .m file allowed...\n" if $1 ne 'm';

	$M_FILE = $arg;
    }
    else
    {
	push(@CC_OPTS, $arg);
    }
}

(my $I_FILE = $M_FILE) =~ s,(?:.*/)?([^/]+?)(?:\.m)?\z,$1.i,;
(my $C_FILE = $I_FILE) =~ s/\.i\z/.c/;
(my $O_FILE = $I_FILE) =~ s/\.i\z/.o/;

unlink $I_FILE, $O_FILE;


########################################################################
#
# Passage d'abord à CPP
my $contents;
#print "$CC @CPP_OPTS -c $M_FILE\n";
open(CPP, '-|') || exec $CC, @CPP_OPTS, -c => $M_FILE;
while (defined(my $line = <CPP>))
{
    $contents .= $line;
}
close CPP;


########################################################################
#
# Ensuite on passe à mcc
my $header = <<HEADER;
# 1 "$M_FILE"
# 1 "<built-in>"
# 1 "<command line>"
typedef void *id;
HEADER

my $ret;
eval
{
    local $SIG{INT} = sub { die "!!!" };

    #print "$CC @CC_OPTS -c - -o $O_FILE\n";
    open(GCC, '|-') || exec $CC, @CC_OPTS, -c => '-', -o => $O_FILE;
    print GCC $header;
    if ($NO_UNLINK)
    {
	open(IFILE, '>', $I_FILE) or die "Can't open $I_FILE: $!\n";
	print IFILE $header;
    }

    $ret = parse($contents, $M_FILE);

    close IFILE if $NO_UNLINK;
    close GCC or $ret = $? >> 8;
};

if ($@ or $ret)
{
    chomp(my $error = $ret // $@);
    print "*** $O_FILE removed ($error)\n";
    unlink $O_FILE;
    $ret = 1;
}

exit $ret;


########################################################################

sub output_top_level
{
    my $str = shift;

    print IFILE $str if $NO_UNLINK;
    print GCC $str;
}


my %TYPEDEFS;
my %CLASSES;
my $CURCLASS;		 # Initialized between @implementation|@interface / @end
my $CURSUPERCLASS;	 # Initialized between @interface / @end
my $INTERFACE;		 # true between @interface / @end
my $REG_CURCLASS;

# Called for each identifier found...
sub identifier ($)
{
    my $word = shift;

    return $TOKENS{mccclass} if exists $CLASSES{$word};
    return $TOKENS{typeword} if exists $TYPEDEFS{$word};

    return $TOKENS{identifier};
}


# New type defined ?
sub add_type
{
    my($typedef, $type) = @_;

    if ($typedef =~ /^typedef/)
    {
	if ($type =~ /(\w+)/)
	{
	    $TYPEDEFS{$1} = 1;
	}
	else
	{
	    print STDERR "can't find new type name in `$type'\n";
	}
    }
}


# Add the class as type
# Allows to define a type from a class when the @interface is
# encountered and use this class in the attributes of the class itself
sub add_class_type
{
    my($class) = (shift =~ /(\w+)/);

    if (exists $CLASSES{$class})
    {
	print STDERR "class `$class' already defined!!!\n";
    }
    else
    {
	$CLASSES{$class} = {};	# But not yet defined !!!
	$TYPEDEFS{"$class\_c"} = 1;
    }
}


# Convert _Class to ((Class_c*)((special_pointer_get())[index]))
sub class2mcc
{
    (my $class = shift) =~ s/^_//;

    $class =~ s/(\s*)\z//;

    return "(($class\_c*)((id**)special_pointer_get())[__$class\__])$1";
}


#
# toto:1 + 2
# :entier
sub keywarg
{
    my($expr, $name) = @_;

    $name .= '_';

    return [ $name, $expr ];
}


#
# $ref_one vient de keywarg()
# $ref_all est une liste de keywarg()
sub keywarg_add
{
    my($ref_one, $ref_all) = @_;

    if (not defined $ref_all)
    {
	return [ $ref_one ];
    }

    push(@$ref_all, $ref_one);

    return $ref_all;
}


#
# Appel de méthode
# [ $expr $ref_msgargs ]
sub msgexpr
{
    my($expr, $ref_msgargs) = @_;

    my($context, $return, $super);
    if ($expr =~ /^(\w+)(\s*)\z/)
    {
	my($recv, $spaces_after) = ($1, $2);

	# Méthode de classe mère
	if ($recv eq 'super')
	{
	    
	    $return = class2mcc($CURCLASS) . "->oSuper$spaces_after";
	    $expr = "(void*)self";

	    $super = 1;
	}
	# Méthode de classe
	elsif ($recv eq 'isa')
	{
	    $return = $expr = "self->oIsa";
	    $return .= $spaces_after;
	}
	# Une classe déjà déclarée (via @interface)
	elsif (exists $CLASSES{$recv})
	{
	    # Sur palmos la classe est une fonction...
	    $context = 1;

	    # Sinon on peut passer par la variable globale directement
	    #$return = $expr;
	}
	# self OU Identifiant quelconque
	else
	{
	    $return = "$recv->oIsa$spaces_after";
	}
    }
    else
    {
	$context = 1;
    }

    # On doit passer par une variable temporaire
    if ($context)
    {
	my $tmp_recv = '__oTmpRecv';
	$return = "({ typeof($expr) $tmp_recv = $expr; $tmp_recv->oIsa";
	$expr = $tmp_recv;
    }

    # Le nom de la methode et les paramètres
    my @args = ($expr);
    my $selector = '_';

    # [oExpr pipo:toto toto:popi]
    if (ref $ref_msgargs)
    {
	foreach my $arg (@$ref_msgargs)
	{
	    $selector .= $arg->[0];
	    push(@args, $arg->[1]);
	}
    }
    # [oExpr pipo]
    else
    {
	$selector .= $ref_msgargs;
    }

    # Mise au propre
    $return .= "->$selector(" . join(', ', @args) . ')';

    # Méthode de la super-classe
    if ($super)
    {
	# Avec type de retour dépendant de la classe
	my $return_type = $CLASSES{$CURCLASS}{methods}{$selector}{returns};
	if (defined $return_type
	    and $return_type =~ s/%CLASS(_C|)%/$CURCLASS\L$1/g)
	{
	    substr($return, 0, 0) = "($return_type)";
	}
    }

    $return .= '; })' if $context;

    return $return;
}


#
# toto:(type)var
# toto:var
# :(type)var
# :var
sub keywdecl
{
    $_[0] .= '_';		# unaryselector
    $_[1] //= 'id';		# type

    return alias [ @_ ];
}


#
# $ref_one vient de keywdecl
# $ref_all est une liste de keywdecl
sub keywdecl_add
{
    my($ref_one, $ref_all) = @_;

    if (not defined $ref_all)
    {
	return [ $ref_one ];
    }

    push(@$ref_all, $ref_one);

    return $ref_all;
}


#
# (type)method
# (type)method:var
# (type)method:var, ...
# method
# method:var
# method:var, ...
sub methodproto
{
    my($type, $ref_method, $ellipsis) = @_;

    my %method;

    if (not defined $CURCLASS)
    {
	print STDERR ("Can't define/declare a method outside a ",
		      "\@interface/\@implementation block\n");
    }

    if (defined $type)
    {
	($method{returns} = $type) =~ s/$REG_CURCLASS/%CLASS\U$1\E%/g;
    }
    else
    {
	$method{returns} = 'id';
    }

    # Il y a des paramètres
    if (ref $ref_method)
    {
	my @params;

	# On fait précéder les méthodes d'un underscore pour éviter
	# les conflits...
	my $name = '_';

	foreach my $ref_part (@$ref_method)
	{
	    $name .= $ref_part->[0];

	    $ref_part->[1] =~ s/$REG_CURCLASS/%CLASS\U$1\E%/g;

	    #               type          , param_name
	    push(@params, [ $ref_part->[1], $ref_part->[2] ]);
	}

	push(@params, [ $ellipsis, '' ]) if $ellipsis;

	$method{name} = $name;
	$method{params} = \@params;
    }
    # Il n'y a pas de paramètre
    else
    {
	# On fait précéder les méthodes d'un underscore pour éviter
	# les conflits...
	$method{name} = '_' . $ref_method;
    }

    return \%method;
}


# Vient après methodproto
sub methoddef
{
    my($method_type, $ref_method, $after) = @_;

    if (not defined $CURCLASS)
    {
	print STDERR ("Can't define/declare a [+-]method outside a ",
		      "\@interface/\@implementation block\n");
    }

    my $class_method = $method_type =~ /^\+/ ? 1 : 0;

    (my $name = $ref_method->{name}) =~ s/(\s+)\z//s;
    my $spaces_after_method = $1 // '';

    my $method;

    # Dans le @interface
    if ($INTERFACE)
    {
	if (exists $CLASSES{$CURCLASS}{methods}{$name})
	{
	    print STDERR "method `$name' of $CURCLASS already defined\n";
	}

	$method .= "$ref_method->{returns} (*$name)";

	$method .= $class_method ? '(%CLASS_C%' : '(%CLASS%';
    }
    # Dans le @implementation
    else
    {
	if (not exists $CLASSES{$CURCLASS}{methods}{$name})
	{
	    print STDERR "method `$name' of $CURCLASS NOT defined\n";
	}

	# Méthode "abstraite", juste le pointeur à définir...
	if ($after =~ /^\s*
			\{\s*
			   (?:return\s*)?
			  self->oIsa\s*->_subclassResponsibility
							\(\s*self\s*\)\s*;\s*
			\}\s*\z/xs)
	{
	    $CLASSES{$CURCLASS}{methods}{$name}{value} = 0;
	}
	# Vraie méthode !
	else
	{
	    $CLASSES{$CURCLASS}{methods}{$name}{value} = $name;
	}

	(my $tmp = $ref_method->{returns}) =~ s/%CLASS(_C|)%/$CURCLASS\L$1/g;
	$method = "static $tmp $name";

	$method .= "($CURCLASS";
	$method .= '_c' if $class_method;
    }

    $method .= '*self';

    # Les paramètres
    if (exists $ref_method->{params})
    {
	my $type;

	foreach my $ref_param (@{$ref_method->{params}})
	{
	    $method .= ", ";

	    # [0] = type / [1] = param_name
	    if ($INTERFACE)
	    {
		# Si le type est la classe Class ou Class_c
		# on remplace par %CLASS% ou %CLASS_C%
		($type = $ref_param->[0])
		    =~ s/$REG_CURCLASS/%CLASS\U$1\E%/g;
	    }
	    else
	    {
		# L'inverse
		($type = $ref_param->[0]) =~ s/%CLASS(_C|)%/$CURCLASS\L$1/g;
	    }

	    if ($type =~ /(.*?)(\[.+)/)
	    {
		$method .= "$1 $ref_param->[1] $2";
	    }
	    else
	    {
		$method .= "$type $ref_param->[1]";
	    }
	}
    }
    $method .= ')';

    if ($INTERFACE)
    {
	# Les %CLASS_C% et %CLASS% seront à remplacer par $CURCLASS(_c|)

	# Le type de retour sans passage à la ligne
	($CLASSES{$CURCLASS}{methods}{$name}{returns} = $ref_method->{returns})
	    =~ tr/\r\n/ /;

	push(@{$CLASSES{$CURCLASS}{class_vars}}, $method . ';');

	# On supprime tout le texte, rien n'est déclaré
	$method =~ tr/ \t\n\r//cd;
	$after =~ tr/ \t\n\r//cd;
    }
    else
    {
	# Méthode "abstraite" !!!
	if ($CLASSES{$CURCLASS}{methods}{$name}{value} eq '0')
	{
	    # On supprime tout le texte, rien n'est défini
	    $method =~ tr/ \t\n\r//cd;
	    $after =~ tr/ \t\n\r//cd;
	}
    }

    $method .= $spaces_after_method . $after;

    return $method;
}


#
# @implementation class
sub atimplementation
{
    my($atimplementation, $class) = @_;

    if (defined $CURCLASS)
    {
	if ($INTERFACE)
	{
	    print STDERR "class $class defined in \@interface / \@end\n";
	}
	else
	{
	    print STDERR "class $class defined in \@implementation / \@end\n";
	}
    }

    if (not exists $DEF_CLASSES{$class})
    {
	print STDERR "class $class not defined in `$CLASSES_FILE'\n";
    }

    $CURCLASS = $class;		# @implementation
    $CURSUPERCLASS = $CLASSES{$class}{super};
    $REG_CURCLASS = qr/\b$class(\_c|)\b/;

    (my $return = $atimplementation . $class) =~ s/\S+//gs;

    return $return;
}


#
# @interface class
sub atinterface
{
    my($class_zz, $super_zz, $attributes, $class_vars) = @_;

    my($class) = ($class_zz =~ m/(\w+)/);

    my $super;
    ($super) = ($super_zz =~ m/(\w+)/) if defined $super_zz;

    if (defined $CURCLASS)
    {
	if ($INTERFACE)
	{
	    print STDERR "class $class defined in \@interface / \@end\n";
	}
	else
	{
	    print STDERR "class $class defined in \@implementation / \@end\n";
	}
    }

    if (not exists $DEF_CLASSES{$class})
    {
	print STDERR "class $class not defined in `$CLASSES_FILE'\n";
    }

    if (exists $CLASSES{$class}{defined})
    {
	print STDERR "class $class already defined\n";
    }

    if (defined $super and not exists $CLASSES{$super})
    {
	print STDERR "superclass $super not yet defined\n";
    }

    my %class;
    my @attributes;

    $REG_CURCLASS = qr/\b$class(\_c|)\b/;

    # Une super classe
    if (defined $super)
    {
	$class{super} = $super;

	# Les attributs (ils existent toujours dans super)
	$class{attributes} = [ @{$CLASSES{$super}{attributes}} ];

	# Les variables de classe
	if (defined $CLASSES{$super}{class_vars})
	{
	    $class{class_vars} = [ @{$CLASSES{$super}{class_vars}} ];

	    if (defined $class_vars)
	    {
		# Si un type est la classe Class ou Class_c
		# on remplace par %CLASS% ou %CLASS_C%
		$class_vars =~ s/$REG_CURCLASS/%CLASS\U$1\E%/g;

		$class_vars =~ s/\{//;
		$class_vars =~ s/\}(\s*)\z/$1/s;

		push(@{$class{class_vars}}, $class_vars);
	    }
	}

	# On récupère les méthodes de la classe mère
	if (exists $CLASSES{$super}{methods})
	{
	    $class{methods} = { %{$CLASSES{$super}{methods}} };
	}
    }
    # Pas de super classe
    else
    {
	# L'attribut obligatoire de base
	$class{attributes} = [ '%CLASS_C% *oIsa;' ];

	# Des variables de classe
	$class{class_vars} = [ $class_vars ] if defined $class_vars;
    }

    # La suite des attributs
    if (defined $attributes)
    {
	# Si un type est la classe Class ou Class_c
	# on remplace par %CLASS% ou %CLASS_C%
	$attributes =~ s/$REG_CURCLASS/%CLASS\U$1\E%/g;

	$attributes =~ s/\{//;
	$attributes =~ s/\}(\s*)\z/$1/s;

	push(@{$class{attributes}}, $attributes);
    }

    $class{defined} = 1;
    $CLASSES{$class} = \%class;

    $CURCLASS = $class;		# @interface
    $CURSUPERCLASS = $super;
    $INTERFACE = 1;

    $TYPEDEFS{"$class\_c"} = 1;

    # Les attributs
    my $return = "typedef struct $class\_class $class\_c; "
	. "typedef struct $class\_object $class; "
	. "struct $class\_object { ";

    $return .= join('', @{$class{attributes}});

    # On remplace toutes les occurences de %CLASS% et de %CLASS_C% par
    # la classe courante
    $return =~ s/%CLASS(_C|)%/$class\L$1/g;

    $return .= "};";

    return $return;
}


#
# @end
sub atend
{
    # On supprime le @end
    (my $at_end = shift) =~ tr/@end//d;

    if (not defined $CURCLASS)
    {
	print STDERR "\@end but no \@interface nor \@implementation opened\n";
    }

    # On était dans un @interface
    if ($INTERFACE)
    {
	# Pour les classes de base on considère que le type de la
	# classe parente est le même que la classe
	my $super = $CURSUPERCLASS // $CURCLASS;

	# La classe Class_c
	$at_end .= "struct $CURCLASS\_class { $CURCLASS\_c *oIsa; "
	    . "$super\_c *oSuper; UInt16 uh_size; const char *pa_name; ";

	# Variables de classes et pointeurs sur méthodes
	if (defined $CLASSES{$CURCLASS}{class_vars})
	{
	    (my $class_vars = join('', @{$CLASSES{$CURCLASS}{class_vars}}))
		=~ s/%CLASS(_C|)%/$CURCLASS\L$1/g;

	    $class_vars =~ tr/\r\n/ /; # Tout sur une seule ligne

	    $at_end .= $class_vars;
	}

	$at_end .= "};";
    }
    else
    {
	# Pour les classes de base, on n'a pas de classe parente !
	# (undef si c'est le cas)
	my $super = $CURSUPERCLASS;

	# La méthode d'initialisation de la classe : initialize_Class()
	$at_end .= "void *initialize_" . $CURCLASS
	    . "(void **poClasses, Boolean b_globals) {";

	if (defined $super)
	{
	    $at_end .=
		"$super\_c *oSuperClass = ($super\_c*)poClasses[__$super\__];";
	}
	
	(my $initialize = <<END_OF_CODE) =~ tr/ \t\r\n/ /s; # Tout/une 1 ligne
  $CURCLASS\_c *oClass = ($CURCLASS\_c*)MemPtrNew(sizeof($CURCLASS\_c));
  if (oClass == 0)
    return 0;
END_OF_CODE

	$at_end .= $initialize;

	# Avec classe de base
	if (defined $super)
	{
	    ($initialize = <<END_OF_CODE) =~ tr/ \t\r\n/ /s; # Tout/une 1 ligne
  MemMove(oClass, oSuperClass, sizeof(*oSuperClass));
  MemSet((char*)oClass + sizeof(*oSuperClass),
	 sizeof(*oClass) - sizeof(*oSuperClass), '\\0');
  oClass->oSuper = oSuperClass;
END_OF_CODE

	    $at_end .= $initialize;
	}
	# Sans classe de base
	else
	{
	    $at_end .= "MemSet((char*)oClass, sizeof(*oClass), '\\0');"
	}


	($initialize = <<END_OF_CODE) =~ tr/ \t\r\n/ /s; # Tout sur une 1 ligne
  oClass->oIsa = oClass;
  oClass->uh_size = sizeof(*oClass);
  oClass->pa_name = "$CURCLASS";
END_OF_CODE

	$at_end .= $initialize;

	# Si les methodes initialize: et deinitialize: n'ont pas été
	# redéfinie par la classe elle-même, on prend celles de Object
	# qui ne font rien
	$at_end .= "{ ";
	my $object_declared;
	foreach my $init_method (qw(_initialize_ _deinitialize_))
	{
	    if (not exists $CLASSES{$CURCLASS}{methods}{$init_method}{value})
	    {
		if (not defined $object_declared)
		{
		    $at_end .= "Object_c *oObjectClass "
			. "= (Object_c *)poClasses[__Object__]; ";
		    $object_declared++;
		}

		$at_end .= "oClass->$init_method "
		    . "= (void(*)($CURCLASS\_c*,Boolean))"
		    . "oObjectClass->$init_method; ";
	    }
	}
	$at_end .= "} ";

	# Les méthodes
	while (my($method_name, $ref_infos)
	       = each %{$CLASSES{$CURCLASS}{methods}})
	{
	    # la méthode a été [re-]définie dans cette classe
	    # ET n'est pas virtuelle
	    if (exists $ref_infos->{value} and $ref_infos->{value} ne '0')
	    {
		$at_end .= "oClass->$method_name = $ref_infos->{value}; "
	    }
	}

	$at_end .= "oClass->_initialize_(oClass, b_globals); "
	    . "return (void*)oClass; "
	    . "}";
    }

    undef $CURCLASS;
    undef $CURSUPERCLASS;
    undef $INTERFACE;
    undef $REG_CURCLASS;

    return $at_end;
}


sub mcc_join
{
    # If some spaces appear from lex, add them to the last string of this join()
    discard_last_lex_token_if($_[-1]);

    return join('', @_);
}

__END__
__C__
#include <stdarg.h>

extern char *gpa_global_buffer;
extern int  gi_global_buffer_len;
extern char gra_global_filename[];

int parse(SV *ps_buffer, char *pa_filename)
{
  STRLEN len;

  gpa_global_buffer = SvPV(ps_buffer, len);
  gi_global_buffer_len = len;

  strcpy(gra_global_filename, pa_filename);

  return yyparse();
}

void yyerror(char *pa_error)
{
  extern int inlineno;
  fprintf(stderr, "%s:%u %s\n", gra_global_filename, inlineno, pa_error);
}

void discard_last_lex_token_if(SV *ps_token)
{
  extern SV *ps_last_lex_token;

  if (ps_last_lex_token == ps_token)
    ps_last_lex_token = NULL;
}

// man perlcall
SV *perlsubf(char *pa_func, char *pa_format, ...)
{
  dSP;
  char *pa_arg, c;
  SV *ps_ret;
#define PERLSUB_SV	0
#define PERLSUB_INT	1
#define PERLSUB_NOT_SV	2
  int i_ret_type = PERLSUB_SV;
  va_list ap;

  ENTER;
  SAVETMPS;

  va_start(ap, pa_format);

  PUSHMARK(SP);
  while ((c = *pa_format++) != '\0')
    switch (c)
    {
    case 'I':
      i_ret_type = PERLSUB_INT;
      break;

    case 'A':
      i_ret_type = PERLSUB_NOT_SV;
      break;

    default:
      printf("Unknown format char `%c' use `u' instead\n", c);

      // Continue...

    case 'u': case 'd':
      mXPUSHi(va_arg(ap, int));
      break;

    case 's':
      XPUSHs(sv_2mortal(newSVpv(va_arg(ap, char *), 0)));
      break;

    case 'c':
    {
      char c = va_arg(ap, u_int);
      XPUSHs(sv_2mortal(newSVpvn(&c, 1)));
    }
    break;

    case 'P':
      XPUSHs(va_arg(ap, SV *));
      break;
    }
  PUTBACK;

  va_end(ap);

  call_pv(pa_func, G_SCALAR);
  
  SPAGAIN;

  switch (i_ret_type)
  {
  case PERLSUB_INT:
    ps_ret = POPs;
    ps_ret = (SV*)SvIV(ps_ret);
    break;

  case PERLSUB_NOT_SV:
    ps_ret = POPs;
    break;

  default:
    ps_ret = SvREFCNT_inc(POPs);
    break;
  }

  PUTBACK;
  FREETMPS;
  LEAVE;

  return ps_ret;
}


SV *perlsubn(char *pa_func, ...)
{
  dSP;
  char *pa_arg, c;
  SV *ps_arg, *ps_ret;
  int i_int_ret = 0;
  va_list ap;

  ENTER;
  SAVETMPS;

  va_start(ap, pa_func);

  PUSHMARK(SP);
  while ((ps_arg = va_arg(ap, SV *)) != NULL)
    XPUSHs(ps_arg);
  PUTBACK;

  va_end(ap);

  call_pv(pa_func, G_SCALAR);
  
  SPAGAIN;

  ps_ret = SvREFCNT_inc(POPs);

  PUTBACK;
  FREETMPS;
  LEAVE;

  return ps_ret;
}
