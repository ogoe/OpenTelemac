#!perl
#---------------------------------------------------------------
#
# Renvoi le chemin du repertoire de configuration
# avec gestion de la redirection via la variable
# d'environnement  SYSTELCFG
#
#-----------------------------------------------------DeltaCAD--

#module perl

sub dirname
{
    my $ret=$_[0];
    $ret=~s/[\/\\][^\/\\]+$//;
    return $ret;
}

sub getSystelIni
#Definition : Determine le systel.ini valide (soit par defaut dans 
#            systel/config/systel.ini soit si elle existe 
#            la variable d'environnement SYSTELCFG qui contient 
#            le chemine complet du fichier)
#Return     : Le chemin complet du systel.ini valide.
{
    my ($systelIni,$ps);

    if($ENV{"OS"} eq "Windows_NT") 
    {
	#Cas du systeme Windows NT
	$ps="\\";
    }
    else
    {
	#Cas du system Unix
	$ps="/";
    }

    #On cherche la presence de la variable systelini, sinon on prend
    #le fichier de config par defaut (configuration de l'install).
    if($ENV{"SYSTELCFG"})
    {
	$systelIni=$ENV{"SYSTELCFG"}.$ps."systel.ini";
#	printf "Using custom configuration file : $systelIni\n";
    }
    else
    {
	#Recupere le premier argument envoye par le shell pour determiner 
	#le chemin racine du projet et le nom du repertoire d'ou est lancer
	#install pour connaitre la plateforme.
	$systelIni=dirname($0);
	$systelIni=dirname($systelIni);
	$systelIni="$systelIni$ps"."config$ps"."systel.ini";
#	printf "Using default configuration file : $systelIni\n";
    }
    return $systelIni;
}

{
    my ($curDir);

    if($ENV{"OS"} eq "Windows_NT") 
    {
	#Cas du systeme Windows NT
	$ps="\\";
	$curDir=`cd`;
    }
    else
    {
	#Cas du system Unix
	$ps="/";
	$curDir=`pwd`;
    }

    $0=~s#\.[/\\]##g;
    #On rajoute si besoin est le chemin a $0
    if(!($0=~/[\\\/]/))
    {
	chomp($curDir);
	$0=$curDir.$ps.$0;
    }

    my $systelIni=getSystelIni();

    $systelIni=dirname($systelIni);
    printf $systelIni."$ps\n";
}











 
