#!perl
#---------------------------------------------------------------
#
# Construit le fichier "cfgmak.mak" exploite dans les Makefile
#  (commande "maktel").
#
# Ce fichier est construit a partir des definitions faites dans
# le fichier de configuration "systel.ini"
#
#-----------------------------------------------------DeltaCAD--

#module perl
use File::Basename;

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

    #On cherche la presence de la variable SYSTELCFG, sinon on prend
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
    }
    return $systelIni;
}

sub clearLine
#$_[0]      : La ligne a nettoyer
#Definition : Enleve le commentaire, les espaces et tab et le new line.
#Return     : La ligne nettoye.
{
    $_[0]=~s/#.*//;         #Enleve un eventuelle commentaire.

    chomp($_[0]);           #Enleve le \n a la fin.
    return $_[0];
}

sub fillHashFromBloc
#$_[0]      : Le chemin complet du fichier a lire.
#$_[1]      : Le nom du bloc a considerer.
#$_[2]      : La "hashe table" ou seront stocker comme cles
#            les variables lues et comme valeurs leurs valeurs lues.
#Definition : Lit le bloc $_[1] du fichier systelIni.
#Return     : La nouvelle "hashe table".
{
    my ($systelIni,$blocName,%hash);
    ($systelIni,$blocName,%hash)=@_;

    open(FILE,"<$systelIni");

    #Cherche le bloc $blocName.
    while(eof(FILE)==0)
    {
	my $line=<FILE>;
	$line=clearLine($line);
	if($line=~/\[$blocName\]/)
	{
	    last;
	}
    }

    #On quitte si c'est la fin du fichier.
    if(eof(FILE))
    {
	printf "Error or undefined $blocName bloc in $systelIni\n";
	exit;
    }


    #Maintenant on lit tous le bloc $blocName en stockant chaque variable
    #dans la "hashe table". On s'arrete au bloc suivant.
    while(eof(FILE)==0)
    {
	my $line=<FILE>;
	$intline=clearLine($line);
	if($intline=~/\[.+\]/)
	{
	    last;   #On sort si on trouve un autre bloc.
	}
	if($intline=~/.+=.*/)
	{

 #Le bloc est bien de la forme "variable=valeur", donc on traite.

	    my $int2lin=$line;
            $int2lin=~s/#.*//;         #Enleve un eventuelle commentaire.
            chomp($_[0]);              #Enleve le \n a la fin.

	    $_ = $int2lin;             #Decomposer (mot_clef, valeur)
	    ($var, $val) = /^\s*(.*?)\s*=\s*(.*).*$/;


	    $var=~s/ \t//g;            #Eliminer spaces + tabs

	    if($val=~/".+"/)
	    {
		$val=~s/[ \t]*"//;     #"
		$val=~s/"[ \t]*//;     #"
	    }
	    else
	    {
		$val=~s/[ \t"]*//g;   #"
	    }
#	    printf "We have $var = $val\n";
	    $hash{$var}=$val;
	}
    }
    close(FILE);
    return %hash;
}

#----------------- Main
{
    my ($curDir,$cfgmak);

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

    my %hash;
    %hash=fillHashFromBloc($systelIni,"GENERAL",%hash);
    %hash=fillHashFromBloc($systelIni,"PERL",%hash);
    %hash=fillHashFromBloc($systelIni,$hash{"HOSTTYPE"},%hash);


    $cfgmak=dirname($systelIni).$ps."cfgmak.mak";
    open(F,">$cfgmak");

    printf F "# \n";
    printf F "# THIS IS A GENERATED FILE.  ";
    printf F "  DO NOT DIRECTLY EDIT THIS FILE.\n";
    printf F "# \n";
    foreach $key (keys(%hash))
    {
      my $val=$hash{$key};
      $val =~ s/<TELEMAC_HOME>/$hash{"PROJECT"}/;
      $val =~ s/<DIRLIB>/$hash{"DIRLIB"}/;
      $val =~ s/<VERSARTE>/$hash{"VERSARTE"}/;

      printf F "$key = $val\n\n";
    }

    close(F);

    print "Mise a jour du fichier '$cfgmak' terminee.\n\n";
}
