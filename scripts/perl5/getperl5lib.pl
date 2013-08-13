#!perl
#Initialise les variables d'environnement pour les lanceurs.

#module perl
#use File::Basename;

sub basename
{
    my $ret=$_[0];
    $ret=~s/.*[\/\\]//;
    return $ret
}

sub dirname
{
    my $ret=$_[0];
    $ret=~s/[\/\\][^\/\\]+$//;
    return $ret;
}


#Appel la fonction main
main();

sub getSystelIni
#Definition : Determine le systel.ini valide (soit par defaut dans 
#            systel/config/systel.ini soit si elle existe 
#            la variable d'environnement SYSTELINI qui contient 
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
	$line=clearLine($line);
	if($line=~/\[.+\]/)
	{
	    last;   #On sort si on trouve un autre bloc.
	}
	if($line=~/.+=.*/)
	{
	    #Le bloc est bien de la forme "variable=valeur", donc on traite.
	    my $var=$line;
	    $var=~s/=.*//;
	    $var=~s/ \t//g;
	    my $val=$line;
	    $val=~s/.*=//;
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

sub main
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

    my %hash;
    %hash=fillHashFromBloc($systelIni,"GENERAL",%hash);
    %hash=fillHashFromBloc($systelIni,"PERL",%hash);
    %hash=fillHashFromBloc($systelIni,$hash{"HOSTTYPE"},%hash);

#    foreach $key (keys(%hash))
#    {
#	printf "wa have '$key' = '$hash{$key}'\n";
#    }
    if($ENV{"PERL5LIB"})
    {
	printf $ENV{"PERL5LIB"};
    }
    else
    {
	printf $hash{"PERL5LIB"};
    }
}

