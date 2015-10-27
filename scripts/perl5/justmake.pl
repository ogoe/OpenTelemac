#!perl
#____________________________________________________________
#
# Generation a scalar version of the TELEMAC system
# See makeallpar90.pl for parallel version
#
# Mis a jour pour TELEMAC V6P1 - F. Decung - 23/02/2010
# Update for scalar version only - P. LANG - 01/07/2010
#
# Original version : DeltaCAD/SA - Mars 1999
#____________________________________________________________


#--------------------Tableau de description des repertoires

@dirs=(
#Libs : Bief, Damocles, Parallel, Special, Mumps
        "sources|utils|special",
        "sources|utils|parallel",
        "sources|utils|damocles",
        "sources|utils|hermes",
        "sources|utils|bief",
        "sources|utils|partel",
        "sources|utils|gretel",
        "sources|utils|diffsel",
#dredgesim
        "sources|dredgesim",
#waqtel
        "sources|waqtel",
#Sisyphe
	"sources|sisyphe",
#Tomawac
	"sources|tomawac",
#T2d
	"sources|telemac2d",
#T3d
	"sources|telemac3d",
#Artemis
	"sources|artemis",
#Estel3d
	"sources|estel3d",
#Postel3d
	"sources|postel3d",
#Stbtel
	"sources|stbtel",
#Api
	"sources|api",

);

#--------------------Usage----------------------------

sub usage
{   printf "\n\nUsage: makeall90 [-y]\n";
    printf "       -y : user confirm mode\n\n";
    exit;
}

#--------------------RunMake---------------------------

sub RunMake
{
    $curdir=$_[0];
    $make=0;

    if($askUser==0)                              #Pas de demande au user
	{   printf "===== Making : $curdir\n";
	    $make=1;
	}
    else
	{    printf "\nDo you confirm making $curdir [y(yes)/s(skip)]  ";
	    if(<STDIN>=~/y/)
	    {                                    #Confirmation par le user.
		printf "Making : $curdir\n";
		$make=1;
	    }
	    else                                 #ne confirm pas: on saute
	    {	printf "Skipping : $curdir\n";
		$make=0;
	    }
	}

#Si OK on lance le traitement du makefile
    if($make==1)
    	{
#  	system ("maktel menage");
 	$error = system ("maktel install");
        if($error != 0)
        {
          die "===== $curdir : Error while running maktel install\n"
        }
        printf "===== $curdir : maktel install\n";
#
# Pour les libs faire en plus les libs Debug et Profile
# Commente, peu utilise...
#       	if  ( ! ( $curdir =~ /share/ ) )
#       	{
#                   printf "===== $curdir : maktel libdebug menage\n";
#                   system ("maktel libdebug");
#                   system ("maktel menage");
#                   printf "===== $curdir : maktel libprofile menage\n";
#                   system ("maktel libprofile");
#                   system ("maktel menage");
#		}
	printf "\n\n";
    	}
}

#--------------------Programme principal--------------

    printf "\n\n\n";
    printf "Ready for  making System TELEMAC90\n";

    $host=`gethosttype`;

#separateur Unix/NT
    $ps="/";
    if($host eq "win")     { $ps="\\";}

#Option interactif/automatique
    $askUser=0;
    if($ARGV[0])
    {
	if($ARGV[0] eq "-y")  { printf "User confirm mode.\n\n";
	                        $askUser=1;
                              }
	else                  { printf "Invalid switch.\n\n";
	                        usage();
	                      }
    }
    else    { printf "Automatic mode.\n\n"; }


    $project=`getproject`;
    chdir($project);

#Traitement de tous les repertoires listes

    foreach $path (@dirs)
    {
        $path =~ s/\|/$ps/g;                      # mettre le separateur de path
        $default_path = $path;
#~~~> SBO@HRW: sources are now within $ps.sources, not $ps."sources_"."$host"
#        $path = "$path"."_"."$host";
        $path = "$path";
#~~~<

        if ( ! chdir("$path") )
           {
           printf "Repertoire '$path' inexistant \n";

           if ( ! chdir("$default_path") )
              {
           	printf "Repertoire '$default_path' inexistant ! (Ignore)\n";
              }
           else
              {
           	printf "\n\n             ========== $default_path :\n\n";
	        RunMake($project.$ps.$default_path);
	        chdir($project);
              }
           }
        else
           {
           	printf "\n\n             ========== $path :\n\n";
	        RunMake($project.$ps.$path);      #Traite le repertoire courant
	        chdir($project);
	   }
    }

#Fin
    printf "\n\nEnd of make\n";

