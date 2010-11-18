C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE ARRAYS WITH PHYSICAL PARAMETERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DPI, LT, NPC, NPM, NPV, NVCOU, NVHMA, TC1, TC2, TM1, TM2, TV1, TV2
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::ALPHIL ALPHIL@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINCOU BINCOU@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINMAR BINMAR@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINVEN BINVEN@endlink, 
!> @link DECLARATIONS_TOMAWAC::COUSTA COUSTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::DDC DDC@endlink, 
!> @link DECLARATIONS_TOMAWAC::DEPTH DEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::DONTEL DONTEL@endlink, 
!> @link DECLARATIONS_TOMAWAC::DZHDT DZHDT@endlink, 
!> @link DECLARATIONS_TOMAWAC::E2FMIN E2FMIN@endlink, 
!> @link DECLARATIONS_TOMAWAC::F1 F1@endlink, 
!> @link DECLARATIONS_TOMAWAC::FETCH FETCH@endlink, 
!> @link DECLARATIONS_TOMAWAC::FPIC FPIC@endlink, 
!> @link DECLARATIONS_TOMAWAC::FRABI FRABI@endlink, 
!> @link DECLARATIONS_TOMAWAC::FREMAX FREMAX@endlink, 
!> @link DECLARATIONS_TOMAWAC::FREQ FREQ@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMMA GAMMA@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRAVIT GRAVIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::HM0 HM0@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDHMA IDHMA@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDTEL IDTEL@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIC INDIC@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIM INDIM@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIV INDIV@endlink, 
!> @link DECLARATIONS_TOMAWAC::INISPE INISPE@endlink, 
!> @link DECLARATIONS_TOMAWAC::MAREE MAREE@endlink, 
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink, 
!> @link DECLARATIONS_TOMAWAC::NBOR NBOR@endlink, 
!> @link DECLARATIONS_TOMAWAC::NF NF@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPTT NPTT@endlink, 
!> @link DECLARATIONS_TOMAWAC::RAISF RAISF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDEPTH SDEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZHDT SDZHDT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SF SF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFR SFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMAA SIGMAA@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMAB SIGMAB@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRED1 SPRED1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRED2 SPRED2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST1 ST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::STETA STETA@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA31 STRA31@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC SUC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC1 SUC1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC2 SUC2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUV SUV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUV1 SUV1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUV2 SUV2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC SVC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC1 SVC1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC2 SVC2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVV SVV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVV1 SVV1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVV2 SVV2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXRELC SXRELC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXRELM SXRELM@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXRELV SXRELV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SYRELC SYRELC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SYRELM SYRELM@endlink, 
!> @link DECLARATIONS_TOMAWAC::SYRELV SYRELV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SZM1 SZM1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SZM2 SZM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA TETA@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA1 TETA1@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA2 TETA2@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA01 TRA01@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENSTA VENSTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENT VENT@endlink, 
!> @link DECLARATIONS_TOMAWAC::VX_CTE VX_CTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::VY_CTE VY_CTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCOB WACCOB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCOF WACCOF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACMAB WACMAB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACMAF WACMAF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACVEB WACVEB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACVEF WACVEF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WAC_FILES WAC_FILES@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLAMDA XLAMDA@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CHDON, IBID, IFREQ, IPLAN, NP0, NP1, NP2, NP3, NP4, NP5, NPOIN_ALL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ANACOS(), ANAMAR(), ANAVEN(), LECDOI(), LECDON(), LECHAM(), OV(), PLANTE(), SPEINI()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.0                                       </center>
!> </td><td> 25/08/2000
!> </td><td>
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHIL
!></td><td>--></td><td>CONSTANTE DE PHILLIPS (ALPHA)
!>    </td></tr>
!>          <tr><td>AT
!></td><td><--</td><td>TEMPS DU CALCUL
!>    </td></tr>
!>          <tr><td>BINCOU
!></td><td>--></td><td>BINAIRE DU FICHIER DES COURANTS
!>    </td></tr>
!>          <tr><td>BINMAR
!></td><td>--></td><td>BINAIRE DU FICHIER DES HAUTEURS DE LA MAREE
!>    </td></tr>
!>          <tr><td>BINVEN
!></td><td>--></td><td>BINAIRE DU FICHIER DES VENTS
!>    </td></tr>
!>          <tr><td>COUSTA
!></td><td>--></td><td>LOGIQUE INDIQUANT UN COURANT STATIONNAIRE
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DE DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>DONTEL
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON RECUPERE UNE DON.TEL.
!>    </td></tr>
!>          <tr><td>DPI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>E2FMIN
!></td><td>--></td><td>VALEUR MINIMALE D'ENERGIE
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>DENSITE SPECTRALE D'ENERGIE
!>    </td></tr>
!>          <tr><td>F1
!></td><td>--></td><td>FREQUENCE MINIMALE
!>    </td></tr>
!>          <tr><td>FETCH
!></td><td>--></td><td>FETCH MOYEN
!>    </td></tr>
!>          <tr><td>FPIC
!></td><td>--></td><td>FREQUENCE DE PIC JONSWAP
!>    </td></tr>
!>          <tr><td>FREMAX
!></td><td>--></td><td>VALEUR MAXIMUM DE LA FREQUENCE DE PIC
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td><--</td><td>FREQUENCES DISCRETISEES
!>    </td></tr>
!>          <tr><td>GAMMA
!></td><td>--></td><td>FACTEUR DE FORME DE PIC JONSWAP
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>HM0
!></td><td>--></td><td>HAUTEUR SIGNIFICATIVE JONSWAP
!>    </td></tr>
!>          <tr><td>IDTEL
!></td><td>--></td><td>RANG DE LA DONNEE TELEMAC A RECUPERER
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>FORMAT DU FICHIER DES COURANTS
!>    </td></tr>
!>          <tr><td>INDIV
!></td><td>--></td><td>FORMAT DU FICHIER DES HAUTEURS
!>    </td></tr>
!>          <tr><td>INISPE
!></td><td>--></td><td>INDICATEUR D'INITIALISATION DU SPECTRE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAREE
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON CONSIDERE LA MAREE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPC
!></td><td>--></td><td>NOMBRE DE POINTS DU FICHIER DES COURANTS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>NPM
!></td><td>--></td><td>NOMBRE DE POINTS DU FICHIER DES HAUTEURS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>---</td><td>NPOIN3*NPRIV
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NPTT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS DU FICHIER TELEMAC
!>    </td></tr>
!>          <tr><td>NPV
!></td><td>--></td><td>NOMBRE DE POINTS DU FICHIER DES VENTS
!>    </td></tr>
!>          <tr><td>NVCOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVHMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU POUR L'UTILISATEUR DE DIMENSION
!>    </td></tr>
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE
!>    </td></tr>
!>          <tr><td>SIGMAA
!></td><td>--></td><td>VALEUR DE SIGMA JONSWAP POUR F
!>    </td></tr>
!>          <tr><td>SIGMAB
!></td><td>--></td><td>VALEUR DE SIGMA JONSWAP POUR F > FP
!>    </td></tr>
!>          <tr><td>SPRED1
!></td><td>--></td><td>ETALEMENT DIRECTIONNEL 1 POUR FRA
!>    </td></tr>
!>          <tr><td>SPRED2
!></td><td>--></td><td>ETALEMENT DIRECTIONNEL 2 POUR FRA
!>    </td></tr>
!>          <tr><td>TC1
!></td><td><--</td><td>TEMPS CORRESPONDANT AU COURANT 1
!>    </td></tr>
!>          <tr><td>TC2
!></td><td><--</td><td>TEMPS CORRESPONDANT AU COURANT 2
!>    </td></tr>
!>          <tr><td>TETA
!></td><td><--</td><td>DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>TETA1
!></td><td>--></td><td>DIRECTION PRINCIPALE 1 POUR FRA
!>    </td></tr>
!>          <tr><td>TETA2
!></td><td>--></td><td>DIRECTION PRINCIPALE 2 POUR FRA
!>    </td></tr>
!>          <tr><td>TM1
!></td><td><--</td><td>TEMPS CORRESP. A LA HAUTEUR DE LA MAREE 1
!>    </td></tr>
!>          <tr><td>TM2
!></td><td><--</td><td>TEMPS CORRESP. A LA HAUTEUR DE LA MAREE 2
!>    </td></tr>
!>          <tr><td>TRA31
!></td><td><-></td><td>TABLEAUX DE TRAVAIL REELS
!>    </td></tr>
!>          <tr><td>TV1
!></td><td><--</td><td>TEMPS CORRESPONDANT AU VENT 1
!>    </td></tr>
!>          <tr><td>TV2
!></td><td><--</td><td>TEMPS CORRESPONDANT AU VENT 2
!>    </td></tr>
!>          <tr><td>UC,VC
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE COURANT
!>    </td></tr>
!>          <tr><td>UC1,VC1
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE COURANT INFERIEUR
!>    </td></tr>
!>          <tr><td>UC2,VC2
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE COURANT SUPERIEUR
!>    </td></tr>
!>          <tr><td>UV,VV
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE VENT INITIAL
!>    </td></tr>
!>          <tr><td>UV1,VV1
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE VENT INFERIEUR
!>    </td></tr>
!>          <tr><td>UV2,VV2
!></td><td><--</td><td>COMPOSANTES DU CHAMP DE VENT SUPERIEUR
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON CONSIDERE UN VENT
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>XLAMDA
!></td><td>--></td><td>FACTEUR DE PONDERATION POUR LA FRA
!>    </td></tr>
!>          <tr><td>XRELC,YRELC
!></td><td>--></td><td>COORDONNEES DES POINTS DES COURANTS RELEVES
!>    </td></tr>
!>          <tr><td>XRELM,YRELM
!></td><td>--></td><td>COORDONNEES DES POINTS DES HAUTEURS RELEVEES
!>    </td></tr>
!>          <tr><td>XRELV,YRELV
!></td><td>--></td><td>COORDONNEES DES POINTS DES VENTS RELEVES
!>    </td></tr>
!>          <tr><td>ZM
!></td><td><--</td><td>HAUTEUR DE LA MAREE INITIALE
!>    </td></tr>
!>          <tr><td>ZM1,ZM2
!></td><td><--</td><td>HAUTEURS DE LA MAREE INFERIEURE ET SUPERIEURE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONDIW
     &( AT, LT , DPI, TC1, TC2, NPC , TV1, TV2, NPV, TM1, TM2 , NPM ,
     &  NVHMA  , NVCOU )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHIL         |-->| CONSTANTE DE PHILLIPS (ALPHA)
C| AT             |<--| TEMPS DU CALCUL
C| BINCOU         |-->| BINAIRE DU FICHIER DES COURANTS
C| BINMAR         |-->| BINAIRE DU FICHIER DES HAUTEURS DE LA MAREE
C| BINVEN         |-->| BINAIRE DU FICHIER DES VENTS
C| COUSTA         |-->| LOGIQUE INDIQUANT UN COURANT STATIONNAIRE
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| DONTEL         |-->| LOGIQUE INDIQUANT SI ON RECUPERE UNE DON.TEL.
C| DPI            |---| 
C| E2FMIN         |-->| VALEUR MINIMALE D'ENERGIE
C| F             |<--| DENSITE SPECTRALE D'ENERGIE
C| F1             |-->| FREQUENCE MINIMALE
C| FETCH          |-->| FETCH MOYEN
C| FPIC           |-->| FREQUENCE DE PIC JONSWAP
C| FREMAX         |-->| VALEUR MAXIMUM DE LA FREQUENCE DE PIC
C| FREQ           |<--| FREQUENCES DISCRETISEES
C| GAMMA          |-->| FACTEUR DE FORME DE PIC JONSWAP
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| HM0            |-->| HAUTEUR SIGNIFICATIVE JONSWAP
C| IDTEL          |-->| RANG DE LA DONNEE TELEMAC A RECUPERER
C| INDIC          |-->| FORMAT DU FICHIER DES COURANTS
C| INDIV          |-->| FORMAT DU FICHIER DES HAUTEURS
C| INISPE         |-->| INDICATEUR D'INITIALISATION DU SPECTRE
C| LT             |---| 
C| MAREE          |-->| LOGIQUE INDIQUANT SI ON CONSIDERE LA MAREE
C| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPC            |-->| NOMBRE DE POINTS DU FICHIER DES COURANTS
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE PROPAGATION
C| NPM            |-->| NOMBRE DE POINTS DU FICHIER DES HAUTEURS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NPRIV          |---| NPOIN3*NPRIV
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD
C| NPTT           |-->| NUMERO DU PAS DE TEMPS DU FICHIER TELEMAC
C| NPV            |-->| NOMBRE DE POINTS DU FICHIER DES VENTS
C| NVCOU          |---| 
C| NVHMA          |---| 
C| PRIVE          |-->| TABLEAU POUR L'UTILISATEUR DE DIMENSION
C| RAISF          |-->| RAISON FREQUENTIELLE
C| SIGMAA         |-->| VALEUR DE SIGMA JONSWAP POUR F
C| SIGMAB         |-->| VALEUR DE SIGMA JONSWAP POUR F > FP
C| SPRED1         |-->| ETALEMENT DIRECTIONNEL 1 POUR FRA
C| SPRED2         |-->| ETALEMENT DIRECTIONNEL 2 POUR FRA
C| TC1            |<--| TEMPS CORRESPONDANT AU COURANT 1
C| TC2            |<--| TEMPS CORRESPONDANT AU COURANT 2
C| TETA           |<--| DIRECTIONS DE PROPAGATION
C| TETA1          |-->| DIRECTION PRINCIPALE 1 POUR FRA
C| TETA2          |-->| DIRECTION PRINCIPALE 2 POUR FRA
C| TM1            |<--| TEMPS CORRESP. A LA HAUTEUR DE LA MAREE 1
C| TM2            |<--| TEMPS CORRESP. A LA HAUTEUR DE LA MAREE 2
C| TRA31          |<->| TABLEAUX DE TRAVAIL REELS
C| TV1            |<--| TEMPS CORRESPONDANT AU VENT 1
C| TV2            |<--| TEMPS CORRESPONDANT AU VENT 2
C| UC,VC          |<--| COMPOSANTES DU CHAMP DE COURANT
C| UC1,VC1        |<--| COMPOSANTES DU CHAMP DE COURANT INFERIEUR
C| UC2,VC2        |<--| COMPOSANTES DU CHAMP DE COURANT SUPERIEUR
C| UV,VV          |<--| COMPOSANTES DU CHAMP DE VENT INITIAL
C| UV1,VV1        |<--| COMPOSANTES DU CHAMP DE VENT INFERIEUR
C| UV2,VV2        |<--| COMPOSANTES DU CHAMP DE VENT SUPERIEUR
C| VENT           |-->| LOGIQUE INDIQUANT SI ON CONSIDERE UN VENT
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
C| XLAMDA         |-->| FACTEUR DE PONDERATION POUR LA FRA
C| XRELC,YRELC    |-->| COORDONNEES DES POINTS DES COURANTS RELEVES
C| XRELM,YRELM    |-->| COORDONNEES DES POINTS DES HAUTEURS RELEVEES
C| XRELV,YRELV    |-->| COORDONNEES DES POINTS DES VENTS RELEVES
C| ZM             |<--| HAUTEUR DE LA MAREE INITIALE
C| ZM1,ZM2        |<--| HAUTEURS DE LA MAREE INFERIEURE ET SUPERIEURE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      DOUBLE PRECISION AT , DPI, TC1, TC2, TV1, TV2, TM1, TM2
      INTEGER          NPC, NPV, NPM
      INTEGER          LT
C
C
      INTEGER          NP0, NP1, NP2, NP3, NP4, NP5
      INTEGER          IPLAN, IFREQ , NVHMA, NVCOU, IBID
C
      CHARACTER*7   CHDON
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      INTEGER          NPOIN_ALL
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
C
C***********************************************************************
C
      AT  = 0.D0
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      NPOIN_ALL = MAX(NPOIN3,NPOIN3*NCSIZE*3)
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
C      NP0 = NPOIN3+1
C      NP1 = 2*NPOIN3
C      NP2 = NP1+1
C      NP3 = 3*NPOIN3
C      NP4 = NP3+1
C      NP5 = 4*NPOIN3
      NP0 = NPOIN_ALL+1
      NP1 = 2*NPOIN_ALL
      NP2 = NP1+1
      NP3 = 3*NPOIN_ALL
      NP4 = NP3+1
      NP5 = 4*NPOIN_ALL

C
C-----------------------------------------------------------------------
C
C   INITIALISES THE TIDAL CURRENT AND WATER LEVEL
C
C
      IF (MAREE) THEN
        IF(LNG.EQ.1) THEN
          CHDON='COURANT'
        ELSE
          CHDON='CURRENT'
        ENDIF
C       READS IN THE TIDAL CURRRENT
!BD_INCKA MODIFICATION WITH CORRECT FILENAMES
C        IF((NOMCOF(1:1).EQ.' ').AND.(NOMCOB(1:1).EQ.' ')) THEN
        IF((WAC_FILES(WACCOF)%NAME(1:1).EQ.' ').AND.
     &                (WAC_FILES(WACCOB)%NAME(1:1).EQ.' ')) THEN
!BD_INCKA END OF MODIFICATION
            WRITE(*,*)'FICHIER ANALYTIQUE POUR COURANT'
          CALL ANAMAR
     &    ( SUC%R  , SVC%R   , STRA31%R, SZM1%R   ,
     &      SZM2%R , SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &      NPOIN2    , AT  , DDC  , LT         )
          WRITE(LU,*)' '
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT DE MAREE'
            WRITE(LU,*)
     &      'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
            WRITE(LU,*)
     &      '==> LE COURANT DE MAREE EST INITIALISE DANS ANAMAR'
          ELSE
            WRITE(LU,*)'USE OF TIDAL CURRENT VELOCITIES'
            WRITE(LU,*)'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL CURRENT VELOCITIES IN ANAMAR'
          ENDIF
        ELSE
!BD_INCKA MODIFICATION WITH THE CORRECT FILENAMES
C          IF (NOMCOF(1:1).NE.' ') THEN
          IF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
C           READS IN THE TIDAL CURRENTS FROM FORMATTED DATA FILE
            CALL LECDOI
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
CBD_INKCA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2, NCOF , BINCOU, NBOR , NPTFR,
     &        NPOIN2, WAC_FILES(WACCOF)%LU , BINCOU, NBOR , NPTFR,
!BD_INCKA END OF MODIFICATION
     &        AT , DDC , TC1, TC2, NPC   ,
     &        SXRELC%R, SYRELC%R,
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        TRA01(1:NPOIN_ALL) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUC1%R , SVC1%R, SUC2%R, SVC2%R,
     &        INDIC , NPOIN_ALL, CHDON, NVCOU)
          ELSE
C           READS IN THE TIDAL CURRENT FROM BINARY FILE
            CALL LECDOI
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
CBD_INKCA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2, NCOB , BINCOU, NBOR , NPTFR,
     &        NPOIN2, WAC_FILES(WACCOB)%LU , BINCOU, NBOR , NPTFR,
!BD_INCKA END OF MODIFICATION
     &        AT , DDC , TC1, TC2, NPC   ,
     &        SXRELC%R, SYRELC%R,
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        TRA01(1:NPOIN_ALL) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUC1%R , SVC1%R, SUC2%R, SVC2%R,
     &        INDIC , NPOIN_ALL, CHDON, NVCOU )
          ENDIF
        ENDIF
C
C       READS IN THE TIDAL WATER LEVEL
!BD_INCKA MODIFICATION WITH THE CORRECT FILENAMES
C        IF((NOMMAF(1:1).EQ.' ').AND.(NOMMAB(1:1).EQ.' ')) THEN
C          IF((NOMCOF(1:1).NE.' ').OR.(NOMCOB(1:1).NE.' ')) THEN
        IF((WAC_FILES(WACMAF)%NAME(1:1).EQ.' ').AND.
     &                       (WAC_FILES(WACMAB)%NAME(1:1).EQ.' ')) THEN
          IF((WAC_FILES(WACCOF)%NAME.NE.' ').OR.
     &                            (WAC_FILES(WACCOB)%NAME.NE.' ')) THEN
!BD_INCKA END OF MODIFICATION
            CALL ANAMAR
     &    ( SUC%R  , SVC%R   , STRA31%R, SZM1%R   ,
     &      SZM2%R , SDZHDT%R, MESH%X%R, MESH%Y%R ,
     &      NPOIN2,   AT  , DDC , LT    )
          ENDIF
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
     &      '==> LA HAUTEUR DE LA MAREE EST INITIALISEE DANS ANAMAR'
          ELSE
            WRITE(LU,*)
     &      '==> INITIALISATION OF TIDAL WATER LEVEL IN ANAMAR'
          ENDIF
        ELSE
!BD_INCKA MODIFICATION WITH THE CORRECT WAC_FILE ETC..
C          IF (NOMMAF(1:1).NE.' ') THEN
          IF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
            WRITE(*,*)'LECTURE COURANT DANS FICHIER FORMATE LECHAM MAF'
            CALL LECHAM
     &      ( STRA31%R, SDZHDT%R, MESH%X%R, MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT NMAF-> WAC_FILE(...)%LU
C     *        NPOIN2, NMAF , BINMAR, NBOR  ,
     &        NPOIN2, WAC_FILES(WACMAF)%LU, BINMAR, NBOR  ,
!BD_INCKA END OF MODIFICATION
     &        NPTFR, AT   , DDC , TM1  , TM2   , NPM  ,
     &        SXRELM%R , SYRELM%R ,
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        TRA01(1:NPOIN_ALL)   , SZM1%R, SZM2%R,
     &        INDIM, NPOIN_ALL, IDHMA ,
     &        NVHMA )
           ELSE
            CALL LECHAM
     &      ( STRA31%R, SDZHDT%R, MESH%X%R, MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2, NMAB , BINMAR, NBOR  ,
     &        NPOIN2, WAC_FILES(WACMAB)%LU , BINMAR, NBOR  ,
!BD_INCKA END OF MODIFICATION
     &        NPTFR, AT   , DDC , TM1  , TM2   , NPM  ,
     &        SXRELM%R , SYRELM%R ,
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        TRA01(1:NPOIN_ALL)   , SZM1%R, SZM2%R,
     &        INDIM, NPOIN_ALL, IDHMA ,
     &        NVHMA )
           ENDIF
        ENDIF
        CALL OV('X=X+Y   ', SDEPTH%R, STRA31%R, ST1%R,
     &                      0.D0, NPOIN2)
      ENDIF
C
C   INITIALISES THE CURRENT
C   AND READS IN A TELEMAC VARIABLE (OPTIONAL)
C
      IF ((COUSTA).OR.(DONTEL)) THEN
!BD_INCKA MODIFICATION WITH THE CORRECT CURRENT FILENAME
C        IF ((NOMCOF(1:1).EQ.' ').AND.(NOMCOB(1:1).EQ.' ')) THEN
        IF ((WAC_FILES(WACCOF)%NAME(1:1).EQ.' ').AND.
     &                 (WAC_FILES(WACCOB)%NAME(1:1).EQ.' ')) THEN
!BD_INCKA END OF MODIFICATION
          IF(COUSTA) THEN
             CALL ANACOS
     &      ( SUC%R, SVC%R, MESH%X%R, MESH%Y%R, NPOIN2)
             WRITE(LU,*)' '
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'PRISE EN COMPTE D''UN COURANT'
               WRITE(LU,*)
     &         'MAIS PAS DE FICHIER DES COURANTS (OU DONNEES TELEMAC)'
               WRITE(LU,*)'==> LE COURANT EST INITIALISE DANS ANACOS'
             ELSE
               WRITE(LU,*)'USE OF CURRENT VELOCITIES'
               WRITE(LU,*)
     &         'BUT NO CURRENT FILE (NEITHER TELEMAC DATA FILE)'
               WRITE(LU,*)
     &         '==> INITIALISATION OF CURRENT VELOCITIES IN ANACOS'
             ENDIF
          ELSE
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'RELECTURE D''UNE VARIABLE TELEMAC IMPOSSIBLE'
             ELSE
               WRITE(LU,*)' READING OF A TELEMAC DATA IMPOSSIBLE '
             ENDIF
             CALL PLANTE(0)
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            CHDON='COURANT'
          ELSE
            CHDON='CURRENT'
          ENDIF
!BD_INCKA MODIFICATION WITH THE CORRECT FILENAME
C          IF (NOMCOF(1:1).NE.' ') THEN
          IF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
             CALL LECDON
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2 , NCOF   , BINCOU ,
     &        NPOIN2 , WAC_FILES(WACCOF)%LU   , BINCOU ,
!BD_INCKA END OF MODIFICATION
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        NBOR , NPTFR, SXRELC%R, SYRELC%R, TRA01(1:NPOIN_ALL),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , DONTEL, COUSTA, INDIC  , NPOIN_ALL , CHDON)
          ELSE
             CALL LECDON
     &      ( SUC%R  , SVC%R   , MESH%X%R, MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2 , NCOB   , BINCOU ,
     &        NPOIN2 , WAC_FILES(WACCOB)%LU   , BINCOU ,
!BD_INCKA END OF MODIFICATION
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        NBOR , NPTFR, SXRELC%R, SYRELC%R, TRA01(1:NPOIN_ALL),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , DONTEL, COUSTA, INDIC  , NPOIN_ALL , CHDON)
          ENDIF
        ENDIF
        DZHDT = 0.D0
      ENDIF
C
C-----------------------------------------------------------------------
C
C   INITIALISES THE WIND
C
      IF (VENT) THEN
        IF(LNG.EQ.1) THEN
          CHDON='VENT   '
        ELSE
          CHDON='WIND   '
        ENDIF
C
!BD_INCKA MODIFICATION WITH THE CORRECT FILENAMES
C        IF ((NOMVEF(1:1).EQ.' ').AND.(NOMVEB(1:1).EQ.' ')) THEN
        IF ((WAC_FILES(WACVEF)%NAME(1:1).EQ.' ').AND.
     &                 (WAC_FILES(WACVEB)%NAME(1:1).EQ.' ')) THEN
!BD_INCKA END OF MODIFICATION
          CALL ANAVEN
     &   ( SUV%R, SVV%R, MESH%X%R, MESH%Y%R ,
     &     NPOIN2,AT,DDC,VX_CTE,VY_CTE)
          WRITE(LU,*)' '
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'PRISE EN COMPTE D''UN VENT'
            WRITE(LU,*)'MAIS PAS DE FICHIER DE VENT'
            WRITE(LU,*)'==> LE VENT EST INITIALISE DANS ANAVEN'
          ELSE
            WRITE(LU,*)'USE OF WIND VELOCITIES'
            WRITE(LU,*)'BUT NO WIND FILE '
            WRITE(LU,*)'==> INITIALISATION OF WIND VELOCITIES IN ANAVEN'
          ENDIF
        ELSE
CBD_INCA MODIFICATION WITH THE CORRECT FILENAMES
C          IF (NOMVEF(1:1).NE.' ') THEN
          IF (WAC_FILES(WACVEF)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
           IF (VENSTA) THEN
             CALL LECDON
     &      ( SUV%R  , SVV%R   , MESH%X%R, MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2 , NVEF   , BINVEN ,
     &        NPOIN2 , WAC_FILES(WACVEF)%LU   , BINVEN ,
CBD_INCA END OF MODIFICATION
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        NBOR , NPTFR, SXRELV%R, SYRELV%R, TRA01(1:NPOIN_ALL),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , .FALSE., VENSTA, INDIV  , NPOIN_ALL ,CHDON)
           ELSE
            CALL LECDOI
     &      ( SUV%R , SVV%R , MESH%X%R , MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2, NVEF , BINVEN, NBOR , NPTFR,
     &        NPOIN2, WAC_FILES(WACVEF)%LU , BINVEN, NBOR , NPTFR,
!BD_INCKA END OF MODIFICATION
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        AT , DDC , TV1, TV2, NPV   , SXRELV%R, SYRELV%R ,
     &        TRA01(1:NPOIN_ALL) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUV1%R , SVV1%R, SUV2%R, SVV2%R,
     &        INDIV , NPOIN_ALL, CHDON, IBID )
           ENDIF
          ELSE
           IF (VENSTA) THEN
             CALL LECDON
     &      ( SUV%R  , SVV%R   , MESH%X%R, MESH%Y%R ,
CBD_INCA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2 , NVEB   , BINVEN ,
     &        NPOIN2 , WAC_FILES(WACVEB)%LU   , BINVEN ,
!BD_INCKA END OF MODIFICATION
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        NBOR , NPTFR, SXRELV%R, SYRELV%R, TRA01(1:NPOIN_ALL),
     &        TRA01(NP0:NP1),TRA01(NP2:NP3),TRA01(NP4:NP5),STRA31%R,
     &        IDTEL, NPTT , .FALSE., VENSTA, INDIV  , NPOIN_ALL , CHDON)
           ELSE
            CALL LECDOI
     &      ( SUV%R , SVV%R , MESH%X%R , MESH%Y%R ,
!BD_INCKA MODIFICATION WITH THE CORRECT LOGICAL UNIT
C     *        NPOIN2, NVEB , BINVEN, NBOR , NPTFR,
     &        NPOIN2, WAC_FILES(WACVEB)%LU, BINVEN, NBOR , NPTFR,
!BD_INCKA END OF MODIFICATION
     &        AT , DDC , TV1, TV2, NPV   , SXRELV%R, SYRELV%R ,
!BD_INCKA MODIFICATION : NPOIN3=> TOTAL NB OF NODES IN THE DOMAIN=> NPOIN_ALL
     &        TRA01(1:NPOIN_ALL) , TRA01(NP0:NP1) , TRA01(NP2:NP3),
     &        SUV1%R , SVV1%R, SUV2%R, SVV2%R,
     &        INDIV , NPOIN_ALL, CHDON, IBID )
           ENDIF
          ENDIF
         ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C   INITIALISES TETA
C     BY DEFAULT THE DIRECTIONS OF PROPAGATION ARE EVENLY DISTRIBUTED
C
      DO IPLAN = 1,NPLAN+1
         TETA(IPLAN) = (IPLAN-1)*DPI/NPLAN
      ENDDO
C
C-----------------------------------------------------------------------
C
C
C     INITIALISES FREQ AND DFREQ, THE FREQUENCIES OF PROPAGATION
C     ARE DISTRIBUTED USING AN EXPONENTIAL LAW
C
      DO IFREQ = 1,NF
        FREQ(IFREQ) = F1*RAISF**(IFREQ-1)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     INITIALISES F
C
      CALL SPEINI
     &  ( SF%R  , TRA01(1:NF)   , TRA01(NP0:NP1),
     &    SUV%R , SVV%R      , SFR%R  , STETA%R  , GRAVIT,
     &    FREMAX   , FETCH , SIGMAA, SIGMAB , GAMMA  , FPIC  , HM0   ,
     &    ALPHIL   , TETA1 , SPRED1, TETA2  , SPRED2 , XLAMDA, NPOIN2,
     &    NPLAN    , NF    , INISPE, E2FMIN , DEPTH  , FRABI  )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C