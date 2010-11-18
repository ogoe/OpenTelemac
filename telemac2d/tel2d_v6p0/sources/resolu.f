C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1. SOLVES THE PROBLEM BY A METHOD OF TYPE ROE FOR
!>               INTERIOR FLUXES AND OF TYPE STEGER AND WARMING FOR I/O;
!>               OR BY A KINETIC SCHEME (ORDER 1 OR 2).
!><br>            2. SOLVES IN TIME USING AN EULER TYPE SCHEME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRE, AIRS, AIRST, AT, CF, CFLWTD, CMI, DIFNU, DIFT, DIFVIT, DIMT, DJX, DJXT, DJY, DJYT, DLIMT, DPX, DPY, DSZ, DT, DTHAUT, DTN, DTT, DTVARI, DX, DXT, DY, DYT, FLUENT, FLUENTN, FLUHBOR, FLUHBTEMP, FLUSCE, FLUSORT, FLUSORTN, FLUTENT, FLUTSOR, FLUX, FLUXT, FLUXTEMP, G, H, HBOR, HC, HCSTOK, HN, HSTOK, HTN, IKLE, ISCE, ITURB, JMI, KDDL, KDIR, KFROT, KNEU, LIMPRO, LIMTRA, LISTIN, LOGFR, LT, LTT, MASSES, MASSOU, MAXSCE, MAXTRA, NBOR, NELEM, NELMAX, NIT, NPOIN, NPTFR, NREJET, NSEG, NTRAC, NUBO, OPTVF, PROPNU, QU, QV, SMH, SMTR, T, T1, T2, T3, T4, T5, TBOR, TMAX, TN, TSCE2, U, UBOR, V, VBOR, VNOIN, W, WINF, X, XNEBOR, Y, YASMH, YNEBOR, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BETA, DMIN, EPS, I, ICIN, IS, ITRAC, IVIS, K, NORDRE, TEST, W1, XNC
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_RESOLU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CALDT(), FLUHYD(), FLUROE(), FLUSEW(), FLUTRAC(), GRADZ(), INTEMP(), MAJ(), MAJTRAC(), OS(), PLANTE(), REINIT(), SMTRAC(), TESTEUR(), VFCFL()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VOLFIN()

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
!>      <td><center>                                           </center>
!> </td><td> 05/09/2007
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> MULTIPLE TRACERS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/03/1998
!> </td><td> N.GOUTAL; INRIA
!> </td><td> ROE SCHEME (NG); KINETIC SCHEMES (INRIA)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRE
!></td><td>--></td><td>AIRES DES ELEMENTS
!>    </td></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>AIRST
!></td><td>--></td><td>AIRES DES SOUS-TRIANGLES DANS CELLULES
!>    </td></tr>
!>          <tr><td>AT,DT,LT
!></td><td>--></td><td>TEMPS, PAS DE TEMPS, NUMERO DU PAS
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>CFLWTD
!></td><td>--></td><td>NOMBRE DE CFL
!>    </td></tr>
!>          <tr><td>CMI
!></td><td>--></td><td>COORDONNEES DES POINTS MILIEUX D'INTERFACE
!>    </td></tr>
!>          <tr><td>DIFNU
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>DIFT
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
!>    </td></tr>
!>          <tr><td>DIFVIT
!></td><td>--></td><td>INDIQUE S'IL FAUT FAIRE LA DIFFUSION DE U,V
!>    </td></tr>
!>          <tr><td>DIMT
!></td><td>--></td><td>DIMENSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>DJX, DJY
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>DJXT,DJYT
!></td><td>---</td><td>TABLEAUX DE TRAVAIL POUR TRACEUR
!>    </td></tr>
!>          <tr><td>DLIMT
!></td><td>--></td><td>DIMENSION DU TRACEUR AU BORD
!>    </td></tr>
!>          <tr><td>DPX, DPY
!></td><td>--></td><td>GRADIENTS DES FONCTIONS DE BASE
!>    </td></tr>
!>          <tr><td>DSZ
!></td><td><-></td><td>VARIATION DE Z POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>DTHAUT
!></td><td>--></td><td>UTILISE POUR CONDITION CFL
!>    </td></tr>
!>          <tr><td>DTN
!></td><td><-></td><td>PAS DE TEMPS   DE TN+1 A TN+2
!>    </td></tr>
!>          <tr><td>DTT
!></td><td><-></td><td>PAS DE TEMPS TRACEUR
!>    </td></tr>
!>          <tr><td>DTVARI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DX,DY
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>DXT,DYT
!></td><td>---</td><td>TABLEAUX DE TRAVAIL POUR TRACEUR
!>    </td></tr>
!>          <tr><td>FLUENT,FLUSORT
!></td><td><--</td><td>FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
!>    </td></tr>
!>          <tr><td>FLUHBTEMP
!></td><td><-></td><td>FLUX BORD POUR TRACEUR
!>    </td></tr>
!>          <tr><td>FLUSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUSORTN,FLUENTN
!></td><td><-></td><td>FLUX MASSE ENTREE ET SORTIE DE TN+1 A TN+2
!>    </td></tr>
!>          <tr><td>FLUTENT,FLUTSOR
!></td><td><--</td><td>FLUX TRACEUR ENTREE ET SORTIE
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td>---</td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>FLUXT,FLUHBOR
!></td><td><-></td><td>FLUX, FLUX BORD ACCUMULES POUR TRACEUR
!>    </td></tr>
!>          <tr><td>FLUXTEMP
!></td><td><-></td><td>FLUX POUR TRACEUR
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>CONSTANTE DE GRAVITE
!>    </td></tr>
!>          <tr><td>H
!></td><td><--</td><td>HAUTEURS D'EAU AU TEMPS N+1
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE H
!>    </td></tr>
!>          <tr><td>HC
!></td><td><-></td><td>H RECONSTRUIT ORDRE 2   CORRIGE
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEURS D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>HSTOK,HCSTOK
!></td><td><-></td><td>H, H CORRIGE  A STOCKER POUR TRACEUR
!>    </td></tr>
!>          <tr><td>HTN,TN
!></td><td>--></td><td>HT, T  AU TEMPS N
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS DES NOEUDS PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>--></td><td>POINTS SOURCES
!>    </td></tr>
!>          <tr><td>ITURB
!></td><td>--></td><td>MODELE DE TURBULENCE  1 : LAMINAIRE
!>    </td></tr>
!>          <tr><td>JMI
!></td><td>--></td><td>NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!>                  POINT MILIEU DE L'INTERFACE
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>CONVENTION POUR LES POINTS LIBRES
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES POINTS DIRICHLET
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONVENTION POUR LES POINTS NEUMANN
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES
!>    </td></tr>
!>          <tr><td>LIMTRA
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR TRACEUR
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>--></td><td>SI OUI, MESSAGES IMPRIMES SUR LISTING.
!>    </td></tr>
!>          <tr><td>LOGFR
!></td><td><-></td><td>REFERENCE DES NOEUDS FRONTIERE
!>    </td></tr>
!>          <tr><td>LTT
!></td><td><-></td><td>NOMBRE DE PAS DE TEMPS TRACEUR
!>    </td></tr>
!>          <tr><td>MASSES
!></td><td><--</td><td>MASSE AJOUTEE PAR TERME SOURCE
!>    </td></tr>
!>          <tr><td>MASSOU
!></td><td><--</td><td>MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NOMBRE TOTAL DE PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NREJET
!></td><td>--></td><td>NOMBRE DE SOURCES/PUITS
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>--></td><td>NUMEROS GLOBAUX DES EXTREMITES DES ARETES
!>    </td></tr>
!>          <tr><td>OPTVF
!></td><td>--></td><td>OPTION SCHEMA
!>                  0:ROE, 1:CINETIQUE ORDRE 1,2:CINETIQUE ORDRE 2
!>    </td></tr>
!>          <tr><td>PROPNU
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION MOLECULAIRE
!>    </td></tr>
!>          <tr><td>QU,QV
!></td><td><-></td><td>COMPOSANTES DU DEBIT AU TEMPS N PUIS  N+1
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERMES SOURCES DE L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>SMTR
!></td><td>---</td><td>TERMES SOURCES DU TRACEUR
!>    </td></tr>
!>          <tr><td>T
!></td><td><--</td><td>TRACEUR MIS A JOUR
!>    </td></tr>
!>          <tr><td>T1,T2,T3,T4,T5
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR T
!>    </td></tr>
!>          <tr><td>TMAX
!></td><td>--></td><td>TEMPS DE FIN DU CALCUL
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>TSCE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td><--</td><td>COMPOSANTES DE LA VITESSE AU TEMPS N+1
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE U
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE V
!>    </td></tr>
!>          <tr><td>VNOIN
!></td><td>--></td><td>NORMALE A L'INTERFACE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>W
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>WINF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>NORMALE AUX POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>INDIQUE SI ON PREND EN COMPTE SMH
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTES DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE RESOLU
     & (W,FLUSCE,NUBO,VNOIN,WINF,AT,DT,LT,NIT,
     &  NELEM,NSEG,NPTFR,FLUX,AIRS,AIRE,
     &  X,Y,IKLE,ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,XNEBOR,
     &  YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL,
     &  HBOR,UBOR,VBOR,FLUSORT,FLUENT,CFLWTD,DTVARI,NELMAX,KFROT,
     &  NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,MASSES,
     &  NTRAC,DIMT,T,HTN,TN,DLIMT,LIMTRA,
     &  TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,CMI,JMI,
     &  SMTR,DXT,DYT,DJXT,DJYT,
     &  DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &  DX,DY,OPTVF,FLUSORTN,FLUENTN,
     &  DSZ,AIRST,HSTOK,HCSTOK,FLUXT,FLUHBOR,
     &  LOGFR,LTT,DTN,FLUXTEMP,FLUHBTEMP,
     &  HC,TMAX,DTT,T1,T2,T3,T4,T5)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRE           |-->| AIRES DES ELEMENTS
C| AIRS           |-->| AIRES DES CELLULES DU MAILLAGE
C| AIRST          |-->| AIRES DES SOUS-TRIANGLES DANS CELLULES
C| AT,DT,LT       |-->| TEMPS, PAS DE TEMPS, NUMERO DU PAS
C| CF             |-->| COEFFICIENT DE FROTTEMENT
C| CFLWTD         |-->| NOMBRE DE CFL
C| CMI            |-->| COORDONNEES DES POINTS MILIEUX D'INTERFACE
C| DIFNU          |-->| COEFFICIENT DE DIFFUSION DU TRACEUR
C| DIFT           |-->| LOGIQUE INDIQUANT S'IL Y A DIFFUSION TRACEUR
C| DIFVIT         |-->| INDIQUE S'IL FAUT FAIRE LA DIFFUSION DE U,V
C| DIMT           |-->| DIMENSION DU TRACEUR
C| DJX, DJY       |---| TABLEAUX DE TRAVAIL
C| DJXT,DJYT      |---| TABLEAUX DE TRAVAIL POUR TRACEUR
C| DLIMT          |-->| DIMENSION DU TRACEUR AU BORD
C| DPX, DPY       |-->| GRADIENTS DES FONCTIONS DE BASE
C| DSZ            |<->| VARIATION DE Z POUR ORDRE 2
C| DTHAUT         |-->| UTILISE POUR CONDITION CFL
C| DTN            |<->| PAS DE TEMPS   DE TN+1 A TN+2
C| DTT            |<->| PAS DE TEMPS TRACEUR
C| DTVARI         |---| 
C| DX,DY          |---| TABLEAUX DE TRAVAIL
C| DXT,DYT        |---| TABLEAUX DE TRAVAIL POUR TRACEUR
C| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
C| FLUHBTEMP      |<->| FLUX BORD POUR TRACEUR
C| FLUSCE         |---| 
C| FLUSORTN,FLUENT|<->| FLUX MASSE ENTREE ET SORTIE DE TN+1 A TN+2
C| FLUTENT,FLUTSOR|<--| FLUX TRACEUR ENTREE ET SORTIE
C| FLUX           |---| TABLEAU DE TRAVAIL
C| FLUXT,FLUHBOR  |<->| FLUX, FLUX BORD ACCUMULES POUR TRACEUR
C| FLUXTEMP       |<->| FLUX POUR TRACEUR
C| G             |-->| CONSTANTE DE GRAVITE
C| H             |<--| HAUTEURS D'EAU AU TEMPS N+1
C| HBOR           |-->| VALEURS IMPOSEES DE H
C| HC             |<->| H RECONSTRUIT ORDRE 2   CORRIGE
C| HN             |-->| HAUTEURS D'EAU AU TEMPS N
C| HSTOK,HCSTOK   |<->| H, H CORRIGE  A STOCKER POUR TRACEUR
C| HTN,TN         |-->| HT, T  AU TEMPS N
C| IKLE           |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| ISCE           |-->| POINTS SOURCES
C| ITURB          |-->| MODELE DE TURBULENCE  1 : LAMINAIRE
C| JMI            |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
C|                |   | POINT MILIEU DE L'INTERFACE
C| KDDL           |-->| CONVENTION POUR LES POINTS LIBRES
C| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
C| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
C| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
C| LIMTRA         |-->| TYPES DE CONDITIONS AUX LIMITES SUR TRACEUR
C| LISTIN         |-->| SI OUI, MESSAGES IMPRIMES SUR LISTING.
C| LOGFR          |<->| REFERENCE DES NOEUDS FRONTIERE
C| LTT            |<->| NOMBRE DE PAS DE TEMPS TRACEUR
C| MASSES         |<--| MASSE AJOUTEE PAR TERME SOURCE
C| MASSOU         |<--| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
C| MAXSCE         |---| 
C| MAXTRA         |---| 
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NIT            |-->| NOMBRE TOTAL DE PAS DE TEMPS
C| NPOIN          |-->| NOMBRE DE NOEUDS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NREJET         |-->| NOMBRE DE SOURCES/PUITS
C| NSEG           |-->| NOMBRE D'ARETES
C| NTRAC          |---| 
C| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES DES ARETES
C| OPTVF          |-->| OPTION SCHEMA
C|                |   | 0:ROE, 1:CINETIQUE ORDRE 1,2:CINETIQUE ORDRE 2
C| PROPNU         |-->| COEFFICIENT DE DIFFUSION MOLECULAIRE
C| QU,QV          |<->| COMPOSANTES DU DEBIT AU TEMPS N PUIS  N+1
C| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
C| SMTR           |---| TERMES SOURCES DU TRACEUR
C| T             |<--| TRACEUR MIS A JOUR
C| T1,T2,T3,T4,T5 |---| TABLEAUX DE TRAVAIL
C| TBOR           |-->| CONDITIONS AUX LIMITES SUR T
C| TMAX           |-->| TEMPS DE FIN DU CALCUL
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| TSCE2          |---| 
C| U,V            |<--| COMPOSANTES DE LA VITESSE AU TEMPS N+1
C| UBOR           |-->| VALEURS IMPOSEES DE U
C| VBOR           |-->| VALEURS IMPOSEES DE V
C| VNOIN          |-->| NORMALE A L'INTERFACE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| W             |<->| TABLEAU DE TRAVAIL
C| WINF           |---| 
C| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
C| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
C| YASMH          |-->| INDIQUE SI ON PREND EN COMPTE SMH
C| ZF             |-->| COTES DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_RESOLU => RESOLU
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NSEG,NPTFR,LT,NIT,NREJET,DIMT
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA
      INTEGER, INTENT(IN) :: DLIMT,OPTVF,JMI(*)
      INTEGER, INTENT(IN) :: KDIR,KNEU,KDDL,ITURB,NELMAX,KFROT,NTRAC
      INTEGER, INTENT(IN) :: NUBO(2,*),LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3),ISCE(NREJET),LIMTRA(DLIMT)
      INTEGER, INTENT(INOUT) :: LTT,LOGFR(*)
C
      LOGICAL, INTENT(IN) :: LISTIN,DTVARI,YASMH,DIFVIT,DIFT
      DOUBLE PRECISION, INTENT(INOUT) :: T1(*),T2(*),T3(*),T4(*),T5(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN)    :: AT,VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN),FLUSORTN,FLUENTN
      DOUBLE PRECISION, INTENT(IN)    :: AIRE(NPOIN),DTHAUT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: VBOR(NPTFR),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),ZF(NPOIN),CF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NELMAX),DPY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: WINF(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN),AIRS(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN),FLUX(NPOIN,3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORT,FLUENT,MASSES
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFLWTD,AIRST(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(*),HCSTOK(2,*),DTT
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: PROPNU,DIFNU,TMAX
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NELMAX),DJY(3,NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NPOIN),DY(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(NELMAX),DJYT(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: DXT(NPOIN),DYT(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: HC(2,NSEG),DTN
C
      TYPE(BIEF_OBJ) , INTENT(IN)     :: TBOR,TN
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HTN,SMTR,FLUHBOR,FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXTEMP,FLUXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IS,K,ICIN,IVIS,NORDRE,ITRAC
C
      DOUBLE PRECISION XNC,W1,EPS,DMIN,BETA,TEST
C
C-----------------------------------------------------------------------
C
      IF(OPTVF.EQ.0) THEN
        ICIN = 0
        NORDRE = 1
      ELSEIF(OPTVF.EQ.1) THEN
        ICIN = 1
        NORDRE = 1
      ELSEIF(OPTVF.EQ.2) THEN
        ICIN = 1
        NORDRE = 2
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'SCHEMA INCONNU : ',OPTVF
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN SCHEME: ',OPTVF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     MINIMUM VALUE FOR CLIPPING
C
      EPS =  1.D-6
C
C     COPIES BOUNDARY CONDITIONS
C
C------
C 1. COMPUTES THE STEADY STATE
C------
C
C  * WINF CONTAINS INITIAL WINF COMPUTED IN BORD
C
      DO 110 K=1,NPTFR
        IF(LIMPRO(K,1).EQ.KDIR) THEN
          WINF(1,K) =  HBOR(K)
          WINF(2,K) =  HBOR(K)*UBOR(K)
          WINF(3,K) =  HBOR(K)*VBOR(K)
        ELSE
          WINF(1,K) =  H(NBOR(K))
          WINF(2,K) =  H(NBOR(K))*UBOR(K)
          WINF(3,K) =  H(NBOR(K))*VBOR(K)
        ENDIF
110   CONTINUE
C
       IF(ICIN .EQ.0) THEN
C-----------------------------------------------------------------------
C        ROE SCHEME
C-----------------------------------------------------------------------
C
      IF(LT.EQ.1) THEN
       WRITE(LU,*) ' '
       WRITE(LU,*) '          ***************** '
       WRITE(LU,*) '          * SCHEMA DE ROE * '
       WRITE(LU,*) '          ***************** '
       WRITE(LU,*) ' '
           ENDIF
C
C     COPIES THE VARIABLES IN W
C
      DO 111 I=1,NPOIN
        W(1,I)= HN(I)
        W(2,I)= QU(I)
        W(3,I)= QV(I)
111   CONTINUE
C
      CALL VFCFL(NUBO,VNOIN,NSEG,NPOIN,X,Y,G,H,EPS,QU,QV,DT,CFLWTD,
     &           DTVARI,LISTIN)
C
C  * WINF CONTAINS INITIAL WINF COMPUTED IN BORD
C     WINF CONTAINS THE BOUNDARY VALUES AFTER USE
C     OF THE INVARIANTS OF RIEMAMM

      CALL FLUSEW(WINF,UBOR,VBOR,NPOIN,EPS,G,W,
     &            XNEBOR,YNEBOR,NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
C
C
      CALL FLUROE(W,FLUSCE,NUBO,VNOIN,
     &            WINF,FLUX,FLUSORT,FLUENT,NELEM,NSEG,NPTFR,
     &            NPOIN,X,Y,AIRS,ZF,EPS,DMIN,G,
     &            XNEBOR,YNEBOR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
C
C ---> INTEGRATION IN TIME
C      --------------------
C
      CALL INTEMP(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DMIN,KFROT,SMH)
C
C  COMPUTES THE ADDITIONAL VOLUME DUE TO THE SOURCES
C
      IF(YASMH) THEN
        MASSES=0.D0
        DO I=1,NPOIN
          MASSES = MASSES + SMH(I)
        ENDDO
        MASSES = DT * MASSES
      ENDIF
C
      DO  I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
C
C       COMPUTES U,V (JMH ADDITION)
C
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
      ENDDO
C
      XNC = 0.D0
      DO I=1,NPOIN
         IF(H(I).GT.EPS) THEN
          W1=SQRT((QU(I)/H(I))**2+(QV(I)/H(I))**2)+SQRT(G*H(I))
          IF(W1.GE.XNC) XNC = W1
          IF(W1.GE.50.D0) THEN
            QU(I) = 0.D0
            QV(I) = 0.D0
          ENDIF
         ENDIF
      ENDDO
C
      ELSE IF(ICIN.EQ.1) THEN
C    *****************************
C
C-----------------------------------------------------------------------
C             KINETIC SCHEME
C-----------------------------------------------------------------------
C
      IVIS=0
      IF(DIFVIT.AND.ITURB.EQ.1) IVIS=1
C
      IF(LT.EQ.1) THEN
C
C             INITIALISES AT THE 1ST TIME STEP
C             ***********************************
C
       WRITE(LU,*) ' '
       WRITE(LU,*) '          ******************** '
       WRITE(LU,*) '          * SCHEMA CINETIQUE * '
       WRITE(LU,*) '          ******************** '
       WRITE(LU,*) ' '
C
C    COMPUTES THE GRADIENT OF ZF
C
       IF(NORDRE.EQ.2) THEN
      CALL GRADZ(NPOIN,NELMAX,NSEG,IKLE,NUBO,X,Y,AIRE,AIRS,CMI,JMI,
     &           ZF,DPX,DPY,DSZ,BETA,AIRST,T1,T2,T3,T4,T5)
       ENDIF
C
C    INITIALISES THE TRACER
C
       IF(NTRAC.GT.0) THEN
         DO ITRAC=1,NTRAC
           MASSOU(ITRAC) = 0.D0
           FLUTENT(ITRAC)= 0.D0
           FLUTSOR(ITRAC)= 0.D0
           DO IS=1,NPOIN
             HTN%ADR(ITRAC)%P%R(IS) = HN(IS) * TN%ADR(ITRAC)%P%R(IS)
           ENDDO
           CALL OS('X=Y     ',X=T%ADR(ITRAC)%P,Y=TN%ADR(ITRAC)%P)
         ENDDO
       ENDIF
C
C   DEFINES A STANDARD TO TELL INTERIOR NODES FROM BOUNDARY NODES
C      (FOR TRACER ORDER 2)
C
       DO IS=1,NPOIN
         LOGFR(IS)=0
       ENDDO
C
      DO K=1,NPTFR
C
        IS=NBOR(K)
        IF(LIMPRO(K,2).EQ.KDIR) LOGFR(IS)=1
        IF(LIMPRO(K,1).EQ.KDIR) LOGFR(IS)=3
        IF(LIMPRO(K,1).EQ.KNEU) LOGFR(IS)=2
C
       ENDDO
C
       ENDIF
C-----------------------------------------------------------------------
C
      IF(LT.EQ.1.OR.NTRAC.EQ.0) THEN
C
C     COPIES VARIABLES IN W
C
      DO I=1,NPOIN
          W(1,I)= HN(I)
        IF (HN(I).GT.EPS) THEN
          W(2,I) = QU(I) / HN(I)
          W(3,I) = QV(I) / HN(I)
        ELSE
          W(2,I) = 0.D0
          W(3,I) = 0.D0
        ENDIF
      ENDDO
C
C  COMPUTES THE TIME STEP SATISFYING THE CFL CONDITION (1ST ORDER)
C
      CALL CALDT(NPOIN,G,HN,U,V,DTHAUT,DTN,CFLWTD)
C
C  COMPUTES THE HYDRODYNAMIC FLUXES
C
      CALL FLUHYD(NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &            W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &            KDDL, HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &            DJX,DJY,DX,DY,DTHAUT,CFLWTD,
     &            DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,DSZ,AIRST,HC,
     &            FLUXTEMP,NTRAC)
C
      IF(NTRAC.GT.0) THEN
C       INITIALISES THE TRACER(S)
        CALL REINIT(NPOIN,NSEG,NPTFR,HN,
     &              SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
      ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C           IF TRACER, END OF INITIALISATION
C
C-----------------------------------------------------------------------
C
C                     UPDATE HYDRODYNAMICS
C-----------------------------------------------------------------------
C
      DT = MIN(DTN,TMAX-AT)
C
      FLUENT =FLUENTN
      FLUSORT =FLUSORTN
      DO ITRAC=1,NTRAC
        FLUTENT(ITRAC)=0.D0
        FLUTSOR(ITRAC)=0.D0
        MASSOU(ITRAC) =0.D0
      ENDDO
C
      CALL MAJ(NPOIN,NSEG,NPTFR,G,DT,AIRS,
     &         HN,QU,QV,W,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KNEU,
     &         SMH,KFROT,CF )
C
C-----------------------------------------------------------------------
C
      IF(NTRAC.GT.0) THEN
C
        DO ITRAC=1,NTRAC
C
C  INCREMENTS MASS FLUXES AND SOURCES FOR TRACER
          CALL FLUTRAC(NSEG,NPTFR,DT,FLUXT%ADR(ITRAC)%P%R,
     &                               FLUHBOR%ADR(ITRAC)%P%R,
     &                               FLUXTEMP%ADR(ITRAC)%P%R,
     &                               FLUHBTEMP%ADR(ITRAC)%P%R,DTT)
C
C         COMPUTES THE SECOND MEMBER FOR TRACER
          CALL SMTRAC(NPOIN,DIMT,AT,DT,SMTR%ADR(ITRAC)%P%R,
     &                SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
C
        ENDDO

C
      ENDIF
C
C  COMPUTES THE ADDITIONAL VOLUME DUE TO THE SOURCES
C
      IF(YASMH) THEN
      MASSES=0.D0
      DO  I=1,NPOIN
      MASSES = MASSES + SMH(I)
      ENDDO
       MASSES = DT * MASSES
      ENDIF
C
      DO 115 I=1,NPOIN
        H(I)  = W(1,I)
        QU(I) = W(2,I)
        QV(I) = W(3,I)
C
C       COMPUTES U,V (JMH ADDITION)
C
        IF (H(I).GT.EPS) THEN
          U(I)  = W(2,I) / H(I)
          V(I)  = W(3,I) / H(I)
        ELSE
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
115   CONTINUE
C
      IF(NTRAC.EQ.0)  RETURN
C
C-----------------------------------------------------------------------
C
C  IF END OF COMPUTATION, UPDATES THE TRACER
C
      IF(AT+DT.GE.TMAX) GOTO 200
C
C-----------------------------------------------------------------------
C     IF TRACER, COMPUTES BY ANTICIPATION THE FLUXES
C     FOR THE NEXT HYDRODYNAMIC TIME STEP
C-----------------------------------------------------------------------
C
C  COPIES THE PRIMITIVE VARIABLES IN W
C
      DO I=1,NPOIN
        W(1,I) = H(I)
        W(2,I) = U(I)
        W(3,I) = V(I)
      ENDDO
C
C  COMPUTES THE TIME STEP SATISFYING THE CFL CONDITION (1ST ORDER)
C
      CALL CALDT(NPOIN,G,H,U,V,DTHAUT,DTN,CFLWTD)
C
C  COMPUTES THE HYDRO FLUXES FOR THE NEXT TIME STEP
C
      CALL FLUHYD(NPOIN,NELMAX,NSEG,NPTFR,NUBO,G,DTN,X,Y,AIRS,IKLE,AIRE,
     &            W,ZF,VNOIN,FLUX,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &            KDDL,HBOR,UBOR,VBOR,FLUENTN,FLUSORTN,NORDRE,CMI,JMI,
     &            DJX,DJY,DX,DY,DTHAUT,CFLWTD,
     &            DPX,DPY,IVIS,PROPNU,FLUHBTEMP,BETA,DSZ,AIRST,HC,
     &            FLUXTEMP,NTRAC)
C
C  CHECKS TRACER FLUXES FOR POSITIVITY
C
C     NO OTHER USE THAN AVOIDING COMPILER WARNING
      TEST=-1.D0
C
      CALL TESTEUR(NPOIN,NSEG,NPTFR,NUBO,DTN,NBOR,
     &             NORDRE,AIRS,AIRST,HSTOK,HCSTOK,
     &             FLUXT,FLUXTEMP,FLUHBOR,FLUHBTEMP,LOGFR,TEST,NTRAC)
C
      IF(TEST.GE.0.D0) RETURN
 200  CONTINUE
C
C  UPDATES THE TRACER (TO BE LESS DIFFUSIVE, TREATS THE TRACER
C                          ONLY WHEN MONOTONICITY IS THREATENED)
C
      LTT=LTT+1
C
      DO ITRAC=1,NTRAC
C
      CALL MAJTRAC(NPOIN,NELMAX,DIMT,DLIMT,NSEG,NPTFR,NUBO,
     &             X,Y,AIRS,IKLE,AIRE,T%ADR(ITRAC)%P%R,
     &             HTN%ADR(ITRAC)%P%R,TN%ADR(ITRAC)%P%R,ZF,NBOR,
     &             TBOR%ADR(ITRAC)%P%R,FLUTENT(ITRAC),FLUTSOR(ITRAC),
     &             SMTR%ADR(ITRAC)%P%R,NORDRE,CMI,JMI,
     &             DJXT,DJYT,DXT,DYT,
     &             DPX,DPY,DIFT,DIFNU,BETA,DSZ,AIRST,HSTOK,
     &             HCSTOK,FLUXT%ADR(ITRAC)%P%R,FLUHBOR%ADR(ITRAC)%P%R,
     &             MASSOU(ITRAC),DTT)
C
C   HT IS IN T AFTER CALL TO MAJTRAC
C
      DO I=1,NPOIN
        HTN%ADR(ITRAC)%P%R(I) = T%ADR(ITRAC)%P%R(I)
        IF(H(I).GT.EPS) THEN
          T%ADR(ITRAC)%P%R(I) = T%ADR(ITRAC)%P%R(I) / H(I)
        ELSE
          T%ADR(ITRAC)%P%R(I) = 0.D0
        ENDIF
      ENDDO
C
      ENDDO
C
C INITIALISES THE TRACER
C
      CALL REINIT(NPOIN,NSEG,NPTFR,H,
     &            SMTR,HSTOK,HC,HCSTOK,FLUXT,FLUHBOR,DTT,NTRAC)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C