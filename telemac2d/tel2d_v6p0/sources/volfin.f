C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INTERFACE FOR CALL TO THE RESOLU SUBROUTINE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRST, AT, CF, CFLWTD, CMI, DIFNU, DIFT, DIFVIT, DIMT, DJX, DJXT, DJY, DJYT, DLIMT, DPX, DPY, DSZ, DT, DTHAUT, DTN, DTT, DTVARI, DX, DY, FLUENT, FLUENTN, FLUHBOR, FLUHBTEMP, FLUSOR, FLUSORTN, FLUTENT, FLUTSOR, FLUXT, FLUXTEMP, G, H, HBOR, HC, HCSTOK, HN, HSTOK, HT, ISCE, ITURB, JMI, KDDL, KDIR, KFROT, KNEU, LIMPRO, LIMTRA, LISTIN, LOGFR, LT, LTT, MASKEL, MASSES, MASSOU, MAXSCE, MAXTRA, MESH, MSK, NBOR, NELEM, NIT, NPOIN, NPTFR, NREJET, NTRAC, OPTVF, PROPNU, QU, QV, S, SMH, SMTR, T, TB, TBOR, TMAX, TN, TSCE2, U, UBOR, V, VBOR, W1, YASMH, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VOLFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> RESOLU(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.8                                       </center>
!> </td><td> 05/08/2007
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
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
!>          <tr><td>FLUENT,FLUSORT
!></td><td><--</td><td>FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
!>    </td></tr>
!>          <tr><td>FLUHBTEMP
!></td><td><-></td><td>FLUX BORD POUR TRACEUR
!>    </td></tr>
!>          <tr><td>FLUSOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUSORTN,FLUENTN
!></td><td><-></td><td>FLUX MASSE ENTREE ET SORTIE DE TN+1 A TN+2
!>    </td></tr>
!>          <tr><td>FLUTENT,FLUTSOR
!></td><td><--</td><td>FLUX TRACEUR ENTREE ET SORTIE
!>    </td></tr>
!>          <tr><td>FLUXT,FLUHBOR
!></td><td><-></td><td>FLUX, FLUX BORD ACCUMULES POUR TRACEUR
!>    </td></tr>
!>          <tr><td>FLUXTEMP
!></td><td><-></td><td>FLUX POUR TRACEUR
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>GRAVITENTES DU DEBIT.
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE H
!>    </td></tr>
!>          <tr><td>HC
!></td><td><-></td><td>H RECONSTRUIT ORDRE 2   CORRIGE
!>    </td></tr>
!>          <tr><td>HN,H
!></td><td><-></td><td>HAUTEURS D'EAU AU TEMPS N ET N+1
!>    </td></tr>
!>          <tr><td>HSTOK,HCSTOK
!></td><td><-></td><td>H, H CORRIGE  A STOCKER POUR TRACEUR
!>    </td></tr>
!>          <tr><td>HT
!></td><td><-></td><td>HT AU TEMPS N
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
!></td><td>--></td><td>CONVENTION POUR LES POINTS LIBRES.
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES POINTS DIRICHLET.
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONVENTION POUR LES POINTS NEUMANN.
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES.
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
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUE DES ELEMENTS.
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
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DE TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, MASQUAGE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>--></td><td>NOMBRE TOTAL D'ITERATIONS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NREJET
!></td><td>--></td><td>NOMBRE DE SOURCES/PUITS
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTVF
!></td><td>--></td><td>OPTION SCHEMA
!>                  0:ROE, 1:CINETIQUE ORDRE 1,2:CINETIQUE ORDRE 2
!>    </td></tr>
!>          <tr><td>PROPNU
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION MOLECULAIRE
!>    </td></tr>
!>          <tr><td>QU,QV
!></td><td><-></td><td>COMPOSANTES DU DEBIT.
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>STRUCTURE VIDE POUR APPEL A VECTOR.
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
!>          <tr><td>TB
!></td><td>--></td><td>BLOC DE TABLEAUX DE TRAVAIL DIMENSION NPOIN
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR T
!>    </td></tr>
!>          <tr><td>TMAX
!></td><td>--></td><td>TEMPS DE FIN DU CALCUL
!>    </td></tr>
!>          <tr><td>TN
!></td><td>--></td><td>T  AU TEMPS N
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>TSCE
!></td><td>--></td><td>VALEURS DU TRACEUR AUX SOURCES
!>    </td></tr>
!>          <tr><td>TSCE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td><-></td><td>COMPOSANTES DE LA VITESSE.
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE U
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE V
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>                  VOIR SON DIMENSIONNEMENT DANS POINT
!>                  ICI DE TAILLE MINIMUM 9*NPOIN + 3*NPTFR
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
                        SUBROUTINE VOLFIN
     & (W1,AT,DT,LT,NIT,NELEM,NPTFR,
     &  TB,ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,
     &  S,MSK,MASKEL,MESH,LIMPRO,NBOR,KDIR,KNEU,KDDL,
     &  HBOR,UBOR,VBOR,MASSES,FLUENT,FLUSOR,CFLWTD,DTVARI,KFROT,
     &  NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,
     &  NTRAC,DIMT,T,HT,TN,DLIMT,LIMTRA,
     &  TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,CMI,JMI,
     &  DJXT,DJYT,DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &  DX,DY,OPTVF,
     &  HSTOK,HCSTOK,LOGFR,DSZ,FLUXT,FLUHBOR,DTN,FLUSORTN,FLUENTN,LTT,
     &  FLUXTEMP,FLUHBTEMP,HC,SMTR,AIRST,TMAX,DTT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
C| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
C| FLUHBTEMP      |<->| FLUX BORD POUR TRACEUR
C| FLUSOR         |---| 
C| FLUSORTN,FLUENT|<->| FLUX MASSE ENTREE ET SORTIE DE TN+1 A TN+2
C| FLUTENT,FLUTSOR|<--| FLUX TRACEUR ENTREE ET SORTIE
C| FLUXT,FLUHBOR  |<->| FLUX, FLUX BORD ACCUMULES POUR TRACEUR
C| FLUXTEMP       |<->| FLUX POUR TRACEUR
C| G             |-->| GRAVITENTES DU DEBIT.
C| HBOR           |-->| VALEURS IMPOSEES DE H
C| HC             |<->| H RECONSTRUIT ORDRE 2   CORRIGE
C| HN,H           |<->| HAUTEURS D'EAU AU TEMPS N ET N+1
C| HSTOK,HCSTOK   |<->| H, H CORRIGE  A STOCKER POUR TRACEUR
C| HT             |<->| HT AU TEMPS N
C| ISCE           |-->| POINTS SOURCES
C| ITURB          |-->| MODELE DE TURBULENCE  1 : LAMINAIRE
C| JMI            |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
C|                |   | POINT MILIEU DE L'INTERFACE
C| KDDL           |-->| CONVENTION POUR LES POINTS LIBRES.
C| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET.
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
C| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN.
C| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES.
C| LIMTRA         |-->| TYPES DE CONDITIONS AUX LIMITES SUR TRACEUR
C| LISTIN         |-->| SI OUI, MESSAGES IMPRIMES SUR LISTING.
C| LOGFR          |<->| REFERENCE DES NOEUDS FRONTIERE
C| LTT            |<->| NOMBRE DE PAS DE TEMPS TRACEUR
C| MASKEL         |-->| MASQUE DES ELEMENTS.
C| MASSES         |<--| MASSE AJOUTEE PAR TERME SOURCE
C| MASSOU         |<--| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
C| MAXSCE         |---| 
C| MAXTRA         |---| 
C| MESH           |-->| BLOC DE TABLEAUX D'ENTIERS DU MAILLAGE.
C| MSK            |-->| SI OUI, MASQUAGE D'ELEMENTS.
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD.
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NIT            |-->| NOMBRE TOTAL D'ITERATIONS.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NREJET         |-->| NOMBRE DE SOURCES/PUITS
C| NTRAC          |---| 
C| OPTVF          |-->| OPTION SCHEMA
C|                |   | 0:ROE, 1:CINETIQUE ORDRE 1,2:CINETIQUE ORDRE 2
C| PROPNU         |-->| COEFFICIENT DE DIFFUSION MOLECULAIRE
C| QU,QV          |<->| COMPOSANTES DU DEBIT.
C| S             |-->| STRUCTURE VIDE POUR APPEL A VECTOR.
C| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
C| SMTR           |---| TERMES SOURCES DU TRACEUR
C| T             |<--| TRACEUR MIS A JOUR
C| TB             |-->| BLOC DE TABLEAUX DE TRAVAIL DIMENSION NPOIN
C| TBOR           |-->| CONDITIONS AUX LIMITES SUR T
C| TMAX           |-->| TEMPS DE FIN DU CALCUL
C| TN             |-->| T  AU TEMPS N
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| TSCE           |-->| VALEURS DU TRACEUR AUX SOURCES
C| TSCE2          |---| 
C| U,V            |<->| COMPOSANTES DE LA VITESSE.
C| UBOR           |-->| VALEURS IMPOSEES DE U
C| VBOR           |-->| VALEURS IMPOSEES DE V
C| W1             |<->| TABLEAU DE TRAVAIL
C|                |   | VOIR SON DIMENSIONNEMENT DANS POINT
C|                |   | ICI DE TAILLE MINIMUM 9*NPOIN + 3*NPTFR
C| YASMH          |-->| INDIQUE SI ON PREND EN COMPTE SMH
C| ZF             |-->| COTES DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_VOLFIN => VOLFIN
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPTFR,KDIR,KNEU,KDDL,DIMT,KFROT,OPTVF
      INTEGER, INTENT(IN)    :: NELEM,NPOIN,LT,NIT,NREJET,ITURB,DLIMT
      INTEGER, INTENT(IN)    :: NTRAC,MAXSCE,MAXTRA
      INTEGER, INTENT(INOUT) :: LTT
      INTEGER, INTENT(IN)    :: LIMPRO(NPTFR,6),NBOR(NPTFR)
      INTEGER, INTENT(IN)    :: LIMTRA(DLIMT)
      INTEGER, INTENT(IN)    :: ISCE(NREJET)
      INTEGER, INTENT(INOUT) :: JMI(*),LOGFR(NPOIN)
      LOGICAL, INTENT(IN)    :: DIFVIT,DIFT,LISTIN,MSK,DTVARI,YASMH
      DOUBLE PRECISION, INTENT(IN) :: PROPNU,DIFNU
      DOUBLE PRECISION, INTENT(INOUT) :: AT,DT,MASSES,DTT
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN),QU(NPOIN),QV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*)
C                                              NSEG    NSEG
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*),HC(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),ZF(NPOIN),G
      DOUBLE PRECISION, INTENT(INOUT) :: HSTOK(DIMT),HCSTOK(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,*),DY(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: AIRST(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DPX(3,*),DPY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(2,*),DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(IN)    :: CFLWTD,DTHAUT(NPOIN),TMAX
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSOR,FLUENT,DTN,MASSOU(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSORTN,FLUENTN
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*),DJYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUTENT(*),FLUTSOR(*)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TB
      TYPE(BIEF_OBJ), INTENT(IN)      :: S,MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ) , INTENT(IN)     :: TBOR,TN
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: T,HT,SMTR,FLUHBOR,FLUHBTEMP
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLUXTEMP,FLUXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     MASS ADDED BY SOURCE TERMS (NULL FOR THE MOMENT)
C
      MASSES = 0.D0
C
C     COMPUTES THE AREA OF THE CELLS
C
      CALL VECTOR(TB%ADR(1)%P,'=','MASBAS          ',11,
     &            1.D0,S,S,S,S,S,S,MESH,MSK,MASKEL)
C
      CALL RESOLU(W1,W1(1+3*NPOIN),MESH%NUBO%I,
     &            MESH%VNOIN%R,W1(1+9*NPOIN),AT,DT,LT,NIT,
     &            NELEM,MESH%NSEG,NPTFR,W1(1+6*NPOIN),
     &            TB%ADR(1)%P%R,MESH%SURFAC%R,
     &            MESH%X%R,MESH%Y%R,MESH%IKLE%I,
     &            ZF,CF,NPOIN,HN,H,U,V,QU,QV,G,LISTIN,
     &            MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            LIMPRO,NBOR,KDIR,KNEU,KDDL,HBOR,UBOR,VBOR,
     &            FLUSOR,FLUENT,CFLWTD,DTVARI,MESH%NELMAX,KFROT,
     &            NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH,MASSES,
     &            NTRAC,DIMT,T,HT,TN,DIMT,LIMTRA,
     &            TBOR,MASSOU,FLUTENT,FLUTSOR,DTHAUT,DPX,DPY,DJX,DJY,
     &            CMI,JMI,SMTR,
     &            TB%ADR(3)%P%R,TB%ADR(4)%P%R,
     &            DJXT,DJYT,DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &            DX,DY,OPTVF,
     &            FLUSORTN,FLUENTN,DSZ,AIRST,HSTOK,HCSTOK,FLUXT,FLUHBOR,
     &            LOGFR,LTT,DTN,FLUXTEMP,FLUHBTEMP,HC,TMAX,DTT,
     &            TB%ADR(6)%P%R,TB%ADR(7)%P%R,TB%ADR(8)%P%R,
     &            TB%ADR(9)%P%R,TB%ADR(10)%P%R)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C