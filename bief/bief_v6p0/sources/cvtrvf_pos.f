C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FINITE VOLUMES, UPWIND, EXPLICIT AND MONOTONIC
!>                ADVECTOR EVEN WITH TIDAL FLATS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!>            WITH NO FRICTION

!>  @warning  DISCRETISATION OF VISC

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AGGLOH, BILAN, CONV, DIFT, DM1, DT, ENTET, F, FBOR, FLBOR, FLBORTRA, FN, FSCEXP, GLOSEG1, GLOSEG2, H, HN, HNT, HPROP, HT, IOPT, KDDL, KDIR, LIMTRA, MASKEL, MASKPT, MASKTR, MASSOU, MESH, MSK, NBOR, NPTFR, OPDTRA, OPTSOU, S, SM, SMH, SMI, SOLSYS, T1, T2, T3, T4, T5, T6, T7, T8, TE1, UDEL, UNSV2D, V2DPAR, VDEL, VISC, VISC_S, YAFLBOR, YASMH, YASMI, ZCONV
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NBMAXNSHARE NBMAXNSHARE@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, CINIT, CPREV, DEJA, FLBNEG, FORMUL, FXMAT, H1N, H2N, HFL1, HFL2, I, I1, I2, INDIC, IOPT1, IOPT2, IPTFR, IR, MASSET, MASSETN, NEWREMAIN, NITER, NPOIN, REMAIN_SEG, TESTING, TET, TETA, YACSTE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> FXMAT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), FLUX_EF_VF(), MULT_INTERFACE_SEG(), OS(), OSDB(), PARCOM(), PARCOM2_SEG(), P_DMAX(), P_DMIN(), P_DSUM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVDFTR(), PROPAG()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 31/08/2009
!> </td><td> J-M HERVOUET   (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AGGLOH
!></td><td>--></td><td>MASS-LUMPING UTILISE DANS L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>BILAN
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON DOIT FAIRE UN BILAN
!>                  DE MASSE. DANS CE CAS IL FAUT RETOURNER LA
!>                  VALEUR DE L'APPORT DES TERMES SOURCES.
!>    </td></tr>
!>          <tr><td>CONV
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A CONVECTION DE F
!>    </td></tr>
!>          <tr><td>DIFT
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>DM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
!>                  SUR LE BILAN DE MASSE DE TRACEUR
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>VALEURS A L' ETAPE N+1.
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td>--></td><td>CONDITIONS DE DIRICHLET SUR F.
!>    </td></tr>
!>          <tr><td>FLBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLBORTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>VALEURS A L' ETAPE N.
!>    </td></tr>
!>          <tr><td>FSCEXP
!></td><td>--></td><td>PARTIE EXPLICITE DU TERME SOURCE
!>                  EGALE A ZERO PARTOUT SAUF POUR LES POINTS
!>                  SOURCES OU IL Y A FSCE - (1-TETAT) FN
!>                  VOIR DIFSOU
!>    </td></tr>
!>          <tr><td>GLOSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H , HN
!></td><td>--></td><td>VALEURS DE LA HAUTEUR A L' ETAPE N+1 ET N
!>    </td></tr>
!>          <tr><td>HNT,HT
!></td><td><--</td><td>TABLEAUX DE TRAVAIL (HAUTEURS MODIFIEES POUR
!>                  TENIR COMPTE DU MASS-LUMPING)
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>--></td><td>HAUTEUR DE PROPAGATION (FAITE DANS CVDFTR).
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>---</td><td>OPTIONS DE CALCUL
!>                  CHIFFRE DES DIZAINES (IOPT2):
!>                  0 : UCONV RESPECTE L'EQUATION DE CONTINUITE
!>                  1 : UCONV NE RESPECTE PAS LA CONTINUITE
!>                  CHIFFRE DES UNITES (IOPT1):
!>                  0 : CONSTANTE PAR ELEMENT NULLE
!>                  1 : CONSTANTE DE CHI-TUAN PHAM
!>                  2 : CONSTANTE DE LEO POSTMA
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>CONVENTION POUR LES DEGRES DE LIBERTE
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONVENTION POUR LES CONDITIONS DE NEUMANN
!>    </td></tr>
!>          <tr><td>LIMTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKTR(1,1)
!></td><td>--></td><td>MASQUE VALANT 1. POUR LES SEGMENTS DIRICHLET
!>    </td></tr>
!>          <tr><td>MASKTR(1,2)
!></td><td>--></td><td>MASQUE VALANT 1. POUR LES SEGMENTS DDL
!>    </td></tr>
!>          <tr><td>MASKTR(1,3)
!></td><td>--></td><td>MASQUE VALANT 1. POUR LES SEGMENTS NEUMANN
!>                  (ET ZERO SINON)
!>    </td></tr>
!>          <tr><td>MASSOU
!></td><td>--></td><td>MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
!>                  VOIR DIFSOU
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPDTRA
!></td><td>--></td><td>MOT-CLE : OPTION POUR LA DIFFUSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>--></td><td>OPTION DE TRAITEMENT DES TERMES SOURCES.
!>                  1 : NORMAL
!>                  2 : DIRAC
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>STRUCTURE BIDON
!>    </td></tr>
!>          <tr><td>SM
!></td><td>--></td><td>TERMES SOURCES .
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERME SOURCE DE L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>SMI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SOLSYS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1,T2,T3,T4,
!></td><td><-></td><td>TABLEAUX DE TRAVAIL (T1 PAS UTILISE)
!>    </td></tr>
!>          <tr><td>T5,T6,T7
!></td><td><-></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE1
!></td><td><-></td><td>TABLEAU DE TRAVAIL SUR LES ELEMENTS
!>    </td></tr>
!>          <tr><td>TETAU
!></td><td>--></td><td>IMPLICITATION SUR U
!>    </td></tr>
!>          <tr><td>U,V,UN,VN
!></td><td>--></td><td>VITESSES A T(N+1) ET T(N)
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>TABLEAUX DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>UDEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VDEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>--></td><td>COEFFICIENTS DE VISCOSITE SUIVANT X,Y ET Z .
!>                  SI P0 : VISCOSITE DONNEE PAR ELEMENT
!>                  SINON : VISCOSITE DONNEE PAR POINT
!>    </td></tr>
!>          <tr><td>VISC_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YAFLBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>LOGIQUE INDIQUANT DE PRENDRE EN COMPTE SMH
!>    </td></tr>
!>          <tr><td>YASMI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZCONV
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CVTRVF_POS
     &(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UDEL,VDEL,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & T1,T2,T3,T4,T5,T6,T7,T8,HNT,HT,AGGLOH,TE1,DT,ENTET,BILAN,
     & OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,LIMTRA,KDIR,KDDL,NPTFR,FLBOR,
     & YAFLBOR,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,GLOSEG1,GLOSEG2,NBOR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AGGLOH         |-->| MASS-LUMPING UTILISE DANS L'EQUATION DE CONTINUITE
C| BILAN          |-->| LOGIQUE INDIQUANT SI ON DOIT FAIRE UN BILAN
C|                |   | DE MASSE. DANS CE CAS IL FAUT RETOURNER LA
C|                |   | VALEUR DE L'APPORT DES TERMES SOURCES.
C| CONV           |-->| LOGIQUE INDIQUANT S'IL Y A CONVECTION DE F
C| DIFT           |-->| LOGIQUE INDIQUANT S'IL Y A DIFFUSION DE F
C| DM1            |---| 
C| DT             |-->| PAS DE TEMPS
C| ENTET          |-->| LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
C|                |   | SUR LE BILAN DE MASSE DE TRACEUR
C| F             |<--| VALEURS A L' ETAPE N+1.
C| FBOR           |-->| CONDITIONS DE DIRICHLET SUR F.
C| FLBOR          |---| 
C| FLBORTRA       |---| 
C| FN             |-->| VALEURS A L' ETAPE N.
C| FSCEXP         |-->| PARTIE EXPLICITE DU TERME SOURCE
C|                |   | EGALE A ZERO PARTOUT SAUF POUR LES POINTS
C|                |   | SOURCES OU IL Y A FSCE - (1-TETAT) FN
C|                |   | VOIR DIFSOU
C| GLOSEG1        |---| 
C| GLOSEG2        |---| 
C| H , HN         |-->| VALEURS DE LA HAUTEUR A L' ETAPE N+1 ET N
C| HNT,HT         |<--| TABLEAUX DE TRAVAIL (HAUTEURS MODIFIEES POUR
C|                |   | TENIR COMPTE DU MASS-LUMPING)
C| HPROP          |-->| HAUTEUR DE PROPAGATION (FAITE DANS CVDFTR).
C| IOPT           |---| OPTIONS DE CALCUL
C|                |   | CHIFFRE DES DIZAINES (IOPT2):
C|                |   | 0 : UCONV RESPECTE L'EQUATION DE CONTINUITE
C|                |   | 1 : UCONV NE RESPECTE PAS LA CONTINUITE
C|                |   | CHIFFRE DES UNITES (IOPT1):
C|                |   | 0 : CONSTANTE PAR ELEMENT NULLE
C|                |   | 1 : CONSTANTE DE CHI-TUAN PHAM
C|                |   | 2 : CONSTANTE DE LEO POSTMA
C| KDDL           |-->| CONVENTION POUR LES DEGRES DE LIBERTE
C| KDIR           |---| 
C| KNEU           |-->| CONVENTION POUR LES CONDITIONS DE NEUMANN
C| LIMTRA         |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKPT         |---| 
C| MASKTR(1,1)    |-->| MASQUE VALANT 1. POUR LES SEGMENTS DIRICHLET
C| MASKTR(1,2)    |-->| MASQUE VALANT 1. POUR LES SEGMENTS DDL
C| MASKTR(1,3)    |-->| MASQUE VALANT 1. POUR LES SEGMENTS NEUMANN
C|                |   | (ET ZERO SINON)
C| MASSOU         |-->| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
C|                |   | VOIR DIFSOU
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |---| 
C| NPTFR          |---| 
C| OPDTRA         |-->| MOT-CLE : OPTION POUR LA DIFFUSION DU TRACEUR
C| OPTSOU         |-->| OPTION DE TRAITEMENT DES TERMES SOURCES.
C|                |   | 1 : NORMAL
C|                |   | 2 : DIRAC
C| S             |-->| STRUCTURE BIDON
C| SM             |-->| TERMES SOURCES .
C| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE
C| SMI            |---| 
C| SOLSYS         |---| 
C| T1,T2,T3,T4,   |<->| TABLEAUX DE TRAVAIL (T1 PAS UTILISE)
C| T5,T6,T7       |<->| TABLEAUX DE TRAVAIL
C| T8             |---| 
C| TE1            |<->| TABLEAU DE TRAVAIL SUR LES ELEMENTS
C| TETAU          |-->| IMPLICITATION SUR U
C| U,V,UN,VN      |-->| VITESSES A T(N+1) ET T(N)
C| UCONV,VCONV    |-->| TABLEAUX DE TRAVAIL.
C| UDEL           |---| 
C| UNSV2D         |---| 
C| V2DPAR         |---| 
C| VDEL           |---| 
C| VISC           |-->| COEFFICIENTS DE VISCOSITE SUIVANT X,Y ET Z .
C|                |   | SI P0 : VISCOSITE DONNEE PAR ELEMENT
C|                |   | SINON : VISCOSITE DONNEE PAR POINT
C| VISC_S         |---| 
C| YAFLBOR        |---| 
C| YASMH          |-->| LOGIQUE INDIQUANT DE PRENDRE EN COMPTE SMH
C| YASMI          |---| 
C| ZCONV          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: OPDTRA,OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: KDDL,IOPT
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: LIMTRA(NPTFR),NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: BILAN,CONV,YASMH,YAFLBOR
      LOGICAL, INTENT(IN)             :: DIFT,MSK,ENTET,YASMI
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN)      :: V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,SM,HNT,HT
      TYPE(BIEF_OBJ), INTENT(IN)      :: FBOR,UDEL,VDEL,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,S,MASKTR,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISC_S,VISC
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DSUM,P_DMIN,P_DMAX
      EXTERNAL         P_DSUM,P_DMIN,P_DMAX
C
      INTEGER I,IOPT1,IOPT2,NPOIN,IPTFR,I1,I2,NITER,REMAIN_SEG,NEWREMAIN
      INTEGER IR
      LOGICAL YACSTE
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION MASSET,MASSETN,C,CPREV,CINIT,HFL1,HFL2,TET,TETA
      DOUBLE PRECISION H1N,H2N,FLBNEG
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT
      LOGICAL TESTING
      DATA TESTING/.FALSE./
C
C-----------------------------------------------------------------------
C
C     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER, ALLOCATABLE :: INDIC(:)
      SAVE
      IF(.NOT.DEJA) THEN
        ALLOCATE(INDIC(MESH%NSEG))
        DEJA=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
      FXMAT=>MESH%MSEG%X%R(1:MESH%NSEG)
C
C-----------------------------------------------------------------------
C
      NPOIN=H%DIM1
C
C     EXTRACTS THE OPTIONS
C
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
C
C-----------------------------------------------------------------------
C
C     STARTS AGAIN FROM NON CORRECTED DEPTH
C
      IF(TESTING) THEN
        C=1.D99
        CINIT=1.D99
        DO I=1,NPOIN
          C    =MIN(C    ,H%R(I))
          CINIT=MIN(CINIT,HN%R(I))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_DMIN(C)
          CINIT=P_DMIN(CINIT)
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, H MIN=',C
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, HN MIN=',CINIT
      ENDIF
C
C     COMPUTES THE FLUXES BY NODES
C
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
      CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
     &            HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
C                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
C                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
C     COMPUTES THE FLUXES BY SEGMENT (TE1 FOLLOWED BY FALSE NOT USED)
C     FXMAT IS NOT ASSEMBLED IN PARALLEL MODE
C
C----------------------------------------
C VARIOUS OPTIONS TO COMPUTE THE FLUXES
C----------------------------------------
C
      CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                MESH%ELTSEG%I,MESH%ORISEG%I,
     &                MESH%IKLE%I,.TRUE.,IOPT1)
C
C----------------------------------------
C
C     AVERAGES FLUXES ON INTERFACE SEGMENTS BY ASSEMBLING AND
C     DIVIDING BY 2. THIS WILL GIVE THE UPWINDING INFORMATION
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FXMAT,FXMAT,FXMAT,MESH%NSEG,1,2,1,MESH,
     &                   1)
        CALL MULT_INTERFACE_SEG(FXMAT,MESH%NH_COM_SEG%I,
     &                          MESH%NH_COM_SEG%DIM1,
     &                          MESH%NB_NEIGHB_SEG,
     &                          MESH%NB_NEIGHB_PT_SEG%I,
     &                          0.5D0,MESH%NSEG)
      ENDIF
C
C----------------------------------------
C END OF THE OPTIONS
C----------------------------------------
C
      CALL CPSTVC(H,T2)
C
C     INITIALISES F TO ITS OLD VALUE
C
      CALL OS('X=Y     ',X=F,Y=FN)
C
      CPREV=0.D0
      DO I=1,MESH%NSEG
        CPREV=CPREV+ABS(FXMAT(I))
      ENDDO
      IF(NCSIZE.GT.1) CPREV=P_DSUM(CPREV)
      CINIT=CPREV
      IF(TESTING) WRITE(LU,*) 'SOMME INITIALE DES FLUX=',CPREV
C
C     LOOP ON THE SEGMENTS,
C     TO TAKE ELIGIBLE FLUXES INTO ACCOUNT
C
C     ADDS THE SOURCES (SMH IS NATURALLY ASSEMBLED IN PARALLEL MODE)
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            HT%R(I)=HN%R(I)+DT*SMH%R(I)
            F%R(I)=FN%R(I)+DT/MAX(HT%R(I),1.D-4)*SMH%R(I)*FSCEXP%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            HT%R(I)=HN%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
            F%R(I)=FN%R(I)+DT/MAX(HT%R(I),1.D-4)*
     &                       UNSV2D%R(I)*SMH%R(I)*FSCEXP%R(I)
          ENDDO
        ENDIF
      ELSE
        DO I=1,NPOIN
          HT%R(I)=HN%R(I)
        ENDDO
      ENDIF
C
C     BOUNDARY FLUXES : ADDS THE ENTERING (NEGATIVE) FLUXES
C     FIRST PUTS FLBOR (BOUNDARY) IN T2 (DOMAIN)
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
C     ASSEMBLES T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        HT%R(I)=HT%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
C       ENTERING FLUXES OF TRACERS
C       THE FINAL DEPTH IS TAKEN
        IF(LIMTRA(IPTFR).EQ.KDIR) THEN
          F%R(I)=FN%R(I)-DT/MAX(HT%R(I),1.D-4)*
     &       UNSV2D%R(I)*MIN(T2%R(I),0.D0)*(FBOR%R(IPTFR)-FN%R(I))
        ELSEIF(LIMTRA(IPTFR).EQ.KDDL) THEN
          IF(T2%R(I).LE.0.D0) THEN
C           FLBORTRA IS NOT ASSEMBLED
            FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*FN%R(I)
          ENDIF
        ENDIF
      ENDDO
C
C     TO OPTIMISE THE LOOP ON SEGMENTS, ONLY SEGMENTS WITH
C     NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST WILL BE
C     UPDATED. TO START WITH, ALL FLUXES ARE ASSUMED NON ZERO
C
      REMAIN_SEG=MESH%NSEG
      DO I=1,REMAIN_SEG
        INDIC(I)=I
      ENDDO
C
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
C
C     AT THIS LEVEL H THE SAME AT INTERFACE POINTS
C
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
C         AVAILABLE DEPTH IS SHARED BETWEEN PROCESSORS
C         NACHB(1,IPTFR) WITH DIMENSION NACHB(NBMAXNSHARE,NPTIR)
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          HT%R(I)=HT%R(I)*MESH%FAC%R(I)
        ENDDO
      ENDIF
C
      C=0.D0
C     DO I=1,MESH%NSEG  (REPLACED WITH 3 FOLLOWING LINES)
      NEWREMAIN=0
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        IF(FXMAT(I).GT.1.D-15) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL1= DT*UNSV2D%R(I1)*FXMAT(I)
          HFL2=-DT*UNSV2D%R(I2)*FXMAT(I)
C         FOR TRACERS
          H1N=HT%R(I1)
          H2N=HT%R(I2)
C         END OF 'FOR TRACERS'
          IF(HFL1.GT.HT%R(I1)) THEN
            TET=HT%R(I1)/HFL1
            HT%R(I1)=0.D0
            HT%R(I2)=HT%R(I2)-HFL2*TET
            FXMAT(I)=FXMAT(I)*(1.D0-TET)
            C=C+ABS(FXMAT(I))
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            HT%R(I1)=HT%R(I1)-HFL1
            HT%R(I2)=HT%R(I2)-HFL2
          ENDIF
C         TRACER (WITH TEST HT%R(I2) CANNOT BE 0.D0)
          IF(H2N.LT.HT%R(I2)) THEN
            F%R(I2)=F%R(I2)+(1.D0-H2N/HT%R(I2))*(F%R(I1)-F%R(I2))
          ENDIF
C         END TRACER
        ELSEIF(FXMAT(I).LT.-1.D-15) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL1= DT*UNSV2D%R(I1)*FXMAT(I)
          HFL2=-DT*UNSV2D%R(I2)*FXMAT(I)
C         FOR TRACERS
          H1N=HT%R(I1)
          H2N=HT%R(I2)
C         END OF 'FOR TRACERS'
          IF(HFL2.GT.HT%R(I2)) THEN
            TET=HT%R(I2)/HFL2
            HT%R(I1)=HT%R(I1)-HFL1*TET
            HT%R(I2)=0.D0
            FXMAT(I)=FXMAT(I)*(1.D0-TET)
            C=C+ABS(FXMAT(I))
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            HT%R(I1)=HT%R(I1)-HFL1
            HT%R(I2)=HT%R(I2)-HFL2
          ENDIF
C         TRACER (WITH TEST HT%R(I1) CANNOT BE 0.D0)
          IF(H1N.LT.HT%R(I1)) THEN
            F%R(I1)=F%R(I1)+(1.D0-H1N/HT%R(I1))*(F%R(I2)-F%R(I1))
          ENDIF
C         END TRACER
        ENDIF
      ENDDO
C
      REMAIN_SEG=NEWREMAIN
C
C     MERGES DEPTHS AND F AT INTERFACE POINTS
C
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
C         ARRAY WITH HT*F AT INTERFACE POINTS
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          T1%R(I)=HT%R(I)*F%R(I)
        ENDDO
C       SUMS UP HT*F AT INTERFACE POINTS
        CALL PARCOM(T1,2,MESH)
C       SUMS UP THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS
        CALL PARCOM(HT,2,MESH)
C       AVERAGES F AT INTERFACE POINTS
        DO IPTFR=1,NPTIR
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          IF(HT%R(I).GT.0.D0) F%R(I)=T1%R(I)/HT%R(I)
        ENDDO
      ENDIF
C
      IF(NCSIZE.GT.1) C=P_DSUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     &             .AND.C.NE.0.D0) THEN
        CPREV=C
        GO TO 777
      ENDIF
C
C     BOUNDARY FLUXES : ADDS THE EXITING (POSITIVE) FLUXES
C                       WITH A POSSIBLE LIMITATION
C
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
C                               T2 = ASSEMBLED FLBOR IN PARALLEL MODE
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        TET=1.D0
C       NEXT LINE SHOULD NEVER HAPPEN (DONE IN POSITIVE_DEPTHS)
        IF(HFL1.GT.HT%R(I)) TET=HT%R(I)/HFL1
C       MAX IS ONLY TO PREVENT TRUNCATION ERROR
        HT%R(I)=MAX(HT%R(I)-HFL1*TET,0.D0)
C       CAPS FLBOR (MUST HAVE ALREADY BEEN DONE
C                            IN POSITIVE_DEPTHS)
C       FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*TET

        IF(LIMTRA(IPTFR).EQ.KDIR) THEN
          F%R(I)=F%R(I)-HFL1*TET/MAX(HT%R(I),1.D-4)*
     &           (FBOR%R(IPTFR)-F%R(I))
          FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*FBOR%R(IPTFR)
        ELSEIF(LIMTRA(IPTFR).EQ.KDDL) THEN
          IF(T2%R(I).GT.0.D0) THEN
            FLBORTRA%R(IPTFR)=FLBOR%R(IPTFR)*F%R(I)
          ENDIF
        ELSE
          FLBORTRA%R(IPTFR)=0.D0
        ENDIF
      ENDDO
C
      IF(TESTING) THEN
        C=0.D0
        DO I=1,NPOIN
          C=C+(HT%R(I)-H%R(I))**2
        ENDDO
C       WRONG BUT DOES NOT BEAR CONSEQUENCES IF 0
        IF(NCSIZE.GT.1) C=P_DSUM(C)
        WRITE(LU,*) 'DIFFERENCE ENTRE H ET HT =',C
!
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,F%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR MIN=',C
        C=-1.D99
        DO I=1,NPOIN
          C=MAX(C,F%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMAX(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR MAX=',C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     EXPLICIT SOURCE TERM
C
      DO I = 1,MESH%NPOIN
        F%R(I) = F%R(I)+DT*SM%R(I)
      ENDDO
C
C     IMPLICIT SOURCE TERM
C
      IF(YASMI) THEN
        DO I = 1,MESH%NPOIN
          F%R(I) = F%R(I)/(1.D0-DT*SMI%R(I)/MAX(H%R(I),1.D-4))
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(ENTET) THEN
        IF(LNG.EQ.1) WRITE(LU,101) NITER
        IF(LNG.EQ.2) WRITE(LU,102) NITER
      ENDIF
!
101   FORMAT(' CVTRVF_POS (SCHEMA 14) : ',1I3,' ITERATIONS')
102   FORMAT(' CVTRVF_POS (SCHEME 14): ',1I3,' ITERATIONS')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C