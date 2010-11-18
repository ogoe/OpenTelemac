C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FINITE VOLUMES, UPWIND, EXPLICIT ADVECTOR.

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
!>    </th><td> AGGLOH, BILAN, CONV, DIFT, DM1, DT, ENTET, F, FBOR, FLBOR, FLBORTRA, FN, FSCEXP, H, HN, HNT, HPROP, HT, IOPT, KDDL, KDIR, LIMTRA, MASKEL, MASKPT, MASKTR, MASSOU, MESH, MSK, NPTFR, OPDTRA, OPTSOU, S, SM, SMH, SMI, SOLSYS, T1, T2, T3, T4, T5, T6, T7, T8, TE1, UCONV, UNSV2D, V2DPAR, VCONV, VISC, VISC_S, YAFLBOR, YASMH, YASMI, ZCONV
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DDT, DTMAX, DT_REMAIN, FORMUL, FXMAT, FXMATPAR, FXT2, I, IELMF, IOPT1, IOPT2, MASSET, MASSETN, NIT, NITMAX, SAVE_HNT, SAVE_HT, TDT, TSOU, YACSTE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CVTRVF, FXMAT, FXMATPAR, SAVE_HNT, SAVE_HT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CFLVF(), CPSTVC(), FLUX_EF_VF(), FLUX_MASK(), OS(), OSDB(), OV(), PARCOM(), PARCOM2_SEG(), PLANTE(), P_DMIN(), TRACVF(), VECTOR()
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
!> </td><td> 09/10/09
!> </td><td> CHI-TUAN PHAM  (LNHE) 01 30 87 ?? ??
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
!>                  2 : SCHEMA N
!>                  3 : SCHEMA PSI
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
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2DPAR
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
                        SUBROUTINE CVTRVF
     &(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,FBOR,MASKTR,MESH,
     & T1,T2,T3,T4,T5,T6,T7,T8,HNT,HT,AGGLOH,TE1,DT,ENTET,BILAN,
     & OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,LIMTRA,KDIR,KDDL,NPTFR,FLBOR,
     & YAFLBOR,V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT)
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
C|                |   | 2 : SCHEMA N
C|                |   | 3 : SCHEMA PSI
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
C| UNSV2D         |---| 
C| V2DPAR         |---| 
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
      USE BIEF, EX_CVTRVF => CVTRVF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: OPDTRA,OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: LIMTRA(NPTFR),KDDL,IOPT
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: BILAN,CONV,YASMH,YAFLBOR
      LOGICAL, INTENT(IN)             :: DIFT,MSK,ENTET,YASMI
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN)      :: V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F,SM,HNT,HT
      TYPE(BIEF_OBJ), INTENT(IN)      :: FBOR,UCONV,VCONV,FN,SMI,SMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(IN)      :: FSCEXP,S,MASKTR,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISC_S,VISC
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELMF,I,IOPT1,IOPT2
      LOGICAL YACSTE
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION MASSET,MASSETN,TSOU,DTMAX,DT_REMAIN,DDT,TDT
      DOUBLE PRECISION FXT2
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_HT,SAVE_HNT
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT,FXMATPAR
C
      DOUBLE PRECISION P_DMIN,P_DSUM
      EXTERNAL         P_DMIN,P_DSUM
C
      INTEGER NITMAX,NIT
      DATA NITMAX/200/
C
C-----------------------------------------------------------------------
C
      SAVE_HT =>HT%R
      SAVE_HNT=>HNT%R
      FXMAT=>MESH%MSEG%X%R(1:MESH%NSEG)
C     IN PARALLEL MODE, ASSEMBLED AND NON ASSEMBLED VERSIONS ARE DIFFERENT
      IF(NCSIZE.GT.1) THEN
        FXMATPAR=>MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG)
      ELSE
        FXMATPAR=>MESH%MSEG%X%R(1:MESH%NSEG)
      ENDIF
C
C-----------------------------------------------------------------------
C
C     EXTRACTS THE OPTIONS
C
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
C
C-----------------------------------------------------------------------
C
C     IELMF = F%ELM
C     FORCED TO LINEAR
      IELMF=11
C
C     TAKES MASS-LUMPING INTO ACCOUNT IN THE CONTINUITY EQUATION
C
      IF(ABS(1.D0-AGGLOH).GT.1.D-8) THEN
        CALL VECTOR(HT ,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,H ,S,S,S,S,S,MESH,MSK,MASKEL)
        CALL VECTOR(HNT,'=','MASVEC          ',IELMF,
     &              1.D0-AGGLOH,HN,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(HT ,2,MESH)
          CALL PARCOM(HNT,2,MESH)
        ENDIF
        CALL OS('X=YZ    ',X=HT ,Y=HT ,Z=UNSV2D)
        CALL OS('X=YZ    ',X=HNT,Y=HNT,Z=UNSV2D)
        CALL OS('X=X+CY  ',X=HT ,Y=H  ,C=AGGLOH)
        CALL OS('X=X+CY  ',X=HNT,Y=HN ,C=AGGLOH)
      ELSE
C       CALL OS('X=Y     ',X=HT ,Y=H )
C       CALL OS('X=Y     ',X=HNT,Y=HN)
        HT%R =>H%R
        HNT%R=>HN%R
      ENDIF
C
C     INITIALISES THE TRACER FLUX AT THE BOUNDARY
C
      DO I=1,MESH%NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
C         FLBOR IS NOT ASSEMBLED IN PARALLEL MODE
          FLBORTRA%R(I)=FLBOR%R(I)*FBOR%R(I)
        ELSE
C         FOR KDDL, WILL BE DONE IN TVF
          FLBORTRA%R(I)=0.D0
        ENDIF
      ENDDO
C
C     COMPUTES THE FLUXES PHIIJ = FXMAT
C
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
      CALL VECTOR(T2,'=',FORMUL,IELMF,-1.D0,
     &            HPROP,DM1,ZCONV,UCONV,VCONV,VCONV,MESH,MSK,MASKEL)
C                 T2 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
C                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
      NIT=0
      DT_REMAIN=DT
      TDT=0.D0
      CALL CPSTVC(HN,T7)
      CALL CPSTVC(H ,T5)
      CALL CPSTVC(H ,T4)
      CALL CPSTVC(F,T8)
C
C     T4 WILL BE F PROGRESSIVELY UPDATED
C     T5 WILL BE THE DEPTH AT THE END OF THE SUB-TIMESTEP
C     (INITIALISED HERE TO CALL CFLVF)
C
      DO I=1,HN%DIM1
        T4%R(I)=FN%R(I)
        T5%R(I)=HNT%R(I)
      ENDDO
C
C     T1 WILL BE THE DEPTH ACCORDING TO THE CONTINUITY EQUATION
C
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=HNT%R(I)
        ENDDO
      ENDIF
C
      IF(.NOT.YAFLBOR) THEN
C       MASK=8 FOR LIQUID BOUNDARIES
        CALL VECTOR(T3,'=','FLUBDF          ',1,1.D0,HPROP,HPROP,HPROP,
     &              UCONV,VCONV,VCONV,MESH,.TRUE.,MASKTR%ADR(8)%P)
      ENDIF
C
100   CONTINUE
      NIT=NIT+1
C
C----------------------------------------
C VARIOUS OPTIONS TO COMPUTE THE FLUXES
C----------------------------------------
C
      IF(NIT.EQ.1.OR.IOPT1.EQ.3) THEN
        CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1,T4)
C       CANCELS FLUXES TO AND FROM MASKED POINTS
        IF(MSK) THEN
          CALL FLUX_MASK(FXMAT,MESH%NSEG,
     &                   MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MASKPT%R)
        ENDIF
C       ASSEMBLES THE FLUXES AT INTERFACES IN PARALLEL MODE, THIS
C       IS FOR UPWINDING (STORED IN SECOND DIMENSION OF MESH%MSEG)
        IF(NCSIZE.GT.1) THEN
          CALL OV('X=Y     ',FXMATPAR,FXMAT,FXMAT,0.D0,MESH%NSEG)
          CALL PARCOM2_SEG(FXMATPAR,FXMATPAR,FXMATPAR,
     &                     MESH%NSEG,1,2,1,MESH,1)
        ENDIF
      ENDIF
C
C---------------------------------------------
C DETERMINES THE LARGEST ADMISSIBLE TIMESTEP
C---------------------------------------------
C
C     THIS COULD BE PUT OUTSIDE THE LOOP, BUT T7 USED LATER IN THE LOOP...
C
C     IN CFLVF, T7 WILL BE FLBOR WITH A DIMENSION NPOIN
      CALL OS('X=0     ',X=T7)
      IF(YAFLBOR) THEN
        CALL OSDB('X=Y     ',T7,FLBOR,FLBOR,0.D0,MESH)
      ELSE
        CALL OSDB('X=Y     ',T7,T3,T3,0.D0,MESH)
      ENDIF
      IF(NCSIZE.GT.1) CALL PARCOM(T7,2,MESH)
C
C     MASKS FLBOR IF(MSK)
C
      IF(MSK) CALL OS('X=XY    ',X=T7,Y=MASKPT)
C
C     COMPUTES THE MAXIMUM TIMESTEP ENSURING MONOTONICITY
C
      CALL CFLVF(DTMAX,T5%R,HT%R,FXMAT,FXMATPAR,
C                                   FLBOR%R(NPOIN)
     &           V2DPAR%R,DT_REMAIN,T7%R   ,SMH%R,
     &           YASMH,T8,MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &           MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH,MSK,MASKPT)
      IF(NCSIZE.GT.1) DTMAX=P_DMIN(DTMAX)
C
      DDT=MIN(DT_REMAIN,DTMAX)
      TDT=TDT+DDT
C
C     T5 WILL TAKE THE SUCCESSIVE VALUES OF H
C     AT THE END OF THE SUB-TIMESTEP
C
      DO I=1,HN%DIM1
        T5%R(I)=HNT%R(I)+TDT*(HT%R(I)-HNT%R(I))/DT
      ENDDO
C
C     IN TVF FACTOR HT/HLIN MAY TRIGGER DIVERGENCE FOR DRY POINTS
C
      IF(MSK) THEN
        DO I=1,HN%DIM1
          IF(MASKPT%R(I).LT.0.5D0) T5%R(I)=HT%R(I)
        ENDDO
      ENDIF
C
C-----------------
C FINAL RESOLUTION
C-----------------
C
      IF(YAFLBOR) THEN
        CALL TRACVF(F,FN,FSCEXP,HT,HNT,FXMAT,FXMATPAR,V2DPAR,UNSV2D,
     &              DDT,FLBOR,FBOR,SMH,YASMH,T1,T2,T4,T5,T6,T7,T8,
     &              MESH,LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,
     &              NIT,DT,TDT)
      ELSE
        CALL TRACVF(F,FN,FSCEXP,HT,HNT,FXMAT,FXMATPAR,V2DPAR,UNSV2D,
     &              DDT,T3,FBOR,SMH,YASMH,T1,T2,T4,T5,T6,T7,T8,MESH,
     &              LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,
     &              NIT,DT,TDT)
      ENDIF
C
      DO I=1,HN%DIM1
        T4%R(I)=F%R(I)
      ENDDO
      IF(IOPT2.EQ.1) THEN
        DO I=1,HN%DIM1
          T1%R(I)=T2%R(I)
        ENDDO
      ENDIF
C
      DT_REMAIN=DT_REMAIN-DDT
C
      IF(DT_REMAIN.NE.0.D0.AND.NIT.LT.NITMAX) GO TO 100
C
      IF(NIT.GE.NITMAX) THEN
        IF(LNG.EQ.1) WRITE(LU,900) NIT
        IF(LNG.EQ.2) WRITE(LU,901) NIT
900     FORMAT(1X,'CVTRVF : ',1I6,' SOUS-ITERATIONS DEMANDEES POUR LE'
     &   ,/,1X,   '         SCHEMA VF. DIMINUER LE PAS DE TEMPS')
901     FORMAT(1X,'CVTRVF: ',1I6,' SUB-ITERATIONS REQUIRED FOR THE'
     &   ,/,1X,   '         VF SCHEME. DECREASE THE TIME-STEP')
        CALL PLANTE(1)
        STOP
      ELSEIF(ENTET) THEN
        IF(LNG.EQ.1) WRITE(LU,902) NIT
        IF(LNG.EQ.2) WRITE(LU,903) NIT
902     FORMAT(1X,'CVTRVF (BIEF) : ',1I6,' SOUS-ITERATIONS')
903     FORMAT(1X,'CVTRVF (BIEF): ',1I6,' SUB-ITERATIONS')
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
C     LOCAL MASS BALANCE (FOR CHECKING PURPOSES)
C
C     CALL OS('X=Y-Z   ',X=T7,Y=T5,Z=HT)
C     PRINT*,'DIFFERENCE ENTRE H RECALCULE ET H : ',DOTS(T7,T7)
C     CHECKS THE TRACER EQUATION
C     CALL CPSTVC(FBOR,T4)
C     T4 : F AT THE BOUNDARIES AS TAKEN FOR THE BOUNDARY FLUXES
C     DO I=1,NPTFR
C       IF(LIMTRA(I).EQ.KDIR) THEN
C         T4%R(I)=FBOR%R(I)
C       ELSE
C         T4%R(I)=FN%R(MESH%NBOR%I(I))
C       ENDIF
C     ENDDO
C     CALL OS('X=YZ    ',X=T6,Y=FN,Z=HNT)
C     CALL OS('X=YZ    ',X=T7,Y=F ,Z=HT )
C     MASSETN=P_DOTS(V2DPAR,T6,MESH)
C     MASSET =P_DOTS(V2DPAR,T7,MESH)
C     FXT2   =P_DOTS(FLBOR,T4,MESH)
C     PRINT*,'MASSE INIT: ',MASSETN,' MASSE FINALE: ',MASSET
C     PRINT*,'FLUX: ',FXT2
C     MASSETN = MASSETN - FXT2*DT
C     TSOU=0.D0
C     IF(YASMH) THEN
C       IF(OPTSOU.EQ.1) THEN
C         DO I=1,MESH%NPOIN
C           MASSETN=MASSETN
C    *             +DT*V2DPAR%R(I)*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
C           TSOU=TSOU+DT*V2DPAR%R(I)*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
C         ENDDO
C       ELSEIF(OPTSOU.EQ.2) THEN
C         DO I=1,MESH%NPOIN
C           MASSETN=MASSETN
C    *             +DT*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
C           TSOU=TSOU+DT*SMH%R(I)*(FSCEXP%R(I)+FN%R(I))
C         ENDDO
C       ENDIF
C     ENDIF
C     PRINT*,'CREATION PAR SOURCE : ',TSOU
C     PRINT*,'ERREUR DE MASSE DE TRACEUR VF : ',MASSETN-MASSET
C     CHECKS THE CONTINUITY EQUATION
C     DO I = 1,MESH%NPOIN
C       T5%R(I)=V2DPAR%R(I)*(HT%R(I)-HNT%R(I))
C     ENDDO
C     DO I = 1,MESH%NSEG
C       T5%R(MESH%GLOSEG%I(I)) =
C    *  T5%R(MESH%GLOSEG%I(I)) + DT*MESH%MSEG%X%R(I)
C       T5%R(MESH%GLOSEG%I(I+MESH%NSEG)) =
C    *  T5%R(MESH%GLOSEG%I(I+MESH%NSEG)) - DT*MESH%MSEG%X%R(I)
C     ENDDO
C     DO I = 1,MESH%NPTFR
C       T5%R(MESH%NBOR%I(I))=T5%R(MESH%NBOR%I(I))+DT*FLBOR%R(I)
C     ENDDO
C     IF(YASMH) THEN
C       IF(OPTSOU.EQ.1) THEN
C         DO I = 1,MESH%NPOIN
C           T5%R(I)=T5%R(I)-DT*V2DPAR%R(I)*SMH%R(I)
C         ENDDO
C       ELSEIF(OPTSOU.EQ.2) THEN
C         DO I = 1,MESH%NPOIN
C           T5%R(I)=T5%R(I)-DT*SMH%R(I)
C         ENDDO
C       ENDIF
C     ENDIF
C     MASSET=0.D0
C     MASSETN = 0.D0
C     DO I = 1,MESH%NPOIN
C       MASSET=MASSET+T5%R(I)
C       MASSETN=MAX(MASSETN,ABS(T5%R(I)))
C     ENDDO
C     PRINT*,'ERREUR DE MASSE GLOBALE : ',MASSET,' LOCALE : ',MASSETN
C
C-----------------------------------------------------------------------
C
C     RETURNS POINTERS HT AND HNT
C
      HT%R =>SAVE_HT
      HNT%R=>SAVE_HNT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C