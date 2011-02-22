
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FINITE VOLUMES, UPWIND, EXPLICIT AND MONOTONIC
!>               ADVECTOR EVEN WITH TIDAL FLATS.
!>               THIS IS A COPY OF CVTRVF_POS, WRITTEN FOR 2 VARIABLES.


C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!>            WITH NO FRICTION

!>  @warning  DISCRETISATION OF VISC

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
!> </td><td> 19/11/2010
!> </td><td> J-M HERVOUET   (LNHE) 01 30 87 80 18
!> </td><td> OPTIMIZATION (2 ABS SUPPRESSED)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C
C#######################################################################
C
                        SUBROUTINE CVTRVF_POS_2
     *(F1,F1N,F1SCEXP,F2,F2N,F2SCEXP,DIFT,CONV,H,HN,HPROP,UDEL,VDEL,DM1,
     * ZCONV,SOLSYS,VISC,VISC_S,SM1,SM2,SMH,YASMH,SMI1,SMI2,YASMI,
     * F1BOR,F2BOR,MASKTR,MESH,T1,T2,T3,T4,T5,T6,T7,T8,HNT,HT,AGGLOH,
     * TE1,DT,ENTET,BILAN,OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,
     * LIMTRA1,LIMTRA2,KDIR,KDDL,NPTFR,FLBOR,YAFLBOR,V2DPAR,UNSV2D,IOPT,
     * FLBORTRA1,FLBORTRA2,MASKPT,GLOSEG1,GLOSEG2,NBOR,
     * OPTION,FLULIM,YAFLULIM)
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
C| F              |<--| VALEURS A L' ETAPE N+1.
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
C| OPTION         |-->| OPTION OF ALGORITHM FOR EDGE-BASED ADVECTION
C|                |   | 1: FAST BUT SENSITIVE TO SEGMENT NUMBERING
C|                |   | 2: INDEPENDENT OF SEGMENT NUMBERING 
C| OPDTRA         |-->| MOT-CLE : OPTION POUR LA DIFFUSION DU TRACEUR
C| OPTSOU         |-->| OPTION DE TRAITEMENT DES TERMES SOURCES.
C|                |   | 1 : NORMAL
C|                |   | 2 : DIRAC
C| S              |-->| DUMMY STRUCTURE
C| SM             |-->| SOURCE TERMS.
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
      USE BIEF, EX_CVTRVF_POS => CVTRVF_POS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: OPDTRA,OPTSOU,KDIR,NPTFR,SOLSYS
      INTEGER, INTENT(IN)             :: KDDL,IOPT,OPTION
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: LIMTRA1(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMTRA2(NPTFR)
C                                                         NSEG
      DOUBLE PRECISION, INTENT(IN)    :: DT,AGGLOH,FLULIM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      LOGICAL, INTENT(IN)             :: BILAN,CONV,YASMH,YAFLBOR
      LOGICAL, INTENT(IN)             :: DIFT,MSK,ENTET,YASMI,YAFLULIM
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,H,HN,DM1,ZCONV,MASKPT
      TYPE(BIEF_OBJ), INTENT(IN)      :: V2DPAR,UNSV2D,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: F1,SM1,F2,SM2,HNT,HT
      TYPE(BIEF_OBJ), INTENT(IN)      :: F1BOR,UDEL,VDEL,F1N,SMI1,SMH
      TYPE(BIEF_OBJ), INTENT(IN)      :: F2BOR,F2N,SMI2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,FLBORTRA1,FLBORTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ), INTENT(IN)      :: F1SCEXP,F2SCEXP,S,MASKTR,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: VISC_S,VISC 
      TYPE(BIEF_MESH)                 :: MESH
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
      DOUBLE PRECISION H1N,H2N,FLBNEG,HSEG1,HSEG2
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: FXMAT
      LOGICAL TESTING
      DATA TESTING/.FALSE./
      DOUBLE PRECISION EPS_FLUX
      DATA             EPS_FLUX/1.D-15/
C
C-----------------------------------------------------------------------
C
C     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER, ALLOCATABLE          :: INDIC(:)
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
C     EXTRACTION DES OPTIONS
C
      IOPT2=IOPT/10
      IOPT1=IOPT-10*IOPT2
C
C-----------------------------------------------------------------------
C
C     STARTING AGAIN FROM NON CORRECTED DEPTH
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
C     CALCUL DES FLUX PAR NOEUDS
C
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
      CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
     *            HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
C                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
C                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
C     CALCUL DES FLUX PAR SEGMENT (TE1 SUIVI DE FALSE NON UTILISE)
C     FXMAT IS NOT ASSEMBLED IN //
C
C----------------------------------------
C DIFFERENT OPTIONS TO COMPUTE THE FLUXES
C----------------------------------------
C
      CALL FLUX_EF_VF(FXMAT,MESH%W%R,MESH%NSEG,MESH%NELEM,
     *                MESH%ELTSEG%I,MESH%ORISEG%I,
     *                MESH%IKLE%I,.TRUE.,IOPT1)
C
C----------------------------------------
C
C     AVERAGING FLUXES ON INTERFACE SEGMENTS BY ASSEMBLING AND
C     DIVIDING BY 2. THIS WILL GIVE THE UPWINDING INFORMATION 
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FXMAT,FXMAT,FXMAT,MESH%NSEG,1,2,1,MESH,
     *                   1)
        CALL MULT_INTERFACE_SEG(FXMAT,MESH%NH_COM_SEG%I,
     *                          MESH%NH_COM_SEG%DIM1,
     *                          MESH%NB_NEIGHB_SEG,
     *                          MESH%NB_NEIGHB_PT_SEG%I,
     *                          0.5D0,MESH%NSEG)
      ENDIF
C
C----------------------------------------
C END OF THE OPTIONS
C----------------------------------------
C
      CALL CPSTVC(H,T2)
C
C     INITIALIZING F1 AND F2 AT THE OLD VALUE
C
      CALL OS('X=Y     ',X=F1,Y=F1N)
      CALL OS('X=Y     ',X=F2,Y=F2N)
C
      CPREV=0.D0
      DO I=1,MESH%NSEG
        CPREV=CPREV+ABS(FXMAT(I))
      ENDDO
      IF(NCSIZE.GT.1) CPREV=P_DSUM(CPREV)
      CINIT=CPREV
      IF(TESTING) WRITE(LU,*) 'SOMME INITIALE DES FLUX=',CPREV
C
C     BOUCLE SUR LES SEGMENTS, POUR PRENDRE EN COMPTE LES FLUX
C     ADMISSIBLES
C
C     ADDING THE SOURCES (SMH IS NATURALLY ASSEMBLED IN //)
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            HT%R(I)=HN%R(I)+DT*SMH%R(I)
            F1%R(I)=F1N%R(I)+DT/MAX(HT%R(I),1.D-4)*
     *                       SMH%R(I)*F1SCEXP%R(I)
            F2%R(I)=F2N%R(I)+DT/MAX(HT%R(I),1.D-4)*
     *                       SMH%R(I)*F2SCEXP%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            HT%R(I)=HN%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
            F1%R(I)=F1N%R(I)+DT/MAX(HT%R(I),1.D-4)*
     *                       UNSV2D%R(I)*SMH%R(I)*F1SCEXP%R(I)
            F2%R(I)=F2N%R(I)+DT/MAX(HT%R(I),1.D-4)*
     *                       UNSV2D%R(I)*SMH%R(I)*F2SCEXP%R(I)
          ENDDO
        ENDIF
      ELSE
        DO I=1,NPOIN
          HT%R(I)=HN%R(I)
        ENDDO
      ENDIF
C
C     BOUNDARY FLUXES : ADDING THE ENTERING (NEGATIVE) FLUXES
C     FIRST PUTTING FLBOR (BOUNDARY) IN T2 (DOMAIN)
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
C     ASSEMBLING T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        HT%R(I)=HT%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0) 
!       ENTERING FLUXES OF TRACERS 
!       THE FINAL DEPTH IS TAKEN       
        IF(LIMTRA1(IPTFR).EQ.KDIR) THEN
          F1%R(I)=F1N%R(I)-DT/MAX(HT%R(I),1.D-4)*
     *    UNSV2D%R(I)*MIN(T2%R(I),0.D0)*(F1BOR%R(IPTFR)-F1N%R(I))
        ELSEIF(LIMTRA1(IPTFR).EQ.KDDL) THEN
          IF(T2%R(I).LE.0.D0) THEN
!           FLBORTRA1 IS NOT ASSEMBLED
            FLBORTRA1%R(IPTFR)=FLBOR%R(IPTFR)*F1N%R(I)
          ENDIF
        ENDIF 
        IF(LIMTRA2(IPTFR).EQ.KDIR) THEN
          F2%R(I)=F2N%R(I)-DT/MAX(HT%R(I),1.D-4)*
     *    UNSV2D%R(I)*MIN(T2%R(I),0.D0)*(F2BOR%R(IPTFR)-F2N%R(I))
        ELSEIF(LIMTRA2(IPTFR).EQ.KDDL) THEN
          IF(T2%R(I).LE.0.D0) THEN
!           FLBORTRA2 IS NOT ASSEMBLED
            FLBORTRA2%R(IPTFR)=FLBOR%R(IPTFR)*F1N%R(I)
          ENDIF
        ENDIF                                       
      ENDDO
C
C     FOR OPTIMIZING THE LOOP ON SEGMENTS, ONLY SEGMENTS
C     WITH NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST
C     WILL BE UPDATED. TO START WITH, ALL FLUXES ASSUMED NON ZERO
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
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     FOR DISTRIBUTING THE DEPTHS BETWEEN SEGMENTS
C
      IF(OPTION.EQ.2) THEN
C
C       T1 : TOTAL FLUX REMOVED OF EACH POINT  
C       T4 : DEPTH H SAVED
C       T5 : H*F1 SAVED
C       T6 : F1 SAVED
C       T7 : H*F2 SAVED
C       T8 : F2 SAVED
C
        CALL CPSTVC(H,T1)
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN
            T1%R(I)=0.D0
            T4%R(I)=HT%R(I)
            T6%R(I)=F1%R(I)
            T8%R(I)=F2%R(I)
            T5%R(I)=HT%R(I)*F1%R(I)
            T7%R(I)=HT%R(I)*F2%R(I)
          ENDDO
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
C             AVAILABLE DEPTH IS SHARED BETWEEN PROCESSORS
              HT%R(I)=HT%R(I)*MESH%FAC%R(I)
              T5%R(I)=T5%R(I)*MESH%FAC%R(I)
              T7%R(I)=T7%R(I)*MESH%FAC%R(I)
            ENDDO
          ENDIF 
        ELSE
C         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN_SEG
            I=INDIC(IR)
            I1=GLOSEG1(I)
            I2=GLOSEG2(I)
            T1%R(I1)=0.D0
            T1%R(I2)=0.D0
C           SAVING THE DEPTH AND TRACER
            T4%R(I1)=HT%R(I1)
            T4%R(I2)=HT%R(I2)
            T6%R(I1)=F1%R(I1)
            T6%R(I2)=F1%R(I2)
            T8%R(I1)=F2%R(I1)
            T8%R(I2)=F2%R(I2)
            T5%R(I1)=HT%R(I1)*F1%R(I1)
            T5%R(I2)=HT%R(I2)*F1%R(I2)
            T7%R(I1)=HT%R(I1)*F2%R(I1)
            T7%R(I2)=HT%R(I2)*F2%R(I2)
          ENDDO
C         CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
C         AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)   
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              T1%R(I)=0.D0
C             SAVING THE DEPTH AND TRACER
              T4%R(I)=HT%R(I)
              T6%R(I)=F1%R(I)
              T8%R(I)=F2%R(I)
C             AVAILABLE DEPTH IS SHARED BETWEEN PROCESSORS
              HT%R(I)=HT%R(I)*MESH%FAC%R(I)
              T5%R(I)=T5%R(I)*MESH%FAC%R(I)
              T7%R(I)=T7%R(I)*MESH%FAC%R(I)
            ENDDO
          ENDIF 
        ENDIF
        DO IR=1,REMAIN_SEG
          I=INDIC(IR)
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          IF(FXMAT(I).GT.EPS_FLUX) THEN
            T1%R(I1)=T1%R(I1)+FXMAT(I)
            HT%R(I1)=0.D0
            T5%R(I1)=0.D0
            T7%R(I1)=0.D0
          ELSEIF(FXMAT(I).LT.-EPS_FLUX) THEN
            T1%R(I2)=T1%R(I2)-FXMAT(I)
            HT%R(I2)=0.D0
            T5%R(I2)=0.D0
            T7%R(I2)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
!
!       FOR ISOLATED POINTS CONNECTED TO AN ACTIVE SEGMENT
!       THAT IS IN ANOTHER SUBDOMAIN     
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            IF(T1%R(I).GT.EPS_FLUX) THEN
              HT%R(I)=0.D0
              T5%R(I)=0.D0
              T7%R(I)=0.D0
            ENDIF
          ENDDO
        ENDIF 
!
      ELSEIF(OPTION.EQ.1) THEN
C
C       AT THIS LEVEL H THE SAME AT INTERFACE POINTS
C       THIS IS DONE EVEN FOR OPTION 2, TO ANTICIPATE THE FINAL PARCOM
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
C           AVAILABLE DEPTH IS SHARED BETWEEN PROCESSORS
C           NACHB(1,IPTFR) WITH DIMENSION NACHB(NBMAXNSHARE,NPTIR)
            I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            HT%R(I)=HT%R(I)*MESH%FAC%R(I)
          ENDDO
        ENDIF
C
      ENDIF
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      C=0.D0
      NEWREMAIN=0
C
      IF(OPTION.EQ.1) THEN
C
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)        
        IF(FXMAT(I).GT.EPS_FLUX) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL1= DT*UNSV2D%R(I1)*FXMAT(I)
          HFL2=-DT*UNSV2D%R(I2)*FXMAT(I)
          H1N=HT%R(I1)
          H2N=HT%R(I2)
          IF(HFL1.GT.HT%R(I1)) THEN
            TET=HT%R(I1)/HFL1
            HT%R(I1)=0.D0
            HT%R(I2)=HT%R(I2)-HFL2*TET
            FXMAT(I)=FXMAT(I)*(1.D0-TET)
            C=C+FXMAT(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            HT%R(I1)=HT%R(I1)-HFL1
            HT%R(I2)=HT%R(I2)-HFL2
          ENDIF
!         TRACER (WITH TEST HT%R(I2) CANNOT BE 0.D0)
          IF(H2N.LT.HT%R(I2)) THEN
            F1%R(I2)=F1%R(I2)
     *              +(1.D0-H2N/HT%R(I2))*(F1%R(I1)-F1%R(I2))
            F2%R(I2)=F2%R(I2)
     *              +(1.D0-H2N/HT%R(I2))*(F2%R(I1)-F2%R(I2))
          ENDIF
!         END TRACER                           
        ELSEIF(FXMAT(I).LT.-EPS_FLUX) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL1= DT*UNSV2D%R(I1)*FXMAT(I)
          HFL2=-DT*UNSV2D%R(I2)*FXMAT(I)
          H1N=HT%R(I1)
          H2N=HT%R(I2)
          IF(HFL2.GT.HT%R(I2)) THEN
            TET=HT%R(I2)/HFL2
            HT%R(I1)=HT%R(I1)-HFL1*TET
            HT%R(I2)=0.D0
            FXMAT(I)=FXMAT(I)*(1.D0-TET)
            C=C-FXMAT(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            HT%R(I1)=HT%R(I1)-HFL1
            HT%R(I2)=HT%R(I2)-HFL2
          ENDIF
!         TRACER (WITH TEST HT%R(I1) CANNOT BE 0.D0)
          IF(H1N.LT.HT%R(I1)) THEN
            F1%R(I1)=F1%R(I1)
     *              +(1.D0-H1N/HT%R(I1))*(F1%R(I2)-F1%R(I1))
            F2%R(I1)=F2%R(I1)
     *              +(1.D0-H1N/HT%R(I1))*(F2%R(I2)-F2%R(I1))
          ENDIF
!         FIN TRACEUR                           
        ENDIF
      ENDDO
C
      ELSEIF(OPTION.EQ.2) THEN
C
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        I1=GLOSEG1(I)
        I2=GLOSEG2(I)
        IF(FXMAT(I).GT.EPS_FLUX) THEN
!         SHARING ON DEMAND: FRACTION OF DEPTH TAKEN 
!         T4 IS THE STORED DEPTH
          IF(T4%R(I1).GT.0.D0) THEN
            HSEG1=T4%R(I1)*FXMAT(I)/T1%R(I1)
!           END OF SHARING ON DEMAND
            HFL1= DT*UNSV2D%R(I1)*FXMAT(I)
            IF(HFL1.GT.HSEG1) THEN
              TET=HSEG1/HFL1
!             HSEG2 AND THUS HT WILL BE STRICTLY POSITIVE
              HSEG2=DT*UNSV2D%R(I2)*FXMAT(I)*TET
              HT%R(I2)=HT%R(I2)+HSEG2
!             GROUPING H*F 
              T5%R(I2)=T5%R(I2)+HSEG2*T6%R(I1)
              T7%R(I2)=T7%R(I2)+HSEG2*T8%R(I1)
!             RECOMPUTING F (AS WEIGHTED AVERAGE)
!             THIS MAY BE DONE SEVERAL TIMES FOR THE SAME POINT
!             BUT THE LAST ONE WILL BE THE GOOD ONE
              F1%R(I2)=T5%R(I2)/HT%R(I2)
              F2%R(I2)=T7%R(I2)/HT%R(I2)
              FXMAT(I)=FXMAT(I)*(1.D0-TET)
              C=C+FXMAT(I) 
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I  
            ELSE
              HSEG1=HSEG1-HFL1
              HSEG2=DT*UNSV2D%R(I2)*FXMAT(I)            
              HT%R(I2)=HT%R(I2)+HSEG2
              T5%R(I2)=T5%R(I2)+HSEG2*T6%R(I1)
              T7%R(I2)=T7%R(I2)+HSEG2*T8%R(I1)
!             THE LAST ONE WILL BE THE GOOD ONE
              F1%R(I2)=T5%R(I2)/HT%R(I2)
              F2%R(I2)=T7%R(I2)/HT%R(I2)
              IF(HSEG1.GT.0.D0) THEN
                HT%R(I1)=HT%R(I1)+HSEG1
                T5%R(I1)=T5%R(I1)+HSEG1*T6%R(I1)
                T7%R(I1)=T7%R(I1)+HSEG1*T8%R(I1)
!               THE LAST ONE WILL BE THE GOOD ONE
                F1%R(I1)=T5%R(I1)/HT%R(I1)
                F2%R(I1)=T7%R(I1)/HT%R(I1)
              ENDIF            
            ENDIF
          ELSE
!           NO WATER NO FLUX TRANSMITTED, NOTHING CHANGED
            C=C+FXMAT(I) 
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I  
          ENDIF
        ELSEIF(FXMAT(I).LT.-EPS_FLUX) THEN
!         SHARING ON DEMAND
          IF(T4%R(I2).GT.0.D0) THEN
            HSEG2=-T4%R(I2)*FXMAT(I)/T1%R(I2)
!           END OF SHARING ON DEMAND
            HFL2=-DT*UNSV2D%R(I2)*FXMAT(I)
            IF(HFL2.GT.HSEG2) THEN
              TET=HSEG2/HFL2
!             HSEG1 AND THUS HT WILL BE STRICTLY POSITIVE
              HSEG1=-DT*UNSV2D%R(I1)*FXMAT(I)*TET
              HT%R(I1)=HT%R(I1)+HSEG1
              T5%R(I1)=T5%R(I1)+HSEG1*T6%R(I2)
              T7%R(I1)=T7%R(I1)+HSEG1*T8%R(I2)
!             THE LAST ONE WILL BE THE GOOD ONE
              F1%R(I1)=T5%R(I1)/HT%R(I1)
              F2%R(I1)=T7%R(I1)/HT%R(I1)
              FXMAT(I)=FXMAT(I)*(1.D0-TET)           
              C=C-FXMAT(I)     
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I          
            ELSE
              HSEG1=-DT*UNSV2D%R(I1)*FXMAT(I)
              HSEG2=HSEG2-HFL2
              HT%R(I1)=HT%R(I1)+HSEG1
              T5%R(I1)=T5%R(I1)+HSEG1*T6%R(I2)
              T7%R(I1)=T7%R(I1)+HSEG1*T8%R(I2)
              F1%R(I1)=T5%R(I1)/HT%R(I1)
              F2%R(I1)=T7%R(I1)/HT%R(I1)
              IF(HSEG2.GT.0.D0) THEN
                HT%R(I2)=HT%R(I2)+HSEG2
                T5%R(I2)=T5%R(I2)+HSEG2*T6%R(I2)
                T7%R(I2)=T7%R(I2)+HSEG2*T8%R(I2)
!               THE LAST ONE WILL BE THE GOOD ONE
                F1%R(I2)=T5%R(I2)/HT%R(I2)
                F2%R(I2)=T7%R(I2)/HT%R(I2)
              ENDIF             
            ENDIF
          ELSE
!           NO WATER NO FLUX TRANSMITTED, NOTHING CHANGED
            C=C-FXMAT(I)     
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I 
          ENDIF
        ENDIF
      ENDDO
C
C     ELSE
C       UNKNOWN OPTION
      ENDIF
C
      REMAIN_SEG=NEWREMAIN
C
C     MERGING DEPTHS AND F AT INTERFACE POINTS
C
      IF(NCSIZE.GT.1) THEN        
        DO IPTFR=1,NPTIR
C         ARRAY WITH HT*F AT INTERFACE POINTS
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          T1%R(I)=HT%R(I)*F1%R(I)
          T3%R(I)=HT%R(I)*F2%R(I)
        ENDDO 
C       SUMMING HT*F AT INTERFACE POINTS 
        CALL PARCOM(T1,2,MESH)
        CALL PARCOM(T3,2,MESH)       
C       SUMMING THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS 
        CALL PARCOM(HT,2,MESH) 
C       AVERAGE F1 AND F2 AT INTERFACE POINTS               
        DO IPTFR=1,NPTIR
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          IF(HT%R(I).GT.0.D0) THEN
            F1%R(I)=T1%R(I)/HT%R(I)
            F2%R(I)=T3%R(I)/HT%R(I)
          ENDIF
        ENDDO      
      ENDIF
C
      IF(NCSIZE.GT.1) C=P_DSUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C      
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     *             .AND.C.NE.0.D0) THEN
        CPREV=C
        GO TO 777
      ENDIF
C
C     BOUNDARY FLUXES : ADDING THE EXITING (POSITIVE) FLUXES
C                       WITH A POSSIBLE LIMITATION
C
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
C                               T2 = // ASSEMBLED FLBOR
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        TET=1.D0
!       NEXT LINE SHOULD NEVER HAPPEN (DONE IN POSITIVE_DEPTHS)
        IF(HFL1.GT.HT%R(I)) TET=HT%R(I)/HFL1
C       MAX IS ONLY TO PREVENT TRUNCATION ERROR
        HT%R(I)=MAX(HT%R(I)-HFL1*TET,0.D0)
C       LIMITATION OF FLBOR (MUST HAVE BEEN DONE ALREADY
C                            IN POSITIVE_DEPTHS)
C       FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*TET
C 
        IF(LIMTRA1(IPTFR).EQ.KDIR) THEN
          F1%R(I)=F1%R(I)-HFL1*TET/MAX(HT%R(I),1.D-4)*
     *           (F1BOR%R(IPTFR)-F1%R(I))
          FLBORTRA1%R(IPTFR)=FLBOR%R(IPTFR)*F1BOR%R(IPTFR)
        ELSEIF(LIMTRA1(IPTFR).EQ.KDDL) THEN
          IF(T2%R(I).GT.0.D0) THEN
            FLBORTRA1%R(IPTFR)=FLBOR%R(IPTFR)*F1%R(I)
          ENDIF
        ELSE
          FLBORTRA1%R(IPTFR)=0.D0
        ENDIF
        IF(LIMTRA2(IPTFR).EQ.KDIR) THEN
          F2%R(I)=F2%R(I)-HFL1*TET/MAX(HT%R(I),1.D-4)*
     *           (F2BOR%R(IPTFR)-F2%R(I))
          FLBORTRA2%R(IPTFR)=FLBOR%R(IPTFR)*F2BOR%R(IPTFR)
        ELSEIF(LIMTRA2(IPTFR).EQ.KDDL) THEN
          IF(T2%R(I).GT.0.D0) THEN
            FLBORTRA2%R(IPTFR)=FLBOR%R(IPTFR)*F2%R(I)
          ENDIF
        ELSE
          FLBORTRA2%R(IPTFR)=0.D0
        ENDIF
      ENDDO
C
      IF(TESTING) THEN
        C=0.D0
        DO I=1,NPOIN
          C=C+(HT%R(I)-H%R(I))**2
        ENDDO
!                       FAUX MAIS PAS GRAVE SI 0.
        IF(NCSIZE.GT.1) C=P_DSUM(C)
        WRITE(LU,*) 'DIFFERENCE ENTRE H ET HT =',C
!
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,F1%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR 1 MIN=',C
        C=-1.D99
        DO I=1,NPOIN
          C=MAX(C,F1%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMAX(C)
        WRITE(LU,*) 'APRES TRAITEMENT TRACEUR 1 MAX=',C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     EXPLICIT SOURCE TERM
C
      DO I = 1,MESH%NPOIN
        F1%R(I) = F1%R(I)+DT*SM1%R(I)
        F2%R(I) = F2%R(I)+DT*SM2%R(I)
      ENDDO
C
C     IMPLICIT SOURCE TERM
C
      IF(YASMI) THEN
        DO I = 1,MESH%NPOIN
          F1%R(I) = F1%R(I)/(1.D0-DT*SMI1%R(I)/MAX(H%R(I),1.D-4))
          F2%R(I) = F2%R(I)/(1.D0-DT*SMI2%R(I)/MAX(H%R(I),1.D-4))
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
101   FORMAT(' CVTRVF_POS_2 (SCHEMA 13 OU 14) : ',1I3,' ITERATIONS')
102   FORMAT(' CVTRVF_POS_2 (SCHEME 13 OR 14): ',1I3,' ITERATIONS')   
C
C-----------------------------------------------------------------------
C
      RETURN
      END
