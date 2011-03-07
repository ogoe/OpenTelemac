!                    **************************
                     SUBROUTINE SUSPENSION_MAIN
!                    **************************
!
     &(SLVTRA,HN,HN_TEL,MU,TOB,ACLADM,KSP,KSR,KS,
     & CF,VOLU2D,V2DPAR,UNSV2D,AFBOR,
     & BFBOR,ZF,LICBOR,IFAMAS,MASKEL,MASKPT,U2D,V2D,NSICLA,NPOIN,
     & NPTFR,IELMT,OPTDIF,RESOL,LT,NIT,OPTBAN,OPTSUP,OPDTRA,
     & KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,ISOUS,NSOUS,DEBUG,
     & DTS,CSF_VASE,CSF_SABLE,ZERO,GRAV,XKX,XKY,KARMAN,
     & XMVE,XMVS,HMIN,XWC,VITCD,VITCE,PARTHENIADES,ENTET,BILMA,MSK,
     & CHARR,IMP_INFLOW_C,MESH,ZF_S,CS,CST,CTILD,CBOR,DISP,
     & IT1,IT2,IT3,IT4,TB,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,W1,
     & TE1,CLT,TE2,TE3,S,AM1_S,AM2_S,MBOR,ELAY,LIMDIF,
     & MASKTR, TETA_SUSP, AC, MASED0, MASINI, MASTEN,
     & MASTOU, ES,AVAIL,  ENTETS, PASS, ZFCL_S,
     & HPROP, FLUDPT, FLUDP, FLUER, DISP_C, KX, KY,
     & KZ, UCONV, VCONV,QSXS, QSYS, QSCLXS, QSCLYS, QSCL_S,
     & QS_S,QS_C,CSTAEQ,ICQ,MASTCP,MASFIN,MASDEPT,MASDEP,MASSOU,
     & CORR_CONV,ZREF,SEDCO,VISC_TEL,CODE,
     & DIFT,DM1,UCONV_TEL,VCONV_TEL,ZCONV,SOLSYS,FLBOR_TEL,FLBOR_SIS,
     & FLBORTRA,NUMLIQ,NFRLIQ,MIXTE,NCOUCH_TASS,CONC_VASE,
     & TOCE_VASE,FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MAIN SUBROUTINE FOR THE SUSPENDED-LOAD TRANSPORT.
!
!history  F. HUVELIN
!+        22/12/2004
!+        
!+   
!
!history  JMH
!+        25/06/2008
!+        V5P9
!+   CALL TO DIFFIN MOVED IN SUSPENSION_COMPUTATION 
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into 
!+   English comments 
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and 
!+   cross-referencing of the FORTRAN sources 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |---| 
!| ACLADM         |---| 
!| AFBOR          |---| 
!| AM1_S          |---| 
!| AM2_S          |---| 
!| AVAIL          |---| 
!| BFBOR          |---| 
!| BILMA          |---| 
!| CBOR           |---| 
!| CF             |---| 
!| CHARR          |---| 
!| CLT            |---| 
!| CODE           |---| 
!| CONC_VASE      |---| 
!| CORR_CONV      |---| 
!| CS             |---| 
!| CSF_SABLE      |---| 
!| CSF_VASE       |---| 
!| CST            |---| 
!| CSTAEQ         |---| 
!| CTILD          |---| 
!| DEBUG          |---| 
!| DIFT           |---| 
!| DISP           |---| 
!| DISP_C         |---| 
!| DM1            |---| 
!| DTS            |---| 
!| ELAY           |---| 
!| ENTET          |---| 
!| ENTETS         |---| 
!| ES             |---| 
!| FLBORTRA       |---| 
!| FLBOR_SIS      |---| 
!| FLBOR_TEL      |---| 
!| FLUDP          |---| 
!| FLUDPT         |---| 
!| FLUER          |---| 
!| FLUER_VASE     |---| 
!| GRAV           |---| 
!| HMIN           |---| 
!| HN             |---| 
!| HN_TEL         |---| 
!| HPROP          |---| 
!| ICQ            |---| 
!| IELMT          |---| 
!| IFAMAS         |---| 
!| IMP_INFLOW_C   |---| 
!| ISOUS          |---| 
!| IT1            |---| 
!| IT2            |---| 
!| IT3            |---| 
!| IT4            |---| 
!| KARMAN         |---| 
!| KDDL           |---| 
!| KDIR           |---| 
!| KENT           |---| 
!| KINC           |---| 
!| KLOG           |---| 
!| KNEU           |---| 
!| KS             |---| 
!| KSORT          |---| 
!| KSP            |---| 
!| KSR            |---| 
!| KX             |---| 
!| KY             |---| 
!| KZ             |---| 
!| LICBOR         |---| 
!| LIMDIF         |---| 
!| LT             |---| 
!| MASDEP         |---| 
!| MASDEPT        |---| 
!| MASED0         |---| 
!| MASFIN         |---| 
!| MASINI         |---| 
!| MASKEL         |---| 
!| MASKPT         |---| 
!| MASKTR         |---| 
!| MASSOU         |---| 
!| MASTCP         |---| 
!| MASTEN         |---| 
!| MASTOU         |---| 
!| MBOR           |---| 
!| MESH           |---| 
!| MIXTE          |---| 
!| MSK            |---| 
!| MS_SABLE       |---| 
!| MS_VASE        |---| 
!| MU             |---| 
!| NCOUCH_TASS    |---| 
!| NFRLIQ         |---| 
!| NIT            |---| 
!| NPOIN          |---| 
!| NPTFR          |---| 
!| NSICLA         |---| 
!| NSOUS          |---| 
!| NUMLIQ         |---| 
!| OPDTRA         |---| 
!| OPTBAN         |---| 
!| OPTDIF         |---| 
!| OPTSUP         |---| 
!| PARTHENIADES   |---| 
!| PASS           |---| 
!| QSCLXS         |---| 
!| QSCLYS         |---| 
!| QSCL_S         |---| 
!| QSXS           |---| 
!| QSYS           |---| 
!| QS_C           |---| 
!| QS_S           |---| 
!| RESOL          |---| 
!| S              |---| 
!| SEDCO          |---| 
!| SLVTRA         |---| 
!| SOLSYS         |---| 
!| T1             |---| 
!| T10            |---| 
!| T11            |---| 
!| T12            |---| 
!| T2             |---| 
!| T3             |---| 
!| T4             |---| 
!| T5             |---| 
!| T6             |---| 
!| T7             |---| 
!| T8             |---| 
!| T9             |---| 
!| TASS           |---| 
!| TB             |---| 
!| TE1            |---| 
!| TE2            |---| 
!| TE3            |---| 
!| TETA_SUSP      |---| 
!| TOB            |---| 
!| TOCE_MIXTE     |---| 
!| TOCE_VASE      |---| 
!| U2D            |---| 
!| UCONV          |---| 
!| UCONV_TEL      |---| 
!| UNSV2D         |---| 
!| V2D            |---| 
!| V2DPAR         |---| 
!| VCONV          |---| 
!| VCONV_TEL      |---| 
!| VISC_TEL       |---| 
!| VITCD          |---| 
!| VITCE          |---| 
!| VOLU2D         |---| 
!| W1             |---| 
!| XKX            |---| 
!| XKY            |---| 
!| XMVE           |---| 
!| XMVS           |---| 
!| XWC            |---| 
!| ZCONV          |---| 
!| ZERO           |---| 
!| ZF             |---| 
!| ZFCL_S         |---| 
!| ZF_S           |---| 
!| ZREF           |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_MAIN => SUSPENSION_MAIN
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (SLVCFG),    INTENT(INOUT) :: SLVTRA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,HN_TEL,MU,TOB,ACLADM
      TYPE (BIEF_OBJ),  INTENT(IN)    :: KSP,KSR,KS
      TYPE (BIEF_OBJ),  INTENT(IN)    :: CF,VOLU2D,AFBOR,BFBOR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: V2DPAR,UNSV2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: LICBOR, IFAMAS, MASKEL, MASKPT
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D, V2D,DM1,ZCONV,FLBOR_TEL
      INTEGER,          INTENT(IN)    :: NSICLA, NPOIN, NPTFR, IELMT
      INTEGER,          INTENT(IN)    :: OPTDIF, RESOL,LT, NIT
      INTEGER,          INTENT(IN)    :: OPTBAN,OPTSUP,OPDTRA,NFRLIQ
      INTEGER,          INTENT(IN)    :: KENT, KSORT, KLOG, KINC, KNEU
      INTEGER,          INTENT(IN)    :: KDIR,KDDL,ISOUS,NSOUS
      INTEGER,          INTENT(IN)    :: DEBUG,SOLSYS,NCOUCH_TASS
      INTEGER,          INTENT(IN)    :: NUMLIQ(NFRLIQ)
      DOUBLE PRECISION, INTENT(IN)    :: DTS,CSF_VASE,CSF_SABLE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: XKX,XKY,KARMAN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, HMIN, XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: VITCD, VITCE
      DOUBLE PRECISION, INTENT(IN)    :: PARTHENIADES
      LOGICAL,          INTENT(IN)    :: ENTET, BILMA, MSK, CHARR
      LOGICAL,          INTENT(IN)    :: IMP_INFLOW_C
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),MIXTE,TASS
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZF_S,CS,CST,CTILD,CBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP,IT1,IT2,IT3,IT4,TB
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T9,T10,T11,T12,W1,TE1,CLT
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: TE2,TE3,S,AM1_S,AM2_S,MBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ELAY, LIMDIF,FLBORTRA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MASKTR
      DOUBLE PRECISION, INTENT(INOUT) :: TETA_SUSP, AC(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASED0(NSICLA), MASINI(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN(NSICLA), MASTOU(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_VASE(10),CONC_VASE(10)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      LOGICAL,          INTENT(INOUT) :: ENTETS, PASS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,HPROP,ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT,FLUDP,FLUER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP_C,KX,KY,KZ,UCONV
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: VCONV,FLBOR_SIS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSXS,QSYS,QSCLXS,QSCLYS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSCL_S,QS_S,CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_VASE,TOCE_MIXTE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MS_SABLE,MS_VASE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: QS_C,VISC_TEL
      TYPE (BIEF_OBJ),  INTENT(IN)    :: UCONV_TEL,VCONV_TEL
      DOUBLE PRECISION, INTENT(OUT)   :: MASTCP(NSICLA),MASFIN(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASDEPT(NSICLA),MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASSOU
      INTEGER, INTENT(IN)             :: ICQ
      LOGICAL, INTENT (IN)            :: CORR_CONV,DIFT
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I
!
       DOUBLE PRECISION, EXTERNAL :: P_DSUM
!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      IF(PASS) THEN
      ! *************************  !
      ! III - INITIAL MASS-BALANCE !
      ! *************************  !
!
      IF(BILMA) THEN
         DO I = 1,NSICLA
!
            CALL VECTOR(T1, '=', 'MASVEC          ', IELMT, 1.D0,
     &                  CS%ADR(I)%P, T1, T1, T1, T1, T1, MESH, MSK,
     &                  MASKEL)
!
            MASED0(I) = DOTS(T1,HN)
            IF(NCSIZE.GT.1) MASED0(I)=P_DSUM(MASED0(I))
            MASINI(I) = MASED0(I)
            MASTEN(I) = 0.D0
            MASTOU(I) = 0.D0
            MASTCP(I) = 0.D0
            IF(LNG.EQ.1) WRITE(LU,1) I, MASED0(I)
            IF(LNG.EQ.2) WRITE(LU,2) I, MASED0(I)
         ENDDO
      ENDIF
!
      !----------------------------------------------------------------!
001   FORMAT(1X,'QUANTITE INITIALE EN SUSPENSION POUR LA CLASSE ',
     &       I2,' : ', G16.7, ' M3')
      !----------------------------------------------------------------!
002   FORMAT(1X,'INITIAL QUANTITY IN SUSPENSION FOR CLASS ',
     &       I2,' : ', G16.7, ' M3')
      !----------------------------------------------------------------!
!     END OF IF(PASS)
      ENDIF
      PASS = .FALSE.
      ! ********************************* !
      ! V - COMPUTES THE DISPERSION       !
      ! ********************************* !
      IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_DISPERSION'
      CALL SUSPENSION_DISPERSION
     &     (TOB,XMVE,HN,OPTDIF,NPOIN,XKX,XKY,T1,T2,T3,KX,KY,KZ,DISP,
     &      U2D,V2D,VISC_TEL,CODE)
      IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_DISPERSION'
      ! ************************************************ !
      ! VI  - COMPUTES THE CONCENTRATION AND EVOLUTION   !
      ! ************************************************ !
       CALL OS('X=Y     ', X=HPROP, Y=HN)
!      CALL OS('X=+(Y,C)', X=HCLIP, Y=HN, C=HMIN)
       DO I = 1, NSICLA
         CALL OS('X=0     ', X=ZFCL_S%ADR(I)%P)
         IF(DEBUG > 0) WRITE(LU,*)
     &                'SUSPENSION_COMPUTATION : ',I,'/',NSICLA
         CALL SUSPENSION_COMPUTATION(SLVTRA,HN,HN_TEL,UCONV,
     & VCONV,CF,MU,TOB,ACLADM,KSP,KSR,KS,ELAY,AVAIL(1:NPOIN,1,I),
     & AFBOR,BFBOR,LIMDIF,
     & CLT,MASKEL,MASKTR,MASKPT,IFAMAS,NPOIN,IELMT,NPTFR,I,LT,NIT,
     & RESOL,OPTBAN,KENT,KDDL,KDIR,KSORT,KLOG,KINC,KNEU,OPTSUP,
     & OPDTRA,DEBUG,CSF_VASE, CSF_SABLE, TETA_SUSP,DTS,MASED0(I),ZERO,
     & XWC(I),KARMAN,XMVE,XMVS,GRAV,HMIN,VITCD,VITCE,
     & PARTHENIADES,ENTETS,ENTET,BILMA,
     & MSK,CHARR,IMP_INFLOW_C,MESH,ZF,CS%ADR(I)%P,
     & CST%ADR(I)%P,CTILD%ADR(I)%P,CBOR%ADR(I)%P,DISP,IT1,IT2,
     & IT3,IT4,TB,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     & W1,TE1,TE2,TE3,S,AM1_S,AM2_S,MBOR,MASTEN(I),MASTOU(I),
     & MASINI(I),AC(I),ZFCL_S%ADR(I)%P,FLUDPT%ADR(I)%P,
     & FLUDP%ADR(I)%P,FLUER%ADR(I)%P,HPROP,DISP_C,CSTAEQ,
     & MASFIN(I),MASDEPT(I),MASDEP(I),MASSOU,QS_C,ICQ,ZREF,
     & CORR_CONV,U2D,V2D,SEDCO(I),DIFT,DM1,ZCONV,UCONV_TEL,
     & VCONV_TEL,SOLSYS,FLBOR_TEL,FLBOR_SIS,FLBORTRA,CODE,VOLU2D,
     & V2DPAR,UNSV2D,NUMLIQ,NFRLIQ,LICBOR,MIXTE,AVAIL,NSICLA,ES,
     & NCOUCH_TASS,CONC_VASE,TOCE_VASE,
     & FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS)
         IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_COMPUTATION'
!
!        CV MODIFICATIONS : 03/2006
!        TAKES INTO ACCOUNT ADVECTION VELOCITY
         CALL OS('X=YZ    ', X=T1, Y=UCONV, Z=HN)
         CALL OS('X=YZ    ', X=T2, Y=VCONV, Z=HN)
!        JMH MODIFICATION : 25/06/2008 WHAT WAS THIS ?
!        CALL OS('X=ABS(Y)', X=T1, Y=T1)
!        CALL OS('X=ABS(Y)', X=T2, Y=T2)
         CALL OS('X=Y     ', X=CS%ADR(I)%P, Y=CST%ADR(I)%P)
         CALL OS('X=YZ    ', X=QSCLXS%ADR(I)%P, Y=CS%ADR(I)%P, Z=T1)
         CALL OS('X=YZ    ', X=QSCLYS%ADR(I)%P, Y=CS%ADR(I)%P, Z=T2)
!
      ENDDO
!
      ! *********************************************************** !
      ! VII  - UPDATES EVOLUTION, CONCENTRATION AND TRANSPORT RATE  !
      ! *********************************************************** !
!
      IF (DEBUG > 0) WRITE(LU,*) 'UPDATING_DATA'
!
      CALL OS('X=0     ', X=QSXS)
      CALL OS('X=0     ', X=QSYS)
      CALL OS('X=0     ', X=ZF_S)
!
      DO I = 1, NSICLA
        CALL OS('X=X+Y   ', X=ZF_S, Y=ZFCL_S%ADR(I)%P)
        CALL OS('X=X+Y   ', X=QSXS, Y=QSCLXS%ADR(I)%P)
        CALL OS('X=X+Y   ', X=QSYS, Y=QSCLYS%ADR(I)%P)
      ENDDO
      CALL OS('X=N(Y,Z)', X=QSCL_S, Y=QSCLXS, Z=QSCLYS)
      CALL OS('X=N(Y,Z)', X=QS_S, Y=QSXS, Z=QSYS)
      IF (DEBUG > 0) WRITE(LU,*) 'END_UPDATING_DATA'
!======================================================================!
!======================================================================!
      RETURN
      END