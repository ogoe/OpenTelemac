!                    ********************************
                     SUBROUTINE BEDLOAD_SOLIDISCHARGE
!                    ********************************
!
     &(MESH,U2D,V2D,UNORM,HN,TW,UW,MU,TOB,CF,TOBW,FW,THETAW,
     & AVA,MASKPT,MASKEL,ACLADM,UNLADM,KSP,KSR,LIQBOR,
     & QBOR,DEBUG,NPOIN,NPTFR,IELMT,ICF,KENT,OPTBAN,
     & HIDFAC,GRAV,DM,D90,XWC,XMVE,XMVS,XKV,VCE,HMIN,
     & HIDI,KARMAN,ZERO,PI,KARIM_HOLLY_YANG,
     & SUSP,MSK,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     & T11,T12,AC,HIDING,QSC,QSS,
     & SLOPEFF,COEFPN,PHISED,CALFA,SALFA,BETA,ZF_C,S,
     & DEVIA,BETA2,SECCURRENT,BIJK,HOULE,UNSV2D,U3D,V3D,CODE)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        20/05/1995
!+        V5P1
!+   
!
!history  B. MINH DUC
!+        **/12/2001
!+        V5P2
!+   
!
!history  M. GONZALES DE LINARES
!+        **/07/2002
!+        V5P3
!+   
!
!history  C. VILLARET
!+        **/10/2003
!+        V5P4
!+   
!
!history  F. HUVELIN
!+        14/09/2004
!+        V5P5
!+   
!
!history  J.-M. HERVOUET
!+        11/03/2008
!+        V5P9
!+   
!
!history  
!+        11/03/2009
!+        
!+   MODIFICATIONS FOR PARALLEL MODE 
!
!history  J.-M. HERVOUET
!+        15/09/2009
!+        V6P0
!+   
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
!| AVA            |---| 
!| BETA           |---| 
!| BETA2          |---| 
!| BIJK           |---| 
!| CALFA          |---| 
!| CF             |---| 
!| COEFPN         |---| 
!| D90            |---| 
!| DEBUG          |---| 
!| DEVIA          |---| 
!| DM             |---| 
!| FW             |---| 
!| GRAV           |---| 
!| HIDFAC         |---| 
!| HIDI           |---| 
!| HIDING         |---| 
!| HMIN           |---| 
!| HN             |---| 
!| HOULE          |---| 
!| ICF            |---| 
!| IELMT          |---| 
!| KARMAN         |---| 
!| KENT           |---| 
!| KSP            |---| 
!| KSR            |---| 
!| LIQBOR         |---| 
!| MASKEL         |---| 
!| MASKPT         |---| 
!| MESH           |---| 
!| MSK            |---| 
!| MU             |---| 
!| NPOIN          |---| 
!| NPTFR          |---| 
!| OPTBAN         |---| 
!| PHISED         |---| 
!| PI             |---| 
!| QBOR           |---| 
!| QSC            |---| 
!| QSS            |---| 
!| S              |---| 
!| SALFA          |---| 
!| SECCURRENT     |---| 
!| SLOPEFF        |---| 
!| SUSP           |---| 
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
!| THETAW         |---| 
!| TOB            |---| 
!| TOBW           |---| 
!| TW             |---| 
!| U2D            |---| 
!| UNLADM         |---| 
!| UNORM          |---| 
!| UNSV2D         |---| 
!| UW             |---| 
!| V2D            |---| 
!| VCE            |---| 
!| XKV            |---| 
!| XMVE           |---| 
!| XMVS           |---| 
!| XWC            |---| 
!| ZERO           |---| 
!| ZF_C           |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_SOLIDISCHARGE => BEDLOAD_SOLIDISCHARGE
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D,  HN, TW, UW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UNORM ,MU, KSR ,KSP
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF, TOBW, FW, THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKPT, MASKEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, UNLADM, LIQBOR, QBOR
      INTEGER,          INTENT(IN)    :: DEBUG
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, ICF
      INTEGER,          INTENT(IN)    :: KENT, OPTBAN,HIDFAC
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, D90, XWC, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: XKV, VCE, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: SUSP, MSK,SECCURRENT,HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7,T8,T9,T10,T11,T12
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC,QSS
!
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: PHISED,BETA,BETA2
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF_C,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CALFA,SALFA,COEFPN
!
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,AVA(NPOIN)
!
!RK
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
!RK
      DOUBLE PRECISION U3DNORM
!
      INTEGER          :: I
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
! ********************************************************
! 0 - COMPUTES THE PARAMETERS FOR THE SLOPE EFFECT
! ********************************************************
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EFFPNT'
!
!     SLOPE EFFECT
!
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        DO I=1,NPOIN
          U3DNORM=SQRT(U3D%R(I)*U3D%R(I)+V3D%R(I)*V3D%R(I))
          IF(U3DNORM.GE.1.D-12) THEN
            CALFA%R(I)=U3D%R(I)/U3DNORM
            SALFA%R(I)=V3D%R(I)/U3DNORM
          ELSE
            CALFA%R(I)=1.D0
            SALFA%R(I)=0.D0
          ENDIF
        ENDDO
      ELSE
        CALL OS('X=Y/Z   ',CALFA, U2D, UNORM, 0.D0, 2, 1.D0, 1.D-12)
        CALL OS('X=Y/Z   ',SALFA, V2D, UNORM, 0.D0, 2, 0.D0, 1.D-12)
      ENDIF
!
      IF(SLOPEFF.EQ.0) CALL OS('X=C     ',X=COEFPN,C=1.D0)
!
      IF(SLOPEFF.NE.0.OR.DEVIA.NE.0) THEN
        CALL BEDLOAD_EFFPNT
     &     (MASKEL,LIQBOR,S,ZF_C,U2D,V2D,UNORM,NPOIN,NPTFR,IELMT,
     &      KENT,BETA,PI,MSK,MESH,T1,T2,T3,T4,
     &      COEFPN,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &      TOB,XMVS,XMVE,DM,GRAV,UNSV2D)
      ENDIF
!
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_EFFPNT'
!
      ! **************************************** !
      ! I - MASKING/EXPOSURE COEFFICIENT         !
      ! **************************************** !
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_HIDING_FACTOR'
!
!     WITH HUNZIKER FORMULATION (6), THE HIDING FACTOR IS COMPUTED
!     WITH THE SOLID DISCHARGE (SEE BEDLOAD_HUNZ_MEYER.F)
!
      IF(ICF.NE.6) THEN
        CALL BEDLOAD_HIDING_FACTOR
     &     (ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
      ENDIF
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_HIDING_FACTOR'
!
      ! ******************************************* !
      ! II - QSC COMPUTED USING EMPIRICAL FORMULATION !
      !      T1 = DQSC/DH                           !
      ! ******************************************* !
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_FORMULA'
!
      CALL BEDLOAD_FORMULA
     &  (U2D,V2D, UNORM,HN, CF, MU,TOB, TOBW, UW, TW, THETAW, FW,
     &   ACLADM, UNLADM, KSP,KSR,AVA, NPOIN, ICF, HIDFAC, XMVS, XMVE,
     &   DM, GRAV, VCE, XKV, HMIN, XWC, D90, KARMAN, ZERO,
     &   PI, SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9,
     &   T10, T11, T12, QSC, QSS, IELMT,SECCURRENT,
     &   SLOPEFF, COEFPN, BIJK, HOULE)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_FORMULA'
!
      ! **************************************************** !
      ! IV - BOUNDARY NODES WITH IMPOSED FLOW                !
      ! **************************************************** !
      IF (DEBUG > 0) WRITE(LU,*) 'BOUNDARY_NODES_TREATMENT'
      DO I = 1 , NPTFR
        IF(LIQBOR%I(I).EQ.KENT) QSC%R(MESH%NBOR%I(I)) = QBOR%R(I)
      ENDDO
      IF (DEBUG > 0) WRITE(LU,*) 'END_BOUNDARY_NODES_TREATMENT'
!
      ! ************************************ !
      ! V - TIDAL FLATS                      !
      ! ************************************ !
      IF(OPTBAN.EQ.2) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'TIDAL_FLATS_TREATMENT'
        CALL OS('X=XY    ', X=QSC, Y=MASKPT)
        IF (DEBUG > 0) WRITE(LU,*) 'END_TIDAL_FLATS_TREATMENT'
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END