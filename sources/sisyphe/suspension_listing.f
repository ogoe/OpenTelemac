!                    *******************************
                     SUBROUTINE SUSPENSION_LISTING !
!                    *******************************
!
     &(MESH,CST,ZFCL_S,UCONV,VCONV,MASKEL,IELMT,DT,MSK,T1)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    WRITES OUT MIN/MAX VALUES.
!
!history  F. HUVELIN
!+        22/12/04
!+        V5P8
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CST            |<->| CONCENTRATION AT TIME T(N+1)
!| DT             |-->| TIME STEP IN SECONDS
!| IELMT          |-->| NUMBER OF ELEMENTS
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS 
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| UCONV          |<->| X-COMPONENT ADVECTION FIELD (TELEMAC)
!| VCONV          |<->| Y-COMPONENT ADVECTION FIELD 
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_LISTING => SUSPENSION_LISTING
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CST,ZFCL_S
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCONV,VCONV,MASKEL
      INTEGER,          INTENT(IN)    :: IELMT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: IMAX,IMA
      DOUBLE PRECISION :: XMAX,XMA
      INTEGER                        P_IMAX
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN,P_IMAX
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      CALL MAXI(XMAX,IMAX,CST%R,MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMAX(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,500) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,510) XMA, IMA
      CALL MINI(XMAX, IMAX, CST%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMIN(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,501) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,511) XMA, IMA
      CALL MAXI(XMAX, IMAX, ZFCL_S%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMAX(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,502) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,512) XMA, IMA
      CALL MINI(XMAX, IMAX, ZFCL_S%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMIN(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,503) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,513) XMA, IMA
!
      CALL CFLPSI(T1, UCONV, VCONV, DT, IELMT, MESH, MSK, MASKEL)
      CALL MAXI(XMAX, IMAX, T1%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMAX(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,507) XMA,IMA
      IF(LNG.EQ.2) WRITE(LU,517) XMA,IMA
      !----------------------------------------------------------------!
500   FORMAT(' CONCENTRATION MAXIMALE     : ',G16.7,' %, NOEUD = ',1I8)
501   FORMAT(' CONCENTRATION MINIMALE     : ',G16.7,' %, NOEUD = ',1I8)
502   FORMAT(' EVOLUTION MAXIMALE         : ',G16.7,'  , NOEUD = ',1I8)
503   FORMAT(' EVOLUTION MINIMALE         : ',G16.7,'  , NOEUD = ',1I8)
507   FORMAT(' CFL MAX POUR LA SUSPENSION : ',G16.7,'  , NOEUD = ',1I8)
      !----------------------------------------------------------------!
510   FORMAT(' MAXIMAL CONCENTRATION    : ',G16.7,' %, NODE = ',1I8)
511   FORMAT(' MINIMAL CONCENTRATION    : ',G16.7,' %, NODE = ',1I8)
512   FORMAT(' MAXIMAL EVOLUTION        : ',G16.7,'  , NODE = ',1I8)
513   FORMAT(' MINIMAL EVOLUTION        : ',G16.7,'  , NODE = ',1I8)
517   FORMAT(' MAX. CFL FOR SUSPENSION  : ',G16.7,'  , NODE = ',1I8)
      !----------------------------------------------------------------!
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE SUSPENSION_LISTING
