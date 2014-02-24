!                    *****************
                     SUBROUTINE WCHIND
!                    *****************
!
     & (WC, C, CINI, CGEL, NPOIN3, HIND_TYPE)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS HINDERED CONCENTRATION DEPENDENT SETTLING VELOCITY IN 3D
!+        ACCORDING TO EITHER 1. WHITEHOUSE ET AL. (2000):
!+
!+        WC = WC* (1 - C/CGEL)^5
!+
!+        OR 2. WINTERWERP (1999):
!+          
!+        WC = 
!+ 
!+
!history  D M KELLY (HRW)
!+        13/01/2012
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HN             |-->| WATER DEPTH
!| NPOIN3         |-->| TOTAL NUMBER OF POINTS IN 3D MESH
!| WC             |<->| SEDIMENT SETTLING VELOCITY 
!| C              |-->| CONETRATION OF SED AT NODES (actually TRAV1)
!| CINI           |-->| THRESHOLD CONCENTATION FOR HINDERING TO START
!| CGEL           |<--| SEDIMENT CONCENTRATION AT WHICH SEDIMENT FORMS A WEAK SOIL (KG/M3)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WC,C
!      DOUBLE PRECISION, INTENT(IN)  :: C(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: CINI, CGEL
      INTEGER, INTENT(IN)           :: HIND_TYPE, NPOIN3
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL:
      INTEGER          :: I
      DOUBLE PRECISION :: QRWC, CORR
      DOUBLE PRECISION :: PHI,PHI_P,PHI_STAR
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN3
!         
! ONLY APPLY HINDERING IF ABOVE THRESHOLD
         IF (C%R(I).GT.CINI) THEN
!             
            IF (HIND_TYPE==1) THEN
!        WHITEHOUSE ET AL. (2000) HINDERED SETTLING (HIND_TYPE=1)
!        MODIFIED BY TBE TO ACCOUNT FOR TURBULENT SETTLING VELOCITY MODEL (BECAUSE WCINI VARIES)   
         PHI = (C%R(I)-CINI) / CGEL
         QRWC = 1.D0-PHI
!
! CV: WC now positive
!
         WC%R(I) = (max(WC%R(I)*QRWC*QRWC*QRWC*QRWC*QRWC,0.D0)) ! OPTIMISED (FASTER EXECUTION THAN QRWC**5)
!
         ELSE IF (HIND_TYPE==2) THEN
!
            PRINT *, 'WINTERWERP HINDRED SETTLING DOES NOT WORK YET'
!        WINTERWERP (1999) HINDERED SETTLING (HIND_TYPE=2)
         PHI      = C%R(I)/CGEL
         PHI_P    = C%R(I)/2650.D0
         PHI_STAR = MIN(1.D0,PHI)
! CV 0< CORRECTION <1
         CORR=(1.D0-PHI_STAR)*(1.D0-PHI_P)/(1.D0+2.5D0*PHI)
         CORR= MIN(MAX(CORR, 0.D0),1.D0)
!         
         WC%R(I)  = WC%R(I)*CORR
!
         ELSE
           PRINT *, 'HINDERED SETTLING MUST BE TYPE 1 OR 2'; STOP 
         END IF
!
         ENDIF
!
      END DO
!
      RETURN
!
      END 
     