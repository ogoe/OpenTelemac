!                    *********************
                     SUBROUTINE FLUX_EF_VF
!                    *********************
!
     &(FLOW,PHIEL,NSEG,NELEM,ELTSEG,ORISEG,IKLE,INIFLO,IOPT,FN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE FLUXES FOR THE FINITE VOLUME SCHEME.
!+
!+           (LEO POSTMA'S METHOD, SAME AS IN DELWAQ).
!
!history  JMH
!+        06/05/2009
!+
!+   Optimisation
!
!history  JMH
!+        01/10/2009
!+
!+   Option -1 added, Argument FN added, PSI SCHEME ADDED
!
!history  LEO POSTMA (DELTARES)
!+        27/10/2009
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
!| ELTSEG         |-->| SEGMENTS OF EVERY TRIANGLE
!| FLOW           |<--| FLUXES PER SEGMENTS
!| FN             |-->| OPTIONAL ARGUMENT FOR PSI SCHEME
!| IKLE           |-->| CONNECTIVITY TABLE
!| INIFLO         |-->| IF(YES) FLOW WILL BE INITIALISED AT 0
!| IOPT           |-->| OPTION FOR THE CONSTANT PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| ORISEG         |-->| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE
!| PHIEL          |-->| PER ELEMENT, FLUXES LEAVING POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF => FLUX_EF_VF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)                  :: NSEG,IOPT,NELEM
      INTEGER, INTENT(IN)                  :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)                  :: ORISEG(NELEM,3)
      INTEGER, INTENT(IN)                  :: IKLE(NELEM,3)
      DOUBLE PRECISION, INTENT(INOUT)      :: FLOW(NSEG)
      DOUBLE PRECISION, INTENT(IN)         :: PHIEL(NELEM,3)
      LOGICAL, INTENT(IN)                  :: INIFLO
      TYPE(BIEF_OBJ), INTENT(IN), OPTIONAL :: FN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,ISEG
      DOUBLE PRECISION A1,A2,A3,F1,F2,F3,THIRD,CSTE,F12,F23,F31
      DOUBLE PRECISION FN1,FN2,FN3,F21,F32,F13
      DOUBLE PRECISION BETA1FI,BETA2FI,BETA3FI,FI
!
      INTRINSIC ABS,MIN,MAX
!
      THIRD=1.D0/3.D0
!
!-----------------------------------------------------------------------
!
!     INITIALISES FLOW TO 0.D0
!
      IF(INIFLO) THEN
        DO ISEG = 1,NSEG
          FLOW(ISEG) = 0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.-1) THEN
!
!-----------------------------------------------------------------------
!
!     FLUXES ALREADY COMPUTED BEFORE CALLING THIS SUBROUTINE
!     THEY ARE JUST ASSEMBLED HERE
!
      DO IELEM = 1,NELEM
!       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,1)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,1)
        ENDIF
!       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,2)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,2)
        ENDIF
!       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + PHIEL(IELEM,3)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - PHIEL(IELEM,3)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.0) THEN
!
!-----------------------------------------------------------------------
!
!     WITH NO CONSTANT
!
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
!       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F1-F2)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F1-F2)
        ENDIF
!       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F2-F3)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F2-F3)
        ENDIF
!       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F3-F1)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F3-F1)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.1) THEN
!
!-----------------------------------------------------------------------
!
!     MINIMISES MAX ( ABS(FLOW) )
!
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
        CSTE=-0.5D0*(MIN(F1-F2,F2-F3,F3-F1)+MAX(F1-F2,F2-F3,F3-F1))
!       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F1-F2+CSTE)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F1-F2+CSTE)
        ENDIF
!       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F2-F3+CSTE)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F2-F3+CSTE)
        ENDIF
!       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + THIRD*(F3-F1+CSTE)
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - THIRD*(F3-F1+CSTE)
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.2) THEN
!
!-----------------------------------------------------------------------
!
!     LEO POSTMA'S METHOD (EQUIVALENT TO FLUXES GIVEN BY N-SCHEME)
!
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
        A1 = ABS(F1)
        A2 = ABS(F2)
        A3 = ABS(F3)
        IF(A1.GE.A2.AND.A1.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 1
          ISEG  = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) - F2
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F2
          ENDIF
          ISEG = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.2) THEN
            FLOW(ISEG) = FLOW(ISEG) - F3
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F3
          ENDIF
        ELSEIF(A2.GE.A1.AND.A2.GE.A3) THEN
!         ALL FLOW TO AND FROM NODE 2
          ISEG = ELTSEG(IELEM,1)
          IF(ORISEG(IELEM,1).EQ.2) THEN
            FLOW(ISEG) = FLOW(ISEG) - F1
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F1
          ENDIF
          ISEG = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) - F3
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F3
          ENDIF
        ELSE
!         ALL FLOW TO AND FROM NODE 3
          ISEG = ELTSEG(IELEM,2)
          IF(ORISEG(IELEM,2).EQ.2) THEN
            FLOW(ISEG) = FLOW(ISEG) - F2
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F2
          ENDIF
          ISEG = ELTSEG(IELEM,3)
          IF(ORISEG(IELEM,3).EQ.1) THEN
            FLOW(ISEG) = FLOW(ISEG) - F1
          ELSE
            FLOW(ISEG) = FLOW(ISEG) + F1
          ENDIF
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSEIF(IOPT.EQ.3.AND.PRESENT(FN)) THEN
!
!-----------------------------------------------------------------------
!
!     PSI-SCHEME
!
      DO IELEM = 1,NELEM
        F1 = PHIEL(IELEM,1)
        F2 = PHIEL(IELEM,2)
        F3 = PHIEL(IELEM,3)
!
        FN1=FN%R(IKLE(IELEM,1))
        FN2=FN%R(IKLE(IELEM,2))
        FN3=FN%R(IKLE(IELEM,3))
!
!       STARTS WITH N-SCHEME (EQUIVALENT TO LEO POSTMA'S IMPLEMENTATION)
!
        F12=MAX(MIN(F1,-F2),0.D0)
        F23=MAX(MIN(F2,-F3),0.D0)
        F31=MAX(MIN(F3,-F1),0.D0)
        F21=MAX(MIN(F2,-F1),0.D0)
        F32=MAX(MIN(F3,-F2),0.D0)
        F13=MAX(MIN(F1,-F3),0.D0)
!
        BETA1FI=F12*(FN1-FN2)+F13*(FN1-FN3)
        BETA2FI=F21*(FN2-FN1)+F23*(FN2-FN3)
        BETA3FI=F31*(FN3-FN1)+F32*(FN3-FN2)
!
        FI=BETA1FI+BETA2FI+BETA3FI
!
!       NOW PSI-SCHEME
!
!       WHAT FOLLOWS IS INSPIRED FROM SUBROUTINE VC08AA
!       WHERE FIJ IS LIJ
!
        IF(FI.GT.0.D0) THEN
          IF(BETA1FI.GT.FI) THEN
            F12=F12*FI/BETA1FI
            F13=F13*FI/BETA1FI
          ELSEIF(BETA1FI.LT.0.D0) THEN
            F12=0.D0
            F13=0.D0
          ENDIF
          IF(BETA2FI.GT.FI) THEN
            F21=F21*FI/BETA2FI
            F23=F23*FI/BETA2FI
          ELSEIF(BETA2FI.LT.0.D0) THEN
            F21=0.D0
            F23=0.D0
          ENDIF
          IF(BETA3FI.GT.FI) THEN
            F31=F31*FI/BETA3FI
            F32=F32*FI/BETA3FI
          ELSEIF(BETA3FI.LT.0.D0) THEN
            F31=0.D0
            F32=0.D0
          ENDIF
        ELSEIF(FI.LT.0.D0) THEN
          IF(BETA1FI.LT.FI) THEN
            F12=F12*FI/BETA1FI
            F13=F13*FI/BETA1FI
          ELSEIF(BETA1FI.GT.0.D0) THEN
            F12=0.D0
            F13=0.D0
          ENDIF
          IF(BETA2FI.LT.FI) THEN
            F21=F21*FI/BETA2FI
            F23=F23*FI/BETA2FI
          ELSEIF(BETA2FI.GT.0.D0) THEN
            F21=0.D0
            F23=0.D0
          ENDIF
          IF(BETA3FI.LT.FI) THEN
            F31=F31*FI/BETA3FI
            F32=F32*FI/BETA3FI
          ELSEIF(BETA3FI.GT.0.D0) THEN
            F31=0.D0
            F32=0.D0
          ENDIF
        ELSE
          F12=0.D0
          F23=0.D0
          F31=0.D0
          F21=0.D0
          F32=0.D0
          F13=0.D0
        ENDIF
!
!       ASSEMBLES
!
!       SEGMENT 1
        ISEG  = ELTSEG(IELEM,1)
        IF(ORISEG(IELEM,1).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + F12 - F21
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - F12 + F21
        ENDIF
!       SEGMENT 2
        ISEG  = ELTSEG(IELEM,2)
        IF(ORISEG(IELEM,2).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + F23 - F32
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - F23 + F32
        ENDIF
!       SEGMENT 3
        ISEG  = ELTSEG(IELEM,3)
        IF(ORISEG(IELEM,3).EQ.1) THEN
          FLOW(ISEG) = FLOW(ISEG) + F31 - F13
        ELSE
          FLOW(ISEG) = FLOW(ISEG) - F31 + F13
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
       IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FLUX_EF_VF :'
          WRITE(LU,*) 'OPTION INCONNUE : ',IOPT
          IF(IOPT.EQ.3.AND..NOT.PRESENT(FN)) THEN
            WRITE(LU,*) 'OPTION 3 : FONCTION CONVECTEE REQUISE'
          ENDIF
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FLUX_EF_VF:'
          WRITE(LU,*) 'UNKNOWN OPTION: ',IOPT
          IF(IOPT.EQ.3.AND..NOT.PRESENT(FN)) THEN
            WRITE(LU,*) 'OPTION 3: ADVECTED FUNCTION REQUIRED'
          ENDIF
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
