!                    *************************
                     SUBROUTINE INIT_COMPO_COH
!                    *************************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE:
!+      NCOUCHES(J) = 10
!+      ES(J,1) = 1.D0
!+      ES(J,2) = 1.D0
!+      ES(J,3) = 1.D0
!+      ES(J,4) = 1.D0
!+      ES(J,5) = 1.D0
!+      ES(J,6) = 1.D0
!+      ES(J,7) = 1.D0
!+      ES(J,8) = 1.D0
!+      ES(J,9) = 1.D0
!+        DO I = 1, NSICLA
!+          DO K = 1, NCOUCHES(J)
!+          AVAIL(J,K,I) = AVA0(I)
!+          ENDDO
!+        ENDDO
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
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
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!    
      INTEGER, INTENT (INOUT):: NCOUCHES(*)
      DOUBLE PRECISION EPAI_VASE(NLAYMAX),EPAI_SABLE(NLAYMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J
!
!-----------------------------------------------------------------------
!
!     EXAMPLE FOR NOMBLAY = 10
!
!     EPAI_VASE(1)=0.0525D0
!     EPAI_VASE(2)=0.0385D0
!     EPAI_VASE(3)=0.03995D0
!     EPAI_VASE(4)=0.0437D0
!     EPAI_VASE(5)=0.0517D0
!     EPAI_VASE(6)=0.1259D0
!     EPAI_VASE(7)=0.4889D0
!     EPAI_VASE(8)=1.5071D0
!     EPAI_VASE(9)=0.86410D0
!     EPAI_VASE(9)=0.80D0
!
!     HERE A CONSTANT
!
      DO J= 1,NOMBLAY
        EPAI_VASE(J) = 0.1D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISING THE NUMBER OF LAYERS
!        
      DO I=1,NPOIN
        NCOUCHES(I) = NOMBLAY
      ENDDO
!
!     BY DEFAULT : UNIFORM BED COMPOSITION (KEY WORDS)
!
      DO J= 1,NOMBLAY
        DO I=1,NPOIN
          CONC(I,J)=CONC_VASE(J)
          ES(I,J)  =EPAI_VASE(J)
        ENDDO
      ENDDO
!
      IF(NSICLA.GT.1) THEN
        DO J=1,NOMBLAY
          EPAI_SABLE(J) = AVA0(1)/AVA0(2)*EPAI_VASE(J)
        ENDDO
        DO J=1,NOMBLAY
          DO I=1,NPOIN
            ES(I,J) = ES(I,J) + EPAI_SABLE(J)
          ENDDO 
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
