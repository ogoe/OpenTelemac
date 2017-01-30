!                    *********************
                     SUBROUTINE INIT_COMPO
!                    *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
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
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I , J
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       BY DEFAULT : UNIFORM BED COMPOSITION
!
        NCOUCHES(J) = 1
        DO I = 1, NSICLA
          AVAIL(J,1,I) = AVA0(I)
        ENDDO

        AVAIL(J,1,1)=       0.2D0*MESH%X%R(J) /16.D0
        AVAIL(J,1,2)=       0.6D0*MESH%X%R(J) /16.D0
        AVAIL(J,1,3)=(16.D0-0.8D0*MESH%X%R(J))/16.D0

!
!  TO BE FILLED BY THE USER
!      NCOUCHES(J) = 10
!      ES(J,1) = 1.D0
!      ES(J,2) = 1.D0
!      ES(J,3) = 1.D0
!      ES(J,4) = 1.D0
!      ES(J,5) = 1.D0
!      ES(J,6) = 1.D0
!      ES(J,7) = 1.D0
!      ES(J,8) = 1.D0
!      ES(J,9) = 1.D0
!        DO I = 1, NSICLA
!          DO K = 1, NCOUCHES(J)
!          AVAIL(J,K,I) = AVA0(I)
!          ENDDO
!        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

