!                    *****************
                     SUBROUTINE ART_CORFON
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE :
!+
!+      DO I = 1,NPOIN
!+        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!+        IF (Y(I).GE.700.D0) THEN
!+           ZF%R(I) = -15.D0
!+        ENDIF
!+      ENDDO
!
!history  J-M HERVOUET
!+        01/03/1990
!+        V5P1
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION R0,R1,R2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
!!      IF(LISFON.GT.0) THEN
!
!!        MAS=.TRUE.
!!        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
!!     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
!!      ENDIF
!
!-----------------------------------------------------------------------
!
! RAYON DU CYLINDRE
!      R0 = 1.D50
!      DO I=1,NPOIN
!         IF ( R0.GT.SQRT( X(I)**2 + Y(I)**2 ) ) THEN
!            R0 = SQRT( X(I)**2 + Y(I)**2 )
!         ENDIF
!       ENDDO
!
!     RAYON DE L'ETENDUE DU FOND PARABOLIQUE
      R0 = 39850.D0
!
      R1 = 3.D0*R0
!
      DO I = 1,NPOIN
        ZF%R(I) = 0.D0
!
        R2 = X(I)**2 + Y(I)**2
        IF (R2.LE.R1**2) ZF%R(I) = 4000.D0*(1.D0-R2/R1**2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END


