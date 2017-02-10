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
      DOUBLE PRECISION D1,D3,L1,HCP,XRCP,XDEBUT
      DOUBLE PRECISION PI
      PARAMETER( PI = 3.1415926535897932384626433D0)
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
! TOPOGRAPHY DATA (m)

      D1 = 6.
      D3 = 2.
! value of variable b
      L1 = 4.
      HCP=0.
      XRCP=0.

      XDEBUT=35.-L1

! TANH VARIATION
      DO I = 1,NPOIN
        XRCP=X(I)-XDEBUT
        HCP =(D1+D3)/2.-( (D1-D3)*TANH(3.*PI*((XRCP/L1)-0.5)) )/2.
        ZF%R(I) = D1-HCP
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
