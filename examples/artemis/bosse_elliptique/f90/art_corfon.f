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
! JCB :
      DOUBLE PRECISION COSA,SINA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! JCB :
      COSA=0.939692621D0
      SINA=0.342020143D0
!
! Elliptical obstacle
      DO I = 1,NPOIN
        T1%R(I)=(X(I)-15.75D0)*COSA-(Y(I)-18.5D0)*SINA
        T2%R(I)=(X(I)-15.75D0)*SINA+(Y(I)-18.5D0)*COSA
        IF(T2%R(I) .GT. 5.2D0) THEN
          ZF%R(I)=-0.45D0
        ELSEIF(((T1%R(I)/4.D0)**2)+((T2%R(I)/3.D0)**2) .LE. 1.D0)
     &  THEN
          ZF%R(I)=-0.45D0-0.02D0*(-5.2D0+T2%R(I))+0.5D0*
     &       SQRT(1.D0-((T1%R(I)/5.D0)**2)-((T2%R(I)/3.75D0)
     &          **2))-0.3D0
        ELSE
          ZF%R(I)=-0.45D0-0.02D0*(-5.2D0+T2%R(I))
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END
