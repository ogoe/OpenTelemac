!                    *********************
                     SUBROUTINE BED_FLUXES
!                    *********************
!
!
!***********************************************************************
! TELEMAC3D   V7P1                                   27/04/2015
!***********************************************************************
!
!brief    BUILDS THE SOURCE TERMS CALCULATED FROM THE BED FLUXES
!+                TO ADD IN THE 3D CONTINUITY EQUATIONS.
!
!history  A JOLY (LNHE)
!+        27/04/2015
!+        V7P1
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I,IP
      INTEGER J,N1,N2,N3
      TYPE(BIEF_OBJ) :: MASK_BC
      DOUBLE PRECISION :: W_SUM,VTEMP
      DOUBLE PRECISION :: XNB,YNB,ZNB

!
!-----------------------------------------------------------------------
!
!
!     SETTING BEDFLU TO 0 AND DEFINING TEMP WORK ARRAYS
!
      CALL OS('X=0     ',X=BEDFLU)
      CALL CPSTVC(BEDFLU,T2_01)
      CALL OS('X=C     ',X=T2_01,C=0.D0)
      CALL CPSTVC(BEDFLU,T2_02)
      CALL OS('X=C     ',X=T2_02,C=1.D0)
      CALL CPSTVC(BEDFLU,T2_03) ! WILL REPLACE SVIDE
      DO I = 1,NPOIN2
        IF(LIWBOF%I(I).EQ.KENT.OR.
     &     LIWBOF%I(I).EQ.KENTU) THEN
          XNB=GRADZF%ADR(1)%P%R(I)
          YNB=GRADZF%ADR(2)%P%R(I)
          ZNB=-SQRT(1-XNB**2-YNB**2)
          T2_01%R(I) = -UBORF%R(I)*XNB-VBORF%R(I)*YNB-WBORF%R(I)*ZNB
        ENDIF
      ENDDO
!
      CALL VECTOR(BEDFLU,'=','FLUBOR          ',
     &            IELM2H,1.D0,T2_01,SVIDE,SVIDE,SVIDE,
     &            SVIDE,SVIDE,MESH2D,.FALSE.,T2_02)
!
      CALL OS('X=X+Y   ',X=SMH,Y=BEDFLU)
!
!-----------------------------------------------------------------------
!
      RETURN
      END