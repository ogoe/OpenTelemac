!                   **********************
                     SUBROUTINE REAER
!                    **********************
!
     &(FORMK2,K2,K22,NPOIN,UN,VN,H,EPS)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENT OF REAERATION K2
!
!                              
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| EPS            |-->| TO AVOID DIVISION BY 0
!| FORMK2         |-->| WHICH FROMULA TO COMPUTE K2
!| H              |-->| WATER DEPTH
!| K2             |<--| COEFFICIENT OF REAERATION
!| K22            |-->| COEFFICIENT OF REAERATION IF IT IS CONSTANT
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| UN,VN          |-->| VELOCITY COMPONENTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_WAQTEL, EX_REAER => REAER
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)     :: FORMK2,NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: EPS,K22
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: H,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: K2
      INTRINSIC ABS,SQRT,MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER           I
      DOUBLE PRECISION  UNORM,PJ
!     
!
      IF(FORMK2.EQ.0)THEN ! ==> CONSTANT K2
        CALL OS('X=C     ',K2,K2,K2,K22)
      ELSE  ! ==> VARIABLE K2
        DO I =1,NPOIN
!
          UNORM = SQRT(UN%R(I)**2+VN%R(I)**2) !  GENERALIZATION OF U IN 1D
!         HEAD OR WATER DEPTH TO REPLACE RH ?
          PJ = H%R(I)  !ZF%R(I)+H%R(I)+UNORM**2/DEUXG
!         FORMULA OF THE TENESSEE VALLEY AUTHORITY
          IF( FORMK2.EQ.1 ) THEN
            K2%R(I) = 5.23D0*UNORM* MAX(H%R(I),EPS)**(-1.67D0)
!         FORMULA OF OWENS ET AL.
          ELSEIF(FORMK2.EQ.2)THEN
            K2%R(I) = 5.33D0*(UNORM**0.67D0)*
     &              MAX(H%R(I),EPS)**(-1.85D0)
!         FORMULA OF CHURCHILL ET AL.
          ELSEIF(FORMK2.EQ.3)THEN
            K2%R(I) = 0.746D0 * (UNORM**2.695D0) /
     &      (MAX(H%R(I),EPS)**(-3.085D0) *
     &       MAX(ABS(PJ),EPS)**0.823D0) ! VERIFY THE FORMULA, SOME DOUBT ?
!         FORMULA OF O CONNOR & DOBBINS'
          ELSEIF(FORMK2.EQ.4)THEN
            K2%R(I) = (3.90D0 * UNORM**0.5D0 ) /
     &               MAX(H%R(I),EPS)**(1.5D0)
!         FORMULA OF ?? INVISIBLE MAN :) : IT SEEMS TO BE A COMBINATION OF THE 3 LAST FORMULA ?!
          ELSEIF( FORMK2.EQ.5 ) THEN
            IF( H%R(I).LE.0.6D0 ) THEN
              K2%R(I) = 5.33D0 * (UNORM**0.67D0) *
     &                MAX(H%R(I),EPS)**(-1.85D0)
            ELSEIF (H%R(I).LT.(12.D0*UNORM-6.6D0)) THEN
              K2%R(I) =  0.746D0*(UNORM**2.695D0)/
     &                (MAX(H%R(I), EPS)**(-3.085D0) *
     &                 MAX(ABS(PJ),EPS)**(0.823D0))
            ELSE
              K2%R(I) = 3.90D0 * (UNORM**0.5D0)/
     &                MAX(H%R(I),EPS)**1.5D0
            ENDIF
          ELSE
            IF(LNG.EQ.1)THEN
              WRITE(LU,110)FORMK2
            ELSE
              WRITE(LU,111)FORMK2
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!
!     ERROR MESSAGES
!
!
110   FORMAT(1X,'FORMULE DE K2:',I3,/,1X, 'NON PROGRAMMEE')
111   FORMAT(1X,'K2 FORMULA :',I3,/,1X, 'NOT AVAILABLE')
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!
