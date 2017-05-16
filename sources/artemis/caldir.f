!                    *****************
                     SUBROUTINE CALDIR
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P3                                   30/06/2013
!***********************************************************************
!
!brief    COMPUTES THE WAVE INCIDENCE
!
!history  C PEYRARD (LNHE)
!+
!+
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION BID
      DOUBLE PRECISION A1, A2 ,ALPHA0, D1, D2, PHI, PHI1, PHI2 ,MODPHI
      DOUBLE PRECISION TETA01, XU1 ,XU2, XV1, XV2, WT0
!
      INTRINSIC SQRT, ATAN2, MOD, ABS, COS, SIN, ATAN
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION, PARAMETER :: ZERO = 1.D-10
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
      PI = 4.D0 * ATAN( 1.D0 )
!      PARAMETER (PI = 3.1415926535897932384626433D0)
!      PARAMETER (RADDEG = 57.29577951D0)
!< SEB @ HRW
!
!=======================================================================
! SPEEDS AT THE SURFACE (AT T=0 AND T=OMEGA/4)
!=======================================================================
!
! COMPUTES THE GRADIENTS (PHIR AND PHII)
!
!
      CALL VECTOR(U0 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHIR , SBID, SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
!
      CALL VECTOR(V0 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHIR , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
!
!     THE OLD VARIABLE U1 IS STORED IN T3
!     BECAUSE IT IS USED TO COMPUTE INCI
!
      CALL VECTOR(T3 , '=' , 'GRADF          X' , IELM ,
     &            1.D0 , PHII , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
!
!     THE OLD VARIABLE V1 IS STORED IN T4
!     BECAUSE IT IS USED TO COMPUTE INCI
!
      CALL VECTOR(T4 , '=' , 'GRADF          Y' , IELM ,
     &            1.D0 , PHII , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
!
      CALL VECTOR(T1 , '=' , 'MASBAS          ' , IELM ,
     &            1.D0 , SBID , SBID , SBID , SBID , SBID , SBID ,
     &            MESH , MSK , MASKEL )
!
      CALL OS( 'X=Y/Z   ' , U0    , U0    , T1 , BID )
      CALL OS( 'X=Y/Z   ' , V0    , V0    , T1 , BID )
      CALL OS( 'X=Y/Z   ' , T3    , T3    , T1 , BID )
      CALL OS( 'X=Y/Z   ' , T4    , T4    , T1 , BID )
!
!=======================================================================
! COMPUTES WAVE INCIDENCE
!=======================================================================
!
!        U0 (D(PHIR)/DX) : A      U1 (D(PHII)/DX): B
!        V0 (D(PHIR)/DY) : C      V1 (D(PHII)/DY): D
! FROM U= A COS WT + B SIN WT  TO : U = A1 COS ( WT - PHI1)
!      V= C COS WT + D SIN WT       V = A2 COS ( WT - PHI2)
!
      DO I=1,NPOIN
        A1 = SQRT ( U0%R(I)*U0%R(I) + T3%R(I)*T3%R(I) )
        PHI1 = ATAN2( T3%R(I),U0%R(I) )
        A2 = SQRT ( V0%R(I)*V0%R(I) + T4%R(I)*T4%R(I) )
        PHI2 = ATAN2( T4%R(I),V0%R(I) )
!
! WRITTEN AS : U = A1 COS ( (WT - PHI1))
!              V = A2 COS ( (WT - PHI1) - PHI )
! WHERE PHI IS BETWEEN 0 AND 2*PI
!
        PHI = PHI2 - PHI1
        IF (PHI.LT.0.D0)   PHI = PHI+2.D0*PI
!
! ESTIMATES THE DIRECTION AND (WT0) WHEN THE ELLIPSE'S MAJOR AXIS
! IS REACHED.
! TREATS INDIVIDUAL CASES (LINEAR POLARISATION)
!
        MODPHI = MOD( PHI, PI )
        IF ( (MODPHI.LT.ZERO).OR.((PI-MODPHI).LT.ZERO) ) THEN
          WT0 = PHI1
          IF ( (PHI.LT.2D0*ZERO).OR.((2.D0*PI-PHI).LT.2D0*ZERO) )THEN
            ALPHA0 = ATAN2( A2,A1 )
          ELSE
!                  (ABS(PHI-PI).LT.2D0*ZERO)
            ALPHA0 = 2.D0*PI - ATAN2( A2,A1 )
          ENDIF
        ELSE
! GENERAL CASE: ELLIPTIC POLARISATION
!        TAN(2*(WT0 - PHI1)) = A2**2*SIN(2*PHI)/(A1**2+A2**2*COS(2*PHI))
          TETA01 = ATAN2( (A2*A2*SIN(2*PHI)) ,
     &                    (A1*A1 + A2*A2*COS(2*PHI)) ) / 2.D0
          XU1 = A1 * COS ( TETA01)
          XV1 = A2 * COS ( TETA01 - PHI )
          XU2 = -A1 * SIN ( TETA01)
          XV2 = -A2 * SIN ( TETA01 - PHI )
          D1 = XU1*XU1 + XV1*XV1
          D2 = XU2*XU2 + XV2*XV2
          IF (D2.GT.D1) THEN
             TETA01 = TETA01 + PI/2.D0
             XU1    = XU2
             XV1    = XV2
          ENDIF
          WT0    = TETA01 + PHI1
          ALPHA0 = ATAN2( XV1,XU1 )
        ENDIF
        INCI%R(I)  = ALPHA0
        T2%R(I) = WT0
      ENDDO
!
! FREE SURFACE IN PHASE WITH ALPHA0
! INCIDENCE IS CONSIDERED POSITIVE WHEN THE FREE SURFACE IS
! POSITIVE.
!
      DO I=1,NPOIN
        A1 = -(PHII%R(I)*COS(T2%R(I))-PHIR%R(I)*SIN(T2%R(I)))
        IF (A1.LT.0.D0) THEN
          IF (INCI%R(I).GE.0.D0) THEN
            INCI%R(I) = INCI%R(I) - PI
          ELSE
            INCI%R(I) = INCI%R(I) + PI
          ENDIF
        ENDIF
      ENDDO


!=======================================================================
!
      RETURN
      END
