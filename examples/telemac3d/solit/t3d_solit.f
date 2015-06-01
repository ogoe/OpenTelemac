!                       *****************
                        SUBROUTINE CONDIM
!                       *****************
!
!
!***********************************************************************
! TELEMAC-3D   V5.1          25/11/97      J-M HERVOUET(LNH) 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I, IPOIN2, IPOIN3
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926535897932384626433D0)
!     FOR SOLITARY WAVE
      DOUBLE PRECISION WX, WH, HV1, HV2, HV3, HV4, X0
!
!***********************************************************************
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU, POUR UNE COTE NULLE.
!
      CALL OS( 'X=C     ' , H   , H , H , 0.D0)
      CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0, NPOIN2)
!
! *** SOLITARY WAVE ***
!
! WH WAVE HEIGHT
! WX WQUILIBRIUM WATER DEPTH
! X0 CREST INITIAL POSITION
!
!
!
!     WH=2.0D0
      WH=1.D0
      WX=10.0D0
!     X0=80.0D0
      X0=150.0D0
!
      DO IPOIN2=1,NPOIN2
        HV1=SQRT(3.0D0/4.0D0*WH/WX**3.0D0)*(X(IPOIN2)-X0)
        HV2=2.0D0/(EXP(HV1)+EXP(-HV1))
        H%R(IPOIN2)=H%R(IPOIN2) + WH*HV2**2
      END DO
!
      CALL OS ('X=Y     ', HN, H, H, 0.D0)
!
!-----------------------------------------------------------------------
!
!     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
!     TRANSF IS KEYWORD "MESH TRANSFORMATION"
!     IF TRANSF = 0, SUBROUTINE CALCOT MUST BE IMPLEMENTED BY THE USER
!
!     AN EQUIVALENT OF TRANSF MUST BE GIVEN FOR EVERY PLANE:
!
!     POSSIBLE VALUES OF TRANSF_PLANE :
!
!     1 : SIGMA TRANSFORMATION WITH EVENLY SPACED PLANES
!     2 : SIGMA TRANSFORMATION WITH PROPORTIONS GIVEN IN ZSTAR
!     3 : PRESCRIBED ELEVATION GIVEN IN ZPLANE
!
!     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
!
!     OTHER EXAMPLES:
!
!     EXAMPLE 1: ALL PLANES WITH PRESCRIBED ELEVATION
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=3
!     ENDDO
!     ZPLANE%R(2)=-7.D0
!     ZPLANE%R(3)=-4.D0
!     ...
!     ZPLANE%R(NPLAN-1)=-0.05D0
!
!
!     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     ZSTAR%R(2)=0.02D0
!     ZSTAR%R(3)=0.1D0
!     ...
!     ZSTAR%R(NPLAN-1)=0.95D0
!
!
!     EXAMPLE 3: ONE PLANE (NUMBER 4) WITH PRESCRIBED ELEVATION
!                AND SIGMA ELSEWHERE
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=1
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=-3.D0
!
!
!     EXAMPLE 4: ONE PLANE WITH PRESCRIBED ELEVATION
!                AND 2 SIGMA TRANSFORMATIONS, WITH NPLAN=7
!                SIGMA TRANSFORMATIONS ARE MEANT BETWEEN
!                BOTTOM, FIXED ELEVATION PLANES AND FREE SURFACE
!                THE VALUES OF ZSTAR ARE LOCAL FOR EVERY
!                SIGMA TRANSFORMATION: 0. FOR LOWER FIXED PLANE
!                                      1. FOR UPPER FIXED PLANE
!
!     DO IPLAN = 1,7
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=3.D0
!     ZSTAR%R(2)=0.2D0
!     ZSTAR%R(3)=0.8D0
!     ZSTAR%R(5)=0.1D0
!     ZSTAR%R(6)=0.9D0
!
!
!***********************************************************************
!
! ON NE DISPOSE PAS AU DEBUT DE CE SOUS-PROG. DE Z EN TOUS LES POINTS.
! (CAR POUR CONNAITRE Z, IL FAUT CONNAITRE ZSTAR ET H).
! NEANMOINS, ON PEUT, A CETTE ETAPE DE LA ROUTINE, CALCULER Z.
! CELA PEUT SERVIR PAR EXEMPLE POUR INITIALISER VITESSES ET TRACEURS.
!
      CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION DES VITESSES
!
      CALL OS( 'X=C     ' , U , U , U , 0.0D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.0D0 )
      CALL OS( 'X=C     ' , W , W , W , 0.0D0 )
!
! SOLITARY WAVE INITIAL VELOCITY - ANALYTICAL SOLUTION
!
      DO IPLAN=1,NPLAN
        DO IPOIN2=1,NPOIN2
!
          IPOIN3 = (IPLAN-1)*NPOIN2 + IPOIN2
!
          HV1=SQRT(3.D0/4.D0*WH/WX**3)*(X(IPOIN3)-X0)
          HV2=2.D0/(EXP(HV1)+EXP(-HV1))
          U%R(IPOIN3)=SQRT(GRAV*WX)*WH/WX*HV2**2

          HV3=(EXP(HV1)-EXP(-HV1))/(EXP(HV1)+EXP(-HV1))
          HV4=SQRT(3.D0*GRAV*WX)
     &       *((SQRT(WH/WX))**3)*(Z(IPOIN3)+WX)/WX
          W%R(IPOIN3)=(HV4*HV2**2)*HV3
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DES TRACEURS
!
      IF (NTRAC.NE.0) THEN
        CALL OS( 'X=C     ', TA, TA, TA, 0.D0)
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
!     IF(ITURBV.EQ.3) THEN
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
! (TO BE DELETED WHEN NO PROJECTION TWO...)
! INITIALIZE THE HYDRODYNAMIC PRESSURE FIELD TO 0.0
! (IT MAY BE APPROPRIATE TO SOLVE A POISSON EQUATION FOR IT)
!
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP ,C=0.D0)
        CALL OS('X=C     ',X=DPN,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
