!                    *****************
                     SUBROUTINE TRISOU
!                    *****************
!
     & (CV1, CV2, SCV1, SCV2, UN3, VN3, TA, X, Y, Z, ZS,
     &  DELTAR,MESH3,FCOR,CORIOL,NTRAC,LT,AT,DT,SURFAC,
     &  T1,ST1, W1, W2, W3, SEDI, GRAV, NPOIN3, NELEM3, NPOIN2,
     &  NELEM2, NPLAN, NETAGE, IKLE3, PRIVE, LV, MSK, MASKEL, INCHYD,
     &  SVOLU,VOLU,SVIDE,IELM3,SMASKEL,NREJEU,ISCE,KSCE,QSCE,USCE,VSCE,
     &  IELM2H,GRADZSX,GRADZSY,Z3,TRAV2,FU2,MESH2D, ST2,T2,ST3,T3,
     &  LATIT, LONGIT, NORD,SMU,SMV,YASEM3D,SCHCVI,DENLAW,FXH,FYH,
     &  COUROU,NPTH,T3D_FILES,T3DBI1)
!
!***********************************************************************
! TELEMAC3D   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    SOURCE TERMS FOR U & V MOMENTUM EQUATIONS.
!
!history  CGD/SOGREAH
!+
!+
!+   CORIOLIS FORCE ADDED
!
!history  AG (LNHE)
!+
!+
!+   BUOYANCY TERMS COMPUTED IN PHYSICAL SPACE
!
!history  JMH
!+        19/12/2008
!+
!+   WAVE DRIVEN CURRENTS ADDED. SEE IF(COUROU)
!
!history  J-M HERVOUET (LNHE)
!+        29/06/2009
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
!| AT             |-->| TIME OF TIME STEP
!| CORIOL         |-->| LOGICAL IF CORIOLIS FORCE OR NOT
!| COUROU         |-->| LOGICAL FOR WAVE DRIVEN CURRENTS
!| CV1            |<->| SOURCE TERMS ON U
!| CV2            |<->| SOURCE TERMS ON V
!| DELTAR         |<->| (RHO-RHO0)/RHO0
!| DENLAW         |-->| CHOICE OF DENSITY LAW (SEE ABOVE)
!| DT             |-->| TIME STEP
!| FCOR           |-->| CORIOLIS COEFFICIENT
!| FU2            |<->| NOT USED
!| FXH            |<->| WAVE STRESSES FROM ARTEMIS OR TOMAWAC
!| FYH            |<->| WAVE STRESSES FROM ARTEMIS OR TOMAWAC
!| GRADZSX        |<->| FREE SURFACE GRADIENT
!| GRADZSY        |<->| FREE SURFACE GRADIENT
!| GRAV           |-->| GRAVITY ACCELERATION
!| IELM2H         |-->| TYPE OF ELEMENT
!| IELM3          |-->| CORRESPONDENCE BETWEEN LOCAL AND 3D GLOBAL
!|                |---| NUMBER
!| INCHYD         |-->| IF YES, HYDROSTATIC INCONSISTENCY FILTER
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |-->| NUMBER OF PLANE FOR SOURCES
!| LATIT          |-->| LATITUDE OF THE ORIGIN POINT
!| LONGIT         |-->| LONGITUDE OF THE ORIGIN POINT: NOT USED
!| LT             |-->| CURRENT TIME STEP NUMBER
!| LV             |-->| VECTOR LENGTH FOR VECTORISATION
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2D         |<->| 2D MESH
!| MESH3          |---| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NORD           |-->| NORTH
!| NPTH           |-->| RECORD NUMBER IN THE WAVE DRIVEN CURRENTS FILE
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NREJEU         |---| NUMBER OF VELOCITY INFORMATION FOR SOURCE
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| QSCE           |---| WATER DISCHARGE OF SOURCES
!| SCHCVI         |-->| ADVECTION SCHEME ON VELOCITY
!| SCV1           |<->| ASSOCIATED STRUCTURES
!| SCV2           |<->| ASSOCIATED STRUCTURES
!| SEDI           |-->| IF YES, THERE IS SEDIMENT
!| SMASKEL        |-->| ASSOCIATED STRUCTURES
!| SMU            |<->| RIGHT-HAND SIDE ON VELOCITIES EQUATIONS
!| SMV            |<->| RIGHT-HAND SIDE ON VELOCITIES EQUATIONS
!| ST1            |<->| ASSOCIATED STRUCTURE
!| ST2            |<->| ASSOCIATED STRUCTURE
!| ST3            |<->| ASSOCIATED STRUCTURE
!| SURFAC         |-->| AREA OF TRIANGLES
!| SVIDE          |<->| VOID VECTOR STRUCTURE
!| SVOLU          |<->| ASSOCIATED STRUCTURE: NOT USED
!| T1             |<->| WORK ARRAY BY POINTS
!| T2             |<->| WORK ARRAY
!| T3             |<->| WORK ARRAY
!| T3D_FILES      |-->| DATA STRUCTURE WITH DATA ON FILES
!| T3DBI1         |-->| BINARY DATA FILE 1
!| TA             |<->| TRACERS
!| TRAV2          |<->| WORK ARRAYS
!| UN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP N
!| USCE           |-->| VELOCITIES OF THE SOURCES ALONG X
!| VN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP N
!| VOLU           |<->| VOLUME AROUND POINTS AT TIME N+1
!| VSCE           |-->|VELOCITIES OF THE SOURCES ALONG Y
!| W1             |<->| WORK ARRY BY 3D ELEMENTS
!| W2             |<->| WORK ARRY BY 3D ELEMENTS
!| W3             |<->| WORK ARRY BY 3D ELEMENTS
!| X              |-->| 3D MESH COORDINATES
!| Y              |-->| 3D MESH COORDINATES
!| YASEM3D        |<->| IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
!|                |   | COMPUTED BEFORE CALLING DIFF3D
!| Z              |-->| 3D MESH COORDINATES
!| Z3             |---| 3D NODE COORDINATES: NOT USED
!| ZS             |<->| COTE PAR RAPPORT A LA SURFACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_TRISOU => TRISOU
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NELEM3,NPOIN2,NELEM2
      INTEGER, INTENT(IN) :: NPLAN,NETAGE,NTRAC,NPTH,T3DBI1
      INTEGER, INTENT(IN) :: LV,LT,IELM2H,NREJEU,IELM3,SCHCVI,DENLAW
!
      INTEGER, INTENT(IN) :: IKLE3(NELEM3,6)
!
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3)
      DOUBLE PRECISION, INTENT(IN)    :: LATIT,LONGIT,NORD
!
      DOUBLE PRECISION, INTENT(INOUT) :: CV1(NPOIN3),CV2(NPOIN3)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SCV1,SCV2,FXH,FYH
!
      DOUBLE PRECISION, INTENT(IN)    :: UN3(NPOIN3), VN3(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: ZS(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEM3,6), W2(NELEM3,6)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELEM3,6)
      DOUBLE PRECISION, DIMENSION(NPOIN2,NPLAN), INTENT(INOUT) :: VOLU
!
      DOUBLE PRECISION, INTENT(INOUT) :: T1(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: T2(NPOIN3), T3(NPOIN3)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ST1, ST2, ST3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: PRIVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DELTAR
      TYPE(BIEF_MESH)                 :: MESH3
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: GRADZSX
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: GRADZSY
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      TYPE(BIEF_OBJ),  INTENT(IN)     :: Z3
      TYPE (BIEF_OBJ), INTENT(IN)     :: SMASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: SVIDE
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: TRAV2, FU2
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: SVOLU,SMU,SMV
!
!                                 * = NSCE
      INTEGER, INTENT(IN) :: ISCE(*),KSCE(*)
      DOUBLE PRECISION, INTENT(IN) :: QSCE(*),USCE(*),VSCE(*)
!
      DOUBLE PRECISION, INTENT(IN) :: GRAV, DT, AT, FCOR
      LOGICAL, INTENT(IN) :: CORIOL, SEDI, MSK, INCHYD,COUROU
      LOGICAL, INTENT(INOUT) :: YASEM3D
      TYPE(BIEF_FILE), INTENT(IN) :: T3D_FILES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM3,IPLAN,IETAGE,IZ,IZM,IZS,ERR,NP,I3D
      DOUBLE PRECISION A,OMEGA,PI,COSNORD,SINNORD,CORI,BETA0Y,FF0
      LOGICAL CORIVAR
!
      INTEGER I,OPTFLO
      CHARACTER(LEN=15) FORMUL
!
      INTEGER I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION DX1,DX2,DX3,DY1,DY2,DY3
      DOUBLE PRECISION DZ1,DZ2,DZ3,DR1,DR2,DR3
      DOUBLE PRECISION SZ1,SZ2,SZ3,SR1,SR2,SR3
      DOUBLE PRECISION DZSUDX,DZSUDY,DRSUDX,DRSUDY
      DOUBLE PRECISION DZ123,C,ATH
!
!     FOR WAVE DRIVEN CURRENTS
!
      CHARACTER*16 NOMX,NOMY
      LOGICAL DEJALU,OKX,OKY
      DATA DEJALU /.FALSE./
      REAL, ALLOCATABLE :: W(:)
      SAVE W
!
!***********************************************************************
!
! INITIALISES
!
!     CALL OS( 'X=C     ' , X=SCV1 , C=0.D0 )
!     CALL OS( 'X=C     ' , X=SCV2 , C=0.D0 )
!
      SCV1%TYPR='0'
      SCV2%TYPR='0'
!
!-----------------------------------------------------------------------
!  BUOYANCY SOURCE TERMS
!-----------------------------------------------------------------------
!
      YASEM3D=.FALSE.
!
      IF(DENLAW.NE.0.AND.NTRAC.GT.0) THEN
!
      SCV1%TYPR='Q'
      SCV2%TYPR='Q'
      CALL OS( 'X=0     ' , X=SCV1 )
      CALL OS( 'X=0     ' , X=SCV2 )
!
!     VOLUME OF TEST FUNCTIONS
!
      CALL VECTOR(ST1, '=', 'MASBAS          ',IELM3, 1.D0,
     &            SVIDE, SVIDE,
     &            SVIDE, SVIDE, SVIDE, SVIDE, MESH3,.FALSE.,SMASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(ST1,2,MESH3)
!
!     1 : BUOYANCY IN REAL MESH
!     2 : BUOYANCY IN TRANSFORMED MESH
!
!     OPTFLO CHANGED FROM 2 INTO 1 BY JMH ON 03/10/2002
!     ENABLES TREATMENT WITH TETRAHEDRONS
!     WHO CAN TELL WHICH IS BEST WITH PRISMS ?
!
      OPTFLO=1
!
      IF(OPTFLO.EQ.1) THEN
!
      YASEM3D=.FALSE.
!
! - G DENSITY GRADIENTS
!     WITH TREATMENT OF HYDROSTATIC INCONSISTENCIES IF NEEDED
!
      FORMUL='GRADF          '
      IF(INCHYD) FORMUL(6:6)='2'
!
!     ===============================================
!     BETTER FILTERING OF HYDROSTATIC INCONSISTENCIES
!     ===============================================
!
!     3 OR 4 IMPLIES THAT 2 IS ALSO APPLIED
!
!     RECOMMENDED : FILTER 4
!
!     FILTER 3
!     IF(INCHYD) FORMUL(6:6)='3'
!     FILTER 4
!     IF(INCHYD) FORMUL(6:6)='4'
!
!
      CALL VECTOR(ST2, '=',FORMUL//'X',IELM3,-GRAV,DELTAR,SVIDE,
     &            SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
      CALL VECTOR(ST3, '=',FORMUL//'Y',IELM3,-GRAV,DELTAR,SVIDE,
     &            SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(ST2,2,MESH3)
        CALL PARCOM(ST3,2,MESH3)
      ENDIF
!
! NODAL VALUE
!
      CALL OVD('X=Y/Z   ',T2,T2,T1,0.D0,NPOIN3,2,0.D0,1.D-9)
      CALL OVD('X=Y/Z   ',T3,T3,T1,0.D0,NPOIN3,2,0.D0,1.D-9)
!
! SIMPSON INTEGRATION
!
      DO IPLAN = NPLAN, 2, -1
        DO I = 1, NPOIN2
          IZ  =(IPLAN-1)*NPOIN2+I
          IZM =(IPLAN-2)*NPOIN2+I
          A = 0.5D0 * (Z(IZ)-Z(IZM))
          CV1(IZM) = CV1(IZ) + (T2(IZ)+T2(IZM)) * A
          CV2(IZM) = CV2(IZ) + (T3(IZ)+T3(IZM)) * A
        ENDDO
      ENDDO
!
! TERMS WITH FREE SURFACE GRADIENT (NODAL VALUES)
!
      DO IPLAN = 1, NPLAN-1
!
        DO I = 1, NPOIN2
          IZ  = (IPLAN-1)*NPOIN2+I
          IZS = (NPLAN-1)*NPOIN2+I
          A = GRAV*(DELTAR%R(IZ)-DELTAR%R(IZS))
!
          CV1(IZ) = CV1(IZ) + A * GRADZSX%R(I)
          CV2(IZ) = CV2(IZ) + A * GRADZSY%R(I)
!
        ENDDO
!
      ENDDO
!
      ELSEIF(OPTFLO.EQ.2) THEN
!
      YASEM3D = .FALSE.
!
! TRANSFORMED MESH
!
! COMPUTES ZS: OPPOSITE OF WATER DEPTH AT CONSIDERED POINT
!
        I2 = NPOIN3 - NPOIN2 + 1
        I4 = NPOIN3
        DO IPLAN = 1,NPLAN
          I1 = NPOIN2*(IPLAN-1) + 1
          I3 = NPOIN2* IPLAN
          CALL OV( 'X=Y-Z   ', ZS(I1:I3), Z(I1:I3), Z(I2:I4), C, NPOIN2)
        END DO
!
        BYELEMENT: DO IELEM3 = 1 , NELEM3
!
           I1 = IKLE3(IELEM3,1)
           I2 = IKLE3(IELEM3,2)
           I3 = IKLE3(IELEM3,3)
           I4 = IKLE3(IELEM3,4)
           I5 = IKLE3(IELEM3,5)
           I6 = IKLE3(IELEM3,6)
!
           DX1 = X(I3) - X(I2)
           DX2 = X(I1) - X(I3)
           DX3 = X(I2) - X(I1)
           DY1 = Y(I2) - Y(I3)
           DY2 = Y(I3) - Y(I1)
           DY3 = Y(I1) - Y(I2)
!
           DR1 = DELTAR%R(I4) - DELTAR%R(I1)
           DR2 = DELTAR%R(I5) - DELTAR%R(I2)
           DR3 = DELTAR%R(I6) - DELTAR%R(I3)
           DZ1 = ZS(I4) - ZS(I1)
           DZ2 = ZS(I5) - ZS(I2)
           DZ3 = ZS(I6) - ZS(I3)
           DZ123 = DZ1 + DZ2 + DZ3
!
           SZ1 = ZS(I4) + ZS(I1)
           SZ2 = ZS(I5) + ZS(I2)
           SZ3 = ZS(I6) + ZS(I3)
           SR1 = DELTAR%R(I4) + DELTAR%R(I1)
           SR2 = DELTAR%R(I5) + DELTAR%R(I2)
           SR3 = DELTAR%R(I6) + DELTAR%R(I3)
!
           IF(MAX(ZS(I1),ZS(I2),ZS(I3)).GT.
     &        MIN(ZS(I4),ZS(I5),ZS(I6)).AND.INCHYD) THEN
              DR1 = 0.D0
              DR2 = 0.D0
              DR3 = 0.D0
              SR1 = 0.D0
              SR2 = 0.D0
              SR3 = 0.D0
           ENDIF
!
           DZSUDX = SZ1 * DY1 + SZ2 * DY2 + SZ3 * DY3
           DZSUDY = SZ1 * DX1 + SZ2 * DX2 + SZ3 * DX3
           DRSUDX = SR1 * DY1 + SR2 * DY2 + SR3 * DY3
           DRSUDY = SR1 * DX1 + SR2 * DX2 + SR3 * DX3
!
           W1(IELEM3,1) = DR1 * DZSUDX - DZ1 * DRSUDX
           W1(IELEM3,2) = DR2 * DZSUDX - DZ2 * DRSUDX
           W1(IELEM3,3) = DR3 * DZSUDX - DZ3 * DRSUDX
           W2(IELEM3,1) = DR1 * DZSUDY - DZ1 * DRSUDY
           W2(IELEM3,2) = DR2 * DZSUDY - DZ2 * DRSUDY
           W2(IELEM3,3) = DR3 * DZSUDY - DZ3 * DRSUDY
           W3(IELEM3,1) = DZ1 + DZ123
           W3(IELEM3,2) = DZ2 + DZ123
           W3(IELEM3,3) = DZ3 + DZ123
!
        END DO BYELEMENT
!
        IF (NETAGE.NE.1) THEN
!
           BYETAGE: DO IETAGE = NETAGE-1 , 1 , -1
              I2 = NELEM2*IETAGE + 1
              I1 = I2 - NELEM2
              CALL OV('X=X+Y   ' , W1(I1,1) , W1(I2,1) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W1(I1,2) , W1(I2,2) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W1(I1,3) , W1(I2,3) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W2(I1,1) , W2(I2,1) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W2(I1,2) , W2(I2,2) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W2(I1,3) , W2(I2,3) , Z , C , NELEM2)
           END DO BYETAGE
!
           I2 = NELEM2 + 1
           I1 = NELEM3 - NELEM2
           CALL OV('X=Y     ' , W1(1,4) , W1(I2,1) , Z , C , I1)
           CALL OV('X=Y     ' , W1(1,5) , W1(I2,2) , Z , C , I1)
           CALL OV('X=Y     ' , W1(1,6) , W1(I2,3) , Z , C , I1)
           CALL OV('X=Y     ' , W2(1,4) , W2(I2,1) , Z , C , I1)
           CALL OV('X=Y     ' , W2(1,5) , W2(I2,2) , Z , C , I1)
           CALL OV('X=Y     ' , W2(1,6) , W2(I2,3) , Z , C , I1)
!
        ENDIF
!
        I1 = NELEM3 - NELEM2 + 1
        CALL OV('X=C     ' , W1(I1,4) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W1(I1,5) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W1(I1,6) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W2(I1,4) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W2(I1,5) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W2(I1,6) , Y , Z , 0.D0 , NELEM2)
!
        CALL OV('X=XY    ' , W1(1,1) , W3 , Z , C , 3*NELEM3)
        CALL OV('X=XY    ' , W1(1,4) , W3 , Z , C , 3*NELEM3)
        CALL OV('X=XY    ' , W2(1,1) , W3 , Z , C , 3*NELEM3)
        CALL OV('X=XY    ' , W2(1,4) , W3 , Z , C , 3*NELEM3)
!
        DO IETAGE = 1 , NETAGE
          I1 = NELEM2*(IETAGE-1) + 1
          CALL OV('X=XY    ' , W3(I1,1) , SURFAC , Z , C , NELEM2)
          CALL OV('X=XY    ' , W3(I1,2) , SURFAC , Z , C , NELEM2)
          CALL OV('X=XY    ' , W3(I1,3) , SURFAC , Z , C , NELEM2)
        ENDDO
        CALL OV('X=Y     ' , W3(1,4) , W3 , Z , C , 3*NELEM3)
!
        CALL ASSVEC(CV1,IKLE3,NPOIN3,NELEM3,NELEM3,41,W1,.FALSE.,
     &              LV,MSK,MASKEL,6)
        CALL ASSVEC(CV2,IKLE3,NPOIN3,NELEM3,NELEM3,41,W2,.FALSE.,
     &              LV,MSK,MASKEL,6)
        CALL ASSVEC(T1,IKLE3,NPOIN3,NELEM3,NELEM3,41,W3,.TRUE. ,
     &              LV,MSK,MASKEL,6)
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(SCV1,2,MESH3)
          CALL PARCOM(SCV2,2,MESH3)
          CALL PARCOM(ST1,2,MESH3)
        ENDIF
!
        CALL OVD('X=CY/Z  ',CV1,CV1,T1,0.25D0*GRAV,NPOIN3,2,0.D0,1D-9)
        CALL OVD('X=CY/Z  ',CV2,CV2,T1,0.25D0*GRAV,NPOIN3,2,0.D0,1D-9)
!
      ENDIF
!
!     IF(NTRAC.GT.0)
      ENDIF
!
!-----------------------------------------------------------------------
!  CORIOLIS FORCE
!
!  NOTE JMH : THERE ARE ADDITIONAL TERMS IF W IS TAKEN INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
      IF(CORIOL) THEN
!
         IF(SCV1%TYPR.EQ.'0') THEN
           CALL OS( 'X=0     ' , X=SCV1 )
           CALL OS( 'X=0     ' , X=SCV2 )
           SCV1%TYPR='Q'
           SCV2%TYPR='Q'
         ENDIF
!
         PI=ACOS(-1.D0)
         OMEGA=2.D0*PI/86164.D0
!
! - NORD IS THE ANGLE BETWEEN NORTH AND THE USER'S Y AXIS
!
         COSNORD=COS(PI*NORD/180.D0)
         SINNORD=SIN(PI*NORD/180.D0)
!
! - IF CORIOLIS FORCE DEPENDS ON Y COORDINATE (DEFAULT)
!
        CORIVAR=.FALSE.
!
        IF(CORIVAR) THEN
!
          FF0=2.D0*OMEGA*SIN(LATIT*PI/180.D0)
!
          DO I=1,NPOIN3
!
!                            6.37D6 : EARTH RADIUS
          BETA0Y=(2.D0*OMEGA/6.37D6)*COS(LATIT*PI/180.D0)
     &                              *(Y(I)*COSNORD-X(I)*SINNORD)
          CORI=FF0+BETA0Y
!
          IF(10.D0*BETA0Y.GE.FF0)THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'TRISOU : APPROX. BETA NON VALIDE'
            ELSE
              WRITE(LU,*) 'TRISOU: BETA APPROX. NOT VALID'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!
          CV1(I)=CV1(I)+VN3(I)*CORI
          CV2(I)=CV2(I)-UN3(I)*CORI
!
          ENDDO
!
        ELSE
!
          CALL OV('X=X+CY   ',CV1,VN3,VN3, FCOR,NPOIN3)
          CALL OV('X=X+CY   ',CV2,UN3,UN3,-FCOR,NPOIN3)
!
        ENDIF
!
      ENDIF
!
!***********************************************************************
!
!     * WITH WAVE DRIVEN CURRENTS
!       -------------------------
!
!       FORCING TERMS FROM A TOMAWAC RESULTS FILE
!
!       BEWARE :    1. MESHES MUST BE THE SAME
!       ---------
!                   2. TAKES THE LAST TIMESTEP FROM TOMAWAC FILE
!
      IF(COUROU) THEN
!
         IF(.NOT.DEJALU.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
!
            ALLOCATE(W(NPOIN2),STAT=ERR)
            IF(ERR.NE.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'ERREUR D''ALLOCATION DE W DANS TRISOU'
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'MEMORY ALLOCATION ERROR OF W IN TRISOU'
              ENDIF
            ENDIF
!
!           T3DBI1 : BINARY DATA FILE 1
            NOMX='FORCE FX        '
            NOMY='FORCE FY        '
            CALL FIND_IN_SEL(FXH,NOMX,T3D_FILES(T3DBI1)%LU,
     &                       W,OKX,NPTH,NP,ATH)
            CALL FIND_IN_SEL(FYH,NOMY,T3D_FILES(T3DBI1)%LU,
     &                       W,OKY,NPTH,NP,ATH)
!
            IF(.NOT.OKX.OR..NOT.OKY) THEN
              IF(LNG.EQ.1) WRITE(LU,5)
              IF(LNG.EQ.2) WRITE(LU,6)
 5            FORMAT(1X,'TRISOU : FORCE FX OU FY NON TROUVES',/,1X,
     &                  '         DANS LE FICHIER DE HOULE')
 6            FORMAT(1X,'TRISOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                  '        IN THE WAVE RESULTS FILE')
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NP.NE.NPOIN2) THEN
              IF(LNG.EQ.1) WRITE(LU,95)
              IF(LNG.EQ.2) WRITE(LU,96)
 95           FORMAT(1X,'TRISOU : SIMULATION DES COURANTS DE HOULE.',/,
     &               1X,'LES MAILLAGES HOULE ET COURANTS SONT ',/,
     &               1X,'DIFFERENTS : PAS POSSIBLE POUR LE MOMENT.')
 96           FORMAT(1X,'TRISOU: WAVE DRIVEN CURRENTS MODELLING.',/,
     &               1X,'WAVE AND CURRENT MODELS MESHES ARE ',/,
     &               1X,'DIFFERENT : NOT POSSIBLE AT THE MOMENT.')
!
              CALL PLANTE(1)
              STOP
            ENDIF
!           WRITES OUT TO LISTING
            IF(LNG.EQ.1) WRITE(LU,115) ATH
            IF(LNG.EQ.2) WRITE(LU,116) ATH
115         FORMAT(1X,/,1X,'TRISOU : COURANTS DE HOULE',/,
     &                  1X,'         LECTURE AU TEMPS ',F10.3,/)
116         FORMAT(1X,/,1X,'TRISOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                  1X,'         READING FILE AT TIME ',F10.3,/)
            DEJALU = .TRUE.
!
         ENDIF
!
!        ADDS TO SOURCE TERMS
!
         IF(SCV1%TYPR.EQ.'0') THEN
           DO I=1,NPOIN2
             DO IPLAN=1,NPLAN
               I3D=((IPLAN-1)*NPOIN2)+I
!              CV1(I3D)=1.5D0*FXH%R(I)  (SOGREAH-PECHON-TEISSON VERSION)
               CV1(I3D)=FXH%R(I)
               CV2(I3D)=FYH%R(I)
             ENDDO
           ENDDO
           SCV1%TYPR='Q'
           SCV2%TYPR='Q'
         ELSE
           DO I=1,NPOIN2
             DO IPLAN=1,NPLAN
               I3D=((IPLAN-1)*NPOIN2)+I
!              CV1(I3D)=CV1(I3D)+1.5D0*FXH%R(I)  (SOGREAH-PECHON-TEISSON VERSION)
               CV1(I3D)=CV1(I3D)+FXH%R(I)
               CV2(I3D)=CV2(I3D)+FYH%R(I)
             ENDDO
           ENDDO
         ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! TAKES THE VELOCITY OF SOURCES INTO ACCOUNT
!
! NOTE : IF USCE AND VSCE ARE NOT GIVEN, CONSIDERS THAT
!        USCE=UN3 AND VSCE=VN3
!
      IF(NREJEU.GT.0.AND.SCHCVI.NE.ADV_NSC.AND.SCHCVI.NE.ADV_PSI
     &              .AND.SCHCVI.NE.ADV_LPO.AND.SCHCVI.NE.ADV_NSC_TF
     &              .AND.SCHCVI.NE.ADV_LPO_TF) THEN
!
        IF(SCV1%TYPR.EQ.'0') THEN
          CALL OS( 'X=0     ' , X=SCV1 )
          CALL OS( 'X=0     ' , X=SCV2 )
          SCV1%TYPR='Q'
          SCV2%TYPR='Q'
        ENDIF
!
!       WITH DISTRIBUTIVE SCHEMES AND FINITE VOLUME SCHEMES
!       THIS IS DONE DIRECTLY INTO SUBROUTINE MURD3D, AND NOT WITH CV1
!
        DO I=1,NREJEU
        CV1((KSCE(I)-1)*NPOIN2+ISCE(I))=CV1((KSCE(I)-1)*NPOIN2+ISCE(I))
     &      + (USCE(I)-UN3((KSCE(I)-1)*NPOIN2+ISCE(I)))*
     &               QSCE(I)/VOLU(ISCE(I),KSCE(I))
        CV2((KSCE(I)-1)*NPOIN2+ISCE(I))=CV2((KSCE(I)-1)*NPOIN2+ISCE(I))
     &      + (VSCE(I)-VN3((KSCE(I)-1)*NPOIN2+ISCE(I)))*
     &               QSCE(I)/VOLU(ISCE(I),KSCE(I))
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
