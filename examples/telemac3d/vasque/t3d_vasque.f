!
!  CHANGES VS SOURCE FILES:
!  IN T3D_CORFON
!  IN SL3: PROGRAMMABLE PART
!
!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        29/09/2011
!+        V6P2
!+   Name changed into T3D_CORFON to avoid duplication with Telemac-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : MESH3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IM,JM,J,POS_LOC
      LOGICAL MAS
      DOUBLE PRECISION EIKON
!
!-----------------------------------------------------------------------
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
        MAS = .TRUE.
!
        CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
!
!-----------------------------------------------------------------------
!
      IM = 47
      JM = 10
!
!  VARIANTE FOND EN PENTE RECTILIGNE + CUVETTE
!
      DO I=1,IM
        DO J=1,JM
!         PENTE RECTILIGNE
          POS_LOC = GLOBAL_TO_LOCAL_POINT(I+(J-1)*IM,MESH3D)
!
!         NOTE JMH: THIS IS VERY HEAVY, THERE SHOULD BE A
!                   FORMULA FUNCTION OF X.
!
          IF(POS_LOC.GT.0) THEN
            ZF(POS_LOC) = -0.6D0+0.46D0*FLOAT(I-1)/FLOAT(IM-1)
!           BOSSE GAUSSIENNE
            IF(I.GT.9.AND.I.LT.29) THEN
              EIKON = -(I-19)**2/20.D0
              ZF(POS_LOC) = ZF(POS_LOC) + 0.1D0*EXP(EIKON)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************************
                     DOUBLE PRECISION FUNCTION SL3
!                    *****************************
!
     &( I , TIME , N , ENTET )
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL
!+                IMPOSED LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNH)
!+        12/12/00
!+        V5P9
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size FCT due to modification of TRACER numbering
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| IF YES, LISTING PRINTOUTS ALLOWED
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER OF POINT IN ORIGINAL MESH
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I,N
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) :: FCT
!
      DOUBLE PRECISION PI,OMEGA,A
      DATA PI/3.141592653589D0/
!
!-----------------------------------------------------------------------
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OKSL3(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT='SL(      '
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:5)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:6)=')'
        ELSE
          IF (LNG.EQ.1) WRITE(LU,*) 'SL3 NE PEUT PAS GERER PLUS
     &                               DE 99 FRONTIERES'
          IF (LNG.EQ.2) WRITE(LU,*) 'SL3 NOT PROGRAMMED FOR MORE
     &                               THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(SL3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OKSL3(I))
!
      ENDIF
!
      IF(.NOT.OKSL3(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     SL IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
! BEGINNING OF SPECIFIC TO THIS CASE
!       SL3 = COTIMP(I)
        OMEGA = PI/300.D0
        A = 0.275D0
        SL3 = A*(COS(OMEGA*TIME)-1.D0)
! END OF PART SPECIFIC TO THIS CASE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
