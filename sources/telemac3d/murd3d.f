!                    *****************
                     SUBROUTINE MURD3D
!                    *****************
!
     &(FC,FN,VOLU,VOLUN,VOLU2,SVOLU2,DB,XB,DIM1XB,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,IKLE3,MESH3,
     & NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL,INFOR,CALFLU,FLUX,FLUEXT,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,TRAIN,NPOIN2,MINFC,MAXFC,MASKPT,
     & OPTBAN,FLODEL,FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN,IELM3,OPTSOU)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    ADVECTION OF A VARIABLE WITH THE DISTRIBUTIVE SCHEME
!+                AFTER HAVING COMPUTED THE DISTRIBUTION MATRIX WHICH IS:
!+
!+            - COMMON TO ALL THE VARIABLES (N SCHEME),
!+
!+            - SPECIFIC TO EACH VARIABLE (PSI SCHEME).
!
!warning  FOR THE N SCHEME
!+            MATRIX B MUST BE CONFUSED WITH MATRIX A
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history
!+        28/08/07
!+
!+   PSI SCHEME RE-WRITTEN, NO THEORETICAL CHANGES BUT
!
!history
!+        19/11/07
!+
!+   RAIN HAD BEEN LEFT OUT IN THE PART WITH ALFA
!
!history  J-M HERVOUET (LNHE)
!+        19/12/07
!+
!+   CHANGED MONOTONICITY CRITERION
!
!history  J-M HERVOUET (LNHE)
!+        29/07/08
!+
!+   TIDAL FLATS WITH OPTBAN=2
!
!history  J-M HERVOUET (LNHE)
!+        04/08/08
!+
!+   DIMENSIONS OF XA AND XB INVERTED (SEE ALSO MT14PP)
!
!history  J-M HERVOUET (LNHE)
!+        22/06/09
!+
!+   FINITE VOLUME SCHEME ADDED
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
!+
!history  J-M HERVOUET (LNHE)
!+        04/01/2012
!+        V6P2
!+   Adaptation to tetrahedra, PSI scheme optimised
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2012
!+        V6P2
!+   Values of tracers in rain taken into account.
!
!history  J-M HERVOUET (LNHE)
!+        27/07/2015
!+        V7P1
!+   Guilty point in case of maximum iterations is now printed with its
!+   global number in parallel.
!
!history  A. JOLY (EDF LAB, LNHE)
!+        27/08/2015
!+        V7P1
!+   Imposed flowrates on the bed.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| DB             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| DIM1XB         |-->| FIRST DIMENSION OF XB
!| DT             |-->| TIME STEP
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FLODEL         |-->| FLUX BY MESH EDGES
!| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUX           |<->| FLUXES TO BE CHANGED
!| FN             |-->| VARIABLE AT TIME N
!| FSCE           |-->| SOURCE
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| IELM3          |-->| TYPE OF ELEMENT (41:PRISM, ETC.)
!| IKLE3          |-->| GLOBAL 3D CONNECTIVITY
!| INFOR          |-->| INFORMATIONS FOR SOLVERS
!| LV             |-->| VECTOR LENGTH OF THE MACHINE
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MAXFC          |<->| MAXIMUM VALUE FOR FC
!| MESH3          |<->| 3D MESH
!| MINFC          |<->| MINIMUM VALUE FOR FC
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| S0F            |-->| EXPLICIT SOURCE TERM
!| SCHCF          |-->| ADVECTION SCHEME FOR F
!| SOURCES        |-->| SOURCES
!| STRA01         |<->| STRUCTURE OF TRA01
!| STRA02         |<->| STRUCTURE OF TRA02
!| STRA03         |<->| STRUCTURE OF TRA03
!| SVOLU2         |-->| STRUCTURE OF VOLU2
!| TRA01          |<->| WORK ARRAY OF DIMENSION NPOIN3 EQUIVALENT TO
!|                |   | VOLU2 FOR CURRENT FINAL TIME
!| TRA02          |<->| WORK ARRAY
!| TRA03          |<->| WORK ARRAY
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| VOLU           |-->| CONTROL VOLUME AT TIME N+1
!| VOLU2          |<->| LIKE VOLU, BUT ASSEMBLED IN PARALLEL
!| VOLUN          |-->| CONTROL VOLUME AT TIME N
!| XB             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D, ONLY: BEDBOU,BEDFLU,T2_18,MESH2D,
     &                                  KSCE, ISCE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,LV,NPOIN2
      INTEGER, INTENT(IN)             :: IELM3,DIM1XB,OPTSOU
!                                                     6 OR 4
      INTEGER, INTENT(IN)             :: IKLE3(NELEM3,*),NSCE,OPTBAN
      INTEGER, INTENT(IN)             :: NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),PLUIE(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3), FLUEXT(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3), VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3), TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE),MASKPT(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DT,TRAIN
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2,MINFC,MAXFC
      TYPE(BIEF_OBJ), INTENT(IN)      :: SOURCES,S0F
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: STRA01,STRA02,STRA03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN3),XB(DIM1XB,NELEM3)
!
!     DIMENSION OF FLODEL AND FLOPAR=NSEG2D*NPLAN+NPOIN2*NETAGE
      DOUBLE PRECISION, INTENT(IN)    :: FLODEL(*),FLOPAR(*)
!
      LOGICAL, INTENT(IN)             :: MSK,INFOR,CALFLU,RAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION DTJ,PHIP,PHIM,ALFA,C,ALFA2,DTJALFA,MINEL,MAXEL
      DOUBLE PRECISION M12,M13,M14,M15,M16,M23,M24,M25,M26,M34
      DOUBLE PRECISION M35,M36,M45,M46,M56,T1,T2,T3,T4,T5,T6
      DOUBLE PRECISION F1MF2,F1MF3,F1MF4,F2MF3,F2MF4,F3MF4,ALFALOC
!
      INTEGER IELEM,IPOIN,NITER,IS,IGUILT,IIS
      INTEGER I1,I2,I3,I4,I5,I6,OPT,ISEG3D,NSEGH,NSEGV
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPS
      DATA EPS /1.D-6/
!     DATA EPS /10.D0/
!
!-----------------------------------------------------------------------
!
      CALL CPSTVC(SVOLU2,STRA01)
      CALL CPSTVC(SVOLU2,STRA02)
      CALL CPSTVC(SVOLU2,STRA03)
      CALL CPSTVC(SVOLU2,MINFC)
      CALL CPSTVC(SVOLU2,MAXFC)
!
      NITER = 0
      DTJ = DT
!
      CALL OV ('X=Y     ',FC,FN,FN,C,NPOIN3)
!
      CALL OV ('X=Y     ',TRA01, VOLUN, VOLUN, C, NPOIN3)
      IF (NCSIZE.GT.1) CALL PARCOM(STRA01,2,MESH3)
!
      CALL OV ('X=Y     ',VOLU2, VOLU, VOLU, C, NPOIN3)
      IF (NCSIZE.GT.1) CALL PARCOM(SVOLU2,2,MESH3)
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!  BUILDS THE PSI SCHEME FROM THE N SCHEME IF SCHCF=5
!  SEE "HYDRODYNAMICS OF FREE SURFACE FLOWS" PAGE 193
!
      IF(SCHCF.EQ.ADV_PSI) THEN
!
        DO IPOIN=1,NPOIN3
          TRA02(IPOIN)=0.D0
        ENDDO
!
        IF(IELM3.EQ.41) THEN
!
        DO IELEM = 1,NELEM3
!
          I1 = IKLE3(IELEM,1)
          I2 = IKLE3(IELEM,2)
          I3 = IKLE3(IELEM,3)
          I4 = IKLE3(IELEM,4)
          I5 = IKLE3(IELEM,5)
          I6 = IKLE3(IELEM,6)
!
          T1 = FC(I1)
          T2 = FC(I2)
          T3 = FC(I3)
          T4 = FC(I4)
          T5 = FC(I5)
          T6 = FC(I6)
!
          M12 = (XB(01,IELEM)-XB(16,IELEM)) * (T1-T2)
          M13 = (XB(02,IELEM)-XB(17,IELEM)) * (T1-T3)
          M14 = (XB(03,IELEM)-XB(18,IELEM)) * (T1-T4)
          M15 = (XB(04,IELEM)-XB(19,IELEM)) * (T1-T5)
          M16 = (XB(05,IELEM)-XB(20,IELEM)) * (T1-T6)
          M23 = (XB(06,IELEM)-XB(21,IELEM)) * (T2-T3)
          M24 = (XB(07,IELEM)-XB(22,IELEM)) * (T2-T4)
          M25 = (XB(08,IELEM)-XB(23,IELEM)) * (T2-T5)
          M26 = (XB(09,IELEM)-XB(24,IELEM)) * (T2-T6)
          M34 = (XB(10,IELEM)-XB(25,IELEM)) * (T3-T4)
          M35 = (XB(11,IELEM)-XB(26,IELEM)) * (T3-T5)
          M36 = (XB(12,IELEM)-XB(27,IELEM)) * (T3-T6)
          M45 = (XB(13,IELEM)-XB(28,IELEM)) * (T4-T5)
          M46 = (XB(14,IELEM)-XB(29,IELEM)) * (T4-T6)
          M56 = (XB(15,IELEM)-XB(30,IELEM)) * (T5-T6)
!
          PHIP = MAX( M12,0.D0) + MAX( M13,0.D0) + MAX( M14,0.D0)
     &         + MAX( M15,0.D0) + MAX( M16,0.D0) + MAX( M23,0.D0)
     &         + MAX( M24,0.D0) + MAX( M25,0.D0) + MAX( M26,0.D0)
     &         + MAX( M34,0.D0) + MAX( M35,0.D0) + MAX( M36,0.D0)
     &         + MAX( M45,0.D0) + MAX( M46,0.D0) + MAX( M56,0.D0)
          PHIM = MAX(-M12,0.D0) + MAX(-M13,0.D0) + MAX(-M14,0.D0)
     &         + MAX(-M15,0.D0) + MAX(-M16,0.D0) + MAX(-M23,0.D0)
     &         + MAX(-M24,0.D0) + MAX(-M25,0.D0) + MAX(-M26,0.D0)
     &         + MAX(-M34,0.D0) + MAX(-M35,0.D0) + MAX(-M36,0.D0)
     &         + MAX(-M45,0.D0) + MAX(-M46,0.D0) + MAX(-M56,0.D0)
!
          IF(PHIP.GE.PHIM) THEN
            ALFA = (PHIP - PHIM) / MAX(PHIP,1.D-10)
            IF(T2.GT.T1) THEN
              TRA02(I2)=TRA02(I2)+XB(16,IELEM)*ALFA*(FC(I1)-FC(I2))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(01,IELEM)*ALFA*(FC(I2)-FC(I1))
            ENDIF
            IF(T3.GT.T1) THEN
              TRA02(I3)=TRA02(I3)+XB(17,IELEM)*ALFA*(FC(I1)-FC(I3))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(02,IELEM)*ALFA*(FC(I3)-FC(I1))
            ENDIF
            IF(T4.GT.T1) THEN
              TRA02(I4)=TRA02(I4)+XB(18,IELEM)*ALFA*(FC(I1)-FC(I4))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(03,IELEM)*ALFA*(FC(I4)-FC(I1))
            ENDIF
            IF(T5.GT.T1) THEN
              TRA02(I5)=TRA02(I5)+XB(19,IELEM)*ALFA*(FC(I1)-FC(I5))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(04,IELEM)*ALFA*(FC(I5)-FC(I1))
            ENDIF
            IF(T6.GT.T1) THEN
              TRA02(I6)=TRA02(I6)+XB(20,IELEM)*ALFA*(FC(I1)-FC(I6))
            ELSE
              TRA02(I1)=TRA02(I1)+XB(05,IELEM)*ALFA*(FC(I6)-FC(I1))
            ENDIF
            IF(T3.GT.T2) THEN
              TRA02(I3)=TRA02(I3)+XB(21,IELEM)*ALFA*(FC(I2)-FC(I3))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(06,IELEM)*ALFA*(FC(I3)-FC(I2))
            ENDIF
            IF(T4.GT.T2) THEN
              TRA02(I4)=TRA02(I4)+XB(22,IELEM)*ALFA*(FC(I2)-FC(I4))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*(FC(I4)-FC(I2))
            ENDIF
            IF(T5.GT.T2) THEN
              TRA02(I5)=TRA02(I5)+XB(23,IELEM)*ALFA*(FC(I2)-FC(I5))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(08,IELEM)*ALFA*(FC(I5)-FC(I2))
            ENDIF
            IF(T6.GT.T2) THEN
              TRA02(I6)=TRA02(I6)+XB(24,IELEM)*ALFA*(FC(I2)-FC(I6))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(09,IELEM)*ALFA*(FC(I6)-FC(I2))
            ENDIF
            IF(T4.GT.T3) THEN
              TRA02(I4)=TRA02(I4)+XB(25,IELEM)*ALFA*(FC(I3)-FC(I4))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*(FC(I4)-FC(I3))
            ENDIF
            IF(T5.GT.T3) THEN
              TRA02(I5)=TRA02(I5)+XB(26,IELEM)*ALFA*(FC(I3)-FC(I5))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(11,IELEM)*ALFA*(FC(I5)-FC(I3))
            ENDIF
            IF(T6.GT.T3) THEN
              TRA02(I6)=TRA02(I6)+XB(27,IELEM)*ALFA*(FC(I3)-FC(I6))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(12,IELEM)*ALFA*(FC(I6)-FC(I3))
            ENDIF
            IF(T5.GT.T4) THEN
              TRA02(I5)=TRA02(I5)+XB(28,IELEM)*ALFA*(FC(I4)-FC(I5))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(13,IELEM)*ALFA*(FC(I5)-FC(I4))
            ENDIF
            IF(T6.GT.T4) THEN
              TRA02(I6)=TRA02(I6)+XB(29,IELEM)*ALFA*(FC(I4)-FC(I6))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(14,IELEM)*ALFA*(FC(I6)-FC(I4))
            ENDIF
            IF(T6.GT.T5) THEN
              TRA02(I6)=TRA02(I6)+XB(30,IELEM)*ALFA*(FC(I5)-FC(I6))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(15,IELEM)*ALFA*(FC(I6)-FC(I5))
            ENDIF
          ELSE
            ALFA = (PHIM - PHIP) / MAX(PHIM,1.D-10)
            IF(T2.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(01,IELEM)*ALFA*(FC(I2)-FC(I1))
            ELSE
              TRA02(I2)=TRA02(I2)+XB(16,IELEM)*ALFA*(FC(I1)-FC(I2))
            ENDIF
            IF(T3.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(02,IELEM)*ALFA*(FC(I3)-FC(I1))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(17,IELEM)*ALFA*(FC(I1)-FC(I3))
            ENDIF
            IF(T4.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(03,IELEM)*ALFA*(FC(I4)-FC(I1))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(18,IELEM)*ALFA*(FC(I1)-FC(I4))
            ENDIF
            IF(T5.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(04,IELEM)*ALFA*(FC(I5)-FC(I1))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(19,IELEM)*ALFA*(FC(I1)-FC(I5))
            ENDIF
            IF(T6.GT.T1) THEN
              TRA02(I1)=TRA02(I1)+XB(05,IELEM)*ALFA*(FC(I6)-FC(I1))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(20,IELEM)*ALFA*(FC(I1)-FC(I6))
            ENDIF
            IF(T3.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(06,IELEM)*ALFA*(FC(I3)-FC(I2))
            ELSE
              TRA02(I3)=TRA02(I3)+XB(21,IELEM)*ALFA*(FC(I2)-FC(I3))
            ENDIF
            IF(T4.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*(FC(I4)-FC(I2))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(22,IELEM)*ALFA*(FC(I2)-FC(I4))
            ENDIF
            IF(T5.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(08,IELEM)*ALFA*(FC(I5)-FC(I2))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(23,IELEM)*ALFA*(FC(I2)-FC(I5))
            ENDIF
            IF(T6.GT.T2) THEN
              TRA02(I2)=TRA02(I2)+XB(09,IELEM)*ALFA*(FC(I6)-FC(I2))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(24,IELEM)*ALFA*(FC(I2)-FC(I6))
            ENDIF
            IF(T4.GT.T3) THEN
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*(FC(I4)-FC(I3))
            ELSE
              TRA02(I4)=TRA02(I4)+XB(25,IELEM)*ALFA*(FC(I3)-FC(I4))
            ENDIF
            IF(T5.GT.T3) THEN
              TRA02(I3)=TRA02(I3)+XB(11,IELEM)*ALFA*(FC(I5)-FC(I3))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(26,IELEM)*ALFA*(FC(I3)-FC(I5))
            ENDIF
            IF(T6.GT.T3) THEN
              TRA02(I3)=TRA02(I3)+XB(12,IELEM)*ALFA*(FC(I6)-FC(I3))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(27,IELEM)*ALFA*(FC(I3)-FC(I6))
            ENDIF
            IF(T5.GT.T4) THEN
              TRA02(I4)=TRA02(I4)+XB(13,IELEM)*ALFA*(FC(I5)-FC(I4))
            ELSE
              TRA02(I5)=TRA02(I5)+XB(28,IELEM)*ALFA*(FC(I4)-FC(I5))
            ENDIF
            IF(T6.GT.T4) THEN
              TRA02(I4)=TRA02(I4)+XB(14,IELEM)*ALFA*(FC(I6)-FC(I4))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(29,IELEM)*ALFA*(FC(I4)-FC(I6))
            ENDIF
            IF(T6.GT.T5) THEN
              TRA02(I5)=TRA02(I5)+XB(15,IELEM)*ALFA*(FC(I6)-FC(I5))
            ELSE
              TRA02(I6)=TRA02(I6)+XB(30,IELEM)*ALFA*(FC(I5)-FC(I6))
            ENDIF
          ENDIF
!
        ENDDO ! IELEM
!
        ELSEIF(IELM3.EQ.51) THEN
!
        DO IELEM = 1,NELEM3
!
          I1 = IKLE3(IELEM,1)
          I2 = IKLE3(IELEM,2)
          I3 = IKLE3(IELEM,3)
          I4 = IKLE3(IELEM,4)
!
          F1MF2 = FC(I1)-FC(I2)
          F1MF3 = FC(I1)-FC(I3)
          F1MF4 = FC(I1)-FC(I4)
          F2MF3 = FC(I2)-FC(I3)
          F2MF4 = FC(I2)-FC(I4)
          F3MF4 = FC(I3)-FC(I4)
!
          M12 = (XB(01,IELEM)-XB(07,IELEM)) * F1MF2
          M13 = (XB(02,IELEM)-XB(08,IELEM)) * F1MF3
          M14 = (XB(03,IELEM)-XB(09,IELEM)) * F1MF4
          M23 = (XB(04,IELEM)-XB(10,IELEM)) * F2MF3
          M24 = (XB(05,IELEM)-XB(11,IELEM)) * F2MF4
          M34 = (XB(06,IELEM)-XB(12,IELEM)) * F3MF4
!
          PHIP = MAX( M12,0.D0) + MAX( M13,0.D0) + MAX( M14,0.D0)
     &         + MAX( M23,0.D0) + MAX( M24,0.D0) + MAX( M34,0.D0)
          PHIM = MAX(-M12,0.D0) + MAX(-M13,0.D0) + MAX(-M14,0.D0)
     &         + MAX(-M23,0.D0) + MAX(-M24,0.D0) + MAX(-M34,0.D0)
!
          IF(PHIP.GE.PHIM) THEN
            ALFA = (PHIP - PHIM) / MAX(PHIP,1.D-10)
            IF(F1MF2.LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*F1MF2
            ELSE
              TRA02(I1)=TRA02(I1)-XB(01,IELEM)*ALFA*F1MF2
            ENDIF
            IF(F1MF3.LT.0.D0) THEN
              TRA02(I3)=TRA02(I3)+XB(08,IELEM)*ALFA*F1MF3
            ELSE
              TRA02(I1)=TRA02(I1)-XB(02,IELEM)*ALFA*F1MF3
            ENDIF
            IF(F1MF4.LT.0.D0) THEN
              TRA02(I4)=TRA02(I4)+XB(09,IELEM)*ALFA*F1MF4
            ELSE
              TRA02(I1)=TRA02(I1)-XB(03,IELEM)*ALFA*F1MF4
            ENDIF
            IF(F2MF3.LT.0.D0) THEN
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*F2MF3
            ELSE
              TRA02(I2)=TRA02(I2)-XB(04,IELEM)*ALFA*F2MF3
            ENDIF
            IF(F2MF4.LT.0.D0) THEN
              TRA02(I4)=TRA02(I4)+XB(11,IELEM)*ALFA*F2MF4
            ELSE
              TRA02(I2)=TRA02(I2)-XB(05,IELEM)*ALFA*F2MF4
            ENDIF
            IF(F3MF4.LT.0.D0) THEN
              TRA02(I4)=TRA02(I4)+XB(12,IELEM)*ALFA*F3MF4
            ELSE
              TRA02(I3)=TRA02(I3)-XB(06,IELEM)*ALFA*F3MF4
            ENDIF
          ELSE
            ALFA = (PHIM - PHIP) / MAX(PHIM,1.D-10)
            IF(F1MF2.LT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-XB(01,IELEM)*ALFA*F1MF2
            ELSE
              TRA02(I2)=TRA02(I2)+XB(07,IELEM)*ALFA*F1MF2
            ENDIF
            IF(F1MF3.LT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-XB(02,IELEM)*ALFA*F1MF3
            ELSE
              TRA02(I3)=TRA02(I3)+XB(08,IELEM)*ALFA*F1MF3
            ENDIF
            IF(F1MF4.LT.0.D0) THEN
              TRA02(I1)=TRA02(I1)-XB(03,IELEM)*ALFA*F1MF4
            ELSE
              TRA02(I4)=TRA02(I4)+XB(09,IELEM)*ALFA*F1MF4
            ENDIF
            IF(F2MF3.LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)-XB(04,IELEM)*ALFA*F2MF3
            ELSE
              TRA02(I3)=TRA02(I3)+XB(10,IELEM)*ALFA*F2MF3
            ENDIF
            IF(F2MF4.LT.0.D0) THEN
              TRA02(I2)=TRA02(I2)-XB(05,IELEM)*ALFA*F2MF4
            ELSE
              TRA02(I4)=TRA02(I4)+XB(11,IELEM)*ALFA*F2MF4
            ENDIF
            IF(F3MF4.LT.0.D0) THEN
              TRA02(I3)=TRA02(I3)-XB(06,IELEM)*ALFA*F3MF4
            ELSE
              TRA02(I4)=TRA02(I4)+XB(12,IELEM)*ALFA*F3MF4
            ENDIF
          ENDIF
!
        ENDDO ! IELEM
!
        ELSE
          WRITE(LU,*) 'ELEMENT ',IELM3,' NOT COMPUTED IN MURD3D'
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(SCHCF.EQ.ADV_NSC) THEN
!
!  COMPUTES THE FLUX TERMS PER UNIT OF TIME:
!
!  I.E. : SUM ON J (LAMBDA(I,J)*(FC(J)-FC(I))
!
!  DECOMPOSED INTO : -(SUM ON J (LAMBDA(I,J)*(FC(I)) WHICH IS DB * FC(I)
!                    +(SUM ON J (LAMBDA(I,J)*(FC(J)) WHICH IS XB * FC
!
!     CALL MV0606
!    &('X=AY    ', TRA02, DB, 'Q', XB, 'Q', FC, C, IKLE3(1,1),
!    & IKLE3(1,2),IKLE3(1,3),IKLE3(1,4),IKLE3(1,5),IKLE3(1,6),
!    & NPOIN3, NELEM3, NELEM3,
!    & W1(1,1),W1(1,2),W1(1,3),W1(1,4),W1(1,5),W1(1,6))
!     CALL ASSVEC
!    &(TRA02,IKLE3,NPOIN3,NELEM3,NELEM3,41,W1,.FALSE.,LV,MSK,MASKEL)
!
!     REPLACES CALL MV0606 AND CALL ASSVEC
!
      CALL OV ('X=YZ    ',TRA02,DB,FC,C,NPOIN3)
!
      IF(IELM3.EQ.41) THEN
!
      DO IELEM = 1 , NELEM3
!
        I1 = IKLE3(IELEM,1)
        I2 = IKLE3(IELEM,2)
        I3 = IKLE3(IELEM,3)
        I4 = IKLE3(IELEM,4)
        I5 = IKLE3(IELEM,5)
        I6 = IKLE3(IELEM,6)
!
        TRA02(I1) = TRA02(I1) + XB(01,IELEM) * FC(I2)
     &                        + XB(02,IELEM) * FC(I3)
     &                        + XB(03,IELEM) * FC(I4)
     &                        + XB(04,IELEM) * FC(I5)
     &                        + XB(05,IELEM) * FC(I6)
!
        TRA02(I2) = TRA02(I2) + XB(16,IELEM) * FC(I1)
     &                        + XB(06,IELEM) * FC(I3)
     &                        + XB(07,IELEM) * FC(I4)
     &                        + XB(08,IELEM) * FC(I5)
     &                        + XB(09,IELEM) * FC(I6)
!
        TRA02(I3) = TRA02(I3) + XB(17,IELEM) * FC(I1)
     &                        + XB(21,IELEM) * FC(I2)
     &                        + XB(10,IELEM) * FC(I4)
     &                        + XB(11,IELEM) * FC(I5)
     &                        + XB(12,IELEM) * FC(I6)
!
        TRA02(I4) = TRA02(I4) + XB(18,IELEM) * FC(I1)
     &                        + XB(22,IELEM) * FC(I2)
     &                        + XB(25,IELEM) * FC(I3)
     &                        + XB(13,IELEM) * FC(I5)
     &                        + XB(14,IELEM) * FC(I6)
!
        TRA02(I5) = TRA02(I5) + XB(19,IELEM) * FC(I1)
     &                        + XB(23,IELEM) * FC(I2)
     &                        + XB(26,IELEM) * FC(I3)
     &                        + XB(28,IELEM) * FC(I4)
     &                        + XB(15,IELEM) * FC(I6)
!
        TRA02(I6) = TRA02(I6) + XB(20,IELEM) * FC(I1)
     &                        + XB(24,IELEM) * FC(I2)
     &                        + XB(27,IELEM) * FC(I3)
     &                        + XB(29,IELEM) * FC(I4)
     &                        + XB(30,IELEM) * FC(I5)
!
      ENDDO
!
      ELSEIF(IELM3.EQ.51) THEN
!
      DO IELEM = 1 , NELEM3
!
        I1 = IKLE3(IELEM,1)
        I2 = IKLE3(IELEM,2)
        I3 = IKLE3(IELEM,3)
        I4 = IKLE3(IELEM,4)
!
        TRA02(I1) = TRA02(I1) + XB(01,IELEM) * FC(I2)
     &                        + XB(02,IELEM) * FC(I3)
     &                        + XB(03,IELEM) * FC(I4)
!
        TRA02(I2) = TRA02(I2) + XB(04,IELEM) * FC(I3)
     &                        + XB(05,IELEM) * FC(I4)
     &                        + XB(07,IELEM) * FC(I1)
!
        TRA02(I3) = TRA02(I3) + XB(06,IELEM) * FC(I4)
     &                        + XB(08,IELEM) * FC(I1)
     &                        + XB(10,IELEM) * FC(I2)
!
        TRA02(I4) = TRA02(I4) + XB(09,IELEM) * FC(I1)
     &                        + XB(11,IELEM) * FC(I2)
     &                        + XB(12,IELEM) * FC(I3)
!
      ENDDO
!
      ELSE
        WRITE(LU,*) 'ELEMENT ',IELM3,' NOT COMPUTED IN MURD3D'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     END OF THE REPLACEMENT OF CALL MV0606
!
      ELSEIF(SCHCF.EQ.ADV_LPO) THEN
!
        IF(IELM3.NE.41) THEN
          WRITE(LU,*) 'MURD3D: ELEMENT ',IELM3,' NOT IMPLEMENTED'
          WRITE(LU,*) '        WITH SCHEME ',SCHCF
          CALL PLANTE(1)
          STOP
        ENDIF
!
        NSEGH=NSEG*NPLAN
        NSEGV=(NPLAN-1)*NPOIN2
!
!       COMPUTES DB AND TRA02 IN UPWIND EXPLICIT FINITE VOLUMES
!       POSSIBLE OPTIMISATION: DB IS NOT COMPUTED IF OPT=2
!
        DO IPOIN=1,NPOIN3
          DB(IPOIN)=0.D0
          TRA02(IPOIN)=0.D0
        ENDDO
!
!       HORIZONTAL AND VERTICAL FLUXES ONLY
!       BEWARE : FLUXES FROM POINT 2 TO 1 IN SEGMENT
!                WITH POINTS 1 AND 2 (SEE PRECON AND FLUX3D)
!
        DO ISEG3D = 1,NSEGH+NSEGV
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(FLOPAR(ISEG3D).GT.0.D0) THEN
            DB(I1)   = DB(I1)   -FLODEL(ISEG3D)
            TRA02(I1)= TRA02(I1)-FLODEL(ISEG3D)*(FC(I1)-FC(I2))
          ELSEIF(FLOPAR(ISEG3D).LT.0.D0) THEN
            DB(I2)   = DB(I2)   +FLODEL(ISEG3D)
            TRA02(I2)= TRA02(I2)+FLODEL(ISEG3D)*(FC(I2)-FC(I1))
          ENDIF
        ENDDO
!
      ELSE
        WRITE(LU,*) 'MURD3D: UNKNOWN ADVECTION SCHEME: ',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA02,2,MESH3)
!
!-----------------------------------------------------------------------
!
!  COMPUTES THE LIMITING SUB-TIMESTEP :
!     MULTIPLIES THE REMAINING SUB TIMESTEP BY DTJ
!     AND ADDS TO VOLU NEGATIVE TERM (MASS AT N+1)
!
!     OPT=1 : OLD CRITERION
!     OPT=2 : NEW CRITERION LESS RESTRICTIVE
      OPT=2
!
      IF(OPT.EQ.2) THEN
!
!     COMPUTES THE LOCAL EXTREMA
!
      DO IPOIN=1,NPOIN3
        MINFC%R(IPOIN)=FC(IPOIN)
        MAXFC%R(IPOIN)=FC(IPOIN)
      ENDDO
!
      IF(IELM3.EQ.41) THEN
!
        DO IELEM=1,NELEM3
          I1=IKLE3(IELEM,1)
          I2=IKLE3(IELEM,2)
          I3=IKLE3(IELEM,3)
          I4=IKLE3(IELEM,4)
          I5=IKLE3(IELEM,5)
          I6=IKLE3(IELEM,6)
          MINEL=MIN(FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6))
          MAXEL=MAX(FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6))
          MINFC%R(I1)=MIN(MINFC%R(I1),MINEL)
          MINFC%R(I2)=MIN(MINFC%R(I2),MINEL)
          MINFC%R(I3)=MIN(MINFC%R(I3),MINEL)
          MINFC%R(I4)=MIN(MINFC%R(I4),MINEL)
          MINFC%R(I5)=MIN(MINFC%R(I5),MINEL)
          MINFC%R(I6)=MIN(MINFC%R(I6),MINEL)
          MAXFC%R(I1)=MAX(MAXFC%R(I1),MAXEL)
          MAXFC%R(I2)=MAX(MAXFC%R(I2),MAXEL)
          MAXFC%R(I3)=MAX(MAXFC%R(I3),MAXEL)
          MAXFC%R(I4)=MAX(MAXFC%R(I4),MAXEL)
          MAXFC%R(I5)=MAX(MAXFC%R(I5),MAXEL)
          MAXFC%R(I6)=MAX(MAXFC%R(I6),MAXEL)
        ENDDO
!
      ELSEIF(IELM3.EQ.51) THEN
!
        DO IELEM=1,NELEM3
          I1=IKLE3(IELEM,1)
          I2=IKLE3(IELEM,2)
          I3=IKLE3(IELEM,3)
          I4=IKLE3(IELEM,4)
          MINEL=MIN(FC(I1),FC(I2),FC(I3),FC(I4))
          MAXEL=MAX(FC(I1),FC(I2),FC(I3),FC(I4))
          MINFC%R(I1)=MIN(MINFC%R(I1),MINEL)
          MINFC%R(I2)=MIN(MINFC%R(I2),MINEL)
          MINFC%R(I3)=MIN(MINFC%R(I3),MINEL)
          MINFC%R(I4)=MIN(MINFC%R(I4),MINEL)
          MAXFC%R(I1)=MAX(MAXFC%R(I1),MAXEL)
          MAXFC%R(I2)=MAX(MAXFC%R(I2),MAXEL)
          MAXFC%R(I3)=MAX(MAXFC%R(I3),MAXEL)
          MAXFC%R(I4)=MAX(MAXFC%R(I4),MAXEL)
        ENDDO
!
      ELSE
        WRITE(LU,*) 'ELEMENT ',IELM3,' NOT COMPUTED IN MURD3D'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IN PARALLEL MODE: GLOBAL EXTREMA
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(MAXFC,3,MESH3)
        CALL PARCOM(MINFC,4,MESH3)
      ENDIF
!
!     NEW COMPUTATION OF TRA03
!
      DO IPOIN=1,NPOIN3
        IF(TRA02(IPOIN).GT.0.D0) THEN
          TRA03(IPOIN)=VOLU2(IPOIN)-DTJ*TRA02(IPOIN)/
     &                 MAX(MAXFC%R(IPOIN)-FC(IPOIN),1.D-12)
        ELSE
          TRA03(IPOIN)=VOLU2(IPOIN)+DTJ*TRA02(IPOIN)/
     &                 MAX(FC(IPOIN)-MINFC%R(IPOIN),1.D-12)
        ENDIF
      ENDDO
!     POSITIVE SOURCES CHANGE THE MONOTONICITY CRITERION
      IF(NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
        ! SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*SOURCES%ADR(IS)%P%R(IPOIN)
!                                            WITH PARCOM
              ENDIF
            ENDDO
          ENDDO
        ELSE IF(OPTSOU.EQ.2) THEN
        ! SOURCE CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IF(ISCE(IS).GT.0) THEN
              IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
              IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*SOURCES%ADR(1)%P%R(IPOIN)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     POSITIVE BED FLUXES CHANGE THE MONOTONICITY CRITERION
!
      IF(BEDBOU) THEN
        ! STORE BEDFLU IN T2_18 AS IT NEEDS TO BE ASSEMBLED
        CALL CPSTVC(BEDFLU,T2_18)
        CALL OS('X=Y     ',X=T2_18,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_18,2,MESH2D)
        DO IPOIN=1,NPOIN2
          IF(T2_18%R(IPOIN).GT.0.D0) THEN
            TRA03(IPOIN)=TRA03(IPOIN)
     &                  -DTJ*T2_18%R(IPOIN)
!                                        WITH PARCOM
          ENDIF
        ENDDO
!         DO IPOIN=1,NPOIN2
!           IF(BEDFLU%R(IPOIN).GT.0.D0) THEN
!             TRA03(IPOIN)=TRA03(IPOIN)
!      &                  -DTJ*BEDFLU%R(IPOIN)
! !                                        WITH PARCOM
!           ENDIF
!         ENDDO
      ENDIF
!
!     POSITIVE BED FLUXES CHANGE THE MONOTONICITY CRITERION
!
      IF(BEDBOU) THEN
        ! STORE BEDFLU IN T2_18 AS IT NEEDS TO BE ASSEMBLED
        CALL CPSTVC(BEDFLU,T2_18)
        CALL OS('X=Y     ',X=T2_18,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_18,2,MESH2D)
        DO IPOIN=1,NPOIN2
          IF(T2_18%R(IPOIN).GT.0.D0) THEN
            TRA03(IPOIN)=TRA03(IPOIN)
     &                  -DTJ*T2_18%R(IPOIN)
!                                        WITH PARCOM
          ENDIF
        ENDDO
!         DO IPOIN=1,NPOIN2
!           IF(BEDFLU%R(IPOIN).GT.0.D0) THEN
!             TRA03(IPOIN)=TRA03(IPOIN)
!      &                  -DTJ*BEDFLU%R(IPOIN)
! !                                        WITH PARCOM
!           ENDIF
!         ENDDO
      ENDIF
!
      ELSEIF(OPT.EQ.1) THEN
!
      IF(SCHCF.EQ.ADV_PSI) THEN
        WRITE(LU,*) 'MURD3D: OPT=1 IS NOT ALLOWED WITH PSI-SCHEME'
        WRITE(LU,*) '        BECAUSE DIAGONAL DB IS NOT COMPUTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     TRA03 : COEFFICIENT OF FC(I), THAT MUST REMAIN POSITIVE FOR MONOTONICITY
      CALL OV('X=Y+CZ  ',TRA03, VOLU, DB, DTJ, NPOIN3)
!
!     POSITIVE SOURCES CHANGE THE MONOTONICITY CRITERION
!
      IF(NSCE.GT.0) THEN
        IF(OPTSOU.EQ.1) THEN
        ! SOURCE NOT CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                            WITHOUT PARCOM
              ENDIF
            ENDDO
          ENDDO
        ELSE IF(OPTSOU.EQ.2) THEN
        ! SOURCE CONSIDERED AS A DIRAC
          DO IS=1,NSCE
            IIS=1
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=2
            IF(ISCE(IS).GT.0) THEN
              IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
              IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                TRA03(IPOIN)=TRA03(IPOIN)
     &                      -DTJ*SOURCES%ADR(IIS)%P%R(IPOIN)
!                                              WITHOUT PARCOM
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     POSITIVE BED FLUXES CHANGE THE MONOTONICITY CRITERION
!
      IF(BEDBOU) THEN
!         DO IPOIN=1,NPOIN2
!           IF(BEDFLU%R(IPOIN).GT.0.D0) THEN
!             TRA03(IPOIN)=TRA03(IPOIN)
!      &                  -DTJ*BEDFLU%R(IPOIN)
! !                                        WITHOUT PARCOM
!           ENDIF
!         ENDDO
        ! STORE BEDFLU IN T2_18 AS IT NEEDS TO BE ASSEMBLED
        CALL CPSTVC(BEDFLU,T2_18)
        CALL OS('X=Y     ',X=T2_18,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_18,2,MESH2D)
        DO IPOIN=1,NPOIN2
          IF(T2_18%R(IPOIN).GT.0.D0) THEN
            TRA03(IPOIN)=TRA03(IPOIN)
     &                  -DTJ*T2_18%R(IPOIN)
!                                        WITHOUT PARCOM
          ENDIF
        ENDDO
      ENDIF
!
!     POSITIVE BED FLUXES CHANGE THE MONOTONICITY CRITERION
!
      IF(BEDBOU) THEN
!         DO IPOIN=1,NPOIN2
!           IF(BEDFLU%R(IPOIN).GT.0.D0) THEN
!             TRA03(IPOIN)=TRA03(IPOIN)
!      &                  -DTJ*BEDFLU%R(IPOIN)
! !                                        WITHOUT PARCOM
!           ENDIF
!         ENDDO
        ! STORE BEDFLU IN T2_18 AS IT NEEDS TO BE ASSEMBLED
        CALL CPSTVC(BEDFLU,T2_18)
        CALL OS('X=Y     ',X=T2_18,Y=BEDFLU)
        IF(NCSIZE.GT.1) CALL PARCOM(T2_18,2,MESH2D)
        DO IPOIN=1,NPOIN2
          IF(T2_18%R(IPOIN).GT.0.D0) THEN
            TRA03(IPOIN)=TRA03(IPOIN)
     &                  -DTJ*T2_18%R(IPOIN)
!                                        WITHOUT PARCOM
          ENDIF
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA03,2,MESH3)
!
      ELSE
        WRITE(LU,*) 'MURD3D: OPT=',OPT,' NOT IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IF MONOTONICITY IS NOT ENSURED, REDUCTION OF TIME-STEP BY FACTOR ALFA
!     THE MINIMUM ON ALL POINTS WILL BE TAKEN
!
      ALFA = 1.D0
      IGUILT=0
      IF(OPTBAN.EQ.2) THEN
        DO IPOIN = 1,NPOIN3
          IF(TRA03(IPOIN).LT.0.D0.AND.MASKPT(IPOIN).GT.0.5D0) THEN
            IF(ABS(TRA01(IPOIN)-TRA03(IPOIN)).GT.EPS) THEN
!             CONSIDERING THAT THE NEW TIME-STEP WILL BE ALFA*DTJ
!             VOLU WILL BE AT THAT TIME ALFA*VOLU(N+1)+(1-ALFA)*VOLU(IN TRA01)
!             MAXIMUM POSSIBLE ALFA IS SUCH THAT VOLU+DB*ALFA*DTJ=0
!             HENCE THE FOLLOWING FORMULA :
              ALFA2=TRA01(IPOIN)/(TRA01(IPOIN)-TRA03(IPOIN))
              IF(ALFA.GT.ALFA2) THEN
                ALFA = ALFA2
                IGUILT=IPOIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSE
        DO IPOIN = 1,NPOIN3
!                                          TIDAL FLATS : VOLUN=0
          IF(TRA03(IPOIN).LT.0.D0.AND.TRA01(IPOIN).GT.EPS) THEN
            IF(ABS(TRA01(IPOIN)-TRA03(IPOIN)).GT.EPS) THEN
!             CONSIDERING THAT THE NEW TIME-STEP WILL BE ALFA*DTJ
!             VOLU WILL BE AT THAT TIME ALFA*VOLU(N+1)+(1-ALFA)*VOLU(IN TRA01)
!             MAXIMUM POSSIBLE ALFA IS SUCH THAT VOLU+DB*ALFA*DTJ=0
!             HENCE THE FOLLOWING FORMULA :
              ALFA2=TRA01(IPOIN)/(TRA01(IPOIN)-TRA03(IPOIN))
              IF(ALFA.GT.ALFA2) THEN
                ALFA = ALFA2
                IGUILT=IPOIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!     SAVING THE LOCAL ALFA (IN CASE OF MAXIMUM ITERATION REACHED
!                            TO KNOW THE MOST GUILTY PROCESSOR)
      ALFALOC=ALFA
      IF(NCSIZE.GT.1) ALFA = P_DMIN(ALFA)
      DTJALFA=DTJ*ALFA
!
!     COMPUTES VOLU AFTER AN EXTRA ALFA*DTJ
!
      CALL OV('X=CX    ',TRA01,TRA01,TRA01,1.D0-ALFA,NPOIN3)
      CALL OV('X=X+CY  ',TRA01,VOLU2,VOLU2,     ALFA,NPOIN3)
!
      IF(CALFLU) THEN
        DO IPOIN = 1,NPOIN3
          FLUX = FLUX + DTJALFA*FC(IPOIN)*FLUEXT(IPOIN)
        ENDDO
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IF(OPTSOU.EQ.1) THEN
            ! SOURCE NOT CONSIDERED AS A DIRAC
              IIS=IS
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=IIS+NSCE
              DO IPOIN=1,NPOIN3
                IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                  FLUX=FLUX
     &                -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
                ELSE
                  FLUX=FLUX
     &                -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
                ENDIF
              ENDDO
            ELSE IF(OPTSOU.EQ.2) THEN
            ! SOURCE CONSIDERED AS A DIRAC
              IIS=1
!             HERE IN PARALLEL SOURCES WITHOUT PARCOM
!             ARE STORED AT ADRESSES 2 (SEE SOURCES_SINKS.F)
              IF(NCSIZE.GT.1) IIS=2
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(SOURCES%ADR(1)%P%R(IPOIN).GT.0.D0) THEN
                  FLUX=FLUX
     &                -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
                ELSE
                  FLUX=FLUX
     &                -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
!       FOR BED FLUXES
        IF(BEDBOU) THEN
          DO IPOIN=1,NPOIN2
!             IF(BEDFLU%R(IPOIN).LE.0.D0) THEN
!               FLUX=FLUX
!      &             -DTJALFA*FC(IPOIN)*BEDFLU%R(IPOIN)
!             ENDIF
            IF(T2_18%R(IPOIN).LE.0.D0) THEN
              FLUX=FLUX
     &             -DTJALFA*FC(IPOIN)*T2_18%R(IPOIN)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     ADVECTION DURING ALFA*DTJ
!
!     SOURCES (BUT WHEN INTAKE, FSCE=FC)
!
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          IF(OPTBAN.EQ.2) THEN
            IF(OPTSOU.EQ.1) THEN
            ! THE SOURCE IS NOT CONSIDERED AS A DIRAC
              DO IPOIN=1,NPOIN3
                IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
                    FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &              *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
                ENDIF
              ENDDO
            ELSE IF(OPTSOU.EQ.2) THEN
            ! THE SOURCE IS CONSIDERED AS A DIRAC
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
                  FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &            *MAX(SOURCES%ADR(1)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
                ENDIF
              ENDIF
            ENDIF
          ELSE
            IF(OPTSOU.EQ.1) THEN
            ! THE SOURCE IS NOT CONSIDERED AS A DIRAC
              IF(ISCE(IS).GT.0) THEN
                IPOIN=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                IF(TRA01(IPOIN).GT.EPS) THEN
                  FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &            *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
                ENDIF
              ENDIF
            ELSE IF(OPTSOU.EQ.2) THEN
            ! THE SOURCE IS CONSIDERED AS A DIRAC
              DO IPOIN=1,NPOIN3
                IF(TRA01(IPOIN).GT.EPS) THEN
                  FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &            *MAX(SOURCES%ADR(1)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!     BEDFLUXES
      IF(BEDBOU) THEN
!         IF(OPTBAN.EQ.2) THEN
!           DO IPOIN=1,NPOIN2
!             IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
!               FC(IPOIN)=FC(IPOIN)-DTJALFA*FC(IPOIN)
!      &          *MAX(BEDFLU%R(IPOIN),0.D0)/TRA01(IPOIN)
!             ENDIF
!           ENDDO
!         ELSE
!           DO IPOIN=1,NPOIN2
!             IF(TRA01(IPOIN).GT.EPS) THEN
!               FC(IPOIN)=FC(IPOIN)-DTJALFA*FC(IPOIN)
!      &        *MAX(BEDFLU%R(IPOIN),0.D0)/TRA01(IPOIN)
!             ENDIF
!           ENDDO
!         ENDIF
        IF(OPTBAN.EQ.2) THEN
          DO IPOIN=1,NPOIN2
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)-DTJALFA*FC(IPOIN)
     &          *MAX(T2_18%R(IPOIN),0.D0)/TRA01(IPOIN)
            ENDIF
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
            IF(TRA01(IPOIN).GT.EPS) THEN
              FC(IPOIN)=FC(IPOIN)-DTJALFA*FC(IPOIN)
     &        *MAX(T2_18%R(IPOIN),0.D0)/TRA01(IPOIN)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN (NOTE: SHOULD BE TAKEN INTO ACCOUNT IN STABILITY CRITERION)
!     VALUE OF TRACER IN RAIN TAKEN INTO ACCOUNT ONLY IF RAIN POSITIVE
!     NOT IN CASE OF EVAPORATION, HENCE THE MAX(PLUIE,0)
!
      IF(RAIN) THEN
        IF(OPTBAN.EQ.2) THEN
          DO IPOIN=1,NPOIN2
            IS=NPOIN3-NPOIN2+IPOIN
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IS).GT.EPS) THEN
              C=TRAIN*MAX(PLUIE(IPOIN),0.D0)-FC(IS)*PLUIE(IPOIN)
              FC(IS)=FC(IS)+DTJALFA*C/TRA01(IS)
            ENDIF
          ENDDO
        ELSE
          DO IPOIN=1,NPOIN2
            IS=NPOIN3-NPOIN2+IPOIN
            IF(TRA01(IS).GT.EPS) THEN
              C=TRAIN*MAX(PLUIE(IPOIN),0.D0)-FC(IS)*PLUIE(IPOIN)
              FC(IS)=FC(IS)+DTJALFA*C/TRA01(IS)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     FLUXES
!
      IF(S0F%TYPR.NE.'0') THEN
        DO IPOIN=1,NPOIN3
          IF(OPTBAN.EQ.2) THEN
          IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*
     &                     (S0F%R(IPOIN)+TRA02(IPOIN))/TRA01(IPOIN)
          ENDIF
          ELSE
          IF(TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*
     &                     (S0F%R(IPOIN)+TRA02(IPOIN))/TRA01(IPOIN)
          ENDIF
          ENDIF
        ENDDO
      ELSE
        IF(OPTBAN.EQ.2) THEN
        DO IPOIN=1,NPOIN3
          IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*TRA02(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
        ELSE
        DO IPOIN=1,NPOIN3
          IF(TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*TRA02(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
        ENDIF
      ENDIF
!
!     DTJ WAS THE REMAINING TIME, ALFA*DTJ HAS BEEN DONE, THE REST IS:
      DTJ = DTJ * (1.D0-ALFA)
      NITER = NITER + 1
      IF(NITER.GE.100.AND.ALFA.LT.1.D0) THEN
        WRITE(LU,*) 'MURD3D: ITERATION NO. REACHED ',NITER,', STOP.'
        IF(NCSIZE.GT.1) THEN
!         GLOBAL NUMBERING IF THERE IS A GUILTY POINT
          IF(IGUILT.GT.0) IGUILT=MESH3%KNOLG%I(IGUILT)
!         ONLY THE MORE GUILTY PROCESSOR IS KEPT
          IF(ALFALOC.NE.P_DMIN(ALFALOC)) IGUILT=0
!         RETRIEVING THE CORRESPONDING POINT
          IGUILT=P_IMAX(IGUILT)
        ENDIF
        WRITE(LU,*) 'ALFA = ',ALFA,' GUILTY POINT = ',IGUILT
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(ALFA.LT.1.D0) GOTO 10
!
!-----------------------------------------------------------------------
!
      IF(INFOR) THEN
        IF(LNG.EQ.1) WRITE(LU,101) SCHCF,NITER
        IF(LNG.EQ.2) WRITE(LU,102) SCHCF,NITER
      ENDIF
!
101   FORMAT(' MURD3D OPTION : ',1I2,'    ',1I4,' ITERATIONS')
102   FORMAT(' MURD3D OPTION: ',1I2,'    ',1I4,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
