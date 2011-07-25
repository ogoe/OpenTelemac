!                    *****************
                     SUBROUTINE MURD3D
!                    *****************
!
     &(FC,FN,VOLU,VOLUN,VOLU2,SVOLU2,DA,XA,DB,XB,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,W1,IKLE3,MESH3,
     & NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL,INFOR,CALFLU,FLUX,FLUEXT,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,NPOIN2,MINFC,MAXFC,MASKPT,
     & OPTBAN,FLODEL,FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
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
!+        16/06/05
!+
!+   SOURCES MODIFIED FOR INTAKES
!
!history
!+        10/11/05
!+
!+   SOURCES TAKEN INTO ACCOUNT IN MONOTONICITY CRITERION
!
!history
!+        12/06/07
!+
!+   SOURCES IN PARALLEL LOOK AT THE USE OF IIS
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
!history
!+        19/12/07
!+
!+   CHANGED MONOTONICITY CRITERION
!
!history
!+        29/07/08
!+
!+   TIDAL FLATS WITH OPTBAN=2
!
!history
!+        04/08/08
!+
!+   DIMENSIONS OF XA AND XB INVERTED (SEE ALSO MT14PP)
!
!history
!+        22/06/09
!+
!+   FINITE VOLUME SCHEME ADDED
!
!history
!+        11/08/09
!+
!+   FINITE VOLUME SCHEME OPTIMISED
!
!history  J.M. JANIN  (LNH)
!+
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
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| DA             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!| DB             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!|                |   | POSSIBLY TRANSFORMED IN OPTION PSI
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| DT             |-->| TIME STEP
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FLODEL         |-->| FLUX BY MESH EDGES
!| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUX           |<->| FLUXES TO BE CHANGED
!| FN             |-->| VARIABLE AT TIME N
!| FSCE           |-->| SOURCE
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
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
!| VOLU           |-->| CONTROL VOLUME AT TIME N+1
!| VOLU2          |<->| LIKE VOLU, BUT ASSEMBLED IN PARALLEL
!| VOLUN          |-->| CONTROL VOLUME AT TIME N
!| W1             |<->| WORK ARRAY (CALCULATION OF MATRICES...)
!| XA             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!| XB             |<->| NOT SYMMETRIC MURD MATRIX OPTION N
!|                |   | POSSIBLY TRANSFORMED IN OPTION PSI
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,LV,NPOIN2
      INTEGER, INTENT(IN)             :: IKLE3(NELEM3,6),NSCE,OPTBAN
      INTEGER, INTENT(IN)             :: NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),PLUIE(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEM3,6)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3), FLUEXT(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3), VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3), TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX
      DOUBLE PRECISION, INTENT(IN)    :: DT,FSCE(NSCE),MASKPT(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2,MINFC,MAXFC
      TYPE(BIEF_OBJ), INTENT(IN)      :: SOURCES,S0F
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: STRA01,STRA02,STRA03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3
      DOUBLE PRECISION, INTENT(INOUT) :: DA(NPOIN3),XA(30,NELEM3)
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN3),XB(30,NELEM3)
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
!
      INTEGER IELEM,IPOIN,NITER,IS,IGUILT,IIS
      INTEGER I1,I2,I3,I4,I5,I6,OPT,ISEG3D,NSEGH,NSEGV
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION P_DMIN,P_DSUM
      EXTERNAL P_DMIN,P_DSUM
!
      DOUBLE PRECISION EPS
      DATA EPS /1.D-6/
!     DATA EPS /10.D0/
!
      INTEGER TOTITER
      DATA TOTITER /0/
!
!***********************************************************************
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
           DB(IPOIN)=0.D0
         ENDDO
!
         DO 20 IELEM = 1,NELEM3
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
            M12 = (XA(01,IELEM)-XA(16,IELEM)) * (T1-T2)
            M13 = (XA(02,IELEM)-XA(17,IELEM)) * (T1-T3)
            M14 = (XA(03,IELEM)-XA(18,IELEM)) * (T1-T4)
            M15 = (XA(04,IELEM)-XA(19,IELEM)) * (T1-T5)
            M16 = (XA(05,IELEM)-XA(20,IELEM)) * (T1-T6)
            M23 = (XA(06,IELEM)-XA(21,IELEM)) * (T2-T3)
            M24 = (XA(07,IELEM)-XA(22,IELEM)) * (T2-T4)
            M25 = (XA(08,IELEM)-XA(23,IELEM)) * (T2-T5)
            M26 = (XA(09,IELEM)-XA(24,IELEM)) * (T2-T6)
            M34 = (XA(10,IELEM)-XA(25,IELEM)) * (T3-T4)
            M35 = (XA(11,IELEM)-XA(26,IELEM)) * (T3-T5)
            M36 = (XA(12,IELEM)-XA(27,IELEM)) * (T3-T6)
            M45 = (XA(13,IELEM)-XA(28,IELEM)) * (T4-T5)
            M46 = (XA(14,IELEM)-XA(29,IELEM)) * (T4-T6)
            M56 = (XA(15,IELEM)-XA(30,IELEM)) * (T5-T6)
!
            PHIP = MAX( M12,0.D0) + MAX( M13,0.D0) + MAX( M14,0.D0)
     &           + MAX( M15,0.D0) + MAX( M16,0.D0) + MAX( M23,0.D0)
     &           + MAX( M24,0.D0) + MAX( M25,0.D0) + MAX( M26,0.D0)
     &           + MAX( M34,0.D0) + MAX( M35,0.D0) + MAX( M36,0.D0)
     &           + MAX( M45,0.D0) + MAX( M46,0.D0) + MAX( M56,0.D0)
            PHIM = MAX(-M12,0.D0) + MAX(-M13,0.D0) + MAX(-M14,0.D0)
     &           + MAX(-M15,0.D0) + MAX(-M16,0.D0) + MAX(-M23,0.D0)
     &           + MAX(-M24,0.D0) + MAX(-M25,0.D0) + MAX(-M26,0.D0)
     &           + MAX(-M34,0.D0) + MAX(-M35,0.D0) + MAX(-M36,0.D0)
     &           + MAX(-M45,0.D0) + MAX(-M46,0.D0) + MAX(-M56,0.D0)
!
            IF(PHIP.GE.PHIM) THEN
               ALFA = (PHIP - PHIM) / MAX(PHIP,1.D-10)
               IF(T2.GT.T1) THEN
                 XB(01,IELEM) = 0.D0
                 XB(16,IELEM) = XA(16,IELEM) * ALFA
               ELSE
                 XB(01,IELEM) = XA(01,IELEM) * ALFA
                 XB(16,IELEM) = 0.D0
               ENDIF
               IF(T3.GT.T1) THEN
                 XB(02,IELEM) = 0.D0
                 XB(17,IELEM) = XA(17,IELEM) * ALFA
               ELSE
                 XB(02,IELEM) = XA(02,IELEM) * ALFA
                 XB(17,IELEM) = 0.D0
               ENDIF
               IF(T4.GT.T1) THEN
                 XB(03,IELEM) = 0.D0
                 XB(18,IELEM) = XA(18,IELEM) * ALFA
               ELSE
                 XB(03,IELEM) = XA(03,IELEM) * ALFA
                 XB(18,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T1) THEN
                 XB(04,IELEM) = 0.D0
                 XB(19,IELEM) = XA(19,IELEM) * ALFA
               ELSE
                 XB(04,IELEM) = XA(04,IELEM) * ALFA
                 XB(19,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T1) THEN
                 XB(05,IELEM) = 0.D0
                 XB(20,IELEM) = XA(20,IELEM) * ALFA
               ELSE
                 XB(05,IELEM) = XA(05,IELEM) * ALFA
                 XB(20,IELEM) = 0.D0
               ENDIF
               IF(T3.GT.T2) THEN
                 XB(06,IELEM) = 0.D0
                 XB(21,IELEM) = XA(21,IELEM) * ALFA
               ELSE
                 XB(06,IELEM) = XA(06,IELEM) * ALFA
                 XB(21,IELEM) = 0.D0
               ENDIF
               IF(T4.GT.T2) THEN
                 XB(07,IELEM) = 0.D0
                 XB(22,IELEM) = XA(22,IELEM) * ALFA
               ELSE
                 XB(07,IELEM) = XA(07,IELEM) * ALFA
                 XB(22,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T2) THEN
                 XB(08,IELEM) = 0.D0
                 XB(23,IELEM) = XA(23,IELEM) * ALFA
               ELSE
                 XB(08,IELEM) = XA(08,IELEM) * ALFA
                 XB(23,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T2) THEN
                 XB(09,IELEM) = 0.D0
                 XB(24,IELEM) = XA(24,IELEM) * ALFA
               ELSE
                 XB(09,IELEM) = XA(09,IELEM) * ALFA
                 XB(24,IELEM) = 0.D0
               ENDIF
               IF(T4.GT.T3) THEN
                 XB(10,IELEM) = 0.D0
                 XB(25,IELEM) = XA(25,IELEM) * ALFA
               ELSE
                 XB(10,IELEM) = XA(10,IELEM) * ALFA
                 XB(25,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T3) THEN
                 XB(11,IELEM) = 0.D0
                 XB(26,IELEM) = XA(26,IELEM) * ALFA
               ELSE
                 XB(11,IELEM) = XA(11,IELEM) * ALFA
                 XB(26,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T3) THEN
                 XB(12,IELEM) = 0.D0
                 XB(27,IELEM) = XA(27,IELEM) * ALFA
               ELSE
                 XB(12,IELEM) = XA(12,IELEM) * ALFA
                 XB(27,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T4) THEN
                 XB(13,IELEM) = 0.D0
                 XB(28,IELEM) = XA(28,IELEM) * ALFA
               ELSE
                 XB(13,IELEM) = XA(13,IELEM) * ALFA
                 XB(28,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T4) THEN
                 XB(14,IELEM) = 0.D0
                 XB(29,IELEM) = XA(29,IELEM) * ALFA
               ELSE
                 XB(14,IELEM) = XA(14,IELEM) * ALFA
                 XB(29,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T5) THEN
                 XB(15,IELEM) = 0.D0
                 XB(30,IELEM) = XA(30,IELEM) * ALFA
               ELSE
                 XB(15,IELEM) = XA(15,IELEM) * ALFA
                 XB(30,IELEM) = 0.D0
               ENDIF
            ELSE
               ALFA = (PHIM - PHIP) / MAX(PHIM,1.D-10)
               IF(T2.GT.T1) THEN
                 XB(01,IELEM) = XA(01,IELEM) * ALFA
                 XB(16,IELEM) = 0.D0
               ELSE
                 XB(01,IELEM) = 0.D0
                 XB(16,IELEM) = XA(16,IELEM) * ALFA
               ENDIF
               IF(T3.GT.T1) THEN
                 XB(02,IELEM) = XA(02,IELEM) * ALFA
                 XB(17,IELEM) = 0.D0
               ELSE
                 XB(02,IELEM) = 0.D0
                 XB(17,IELEM) = XA(17,IELEM) * ALFA
               ENDIF
               IF(T4.GT.T1) THEN
                 XB(03,IELEM) = XA(03,IELEM) * ALFA
                 XB(18,IELEM) = 0.D0
               ELSE
                 XB(03,IELEM) = 0.D0
                 XB(18,IELEM) = XA(18,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T1) THEN
                 XB(04,IELEM) = XA(04,IELEM) * ALFA
                 XB(19,IELEM) = 0.D0
               ELSE
                 XB(04,IELEM) = 0.D0
                 XB(19,IELEM) = XA(19,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T1) THEN
                 XB(05,IELEM) = XA(05,IELEM) * ALFA
                 XB(20,IELEM) = 0.D0
               ELSE
                 XB(05,IELEM) = 0.D0
                 XB(20,IELEM) = XA(20,IELEM) * ALFA
               ENDIF
               IF(T3.GT.T2) THEN
                 XB(06,IELEM) = XA(06,IELEM) * ALFA
                 XB(21,IELEM) = 0.D0
               ELSE
                 XB(06,IELEM) = 0.D0
                 XB(21,IELEM) = XA(21,IELEM) * ALFA
               ENDIF
               IF(T4.GT.T2) THEN
                 XB(07,IELEM) = XA(07,IELEM) * ALFA
                 XB(22,IELEM) = 0.D0
               ELSE
                 XB(07,IELEM) = 0.D0
                 XB(22,IELEM) = XA(22,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T2) THEN
                 XB(08,IELEM) = XA(08,IELEM) * ALFA
                 XB(23,IELEM) = 0.D0
               ELSE
                 XB(08,IELEM) = 0.D0
                 XB(23,IELEM) = XA(23,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T2) THEN
                 XB(09,IELEM) = XA(09,IELEM) * ALFA
                 XB(24,IELEM) = 0.D0
               ELSE
                 XB(09,IELEM) = 0.D0
                 XB(24,IELEM) = XA(24,IELEM) * ALFA
               ENDIF
               IF(T4.GT.T3) THEN
                 XB(10,IELEM) = XA(10,IELEM) * ALFA
                 XB(25,IELEM) = 0.D0
               ELSE
                 XB(10,IELEM) = 0.D0
                 XB(25,IELEM) = XA(25,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T3) THEN
                 XB(11,IELEM) = XA(11,IELEM) * ALFA
                 XB(26,IELEM) = 0.D0
               ELSE
                 XB(11,IELEM) = 0.D0
                 XB(26,IELEM) = XA(26,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T3) THEN
                 XB(12,IELEM) = XA(12,IELEM) * ALFA
                 XB(27,IELEM) = 0.D0
               ELSE
                 XB(12,IELEM) = 0.D0
                 XB(27,IELEM) = XA(27,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T4) THEN
                 XB(13,IELEM) = XA(13,IELEM) * ALFA
                 XB(28,IELEM) = 0.D0
               ELSE
                 XB(13,IELEM) = 0.D0
                 XB(28,IELEM) = XA(28,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T4) THEN
                 XB(14,IELEM) = XA(14,IELEM) * ALFA
                 XB(29,IELEM) = 0.D0
               ELSE
                 XB(14,IELEM) = 0.D0
                 XB(29,IELEM) = XA(29,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T5) THEN
                 XB(15,IELEM) = XA(15,IELEM) * ALFA
                 XB(30,IELEM) = 0.D0
               ELSE
                 XB(15,IELEM) = 0.D0
                 XB(30,IELEM) = XA(30,IELEM) * ALFA
               ENDIF
            ENDIF
!
            DB(I1)=DB(I1) - XB(01,IELEM) - XB(02,IELEM)
     &                    - XB(03,IELEM)
     &                    - XB(04,IELEM) - XB(05,IELEM)
            DB(I2)=DB(I2) - XB(16,IELEM) - XB(06,IELEM)
     &                    - XB(07,IELEM)
     &                    - XB(08,IELEM) - XB(09,IELEM)
            DB(I3)=DB(I3) - XB(17,IELEM) - XB(21,IELEM)
     &                    - XB(10,IELEM)
     &                    - XB(11,IELEM) - XB(12,IELEM)
            DB(I4)=DB(I4) - XB(18,IELEM) - XB(22,IELEM)
     &                    - XB(25,IELEM)
     &                    - XB(13,IELEM) - XB(14,IELEM)
            DB(I5)=DB(I5) - XB(19,IELEM) - XB(23,IELEM)
     &                    - XB(26,IELEM)
     &                    - XB(28,IELEM) - XB(15,IELEM)
            DB(I6)=DB(I6) - XB(20,IELEM) - XB(24,IELEM)
     &                    - XB(27,IELEM)
     &                    - XB(29,IELEM) - XB(30,IELEM)
!
20       CONTINUE
!
!        DB IS : - SUM ON J OF LAMBDA(I,J)
!
!        CALL ASSVEC
!    &   (DB,IKLE3,NPOIN3,NELEM3,NELEM3,41,W1,.TRUE.,LV,MSK,MASKEL)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI) THEN
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
!     END OF THE REPLACEMENT OF CALL MV0606
!
      ENDIF
!
      IF(SCHCF.EQ.ADV_LPO) THEN
!
        NSEGH=NSEG*NPLAN
        NSEGV=(NPLAN-1)*NPOIN2
!
!       COMPUTES DB AND TRA02 IN UPWIND EXPLICIT FINITE VOLUMES
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
      DO IPOIN=1,NPOIN3
        MINFC%R(IPOIN)=FC(IPOIN)
        MAXFC%R(IPOIN)=FC(IPOIN)
      ENDDO
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
        DO IS=1,NSCE
          DO IPOIN=1,NPOIN3
            IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
              TRA03(IPOIN)=TRA03(IPOIN)
     &                    -DTJ*SOURCES%ADR(IS)%P%R(IPOIN)
!                                          WITH PARCOM
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      ELSEIF(OPT.EQ.1) THEN
!
!     TRA03 : COEFFICIENT OF FC(I), THAT MUST REMAIN POSITIVE FOR MONOTONICITY
      CALL OV('X=Y+CZ  ',TRA03, VOLU, DB, DTJ, NPOIN3)
!
!     POSITIVE SOURCES CHANGE THE MONOTONICITY CRITERION
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          DO IPOIN=1,NPOIN3
            IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
              TRA03(IPOIN)=TRA03(IPOIN)
     &                    -DTJ*SOURCES%ADR(IS+NSCE)%P%R(IPOIN)
!                                          WITHOUT PARCOM
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA03,2,MESH3)
!
      ENDIF
!
!     IF MONOTONICITY IS NOT ENSURED, REDUCTION OF TIME-STEP BY FACTOR ALFA
!     THE MINIMUM ON ALL POINTS WILL BE TAKEN
!
      ALFA = 1.D0
      IGUILT=1
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
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                FLUX=FLUX
     &              -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ELSE
                FLUX=FLUX
     &              -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!     ADVECTION DURING ALFA*DTJ
!
!     SOURCES (BUT WHEN INTAKE, FSCE=FC)
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          IF(OPTBAN.EQ.2) THEN
            DO IPOIN=1,NPOIN3
              IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
                FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &          *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
              ENDIF
            ENDDO
          ELSE
            DO IPOIN=1,NPOIN3
              IF(TRA01(IPOIN).GT.EPS) THEN
                FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &          *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!     RAIN
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IS=NPOIN3-NPOIN2+IPOIN
          IF(OPTBAN.EQ.2) THEN
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IS).GT.EPS) THEN
              FC(IS)=FC(IS)-DTJALFA*FC(IS)*PLUIE(IPOIN)/TRA01(IS)
            ENDIF
          ELSE
            IF(TRA01(IS).GT.EPS) THEN
              FC(IS)=FC(IS)-DTJALFA*FC(IS)*PLUIE(IPOIN)/TRA01(IS)
            ENDIF
          ENDIF
        ENDDO
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
      TOTITER = TOTITER + 1
      IF(NITER.GE.100) THEN
        WRITE (LU,*) 'MURD3D: ITERATION NO. REACHED ',NITER,', STOP.'
        WRITE (LU,*) 'ALFA = ',ALFA
        WRITE (LU,*) 'GUILTY POINT = ',IGUILT
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
