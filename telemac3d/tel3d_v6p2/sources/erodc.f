!                    ****************
                     SUBROUTINE ERODC
!                    ****************
!
     &( CONC  , EPAI   , FLUER  , TOB    , DENSI  ,
     &  MPART , DT     , NPOIN2 , NCOUCH )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS EROSION
!+               (WITHIN MULTI-LAYER CONSOLIDATION MODEL).
!+
!+            THE USER PROVIDES THE LAW DEFINING THE CRITICAL
!+                EROSION VELOCITY AS A FUNCTION OF THE CONCENTRATION.
!+
!+            THE EROSION LAW CAN BE CHANGED BY THE USER
!+               (PARTHENIADES FORMULATION BY DEFAULT).
!
!history  C LE NORMANT (LNH)
!+        04/05/93
!+        V5P5
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONC           |-->| CONCENTRATION OF BED LAYER
!| DENSI          |-->| FLUID DENSITY
!| DT             |-->| TIME STEP
!| EPAI           |<->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ total bed thickness)
!| FLUER          |<->| EROSION  FLUX
!| MPART          |-->| EMPIRICAL COEFFICIENT (PARTHENIADES)
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| TOB            |-->| BOTTOM FRICTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, NCOUCH
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2),DENSI(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: MPART, DT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC, IPOIN
      DOUBLE PRECISION VITCE, TOCE , QS, TEMPS , QERODE
      INTRINSIC MIN , MAX
!
!-----------------------------------------------------------------------
!
      DO IPOIN=1,NPOIN2
!
!     ---- TEMPS:TIME COUNTER FOR EROSION ----
!
           TEMPS=DT
!
!     ---- QERODE: ERODED QUANTITY ----
!
           QERODE=0.D0
!
          DO IC=NCOUCH,1,-1
!
            IF (TEMPS.LE.1.D-6) GO TO 20
!
!     ---- EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS ----
!
!   *******    COMPUTES CRITICAL FRICTION VELOCITY
!              AS A FUNCTION OF CONCENTRATION
!             (EMPIRICAL RELATION : TO BE MODIFIED BY USER)
!
            IF (CONC(IC).LT.240.D0) THEN
              VITCE=3.2D-5*(CONC(IC)**1.175D0)
            ELSE
              VITCE=5.06D-8*(CONC(IC)**2.35D0)
            ENDIF
!
! THE 4 LINES ARE ERROR MESSAGES TO BE SUPPRESSED
!
         WRITE(LU,*)
         IF (LNG.EQ.1) WRITE(LU,11)
         IF (LNG.EQ.2) WRITE(LU,12)
         CALL PLANTE(1)
         STOP
!
11    FORMAT('SOUS PROGRAMME ERODC : DONNER LA VITESSE CRITIQUE',/,
     &       'D''EROSION FONCTION DE LA CONCENTRATION')
12    FORMAT('SUBROUTINE ERODC : EXPRESS THE CRITICAL SHEAR STRESS',/,
     &       'FUNCTION OF THE CONCENTRATION')
!
!    ---- CRITICAL BED SHEAR STRENGTH -----
!
            TOCE=DENSI(IPOIN)*VITCE**2
            IF (TOB(IPOIN).LE.TOCE) GOTO 20
!
!    ---- EROSION FLUX COMPUTATION ------
!
            FLUER(IPOIN)=MPART*((TOB(IPOIN)/TOCE)-1.D0)
!
!    ---- SOLID QUANTITY WITHIN THE LAYER BEFORE EROSION ----
!
            QS=CONC(IC)*EPAI(IC,IPOIN)
!
!    ---- LAYER THICKNESS AFTER EROSION ----
!
            EPAI(IC,IPOIN)=MAX(0.D0,EPAI(IC,IPOIN)-
     &                             (FLUER(IPOIN)*TEMPS/CONC(IC)))
!
!    ---- ERODED QUANTITY ----
!
            QERODE=QERODE+MIN(FLUER(IPOIN)*TEMPS,QS)
!
!    ---- TIME LEFT AFTER EROSION OF LAYER ----
!
            TEMPS=DMAX1(0.D0,TEMPS-(QS/FLUER(IPOIN)))
!
         ENDDO
!
20     CONTINUE
!
!     -----END OF THE EROSION STEP-----
!
       FLUER(IPOIN)=QERODE/DT
!
      END DO
!
      RETURN
      END SUBROUTINE ERODC
