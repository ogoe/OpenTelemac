!                    ****************
                     SUBROUTINE ERODC
!                    ****************
!
     &( CONC  , EPAI   , FLUER  , TOB    , DENSI  ,
     &  MPART , DT     , NPOIN2 , NCOUCH ,TOCE, HN, HMIN,
     &  MIXTE, EPAICO)
!
!***********************************************************************
! TELEMAC3D   V7P2
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!history  J.M. HERVOUET (EDF-LNHE)
!+        07/09/2015
!+        V7P1
!+   Optimisation and divisions by 0 more secured.
!
!history  J.M. HERVOUET (EDF-LNHE)
!+        30/05/2016
!+        V7P2
!+   Formulas TOB/MAX(TOCE,1.D-10)-1.D0 secured, they could be negative
!+   They are replaced by (TOB_TOCE)/MAX(TOCE,1.D_20)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONC           |-->| CONCENTRATION OF BED LAYER
!| DENSI          |-->| FLUID DENSITY
!| DT             |-->| TIME STEP
!| EPAI           |<->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ total bed thickness)
!| EPAICO         |-->| THICKNESS OF COHESIVE SUB-LAYER
!| FLUER          |<->| EROSION  FLUX
!| HN             |-->| DEPTH AT TIME N
!| HMIN           |-->| MINIMAL VALUE FOR DEPTH
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| MPART          |-->| EMPIRICAL COEFFICIENT (PARTHENIADES)
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| TOB            |-->| BOTTOM FRICTION
!| TOCE           |-->| CRITICAL EROSION SHEAR STRESS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_ERODC => ERODC
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN2,NCOUCH
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NPOIN2,NCOUCH), HN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOCE(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: EPAICO(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2),DENSI(NPOIN2)
      LOGICAL,          INTENT(IN)    :: MIXTE
      DOUBLE PRECISION, INTENT(IN)    :: MPART,DT,HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC,IPOIN
      DOUBLE PRECISION QS,TEMPS,QERODE,FLUER_LOC
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
      IF(MIXTE) THEN
!
        DO IPOIN=1,NPOIN2
          IF(HN(IPOIN).LT.HMIN) THEN
            QERODE=0.D0
          ELSEIF(TOB(IPOIN).GT.TOCE(IPOIN,1)) THEN
!           EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS
            FLUER_LOC=MPART*(TOB(IPOIN)-TOCE(IPOIN,1))/
     &                              MAX(TOCE(IPOIN,1),1.D-20)
!           LIMITING LARGE VALUES IN VIEW OF AVAILABLE SEDIMENT
            QERODE=MIN(FLUER_LOC*DT,CONC(IPOIN,1)*EPAICO(IPOIN))
          ELSE
            QERODE=0.D0
          ENDIF
          FLUER(IPOIN)=QERODE/DT
        ENDDO
!
      ELSE
!
        DO IPOIN=1,NPOIN2
!
          FLUER(IPOIN) = 0.D0
          IF(HN(IPOIN).LT.HMIN) CYCLE
!
!         TEMPS: TIME COUNTER FOR EROSION
          TEMPS=DT
          QERODE=0.D0
!
          DO IC=1, NCOUCH
            IF(TEMPS.LE.1.D-12) EXIT
!           EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS
            IF(TOB(IPOIN).GT.TOCE(IPOIN,IC)) THEN
              FLUER_LOC=MPART*(TOB(IPOIN)-TOCE(IPOIN,IC))/
     &                                MAX(TOCE(IPOIN,IC),1.D-20)
!CV ..        LAYER THICKNESS AFTER EROSION ----
!             EPAI(IC,IPOIN)=MAX(0.D0,EPAI(IC,IPOIN)-
!     &                             (FLUER(IPOIN)*TEMPS/CONC(IC)))
!CV :         done in fonvas
              QS=MIN(FLUER_LOC*TEMPS,CONC(IPOIN,IC)*EPAI(IPOIN,IC))
              QERODE=QERODE+QS
!             TEMPS= TEMPS-(QS/MAX(FLUER_LOC,1.D-10))
!             PROPOSITION JMH
              IF(QS.GT.1.D-20) THEN
                TEMPS=TEMPS*(1.D0-QS/(FLUER_LOC*TEMPS))
              ELSE
                EXIT
              ENDIF
            ENDIF
          ENDDO
!
!         END OF THE EROSION STEP
!
          FLUER(IPOIN)=QERODE/DT
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

