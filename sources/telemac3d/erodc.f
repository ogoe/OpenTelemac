!                    ****************
                     SUBROUTINE ERODC
!                    ****************
!
     &( CONC  , EPAI   , FLUER  , TOB    , DENSI  ,
     &  MPART , DT     , NPOIN2 , NCOUCH ,TOCE,HN,HMIN)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
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
!     
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, NCOUCH
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NPOIN2,NCOUCH), HN(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOCE(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2),DENSI(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: MPART, DT, HMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IC, IPOIN
      DOUBLE PRECISION   QS, TEMPS , QERODE
      INTRINSIC MIN , MAX
!
      DOUBLE PRECISION FLUER_LOC
!
!-----------------------------------------------------------------------
!
!
!CV UNIFORM BED
!
!      IF(NCOUCH.EQ.1) THEN
!
!        DO IPOIN=1,NPOIN2
!
!          FLUER(IPOIN) = 0.D0
!###>TBE
!          IF (HN(IPOIN)<HMIN) THEN
!             GOTO 10
!          ENDIF
!###<TBE
!
!         TESTS IF TOB > CRITICAL EROSION FRICTION OF SURFACE LAYER
!
!          IF ((TOB(IPOIN)-TOCE(IPOIN,1)).GE.1.D-8) THEN  
!
!            FLUER(IPOIN)=MPART*((TOB(IPOIN)/TOCE(IPOIN,1))-1.D0)
!            FLUER(IPOIN)=MIN(FLUER(IPOIN),
!     &               EPAI(IPOIN,1)*CONC(IPOIN,1)/DT)
!
!          ENDIF
!###>TBE
!10      CONTINUE
!###<TBE
!        ENDDO
!
!      ELSE
!CV
!     ---- TEMPS:TIME COUNTER FOR EROSION ----
!
        DO IPOIN=1,NPOIN2
!            
!###>TBE
        FLUER(IPOIN) = 0.D0
        IF(HN(IPOIN).LT.HMIN) GOTO 30
!###<TBE
!
! initialisation
           TEMPS=DT
           QERODE=0.D0
!
           DO IC=1, NCOUCH
!
            IF (TEMPS.LE.1.D-12) GO TO 20
!
!     ---- EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS ----

           IF (TOB(IPOIN).GT.TOCE(IPOIN,IC)) THEN
!
            FLUER_LOC=MPART*((TOB(IPOIN)/TOCE(IPOIN,IC))-1.D0)
            QS=CONC(IPOIN,IC)*EPAI(IPOIN,IC)
!CV ...
!    ---- LAYER THICKNESS AFTER EROSION ----
!
!            EPAI(IC,IPOIN)=MAX(0.D0,EPAI(IC,IPOIN)-
!     &                             (FLUER(IPOIN)*TEMPS/CONC(IC)))
!
! ...CV : done in fonvas
!
            QS=MIN(FLUER_LOC*TEMPS,CONC(IPOIN,IC)*EPAI(IPOIN,IC))
            QERODE=QERODE+QS
            TEMPS= TEMPS-(QS/FLUER_LOC)

           ENDIF
!
  
        ENDDO
!
20     CONTINUE
!
!     -----END OF THE EROSION STEP-----
!
       FLUER(IPOIN)=QERODE/DT
!###>TBE
30     CONTINUE
!###<TBE
  
      ENDDO
!     
!      ENDIF
!
      RETURN
      END
