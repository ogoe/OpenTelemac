!                    ****************
                     SUBROUTINE ERODE
!                    ****************
!
     &( IVIDE  , EPAI   ,
     &  HDEP   , FLUER  , TOB   , DENSI,
     &  NPOIN2 , NPFMAX , NPF   , MPART  ,
     &  TOCE   , CFDEP  , RHOS  , DT   , GIBSON)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS EROSION.
!+
!+            THE USER PROVIDES THE LAW DEFINING THE CRITICAL
!+                EROSION VELOCITY AS A FUNCTION OF THE CONCENTRATION.
!+
!+            THE EROSION LAW CAN BE CHANGED BY THE USER
!+               (PARTHENIADES FORMULATION BY DEFAULT).
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        V5P7
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
!| CFDEP          |-->| CONCENTRATION OF DEPOSIT
!| DENSI          |-->| FLUID DENSITY
!| DT             |-->| TIME STEP
!| EPAI           |<->| THICKNESS OF SOLID BED LAYER
!| FLUER          |<->| EROSION FLUX
!| GIBSON         |-->| LOGICAL FOR SETTLING MODEL
!|                |   | (GIBSON MODEL)
!| HDEP           |<->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |<->| VOID RATIO
!| MPART          |-->| EMPIRICAL COEFFICIENT PARTHENIADES
!| NPF            |<->| NUMBER OF POINTS WITHIN THE BED
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| RHOS           |-->| DENSITY OF  SEDIMENT
!| TOB            |-->| BOTTOM FRICTION
!| TOCE           |-->| CRITICAL  EROSION SHEAR STRENGTH (FRESH DEPOSIT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NPOIN2, NPFMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX, NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1, NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2), FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2),  DENSI(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: DT, RHOS, CFDEP
      DOUBLE PRECISION, INTENT(IN) :: MPART,TOCE
!
      INTEGER, INTENT(INOUT) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: GIBSON
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN, IPF, NERODE, NCOUCH
      DOUBLE PRECISION VITCE , TOCEC , QERODE , ECOUCH , TS , QS, TEMPS
      INTRINSIC MIN , MAX
!
!-----------------------------------------------------------------------
!     ---- START OF LOOP ON 2D MESH POINTS ----
!
! JAJ CLUMSY GOTO'S FROM A LOOP AND AN IF-STRUCTURE...
!
      DO IPOIN = 1, NPOIN2
!
!      ---- TEMPS: TIME COUNTER FOR EROSION ----
!
           TEMPS = DT
!
!      ---- QERODE : ERODED QUANTITY OF SEDIMENT ----
!
           QERODE = 0.D0
!
!      ---- NERODE : NUMBER OF ERODED LAYERS ----
!
          NERODE = 0
!
!      ---- EROSION OF FRESH DEPOSIT ----
!
        IF (HDEP(IPOIN).GE.1.D-10) THEN
!
!         TESTS IF TOB > CRITICAL EROSION FRICTION OF SURFACE LAYER
!
          IF ((TOB(IPOIN)-TOCE).LE.1.D-8) GOTO 20
!
          FLUER(IPOIN)=MPART*((TOB(IPOIN)/TOCE)-1.D0)
          QERODE=MIN(FLUER(IPOIN)*TEMPS,HDEP(IPOIN)*CFDEP)
!
!      ---- REACTUALISES DEPOSIT THICKNESS AFTER EROSION ----
!
          HDEP(IPOIN)=
     &     MAX(HDEP(IPOIN)-(FLUER(IPOIN)*TEMPS/CFDEP),0.D0)
!
!      ---- TIME LEFT TO ERODE UNDERLAYERS ----
!
          TEMPS=TEMPS-QERODE/FLUER(IPOIN)
!
        ENDIF
!
!      ---- EROSION OF MUD BED ----
!
         IF (GIBSON) THEN
          DO IPF=2,NPF(IPOIN)
!
!      ---- NCOUCH: NUMBER OF SUPERFICIAL LAYER ----
!
!         BUG CORRECTED 29/06/2006 AFTER WARNING BY CHC-NRC (THANKS)
!         NCOUCH=-IPF+NPF(NPOIN2)+1
          NCOUCH=-IPF+NPF(IPOIN)+1
!
            IF (TEMPS.LE.1.D-8) GOTO 20
!
!      ---- CONCENTRATION OF SUPERFICIAL LAYER ----
!
            ECOUCH=(IVIDE(NCOUCH,IPOIN)+IVIDE(NCOUCH+1,IPOIN))/2.D0
            TS=RHOS/(1.D0+ECOUCH)
!
!      ---- CRITICAL FRICTION VELOCITY AS A FUNCTION OF CONCENTRATION ----
!
!   (EMPIRICAL RELATION, LOIRE ESTUARY, FRITSCH ET AL. 1989 )
            IF(TS.LT.240.D0) THEN
              VITCE=3.2D-5*(TS**1.175D0)
            ELSE
              VITCE=5.06D-8*(TS**2.35D0)
            ENDIF
!
! SUPPRESS 4 FOLLOWING LINES TO ACTIVATE THE SUBROUTINE
!
         WRITE(LU,*)
         IF (LNG.EQ.1) WRITE(LU,11)
         IF (LNG.EQ.2) WRITE(LU,12)
         CALL PLANTE(1)
         STOP
!
11    FORMAT('SOUS-PROGRAMME ERODE : DONNER LA VITESSE CRITIQUE',/,
     &       'D''EROSION DU LIT CONSOLIDE FONCTION DE LA CONCENTRATION')
12    FORMAT('SUBROUTINE ERODE : EXPRESS THE CRITICAL SHEAR STRESS FOR',
     & /, 'EROSION (CONSOLIDATED BED) FUNCTION OF THE CONCENTRATION')
!
!-----------------------------------------------------------------------
!
!        ---- COMPUTES THE CRITICAL BED SHEAR STRENGTH FOR EROSION
!                          FOR CONSOLIDATED BED , TOCEC
            TOCEC=DENSI(IPOIN)*VITCE**2
!
!        ---- EROSION FLUX
!
            FLUER(IPOIN)=MPART*MAX((TOB(IPOIN)/TOCEC)-1.D0,0.D0)
!
!        ---- MAXIMAL AMOUNT OF SEDIMENT AVAILABLE FOR EROSION
!
            QS=RHOS*EPAI(NCOUCH,IPOIN)
!
            IF ((FLUER(IPOIN)*TEMPS).GE.QS) THEN
!
!        ---- EROSION OF THE WHOLE BED LAYER
!
             EPAI(NCOUCH,IPOIN)=0.D0
!
!        ---- ERODED QUANTITY
!
             QERODE=QERODE+QS
!
!        ---- TIME LEFT AFTER EROSION OF LAYER
!
             TEMPS=TEMPS-(QS/FLUER(IPOIN))
!
!        ---- NUMBER OF ERODED LAYERS
!
             NERODE=NERODE+1
!
            ELSE
!
!        ---- PARTIAL EROSION OF BED LAYER
!
             EPAI(NCOUCH,IPOIN)=EPAI(NCOUCH,IPOIN)-
     &                                        (FLUER(IPOIN)*TEMPS/RHOS)
!
!        ---- ERODED QUANTITY
!
             QERODE=QERODE+(FLUER(IPOIN)*TEMPS)
!
             GOTO 20
!
            ENDIF
           ENDDO
         ENDIF
!
20     CONTINUE
! GOTO TARGET
!
!     ----- END OF EROSION STEP -----
!
        FLUER(IPOIN)=QERODE/DT
        NPF(IPOIN)=NPF(IPOIN)-NERODE
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ERODE
