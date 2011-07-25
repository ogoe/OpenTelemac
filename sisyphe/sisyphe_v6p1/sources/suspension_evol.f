!                    **************************
                     SUBROUTINE SUSPENSION_EVOL
!                    **************************
!
     &  (ZFCL_S,FLUDP,FLUER,DT, NPOIN,XMVS, QFLUX,MS_VASE,ES,
     &   CONC_VASE,NCOUCH_TASS)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE EVOLUTION FOR MUD ACCORDING TO FLUDP
!+                AND FLUER; AND UPDATES THE MASS OF THE LAYERS +
!+                EACH LAYER THICKNESS + TOTAL THICKNESS.
!
!note     COMPUTE ES AGAIN AT THE END AND
!+         VERIFY THE CRITERION ELAY=ZF-ZR
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONC_VASE      |-->|  INPUT CONCENTRATION OF EACH LAYER (IN KG/M3)
!| CSF            |-->|  SAND BED CONCENTRATION (NOT USED)
!| DT             |-->| TIME STEP
!| FLUDP          |<->| DEPOSITION FLUX
!| FLUER          |<->| EROSION FLUX
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NCOUCH_TASS    |-->| NUMBER OF VERTICAL BED LAYERS
!| NPOIN          |-->| NUMBER OF POINTS
!| QFLUX          |---| NET EROSION MINUS DEPOSITION RATE
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| XMVS           |-->| WATER DENSITY
!| ZFCL_S         |<->| BED EVOLUTION PER CLASS, DUE TO SUSPENDED SEDIMENT 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      ! 2/ GLOBAL VARIABLES
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,FLUDP,FLUER,QFLUX
      DOUBLE PRECISION, INTENT(IN)    :: DT, XMVS
      INTEGER, INTENT(IN) :: NPOIN,NCOUCH_TASS
      DOUBLE PRECISION, INTENT(IN) :: CONC_VASE(NCOUCH_TASS)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,NCOUCH_TASS)
      DOUBLE PRECISION,  INTENT(INOUT) :: ES(NPOIN,NCOUCH_TASS)
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I,J
!
      DOUBLE PRECISION ZERO, MER
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
       ZERO = 1.D-08
! 
! COMPUTES THE SEDIMENT FLUX DURING EACH TIMESTEP
!  QFLUX IS IN KG/M2 (MUD CONC ARE ALSO IN KG/M3) 
! 
           CALL OS('X=Y-Z   ', X=QFLUX, Y=FLUDP, Z=FLUER)
           CALL OS('X=CX    ', X=QFLUX, C=DT)
           CALL OS('X=CX    ', X=QFLUX, C=XMVS)
C
         IF(NCOUCH_TASS.EQ.1)  THEN
              CALL OS('X=CY    ', X=ZFCL_S,Y= QFLUX, 
     &             C=1.D0/CONC_VASE(1))
!
         ELSE
             DO I = 1, NPOIN
!
! DEPOSITION IN THE FIRST LAYER
!
             IF (QFLUX%R(I).GT.ZERO) THEN
                ZFCL_S%R(I) = QFLUX%R(I) / CONC_VASE(1)
                MS_VASE(I,1) = MS_VASE (I,1) +QFLUX%R(I)
!
              ELSEIF(QFLUX%R(I).LT.ZERO) THEN
!
! EROSION OF SUCCESSIVE LAYERS
!
!
                ZFCL_S%R(I) = 0.D0
                MER = - QFLUX%R(I)
!
                DO J = 1, NCOUCH_TASS
!
! CONC ARE IN KG/M3
!
                 IF (MER.LE.MS_VASE(I,J)) THEN
                   MS_VASE(I,J)= MS_VASE(I,J) - MER
                   ZFCL_S%R(I)= ZFCL_S%R(I) - MER/CONC_VASE(J)
                   ES(I,J)= MS_VASE(I,J)/CONC_VASE(J)
                   GO TO 40
!
                ELSE
!
! EROSION OF THE WHOLE LAYER
!
                   MER= MER - MS_VASE(I,J)
                   ZFCL_S%R(I)= ZFCL_S%R(I) -ES(I,J)
!     &                MS_VASE(I,J)/CONC_VASE(J)
                   MS_VASE(I,J)=0.D0
                   ES(I,J) = 0.D0
! 
               ENDIF
! END OF THE LOOP ON THE LAYERS
             ENDDO
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ATTENTION COUCHES VIDES: NOEUD I=',I
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'BEWARE, ALL LAYERS EMPTY, NODE I=',I
          ENDIF
!          CALL PLANTE(1)
!          STOP
! END EROSION
          ENDIF
!
  40      CONTINUE
!
! END OF THE LOOP ON THE NODES
!
        ENDDO
      ENDIF
!======================================================================!
!======================================================================!
!
      RETURN
      END
