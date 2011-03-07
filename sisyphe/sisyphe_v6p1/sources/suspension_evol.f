!                    **************************
                     SUBROUTINE SUSPENSION_EVOL
!                    **************************
!
     &  (ZFCL_S,FLUDP,FLUER,DT, NPOIN,CSF,XMVS, QFLUX,MS,
     &   SEDCO,CONC_VASE,NCOUCH_TASS)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CONC_VASE      |---| 
!| CSF            |---| 
!| DT             |---| 
!| FLUDP          |---| 
!| FLUER          |---| 
!| MS             |---| 
!| NCOUCH_TASS    |---| 
!| NPOIN          |---| 
!| QFLUX          |---| 
!| SEDCO          |---| 
!| XMVS           |---| 
!| ZFCL_S         |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      ! 2/ GLOBAL VARIABLES
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,FLUDP,FLUER,QFLUX
      DOUBLE PRECISION, INTENT(IN)    :: DT, XMVS, CSF
      INTEGER, INTENT(IN) :: NPOIN,NCOUCH_TASS
      LOGICAL, INTENT(IN) :: SEDCO
      DOUBLE PRECISION, INTENT(IN) :: CONC_VASE(NCOUCH_TASS)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS(NPOIN,NCOUCH_TASS)
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I,J
!
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION CONC,MER
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
       ZERO = 1.D-08
!
! COMPUTES THE SEDIMENT FLUX AT EACH TIMESTEP
!
           CALL OS('X=Y-Z   ', X=QFLUX, Y=FLUDP, Z=FLUER)
           CALL OS('X=CX    ', X=QFLUX, C=DT)
           IF(NCOUCH_TASS.EQ.1)   CALL OS('X=CY    ',
     &         X=ZFCL_S, Y= QFLUX, C=1.D0/CSF)
           IF(NCOUCH_TASS.GT.1) THEN
             DO I = 1, NPOIN
!
! DEPOSITION IN THE FIRST LAYER
!
             IF (QFLUX%R(I).GT.ZERO) THEN
                ZFCL_S%R(I) = QFLUX%R(I) / CSF
                MS(I,1) = MS (I,1) +QFLUX%R(I)*XMVS
!
              ELSEIF(QFLUX%R(I).LT.ZERO) THEN
!
! EROSION OF SUCCESSIVE LAYERS
!
!
                ZFCL_S%R(I) = 0.D0
                MER = - QFLUX%R(I) *XMVS
!
                DO J = 1, NCOUCH_TASS
!
                 IF(.NOT.SEDCO) CONC= XMVS * CSF
                 IF(SEDCO) CONC=XMVS*CONC_VASE(J)
!
                 IF (MER.LE.MS(I,J)) THEN
                   MS(I,J)= MS(I,J) - MER
                   ZFCL_S%R(I)= ZFCL_S%R(I) - MER/CONC
                   GO TO 40
!
                ELSE
!
! EROSION OF THE WHOLE UNDER-LAYER
!
                   MER= MER - MS(I,J)
                   ZFCL_S%R(I)= ZFCL_S%R(I) -
     &                MS(I,J)/CONC
                   MS(I,J)=0.D0
!
               ENDIF
! END OF THE LOOP ON THE LAYERS
             ENDDO
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