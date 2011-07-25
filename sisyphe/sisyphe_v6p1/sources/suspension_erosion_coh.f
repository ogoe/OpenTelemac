!                    *********************************
                     SUBROUTINE SUSPENSION_EROSION_COH
!                    *********************************
!
     &(TAUP,NPOIN,XMVE,XMVS,GRAV,VITCE,
     & PARTHENIADES,ZERO,DEBUG,
     & FLUER, ES, TOCE_VASE, NCOUCH_TASS, DT, MS_VASE,TASS)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION
!+                ACCOUNTING FOR THE VERTICAL STRUCTURE.
!+
!+            !! NEW SUBROUTINE !!
!
!history  C. VILLARET
!+        31/07/2008
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
!| DEBUG          |-->| DEBUG FLAG
!| DT             |-->| TIME STEP
!| ES             |<->| THICKNESS OF EACH LAYER (Not modified here)
!| FLUER          |<->| EROSION RATE
!| GRAV           |-->| GRAVITY ACCELERATION
!| MS_VASE        |<->| MASS OF MUD PER LAYER (not modified here)
!| NCOUCH_TASS    |-->| NUMBER OF LAYERS OF THE CONSOLIDATION MODEL
!| NPOIN          |-->| NUMBER OF POINTS
!| PARTHENIADES   |-->| PARTHENIADES CONSTANT (M/S)
!| TASS           |-->| A SUPPRIMER
!| TAUP           |-->| SKIN FRICTION
!| TOCE_VASE      |-->| CRITICAL BED SHEAR STRESS OF THE MUDPER LAYER
!| VITCE          |-->| A REMPLACER PAR SQRT(TOCE_VASE(1)/XMVS)
!| XMVE           |-->| DENSITY OF FLUID
!| XMVS           |-->| DENSITY OF SOLID
!| ZERO           |-->| ZERO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_SUSPENSION_EROSION_COH=>
     &                          SUSPENSION_EROSION_COH
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG
      DOUBLE PRECISION, INTENT(IN)    :: XMVE,XMVS,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: VITCE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,PARTHENIADES
! FOR CONSOLIDATION
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(10), DT
      INTEGER,          INTENT(IN)    :: NCOUCH_TASS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
!
      LOGICAL, INTENT(IN) :: TASS
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I, J
      DOUBLE PRECISION :: USTARP,AUX
      DOUBLE PRECISION :: FLUER_LOC(10), QER_VASE,TEMPS, QE_COUCHE
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! *************************************************  !
      ! IA - FORMULATION FOR COHESIVE SEDIMENTS            !
      !      (WITHOUT CONSOLIDATION: UNIFORM SEDIMENT BED) !                                   !
      ! ******************************************* *****  !
      IF(NCOUCH_TASS.EQ.1) THEN
        DO I = 1, NPOIN
          USTARP =SQRT(TAUP%R(I)/XMVE)
          IF(VITCE.GT.1.D-8) THEN
            AUX = MAX(((USTARP/VITCE)**2 - 1.D0),0.D0)
          ELSE
            AUX = 0.D0
          ENDIF
          FLUER%R(I) = PARTHENIADES*AUX
        ENDDO
      ELSE
      ! **************************************************** !
      ! IB - FORMULATION FOR COHESIVE SEDIMENTS  + CONSOLIDATION !
      !      (WITH BEDLOAD)                                  !
      ! **************************************************** !
!      BEWARE: HERE PARTHENIADES IS IN M/S 
        DO I=1,NPOIN
!
          DO J=1,NCOUCH_TASS
            IF(TAUP%R(I).GT.TOCE_VASE(J))THEN
              FLUER_LOC(J)=PARTHENIADES*
     &              ((TAUP%R(I)/MAX(TOCE_VASE(J),1.D-08))-1.D0)
            ELSE
              FLUER_LOC(J)=0.D0
            ENDIF
          ENDDO
          QER_VASE = 0.D0
          TEMPS= DT
!
          DO J= 1, NCOUCH_TASS
            IF(ES(I,J).GE.1.D-6) THEN
!             COMPUTES THE MASS POTENTIALLY ERODABLE IN LAYER J (KG/M2)
              QE_COUCHE = FLUER_LOC(J) *XMVS * TEMPS
              IF(QE_COUCHE.LT.MS_VASE(I,J)) THEN
                QER_VASE = QER_VASE  + QE_COUCHE
                GO TO 10
              ELSE
                QER_VASE = QER_VASE + MS_VASE(I,J)
                TEMPS= TEMPS-MS_VASE(I,J)/FLUER_LOC(J)/XMVS
                TEMPS=MAX(TEMPS,0.D0)
              ENDIF
            ENDIF
          ENDDO
!
!          IF(LNG.EQ.1) THEN
!            WRITE(LU,*) 'ATTENTION TOUTES LES COUCHES SONT VIDES'
!          ENDIF
!          IF(LNG.EQ.2) THEN
!            WRITE(LU,*) 'BEWARE, ALL LAYERS EMPTY'
!          ENDIF
!          CALL PLANTE(1)
!          STOP
10        CONTINUE
!   

          FLUER%R(I) = QER_VASE/DT/XMVS
!
        ENDDO
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
