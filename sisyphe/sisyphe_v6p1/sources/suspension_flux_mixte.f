!                    *********************************
                     SUBROUTINE SUSPENSION_FLUX_MIXTE!
!                    *********************************
!
     &  (TAUP,HN,ACLADM,CS,NPOIN,
     &   CHARR,XMVE,XMVS,GRAV,HMIN,XWC,
     &   ZERO,KARMAN,PARTHENIADES,FLUER_SABLE,FLUER_VASE,ZREF,
     &   AC,CSTAEQ,QSC,ICQ,DEBUG,AVAIL,NSICLA,ES,
     &   TOCE_VASE,NCOUCH_TASS,DT,TOCE_MIXTE,MS_SABLE,MS_VASE)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION.
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
!| AC             |---| 
!| ACLADM         |---| 
!| AVAIL          |---| 
!| CHARR          |---| 
!| CS             |---| 
!| CSTAEQ         |---| 
!| DEBUG          |---| 
!| DT             |---| 
!| ES             |---| 
!| FLUER_SABLE    |---| 
!| FLUER_VASE     |---| 
!| GRAV           |---| 
!| HMIN           |---| 
!| HN             |---| 
!| ICQ            |---| 
!| KARMAN         |---| 
!| MS_SABLE       |---| 
!| MS_VASE        |---| 
!| NCOUCH_TASS    |---| 
!| NPOIN          |---| 
!| NSICLA         |---| 
!| PARTHENIADES   |---| 
!| QSC            |---| 
!| TAUP           |---| 
!| TOCE_MIXTE     |---| 
!| TOCE_VASE      |---| 
!| XMVE           |---| 
!| XMVS           |---| 
!| XWC            |---| 
!| ZERO           |---| 
!| ZREF           |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_FLUX_MIXTE=>SUSPENSION_FLUX_MIXTE
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : FDM
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP,HN,ACLADM,CS
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG,NSICLA
      INTEGER,          INTENT(IN)    :: NCOUCH_TASS
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, GRAV, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, KARMAN, PARTHENIADES
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: AC,AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_SABLE,FLUER_VASE
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) ::TOCE_MIXTE(NPOIN,10)
!
      DOUBLE PRECISION, INTENT(IN)      :: DT
!
      TYPE(BIEF_OBJ),   INTENT(IN)       ::  QSC
      INTEGER,          INTENT (IN)      :: ICQ
!
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(10)
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER I, J,N
      DOUBLE PRECISION FLUERSABLE,FLUERVASE,FLUER_LOC(10)
!
      DOUBLE PRECISION QE_MOY,TOCE_SABLE,TEMPS,QER_VASE,QER_SABLE
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ******************************************** !
      ! I - COMPUTES THE CRITICAL SHEAR STRESS
      !    --> TOCE_SABLE
      ! ******************************************** !
!
! COMPUTES TOCE_SABLE VIA THE SHIELDS PARAMETER
         TOCE_SABLE= AC*(XMVS-XMVE)*GRAV*FDM(1)
!
      ! **************************************** !
!        II-COMPUTES EROSION
      ! **************************************** !
!---------DOES THE EROSION COMPUTATION ONLY ONCE (SAND FOR EXAMPLE
!         BECAUSE THE COMPUTED FLUX IS A GLOBAL FLUX COMMON TO THE 2 SEDIMENTS)
!---------COMPUTES THE THEORETICAL FLUX OF EROSION FOR EACH (SEDIMENT INFINITELY AVAILABLE IN EACH LAYER)
!
!---------COMPUTES THE CRITICAL STRESS FOR EACH LAYER AS A FUNCTION OF THE PROPORTION OF MUD
      DO J=1,NCOUCH_TASS
        DO I=1,NPOIN
          IF(AVAIL(I,J,2).LE.0.3D0)THEN
            TOCE_MIXTE(I,J)=TOCE_SABLE
          ELSEIF(AVAIL(I,J,2).GE.0.5D0)THEN
             TOCE_MIXTE(I,J)=TOCE_VASE(J)
          ELSE
                 TOCE_MIXTE(I,J)= TOCE_SABLE +
     &   (AVAIL(I,J,2)-0.3D0)*(TOCE_VASE(J)-TOCE_SABLE)/(0.5D0-0.3D0)
          ENDIF
        ENDDO
      ENDDO
! CV MODIFICATIONS: INTRODUCE TOCE IN ARGUMENT
!         AC(I) = TOCE_MIXTE(I,J)/((XMVS-XMVE)*GRAV*ACLADM%R(I))
!
        IF(ICQ.EQ.1) THEN
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FREDSOE'
!
           CALL SUSPENSION_FREDSOE(ACLADM,TAUP,NPOIN,
     &         GRAV,XMVE,XMVS,ZERO,AC,CSTAEQ)
!
          IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_FREDSOE'
!
        ELSEIF(ICQ.EQ.2) THEN
!
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BIJKER'
!
               CALL SUSPENSION_BIJKER(TAUP,HN,NPOIN,CHARR,QSC,ZREF,
     &                                ZERO,HMIN,CSTAEQ,XMVE)
!
          IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BIJKER'
!
        ENDIF
!
!      DO J=NCOUCH_TASS,1,-1
!        DO I=1,NPOIN
!           CSTAEQ_COUCHE(I,J)=CSTAEQ%R(I)
!        ENDDO
!      ENDDO
!
      DO I=1,NPOIN
!
        DO J=1,NCOUCH_TASS
!
!-----------COMPUTES FLUER_SABLE_VASE AS A FUNCTION OF THE PROPORTION OF MUD
!
          IF(AVAIL(I,J,2).LE.0.3D0)THEN
!-------------PROPORTION OF MUD < 30%, FLUXES ARE SIMILAR TO THOSE FOR SAND ONLY
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
                 FLUER_LOC(J)=CSTAEQ%R(I)*XWC
            ELSE
               FLUER_LOC(J)=0.D0
            ENDIF
!-------------PROPORTION OF MUD > 50%, FLUXES ARE SIMILAR TO THOSE FOR MUD ONLY
          ELSEIF(AVAIL(I,J,2).GE.0.5D0)THEN
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
               FLUER_LOC(J)=PARTHENIADES*
     &              ((TAUP%R(I)/TOCE_MIXTE(I,J))-1.D0)
            ELSE
               FLUER_LOC(J)=0.D0
            ENDIF
!-------------PROPORTION OF MUD >30% AND <50%, INTERPOLATES THE FLUXES
!             AND CRITICAL SHEAR STRESS
          ELSE
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
               FLUERSABLE=CSTAEQ%R(I)*XWC
               FLUERVASE=PARTHENIADES*
     &             ((TAUP%R(I)/TOCE_MIXTE(I,J))-1.D0)
            ELSE
               FLUERSABLE=0.D0
               FLUERVASE=0.D0
            ENDIF
               FLUER_LOC(J)=(AVAIL(I,J,2)-0.3D0)/
     &           (0.5D0-0.3D0)*(FLUERVASE-FLUERSABLE)+FLUERSABLE
          ENDIF
        ENDDO
!
! COMPUTES THE EROSION DEPTH ZER_MOY
! AND ERODED MASSES
          QER_VASE = 0.D0
          QER_SABLE = 0.D0
!
          TEMPS= DT
!
          DO J= 1, NCOUCH_TASS
           IF(ES(I,J).GE.1.D-6) THEN
!
! COMPUTES THE MASS POTENTIALLY ERODABLE IN LAYER J (KG/M2)
!
             QE_MOY= FLUER_LOC(J) *XMVS * TEMPS
!
             IF(QE_MOY.LT.(MS_SABLE(I,J)
     &            +MS_VASE(I,J))) THEN
!
                  QER_VASE = QER_VASE
     &                  + QE_MOY*MS_VASE(I,J)/
     &                      (MS_VASE(I,J)+MS_SABLE(I,J))
                  QER_SABLE = QER_SABLE
     &                    + QE_MOY*MS_SABLE(I,J)
     &                      /(MS_VASE(I,J)+MS_SABLE(I,J))
!V
                 GO TO 10
!
              ELSE
!
                  QER_VASE = QER_VASE + MS_VASE(I,J)
                  QER_SABLE = QER_SABLE + MS_SABLE(I,J)
                 TEMPS= TEMPS -
     &             (MS_SABLE(I,J)+MS_VASE(I,J))
     &                      /FLUER_LOC(J)/XMVS
              ENDIF
          ENDIF
!
         ENDDO
          WRITE(LU,*) 'ATTENTION TOUTES LES COUCHES SONT VIDES'
!          STOP
  10    CONTINUE
!
      ! ************************************************ !
      ! II-COMPUTES THE FLUX OF EROSION FOR SAND/MUD     !
      ! ************************************************ !
!
! Q_VASE REPRESENTS THE SURFACE MASS OF MUD TO BE ERODED TO REACH ZER_MOY
! Q_SABLE REPRESENTS THE SURFACE MASS OF SAND TO BE ERODED TO REACH ZER_MOY
!
        FLUER_VASE%R(I)  = QER_VASE /(DT*XMVS)
        FLUER_SABLE%R(I) = QER_SABLE/(DT*XMVS)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END