        SUBROUTINE SUSPENSION_FLUX_MIXTE!
!
     &  (TAUP,HN,FDM,NPOIN,
     &   CHARR,XMVE,XMVS,VCE,GRAV,HMIN,XWC,
     &   ZERO,PARTHENIADES,FLUER_SABLE,FLUER_VASE,ZREF,
     &   AC,CSTAEQ,QSC,ICQ,DEBUG,AVAIL,NSICLA,ES,
     &   TOCE_VASE,TOCE_SABLE,
     &   NCOUCH_TASS,DT,TOCE_MIXTE,MS_SABLE,MS_VASE)
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION.
! 
!history C. VILLARET + JMH 2008
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
!history C. VILLARET 20/03/2011
!   Change of arguments FDM insteam of ACLADM
!   KARMAN suppressed
!   Added TOCE _ SABLE + VCE 
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CHARR          |-->| BEDLOAD
!| CS             |<->| CONCENTRATION AT TIME N
!| CSTAEQ         |<->| EQUILIBRIUM CONCENTRATION
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DT             |-->| TIME STEP
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| FLUER_SABLE    |<->| EROSION FLUX FOR MIXED SEDIMENTS
!| FLUER_VASE     |<->| EROSION FLUX FOR MIXED SEDIMENTS
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| ICQ            |-->| REFERENCE CONCENTRATION FORMULA
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NCOUCH_TASS    |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| PARTHENIADES   |-->| CONSTANT OF THE KRONE AND PARTHENIADES EROSION LAW (KG/M2/S)
!| QSC            |<->| BEDLOAD TRANSPORT RATE
!| TAUP           |-->| CRITICAL SHEAR STRESS
!| TOCE_MIXTE     |<->| CRITICAL SHEAR STRESS FOR MIXED SEDIMENTS
!| TOCE_SABLE     |<->| CRITICAL SHEAR STRESS FOR SAND
!| TOCE_VASE      |<->| CRITICAL EROSION SHEAR STRESS OF THE MUD PER LAYER (N/M2)
!| VCE            |-->| FLOW VISCOSITY
!| XMVE           |-->| FLUID DENSITY 
!| XMVS           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITIES 
!| ZERO           |-->| ZERO
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_FLUX_MIXTE=>SUSPENSION_FLUX_MIXTE
      USE BIEF
C 
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP,HN
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG,NSICLA
      INTEGER,          INTENT(IN)    :: NCOUCH_TASS
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, VCE,GRAV, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, PARTHENIADES
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: AC,AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_SABLE,FLUER_VASE
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) ::TOCE_MIXTE(NPOIN,10)
C
      DOUBLE PRECISION, INTENT(IN)      :: DT, FDM
C

      TYPE(BIEF_OBJ),   INTENT(IN)       ::  QSC
      INTEGER,          INTENT (IN)      :: ICQ
C
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(10)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_SABLE
C
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER I, J
      DOUBLE PRECISION FLUERSABLE,FLUERVASE,FLUER_LOC(10)
C
      DOUBLE PRECISION QE_MOY,TEMPS,QER_VASE,QER_SABLE

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!

      ! ******************************************** !
      ! I - COMPUTES THE CRITICAL SHEAR STRESS
      !    --> TOCE_SABLE
      ! ******************************************** !
      ! **************************************** !
!        II-COMPUTES EROSION
      ! **************************************** !
C---------DOES THE EROSION COMPUTATION ONLY ONCE (SAND FOR EXAMPLE
C         BECAUSE THE COMPUTED FLUX IS A GLOBAL FLUX COMMON TO THE 2 SEDIMENTS)
C---------COMPUTES THE THEORETICAL FLUX OF EROSION FOR EACH (SEDIMENT INFINITELY C               AVAILABLE IN EACH LAYER)
C
C---------COMPUTES THE CRITICAL STRESS FOR EACH LAYER AS A FUNCTION OF THE C                 PROPORTION OF MUD
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
C
C CV MODIFICATIONS: INTRODUCE TOCE IN ARGUMENT
C         AC(I) = TOCE_MIXTE(I,J)/((XMVS-XMVE)*GRAV*ACLADM%R(I))
C
        IF(ICQ.EQ.1) THEN
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FREDSOE'
C
           CALL SUSPENSION_FREDSOE(FDM,TAUP,NPOIN,
     &         GRAV,XMVE,XMVS,ZERO,AC,CSTAEQ)
C
          IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_FREDSOE'
C
          DO I=1,NPOIN
            CSTAEQ%R(I)=CSTAEQ%R(I)*AVAIL(I,1,1)
          ENDDO
C          
        ELSEIF(ICQ.EQ.2) THEN
C
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BIJKER'
C
               CALL SUSPENSION_BIJKER(TAUP,HN,NPOIN,CHARR,QSC,ZREF,
     &                                ZERO,HMIN,CSTAEQ,XMVE)
C
          IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BIJKER'
C
C CV mars 2011
C
        ELSEIF(ICQ.EQ.3) THEN
         IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_VANRIJN'
 
         CALL SUSPENSION_VANRIJN(FDM,TAUP,NPOIN,
     &                      GRAV,XMVE,XMVS,VCE,ZERO,AC,CSTAEQ,ZREF)
         IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_VANRIJN'
          DO I=1,NPOIN
            CSTAEQ%R(I)=CSTAEQ%R(I)*AVAIL(I,1,1)
          ENDDO
C        fin modif CV
        ENDIF 
C
C      DO J=NCOUCH_TASS,1,-1
C        DO I=1,NPOIN
C           CSTAEQ_COUCHE(I,J)=CSTAEQ%R(I)
C        ENDDO
C      ENDDO
C
      DO I=1,NPOIN
C
        DO J=1,NCOUCH_TASS
C
C-----------COMPUTES FLUER_SABLE_VASE AS A FUNCTION OF THE PROPORTION OF MUD
C
          IF(AVAIL(I,J,2).LE.0.3D0)THEN
C-------------PROPORTION OF MUD < 30%, FLUXES ARE SIMILAR TO THOSE FOR SAND ONLY
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
                 FLUER_LOC(J)=CSTAEQ%R(I)*XWC
            ELSE
               FLUER_LOC(J)=0.D0
            ENDIF
C-------------PROPORTION OF MUD > 50%, FLUXES ARE SIMILAR TO THOSE FOR MUD ONLY
          ELSEIF(AVAIL(I,J,2).GE.0.5D0)THEN
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
               FLUER_LOC(J)=PARTHENIADES*
     &              ((TAUP%R(I)/TOCE_MIXTE(I,J))-1.D0)
            ELSE
               FLUER_LOC(J)=0.D0
            ENDIF
C-------------PROPORTION OF MUD >30% AND <50%, INTERPOLATES THE FLUXES
C             AND CRITICAL SHEAR STRESS
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

C
C COMPUTES THE EROSION DEPTH ZER_MOY
C AND ERODED MASSES
          QER_VASE = 0.D0
          QER_SABLE = 0.D0
C
          TEMPS= DT
C
          DO J= 1, NCOUCH_TASS
           IF(ES(I,J).GE.1.D-6) THEN
C
C COMPUTES THE MASS POTENTIALLY ERODABLE IN LAYER J (KG/M2)
C
             QE_MOY= FLUER_LOC(J) *XMVS * TEMPS
C
             IF(QE_MOY.LT.(MS_SABLE(I,J)
     &            +MS_VASE(I,J))) THEN
C
                  QER_VASE = QER_VASE
     &                  + QE_MOY*MS_VASE(I,J)/
     &                      (MS_VASE(I,J)+MS_SABLE(I,J))
                  QER_SABLE = QER_SABLE
     &                    + QE_MOY*MS_SABLE(I,J)
     &                      /(MS_VASE(I,J)+MS_SABLE(I,J))
CV
                 GO TO 10
C
              ELSE
C
                  QER_VASE = QER_VASE + MS_VASE(I,J)
                  QER_SABLE = QER_SABLE + MS_SABLE(I,J)
                 TEMPS= TEMPS -
     &             (MS_SABLE(I,J)+MS_VASE(I,J))
     &                      /FLUER_LOC(J)/XMVS
              ENDIF
          ENDIF
C
         ENDDO
          WRITE(LU,*) 'ATTENTION TOUTES LES COUCHES SONT VIDES'
C          STOP

  10    CONTINUE
C
      ! ************************************************ !
      ! II-COMPUTES THE FLUX OF EROSION FOR SAND/MUD     !
      ! ************************************************ !
C
C Q_VASE REPRESENTS THE SURFACE MASS OF MUD TO BE ERODED TO REACH ZER_MOY
C Q_SABLE REPRESENTS THE SURFACE MASS OF SAND TO BE ERODED TO REACH ZER_MOY
C
        FLUER_VASE%R(I)  = QER_VASE /(DT*XMVS)
        FLUER_SABLE%R(I) = QER_SABLE/(DT*XMVS)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
