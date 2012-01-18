!                        **********************
                         SUBROUTINE TASSEMENT_2
!                        **********************
!
     *(ZF,NPOIN,DTS,ELAY,DZF_TASS,T2,LT,XMVS,XMVE,GRAV,NCOUCH_TASS,
     * ES,CONC_VASE,MS_VASE,XWC,COEF_N,CONC_GEL,CONC_MAX)
!
!***********************************************************************
! SISYPHE   V6P2                                   13/01/2012
!***********************************************************************
!
!brief    COMPUTES THE CONSOLIDATION BASED ON GIBSON THEORY
!+               
!
!history  Lan Anh Van (10/01/2011)
!+       
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| COEF_N         |-->| PERMEABILITY COEFFICIENT
!| CONC_GEL       |-->| GEL CONCENTRATION
!| CONC_MAX       |-->| MAXIMUM CONCENTRATION
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| DTS            |-->| TIME STEP FOR SUSPENSION
!| DZF_TASS       |-->| BED EVOLUTION DUE TO CONSOLIDATION
!| ELAY           |<->| THICKNESS OF EACH LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| GRAV           |-->| GRAVITY ACCELERATION
!| LT             |-->| ITERATION 
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NCOUCH_TASS    |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| XMVE           |-->| WATER DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
      USE INTERFACE_SISYPHE, EX_TASSEMENT_2 => TASSEMENT_2
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,INTENT(IN)              :: NPOIN
      INTEGER, INTENT(IN)             :: LT,NCOUCH_TASS
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      DOUBLE PRECISION, INTENT(IN)    :: XMVS,XMVE,GRAV
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DZF_TASS,ELAY,T2,ZF
      DOUBLE PRECISION, INTENT(INOUT) :: MS_VASE(NPOIN,NCOUCH_TASS)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NCOUCH_TASS)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NCOUCH_TASS)
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: COEF_N,CONC_GEL,CONC_MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      INTEGER I,J
!     FALLING VELOCITY OF EACH LAYER
      DOUBLE PRECISION V_S(NLAYMAX)
!     EFFECTIVE STRESS OF EACH LAYER
      DOUBLE PRECISION SIG_EFF(NLAYMAX)
!     PERMEABILITY
      DOUBLE PRECISION KSED(NLAYMAX),KCONSO(NLAYMAX)
!     SEDIMENT FLUX BETWEEN TWO CONSECUTIVE LAYERS
      DOUBLE PRECISION FLUX(NLAYMAX)
!
!     ******************************************************************
!     * PROGRAM SIMULATING THE SEDIMENTATION-CONSOLIDATION             *
!     ******************************************************************
!  
        DO I =1,NPOIN 
        T2%R(I)=0.D0
          DO J=1,NCOUCH_TASS
            T2%R(I)=T2%R(I)+ES(I,J)
          ENDDO
c 
c        EFFECTIVE STRESS
c        ------------------------
          DO J = 1,NCOUCH_TASS
            SIG_EFF(J)=119033.d0*(CONC_VASE(J)/XMVS)**14
          ENDDO

c        PERMEABILITY
c        --------------          
          DO J=1,NCOUCH_TASS-1
C        SEDIMENTATION 
             KSED(J)=XWC*(1.D0-CONC_VASE(J)/XMVS)*
     &               (1.D0-(CONC_VASE(J)/CONC_GEL))**COEF_N/
     &               ((XMVS-XMVE)*(CONC_VASE(J)/XMVS)/XMVE)
C        CONSOLIDATION  
             KCONSO(J)=XWC*(1.D0-CONC_VASE(J)/XMVS)*
     &               (1.D0-(CONC_VASE(J)/CONC_MAX))**COEF_N/
     &               ((XMVS-XMVE)*(CONC_VASE(J)/XMVS)/XMVE)
c
!          IF (CONC_VASE(J).gt.CONC_GEL) THEN
            IF(LT.GT.11000.D0) THEN
c
c      SEDIMENTATION AND CONSOLIDATION :      
c      --------------------------------
            IF ((ES(I,J+1) + ES(I,J)).gt.1.d-8) THEN
              V_S(J) =
     &            KCONSO(J) * CONC_VASE(J) * (1.D0/XMVS - 1.D0/XMVE)
     &            + ( KCONSO(J) / (XMVE * GRAV)) *
     &            (SIG_EFF(J+1) - SIG_EFF(J)) / 
     &            (0.5D0 * (ES(I,J+1) + ES(I,J)))
            ELSE
              V_S(J) = 1.d8
            ENDIF
          ELSE
c       PURE SEDIMENTATION :
c      ---------------        
                  V_S(J) = KSED(J)*CONC_VASE(J)*(1.D0/XMVS-1.D0/XMVE)
          ENDIF
        ENDDO
C
          DO J=1,NCOUCH_TASS
           IF (V_S(J).gt.0.d0) V_S(J) = 0.d0 
          ENDDO
C
c        FALLVING VELOCITY AT THE LEVEL OF ZR (AT THE BED)
            V_S(NCOUCH_TASS) = 0.D0
C        SEDIMENT FLUX :
c      ---------------             
          DO J=NCOUCH_TASS-1,1,-1
            FLUX(J) =
     *      (V_S(J)-V_S(J+1))*CONC_VASE(J+1)*CONC_VASE(J)/
     *            (CONC_VASE(J+1)-CONC_VASE(J)) 
            IF (FLUX(J).gt.0.D0) FLUX(J) = 0.D0
          ENDDO
c        SEDIMENT FLUX AT THE RIGID BED
          FLUX(NCOUCH_TASS) = 0.D0
c
c        REDISTRIBUTE THE MASS :
c        ----------------------------------
C        RECALCULATE THE FLUX FROM LAYER 1 TO NCOUCH_TASS       
          IF ((MS_VASE(I,1)+DTS*FLUX(1)).LT.0.D0) THEN
                FLUX(1) = -MS_VASE(I,1)/DTS
          ENDIF
          DO J=2,NCOUCH_TASS
            IF ((MS_VASE(I,J)-DTS*(FLUX(J-1)-FLUX(J))).LT.0.D0) THEN
                FLUX(J) = -MS_VASE(I,J)/DTS + FLUX(J-1)
            ENDIF
          ENDDO
C        MASS OF FIRST LAYER        
          MS_VASE(I,1)=MS_VASE(I,1)+DTS*FLUX(1)
c        MASS OF LAYER 2 TO NCOUCH_TASS                
          DO J=2,NCOUCH_TASS
            MS_VASE(I,J) = MS_VASE(I,J) - DTS * (FLUX(J-1)-FLUX(J))
          ENDDO
C                                                                                                                                                                      
C        THICKNESSES
          ELAY%R(I)=0.D0        
C
          DO J=1,NCOUCH_TASS
            ES(I,J) = MS_VASE(I,J) / CONC_VASE(J)
            ELAY%R(I)=ELAY%R(I) + ES(I,J)
          ENDDO 
C        BED EVOLUTION DUE TO CONSOLIDATION
          DZF_TASS%R(I)=ELAY%R(I)-T2%R(I)
       ENDDO
C  END SUBROUTINE TASSEMENT_2
        RETURN 
        END

