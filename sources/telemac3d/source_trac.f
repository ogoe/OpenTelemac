!                    **********************
                     SUBROUTINE SOURCE_TRAC
!                    **********************
!
!
!***********************************************************************
! TELEMAC3D   V7P0                                   09/07/2014
!***********************************************************************
!
!brief    PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.
!
!history  CDG/SOGREAH
!+        **/06/2001
!+
!+   TRACER SOURCES
!
!history  J-M HERVOUET (LNHE)
!+        21/10/2004
!+        V5P5
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
!history  A. GINEAU, N. DURAND, N. LORRAIN, C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Adding an example of the penetration of the solar radiation
!+   for exchange with atmosphere
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D
!     HEAT EXCHANGE WITH ATMOSPHERE. UNCOMMENT IF USED
!      USE EXCHANGE_WITH_ATMOSPHERE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER ITRAC
!
!     HEAT EXCHANGE WITH ATMOSPHERE. UNCOMMENT IF USED
!      INTEGER IPLAN,I,J,NFO
!      DOUBLE PRECISION WW,WINDX,WINDY,T_AIR,PATM,HREL,NEBU,RAINFALL
!      DOUBLE PRECISION TREEL,SAL,RO,LAMB,RAY_SOL,LATITUDE,LONGITUDE
!      DOUBLE PRECISION ZSD,KD
!
!----------------------------------------------------------------------
!
!     SETS SOURCE TERMS TO ZERO
!
      IF(NTRAC.GE.1) THEN
!
!       CALL OS ( 'X=C     ' , X=S0TA , C=0.D0 )
!       CALL OS ( 'X=C     ' , X=S1TA , C=0.D0 )
!
!       SOURCE TERMS SIMPLY MARKED
!
!       BEWARE, PUT Q INSTEAD OF 0 IN TYPR IF NOT NIL
!
        DO ITRAC=1,NTRAC
          S0TA%ADR(ITRAC)%P%TYPR='0'
          S1TA%ADR(ITRAC)%P%TYPR='0'
        ENDDO
!
!       EXAMPLE OF RADIOACTIVE DECAY E**(-KT) ON FIRST TRACER, HERE C=K
!
!       S1TA%ADR(1)%P%TYPR='Q'
!       CALL OS('X=C     ',S1TA%ADR(1)%P,C=1.D0)
!
!!       EXAMPLE OF PENETRATION OF THE SOLAR RADIATION
!!       IF EXCHANGE WITH ATMPOSPHERE IS USED, DO NOT FORGET TO
!!       UNCOMMENT THE DECLARATIONS OF THE MODULE + THE VARIABLES ABOVE
!!
!        ITRAC = 1 ! FIRST TRACER IS THE WATER TEMPERATURE, MAY BE CHANGED
!!       SOURCE IN TEMPERATURE NOT EQUAL TO ZERO
!        S0TA%ADR(ITRAC)%P%TYPR='Q'	
!!       DONNEES METEO DATA
!        NFO = T3D_FILES(T3DFO1)%LU   ! FORMATTED DATA FILE 1
!        CALL INTERPMETEO(WW,WINDX,WINDY,T_AIR,PATM,HREL,NEBU,RAINFALL,
!     &                   AT,NFO)
!!       INCIDENT SOLAR RADIATION
!!       LATITUDE AND LONGITUDE TO BE CHANGED DEPENDING ON THE MEAN LOCATION
!        LATITUDE  = 43.4458D0
!        LONGITUDE = 5.1139D0
!        CALL SOLRAD(RAY_SOL,NEBU,MARDAT,MARTIM,AT,LATITUDE,LONGITUDE)
!!       FORMULA FOR TURBID WATER WITH SECCHI LENGTH
!!       ZSD: SECCHI LENGTH
!        ZSD = 0.9D0
!        KD  = 1.7D0/ZSD ! 83% OF THE INCIDENT ENERGY IS ABSORBED
!        SAL = 0.D0
!        DO I=1,NPOIN2
!          DO IPLAN=1,NPLAN
!            J = I + (IPLAN-1)*NPOIN2
!            TREEL=TA%ADR(ITRAC)%P%R(NPOIN3-NPOIN2+I)
!            RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
!            LAMB=RO*CP
!            S0TA%ADR(ITRAC)%P%R(J) = 
!     &       KD*EXP(KD*(Z(J)-Z(I+(NPLAN-1)*NPOIN2)))
!     &      *RAY_SOL/LAMB
!!
!!           EXAMPLE OF FORMULA FOR TURBID WATER
!!           ALL CONSTANTS MAY BE TUNED
!!           0.22D0 = 1.D0-0.78D0
!!           S0TA%ADR(ITRAC)%P%R(J) =
!!           ( 0.78D0*0.66D0 *EXP(0.66D0* (Z(J)-Z(I+(NPLAN-1)*NPOIN2)))
!!     &      +0.22D0*0.125D0*EXP(0.125D0*(Z(J)-Z(I+(NPLAN-1)*NPOIN2))))
!!     &     *RAY_SOL/LAMB
!!
!!           EXAMPLE OF FORMULA FOR CLEAR WATER
!!           ALL CONSTANTS MAY BE TUNED
!!           0.42D0 = 1.D0-0.58D0
!!           S0TA%ADR(ITRAC)%P%R(J) =
!!           ( 0.58D0/0.35D0*EXP((Z(J)-Z(I+(NPLAN-1)*NPOIN2))/0.35D0)
!!     &      +0.42D0/23.D0 *EXP((Z(J)-Z(I+(NPLAN-1)*NPOIN2))/23.D0 ))
!!     &     *RAY_SOL/LAMB
!          ENDDO
!        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
