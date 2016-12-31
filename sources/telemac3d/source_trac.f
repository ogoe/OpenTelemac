!                    **********************
                     SUBROUTINE SOURCE_TRAC
!                    **********************
     & (LT)
!
!
!***********************************************************************
! TELEMAC3D   V7P1
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
!history  A. LEROY (LNHE)
!+        25/11/2015
!+        V7P1
!+   Remove the call to INTERPMETEO: all the meteo variables are now
!+   read in meteo.f and stored in variables of waqtel
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LT             |-->| ITERATION NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC,ONLY : COUPLING
      USE DECLARATIONS_TELEMAC3D
!     HEAT EXCHANGE WITH ATMOSPHERE
      USE EXCHANGE_WITH_ATMOSPHERE
      USE DECLARATIONS_WAQTEL,ONLY : NEBU,ZSD,WAQPROCESS,RAYEFF
      USE INTERFACE_WAQTEL
      USE INTERFACE_TELEMAC3D,EX_SOURCE_TRAC =>SOURCE_TRAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: LT
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!----------------------------------------------------------------------
!
      INTEGER ITRAC
      DOUBLE PRECISION DUMM(2)
!
!      INTEGER IPLAN,I,J
!      DOUBLE PRECISION TREEL,SAL,RO,LAMB,RAY_SOL,LATITUDE,LONGITUDE
!      DOUBLE PRECISION KD
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
!!       SOURCE IN TEMPERATURE NOT EQUAL TO ZERO
!        S0TA%ADR(IND_T)%P%TYPR='Q'
!!       INCIDENT SOLAR RADIATION
!!       LATITUDE AND LONGITUDE TO BE CHANGED DEPENDING ON THE MEAN LOCATION
!        LATITUDE  = 43.4458D0
!        LONGITUDE = 5.1139D0
!        CALL SOLRAD(RAY_SOL,NEBU,MARDAT,MARTIM,AT,LATITUDE,LONGITUDE)
!!       FORMULA FOR TURBID WATER WITH SECCHI LENGTH
!!       ZSD: SECCHI LENGTH DECLARED IN THE WAQTEL MODULE
!!       IT CAN BE READ IN A FILE LIKE OTHER METEO DATA
!!       IF NOT PROVIDED, THE USER CAN TRY TO USE A CONSTANT HERE, E.G.
!!       ZSD = 0.9D0 ! MAY BE UNCOMMENTED TOO
!        KD  = 1.7D0/ZSD ! 83% OF THE INCIDENT ENERGY IS ABSORBED
!        SAL = 0.D0
!        DO I=1,NPOIN2
!          DO IPLAN=1,NPLAN
!            J = I + (IPLAN-1)*NPOIN2
!            TREEL=TA%ADR(IND_T)%P%R(NPOIN3-NPOIN2+I)
!            IF (IND_S.NE.0) THEN
!              SAL = TA%ADR(IND_S)%P%R(NPOIN3-NPOIN2+I)
!            ENDIF
!            RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
!            LAMB=RO*CP
!            S0TA%ADR(IND_T)%P%R(J) =
!     &       KD*EXP(KD*(Z(J)-Z(I+(NPLAN-1)*NPOIN2)))
!     &      *RAY_SOL/LAMB
!!
!!           EXAMPLE OF FORMULA FOR TURBID WATER
!!           ALL CONSTANTS MAY BE TUNED
!!           0.22D0 = 1.D0-0.78D0
!!           S0TA%ADR(IND_T)%P%R(J) =
!!           ( 0.78D0*0.66D0 *EXP(0.66D0* (Z(J)-Z(I+(NPLAN-1)*NPOIN2)))
!!     &      +0.22D0*0.125D0*EXP(0.125D0*(Z(J)-Z(I+(NPLAN-1)*NPOIN2))))
!!     &     *RAY_SOL/LAMB
!!
!!           EXAMPLE OF FORMULA FOR CLEAR WATER
!!           ALL CONSTANTS MAY BE TUNED
!!           0.42D0 = 1.D0-0.58D0
!!           S0TA%ADR(IND_T)%P%R(J) =
!!           ( 0.58D0/0.35D0*EXP((Z(J)-Z(I+(NPLAN-1)*NPOIN2))/0.35D0)
!!     &      +0.42D0/23.D0 *EXP((Z(J)-Z(I+(NPLAN-1)*NPOIN2))/23.D0 ))
!!     &     *RAY_SOL/LAMB
!          ENDDO
!        ENDDO
!
      ENDIF
!***********************************************************************
!     WATER QUALITY COUPLING
!*********************************************************************** 
      IF(INCLUS(COUPLING,'WAQTEL'))THEN
!
!       ACTIVATE IMPLICIT SOURCE TERMS
!        IF(LT.EQ.1) CALL YASMI_WAQ(NTRAC,YASMI) 
!
!       MAIN ROUTINE FOR WATER QUALITY
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALL OF SOURCE_WAQ'
!                     TEXP,TIMP,TN
        CALL SOURCE_WAQ
     &  (NPOIN3,NPOIN2,S0TA,S1TA,TA,NTRAC,WAQPROCESS,RAYEFF,IND_T,IND_S,
     &   HN,HPROP,U,V,CF,T3_01,T3_02,T3_03,T3_04,T3_05,T3_07,T3_08,
     &   T3_09,T3_10,T3_11,T3_12,T3_13,
     &   T2_01,T2_02,T2_03,
     &   PATMOS,LISTIN,GRAV,ZF,DEBUG,DUMM,DT,3,VOLU2D,NPLAN,LATIT,
     &   LONGIT,AT,MARDAT,MARTIM,ZPROP)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM SOURCE_WAQ'
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
