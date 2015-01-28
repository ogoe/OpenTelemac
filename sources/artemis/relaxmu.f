!                    *****************
                     SUBROUTINE RELAXMU
!                    *****************
     &(ECRHMU,MODHMU,ITERMU)
!
!
!***********************************************************************
! ARTEMIS   V7P0                                   06/2014
!***********************************************************************
!
!brief    COMPUTES THE NEW DISSIPATION COEFFICIENT
!+                USING RELAXATION METHOD
!
!history  C PEYRARD (LNHE)
!+        27/03/2014
!+        V7P0
!+        NEW SUBROUTINE CREATED / IMPLEMENTED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ITERMU             |-->| INDICE OF THE CURRENT CALCULATION
!| ECRHMU             |-->| ERROR ON WAVE HEIGHT BETWEEN 2 ITERATIONS
!| MODHMU             |-->| MODULE OF WAVE HEIGHT FOR THE CURRENT ITER.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I,ITERMU
      DOUBLE PRECISION ECRHMU,MODHMU
!-----------------------------------------------------------------------
!
      INTRINSIC ABS,MIN,MAX,LOG
      DOUBLE PRECISION P_DMAX
      EXTERNAL P_DMAX
!
!----------------------------------------------------------------------
!
!     -------------------------------------------------------
!     RELAXATION ON MU2 TO TRY AND AVOID OSCILLATIONS IN THE
!     CONVERGENCE OF THE SOLVEUR
!     -------------------------------------------------------
!
!
      ECRHMU=0D0
      MODHMU = 1.D-9
      DO I = 1,NPOIN
!       ----------
!       RELAXATION
!       ----------
!
        MU2%R(I) = MU%R(I) + RELDIS * (MU2%R(I) - MU%R(I))
        IF(ITERMU.EQ.0) THEN
          HMUANC%R(I) = HMU%R(I)
          ECRHMU = 1.D0
          MODHMU = 1.D0
          MU%R(I) = MU2%R(I)
        ELSE
!          WRITE(6,*) 'HMU,HMUANC=',HMU%R(I),HMUANC%R(I)
          ECRHMU = MAX(ECRHMU,ABS(HMU%R(I)-HMUANC%R(I)))
          MODHMU = MAX(MODHMU,ABS(HMU%R(I)))
          MU%R(I) = MU2%R(I)
          HMUANC%R(I) = HMU%R(I)
        ENDIF
      ENDDO
!      WRITE(6,*) 'MODHMU AVANT MAX=',MODHMU
!
!
!     RELAXES THE RELAXATION AT EACH SUB-ITERATION
!     TO FACILITATE CONVERGENCE OF THE ALGORITHM USED TO
!     COMPUTE DISSIPATION (REGULAR WAVES)
!
      IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
        RELDIS = RELDIS * 0.85D0
      ENDIF
!
      IF (NCSIZE .GT. 1) THEN
        ECRHMU = P_DMAX(ECRHMU)
        MODHMU = P_DMAX(MODHMU)
      END IF
!      WRITE(6,*) 'MODHMU APRES MAX=',MODHMU,LNG
      IF (LNG.EQ.1) WRITE(LU,*) 'ECART ENTRE DEUX
     &        SOUS-ITERATIONS (%)',
     &        100*ECRHMU/MODHMU
      IF (LNG.EQ.2) WRITE(LU,*) 'DIFF. BETWEEN TWO
     &        SUB-ITERATIONS (%) ',
     &        100*ECRHMU/MODHMU
      ITERMU = ITERMU + 1
!      WRITE(6,*) 'ITERMU FIN RELAX=',ITERMU
!
!
!     -----------------------------------------------------------
!     IF NUMBER OF SUB-ITERATIONS FOR MU >= MAX NUMBER OF SUB-ITERATIONS
!     EXITS THE LOOP OVER MU AND SETS THE RELATIVE DIFFERENCE
!     ECRHMU/MODHMU TO 10 % OF EPSDIS
!     -----------------------------------------------------------
!
      IF(ITERMU.GE.NITDIS) THEN
        IF (LNG.EQ.1) WRITE(LU,100) ITERMU
        IF (LNG.EQ.2) WRITE(LU,101) ITERMU
 100    FORMAT(/,1X,'BERKHO (ARTEMIS): NOMBRE DE SOUS-ITERATIONS',
     & 1X,'MAXIMUM ATTEINT :',1X,I3)
 101    FORMAT(/,1X,'BERKHO (ARTEMIS): YOU REACHED THE MAXIMUM',
     & 1X,'NUMBER OF SUB-ITERATIONS :)',1X,I3)
            ECRHMU = EPSDIS*MODHMU/10.D0
      ENDIF
!
      RETURN
      END SUBROUTINE
