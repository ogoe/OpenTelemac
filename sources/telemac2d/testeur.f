!                    ******************
                     SUBROUTINE TESTEUR
!                    ******************
!
     &(NS,NSEG,NPTFR,NUBO,DT,NBOR,NORDRE,AIRS,AIRST,HSTOK,
     & HCSTOK,FLUXT,FLUXTEMP,FLUHBOR,FLUHBTEMP,LOGFR,TEST,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CHECKS FOR POSITIVE TRACER VALUES.
!
!history  INRIA
!+        05/09/2007
!+        V5P8
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
!| AIRS           |-->| CELL'S AREA
!| AIRST          |-->| AREA OF SUB-TRIANGLES IN THE CELLS
!| DT             |-->| TIME STEP
!| FLUHBOR        |-->| MASS FLUX AT THE BOUNDARY
!| FLUHBTEMP      |-->| BOUNDARY FLUX FOR TRACERS
!| FLUXT,FLUHBOR  |<->| FLUX
!| FLUXTEMP       |<->| FLUX FOR TRACER
!| HSTOK          |-->| H TO STOCK FOR TRACER
!| HCSTOK         |-->| H CORRECTED TO STOCK FOR TRACER
!| LOGFR          |-->| REFERENCE OF BOUNDARY NODES
!| NBOR           |-->| GLOBAL INDICES FOR BORD NODES
!| NORDRE         |-->| ORDRE OF THE SCHEME
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| NS             |-->| TOTAL NUMBER OF NODES IN THE MESH
!| NSEG           |-->| TOTAL NUMBER OF EDGES
!| NTRAC          |-->| TOTAL NUMBER OF TRACERS
!| NU             |-->| NUMBERING OF NODES IN THE TRIANGLES
!| NUBO           |-->| GLOBAL INDICES OF EDGE EXTREMITIES
!| TEST           |<--| RESULT OF THE TEST
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : DEJA_TESTEUR, FLUX_TESTEUR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NSEG,NPTFR,NORDRE,NTRAC
      INTEGER, INTENT(IN)             :: NUBO(2,NSEG),NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LOGFR(*)
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NS),AIRST(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: TEST
      DOUBLE PRECISION, INTENT(IN)    :: HSTOK(*)
      DOUBLE PRECISION, INTENT(IN)    :: HCSTOK(2,*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: FLUHBOR,FLUHBTEMP,FLUXT
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: FLUXTEMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NSG,NUBO1,NUBO2,ERR,ITRAC
!
      DOUBLE PRECISION AUX
!
!------------------------------------------------------------------------
!
      IF(.NOT.DEJA_TESTEUR) THEN
        ALLOCATE(FLUX_TESTEUR(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'TESTEUR : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &         'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'TESTEUR: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA_TESTEUR=.TRUE.
      ENDIF
!
!------------------------------------------------------------------------
!
!     TEST ON ALL THE TRACERS, ONE TEST IS SUFFICIENT
!
      DO ITRAC=1,NTRAC
!
        DO IS=1,NS
          FLUX_TESTEUR(IS) = 0.D0
        ENDDO
!
!   TEST FOR ORDER 1
!
        IF(NORDRE.EQ.1) THEN
!
          DO NSG=1,NSEG
            NUBO1=NUBO(1,NSG)
            NUBO2=NUBO(2,NSG)
            AUX = FLUXT%ADR(ITRAC)%P%R(NSG)
     &            + DT*FLUXTEMP%ADR(ITRAC)%P%R(NSG)
            IF(AUX.GE.0.D0) THEN
              FLUX_TESTEUR(NUBO1) = FLUX_TESTEUR(NUBO1) + AUX
            ELSE
              FLUX_TESTEUR(NUBO2) = FLUX_TESTEUR(NUBO2) - AUX
            ENDIF
          ENDDO
          IF(NPTFR.GT.0)THEN !USEFUL FOR PARALLEL CASES
            DO K=1,NPTFR
              IS =NBOR(K)
              AUX = FLUHBOR%ADR(ITRAC)%P%R(K)
     &            + DT*FLUHBTEMP%ADR(ITRAC)%P%R(K)
              IF(AUX.GE.0.D0) FLUX_TESTEUR(IS)=FLUX_TESTEUR(IS)+AUX
            ENDDO
          ENDIF
          DO IS=1,NS
            TEST=AIRS(IS)*HSTOK(IS)-FLUX_TESTEUR(IS)
            IF(TEST.LT.0.D0) RETURN
          ENDDO
!
        ELSE
!
!   TEST FOR ORDER 2
!
          DO NSG=1,NSEG
            NUBO1=NUBO(1,NSG)
            NUBO2=NUBO(2,NSG)
!
            AUX = FLUXT%ADR(ITRAC)%P%R(NSG)
     &          + DT*FLUXTEMP%ADR(ITRAC)%P%R(NSG)
!
            IF(AUX.GE.0.D0) THEN
!
              TEST=AIRST(1,NSG)*HCSTOK(1,NSG)-AUX
              IF(LOGFR(NUBO1).NE.0.AND.
     &           AIRST(1,NSG)*HCSTOK(1,NSG).GT.0.D0) THEN
                FLUX_TESTEUR(NUBO1)= MAX(FLUX_TESTEUR(NUBO1),
     &          AUX/(AIRST(1,NSG)*HCSTOK(1,NSG)))
              ENDIF
!
            ELSE
!
              TEST=AIRST(2,NSG)*HCSTOK(2,NSG)+AUX
              IF(LOGFR(NUBO2).NE.0.AND.
     &          AIRST(2,NSG)*HCSTOK(2,NSG).GT.0.D0) THEN
                FLUX_TESTEUR(NUBO2)= MAX(FLUX_TESTEUR(NUBO2),
     &          -AUX/(AIRST(2,NSG)*HCSTOK(2,NSG)))
              ENDIF
!
            ENDIF
            IF(TEST.LT.0.D0) RETURN
          ENDDO
!
!   TEST FOR THE BOUNDARY NODES
!
          DO K=1,NPTFR
            IS =NBOR(K)
            AUX = FLUHBOR%ADR(ITRAC)%P%R(K)
     &          + DT*FLUHBTEMP%ADR(ITRAC)%P%R(K)
            IF(AUX.GE.0.D0) THEN
              TEST=AIRS(IS)*HSTOK(IS)-AUX
              IF(AIRS(IS)*HSTOK(IS).GT.0.D0.AND.
     &        (1.D0-FLUX_TESTEUR(IS)).LT. AUX/(AIRS(IS)*HSTOK(IS)))
     &        TEST =-1.D0
            ENDIF
            IF(TEST.LT.0.D0) RETURN
!
          ENDDO
        ENDIF
      ENDDO
!
!------------------------------------------------------------------------
!
      RETURN
      END
