!                    **************************
                     SUBROUTINE UTIMP_TELEMAC2D
!                    **************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
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
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_PARALLEL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,ITRAC,I1(20),I2(20),IPOIN
      LOGICAL DEJAUTIMP
      DOUBLE PRECISION MINIMORUM(20),MAXIMORUM(20),MAXIM(20),MINIM(20)
      DOUBLE PRECISION EIKON,C,AREA
      DATA DEJAUTIMP/.FALSE./
      SAVE MINIMORUM,MAXIMORUM,I1,I2
      CALL CPSTVC(T%ADR(1)%P,T1)
      IF(.NOT.DEJAUTIMP) THEN
        DO K=1,20
          MINIMORUM(K)=1.D99
          MAXIMORUM(K)=-MINIMORUM(K)
          I1(K)=0
          I2(K)=0
        ENDDO
        DEJAUTIMP=.TRUE.
      ENDIF
      DO K=1,20
        MINIM(K)=1.D99
        MAXIM(K)=-MINIM(K)
      ENDDO
      DO ITRAC=1,T%N
        DO K=1,T%ADR(ITRAC)%P%DIM1
          IF(T%ADR(ITRAC)%P%R(K).LT.MINIMORUM(ITRAC)) THEN
            I1(ITRAC)=K
            MINIMORUM(ITRAC)=T%ADR(ITRAC)%P%R(K)
          ENDIF
          MINIM(ITRAC)=MIN(MINIM(ITRAC),T%ADR(ITRAC)%P%R(K))
        ENDDO
        DO K=1,T%ADR(ITRAC)%P%DIM1
          IF(T%ADR(ITRAC)%P%R(K).GT.MAXIMORUM(ITRAC)) THEN
            I2(ITRAC)=K
            MAXIMORUM(ITRAC)=T%ADR(ITRAC)%P%R(K)
          ENDIF
          MAXIM(ITRAC)=MAX(MAXIM(ITRAC),T%ADR(ITRAC)%P%R(K))
        ENDDO
        DO IPOIN=1,T%ADR(ITRAC)%P%DIM1
          EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.2D0)**2 ) / 2.D0
          T1%R(IPOIN)=T%ADR(ITRAC)%P%R(IPOIN)-EXP(-EIKON)
        ENDDO
      ENDDO
      DO ITRAC=1,NTRAC
        MINIMORUM(ITRAC)=P_DMIN(MINIMORUM(ITRAC))
        MINIM(ITRAC)=P_DMIN(MINIM(ITRAC))
        PRINT*,
     &  NAMETRAC(ITRAC),' MINIMORUM=',MINIMORUM(ITRAC),' EN ',I1(ITRAC)
        PRINT*,
     &  NAMETRAC(ITRAC),' MINIM=',MINIM(ITRAC),' EN ',I1(ITRAC)
      ENDDO
      DO ITRAC=1,NTRAC
        MAXIMORUM(ITRAC)=P_DMAX(MAXIMORUM(ITRAC))
        MAXIM(ITRAC)=P_DMAX(MAXIM(ITRAC))
        PRINT*,
     &  NAMETRAC(ITRAC),' MAXIMORUM=',MAXIMORUM(ITRAC),' EN ',I2(ITRAC)
        PRINT*,
     &  NAMETRAC(ITRAC),' MAXIM=',MAXIM(ITRAC),' EN ',I2(ITRAC)
      ENDDO
!     COMPUTING THE MEAN DEVIATION
      AREA=0.D0
      DO IPOIN=1,T%ADR(1)%P%DIM1
        AREA=AREA+VOLU2D%R(IPOIN)
      ENDDO
      IF(NCSIZE.GT.1) AREA=P_DSUM(AREA)
      DO ITRAC=1,T%N
        C=0.D0
        DO IPOIN=1,T%ADR(ITRAC)%P%DIM1
          EIKON=( (X(IPOIN)-15.D0)**2 + (Y(IPOIN)-10.2D0)**2 ) / 2.D0
          C=C+VOLU2D%R(IPOIN)*(T%ADR(ITRAC)%P%R(IPOIN)-EXP(-EIKON))**2
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_DSUM(C)
        ENDIF
        C=C/AREA
        PRINT*,NAMETRAC(ITRAC),' 10**3 STANDARD DEVIATION=',1.D3*SQRT(C)
      ENDDO
!
!***********************************************************************
! USER OUTPUT
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D

