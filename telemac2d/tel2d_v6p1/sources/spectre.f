!                    ******************
                     SUBROUTINE SPECTRE
!                    ******************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    HARMONIC ANALYSIS OF TIDE WAVES USING LEAST-SQUARE
!+                FITTING METHOD.
!
!reference  "SIMULATION DES COURANTS DE MAREE EN MANCHE ET PROCHE ATLANTIQUE",
!+                       EDF REPORT, J. M. JANIN ET. AL., PP 40-41.
!
!history  J-M HERVOUET LNH
!+        26/10/1994
!+        V5P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION PI,CFX,CFY,A
!
      DOUBLE PRECISION, ALLOCATABLE :: AM(:,:),BM(:,:),HA(:)
!
      INTEGER M,I,J,K,N,AFBGN,AFEND
      SAVE AFBGN,AFEND,AM,BM,HA
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
      INTRINSIC COS,SIN,ACOS,DMOD,DATAN2
!
      DOUBLE PRECISION P_DMIN,P_DMAX
      EXTERNAL         P_DMIN,P_DMAX
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA) THEN
!
        AFBGN=INT((TAFBGN-AT)/DT +1.D-6)
        AFEND=INT((TAFEND-AT)/DT +1.D-6)
        IF(AFEND.LE.AFBGN) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
            WRITE(LU,*) 'BORNES EN TEMPS POUR L''ANALYSE DE FOURIER'
            WRITE(LU,*) 'VERIFIER OU RENSEIGNER CE MOT-CLE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)
            WRITE(LU,*) 'TIME RANGE FOR FOURIER ANALYSIS'
            WRITE(LU,*) 'CHECK OR USE THIS KEY-WORD'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(AM(2*NPERIAF,2*NPERIAF))
        ALLOCATE(BM(2*NPERIAF,2*NPERIAF))
        ALLOCATE(HA(2*NPERIAF))
        DEJA=.TRUE.
!       INITIALISES TO ZERO AT THE FIRST CALL (THINK ABOUT DESIMP)
        DO I = 1,NPERIAF
          DO J = 1,NPOIN
            AMPL%ADR(I)%P%R(J) = 0.D0
            PHAS%ADR(I)%P%R(J) = 0.D0
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      PI = ACOS(-1.D0)
      M = AFEND-AFBGN+1
!
      IF(LT.GE.AFBGN.AND.LT.LE.AFEND) THEN
         DO I=1,NPERIAF
            A=2.D0*PI*DMOD(AT/PERIAF(I),1.D0)
            CFX=COS(A)/M
            CFY=SIN(A)/M
            DO J=1,NPOIN
         AMPL%ADR(I)%P%R(J)=AMPL%ADR(I)%P%R(J)+(H%R(J)+ZF%R(J))*CFX
         PHAS%ADR(I)%P%R(J)=PHAS%ADR(I)%P%R(J)+(H%R(J)+ZF%R(J))*CFY
            ENDDO
         ENDDO
      ENDIF
!
! ESTABLISHES THE COEFFICIENT MATRICES AND INVERSES IT. MULTIPLIED BY
! RIGHT HAND SIDES, WE GET THE UNKOWNS OF LINEAR EQNS. NOW, AMPL
! CONTAINS THE AMPLITUDE OF THE SIN HARMONIC COMPONENTS OF SURFACE
! ELEVATION; PHAS CONTAINS THE AMPLITUDE OF THE COS HARMONIC
! COMPONENTS OF SURFACE ELEVATION.
!
      IF(LT.EQ.AFEND) THEN
         CALL COEFMAT(PERIAF,DT,M,AM,NPERIAF)
         N=2*NPERIAF
         CALL INVMTX(AM,BM,N)
         DO J=1,NPOIN
            DO I=1,NPERIAF
               HA(I) = AMPL%ADR(I)%P%R(J)
               HA(I+NPERIAF) = PHAS%ADR(I)%P%R(J)
            ENDDO
            DO I = 1,NPERIAF
               AMPL%ADR(I)%P%R(J) = 0
               PHAS%ADR(I)%P%R(J) = 0
               DO K=1,2*NPERIAF
         AMPL%ADR(I)%P%R(J) = AMPL%ADR(I)%P%R(J)+BM(I,K)*HA(K)
         PHAS%ADR(I)%P%R(J) = PHAS%ADR(I)%P%R(J)+BM(I+NPERIAF,K)*HA(K)
               ENDDO
            ENDDO
         ENDDO
!
         DO J = 1, NPOIN
            DO I = 1,NPERIAF
               CFX = AMPL%ADR(I)%P%R(J)
               CFY = PHAS%ADR(I)%P%R(J)
               AMPL%ADR(I)%P%R(J) = SQRT(CFX**2+CFY**2)
               PHAS%ADR(I)%P%R(J) = 180.D0*DATAN2(CFY,CFX)/PI
               IF (PHAS%ADR(I)%P%R(J).LT.0.D0)
     &	           PHAS%ADR(I)%P%R(J)=PHAS%ADR(I)%P%R(J)+360.D0
            ENDDO
         ENDDO
      ENDIF
!
! OUTPUTS THE AMPLITUDES AND PHASES AT EACH POINT OF INTEREST
!
      IF(LT.EQ.NIT) THEN
         DO I = 1, NPERIAF
            IF(NPTS.GT.0) THEN
               WRITE(LU,*) ' '
               WRITE(LU,*) ' '
               WRITE(LU,*) ' '
               IF(LNG.EQ.1) WRITE(LU,*) 'ANALYSE DE LA PERIODE ',
     &	                                 PERIAF(I), ' S :'
               IF(LNG.EQ.2) WRITE(LU,*) 'ANALYSE OF PERIOD ',
     &	                                 PERIAF(I), ' S :'
               WRITE(LU,*) ' '
               WRITE(LU,90) 'NOM DE POINT', 'AMPLITUDE', 'PHASE'
               WRITE(LU,*) ' '
               DO J = 1, NPTS
!                 IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
                  IF(NCSIZE.GT.0) THEN
                    WRITE(LU,100) NAME_PTS(J),
     &                            P_DMIN(AMPL%ADR(I)%P%R(LIST_PTS(J)))
     &                           +P_DMAX(AMPL%ADR(I)%P%R(LIST_PTS(J))),
     &	                          P_DMIN(PHAS%ADR(I)%P%R(LIST_PTS(J)))
     &                           +P_DMAX(PHAS%ADR(I)%P%R(LIST_PTS(J)))
                  ELSE
                    WRITE(LU,100) NAME_PTS(J),
     &                            AMPL%ADR(I)%P%R(LIST_PTS(J)),
     &	                          PHAS%ADR(I)%P%R(LIST_PTS(J))
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
!
 90   FORMAT(1X, A15, A16  , A11  )
 100  FORMAT(1X, A15, F16.3, F11.2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
