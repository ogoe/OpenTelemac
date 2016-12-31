!                    *****************
                     SUBROUTINE TWCALE
!                    *****************
!
!***********************************************************************
! ARTEMIS   V7P1
!***********************************************************************
!
!brief    DISCRETISES AN ENERGY SPECTRUM IN NDALE x NPALE CELLS
!+                OF EQUAL ENERGY. THE RESULT IS A LIST OF
!+                DIRECTION CORRESPONDING TO EACH BAND AND
!+                A MATRIX GIVING PERIODS FOR EACH CELL.
!
!history  C.PEYRARD
!+        07/2014
!+        V7P0
!+  creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DALE           |-->| ISO ENERGY DIRECTIONS TO BE SOLVED
!| PDALE          |-->| MATRIX : FOR EACH ISO-ENERGY DIRECTION
!|                |   |          GIVES THE ISO-ENERGY FREQUENCIES
!| PMAX           |-->| MAXIMUM FREQUENCY FOR SPECTRUM
!| PMIN           |-->| MINIMUM FREQUENCY FOR SPECTRUM
!| TETMAX         |-->| MAXIMUM DIRECTION FOR SPECTRUM
!| TETMIN         |-->| MINIMUM DIRECTION FOR SPECTRUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, ONLY : STWC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER NPASF,NPASD
      INTEGER IDALE,JPALE,ID,IFF,I,J
!
      DOUBLE PRECISION SUMB,SUMICI,DF,VAR,DTETA
      DOUBLE PRECISION FMIN,FMAX
      DOUBLE PRECISION POIDS,DEGRAD
      DOUBLE PRECISION SUMD
!
      DEGRAD=4.D0*ATAN(1.D0)/180.D0
!     CHECK IF TETMIN<TETMAX
      IF(TETMAX.LE.TETMIN) THEN
        WRITE(LU,*) 'ROUTINE twcale.f, tomawac spectrum discretization'
        WRITE(LU,*) '------ !! MAX DIRECTION < MIN DIRECTION !! ------'
        WRITE(LU,*) ' CHANGE MAXIMUM OR MINIMUM ANGLE OF PROPAGATION  '
        WRITE(LU,*) ' IN THE USER PARAMETER FILE.                     '
        CALL PLANTE(1)
        STOP
      ENDIF
!     CHECK IF PMIN<PMAX
      IF(PMAX.LE.PMIN) THEN
        WRITE(LU,*) 'ROUTINE twcale.f, tomawac spectrum discretization'
        WRITE(LU,*) '--------- !! MAX PERIOD < MIN PERIOD !! ---------'
        WRITE(LU,*) ' CHANGE MAXIMUM OR MINIMUM SPECTRAL PERIOD       '
        WRITE(LU,*) ' IN THE USER PARAMETER FILE.                     '
        CALL PLANTE(1)
        STOP
      ENDIF
!     MIN/MAX FREQUENCY
      FMIN = 1.D0 / PMAX
      FMAX = 1.D0 / PMIN
!
!     NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD : DIRECTIONS
      NPASD = 500*NDALE
!
!     NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD : FREQUENCIES
      NPASF = 500*NPALE
!
!     WIDTH OF AN INTEGRATION INTERVAL
      DTETA = (TETMAX-TETMIN)/FLOAT(NPASD)
!
!     WIDTH OF AN INTEGRATION INTERVAL
      DF = (FMAX-FMIN)/FLOAT(NPASF)
!
!     SIGNIFICANT WAVE HEIGHT INIT.
!
      HSCAL=0.D0
!
!-----------------------------------------------------------------------
!     INTEGRAL OF THE SPECTRUM (trapezoidal method)
      SUMB = 0.D0
      DO ID = 1,NPASD+1
        SUMD=0.D0
        DO IFF = 1,NPASF+1
!         IF FREQ AND/OR DIR ON THE BOUNDARY OF THE INTEGRATION DOMAIN :  contribution/2
          POIDS=1.D0
          IF(IFF.EQ.1.OR.IFF.EQ.NPASF+1) THEN
            POIDS=0.5D0*POIDS
          ENDIF
          IF(ID.EQ.1.OR.ID.EQ.NPASD+1) THEN
            POIDS=0.5D0*POIDS
          ENDIF
          VAR=STWC(FMIN+FLOAT(IFF-1)*DF,TETMIN+FLOAT(ID-1)*DTETA)
          SUMD = SUMD + POIDS*VAR*DF
        ENDDO
        SUMB=SUMB+SUMD*DTETA*DEGRAD
      ENDDO
!     SIGNIFICANT WAVE HEIGHT CORRESPONDIG TO TOTAL ENERGY STORAGE
      HSCAL=4.D0*SQRT(SUMB)
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '========== START SPECTRUM INTERPOLATION ==========='
!     =======================================================
!                          DIRECTIONS
!     =======================================================
!     DIVIDES THE SPECTRUM INTO 2*NDALE BANDS OF EQUAL ENERGY
      SUMB = SUMB/FLOAT(2*NDALE)
!
!     FIRST TERM OF DIRECTION DISCRETIZATION
      I=1
      DTWC(I) = 1
!
!     IDENTIFIES THE ANGLES EVERY (I)*SUMB (I=1,NDALE)
      SUMICI = 0.D0
!
      DO ID = 1,NPASD+1
!       COMPUTE INTERGAL AS ABOVE
!       --------
        SUMD=0.D0
        DO IFF = 1,NPASF+1
!         IF FREQ AND/OR DIR ON THE BOUNDARY OF THE INTEGRATION DOMAIN :  contribution/2
          POIDS=1.D0
          IF ((IFF.EQ.1).OR.(IFF.EQ.NPASF+1)) THEN
            POIDS=0.5D0*POIDS
          ENDIF
          IF ((ID.EQ.1).OR.(ID.EQ.NPASD+1)) THEN
            POIDS=0.5D0*POIDS
          ENDIF
          VAR=STWC(FMIN+FLOAT(IFF-1)*DF,TETMIN+FLOAT(ID-1)*DTETA)
          SUMD = SUMD + POIDS*VAR*DF
        ENDDO
        SUMICI=SUMICI+SUMD*DTETA*DEGRAD
!
!       CHECK IF SUMB/(2 NDALE) IS REACHED AND SAVE TETA
!
        IF (SUMICI.GE.SUMB*FLOAT(I).OR.ID.EQ.NPASD+1) THEN
          I=I+1
          DTWC(I) = ID
        ENDIF
!
      ENDDO
!
! NOW WE HAVE IN DTWC :
!  TETMIN   T1   Ts1   T2   Ts2     ......... Tn Tmax
!  TETMIN -> Ts1,Ts2... : SUMB/NDALE limit
!     &      T1,T2...   : mean direction to be computed by ARTEMIS
!
!----------------------------------------------------------------------
!     =======================================================
!                          FREQUENCIES
!     =======================================================
!     DIVIDES THE SPECTRUM/NDALE INTO 2*NPALE BANDS OF EQUAL ENERGY
      SUMB = SUMB/FLOAT(NPALE)
!     IDENTIFIES THE FREQUENCIES EVERY (I)*SUMB (I=1,NPALE)
      I    =1
      IDALE=1
!
!     FOR EACH DIRECTION DOMAIN
!
98    CONTINUE
      SUMICI = 0.D0
      J=1
      FTWC(J) = FMIN
      DO IFF = 1,NPASF+1
!
!       COMPUTE INTERGAL AS ABOVE
!
        SUMD=0.D0
        DO ID = DTWC(I),DTWC(I+2)
          POIDS=1.D0
          IF ((IFF.EQ.1).OR.(IFF.EQ.NPASF+1)) THEN
            POIDS=0.5D0*POIDS
          ENDIF
          IF ((ID.EQ.1).OR.(ID.EQ.NPASD+1)) THEN
            POIDS=0.5D0*POIDS
          ENDIF
          VAR=STWC(FMIN+FLOAT(IFF-1)*DF,TETMIN+FLOAT(ID-1)*DTETA)
          SUMD = SUMD + POIDS*VAR*DTETA*DEGRAD
        ENDDO
        SUMICI=SUMICI+SUMD*DF
!
!       CHECK IF SUMB/(2 NPALE) IS REACHED AND SAVE TETA
!
        IF (SUMICI.GE.SUMB*FLOAT(J).OR.IFF.EQ.NPASF+1) THEN
          J=J+1
          FTWC(J) = FMIN+FLOAT(IFF-1)*DF
        ENDIF
!
      ENDDO
!
!     STOCK PERIODS IN A LINE OF PDALE
!
      DO JPALE=1,NPALE
        PDALE%R((IDALE-1)*NPALE+(NPALE-JPALE+1)) = 1.D0/FTWC(2*JPALE)
      ENDDO
!
!  NOW WE HAVE IN PDALE :
!  DIRECTION1 :  T11   T12   T13  .....
!  DIRECTION2 :  T21   T22   T23  .....
!     .
!     .
!     .
!  (DIRECTION i is given in DALE)
!
!    GO TO NEXT DIRECTION
!
      DALE%R(IDALE)=TETMIN+FLOAT(DTWC(I+1)-1)*DTETA
      IF(I.LT.(2*NDALE-1)) THEN
        I    = I    +2
        IDALE= IDALE+1
        GOTO 98
      ENDIF
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '=========== END SPECTRUM INTERPOLATION ============'
      WRITE(LU,*) '==================================================='
      RETURN
      END


