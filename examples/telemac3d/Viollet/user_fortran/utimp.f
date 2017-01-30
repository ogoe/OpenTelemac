!                    ****************
                     SUBROUTINE UTIMP
!                    ****************
!
     & (LT,TIME,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRINTS OUT SPECIFIC RESULTS
!+               (SEE THE LIST OF VARIABLES IN DECLARATIONS_TELEMAC3D).
!+
!+            FOR BIEF_OBJ STRUCTURES, THE DOUBLE PRECISION ARRAY
!+                IS IN COMPONENT R, E.G. U%R FOR THE VELOCITY.
!
!history  C LE NORMANT(LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P2
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| GRADEBL        |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| GRAPRDL        |-->| GRAPHIC PRINTOUT PERIOD
!| LISDEBL        |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| LISPRDL        |-->| LISTING PRINTOUT PERIOD
!| LT             |-->| ITERATION NUMBER
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: LT,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MOD

!
!***********************************************************************
! USER OUTPUT ACCORDING TO THE VALUES: TO BE IMPLEMENTED
!
!
! BEGIN OF PART SPECIFIC TO THIS CASE
      IF(LT.EQ.NIT) THEN
        CALL WRITE_NRFO(T3D_FILES(T3DRFO)%LU,VARSO3,TEXT3,NPOIN3,
     &                  NPOIN2,NPLAN,MAXVA3,X,Y)
      ENDIF
! END OF PART SPECIFIC TO THIS CASE
!
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP
!                    *********************
                     SUBROUTINE WRITE_NRFO
!                    *********************
!
     &(NRFO,VARSOR,TEXT,NPOIN3,NPOIN2,NPLAN,MAXTAB,X,Y)
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!
!brief    WRITES IN A FORMATTED FILE THE VELOCITIES U AND THE SCALAR
!         AT 3 POINTS OF THE SPACE
!         THIS WRITING IS NECESSARY TO DRAW COMPARISON CURVES
!         WITH PIERRE-LOUIS VIOLLET S EXPERIENCES
!
!history  B. DELHOM (INCKA PREST)
!+        15/10/2009
!+        V6P0
!+   Creation IN VERSION 5.9
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXTAB         |-->| MAXIMUM NUMBER OF VARIABLES
!| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| NRFO           |-->| LOGICAL UNIT FOR FORMATTED RESULT FILE
!| VARSOR         |<->| RESULTS OF THE COMPUTATION
!| TEXT           |-->| NAMES FO VARIABLES OF THE COMPUTATION
!| X              |-->| ABSCISSA COORDONATES
!| Y              |-->| ORDINATE COORDONATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NRFO,NPOIN3,NPOIN2,NPLAN,MAXTAB
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3), Y(NPOIN3)
!
      CHARACTER(LEN=32), INTENT(IN) :: TEXT(MAXTAB)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,I,IPOIN2,IPLAN,INUM(3),I0,IT,IU
      DOUBLE PRECISION EPS,Z_RED,TEMP(NPLAN),U0(NPLAN),T_MIN,T_MAX,U1,
     &                 DTEMP
      DOUBLE PRECISION VITU1(NPLAN),VITU2(NPLAN),VITU3(NPLAN),
     &                 TEMP1(NPLAN),TEMP2(NPLAN),TEMP3(NPLAN)
!
      CHARACTER(LEN=10) TEXT1
!
      DATA EPS / 1.D-6 /
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL P_DMAX,P_DMIN
!
!-----------------------------------------------------------------------
!
      I0 = 0
      INUM(1) = 0
      INUM(2) = 0
      INUM(3) = 0
!
      DO IPLAN=1,NPLAN
        U0(IPLAN)   = 0.D0
        TEMP(IPLAN) = 0.D0
        VITU1(IPLAN) = 0.D0
        VITU2(IPLAN) = 0.D0
        VITU3(IPLAN) = 0.D0
        TEMP1(IPLAN) = 0.D0
        TEMP2(IPLAN) = 0.D0
        TEMP3(IPLAN) = 0.D0
      ENDDO
!
      DO IPOIN2=1,NPOIN2
        IF ((ABS(X(IPOIN2)-0.D0) .LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) I0 = IPOIN2
        IF ((ABS(X(IPOIN2)-1.D0) .LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) INUM(1) = IPOIN2
        IF ((ABS(X(IPOIN2)-3.D0) .LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) INUM(2) = IPOIN2
        IF ((ABS(X(IPOIN2)-10.D0).LT.EPS).AND.
     &      (ABS(Y(IPOIN2)-0.5D0).LT.EPS)) INUM(3) = IPOIN2
      ENDDO

      DO IVAR=1,MAXTAB
        TEXT1 = TEXT(IVAR)
        IF (TEXT1=='VELOCITY U') IU = IVAR
        IF (TEXT1=='TEMPERATUR') IT = IVAR
      ENDDO

      IF(I0.NE.0) THEN
        DO IPLAN=1,NPLAN
          U0(IPLAN)   = VARSOR%ADR(IU)%P%R(I0+(IPLAN-1)*NPOIN2)
          TEMP(IPLAN) = VARSOR%ADR(IT)%P%R(I0+(IPLAN-1)*NPOIN2)
        ENDDO
      ENDIF
      T_MIN = MINVAL(TEMP)
      T_MAX = MAXVAL(TEMP)
!
      IF(NCSIZE.GT.1) THEN
        T_MIN = P_DMAX(T_MIN) + P_DMIN(T_MIN)
        T_MAX = P_DMAX(T_MAX) + P_DMIN(T_MAX)
      ENDIF
!
      DTEMP = T_MAX-T_MIN
!      U1    = MAXVAL(U0)/2.D0
      U1    = 1.D0/30.D0
!
      IF(INUM(1).NE.0) THEN
        DO IPLAN=1,NPLAN
          VITU1(IPLAN) = VARSOR%ADR(IU)%P%R(INUM(1)+(IPLAN-1)*NPOIN2)/U1
          TEMP1(IPLAN) = ( VARSOR%ADR(IT)%P%R(INUM(1)+(IPLAN-1)*NPOIN2)
     &                    -T_MIN)/DTEMP
        ENDDO
      ENDIF
!
      IF(INUM(2).NE.0) THEN
        DO IPLAN=1,NPLAN
          VITU2(IPLAN) = VARSOR%ADR(IU)%P%R(INUM(2)+(IPLAN-1)*NPOIN2)/U1
          TEMP2(IPLAN) = ( VARSOR%ADR(IT)%P%R(INUM(2)+(IPLAN-1)*NPOIN2)
     &                    -T_MIN)/DTEMP
        ENDDO
      ENDIF
!
      IF(INUM(3).NE.0) THEN
        DO IPLAN=1,NPLAN
          VITU3(IPLAN) = VARSOR%ADR(IU)%P%R(INUM(3)+(IPLAN-1)*NPOIN2)/U1
          TEMP3(IPLAN) = ( VARSOR%ADR(IT)%P%R(INUM(3)+(IPLAN-1)*NPOIN2)
     &                    -T_MIN)/DTEMP
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        DO IPLAN=1,NPLAN
          VITU1(IPLAN) = P_DMAX(VITU1(IPLAN)) +  P_DMIN(VITU1(IPLAN))
          VITU2(IPLAN) = P_DMAX(VITU2(IPLAN)) +  P_DMIN(VITU2(IPLAN))
          VITU3(IPLAN) = P_DMAX(VITU3(IPLAN)) +  P_DMIN(VITU3(IPLAN))
          TEMP1(IPLAN) = P_DMAX(TEMP1(IPLAN)) +  P_DMIN(TEMP1(IPLAN))
          TEMP2(IPLAN) = P_DMAX(TEMP2(IPLAN)) +  P_DMIN(TEMP2(IPLAN))
          TEMP3(IPLAN) = P_DMAX(TEMP3(IPLAN)) +  P_DMIN(TEMP3(IPLAN))
        ENDDO
      ENDIF
!
      DO IPLAN=1,NPLAN
        Z_RED = (IPLAN-1)
        Z_RED = 2.D0*DBLE(IPLAN-1)/DBLE(NPLAN-1)
        WRITE(NRFO,1001)Z_RED,
     &                   VITU1(IPLAN),VITU2(IPLAN),VITU3(IPLAN),
     &                   TEMP1(IPLAN),TEMP2(IPLAN),TEMP3(IPLAN)
      ENDDO


 1001 FORMAT(F7.4,F7.4,F7.4,F7.4,F7.4,F7.4,F7.4)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

