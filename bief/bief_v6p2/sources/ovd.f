!                    **************
                     SUBROUTINE OVD
!                    **************
!
     & ( OP , X , Y , Z , C , NPOIN , IOPT , D , EPS )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON VECTORS INCLUDING DIVISIONS
!+                DIVISION BY 0 CAN BE TESTED.
!+
!+            IN THE EVENT OF A DIVIDE CHECK, CAN EITHER STOP THE
!+                PROGRAM OR SET THE RESULT OF THE OPERATION TO
!+                A VALUE: D.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   OP = 'X=1/Y   '     :  COPIES INVERSE OF Y IN X
!+   OP = 'X=Y/Z   '     :  DIVIDES Y BY Z
!+   OP = 'X=CY/Z  '     :  DIVIDES C.Y BY Z
!+   OP = 'X=CXY/Z '     :  DIVIDES C.X.Y BY Z
!+   OP = 'X=X+CY/Z'     :  ADDS C.Y/Z TO X
!
!warning  DIVIDE OPERATIONS INTERNALLY TAKE CARE OF DIVISIONS BY 0.
!+            SUCCESSFUL EXIT OF OVD IS THEREFORE NOT A PROOF THAT Y
!+            OR Z NEVER ARE 0
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        26/11/93
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
!| C              |-->| A GIVEN CONSTANT
!| D              |-->| A DIAGONAL MATRIX
!| EPS            |-->| THRESHOLD TO AVOID DIVISIONS BY ZERO
!| IOPT           |-->| OPTION FOR DIVISIONS BY ZERO
!|                |   | 1: NO TEST DONE (WILL CRASH IF DIVISION BY 0.).
!|                |   | 2: INFINITE TERMS REPLACED BY CONSTANT INFINI.
!|                |   | 3: STOP IF DIVISION BY ZERO.
!|                |   | 4: DIVISIONS BY 0. REPLACED BY DIVISIONS/ZERO
!|                |   |    ZERO BEING AN OPTIONAL ARGUMENT
!| NPOIN          |-->| SIZE OF VECTORS
!| OP             |-->| STRING INDICATING THE OPERATION TO BE DONE
!| X              |<--| RESULTING VECTOR 
!| Y              |-->| TO BE USED IN THE OPERATION
!| Z              |-->| TO BE USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: NPOIN,IOPT
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN),Z(NPOIN),C,D,EPS
      CHARACTER(LEN=8), INTENT(IN)    :: OP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=1/Y   ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO 30 I=1,NPOIN
            X(I) = 1.D0/Y(I)
30      CONTINUE
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO 31 I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSE
            X(I) = D
          ENDIF
!
31      CONTINUE
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO 32 I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
32      CONTINUE
!
        ELSEIF(IOPT.EQ.4) THEN
!
        DO 33 I=1,NPOIN
!
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSEIF (Y(I).GE.0.D0) THEN
            X(I) =  1.D0/EPS
          ELSE
            X(I) = -1.D0/EPS
          ENDIF
!
33      CONTINUE
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=Y/Z   ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO 40 I=1,NPOIN
            X(I) = Y(I) / Z(I)
40      CONTINUE
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO 41 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
41      CONTINUE
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO 42 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
42      CONTINUE
!>>>>
        ELSEIF(IOPT.EQ.4) THEN
!
        DO 43 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSEIF (ABS(Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  Y(I) / EPS
          ELSE
            X(I) = -Y(I) / EPS
          ENDIF
!
43      CONTINUE
!<<<<
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CY/Z  ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO 50 I=1,NPOIN
            X(I) = C*Y(I) / Z(I)
50      CONTINUE
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO 51 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
51      CONTINUE
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO 52 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
52      CONTINUE
!>>>>
        ELSEIF(IOPT.EQ.4) THEN
!
        DO 53 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSEIF (ABS(C*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  C*Y(I) / EPS
          ELSE
            X(I) = -C*Y(I) / EPS
          ENDIF
!
53      CONTINUE
!<<<<
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CXY/Z ') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO 60 I=1,NPOIN
            X(I) = C*X(I)*Y(I) / Z(I)
60      CONTINUE
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO 61 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
61      CONTINUE
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO 62 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
62      CONTINUE
!>>>>
        ELSEIF(IOPT.EQ.4) THEN
!
        DO 63 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSEIF (ABS(C*X(I)*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  C*Y(I) / EPS
          ELSE
            X(I) = -C*Y(I) / EPS
          ENDIF
!
63      CONTINUE
!<<<<
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CY/Z') THEN
!
        IF(IOPT.EQ.1) THEN
!
        DO 70 I=1,NPOIN
            X(I) = X(I) + C * Y(I) / Z(I)
70      CONTINUE
!
        ELSEIF(IOPT.EQ.2) THEN
!
        DO 71 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C * Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
!
71      CONTINUE
!
        ELSEIF(IOPT.EQ.3) THEN
!
        DO 72 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C * Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
!
72      CONTINUE
!>>>>
        ELSEIF(IOPT.EQ.4) THEN
!
        DO 73 I=1,NPOIN
!
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C*Y(I) / Z(I)
          ELSEIF (ABS(C*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) = X(I) + C*Y(I) / EPS
          ELSE
            X(I) = X(I) - C*Y(I) / EPS
          ENDIF
!
73      CONTINUE
!<<<<
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,3000) OP
         IF (LNG.EQ.2) WRITE(LU,4000) OP
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
1000     FORMAT(1X,'OVD (BIEF) : DIVISION PAR ZERO AU POINT ',1I6,
     &             ' LORS DE L''OPERATION ',A8,/,1X,
     &             'LE CRITERE EST ',G16.7)
2000     FORMAT(1X,'OVD (BIEF) : DIVIDE BY ZERO AT POINT ',1I6,
     &             ' FOR OPERATION ',A8,/,1X,
     &             'THE CRITERION IS ',G16.7)
3000     FORMAT(1X,'OVD (BIEF) : OPERATION INCONNUE : ',A8)
4000     FORMAT(1X,'OVD (BIEF) : UNKNOWN OPERATION: ',A8)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
