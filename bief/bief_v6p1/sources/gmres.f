!                    ****************
                     SUBROUTINE GMRES
!                    ****************
!
     & (X,A,B,MESH,R0,V,AV,CFG,INFOGR,AUX)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES A LINEAR SYSTEM A X = B
!+                USING THE GMRES (GENERALISED MINIMUM RESIDUAL) METHOD.
!code
!+  ALGORITHM:
!+
!+        |
!+        |   INITIALISATION
!+        |   ---------------
!+        |
!+        |   R0 = B - A X0
!+        |
!+        |   V1 = R0 / IIR0II
!+        |
!+        |   K = DIMENSION OF THE KRYLOV SPACE
!+        |
!+        |
!+        |
!+        |
!+        |   ITERATIONS
!+        |   ----------
!+        |
!+        | 1) BUILDS AN ORTHONORMAL BASE (VI) OF DIMENSION K+1
!+        |
!+        |   I = 1,...,K
!+        |
!+        |       VI+1 = A * VI
!+        |
!+        |       J = 1,...,I
!+        |
!+        |       HI+1,J = ( VI+1 , VJ )   HESSENBERG MATRIX (K+1,K)
!+        |
!+        |       VI+1 <--- VI+1  -  HI+1,J * VJ
!+        |
!+        |       VI+1 = VI+1 / IIVI+1II
!+        |
!+        |
!+        | 2) MULTIPLIES H BY Q  =  RK * RK-1 * ... * R1
!+        |
!+        |  GIVENS' ROTATION MATRIX RI :
!+        |                                    -                     -
!+        |                                   I ID(J-1)               I
!+        |                                   I                       I
!+        |                                   I        CJ  SJ         I
!+        |                              RI = I       -SJ  CJ         I
!+        |                                   I                       I
!+        |                                   I              ID(K-J)  I
!+        |                                    -                     -
!+        |
!+        |                         2    2
!+        |       WITH      CJ + SJ  =  1
!+        |
!+        |                 CJ AND SJ SUCH THAT H BECOMES TRIANGULAR
!+        |
!+        |                 ID(J-1) :    IDENTITY MATRIX J-1*J-1
!+        |
!+        |
!+        | 3) SOLVES THE SYSTEM H Y = Q E
!+        |                         -      -
!+        |       E VECTOR (NR0,0,0,0,0,0,.....)
!+        |
!+        |       NR0 NORM OF THE RESIDUAL
!+        |
!+        |
!+        |
!+        | 4) COMPUTES X(M+1) = X(M)  +  V * Y
!+        |
!+        |  V : MATRIX (3*NPOIN,K) WHICH COLUMNS ARE THE VJ
!+        |
!+        | 5) CHECKS THE RESIDUAL...
!+        |
!
!warning  CROUT AND GSEBE PRECONDITIONING ARE TREATED HERE LIKE A
!+            LU PRECONDITIONING. FOR CROUT IT MEANS THAT THE DIAGONAL
!+            OF THE PRECONDITIONING MATRIX IS NOT TAKEN INTO ACCOUNT
!code
!+-----------------------------------------------------------------------
!+                        PRECONDITIONING
!+-----------------------------------------------------------------------
!+    PRECON VALUE     I                  MEANING
!+-----------------------------------------------------------------------
!+        0 OR 1       I  NO PRECONDITIONING
!+                     I
!+        2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!+                     I  DIAGONAL
!+                     I
!+        3            I  BLOCK-DIAGONAL PRECONDITIONING
!+                     I
!+        5            I  DIAGONAL PRECONDITIONING USING THE ABSOLUTE
!+                     I  VALUE OF THE MATRIX DIAGONAL
!+                     I
!+        7            I  CROUT EBE PRECONDITIONING
!+                     I
!+       11            I  GAUSS-SEIDEL EBE PRECONDITIONING
!+                     I
!+-----------------------------------------------------------------------
!
!history  C MOULIN (LNH)
!+        26/08/93
!+
!+
!
!history  J-M  HERVOUET  (LNH)
!+        24/04/97
!+        V5P6
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
!| A              |-->| MATRIX OF THE SYSTEM
!| AUX            |-->| MATRIX FOR PRECONDITIONING.
!| AV             |<->| WORK ARRAY
!| B              |-->| RIGHT-HAND SIDE OF THE SYSTEM
!| CFG            |-->| STRUCTURE OF SOLVER CONFIGURATION
!| INFOGR         |-->| IF YES, PRINT A LOG.
!| MESH           |-->| MESH STRUCTURE.
!| R              |<->| RESIDUAL
!| V              |<->| WORK ARRAY 
!| X              |<->| INITIAL VALUE, THEN SOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_GMRES => GMRES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(SLVCFG), INTENT(INOUT)    :: CFG
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: B
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: X,V,AV,R0
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: AUX
      LOGICAL, INTENT(IN)            :: INFOGR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!  MAXIMUM CFG%KRYLOV=20
!
      DOUBLE PRECISION H(21,20),C(20),S(20),E(21),BID
!
      DOUBLE PRECISION R,ZZ,NB,PREC,NR0
!
      INTEGER I,J,K,L,M
!
      LOGICAL RELAT,CROUT,GSEB,PRECO
!
      INTRINSIC SQRT,ABS
!
!-----------------------------------------------------------------------
!
      K = CFG%KRYLOV
!
      CROUT=.FALSE.
      IF(MOD(CFG%PRECON,7).EQ.0) CROUT=.TRUE.
      GSEB=.FALSE.
      IF(MOD(CFG%PRECON,11).EQ.0) GSEB=.TRUE.
      PRECO=.FALSE.
      IF(CROUT.OR.GSEB.OR.MOD(CFG%PRECON,13).EQ.0) PRECO=.TRUE.
!
      IF(PRECO) THEN
!                  -1
!       COMPUTES  L   B
        CALL GODOWN(B, AUX,B,'D',MESH,.FALSE.)
      ENDIF
!
! INITIALISES
!
      NB = P_DOTS(B,B,MESH)
      NB = SQRT(NB)
!
      RELAT = .TRUE.
      IF(NB.LT.1.D0) THEN
        NB = 1.D0
        RELAT = .FALSE.
      ENDIF
!
      M = 0
!
! INITIALISES THE RESIDUAL R : A X0 - B
!
      CALL MATRBL( 'X=AY    ',R0,A,X,BID,MESH)
!
      IF(PRECO) THEN
        CALL GODOWN(R0, AUX,R0,'D',MESH,.FALSE.)
        CALL PUOG(X,AUX,X, 'D',MESH,.FALSE.)
      ENDIF
!
      CALL OS( 'X=X-Y   ' , R0 , B , B , BID )
!
! CHECKS THAT THE ACCURACY IS NOT ALREADY REACHED
!
      NR0=P_DOTS(R0,R0,MESH)
      NR0=SQRT(NR0)
      PREC = NR0/NB
!
      IF (PREC.LE.CFG%EPS) GO TO 3000
!
!-----------------------------------------------------------------------
!                   ITERATIONS LOOP
!-----------------------------------------------------------------------
!
20    CONTINUE
!
      M = M+1
!
! COMPUTES THE VECTOR V1 = - R0 / NORM(R0)
! (- SIGN BECAUSE R = AX - B INSTEAD OF B - AX)
!
      CALL OS('X=CY    ', V%ADR(1)%P , R0 , R0 , -1.D0/NR0 )
!
!-----------------------------------------------------------------------
!         COMPUTES THE ORTHONORMAL BASE AND MATRIX H
!-----------------------------------------------------------------------
!
!     K-1 1ST COLUMNS
!
      DO 10 J=1,K-1
!
        IF(PRECO) THEN
          CALL GOUP(B , AUX , V%ADR(J)%P ,'D',MESH,.TRUE.)
          CALL MATRBL( 'X=AY    ',AV%ADR(J)%P,A,B,BID,MESH)
          CALL GODOWN(AV%ADR(J)%P,AUX,AV%ADR(J)%P,'D',MESH,.FALSE.)
        ELSE
          CALL MATRBL( 'X=AY    ',AV%ADR(J)%P,A,V%ADR(J)%P,BID,MESH)
        ENDIF
!
        CALL OS('X=Y     ', V%ADR(J+1)%P ,AV%ADR(J)%P , X , BID )
!
        DO 30 I = 1,J
!
          H(I,J) = P_DOTS( V%ADR(J+1)%P , V%ADR(I)%P , MESH )
!
          CALL OS('X=X+CY  ',V%ADR(J+1)%P,V%ADR(I)%P,V%ADR(I)%P,-H(I,J))
!
30      CONTINUE
!
         H(J+1,J)=P_DOTS(V%ADR(J+1)%P,V%ADR(J+1)%P,MESH)
         H(J+1,J) = SQRT( H(J+1,J) )
!
         CALL OS('X=CX    ',V%ADR(J+1)%P, B, B, 1.D0/H(J+1,J))
!
10    CONTINUE
!
! K-TH COLUMN (VECTOR V(K+1) IS NOT COMPLETELY BUILT)
!
        IF(PRECO) THEN
          CALL GOUP(B , AUX , V%ADR(K)%P , 'D' ,MESH,.TRUE.)
          CALL MATRBL( 'X=AY    ',AV%ADR(K)%P,A,B,BID,MESH)
          CALL GODOWN(AV%ADR(K)%P,AUX,AV%ADR(K)%P,'D',MESH,.FALSE.)
        ELSE
          CALL MATRBL( 'X=AY    ',AV%ADR(K)%P,A,V%ADR(K)%P,BID,MESH)
        ENDIF
!
        H(K+1,K) = P_DOTS( AV%ADR(K)%P , AV%ADR(K)%P , MESH )
!
        DO 31 I = 1,K
!
          H(I,K) = P_DOTS( AV%ADR(K)%P , V%ADR(I)%P , MESH )
          H(K+1,K) = H(K+1,K) - H(I,K)**2
!
31      CONTINUE
!       IN THEORY H(K+1,K) IS POSITIVE
!       TO MACHINE ACCURACY
        H(K+1,K) = SQRT( ABS(H(K+1,K)) )
!
!-----------------------------------------------------------------------
! BUILDS GIVENS' ROTATIONS AND APPLIES TO H AND E
!-----------------------------------------------------------------------
!
!     OTHER COMPONENTS FOR E ARE 0
      E(1) = NR0
!
!  ROTATIONS IN RANGE [1 - K]
!
      DO 40 I = 1 , K
!
!     MODIFIES COLUMN I OF H BY THE PREVIOUS ROTATIONS
      IF(I.GE.2) THEN
        DO 41 J = 1,I-1
          ZZ       =  C(J) * H(J,I) + S(J) * H(J+1,I)
          H(J+1,I) = -S(J) * H(J,I) + C(J) * H(J+1,I)
          H(J,I) = ZZ
41      CONTINUE
      ENDIF
!     MODIFIES COLUMN I OF H BY ROTATION I
      R = SQRT( H(I,I)**2 + H(I+1,I)**2 )
      IF(ABS(R).LT.1.D-6) THEN
        IF(INFOGR) THEN
          IF (LNG.EQ.1) WRITE(LU,91) R
          IF (LNG.EQ.2) WRITE(LU,92) R
        ENDIF
        GO TO 3000
      ENDIF
      C(I) =  H(I,I)   / R
      S(I) =  H(I+1,I) / R
      H(I,I) = R
!     H(I+1,I) = 0.D0    (WILL NOT BE USED AGAIN)
!     MODIFIES VECTOR E
      E(I+1) = -S(I) * E(I)
      E(I  ) =  C(I) * E(I)
!
40    CONTINUE
!
!-----------------------------------------------------------------------
! SOLVES SYSTEM H*Y = E     (H UPPER TRIANGULAR OF DIMENSION K)
!                            Y SAME AS E
!
! H(I,I) <> 0 HAS ALREADY BEEN CHECKED ON R
!-----------------------------------------------------------------------
!
      E(K) = E(K) / H(K,K)
      DO 120 J = K-1,1,-1
      DO 130 L = J+1,K
        E(J) = E(J) - H(J,L) * E(L)
130   CONTINUE
      E(J) = E(J) / H(J,J)
120   CONTINUE
!
!-----------------------------------------------------------------------
! BUILDS THE SOLUTION FOR STEP M : X(M+1) = X(M) + VK * Y(K)
!-----------------------------------------------------------------------
!
      DO 150 L = 1,K
!
!  COMPUTES THE NEW SOLUTION X
!
        CALL OS('X=X+CY  ', X , V%ADR(L)%P , X , E(L) )
!
!  COMPUTES THE RESIDUAL : RM+1 = RM + A * VK * ZK
!
        CALL OS('X=X+CY  ', R0 , AV%ADR(L)%P , R0 , E(L) )
!
150   CONTINUE
!
! CHECKS THAT THE ACCURACY IS NOT ALREADY REACHED
! THE RESIDUAL NORM IS GIVEN BY ABS(E(K+1))
!
!     NR0  =NORM OF R0
      NR0  = ABS(E(K+1))
      PREC = NR0/NB
!
      IF (PREC.GE.CFG%EPS.AND.M.LT.CFG%NITMAX)  GOTO 20
!
3000  CONTINUE
!
!-----------------------------------------------------------------------
!
      IF(PRECO) THEN
        CALL GOUP( X,AUX,X,'D',MESH,.FALSE.)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IF(INFOGR) THEN
        IF(M.LT.CFG%NITMAX) THEN
          IF(INFOGR) THEN
          IF(RELAT) THEN
            IF (LNG.EQ.1) WRITE(LU,101) M,PREC
            IF (LNG.EQ.2) WRITE(LU,102) M,PREC
          ELSE
            IF (LNG.EQ.1) WRITE(LU,201) M,PREC
            IF (LNG.EQ.2) WRITE(LU,202) M,PREC
          ENDIF
          ENDIF
        ELSE
          IF(RELAT) THEN
            IF (LNG.EQ.1) WRITE(LU,103) M,PREC
            IF (LNG.EQ.2) WRITE(LU,104) M,PREC
          ELSE
            IF (LNG.EQ.1) WRITE(LU,203) M,PREC
            IF (LNG.EQ.2) WRITE(LU,204) M,PREC
          ENDIF
        ENDIF
!     ENDIF
!
      RETURN
!
!-----------------------------------------------------------------------
!
 91   FORMAT(1X,'GMRES (BIEF) : ECHEC DE L''ALGORITHME  R= ',G16.7,/,
     &       1X,'               SI LA MATRICE EST DIAGONALE, PRENDRE',/,
     &       1X,'               COMME SOLVEUR LE GRADIENT CONJUGUE.')
 92   FORMAT(1X,'GMRES (BIEF) : ALGORITHM FAILED  R=',G16.7,/,
     &       1X,'               IF THE MATRIX IS DIAGONAL, CHOOSE',/,
     &       1X,'               THE CONJUGATE GRADIENT SOLVER.')
101   FORMAT(1X,'GMRES (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION RELATIVE:',G16.7)
102   FORMAT(1X,'GMRES (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
201   FORMAT(1X,'GMRES (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION ABSOLUE :',G16.7)
202   FORMAT(1X,'GMRES (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
103   FORMAT(1X,'GMRES (BIEF) : MAX D'' ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION RELATIVE:',G16.7)
104   FORMAT(1X,'GMRES (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
203   FORMAT(1X,'GMRES (BIEF) : MAX D'' ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION ABSOLUE :',G16.7)
204   FORMAT(1X,'GMRES (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISION:',G16.7)
!
!-----------------------------------------------------------------------
!
      END
