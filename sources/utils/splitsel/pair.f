      MODULE PAIR
        TYPE TYPE_PAIR
          DOUBLE PRECISION :: R1
          DOUBLE PRECISION :: R2
        END TYPE TYPE_PAIR
      CONTAINS
!
        LOGICAL FUNCTION PAIR_GT(P1,P2)
           ! BRIEF RESULT OF P1 > P2
           !
           ! PARAM P1 [IN] FIRST PAIR
           ! PARAM P2 [IN] SECOND PAIR
           TYPE(TYPE_PAIR), INTENT(IN) :: P1
           TYPE(TYPE_PAIR), INTENT(IN) :: P2

           IF(P1%R1.EQ.P2%R1) THEN
             PAIR_GT = P1%R2.GT.P2%R2
           ELSE
             PAIR_GT = P1%R1.GT.P2%R1
           ENDIF
        END FUNCTION
!
        SUBROUTINE PAIR_COPY(P1,P2)
           ! BRIEF RESULT OF P1 = P2
           !
           ! PARAM P1 [INOUT] FIRST PAIR
           ! PARAM P2 [IN] SECOND PAIR
           TYPE(TYPE_PAIR), INTENT(INOUT) :: P1
           TYPE(TYPE_PAIR), INTENT(IN) :: P2
           P1%R1 = P2%R1
           P1%R2 = P2%R2
        END SUBROUTINE
!
        SUBROUTINE SHELL (N, A, B)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)              :: N
      TYPE(TYPE_PAIR), INTENT(INOUT)          :: A(N)
      INTEGER, INTENT(OUT)             :: B(N)
!
      INTEGER                          :: I, J, INC
      TYPE(TYPE_PAIR)                       :: V
      INTEGER                          :: W
!
      INTEGER                          :: ALPHA
!
      WRITE(*,*) 'SORTING BY SHELLQ'
!
      ALPHA=2
!
      DO I=1,N
        B(I)=I
      ENDDO
!
      INC=1
 1    INC=ALPHA*INC+1
      IF (INC.LE.N) GOTO 1
 2    CONTINUE
        INC=INC/ALPHA
        DO I=INC+1,N
          CALL PAIR_COPY(V,A(I))
          !V=A(I)
          W=B(I)
          J=I
 3        IF (PAIR_GT(A(J-INC),V)) THEN
            CALL PAIR_COPY(A(J),A(J-INC))
            B(J)=B(J-INC)
            J=J-INC
            IF (J.LE.INC) GOTO 4
          GOTO 3
          ENDIF
 4        CALL PAIR_COPY(A(J),V)
          B(J)=W
        ENDDO
!
      IF (INC.GT.1) GOTO 2
!
      RETURN
      END SUBROUTINE
      END MODULE
