      MODULE PAIR
        TYPE TYPE_PAIR
          double precision :: r1
          double precision :: r2
        END TYPE TYPE_PAIR
      contains
!       
        logical function pair_gt(p1,p2)
           ! brief REsult of p1 > p2
           !       
           ! param p1 [in] First Pair
           ! param p2 [in] Second Pair
           type(type_pair), intent(in) :: p1
           type(type_pair), intent(in) :: p2
           
           if(p1%r1.eq.p2%r1) then
             pair_gt = p1%r2.gt.p2%r2
           else
             pair_gt = p1%r1.gt.p2%r1
           endif 
        end function
!
        subroutine pair_copy(p1,p2)
           ! brief REsult of p1 = p2
           !       
           ! param p1 [inout] First Pair
           ! param p2 [in] Second Pair
           type(type_pair), intent(inout) :: p1
           type(type_pair), intent(in) :: p2
           p1%r1 = p2%r1
           p1%r2 = p2%r2
        end subroutine
!
        SUBROUTINE SHELL (N, A, B)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN)              :: N
      TYPE(type_pair), INTENT(INOUT)          :: A(N)
      INTEGER, INTENT(OUT)             :: B(N)
!
      INTEGER                          :: I, J, INC
      TYPE(type_pair)                       :: V
      INTEGER                          :: W
!
      INTEGER                          :: ALPHA
!
      write(*,*) 'Sorting by SHELLQ'
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
            call pair_copy(v,A(I))
            !V=A(I)
            W=B(I)
            J=I
 3          IF (pair_gt(A(J-INC),V)) THEN
!3          IF (A(J-INC).GT.V) THEN
               call pair_copy(A(J),A(J-INC))
!              A(J)=A(J-INC)
               B(J)=B(J-INC)
               J=J-INC
               IF (J.LE.INC) GOTO 4
            GOTO 3
            ENDIF
 4          call pair_copy(A(J),v)
!4          A(J)=V
            B(J)=W
         ENDDO
!
      IF (INC.GT.1) GOTO 2
!
      RETURN
      END subroutine
      END MODULE
