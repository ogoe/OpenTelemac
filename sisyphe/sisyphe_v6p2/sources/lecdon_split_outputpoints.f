!                 ************************************
                  SUBROUTINE LECDON_SPLIT_OUTPUTPOINTS
!                 ************************************
!
     &(INT_LIST, POINT_ARRAY, FULLOUTPUT)
!
!***********************************************************************
! SISYPHE   V6P2                                  21/08/2010
!***********************************************************************
!
!brief    SPLITS A LIST OF INTEGERS and RETURNS AN ARRAY WITH THEM
!
!history  U.H. MERKEL
!+        03/05/2012
!+        V6P2
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| INT_LIST       |<->| STRING OF INTEGER NUMBERS
!| POINT_ARRAY    |<->| ARRAY with same INTEGERS
!| FULLOUTPUT     |<->| if FULLOUTPUT IS FOUND THIS IS TRUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=*), INTENT(INOUT) :: INT_LIST
      INTEGER, INTENT(INOUT) :: POINT_ARRAY(100)
      LOGICAL, INTENT(INOUT) :: FULLOUTPUT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
! INTERNAL VARIABLES:
!
      CHARACTER C(2)
      CHARACTER(LEN=8) MOT(100)
      INTEGER I,J,LONG,I1,I2,NMOT,L, iNTLU
      LOGICAL OK
!
      INTRINSIC LEN
!
!-----------------------------------------------------------------------
!
!  RECOGNISED SEPARATORS IN 'INT_LIST'
!
      C(1) = ';'
      C(2) = '|'
      LONG = LEN(INT_LIST)
      if (LONG.EQ.0) THEN
        WRITE(LU,*) 'Lecdon_SPLIT String Error'
        call PLANTE(1)
        STOP
      endif
!
      do I=1,LONG
        do J=1,2
          if(INT_LIST(I:I).EQ.C(J)) INT_LIST(I:I) = ' '
        enddo
      enddo
!
! 'INT_LIST' NOW IS MADE UP OF WORDS SEPARATED BY WHITE SPACES
!
      I1 = 0
      NMOT=0
!
 10   CONTINUE
      if (I1.GE.LONG) GOTO 30
      I1=I1+1
      if (INT_LIST(I1:I1).EQ.' ') GOTO 10
!
      I2=0
!
 20   CONTINUE
      I2=I2+1
      if (INT_LIST(I1+I2:I1+I2).NE.' ') GOTO 20
!
      NMOT=NMOT+1
      MOT(NMOT)=INT_LIST(I1:I1+I2)
      I1=I1+I2
      GOTO 10
!
30    CONTINUE
!
!     Builds The POINT_ARRAY
!
      FULLOUTPUT = .FALSE.
      do J=1,100
        POINT_ARRAY(J) = -1
      enddo
      do J=1,NMOT
        Read(Mot(j),* ) POINT_ARRAY(j)
        if (POINT_ARRAY(J).eq.0) FULLOUTPUT = .TRUE.
      enddo
!
!-----------------------------------------------------------------------
!
      RETURN
      END

