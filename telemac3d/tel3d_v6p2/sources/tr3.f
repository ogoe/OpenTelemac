!                    *****************************
                     DOUBLE PRECISION FUNCTION TR3
!                    *****************************
!
     &( I , ITRAC , N , TIME , ENTET )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE TRACER  FOR TRACER IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        08/04/09
!+        V6P0
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
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS PRINTED
!| I              |-->| LIQUID BOUNDARY NUMBER
!| ITRAC          |-->| TRACER NUMBER
!| N              |-->| GLOBAL NUMBER OF POINT
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: I,ITRAC,N
      DOUBLE PRECISION, INTENT(IN) :: TIME
      LOGICAL, INTENT(IN)          :: ENTET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER J,IRANK
      LOGICAL DEJA,OK(99)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, INITIALISES OK TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO J=1,99
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
!     RANK OF VALUE IN ARRAY TRACER OR IN LIQUID BOUNDARY FILE
!
      IRANK=ITRAC+(I-1)*NTRAC
      IF(IRANK.GT.99) THEN
        WRITE(LU,*) 'CHANGE DIMENSION OF OK IN TR3, ',IRANK,
     &              ' AT LEAST REQUIRED, IN FACT NFRLIQ*NTRAC'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(OK(IRANK).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE TR(1), TR(2), ETC, TR(99), DEPENDING ON IRANK
        FCT(1:3)='TR('
        IF(IRANK.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') IRANK
          FCT(5:8)=')   '
        ELSEIF(IRANK.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') IRANK
          FCT(6:8)=')  '
        ELSE
          WRITE(LU,*) 'TR3 NOT PROGRAMMED FOR MORE THAN 99 VALUES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(TR3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OK(IRANK))
!
      ENDIF
!
      IF(.NOT.OK(IRANK).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     TRACER IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!     (FIRST THE NTRAC VALUES OF LIQUID BOUNDARY 1, ETC.)
!
        TR3 = TRACER(IRANK)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
