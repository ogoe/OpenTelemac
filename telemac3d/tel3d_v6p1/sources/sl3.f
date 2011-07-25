!                    *****************************
                     DOUBLE PRECISION FUNCTION SL3
!                    *****************************
!
     &( I , TIME , N , ENTET )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL
!+                IMPOSED LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNH)
!+        12/12/00
!+        V5P9
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
!| ENTET          |-->| IF YES, LISTING PRINTOUTS ALLOWED
!| I              |-->| NUMBER OF LIQUID BOUNDARY
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
      INTEGER          , INTENT(IN) :: I,N
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER J
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
!-----------------------------------------------------------------------
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OK(I).AND.T3D_FILES(T3DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT(1:3)='SL('
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:8)=')   '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:8)=')  '
        ELSE
          CALL PLANTE(1)
          STOP 'SL3 NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(SL3,FCT,TIME,T3D_FILES(T3DIMP)%LU,
     &                      ENTET,OK(I))
!
      ENDIF
!
      IF(.NOT.OK(I).OR.T3D_FILES(T3DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     SL IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        SL3 = COTIMP(I)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
