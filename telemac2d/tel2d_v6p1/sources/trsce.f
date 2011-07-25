!                    *******************************
                     DOUBLE PRECISION FUNCTION TRSCE
!                    *******************************
!
     &( TIME , I , ITRAC )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE TRACER VALUES AT THE SOURCES.
!+                THIS VALUE MAY VARY IN TIME.
!
!history  J-M HERVOUET (LNHE)
!+        08/04/2008
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
!| I              |-->| SOURCE RANK
!| ITRAC          |-->| TRACER RANK
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: AT,ENTET,NTRAC,TSCE,NREJET,
     &                                  T2D_FILES,T2DVEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER         , INTENT(IN) :: I,ITRAC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER N,IRANK
      LOGICAL DEJA,OK(99)  ! 99.GE.NREJET*NTRAC
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, OK INITIALISED TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        IF(NREJET*NTRAC.GT.99) THEN
          WRITE(LU,*) 'CHANGE DIMENSION OF OK IN TRSCE, ',NREJET*NTRAC,
     &                ' REQUIRED'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO N=1,NREJET*NTRAC
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF A SOURCE FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!
!     IRANK CORRESPONDS TO TELEMAC2D DOCUMENTATION
!     TRACER 1 OF SOURCE 1, TRACER 2 OF SOURCE 1, ETC.
      IRANK=ITRAC+NTRAC*(I-1)
      IF(OK(IRANK).AND.T2D_FILES(T2DVEF)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE T(1), T(2), ETC, T(99), DEPENDING ON I AND ITRAC
        FCT='TR(     '
        IF(IRANK.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') IRANK
          FCT(5:5)=')'
        ELSEIF(IRANK.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') IRANK
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 DATA'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(TRSCE,FCT,AT,T2D_FILES(T2DVEF)%LU,
     &                        ENTET,OK(IRANK))
!
      ENDIF
!
!     BEWARE, AN ERROR IN THE SOURCE FILE MAY REMAIN UNNOTICED
!     BECAUSE WE RESORT HERE TO THE STEERING FILE
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DVEF)%NAME(1:1).EQ.' ') THEN
!
!       PROGRAMMABLE PART
!       TSCE IS TAKEN FROM THE STEERING FILE
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
        TRSCE = TSCE(I,ITRAC)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
