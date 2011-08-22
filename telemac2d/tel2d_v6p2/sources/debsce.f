!                    ********************************
                     DOUBLE PRECISION FUNCTION DEBSCE
!                    ********************************
!
     &( TIME , I , DISCE )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE PRESCRIBED DISCHARGE OF EVERY SOURCE POINT.
!+
!+            VARIATIONS WRT TIME AND SPACE MAY BE IMPLEMENTED.
!
!note     T2DVEF IS THE SOURCES FILE IN TELEMAC-2D
!
!history  J-M HERVOUET (LNHE)
!+        03/04/2008
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
!| DISCE          |-->| ARRAY OF DISCHARGES OF SOURCES.
!|                |   | READ IN THE PARAMETER FILE.
!|                |   | NAME OF DISCE IS DSCE IN TELEMAC-2D.
!| I              |-->| NUMBER OF THE SOURCE
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: MAXSCE,AT,ENTET,NREJET,
     &                                  T2D_FILES,T2DVEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME,DISCE(*)
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER N
      LOGICAL DEJA,OK(MAXSCE)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, OK INITIALISED TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO N=1,NREJET
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF SOURCES FILE EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!
      IF(OK(I).AND.T2D_FILES(T2DVEF)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT(1:2)='Q('
        IF(I.LT.10) THEN
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:8)=')    '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:8)=')   '
        ELSE
          WRITE(LU,*) 'DEBSCE NOT PROGRAMMED FOR MORE THAN 99 SOURCES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(DEBSCE,FCT,AT,T2D_FILES(T2DVEF)%LU,
     &                        ENTET,OK(I))
!
      ENDIF
!
!     BEWARE, AN ERROR IN THE SOURCES FILE MAY REMAIN UNNOTICED
!     BECAUSE WE RESORT HERE TO THE PARAMETER FILE
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DVEF)%NAME(1:1).EQ.' ') THEN
!
!       PROGRAMMABLE PART
!       DISCE IS TAKEN IN THE PARAMETER FILE
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-2D
        DEBSCE = DISCE(I)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
