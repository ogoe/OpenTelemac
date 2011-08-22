!                    ************************************
                     DOUBLE PRECISION FUNCTION T3D_DEBSCE
!                    ************************************
!
     &( TIME , I , DISCE )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE DISCHARGE FOR EVERY SOURCE POINT
!+               (CAN BE A FUNCTION OF TIME AND SPACE/DEPTH).
!
!note     NOMVEF AND NVEF ARE THE NAME AND LOGICAL UNIT OF THE SOURCE
!+         FILE IN TELEMAC-2D AND 3D.
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
!| DISCE          |-->| ARRAY OF DISCHARGES OF SOURCES.
!|                |   | READ IN THE PARAMETER FILE.
!|                |   | NAME OF DISCE IS QSCE IN TELEMAC-3D.
!| I              |-->| NUMBER OF THE SOURCE
!| TIME           |-->| TIME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!     USE DECLARATIONS_TELEMAC3D, ONLY: MAXSCE,AT,INFOGR,NSCE,T3D_FILES
      USE DECLARATIONS_TELEMAC3D
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
!     FIRST CALL, INITIALISES OK TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO N=1,NSCE
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF SOURCE FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OK(I).AND.T3D_FILES(T3DVEF)%NAME(1:1).NE.' ') THEN
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
          WRITE(LU,*) 'T3D_DEBSCE NOT PROGRAMMED'
          WRITE(LU,*) 'FOR MORE THAN 99 SOURCES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_SOURCES(T3D_DEBSCE,FCT,AT,T3D_FILES(T3DVEF)%LU,
     &                        INFOGR,OK(I))
!
      ENDIF
!
!     BEWARE: AN ERROR IN THE SOURCE FILE MAY REMAIN UNNOTICED
!     BECAUSE RESORTS HERE TO THE STEERING FILE
!
      IF(.NOT.OK(I).OR.T3D_FILES(T3DVEF)%NAME(1:1).EQ.' ') THEN
!
!       PROGRAMMABLE PART
!       DISCE IS TAKEN FROM THE STEERING FILE
!
!       GLOBAL NUMBER OF SOURCE I IS ISCE(I) IN TELEMAC-3D
        T3D_DEBSCE = DISCE(I)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
