!                    *****************************
                     DOUBLE PRECISION FUNCTION VIT
!                    *****************************
!
     &( I , N )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE VELOCITY FOR VEL IMPOSED LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
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
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_VIT => VIT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER J
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, OK INITIALISED TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO J=1,MAXFRO
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF THE LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE VIT(1), VIT(2), ETC, VIT(99), DEPENDING ON I
        FCT(1:4)='VIT('
        IF(I.LT.10) THEN
          WRITE(FCT(5:5),FMT='(I1)') I
          FCT(6:8)=')  '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(5:6),FMT='(I2)') I
          FCT(7:8)=') '
        ELSE
          STOP 'VIT NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(VIT,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I))
!
      ENDIF
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     VIT IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        IF(NVITES.GE.I) THEN
          VIT = VITES(I)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) I
200       FORMAT(1X,/,1X,'VIT : VITESSES IMPOSEES EN NOMBRE INSUFFISANT'
     &             ,/,1X,'      DANS LE FICHIER DES PARAMETRES'
     &             ,/,1X,'      IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) I
201       FORMAT(1X,/,1X,'VIT : MORE PRESCRIBED VELOCITIES ARE REQUIRED'
     &             ,/,1X,'      IN THE PARAMETER FILE'
     &             ,/,1X,'      AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
