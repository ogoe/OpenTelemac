!                    ****************************
                     DOUBLE PRECISION FUNCTION SL
!                    ****************************
!
     &( I , N )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL IMPOSED
!+                LIQUID BOUNDARIES.
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
      USE INTERFACE_TELEMAC2D, EX_SL => SL
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
!-----------------------------------------------------------------------
!
!     IF THE LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
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
          WRITE(LU,*) 'SL NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(SL,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I))
!
      ENDIF
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     SL IS READ FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        IF(NCOTE.GE.I) THEN
          SL = COTE(I)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) I
100       FORMAT(1X,/,1X,'SL : COTES IMPOSEES EN NOMBRE INSUFFISANT'
     &             ,/,1X,'     DANS LE FICHIER DES PARAMETRES'
     &             ,/,1X,'     IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,101) I
101       FORMAT(1X,/,1X,'SL: MORE PRESCRIBED ELEVATIONS ARE REQUIRED'
     &             ,/,1X,'     IN THE PARAMETER FILE'
     &             ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
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
