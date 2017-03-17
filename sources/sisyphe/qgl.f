!                    *****************************
                     DOUBLE PRECISION FUNCTION QGL
!                    *****************************
!
     &(I,AT)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PRESCRIBES THE SOLID DISCHARGE FOR  IMPOSED
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size FCT due to modification of TRACER numbering
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER IN THE ORIGINAL MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_SPECIAL
!
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I
      DOUBLE PRECISION, INTENT(IN):: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
!
!-----------------------------------------------------------------------
!
!     IF THE LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK IS SET  TO .FALSE.
!
      IF(OKQGL(I).AND.SIS_FILES(SISLIQ)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE QGL(1), QGL(2), ETC, QGL(9), DEPENDING ON I
        FCT='QG(      '
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:5)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*)'I=',I
          WRITE(LU,*) 'QGL NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
!
        CALL READ_FIC_CONC(QGL,FCT,AT,SIS_FILES(SISLIQ)%LU,
     &                     ENTET,OKQGL(I))
!
      ENDIF
!
      IF(.NOT.OKQGL(I).OR.SIS_FILES(SISLIQ)%NAME(1:1).EQ.' ') THEN
!       PROGRAMMABLE PART
!       SL IS READ FROM THE STEERING FILE, BUT MAY BE CHANGED
        IF(LNG.EQ.1) WRITE(LU,10 0) I
100     FORMAT(1X,/,1X,'QG : CONC IMPOSEES EN NOMBRE INSUFFISANT'
     &           ,/,1X,'     DANS LE FICHIER DES PARAMETRES'
     &           ,/,1X,'     IL EN FAUT AU MOINS : ',1I6)
        IF(LNG.EQ.2) WRITE(LU,101) I
101     FORMAT(1X,/,1X,'QG: MORE PRESCRIBED CONCENTRATIONS ARE REQUIRED'
     &           ,/,1X,'     IN THE PARAMETER FILE'
     &           ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
