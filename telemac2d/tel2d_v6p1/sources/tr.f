!                    ****************************
                     DOUBLE PRECISION FUNCTION TR
!                    ****************************
!
     &( I , ITRAC , N , IERR )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRESCRIBES THE TRACER VALUES FOR TRACER IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        02/04/2009
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
!| I              |-->| BOUNDARY RANK
!| IERR           |<--| IF 0, OK, IF 1: PROBLEM
!| ITRAC          |-->| TRACER RANK
!| N              |-->| GLOBAL NUMBER OF POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_TR => TR
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: I,N,ITRAC
      INTEGER, INTENT(INOUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER J,IRANK
      LOGICAL DEJA,OK(MAXFRO*MAXTRA)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     A PRIORI ASSUMES THAT TR WILL BE FOUND
!
      IERR=0
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
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!     RANK OF VALUE IN ARRAY TRACER OR IN LIQUID BOUNDARY FILE
!
      IRANK=ITRAC+(I-1)*NTRAC
      IF(OK(IRANK).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE TR(1), TR(2), ETC, TR(99), DEPENDING ON I
        FCT(1:3)='TR('
        IF(IRANK.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') IRANK
          FCT(5:8)=')   '
        ELSEIF(IRANK.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') IRANK
          FCT(6:8)=')  '
        ELSE
          WRITE(LU,*) 'TR NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(TR,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OK(IRANK))
!
      ENDIF
!
!     IF VALUE NOT FOUND IN THE LIQUID BOUNDARY FILE
!     OR IF THERE IS NO LIQUID BOUNDARY FILE
!     ATTEMPTS TO FIND IT IN THE STEERING FILE
!
      IF(.NOT.OK(IRANK).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
        IF(NTRACE.GE.IRANK) THEN
          TR = TRACER(IRANK)
          OK(IRANK)=.TRUE.
        ELSEIF(NTRACE.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,300) IRANK
300       FORMAT(1X,/,1X,'TR : VALEURS IMPOSEES DU TRACEUR'
     &             ,/,1X,'     EN NOMBRE INSUFFISANT'
     &             ,/,1X,'     DANS LE FICHIER DES PARAMETRES'
     &             ,/,1X,'     IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,301) IRANK
301       FORMAT(1X,/,1X,'TR : MORE PRESCRIBED TRACER VALUES'
     &             ,/,1X,'     ARE REQUIRED IN THE PARAMETER FILE'
     &             ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
      ENDIF
!
!     NOTHING FOUND: VALUES WILL BE TAKEN FROM BOUNDARY CONDITION FILE
!
      IF(.NOT.OK(IRANK)) THEN
        TR=0.D0
        IERR=1
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
