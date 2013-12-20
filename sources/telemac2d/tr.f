!                    ****************************
                     DOUBLE PRECISION FUNCTION TR
!                    ****************************
!
     &( I , ITRAC , N , IERR )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   08/11/2011
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
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size FCT and OK due to modification of TRACER
!+    numbering TRACER is now identified by 2 values (Ifront, Itracer)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| BOUNDARY RANK
!| IERR           |<--| IF 0, OK, IF 1: PROBLEM
!| ITRAC          |-->| TRACER RANK
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER IN THE ORIGINAL MESH
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
      CHARACTER*9 FCT
      INTEGER J,K,IRANK
      LOGICAL, SAVE :: DEJA=.FALSE.
      LOGICAL, DIMENSION(MAXFRO,MAXTRA), SAVE :: OK
!
!     A PRIORI ASSUMES THAT TR WILL BE FOUND
!
      IERR=0
!
!     FIRST CALL, OK INITIALISED TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO K=1, MAXTRA
           DO J=1,MAXFRO
              OK(J,K)=.TRUE.
           ENDDO
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!     RANK OF VALUE IN ARRAY TRACER OR IN LIQUID BOUNDARY FILE
!
      IF(OK(I,ITRAC).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE TR(I,ITRAC)
        FCT='TR(      '
        IRANK=4
        IF(I.LT.10) THEN
          WRITE(FCT(IRANK:IRANK),FMT='(I1)') I
          IRANK=IRANK+1
          FCT(IRANK:IRANK)=','
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(IRANK:IRANK+1),FMT='(I2)') I
          IRANK=IRANK+2
          FCT(IRANK:IRANK)=','
        ELSE
          WRITE(LU,*) 'TR NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        IRANK=IRANK+1
        IF(ITRAC.LT.10) THEN
          WRITE(FCT(IRANK:IRANK),FMT='(I1)') ITRAC
          IRANK=IRANK+1
          FCT(IRANK:IRANK)=')'
        ELSEIF(ITRAC.LT.100) THEN
          WRITE(FCT(IRANK:IRANK+1),FMT='(I2)') ITRAC
          IRANK=IRANK+2
          FCT(IRANK:IRANK)=')'
        ELSE ! Probably unused because ntrac<maxtrac
          WRITE(LU,*) 'TRSCE NOT PROGRAMMED FOR MORE THAN 99 TRACERS'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(TR,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OK(I,ITRAC))
!
      ENDIF
!
!     IF VALUE NOT FOUND IN THE LIQUID BOUNDARY FILE
!     OR IF THERE IS NO LIQUID BOUNDARY FILE
!     ATTEMPTS TO FIND IT IN THE STEERING FILE
!
      IF(.NOT.OK(I,ITRAC).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
        IRANK=ITRAC+(I-1)*NTRAC
        IF(NTRACE.GE.IRANK) THEN
          TR = TRACER(IRANK)
          OK(I,ITRAC)=.TRUE.
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
      IF(.NOT.OK(I,ITRAC)) THEN
        TR=0.D0
        IERR=1
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
