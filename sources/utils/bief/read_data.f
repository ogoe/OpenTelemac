!                    **********************
                     SUBROUTINE READ_DATA
!                    **********************
!
     &(FFORMAT,NFIC,VAR_VALUE,VAR_NAME,NPOIN,IERR,RECORD,
     & TIME)
!
!***********************************************************************
! HERMES   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR A RESULT ARRAY IN A SELAFIN FILE.
!
!history  J-M HERVOUET (LNH)
!+        08/08/98
!+        V5P2
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
!history  R. KOPMANN (EDF R&D, LNHE)
!+        16/04/2013
!+        V6P3
!+   Adding the format FFORMAT
!
!history  Y AUDOUIN
!+        21/05/2015
!+        V7P0
!+   Adapt code to work with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NFIC           |-->| NUMERO DU CANAL DU FICHIER
!| NPOIN          |-->| NUMBER OF POINT (SIZE OF VAR_VALUE)
!| FFORMAT        |-->| FORMAT OF THE FILE
!| VAR_VALUE      |<--| WHERE TO PUT THE RESULT
!| VAR_NAME       |-->| NAME OF VARIABLE (16 CHARACTERS)
!| IERR           |<--| 0 IF VARIABLE FOUND 1 OTHERWISE
!| RECORD         |-->| NUMBER OF THE REQUESTED RECORD
!| TIME           |<--| TIME OF RECORD (OPTIONAL)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)    :: NFIC
      INTEGER,           INTENT(IN)    :: NPOIN
      CHARACTER(LEN=8),  INTENT(IN)    :: FFORMAT
      DOUBLE PRECISION,  INTENT(INOUT) :: VAR_VALUE(NPOIN)
      CHARACTER(LEN=16), INTENT(IN)    :: VAR_NAME
      INTEGER,           INTENT(OUT)   :: IERR
      INTEGER,           INTENT(IN)    :: RECORD
      DOUBLE PRECISION,  INTENT(OUT), OPTIONAL :: TIME
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=16), ALLOCATABLE :: VAR_LIST(:)
      CHARACTER(LEN=16), ALLOCATABLE :: UNIT_LIST(:)
      INTEGER :: NVAR
      INTEGER :: I,NTIMESTEP
!
!-----------------------------------------------------------------------
!
      ! WE CHECK IF THE RECORD IS IN THE FILE BY GETTING THE NUMBER OF
      ! TIMESTEPS AND CHECKING THAT THE RECORD IS INDEED
      ! BETWEEN 0 AND NTIMESTEP - 1
      CALL GET_DATA_NTIMESTEP(FFORMAT,NFIC,NTIMESTEP,IERR)
      CALL CHECK_CALL(IERR, 'READ_DATA:GET_DATA_NTIMESTEP')
      IF((RECORD.LT.0).AND.(RECORD.GE.NTIMESTEP)) THEN
        IERR = HERMES_RECORD_UNKNOWN_ERR
        RETURN
      ENDIF
      ! CHECKING THAT THE VARIABLE IS INDEED IN THE FILE
      !  BY LOOPING ON THE LIST OF VARIABLE
      CALL GET_DATA_NVAR(FFORMAT,NFIC,NVAR,IERR)
      CALL CHECK_CALL(IERR, 'READ_DATA:GET_DATA_NVAR')
      ALLOCATE(VAR_LIST(NVAR),STAT=IERR)
      ALLOCATE(UNIT_LIST(NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR, 'VAR_LIST')
      CALL GET_DATA_VAR_LIST(FFORMAT,NFIC,NVAR,VAR_LIST,UNIT_LIST,IERR)
      CALL CHECK_CALL(IERR, 'READ_DATA:GET_DATA_VAR_LIST')
      I=1
      DO WHILE(I.LE.NVAR)
        IF (VAR_LIST(I).EQ.VAR_NAME) EXIT
        I = I +1
      ENDDO
      IF(I.EQ.NVAR+1) THEN
        IERR = HERMES_VAR_UNKNOWN_ERR
        RETURN
      ENDIF
      DEALLOCATE(VAR_LIST)
      DEALLOCATE(UNIT_LIST)

      IF(PRESENT(TIME)) THEN
        CALL GET_DATA_TIME(FFORMAT,NFIC,RECORD,TIME,IERR)
        IF(IERR.NE.0) THEN
          IF (LNG.EQ.1) WRITE(LU,*) 'ERREUR LORS DE LA LECTURE DE LA ',
     &                              'VALEUR DU PAS DE TEMPS POUR ',
     &                              'L ITERATION :', RECORD
          IF (LNG.EQ.2) WRITE(LU,*) 'ERROR WHILE READING TIME VALUE ',
     &                              'FOR RECORD:',RECORD
          CALL PLANTE(1)
        ENDIF
      ENDIF
      CALL GET_DATA_VALUE(FFORMAT,NFIC,RECORD,VAR_NAME,VAR_VALUE,
     &                    NPOIN,IERR)
      IF(IERR.NE.0) THEN
        IF (LNG.EQ.1) WRITE(LU,*) 'ERREUR LORS DE LA LECTURE DES ',
     &                            'RESULTATS POUR LA VARIABLE',
     &                             VAR_NAME,
     &                            'A L ITERATION :', RECORD
        IF (LNG.EQ.2) WRITE(LU,*) 'ERROR WHILE READING VALUE ',
     &                            'FOR FOR VARIABLE:',VAR_NAME,
     &                            'FOR RECORD:',RECORD
        CALL PLANTE(1)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
