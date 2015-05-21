!                    ***********************
                     SUBROUTINE READ_DATASET
!                    ***********************
!
     &(FFORMAT,FID,VARSOR,NPOIN,RECORD,AT,VAR_LIST,TROUVE,ALIRE,LISTIN,
     & LASTRECORD,MAXVAR)
!
!***********************************************************************
! BIEF   V7P1 
!***********************************************************************
!
!brief    Reads the results for a given time step and 
!+        a given list of variables
!
!history Y AUDOUIN (LNHE)
!+        19/05/2014
!+        V7P1
!+        First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| LOGICAL UNIT OF FILE
!| VARSOR         |<->| LIST OF ARRAY THAT WILL CONTAINS THE VARIABLE 
!|                |   | VALUE ON EACH POINT
!| NPOIN          |-->| NUMBER OF POINT IN THE GEOMETRY MESH
!| RECORD         |-->| TIME STEP OF THE DATASET
!| AT             |<->| TIME OF THE DATASET
!| VAR_LIST       |-->| NAMES AND UNITS OF VARIABLES.
!| TROUVE         |<--| GIVES (TROUVE(K)=1) IF VARIABLES PRESENT IN THE FILE
!|                |   | 0 OTHERWISE
!| ALIRE          |-->| VARIABLES TO BE READ (FOR OTHERS RECORD SKIPPED)
!|                |   | CLANDESTINE VARIABLES ARE SYSTEMATICALLY READ
!| LISTIN         |-->| IF YES, INFORMATIONS PRINTED ON LISTING
!| LASTRECORD     |<->| LASTRECORD = .TRUE. LAST RECORD WILL BE READ
!|                |   | LASTRECORD = .FALSE. : RECORD "RECORD" WILL BE READ
!| MAXVAR         |-->| DIMENSION OF ARRAY RELATED TO VARIABLES: ALIRE,..
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE BIEF_DEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: VARSOR
      INTEGER, INTENT(IN)             :: NPOIN
      INTEGER, INTENT(IN)             :: MAXVAR
      INTEGER, INTENT(IN)             :: FID
      INTEGER, INTENT(INOUT)          :: RECORD
      INTEGER, INTENT(INOUT)          :: TROUVE(MAXVAR)
      INTEGER, INTENT(IN)             :: ALIRE(MAXVAR)
      CHARACTER(LEN=8), INTENT(IN)    :: FFORMAT
      CHARACTER(LEN=32), INTENT(IN)   :: VAR_LIST(MAXVAR)
      DOUBLE PRECISION, INTENT(INOUT) :: AT
      LOGICAL, INTENT(IN)             :: LISTIN,LASTRECORD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IREC
      INTEGER :: L,IERR,IPLAN,I,IP1,IP2
      LOGICAL :: INTERPOLATE
      INTEGER :: NPOIN_PREV, NPLAN_PREV, NPLAN, NPOIN2
      DOUBLE PRECISION, ALLOCATABLE :: WD(:)
      DOUBLE PRECISION :: TETA,ARG
      CHARACTER(LEN=16), ALLOCATABLE :: VARNAME(:),VARUNIT(:)
      CHARACTER(LEN=80) :: TITLE
      INTEGER :: NVAR,IVAR
!
!-----------------------------------------------------------------------
!
!     We need the number of points to read the data
!
      CALL GET_MESH_NPOIN(FFORMAT,FID,POINT_BND_ELT_TYPE,
     &                    NPOIN_PREV,IERR)
      CALL CHECK_CALL(IERR,'READ_DATASET:GET_MESH_NPOIN')
      CALL GET_MESH_NPLAN(FFORMAT,FID,NPLAN_PREV,IERR)
      CALL CHECK_CALL(IERR,'READ_DATASET:GET_MESH_NPLAN')
!      
!     Printout
!
      CALL GET_MESH_TITLE(FFORMAT,FID,TITLE,IERR)
      CALL CHECK_CALL(IERR,'BIEF_VALIDA:GET_MESH_TITLE')
!
      CALL GET_DATA_NVAR(FFORMAT,FID,NVAR,IERR)
      CALL CHECK_CALL(IERR,'BIEF_VALIDA:GET_DATA_NVAR')
!
      ALLOCATE(VARNAME(NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VARNAME')
      ALLOCATE(VARUNIT(NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VARUNIT')
!
      CALL GET_DATA_VAR_LIST(FFORMAT,FID,NVAR,VARNAME,VARUNIT,IERR)
      CALL CHECK_CALL(IERR,'BIEF_VALIDA:GET_DATA_NVAR')
      
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,300) TITLE
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,301) TITLE
300   FORMAT(1X,//,1X,'TITRE DU CAS PRECEDENT: ',A72,/)
301   FORMAT(1X,//,1X,'TITLE OF PREVIOUS COMPUTATION: ',A72,/)
!
      DO IVAR=1,NVAR
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,11)
     &    VARNAME(IVAR),VARUNIT(IVAR)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,111)
     &    VARNAME(IVAR),VARUNIT(IVAR)
11      FORMAT(1X,'NOM: ' ,A16,'  UNITE: ',A16)
111     FORMAT(1X,'NAME: ',A16,'  UNIT: ' ,A16)
      ENDDO ! IVAR
      DEALLOCATE(VARNAME,VARUNIT)
!
!     INTERPOLATES ?
!
      INTERPOLATE=.FALSE.
      IF(NPOIN_PREV.NE.NPOIN) THEN
        INTERPOLATE=.TRUE.
        NPOIN2=NPOIN_PREV/NPLAN_PREV
        NPLAN=NPOIN/NPOIN2
        ALLOCATE(WD(NPOIN_PREV),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'READ_DATASET:WD')
      ENDIF
!
!     GET THE NUMBER OF TIME STEPS
!
      IF(LASTRECORD) THEN
        CALL GET_DATA_NTIMESTEP(FFORMAT,FID,IREC,IERR)
        CALL CHECK_CALL(IERR,'READ_DATASET:GET_DATA_NTIMESTEP')
!       Records go from 0 to ntimestep - 1
        IREC = IREC - 1
        RECORD = IREC 
      ELSE
        IREC = RECORD 
      ENDIF
!
!     GET THE TIME ASSOCIATED WITH THE RECORD
!
      CALL GET_DATA_TIME(FFORMAT,FID,IREC,AT,IERR)
!
!     Check if the record is in the file
!
      IF(IERR.EQ.HERMES_RECORD_UNKNOWN_ERR) THEN
        IF(LNG.EQ.1) WRITE(LU,75) IREC
        IF(LNG.EQ.2) WRITE(LU,76) IREC
        CALL PLANTE(1)
      ELSE
        CALL CHECK_CALL(IERR,'READ_DATASET:GET_DATA_TIME')
      ENDIF
!
      DO L=1,MIN(MAXVAR,VARSOR%N)
!
        IF((ALIRE(L).EQ.1).AND.(ASSOCIATED(VARSOR%ADR(L)%P))
     &      .AND.(VAR_LIST(L)(1:1).NE.' ')) THEN
!         Interpolate the results if necessary
          IF(INTERPOLATE) THEN
            CALL GET_DATA_VALUE(FFORMAT,FID,IREC,VAR_LIST(L)(1:16),
     &                          WD,NPOIN_PREV,IERR)
!           COPIES BOTTOM AND FREE SURFACE
            CALL OV('X=Y     ',VARSOR%ADR(L)%P%R,WD,WD,0.D0,NPOIN2)
            CALL OV('X=Y     ',VARSOR%ADR(L)%P%R(NPOIN-NPOIN2+1:NPOIN),
     &                         WD(NPOIN_PREV-NPOIN2+1:NPOIN_PREV),
     &                         WD,0.D0,NPOIN2)
!           INTERPOLATES OTHER PLANES
            IF(NPLAN.GT.2) THEN
              DO IPLAN=2,NPLAN-1
                ARG=(NPLAN_PREV-1)*FLOAT(IPLAN-1)/FLOAT(NPLAN-1)
                TETA=ARG-INT(ARG)
!               IP1 : LOWER PLANE NUMBER - 1
                IP1=INT(ARG)
!               IP2 : UPPER PLANE NUMBER - 1
                IP2=IP1+1
                DO I=1,NPOIN2
                  VARSOR%ADR(L)%P%R(I+NPOIN2*(IPLAN-1))=
     &            TETA *WD(I+NPOIN2*IP2)+(1.D0-TETA)*WD(I+NPOIN2*IP1)
                ENDDO
              ENDDO
            ENDIF
          ELSE
             
            CALL GET_DATA_VALUE(FFORMAT,FID,IREC,VAR_LIST(L)(1:16),
     &                          VARSOR%ADR(L)%P%R,NPOIN_PREV,IERR)
           
          ENDIF
          ! If the variable is not in the file
          IF(IERR.EQ.HERMES_VAR_UNKNOWN_ERR) THEN
            TROUVE(L) = 0
          ELSE
            CALL CHECK_CALL(IERR,'READ_DATASET:GET_DATA_VALUE')
            TROUVE(L) = 1
          ENDIF
           
        ELSE
          ! if the record is not in the file
          TROUVE(L) = 0
        ENDIF
!
      ENDDO ! L
      IF(INTERPOLATE) DEALLOCATE(WD)
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,130) IREC+1
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,131) IREC+1
130   FORMAT(/,1X,'READ_DATASET : LECTURE A L''ENREGISTREMENT ',1I5)
131   FORMAT(/,1X,'READ_DATASET : READ OF RECORD ',1I5)
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,140) AT
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,141) AT
140   FORMAT(//,1X,'TEMPS DE L''ENREGISTREMENT : ',G16.7,' S')
141   FORMAT(//,1X,'TIME OF RECORD: ',G16.7,' S')
!
!-----------------------------------------------------------------------
!
75      FORMAT(/,1X,'LE PAS DE TEMP : ',I16,/,1X,
     &              'N EST PAS DANS LE FICHIER')
76      FORMAT(/,1X,'TIME STEP : ',I16,/,1X,'IS NOT IN THE FILE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
