!                    ************************
                     SUBROUTINE FIND_VARIABLE
!                    ************************
!
     &(FFORMAT,FID,TIME,VAR_NAME,RES,N,EPS,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Returns The value for each point of a given variable
!+        for a given time interpolate from two time step if the time 
!+        does not exist in the file
!
!history  Y AUDOUIN (LNHE)
!+        01/07/2015
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FILE_ID        |-->| FILE DESCRIPTOR
!| TIME           |-->| TIME TO INTERPOLATE FROM THE FILE
!| VAR_NAME       |-->| VARIABLE FOR WHICH WE NEED THE VALUE
!| RES            |<->| VALUE FOR EACH POINT AT TIME STEP RECORD
!|                |   | FOR THE VARIABLE VAR_NAME
!| N              |-->| SIZE OF RES_VALUE
!| EPS            |-->| EPSILON TO DETERMINE IF TWO REAL ARE EQUAL
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8),  INTENT(IN)    :: FFORMAT
      INTEGER,           INTENT(IN)    :: FID
      DOUBLE PRECISION,  INTENT(IN)    :: TIME
      CHARACTER(LEN=16), INTENT(IN)    :: VAR_NAME
      INTEGER,           INTENT(IN)    :: N
      DOUBLE PRECISION,  INTENT(INOUT) :: RES(N)
      DOUBLE PRECISION,  INTENT(IN)    :: EPS
      INTEGER,           INTENT(OUT)   :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER RECORD1,RECORD2
      DOUBLE PRECISION :: TIME1,TIME2,COEF
      DOUBLE PRECISION :: RES1(N),RES2(N)
      INTEGER NTIMESTEP,I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! GET THE NUMBER OF TIME STEPS
      CALL GET_DATA_NTIMESTEP(FFORMAT,FID,NTIMESTEP,IERR)
      CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
      ! IDENTIFY THE TWO RECORDS INBETWEEN OUR TIME
      RECORD1 = 0
      RECORD2 = NTIMESTEP-1
      CALL GET_DATA_TIME(FFORMAT,FID,RECORD1,TIME1,IERR)
      CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
      CALL GET_DATA_TIME(FFORMAT,FID,RECORD2,TIME2,IERR)
      CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC2')
      ! Quick check to see if the tim is indeed within the first 
      ! and last time step of the file
      IF ((TIME1.GT.TIME.AND.ABS(TIME1-TIME).GT.EPS)
     &    .OR.(TIME2.LT.TIME.AND.ABS(TIME2-TIME).GT.EPS)) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'LE TEMPS : ',TIME,'N EST PAS INCLUS ',
     &                           'DANS LE FICHIER I.E.[',
     &                           TIME1,'-',TIME2,']'
        IF(LNG.EQ.2) WRITE(LU,*)'TIME: ',TIME,'IS NOT WITHIN THE RANGE',
     &                          ' OF THE FILE I.E.[',TIME1,'-',TIME2,']'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO WHILE ((TIME1.LT.TIME).AND.(ABS(TIME1-TIME).GT.EPS) )
        RECORD1 = RECORD1 + 1
        CALL GET_DATA_TIME(FFORMAT,FID,RECORD1,TIME1,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
      ENDDO 
      ! If the time is on the file return the value for that record
      ! otherwise we interpolate between the two time step
      IF (ABS(TIME1-TIME).LE.EPS) THEN
        CALL GET_DATA_VALUE(FFORMAT,FID,RECORD1,VAR_NAME,RES,N,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_VALUE:REC1')
      ELSE
        ! Get the time and values for the record before and after time
        RECORD2 = RECORD1
        RECORD1 = RECORD1 -1
        CALL GET_DATA_TIME(FFORMAT,FID,RECORD1,TIME1,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC1')
        CALL GET_DATA_TIME(FFORMAT,FID,RECORD2,TIME2,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_TIME:REC2')
!       Get the value for each time step        
        CALL GET_DATA_VALUE(FFORMAT,FID,RECORD1,VAR_NAME,RES1,N,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_VALUE:REC1')
        CALL GET_DATA_VALUE(FFORMAT,FID,RECORD2,VAR_NAME,RES2,N,IERR)
        CALL CHECK_CALL(IERR,'FIND_VARIABLE:GET_DATA_VALUE:REC1')
!       Interpolates in time
!      
        COEF=(TIME-TIME1)/(TIME2-TIME1)
!      
        DO I=1,N
          RES(I)=(RES2(I)-RES1(I))*COEF+RES1(I)
        ENDDO
      ENDIF


!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
