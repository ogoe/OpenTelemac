!                    **************
                     PROGRAM HOMERE_PARTEL
!                    **************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    PREPROCESSING STEP BEFORE A PARALLEL COMPUTATION
!
!history   Y. AUDOUIN (EDF)
!+
!+         FIRST  VERSION 
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARTEL
      IMPLICIT NONE
!
      INTEGER LNG,LU,LI
      COMMON /INFO/ LNG,LU,LI
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      LOGICAL FOUND
      CHARACTER(LEN=MAXLENHARD) :: NAMEINP
      CHARACTER(LEN=MAXLENHARD) :: NAMECLI
      INTEGER :: NPARTS
      INTEGER :: PMETHOD
      LOGICAL :: WITH_SECTIONS, DONE_SECTIONS
      CHARACTER(LEN=MAXLENHARD) :: NAMESEC
      LOGICAL :: WITH_ZONES, DONE_ZONES
      CHARACTER(LEN=MAXLENHARD) :: NAMEZFI
!
      LOGICAL :: IS
      INTEGER I, J, K, L , M, N, ERR, ISO, IDUM
      INTEGER ISEG, II, ILOOP
      INTEGER I_LEN, I_S, I_SP, I_LENCLI, I_LENINP
      INTEGER JJ
!
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      LNG=2 ! JE NE PARLE FRANCAIS, JE SUIS BARBARIEN
      LU=6  ! FORTRAN STANDARD OUPUT CHANNEL
      LI=5  ! FORTRAN STANDARD INPUT CHANNEL
!
!     ALLOCATE(NULLTABLE(0))       
      WITH_SECTIONS = .FALSE.
      DONE_SECTIONS = .FALSE.
      WITH_ZONES = .FALSE.
      DONE_ZONES = .FALSE.
!
!----------------------------------------------------------------------
! INTRODUCE YOURSELF
!
      WRITE(LU,*) ' '
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) '  PARTEL: TELEMAC SELAFIN METISOLOGIC PARTITIONER'
      WRITE(LU,*) '                                                   '
      WRITE(LU,*) '  REBEKKA KOPMANN & JACEK A. JANKOWSKI (BAW)'
      WRITE(LU,*) '                 JEAN-MICHEL HERVOUET (LNHE)'
      WRITE(LU,*) '                 CHRISTOPHE DENIS     (SINETICS) '
      WRITE(LU,*) '                 YOANN AUDOUIN        (LNHE) '
      WRITE(LU,*) '  PARTEL (C) COPYRIGHT 2000-2002 '
      WRITE(LU,*) '  BUNDESANSTALT FUER WASSERBAU, KARLSRUHE'
      WRITE(LU,*) ' '
      WRITE(LU,*) '  METIS 5.0.2 (C) COPYRIGHT 2012 '
      WRITE(LU,*) '  REGENTS OF THE UNIVERSITY OF MINNESOTA '
      WRITE(LU,*) ' '
      WRITE(LU,*) '  BIEF 6.2 (C) COPYRIGHT 2012 EDF'
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) ' '
      WRITE(LU,*) ' '
      WRITE(LU,*) '  MAXIMUM NUMBER OF PARTITIONS: ',MAXNPROC
      WRITE(LU,*) ' '
      WRITE(LU,*) '+--------------------------------------------------+'
      WRITE(LU,*) ' '
!
!----------------------------------------------------------------------
! NAMES OF THE INPUT FILES:
!
      INQUIRE(FILE='PARTEL.PAR',EXIST=FOUND)
      IF( FOUND ) OPEN(UNIT=72,FILE='PARTEL.PAR')

      IF (FOUND) REWIND(72)
      DO 
        IF( FOUND ) THEN                  
           WRITE(LU, FMT='(/,'' SELAFIN INPUT NAME <INPUT_NAME>: '')')
           READ(72,*) NAMEINP             
        ELSE                              
           WRITE(LU, ADVANCE='NO', FMT=
     &         '(/,'' SELAFIN INPUT NAME <INPUT_NAME>: '')')
           READ(LI,'(A)') NAMEINP
        ENDIF                            
        IF (NAMEINP.EQ.' ') THEN
          WRITE (LU,'('' NO FILENAME'')') 
        ELSE
! CONTINUE WITH TELEMAC CODES
          WRITE(LU,*) 'INPUT: ',NAMEINP
          EXIT 
        END IF  
      END DO

      INQUIRE (FILE=NAMEINP,EXIST=IS)
      IF (.NOT.IS) THEN 
        WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMEINP
        CALL PLANTE(1)
        STOP
      END IF  
!
      DO
        IF( FOUND ) THEN                      
           WRITE(LU, FMT='(/,'' BOUNDARY CONDITIONS FILE NAME : '')')
           READ(72,*) NAMECLI                 
        ELSE                                  
           WRITE(LU, ADVANCE='NO', FMT=
     &           '(/,'' BOUNDARY CONDITIONS FILE NAME : '')')
           READ(LI,'(A)') NAMECLI
        ENDIF                                
        IF (NAMECLI.EQ.' ') THEN
          WRITE (LU,'('' NO FILENAME'')') 
        ELSE
          WRITE(LU,*) 'INPUT: ',NAMECLI
          EXIT
        END IF
      END DO
!  
      INQUIRE (FILE=NAMECLI,EXIST=IS)
      IF (.NOT.IS) THEN 
        WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMECLI
        CALL PLANTE(1)
        STOP
      END IF  
!
      DO 
        IF( FOUND ) THEN                     
           WRITE(LU,FMT=
     &    '(/,'' NUMBER OF PARTITIONS <NPARTS> [2 -'',I6,'']: '')') 
     &        MAXNPROC
           READ(72,*) NPARTS                 
        ELSE                                 
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' NUMBER OF PARTITIONS <NPARTS> [2 -'',I6,'']: '')') 
     &        MAXNPROC
           READ(LI,*) NPARTS
        ENDIF                               
        IF ( (NPARTS > MAXNPROC) .OR. (NPARTS < 2) ) THEN
          WRITE(LU,
     &    '('' NUMBER OF PARTITIONS MUST BE IN [2 -'',I6,'']'')') 
     &      MAXNPROC
        ELSE
          WRITE(LU,'('' INPUT: '',I4)') NPARTS
          EXIT
        END IF 
      END DO
!
      WRITE(LU,FMT='(/,'' PARTITIONING OPTIONS: '')')

      DO 
        IF( FOUND ) THEN                     
           WRITE(LU, FMT=
     &    '(/,'' PARTITIONING METHOD <PMETHOD> 
     &    [1 (METIS) OR 2 (SCOTCH)]: '')') 
           READ(72,*) PMETHOD                
        ELSE                                 
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' PARTITIONING METHOD <PMETHOD> 
     &    [1 (METIS) OR 2 (SCOTCH)]: '')') 
           READ(LI,*) PMETHOD
        ENDIF                                 
        IF ( (PMETHOD > 2) .OR. (PMETHOD < 1) ) THEN
          WRITE(LU,
     &    '('' PARTITIONING METHOD MUST BE 1 OR 2'')') 
        ELSE
          WRITE(LU,'('' INPUT: '',I3)') PMETHOD
          EXIT
        END IF 
      END DO
!
! #### THE SECTIONS FILE NAME 
!
      DO
        IF( FOUND ) THEN                      
           WRITE(LU, FMT=
     &    '(/,'' WITH SECTIONS? [1:YES 0:NO]: '')') 
           READ(72,*) I
        ELSE                                  
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' WITH SECTIONS? [1:YES 0:NO]: '')') 
           READ(LI,*) I
        ENDIF                                
        IF ( I<0 .OR. I>1 ) THEN
          WRITE(LU,'('' PLEASE ANSWER 1:YES OR 0:NO '')') 
        ELSE
          WRITE(LU,'('' INPUT: '',I4)') I
          EXIT
        END IF 
      END DO
      IF (I==1) WITH_SECTIONS=.TRUE.
!
      IF (WITH_SECTIONS) THEN 
        DO
          WRITE(LU, ADVANCE='NO', FMT=
     &      '(/,'' CONTROL SECTIONS FILE NAME (OR RETURN) : '')')
          IF(FOUND) THEN
            READ(72,'(A)') NAMESEC
          ELSE
            READ(LI,'(A)') NAMESEC
          ENDIF
          IF (NAMESEC.EQ.' ') THEN
            WRITE (LU,'('' NO FILENAME '')') 
          ELSE
            WRITE(LU,*) 'INPUT: ',NAMESEC
            EXIT
          ENDIF
        ENDDO
!  
        INQUIRE (FILE=NAMESEC,EXIST=IS)
        IF (.NOT.IS) THEN
          WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMESEC
          CALL PLANTE(1)
          STOP
        ENDIF  
      ENDIF
      ! Not recalculating section if already done in previous run of partel
      INQUIRE(FILE=TRIM(NAMESEC)//EXTENS(NPARTS,0),EXIST=DONE_SECTIONS)
      WITH_SECTIONS = WITH_SECTIONS .AND. (.NOT. DONE_SECTIONS)
!
! #### THE ZONES FILE NAME 
!
      DO
        IF( FOUND ) THEN                      
           WRITE(LU, FMT=
     &    '(/,'' WITH ZONES? [1:YES 0:NO]: '')') 
           READ(72,*) I                       
        ELSE                                  
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' WITH ZONES? [1:YES 0:NO]: '')') 
           READ(LI,*) I
        ENDIF                                
        IF ( I<0 .OR. I>1 ) THEN
          WRITE(LU,'('' PLEASE ANSWER 1:YES OR 0:NO '')') 
        ELSE
          WRITE(LU,'('' INPUT: '',I4)') I
          EXIT
        END IF 
      END DO
      IF (I==1) WITH_ZONES=.TRUE.
!
      IF (WITH_ZONES) THEN 
        DO
          WRITE(LU, ADVANCE='NO', FMT=
     &      '(/,'' FRICTION ZONES FILE NAME (OR RETURN) : '')')
          IF(FOUND) THEN
            READ(72,'(A)') NAMEZFI
          ELSE
            READ(LI,'(A)') NAMEZFI
          ENDIF
          IF (NAMEZFI.EQ.' ') THEN
            WRITE (LU,'('' NO FILENAME '')') 
          ELSE
            WRITE(LU,*) 'INPUT: ',NAMEZFI
            EXIT
          ENDIF
        ENDDO
!  
        INQUIRE (FILE=NAMEZFI,EXIST=IS)
        IF (.NOT.IS) THEN
          WRITE (LU,'('' FILE DOES NOT EXIST: '',A30)') NAMEZFI
          CALL PLANTE(1)
          STOP
        ENDIF  
      ENDIF
      ! Not recalculating zone if already done in previous run of partel
      INQUIRE(FILE=TRIM(NAMEZFI)//EXTENS(NPARTS-1,0),EXIST=DONE_ZONES)
      WITH_ZONES = WITH_ZONES .AND. (.NOT. DONE_ZONES)
!
      IF( FOUND ) CLOSE(72)
!
!
! FIND THE INPUT FILE CORE NAME LENGTH
!
      I_S  = LEN(NAMEINP)
      I_SP = I_S + 1
      DO I=1,I_S
        IF (NAMEINP(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN = I_SP - I
      I_LENINP = I_LEN
!
      IF (I_LENINP > MAXLENSOFT) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE INPUT FILE:'
        WRITE(LU,*) NAMEINP
        WRITE(LU,*) 'IS LONGER THAN ',MAXLENSOFT,' CHARACTERS' 
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE(1)
        STOP
      ENDIF
!
! CORE NAME LENGTH
!
      I_S  = LEN(NAMECLI)
      I_SP = I_S + 1
      DO I=1,I_S
        IF (NAMECLI(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN = I_SP - I
      I_LENCLI = I_LEN
!
      IF (I_LENCLI > MAXLENSOFT) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ATTENTION:'
        WRITE(LU,*) 'THE NAME OF THE BOUNDARY CONDITIONS FILE:'
        WRITE(LU,*) NAMECLI
        WRITE(LU,*) 'IS LONGER THAN ',MAXLENSOFT,' CHARACTERS' 
        WRITE(LU,*) 'WHICH IS THE LONGEST APPLICABLE NAME FOR TELEMAC '
        WRITE(LU,*) 'INPUT AND OUTPUT FILES. STOPPED. '
        CALL PLANTE(1)
        STOP
      ENDIF
      
      IF(NAMEINP(1:3) .EQ. 'ES3') THEN
        CALL PARES3D(NAMEINP, NAMECLI, NPARTS, PMETHOD)
      ELSE
        CALL PARTEL(NAMEINP, NAMECLI, NPARTS, PMETHOD, 
     &              WITH_SECTIONS, NAMESEC, 
     &              WITH_ZONES, NAMEZFI)
      ENDIF
      END PROGRAM
