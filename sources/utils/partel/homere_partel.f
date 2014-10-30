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
!+    V STOBIAC
!+    READING OF THE MESH FORMAT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARTEL
      IMPLICIT NONE
!
      INTEGER :: LNG,LU,LI
      COMMON /INFO/ LNG,LU,LI
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      LOGICAL :: FOUND
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
      INTEGER :: I, J, K, L , M, N, ERR, ISO, IDUM
      INTEGER :: ISEG, II, ILOOP
      INTEGER :: I_LEN, I_S, I_SP, I_LENCLI, I_LENINP
      INTEGER :: JJ, IPAR, ICAS, IOS
!
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
!
      CHARACTER(LEN=43) :: LINE, LINE_ADJ
      CHARACTER(LEN=38) :: KEY_FORMAT
      LOGICAL :: FORMAT_MED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      LNG=2 ! JE NE PARLE FRANCAIS, JE SUIS BARBARIEN
      LU=6  ! FORTRAN STANDARD OUPUT CHANNEL
      LI=5  ! FORTRAN STANDARD INPUT CHANNEL
!
!     FILE DESCRIPTOR
      IPAR = 72
      ICAS = 73
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
      IF( FOUND ) OPEN(UNIT=IPAR,FILE='PARTEL.PAR')

      IF (FOUND) REWIND(IPAR)
      DO 
        IF( FOUND ) THEN                  
           WRITE(LU, FMT='(/,'' SELAFIN INPUT NAME <INPUT_NAME>: '')')
           READ(IPAR,*) NAMEINP             
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
           READ(IPAR,*) NAMECLI                 
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
           READ(IPAR,*) NPARTS                 
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
           READ(IPAR,*) PMETHOD                
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
           READ(IPAR,*) I
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
            READ(IPAR,'(A)') NAMESEC
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
           READ(IPAR,*) I                       
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
            READ(IPAR,'(A)') NAMEZFI
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
      IF( FOUND ) CLOSE(IPAR)
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
!
!     READ THE FORMAT OF THE MESH (ONLY FOR ESTEL FOR NOW)
      FORMAT_MED = .FALSE.
      INQUIRE(FILE='ES3CAS',EXIST=FOUND)
!
      IF (FOUND) THEN
!
!       OPEN THE FILE CASE FILE FOR ESTEL
        OPEN(UNIT=ICAS,FILE='ES3CAS')
        REWIND(ICAS)
!
!       DEFINITION OF THE KEY WORD AND ITS SIZE
        KEY_FORMAT = 'FILE FORMAT FOR THE 3D INPUT MESH FILE'
        I_LEN = LEN(KEY_FORMAT)
!
!       LOOP ON THE LINE OF THE INPUT FILE
        IOS = 0
        DO WHILE (IOS .EQ. 0)
          READ(UNIT=ICAS,FMT='(A43)',IOSTAT=IOS) LINE
!
!         SECURITY TO ERASE SPACE AT THE BEGINNING OF THE LINE
          LINE_ADJ=ADJUSTL(LINE)
!
!         LOOP TO FIND THE NUMBER OF THE FORMAT
          IF (LINE_ADJ(1:I_LEN) .EQ. KEY_FORMAT(1:I_LEN)) THEN
            DO I=I_LEN+1,43
              IF (LINE_ADJ(I:I).EQ.'4') FORMAT_MED = .TRUE.
            ENDDO
            EXIT
          ENDIF
        ENDDO
!
        CLOSE (ICAS)
      ENDIF
!
      IF(NAMEINP(1:3) .EQ. 'ES3') THEN
        CALL PARES3D(NAMEINP, NAMECLI, NPARTS, PMETHOD, FORMAT_MED)
      ELSE
        CALL PARTEL(NAMEINP, NAMECLI, NPARTS, PMETHOD, 
     &              WITH_SECTIONS, NAMESEC, 
     &              WITH_ZONES, NAMEZFI)
      ENDIF
      END PROGRAM
