!                    **************
                     PROGRAM PARTEL
!                    **************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    PREPROCESSING STEP BEFORE A PARALLEL COMPUTATION
!
!history   R. KOPMANN (BAW)
!+
!+
!+         FIRST  VERSION JANUARY-MARCH 2000
!
!history   JAJ
!+      12/12/2000
!+      SECOND VERSION PINXIT
!+     PARTITIONING OF GEOMETRY AND 2D RESULT FILES POSSIBLE

!history   JAJ
!+      22/02/2002
!+      THIRD VERSION
!+     ERRORS IN BC VALUES IN DECOMPOSED BC FILES REMOVED
!+     ERRONEOUS TREATMENT OF ISLANDS DEBUGGED
!
!history   J-M HERVOUET ; JAJ
!+      17/04/2002
!+     FOURTH VERSION
!+     PARTITIONING FOR 3D RESULT FILES DONE BY JMH
!+     INCLUDING BOTH PARTITIONING METHODS AND BEAUTIFYING BY JAJ
!
!history  J-M HERVOUET
!+     21/01/2003
!+     FIFTH VERSION
!+     CORRECTED A WRONG DIMENSION OF THE ARRAY CUT, AN ERROR
!+     OCCURING BY A LARGER NUMBER OF PROCESSORS
!
!
!history  J-M HERVOUET
!+       12/03/2003
!+      SEVENTH VERSION
!+      ALGORITHM CHANGED : A SEGMENT IS IN A SUBDOMAIN IF IT BELONGS
!+      TO AN ELEMENT IN THE SUBDOMAIN NOT IF THE 2 POINTS OF THE
!+      SEGMENT BELONG TO THE SUBDOMAIN.
!+       SPECIFIC ELEBD INCLUDED, ALL REFERENCE TO MPI OR BIEF REMOVED
!
!history  J-M HERVOUET
!+        01/09/2003
!+      EIGHTH VERSION
!+      UBOR AND VBOR INVERTED LINE 613 WHEN READING THE CLI FILE.
!+    OTHER MODIFICATIONS PERFORMED
!
!history  C. DENIS J-M HERVOUET (SINETICS & LNHE)
!+        22/06/2012
!+        V6P2
!+      Double precision SERAFIN now possible.
!
!history  Y. AUDOUIN (STFC & LNHE)
!+        25/06/2012
!+        V6P2
!+      Interface for latest release of METIS (>= Version 5)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : NBMAXNSHARE, NCSIZE, NPTIR, CHAIN_TYPE
      IMPLICIT NONE
!
!     MAXIMUM NUMBER OF HALO, IN THE PARALLEL VERSION THE NUMBER OF HALO WILL BE DIRECTLY COMPUTED
      INTEGER, PARAMETER :: NBMAXHALO=100000
!
      INTEGER, PARAMETER :: MAXNPROC = 100000 ! MAX PARTITION NUMBER [00000..99999]
      INTEGER, PARAMETER :: MAXLENSOFT = 144 ! SOFT MAX FILE NAME LENGTH
      INTEGER, PARAMETER :: MAXLENHARD = 250 ! HARD MAX FILE NAME LENGTH
      INTEGER, PARAMETER :: MAXADDCH = 10 ! MAX ADDED SUFFIX LENGTH
      INTEGER, PARAMETER :: MAXVAR = 100  ! MAX NUMBER OF VARIABLES
!
      INTEGER PMETHOD
      INTEGER NVAR, NPLAN, NPTFR, NPTFRMAX
      INTEGER NELEM, NPOIN, NDP, NELEM2, NPOIN2, NDUM
      INTEGER IB(10)
!
      INTEGER, ALLOCATABLE :: IKLES(:), IKLES_P(:)
      INTEGER, ALLOCATABLE :: IKLES3D(:),IKLES3D_P(:,:,:)
      INTEGER, ALLOCATABLE :: IRAND(:)
      INTEGER, ALLOCATABLE :: LIHBOR(:), LIUBOR(:), LIVBOR(:)
      INTEGER, ALLOCATABLE :: LITBOR(:)
      INTEGER, ALLOCATABLE :: NPOIN_P(:), NELEM_P(:), NPTFR_P(:)
      INTEGER, ALLOCATABLE :: NBOR(:), NPTIR_P(:)
      INTEGER, ALLOCATABLE :: NUMLIQ(:)
      INTEGER, ALLOCATABLE :: KNOLG(:,:), KNOGL(:,:),CHECK(:)
      INTEGER, ALLOCATABLE :: ELELG(:,:)
      INTEGER, ALLOCATABLE :: CUT(:), CUT_P(:,:), SORT(:)
      INTEGER, ALLOCATABLE :: PART_P(:,:)
!
!     FOR DOUBLE PRECISION SERAFIN FORMAT
!
      DOUBLE PRECISION, ALLOCATABLE :: F_D(:,:)
!
      REAL, ALLOCATABLE    :: F(:,:)
      REAL, ALLOCATABLE    :: HBOR(:) 
      REAL, ALLOCATABLE    :: UBOR(:), VBOR(:), AUBOR(:)
      REAL, ALLOCATABLE    :: TBOR(:), ATBOR(:), BTBOR(:)
!
      REAL TIMES, TIMED
      DOUBLE PRECISION TIMES_D, TIMED_D
!
      INTEGER :: NINP=10, NCLI=11, NSCT=13
      INTEGER :: NOUT=17, NCLM=18
      INTEGER TIME(3),DATE(3),TIME_TMP(3), DATE_TMP(3)
!
      CHARACTER(LEN=80)  :: TITLE
      CHARACTER(LEN=32)  :: VARI, VARIABLE(MAXVAR)
      CHARACTER(LEN=MAXLENHARD)  :: NAMEINP, NAMECLI, NAMEOUT, NAMECLM
      CHARACTER(LEN=12)  :: FMT4
!
      INTEGER MAX_NELEM_P
      INTEGER  MAX_NPOIN_P,MAX_N_NEIGH
      INTEGER I, J, K, L , M, N, ERR, ISO, IDUM
      INTEGER ISEG, II, ILOOP
      INTEGER I_LEN, I_S, I_SP, I_LENCLI, I_LENINP
      INTEGER JJ
!
      REAL XSEG, YSEG
      LOGICAL IS, TIMECOUNT
!
! METISOLOGY
!
      INTEGER, ALLOCATABLE :: EPART(:), NPART(:)
      INTEGER NPARTS
!
! FOR CALLING FRONT2
!
      INTEGER, PARAMETER :: MAXFRO = 3000
      INTEGER, ALLOCATABLE :: NACHB(:,:)
      INTEGER, ALLOCATABLE :: DEJAVU(:), KP1BOR(:,:)
      INTEGER :: NFRLIQ,NFRSOL
      INTEGER,ALLOCATABLE :: DEBLIQ(:),FINLIQ(:)
      INTEGER,ALLOCATABLE :: DEBSOL(:),FINSOL(:)
      DOUBLE PRECISION, ALLOCATABLE :: X_D(:), Y_D(:)
!
! FOR CALLING BIEF MESH SUBROUTINES (TO BE OPTIMISED SOON):
!
      INTEGER, ALLOCATABLE :: IFABOR(:,:), IFANUM(:,:), NELBOR(:)
      INTEGER, ALLOCATABLE :: NULONE(:,:)
      INTEGER, ALLOCATABLE :: IKLE(:,:), IKLBOR(:,:), ISEGF(:)
      INTEGER, ALLOCATABLE :: IT1(:), IT2(:), IT3(:)
!
      INTEGER NPOIN_TOT
      REAL TMP
      DOUBLE PRECISION TMP_D
!
      INTEGER LNG,LU,LI
      COMMON /INFO/ LNG,LU
!
! TIME MEASURING 
!
      INTEGER  TDEB, TFIN, TDEBP, TFINP, TEMPS, PARSEC
!
!----------------------------------------------------------------------
!
!JAJ NEW FOR PARALLEL CHARACTERISTICS ////
! HALO ELEMENTS: THESE ADJACENT TO THE INTERFACE EDGES HAVING 
! NEIGHBOURS BEHIND A BOUNDARY 
!
      ! THE ELEMENTAL GLOBAL->LOCAL NUMBERING TRANSLATION TABLE 
      ! THIS IS ELEGL SAVED FROM ALL PARTITIONS FOR FURTHER USE
      INTEGER, ALLOCATABLE :: GELEGL(:,:)
!
      ! THE HALO ELEMENTS NEIGHBOURHOOD DESCRIPTION FOR A HALO CELL 
      INTEGER, ALLOCATABLE :: IFAPAR(:,:,:)
!
      ! THE NUMBER OF HALO CELLS PRO PARTITION 
      INTEGER, ALLOCATABLE :: NHALO(:) 
!
!     WORK VARIABLES
! 
      INTEGER IFALOC(3)
      LOGICAL FOUND
      INTEGER NDP_2D
      INTEGER EF,POS
      INTEGER, ALLOCATABLE :: NBRE_EF(:),NBRE_EF_LOC(:),EF_I(:)
      INTEGER, ALLOCATABLE :: TAB_TMP(:),EF_II(:)
      LOGICAL HALO
      INTEGER NOEUD,NBRE_NOEUD_INTERNE
      INTEGER NBRE_EF_I
      LOGICAL INTERFCE      
!
! #### FOR SECTIONS 
!
      TYPE (CHAIN_TYPE), ALLOCATABLE :: CHAIN(:)
      INTEGER, PARAMETER :: NSEMAX=500 ! MAX NUMBER OF SEGMENTS IN A SECTION 
      INTEGER, ALLOCATABLE :: LISTE(:,:), ANPBEG(:),ANPEND(:) 
      INTEGER :: NSEC, IHOWSEC, ISEC, IELEM, IM(1), IN(1), NPBEG, NPEND 
      INTEGER :: PT, I1,I2,I3, ARR,DEP, ILPREC,ILBEST,ELBEST,IGBEST
      DOUBLE PRECISION :: XA, YA, DISTB, DISTE, DMINB, DMINE
      DOUBLE PRECISION :: DIST1, DIST2, DIST3, DIST
      CHARACTER(LEN=MAXLENHARD) :: NAMESEC
      LOGICAL :: WITH_SECTIONS=.FALSE., DONE_SECTIONS=.FALSE.
!
! #### FOR ZONES 
!
      LOGICAL :: WITH_ZONES=.FALSE., DONE_ZONES=.FALSE.
      INTEGER, ALLOCATABLE :: FRICTION(:)
      CHARACTER(LEN=MAXLENHARD) :: NAMEZFI
      INTEGER :: NZFI,I_GLOB,VAL_ZFI
!
!     CD: FLAG FOR SERAFIN FORMAT (.TRUE. IF DOUBLE PRECISION)
!
      LOGICAL SERAFIND
!     PARTEL EXTENS
      CHARACTER(len=11) :: EXTENS
      EXTERNAL EXTENS
!
!----------------------------------------------------------------------
!
      NDP_2D=3
!
      CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
      TIMECOUNT = .TRUE.
      IF (PARSEC==0) TIMECOUNT = .FALSE.  ! COUNT_RATE == 0 : NO CLOCK
      IF (TIMECOUNT) TDEB = TEMPS
!
      LNG=2 ! JE NE PARLE FRANCAIS, JE SUIS BARBARIEN
      LU=6  ! FORTRAN STANDARD OUPUT CHANNEL
      LI=5  ! FORTRAN STANDARD INPUT CHANNEL
!
!     ALLOCATE(NULLTABLE(0))       
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
           READ(72,*) NAMEINP             
        ELSE                              
           WRITE(LU, ADVANCE='NO', FMT=
     &         '(/,'' SELAFIN INPUT NAME <INPUT_NAME>: '')')
           READ(LI,'(A)') NAMEINP
        ENDIF                            
        IF (NAMEINP.EQ.' ') THEN
          WRITE (LU,'('' NO FILENAME'')') 
        ELSE
          IF (NAMEINP(1:3)=='ES3') THEN
!----------------------------------------------------------------------
! NAMES OF THE INPUT FILE TO EVENTUALLY GUIDE TO PARES3D
! IF PARALLEL COMPUTATION WITH ESTEL3D
! PARTEL ADAPTED TO ESTEL3D CODE
            CALL PARES3D(NAMEINP,LI,FOUND)
            GOTO 299
          ELSE
! CONTINUE WITH TELEMAC CODES
            WRITE(LU,*) 'INPUT: ',NAMEINP
            EXIT 
          ENDIF
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
           READ(72,*) PMETHOD                
        ELSE                                 
           WRITE(LU, ADVANCE='NO',FMT=
     &    '(/,'' PARTITIONING METHOD <PMETHOD> 
     &    [1 (metis) OR 2 (scotch)]: '')') 
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
      I_LEN=I_SP - I
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
      OPEN(NINP,FILE=NAMEINP,STATUS='OLD',FORM='UNFORMATTED')
      REWIND NINP
!
!----------------------------------------------------------------------
!
! START READING THE GEOMETRY OR RESULT FILE
!
      READ(NINP,END=290,ERR=290) TITLE
!
!     SINGLE OR DOUBLE PRECISION
!
      SERAFIND=.FALSE.
      IF(TITLE(73:80) .EQ. 'SERAFIND') THEN 
        SERAFIND=.TRUE.
      ENDIF
!
!     NUMBER OF VARIABLES IN THE FILE
!
      READ (NINP) I, J
      NVAR = I + J 
!
      DO I=1,NVAR
        READ(NINP) VARI     
        VARIABLE(I) = VARI     
        DO J=1,32
          IF(VARI(J:J).EQ.' ') VARI(J:J) = '-'
        ENDDO
      ENDDO 
!
! READ THE REST OF THE SELAFIN FILE
! 10 INTEGERS, THE FIRST IS THE NUMBER OF RECORDS (TIMESTEPS)
!
      READ(NINP) (IB(I), I=1,10)
      IF(IB(8).NE.0.OR.IB(9).NE.0) THEN
        WRITE(LU,*) 'THIS IS A PARTIAL OUTPUT FILE'
        WRITE(LU,*) 'MAYBE MEET GRETEL BEFORE...'
      ENDIF 
      NPLAN = IB(7) 
      IF(IB(10).EQ.1) THEN 
        READ(NINP) DATE(1),DATE(2),DATE(3),TIME(1),TIME(2),TIME(3)
      ENDIF 
!
      READ(NINP) NELEM,NPOIN,NDP,NDUM
      NPOIN_TOT=NPOIN
      IF(NPLAN.GT.1) THEN 
        WRITE(LU,*) ' '
        WRITE(LU,*) '3D MESH DETECTED' 
        NPOIN2 = NPOIN/NPLAN
        NELEM2 = NELEM/(NPLAN-1)
        WRITE(LU,*) 'NDP NODES PER ELEMENT:             ',NDP
        WRITE(LU,*) 'NPLAN NUMBER OF MESH LEVELS:       ',NPLAN
        WRITE(LU,*) 'NPOIN2 NUMBER OF 2D MESH NODES:    ',NPOIN2
        WRITE(LU,*) 'NPOIN NUMBER OF 3D MESH NODES:     ',NPOIN
        WRITE(LU,*) 'NELEM2 NUMBER OF 2D MESH ELEMENTS: ',NELEM2
        WRITE(LU,*) 'NELEM NUMBER OF 3D MESH ELEMENTS:  ',NELEM
        IF (MOD(NPOIN,NPLAN).NE.0) THEN 
          WRITE (LU,*) 'BUT NPOIN2 /= NPOIN3/NPLAN!'
          CALL PLANTE(1)
          STOP   
        ENDIF
        IF (MOD(NELEM,(NPLAN-1)).NE.0) THEN 
          WRITE (LU,*) 'BUT NELEM2 /= NELEM3/NPLAN!'
          CALL PLANTE(1)
          STOP
        ENDIF
        WRITE(LU,*) ' '
      ELSE
        WRITE(LU,*) ' '
        WRITE(LU,*) 'ONE-LEVEL MESH.'
        WRITE(LU,*) 'NDP NODES PER ELEMENT:         ',NDP
        WRITE(LU,*) 'NPOIN NUMBER OF MESH NODES:    ',NPOIN
        WRITE(LU,*) 'NELEM NUMBER OF MESH ELEMENTS: ',NELEM
        WRITE(LU,*) ' '
        NPOIN2 = NPOIN
        NELEM2 = NELEM
      ENDIF
!
      IF(NDP.EQ.3) THEN  
        WRITE(LU,*) 'THE INPUT FILE ASSUMED TO BE 2D SELAFIN'
      ELSEIF (NDP.EQ.6) THEN
        WRITE(LU,*) 'THE INPUT FILE ASSUMED TO BE 3D SELAFIN'
      ELSE   
        WRITE(LU,*) 'THE ELEMENTS ARE NEITHER TRIANGLES NOR PRISMS!'
        WRITE(LU,*) 'NDP = ',NDP
        CALL PLANTE(1)
        STOP
      ENDIF
!
! NOW LET US ALLOCATE 
!
      ALLOCATE (IKLES(NELEM2*3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLES')
      IF(NPLAN.GT.1) THEN
        ALLOCATE (IKLES3D(NELEM*NDP),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'IKLES3D')
      ENDIF 
      ALLOCATE (IRAND(NPOIN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IRAND')
!
!     SIZE 3: FIRST TWO FUNCTIONS ARE X AND Y, 3 IS ALL OTHER
!             VARIABLES (THEY WILL BE COPIED AND WRITTEN
!             ONE AFTER THE OTHER...)
!     NPOIN IS 3D HERE IN 3D
!
      IF(SERAFIND) THEN
        ALLOCATE (F(NPOIN,3),STAT=ERR) 
        CALL CHECK_ALLOCATE(ERR, 'F')
        ALLOCATE (F_D(NPOIN,3),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'F_D')
      ELSE
        ALLOCATE (F(NPOIN,3),STAT=ERR)
        IF(ERR.NE.0) CALL CHECK_ALLOCATE(ERR, 'F')
      ENDIF
!
! CONNECTIVITY TABLE:
!
      IF(NPLAN.LE.1) THEN
        READ(NINP) ((IKLES((K-1)*NDP+J),J=1,NDP),K=1,NELEM)        
      ELSE
        READ(NINP) ((IKLES3D((K-1)*NDP+J),J=1,NDP),K=1,NELEM)
!       BUILDING IKLES
        DO J=1,3
          DO K=1,NELEM2
            IKLES((K-1)*3+J)=IKLES3D((K-1)*6+J)
          ENDDO
        ENDDO
      ENDIF
!
! BOUNDARY NODES INDICATIONS
!
      READ(NINP) (IRAND(J),J=1,NPOIN)
!
! IRAND IS NOT ALWAYS CORRECT AND MAY LEAD TO ERRORS
! THE BO0UNDARY FILE IS USED INSTEAD
!
! X-, Y-COORDINATES
!
      IF(SERAFIND) THEN
        READ(NINP,END=297,ERR=297) (F_D(J,1),J=1,NPOIN)
        READ(NINP,END=297,ERR=297) (F_D(J,2),J=1,NPOIN)
!       COPYING F_D IN F FOR FRONT2_PARTEL 
        DO J=1,NPOIN
          F(J,1)=REAL(F_D(J,1))
          F(J,2)=REAL(F_D(J,2))
        ENDDO
      ELSE
        READ(NINP,END=297,ERR=297) (F(J,1),J=1,NPOIN)
        READ(NINP,END=297,ERR=297) (F(J,2),J=1,NPOIN)
      ENDIF
!     
! NOW THE LOOP OVER ALL RECORDS (TIMESTEPS) - FOR AN INITIAL 
! CONDITIONS FILE AUTOMATICALLY THE LAST TIME STEP VALUES ARE 
! TAKEN (!)
!
      ILOOP = 0
!
!     INFINITE LOOP THAT ENDS WITH THE END=111 INSTRUCTION IN READ
!
      DO 
!
!       READ THE TIME STEP
!
        IF(SERAFIND) THEN
          READ(NINP, END=111, ERR=298) TIMES_D          
          TIMED_D = TIMES_D/3600.D0
          WRITE(LU,*) 'TIMESTEP: ',TIMES_D,'S = ',TIMED_D,'H'
        ELSE
          READ(NINP, END=111, ERR=298) TIMES
          TIMED = TIMES/3600.
          WRITE(LU,*) 'TIMESTEP: ',TIMES,'S = ',TIMED,'H'
        ENDIF
        ILOOP = ILOOP + 1
!
!       READ THE TIME VARIABLES; NO 1 AND 2 ARE X,Y
!
!       NOTE JMH 20/06/2012: HERE RECORDS ARE JUST JUMPED
!       NO NEED TO STORE, IT ALLOWS REDUCING THE SIZE OF F
!
        DO K=3,NVAR+2
!         READ(NINP, END=301, ERR=301) (F(J,K), J=1,NPOIN)
          READ(NINP, END=301, ERR=301) 
        ENDDO
!
      ENDDO
!
111   CONTINUE  
      CLOSE (NINP)
      WRITE(LU,*) 'THERE ARE ',ILOOP,' TIME-DEPENDENT RECORDINGS'
!
!----------------------------------------------------------------------
!
!     READ THE BOUNDARY CONDITIONS FILE, BUT ALLOCATE FIRST
!
!     NPTFRMAX MUST BE GREATER OR EQUAL TO NPFTR
!
      NPTFRMAX = NPOIN2
!
!     IT COULD BE THAT (TO BE PROVEN, BUT NILES MAY BE LARGE)
!     NPTFRMAX = 2*NPOIN2-NELEM2-2+2*NILES
!
      ALLOCATE (LIHBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LIHBOR')
      ALLOCATE (LIUBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LIUBOR')
      ALLOCATE (LIVBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LIVBOR')
      ALLOCATE (HBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'HBOR')
      ALLOCATE (UBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'UBOR')
      ALLOCATE (VBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'VBOR')
      ALLOCATE (AUBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'AUBOR')
      ALLOCATE (TBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'TBOR')
      ALLOCATE (ATBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ATBOR')
      ALLOCATE (BTBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'BTBOR')
      ALLOCATE (LITBOR(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LITBOR')
      ALLOCATE (NBOR(NPTFRMAX),STAT=ERR)  ! FOR FRONT2
      CALL CHECK_ALLOCATE(ERR, 'NBOR')
      ALLOCATE (NUMLIQ(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NUMLIQ')
      ALLOCATE (CHECK(NPTFRMAX),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'CHECK')
!
! CORE NAME LENGTH
!
      I_S  = LEN(NAMECLI)
      I_SP = I_S + 1
      DO I=1,I_S
         IF (NAMECLI(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
      I_LENCLI = I_LEN
!
      IF (I_LENINP > MAXLENSOFT) THEN
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
      OPEN(NCLI,FILE=NAMECLI,STATUS='OLD',FORM='FORMATTED')
      REWIND NCLI
!
!     READING BOUNDARY FILE AND COUNTING BOUNDARY POINTS
!
      K=1
 900  CONTINUE
      READ(NCLI,*,END=901,ERR=901) LIHBOR(K),LIUBOR(K),
     &                             LIVBOR(K),
     &             HBOR(K),UBOR(K),VBOR(K),AUBOR(K),LITBOR(K),
     &             TBOR(K),ATBOR(K),BTBOR(K),NBOR(K),CHECK(K)
!
      K=K+1
      GOTO 900
 901  CONTINUE
      NPTFR = K-1
      CLOSE(NCLI)
!
!----------------------------------------------------------------------
! NUMBERING OF OPEN BOUNDARIES 
! NUMBERING OF LIQUID BOUNDARY, IF 0 = SOLID
! OPN: NUMBER OF OPEN BOUNDARY
! IN ORDER TO DO IT IN THE SAME WAY AS TELEMAC DOES, 
! IT IS BEST TO CALL FRONT2 HERE
!
! FOR CALLING BIEF MESH SUBROUTINES
! CAN BE OPTIMISED / USES A LOT OF MEMORY 
! THE ONLY REASON IS TO OBTAIN KP1BOR AND NUMLIQ
!
      ALLOCATE (DEJAVU(NPTFR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'DEJAVU')
      ALLOCATE (KP1BOR(NPTFR,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'KP1BOR')
!     CHANGED NELEM TO NELEM2, NDP TO 3 HUH! 
!     CAUSING ERRORS WHEN 3D RESTART/REFERENCE FILES ARE PARTITIONED
!     AND BC FILE IS WRITTEN AGAIN (WHAT FOR, ACTUALLY???) 
!     CAUSE: CALLING VOISIN WITH NELEM2 BUT IFABOR(NELEM=NELEM3,NDP=6)
      ALLOCATE (IFABOR(NELEM2,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IFABOR')
      ALLOCATE (IFANUM(NELEM2,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IFANUM')
      ALLOCATE (IKLBOR(NPTFR,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLBOR')
      ALLOCATE (NELBOR(NPTFR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NELBOR')
      ALLOCATE (NULONE(NPTFR,2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NULONE')
      ALLOCATE (ISEGF(NPTFR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ISEGF')
      ALLOCATE (IKLE(NELEM2,3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLE')
      ALLOCATE (IT1(NPOIN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IT1')
      ALLOCATE (IT2(NPOIN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IT2')
      ALLOCATE (IT3(NPOIN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IT3')
      NPTIR = 1
      ALLOCATE (NACHB(NBMAXNSHARE,NPTIR),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NACHB')
      ALLOCATE (DEBSOL(MAXFRO),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'DEBSOL')
      ALLOCATE (FINSOL(MAXFRO),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'FINSOL')
      ALLOCATE (DEBLIQ(MAXFRO),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'DEBLIQ')
      ALLOCATE (FINLIQ(MAXFRO),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'FINLIQ')
      ALLOCATE (X_D(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'X_D')
      ALLOCATE (Y_D(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'Y_D')
!
! TRANSFORM IKLES--> IKLE FOR 2D ROUTINES  (AN OLD TELEMAC DISEASE) 
!
      DO I = 1,3
        DO J  = 1,NELEM2
          IKLE(J,I) = IKLES((J-1)*3+I)
        ENDDO
      ENDDO
!
      ! Set NCSIZE TO 1 TO USE VOISIN IN SERIAL MODE
      NCSIZE =  1
   
      CALL VOISIN(IFABOR, NELEM2, NELEM2, 11, IKLE, NELEM2,
     &                   NPOIN2, NACHB, NBOR(1:NPTFR), NPTFR, IT1, IT2)
      DEALLOCATE(NACHB)
!
      CALL ELEBD (NELBOR, NULONE, KP1BOR, IFABOR, NBOR(1:NPTFR), IKLE, 
     &                   NELEM2, IKLBOR, NELEM2, NELEM2, 
     &                   NPOIN2, NPTFR, 11, LIHBOR, 2, IFANUM,
     &                   1, ISEGF, IT1, IT2, IT3 )
!
      IF (NAMEINP(1:3)== 'ART') THEN
         OPEN(UNIT=89,FILE='FRONT_GLOB.DAT')
         WRITE(89,*) NPOIN_TOT
         WRITE(89,*) NPTFR
         DO K=1,NPTFR
            WRITE(89,*) NBOR(K)
         END DO 
         DO K=1,NPTFR
            WRITE(89,*) KP1BOR(K,1)
         END DO
         DO K=1,NPTFR
            WRITE(89,*) KP1BOR(K,2)
         END DO 
         CLOSE(89)
      END IF
      NFRLIQ = 0
      NFRSOL = 0
      DEBSOL(:) = 0
      FINSOL(:) = 0
      DEBLIQ(:) = 0
      FINLIQ(:) = 0
      DO I=1,NPOIN2
        X_D(I) = dble(F(I,1))
        Y_D(I) = dble(F(I,2))
      ENDDO
      CALL FRONT2 
     &       (NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &       LIHBOR,LIUBOR,X_D,Y_D,
     &       NBOR,KP1BOR(1:NPTFR,1),DEJAVU,NPOIN2,NPTFR,
     &       2,.TRUE.,NUMLIQ,MAXFRO)  
!
      DEALLOCATE (DEJAVU)
      DEALLOCATE (X_D)
      DEALLOCATE (Y_D)
!JAJ //// IFABOR APPLIED LATER FOR FINDING HALO CELL NEIGHBOURHOODS 
!!!!      DEALLOCATE (IFABOR)
      DEALLOCATE (IFANUM)
      DEALLOCATE (IKLBOR)
      DEALLOCATE (NULONE)
      DEALLOCATE (ISEGF)
      DEALLOCATE (IT1)
      DEALLOCATE (IT2)
      DEALLOCATE (IT3)
!
!======================================================================
! PARTITIONING
!
!
!
!======================================================================
! STEP 2 : PARTITIONING THE MESH 
!
! OTHER PARTITIONING METHODS SHOULD BE USED (SCOTCH FOR EXAMPLE)
!     ALL PROCESSORS PERFORM THIS TASK TO AVOID COMMUNICATION
!     THE USE OF PARMETIS OR PTSCOTCH COULD BE USED FOR LARGER MESHES
!     IF THERE WILL BE SOME MEMORY ALLOCATION PROBLEM 
!======================================================================      
!    
      ALLOCATE(EPART(NELEM2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'EPART')
      ALLOCATE(NPART(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NPART')

!
!     PARTITIONNING METHOD
!     1 : METIS
!     2 : SCOTCH
      WRITE(LU,*) ' THE MESH PARTITIONING STEP STARTS'
      IF(TIMECOUNT) THEN 
!       CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
        TDEBP = TEMPS
      ENDIF
      CALL PARTITIONER(PMETHOD, NELEM2, NPOIN2, 3, NPARTS, 
     &                 IKLES, EPART, NPART)
      IF (TIMECOUNT) THEN
!       CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
        TFINP = TEMPS
        WRITE(LU,*) ' RUNTIME OF METIS ',
     &            (1.0*(TFINP-TDEBP))/(1.0*PARSEC),' SECONDS'
      ENDIF
      WRITE(LU,*) ' THE MESH PARTITIONING STEP HAS FINISHED'
!
!
!----------------------------------------------------------------------
!      
!======================================================================
! STEP 3 : ALLOCATE THE GLOBAL  ARRAYS NOT DEPENDING OF THE PARTITION
!     
!======================================================================   
!     
!     KNOGL(I) =>  GLOBAL LABEL OF THE LOCAL POINT I
! 
      ALLOCATE (KNOGL(NPOIN2,NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'KNOGL')
      KNOGL(:,:)=0
!      
!     NBRE_EF(I) => NUMBER OF FINITE ELEMENT CONTAINING I
!     I IS A GLOBAL LABEL 
      ALLOCATE (NBRE_EF(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NBRE_EF')
!      
      ALLOCATE (PART_P(NPOIN2,0:NBMAXNSHARE),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'PART_P')
      PART_P(:,:)=0
!      
      ALLOCATE (CUT_P(NPOIN2,NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'CUT_P')
!      
      ALLOCATE (GELEGL(NELEM2,NPARTS),STAT=ERR) 
      CALL CHECK_ALLOCATE(ERR, 'GELEGL')
      
      ALLOCATE (SORT(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'CUT_P')
      
      ALLOCATE (CUT(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'CUT_P')
      
      ALLOCATE (NELEM_P(NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NELEM_P')
       
      ALLOCATE (NPOIN_P(NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NPOIN_P')
      
      ALLOCATE (NPTFR_P(NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NPTFR_P')
      
      ALLOCATE (NPTIR_P(NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NPTIR_P')
      
      ALLOCATE (NHALO(NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NHALO')

      ALLOCATE(TAB_TMP( NBMAXNSHARE),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'TAB_TMP')
      
      ALLOCATE(IFAPAR(NPARTS,7,NBMAXHALO),STAT=ERR)
         CALL CHECK_ALLOCATE(ERR, 'IFAPAR')
         IFAPAR(:,:,:)=0
!
!======================================================================
! STEP 4 : COMPUTE THE NUMBER OF FINITE ELEMENTS AND POINTS
!     BELONGING TO SUBMESH I
!
!======================================================================   
!
      
!     FIRSTLY, ALL MPI PROCESSES  WORK ON THE WHOLE MESH
!     ----------------------------------------------      
!   
!     LOOP OVER THE FINITE ELEMENT OF THE MESH 
!     TO COMPUTE THE NUMBER OF FINITE ELEMENTS CONTAINING EACH POINT NOEUD
         IF (NAMEINP(1:3) == 'ART') THEN     
            DO EF=1,NELEM2
               DO K=1,NDP_2D
                  NOEUD=IKLES((EF-1)*3+K)
                  IF (IRAND(NOEUD) .NE. 0) THEN
                     EPART(EF)=1
                  END IF
               END DO 
            END DO
         END IF
         
         NBRE_EF(:)=0
      DO EF=1,NELEM2
         DO K=1,NDP_2D
            NOEUD=IKLES((EF-1)*3+K)
            NBRE_EF(NOEUD)=NBRE_EF(NOEUD)+1
         END DO
      END DO 
      DO I=1,NPARTS    
!
!     LOOP OVER THE FINITE ELEMENT OF THE MESH TO COMPUTE 
!     THE NUMBER OF THE FINITE ELEMENT AND POINTS BELONGING 
!     TO SUBMESH I
!   
         NELEM_P(I)=0
         NPOIN_P(I)=0
         DO EF=1,NELEM2
            IF(EPART(EF).EQ.I) THEN
               NELEM_P(I)=NELEM_P(I)+1
               DO K=1,NDP_2D
                  NOEUD=IKLES((EF-1)*3+K)
                  IF (KNOGL(NOEUD,I) .EQ. 0) THEN
                     NPOIN_P(I)=NPOIN_P(I)+1
                     KNOGL(NOEUD,I)=NPOIN_P(I)
                  END IF
               END DO 
            END IF
         END DO
      END DO  
!    
!======================================================================
!     STEP 4 : ALLOCATION OF LOCAL ARRAYS NEEDED BY MPI PROCESSUS ID
!              WORKING ON SUBMESH ID+1
!======================================================================   
!
      MAX_NELEM_P=MAXVAL(NELEM_P)
      MAX_NPOIN_P=MAXVAL(NPOIN_P)
!
!     ELEGL(E) => GLOBAL LABEL OF THE FINITE ELEMENT E
!     E IS THE LOCAL LABEL ON SUBMESH I 
      ALLOCATE (ELELG(MAX_NELEM_P,NPARTS),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ELELG')
      ELELG(:,:)=0
!     KNOLG(I) => GLOBAL LABEL OF THE POINT I
!     I IS THE LOCAL LABEL ON SUBDOMAIN I
      IF(NPLAN.EQ.0) THEN
        ALLOCATE (KNOLG(MAX_NELEM_P,NPARTS),STAT=ERR)
      ELSE
        ALLOCATE (KNOLG(MAX_NPOIN_P*NPLAN,NPARTS),STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'KNOLG')
      KNOLG(:,:)=0
!     NBRE_EF_LOC(I) : NUMBER OF FINITE ELEMENTS CONTAINING THE POINT I
!                      ON SUBMESH I  
!     I IS THE LOCAL LABEL ON SUBMESH I
      ALLOCATE (NBRE_EF_LOC(MAX_NELEM_P),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'NBRE_EF_LOC')
!
!     EF_I(E) IS THE GLOBAL LABEL OF THE INTERFACE FINITE ELEMENT NUMBER E
      ALLOCATE (EF_I(MAX_NELEM_P),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'EF_I')
!     EF_II(E) IS THE LOCAL LABEL OF THE INTERFACE FINITE ELEMENT NUMBER E
      ALLOCATE (EF_II(MAX_NELEM_P),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'EF_II')      
!
!======================================================================
!     STEP 5 : INITIALISATION  OF LOCAL ARRAYS 
!              (GELELG AND ELELG, NBRE_EF_LOC)             
!======================================================================
!   
      DO I=1,NPARTS
         NELEM_P(I)=0
         DO EF=1,NELEM2
           IF(EPART(EF).EQ.I) THEN
              NELEM_P(I)=NELEM_P(I)+1
              ELELG(NELEM_P(I),I)=EF
              GELEGL(EF,I)=NELEM_P(I)
           ENDIF
         ENDDO
         DO J=1,NPOIN_P(I)
           NBRE_EF_LOC(J)=0
         ENDDO
!        
!======================================================================
!     STEP 5 : COMPUTE THE NUMBER OF BOUNDARY AND INTERFACE POINTS
!              INITIALISATION OF NBRE_EF_LOC 
!======================================================================   
!         
         NPOIN_P(I)=0
         NPTFR_P(I)=0
         NBRE_NOEUD_INTERNE=0
!         
         DO J=1,NELEM_P(I)
           EF=ELELG(J,I)
           DO K=1,3
             NOEUD=IKLES((EF-1)*3+K)
             NBRE_EF_LOC(KNOGL(NOEUD,I))=NBRE_EF_LOC(KNOGL(NOEUD,I))+1 
             IF(NBRE_EF_LOC(KNOGL(NOEUD,I)) .EQ. 1) THEN
!              THE POINT NOEUD IS ENCOUNTERED FOR THE FIRST TIME 
               NPOIN_P(I)=NPOIN_P(I)+1    
!              IS NOEUD A BOUNDARY POINT ?     
               IF(IRAND(NOEUD) .NE. 0) THEN
                 NPTFR_P(I)= NPTFR_P(I)+1
               ENDIF
!              MODIFICATION OF KNOGL
               KNOLG(NPOIN_P(I),I)=NOEUD
             ENDIF   
!            NOEUD IS A INTERNAL POINT IF ALL FINITE ELEMENTS
!            CONTAINING IT BELONGS TO THE SAME SUBMESH
             IF(NBRE_EF_LOC(KNOGL(NOEUD,I)) .EQ. NBRE_EF(NOEUD)) THEN 
               NBRE_NOEUD_INTERNE=NBRE_NOEUD_INTERNE+1
             ENDIF
           ENDDO
         ENDDO
!        
         NPTIR_P(I)=0 
!
!        NOMBRE DE NOEUD INTERFACE DU SDI
!
         NBRE_EF_I=0            ! NOMBRE D'ELEMENTS FINIS INTERFACES DU SDI
         DO J=1,NELEM_P(I)      ! ON PARCOURS A NOUVEAU LES ELEMENTS FINIS DU SDI
           INTERFCE=.FALSE.
           EF=ELELG(J,I)
           DO K=1,NDP_2D
             NOEUD=IKLES((EF-1)*3+K)
             IF(ABS(NBRE_EF_LOC(KNOGL(NOEUD,I))).NE.NBRE_EF(NOEUD))
     &        THEN
               INTERFCE=.TRUE.
             ENDIF
             IF(NBRE_EF_LOC(KNOGL(NOEUD,I)).NE.NBRE_EF(NOEUD).AND. 
     &          NBRE_EF_LOC(KNOGL(NOEUD,I)).GT.0) THEN
!              NOEUD EST INTERFACE CAR IL RESTE DES ELEMENTS FINIS
!              HORS DE SDI QUI LE CONTIENT
               INTERFCE=.TRUE.
               NPTIR_P(I)=NPTIR_P(I)+1
               CUT_P(NPTIR_P(I),I)=NOEUD
               PART_P(NOEUD,0)=PART_P(NOEUD,0)+1
               POS=PART_P(NOEUD,0)
               IF(POS.GT.NBMAXNSHARE-1) THEN
                 WRITE(LU,*)  'ERROR : AN INTERFACE NODE BELONGS TO 
     &                 MORE THAN NBMAXNSHARE-1 SUBDOMAINS'
                  CALL PLANTE(1)
                  STOP 
               ENDIF
               PART_P(NOEUD,POS)=I
               NBRE_EF_LOC(KNOGL(NOEUD,I))=-NBRE_EF_LOC(KNOGL(NOEUD,I))
             ENDIF
           ENDDO 
           IF(INTERFCE) THEN 
             NBRE_EF_I=NBRE_EF_I+1 ! L'ELEMENT FINI EST DONC AUSSI INTERFACE
             EF_I(NBRE_EF_I)=EF
             EF_II(NBRE_EF_I)=J
           ENDIF
         ENDDO
!         
!        FIRST LOOP TO COMPUTE THE NUMBER OF HALO TO ALLOCATE IFAPAR 
!        
!        FILLING IFAPAR
!
         NHALO(I)=0
         DO J=1,NBRE_EF_I  ! ON PARCOURT JUSTE LES ELEMENTS FINIS INTERFACES POUR   
                           ! DETERMINER DES HALO
            EF=EF_I(J)
            HALO=.FALSE.
            IFALOC(:)=IFABOR(EF,:)
            WHERE (IFALOC .GT. 0) 
               IFALOC=EPART(IFALOC)
            END WHERE
            HALO=ANY(IFALOC .GT. 0 .AND. IFALOC .NE. I)
            IF(HALO) THEN
               NHALO(I)=NHALO(I)+1
               IF(NHALO(I) > NBMAXHALO) THEN
                 WRITE(LU,*)  'ERROR : NBMAXHALO TOO SMALL'
                 CALL PLANTE(1)
                 STOP 
               ENDIF
               IFAPAR(I,1,NHALO(I))=EF_II(J)
               IFAPAR(I,2:4,NHALO(I))=IFALOC(:)
               IFAPAR(I,5:7,NHALO(I))=IFABOR(EF_I(J),:)
            ENDIF
         ENDDO 
      ENDDO
!         
      MAX_N_NEIGH=MAXVAL(PART_P(:,0))
      IF ( MAX_N_NEIGH > NBMAXNSHARE-1 ) THEN 
         WRITE(LU,*) 'SERIOUS WARNING: ' 
         WRITE(LU,*) 
     &        'AN INTERFACE NODE BELONGS TO ',
     &        'MORE THAN NBMAXNSHARE-1 SUBDOMAINS'
         WRITE(LU,*) 'TELEMAC MAY PROTEST!'
      ENDIF 
      IF(MAX_N_NEIGH.GT.MAXNPROC) THEN
         WRITE (LU,*) 'THERE IS A NODE WHICH BELONGS TO MORE THAN ',
     &        MAXNPROC,' PROCESSORS, HOW COME?'
         CALL PLANTE(1)
          STOP 
      ENDIF
      IF (MAX_N_NEIGH.LT.NBMAXNSHARE-1) MAX_N_NEIGH = NBMAXNSHARE-1
!       
      DO I=1,NPARTS
!        
!-----------------------------------------------------------------------
! THE CORE NAMES FOR THE OUTPUT BC FILES ACCORDING TO THE NUMBER OF PARTS
!
      NAMECLM = NAMECLI    ! CORE NAME LENGTH IS I_LENCLI
      NAMEOUT = NAMEINP    ! CORE NAME LENGTH IS I_LENINP
!
!----------------------------------------------------------------------
!     WORK ON THE BOUNDARIES WRITING THE BC FILES SIMULTANEOUSLY...
!     
      NAMECLM(I_LENCLI+1:I_LENCLI+11) = EXTENS(NPARTS-1,I-1)
      OPEN(NCLM,FILE=NAMECLM,STATUS='UNKNOWN',FORM='FORMATTED')
      REWIND(NCLM)
!     
! FILE OPENED, NOW WORK ON BOUNDARIES 
! -----------------------------------
!
! WHEN THE BOUNDARY NODE BELONGS TO THIS SUBDOMAIN IT WILL BE TAKEN
! J IS THE RUNNING BOUNDARY NODE NUMBER
!
       J = 0
!
       DO K=1,NPTFR
!
! BOUNDARY NODES BELONGING TO THIS PARTITION
!
         IF(KNOGL(NBOR(K),I).NE.0) THEN
           J = J + 1
           ISEG = 0
           XSEG = 0.0
           YSEG = 0.0
!     
!     IF THE ORIGINAL (GLOBAL) BOUNDARY LEADS FURTHER INTO 
!     ANOTHER PARTITION THEN ISEG IS SET NOT EQUAL TO ZERO
!     THE NEXT NODE ALONG THE GLOBAL BOUNDARY HAS IPTFR = M
!     (BUT CHECK THE CASE THE CIRCLE CLOSES)
!     
           M = KP1BOR(K,1)
!
! NBOR_P CANNOT BE USED, IT IS NOT FULLY FILLED WITH DATA
!
           ISO = 0
!     CHECKING IF THE ADJACENT ELEMENT IS NOT IN THE
!     SUB-DOMAIN
           IF (EPART(NELBOR(K)).NE.I) THEN
!     
                  ISEG = NBOR(M)
                  IF(SERAFIND) THEN
                    XSEG = REAL(F_D(ISEG,1))
                    YSEG = REAL(F_D(ISEG,2))
                  ELSE
                    XSEG = F(ISEG,1)
                    YSEG = F(ISEG,2)
                  ENDIF
                  ISO = ISO + 1
            ENDIF
!     
            M = KP1BOR(K,2)
!     
!     SAME AS ABOVE, BUT PREVIOUS SEGMENT ,THUS M, NOT K
            IF (EPART(NELBOR(M)).NE.I) THEN
               ISEG = -NBOR(M)
               IF(SERAFIND) THEN
                 XSEG = REAL(F_D(-ISEG,1))
                 YSEG = REAL(F_D(-ISEG,2))
               ELSE
                 XSEG = F(-ISEG,1)
                 YSEG = F(-ISEG,2)
               ENDIF
               ISO = ISO + 1
            ENDIF
!     
!     WHEN BOTH NEIGHBOURS BOUNDARY NODES BELONG TO ANOTHER PARTITION
!     
            IF (ISO == 2) THEN
               ISEG = -9999
               ISO = 0
               WRITE(LU,*) 'ISOLATED BOUNDARY POINT', 
     &              NBOR(K), KNOGL(NBOR(K),I)
            ENDIF
!     
!     WRITE A LINE OF THE FIRST (CLASSICAL) PART OF THE BOUNDARY FILE
! CONCERNING THE NODE WHICH HAS BEEN RESEARCHED
!
            WRITE (NCLM,4000) 
     &           LIHBOR(K), LIUBOR(K), LIVBOR(K),
     &           HBOR(K), UBOR(K), VBOR(K), 
     &            AUBOR(K), LITBOR(K), TBOR(K), ATBOR(K), BTBOR(K),
!     JMH 16/06/2008: INITIAL LINE NUMBER OR COLOUR
     &           NBOR(K),CHECK(K), ISEG, XSEG, YSEG, NUMLIQ(K)
!    &           NBOR(K),    J   , ISEG, XSEG, YSEG, NUMLIQ(K)
     
!     19/10/2007 ER+JMH SUR RECOMMANDATION CHARLES MOULINEC
!     MAIS XSEG ET YSEG NE SONT PLUS UTILISES
 4000       FORMAT (1X,I2,1X,2(I1,1X),3(F24.12,1X),1X,
     &           F24.12,3X,I1,1X,3(F24.12,1X),1I9,1X,1I9,
     &           1X,I10,1X,2(F27.15,1X),I8)
         ENDIF
!     
      END DO
!      
      FMT4='(I7)'
      WRITE (NCLM,*) NPTIR_P(I)
       IF (MAX_N_NEIGH < NBMAXNSHARE-1) MAX_N_NEIGH = NBMAXNSHARE-1
       FMT4='(   (I7,1X))'
       WRITE (FMT4(2:4),'(I3)') MAX_N_NEIGH+1
!  
!      SORTING NODE NUMBERS TO SORT(J) SO THAT CUT_P(SORT(J)) IS ORDERED 
!      CUT IS OVERWRITTEN NOW
!
         DO J=1,NPTIR_P(I)
           CUT(J)=CUT_P(J,I)
         ENDDO
!
! IF A NODE HAS BEEN ALREADY FOUND AS MIN, CUT(NODE) GETS 0
!
         DO J=1,NPTIR_P(I)
           IDUM = NPOIN2+1  ! LARGEST POSSIBLE NODE NUMBER + 1
           K=0
 401       CONTINUE
           K = K + 1
           IF ( CUT(K) /= 0 .AND. CUT_P(K,I) < IDUM ) THEN
             SORT(J) = K
             IDUM = CUT_P(K,I)
           ENDIF
           IF ( K < NPTIR_P(I) ) THEN
             GOTO 401
           ELSE
             CUT(SORT(J)) = 0
           ENDIF
         ENDDO
!
         DO J=1,NPTIR_P(I)
            TAB_TMP=0
            L=0
            DO K=1,MAX_N_NEIGH              
               IF(PART_P(CUT_P(SORT(J),I),K) .NE. I .AND. 
     &        PART_P(CUT_P(SORT(J),I),K) .NE. 0) THEN
                  L=L+1
               TAB_TMP(L)=PART_P(CUT_P(SORT(J),I),K)
               ENDIF
            ENDDO 
         WRITE(NCLM,FMT=FMT4) CUT_P(SORT(J),I),
     &                  (TAB_TMP(K)-1, K=1,MAX_N_NEIGH)
         ENDDO
!                                 !     
         DO J=1,NHALO(I)
            DO M=0,2
               IF (IFAPAR(I,2+M,J)>0) THEN
                  IFAPAR(I,5+M,J)=GELEGL(IFAPAR(I,5+M,J),
     &                 IFAPAR(I,2+M,J))
               END IF
            ENDDO
         ENDDO
          DO J=1,NHALO(I)
           DO M=0,2
              IF (IFAPAR(I,2+M,J)>0) THEN
                 IFAPAR(I,2+M,J)=IFAPAR(I,2+M,J)-1
              END IF
           END DO
        END DO      
!
      WRITE(NCLM,'(I9)') NHALO(I)
      DO K=1,NHALO(I)
         WRITE (NCLM,'(7(I9,1X))') IFAPAR(I,:,K) 
      END DO 
!     
      CLOSE(NCLM)
      END DO 
!
      DEALLOCATE(IFAPAR)
      DEALLOCATE(PART_P)
      DEALLOCATE(LIHBOR)
      DEALLOCATE(LIUBOR)
      DEALLOCATE(LIVBOR)
      DEALLOCATE(HBOR)
      DEALLOCATE(UBOR)
      DEALLOCATE(VBOR)
      DEALLOCATE(AUBOR)
      DEALLOCATE(LITBOR)
      DEALLOCATE(TBOR)
      DEALLOCATE(ATBOR)
      DEALLOCATE(BTBOR)
      DEALLOCATE(NBOR)
      DEALLOCATE(NUMLIQ)
      DEALLOCATE(TAB_TMP)
      DEALLOCATE(CHECK)
      DEALLOCATE(GELEGL)
      DEALLOCATE(CUT)
      DEALLOCATE(CUT_P)
      DEALLOCATE(SORT)
!     
      ALLOCATE(IKLES_P(MAX_NELEM_P*3),STAT=ERR)
      IF(NPLAN.GT.1) THEN
        ALLOCATE(IKLES3D_P(6,MAX_NELEM_P,NPLAN-1),STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'IKLES3D_P')
!      
      DO I=1,NPARTS
!
        WRITE(LU,*) 'TREATING SUB-DOMAIN ', I 
!     
!       WRITING GEOMETRY FILES FOR ALL PARTS/PROCESSORS
!
        NAMEOUT(I_LENINP+1:I_LENINP+11) = EXTENS(NPARTS-1,I-1)
!   
        OPEN(NOUT,FILE=NAMEOUT,FORM='UNFORMATTED',STATUS='UNKNOWN') 
!       
        REWIND(NOUT)
!     
!       TITLE, THE NUMBER OF VARIABLES
!     
        WRITE(NOUT) TITLE
        WRITE(NOUT) NVAR,0
        DO K=1,NVAR
          WRITE(NOUT) VARIABLE(K)
        ENDDO
!     
!     10 INTEGERS...
! 1.  IS THE NUMBER OF RECORDINGS IN FILES
! 8.  IS THE NUMBER OF BOUNDARY POINTS (NPTFR_P)
! 9.  IS THE NUMBER OF INTERFACE POINTS (NPTIR_P)
! 10. IS 0 WHEN NO DATE PASSED; 1 IF A DATE/TIME RECORD FOLLOWS
!
!       IB(7) = NPLAN   (ALREADY DONE)
        IB(8) = NPTFR_P(I)
        IB(9) = NPTIR_P(I)
        WRITE(NOUT) (IB(K), K=1,10)
        IF(IB(10).EQ.1) THEN 
           WRITE(NOUT) DATE(1),DATE(2),DATE(3), 
     &                 TIME(1),TIME(2),TIME(3)           
        ENDIF 

        IF(NPLAN.LE.1) THEN
          WRITE(NOUT) NELEM_P(I), NPOIN_P(I), NDP, NDUM
        ELSE
          WRITE(NOUT) NELEM_P(I)*(NPLAN-1),
     &          NPOIN_P(I)*NPLAN, NDP, NDUM
        ENDIF
!     
        DO J=1,NELEM_P(I)
           EF=ELELG(J,I)
           DO K=1,3
             IKLES_P((J-1)*3+K) = KNOGL(IKLES((EF-1)*3+K),I)
           ENDDO
        ENDDO
        IF(NPLAN.GT.1) THEN
           DO K = 1,NPLAN-1
              DO J = 1,NELEM_P(I)       
                IKLES3D_P(1,J,K) = IKLES_P(1+(J-1)*3) + (K-1)*NPOIN_P(I)
                IKLES3D_P(2,J,K) = IKLES_P(2+(J-1)*3) + (K-1)*NPOIN_P(I)
                IKLES3D_P(3,J,K) = IKLES_P(3+(J-1)*3) + (K-1)*NPOIN_P(I)
                IKLES3D_P(4,J,K) = IKLES_P(1+(J-1)*3) +  K   *NPOIN_P(I)
                IKLES3D_P(5,J,K) = IKLES_P(2+(J-1)*3) +  K   *NPOIN_P(I)
                IKLES3D_P(6,J,K) = IKLES_P(3+(J-1)*3) +  K   *NPOIN_P(I)
              ENDDO
           ENDDO
        ENDIF
!
        IF(NPLAN.EQ.0) THEN
          WRITE(NOUT) ((IKLES_P((J-1)*3+K),K=1,3),J=1,NELEM_P(I))
        ELSE           
          WRITE(NOUT)
     &         (((IKLES3D_P(L,J,K),L=1,6),J=1,NELEM_P(I)),K=1,NPLAN-1)
        ENDIF
!     
! INSTEAD OF IRAND, KNOLG IS WRITTEN !!!
! I.E. THE TABLE PROCESSOR-LOCAL -> PROCESSOR-GLOBAL NODE NUMBERS
!
        IF(NPLAN.EQ.0) THEN
          WRITE(NOUT) (KNOLG(J,I), J=1,NPOIN_P(I))
        ELSE
!         BEYOND NPOIN_P(I) : DUMMY VALUES IN KNOLG, NEVER USED
          WRITE(NOUT) (KNOLG(J,I), J=1,NPOIN_P(I)*NPLAN)
        ENDIF
!
! NODE COORDINATES X AND Y
!
        IF(NPLAN.EQ.0) THEN
          IF(SERAFIND) THEN
            WRITE(NOUT) (F_D(KNOLG(J,I),1),J=1,NPOIN_P(I))  
            WRITE(NOUT) (F_D(KNOLG(J,I),2),J=1,NPOIN_P(I)) 
          ELSE
            WRITE(NOUT) (F(KNOLG(J,I),1),J=1,NPOIN_P(I))  
            WRITE(NOUT) (F(KNOLG(J,I),2),J=1,NPOIN_P(I))
          ENDIF
        ELSE 
          IF(SERAFIND) THEN
            WRITE(NOUT) ((F_D(KNOLG(J,I)+(L-1)*NPOIN2,1),
     &                      J=1,NPOIN_P(I)), L=1,NPLAN)  
            WRITE(NOUT) ((F_D(KNOLG(J,I)+(L-1)*NPOIN2,2),
     &                      J=1,NPOIN_P(I)), L=1,NPLAN) 
          ELSE                     
            WRITE(NOUT) ((F(KNOLG(J,I)+(L-1)*NPOIN2,1),J=1,NPOIN_P(I)), 
     &                                                      L=1,NPLAN)  
            WRITE(NOUT) ((F(KNOLG(J,I)+(L-1)*NPOIN2,2),J=1,NPOIN_P(I)), 
     &                                                      L=1,NPLAN)
          ENDIF  
        ENDIF
!
! TIME STAMP (SECONDS) 
!
!!
!!   -------------------------------------------------------------------
!!   MODIFICATION TO PUT ALL THE RECORDINGS IN PARALLEL 
!!   GEO FILE 08/06/2011
!!   -------------------------------------------------------------------
!!     FIRST STEP : CLOSE/REOPEN/REWIND THE FILE AND READ ALL THE RECORDINGS UNTIL
!!     THOSE CONCERNING THE TIME-DEPENDENT  VARIABLES
!!
        CLOSE(NINP)
        OPEN(NINP,FILE=NAMEINP,STATUS='OLD',FORM='UNFORMATTED')
        REWIND(NINP)
        READ (NINP) TITLE
        READ (NINP) II, JJ
        NVAR = II + JJ 
        DO II=1,NVAR
          READ(NINP) VARI
        ENDDO 
        READ (NINP) (II, JJ=1,10)
        IF(II.EQ.1) THEN 
          READ(NINP) DATE_TMP(1), DATE_TMP(2), DATE_TMP(3), 
     &               TIME_TMP(1), TIME_TMP(2), TIME_TMP(3)
        ENDIF 
        READ(NINP) II,II,II,II
        READ(NINP) ((II,JJ=1,NDP),K=1,NELEM)
        READ(NINP) (II,JJ=1,NPOIN)
        IF(SERAFIND) THEN
          READ(NINP) (TMP_D,JJ=1,NPOIN)
          READ(NINP) (TMP_D,JJ=1,NPOIN)
        ELSE
          READ(NINP) (TMP,JJ=1,NPOIN)
          READ(NINP) (TMP,JJ=1,NPOIN)
        ENDIF
!
!       SECOND STEP 
!       EACH RECORDING IS READ AND ONLY THE LOCAL VARIABLES ARE STORED 
!       INTO THE PARALLEL GEO FILE
! 
!       INFINITE LOOP THAT ENDS WITH END=1111 IN READ STATEMENT
!       
        DO 
          IF(SERAFIND) THEN
            READ(NINP, END=1111, ERR=300) TIMES_D
            WRITE(NOUT) TIMES_D
          ELSE
            READ(NINP, END=1111, ERR=300) TIMES
            WRITE(NOUT) TIMES
          ENDIF
          DO K=3,NVAR+2
            IF(SERAFIND) THEN
              READ(NINP, END=302, ERR=302) (F_D(J,3), J=1,NPOIN)
            ELSE
              READ(NINP, END=302, ERR=302) (F(J,3), J=1,NPOIN)
            ENDIF
            IF(NPLAN.EQ.0) THEN
              IF(SERAFIND) THEN
                WRITE(NOUT) (F_D(KNOLG(J,I),3),J=1,NPOIN_P(I))
              ELSE
                WRITE(NOUT) (F(KNOLG(J,I),3),J=1,NPOIN_P(I))
              ENDIF
            ELSE
              IF(SERAFIND) THEN
                WRITE(NOUT) ((F_D(KNOLG(J,I)+(L-1)*NPOIN2,3),
     &                        J=1,NPOIN_P(I)),L=1,NPLAN)
              ELSE
                WRITE(NOUT) ((F(KNOLG(J,I)+(L-1)*NPOIN2,3),
     &                        J=1,NPOIN_P(I)),L=1,NPLAN)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
1111    CONTINUE   
        CLOSE (NINP)
        CLOSE (NOUT)    
      ENDDO
CD   -------------------------------------------------------------------
CD   END OF THE MODIFICATION TO PUT ALL THE
CD   RECORDINGS IN PARALLEL GEO FILE 08/06/2011
CD   -------------------------------------------------------------------
! //// JAJ: LA FINITA COMMEDIA FOR PARALLEL CHARACTERISTICS, BYE! 
!----------------------------------------------------------------------
! !JAJ #### DEAL WITH SECTIONS 
!
      IF (NPLAN.NE.0) WITH_SECTIONS=.FALSE.
      IF (WITH_SECTIONS) THEN ! PRESENTLY, FOR TELEMAC2D, EV. SISYPHE 
!
      WRITE(LU,*) 'DEALING WITH SECTIONS WITH FILE ',TRIM(NAMESEC)
      OPEN (NSCT,FILE=TRIM(NAMESEC),FORM='FORMATTED',STATUS='OLD') 
      READ (NSCT,*) ! COMMENT LINE
      READ (NSCT,*) NSEC, IHOWSEC
      IF (.NOT.ALLOCATED(CHAIN)) ALLOCATE (CHAIN(NSEC))
      IF (IHOWSEC<0) THEN 
        DO ISEC=1,NSEC
          READ (NSCT,*) CHAIN(ISEC)%DESCR
          READ (NSCT,*) CHAIN(ISEC)%NPAIR(:)
          CHAIN(ISEC)%XYBEG(1)=F(CHAIN(ISEC)%NPAIR(1),1)
          CHAIN(ISEC)%XYBEG(2)=F(CHAIN(ISEC)%NPAIR(1),2)
          CHAIN(ISEC)%XYEND(1)=F(CHAIN(ISEC)%NPAIR(2),1)
          CHAIN(ISEC)%XYEND(2)=F(CHAIN(ISEC)%NPAIR(2),2)
          WRITE(LU,*) 'SECTION ',CHAIN(ISEC)%DESCR
          WRITE(LU,*) 'BEGINS AT X=',CHAIN(ISEC)%XYBEG(1),
     &                         ' Y=',CHAIN(ISEC)%XYBEG(2)
          WRITE(LU,*) 'ENDS   AT X=',CHAIN(ISEC)%XYEND(1),
     &                         ' Y=',CHAIN(ISEC)%XYEND(2)
        ENDDO 
      ELSE
        DO ISEC=1,NSEC
          READ (NSCT,*) CHAIN(ISEC)%DESCR
          READ (NSCT,*) CHAIN(ISEC)%XYBEG(:), CHAIN(ISEC)%XYEND(:)
          CHAIN(ISEC)%NPAIR(:)=0
        ENDDO 
      ENDIF
      CLOSE(NSCT)
! 
!     IF TERMINAL POINTS GIVEN BY COORDINATES, FIND NEAREST NODES FIRST
!
      WRITE(LU,*) 'NPOIN:',NPOIN
      IF(IHOWSEC.GE.0) THEN 
        DO ISEC=1,NSEC
          XA=F(1,1) 
          YA=F(1,2)
          DMINB = (CHAIN(ISEC)%XYBEG(1)-XA)**2 
     &          + (CHAIN(ISEC)%XYBEG(2)-YA)**2 
          DMINE = (CHAIN(ISEC)%XYEND(1)-XA)**2 
     &          + (CHAIN(ISEC)%XYEND(2)-YA)**2 
          CHAIN(ISEC)%NPAIR(1)=1
          CHAIN(ISEC)%NPAIR(2)=1
          DO I=2,NPOIN ! COMPUTATIONALLY INTENSIVE 
            XA=F(I,1)
            YA=F(I,2)
            DISTB = (CHAIN(ISEC)%XYBEG(1)-XA)**2 
     &            + (CHAIN(ISEC)%XYBEG(2)-YA)**2 
            DISTE = (CHAIN(ISEC)%XYEND(1)-XA)**2 
     &            + (CHAIN(ISEC)%XYEND(2)-YA)**2 
            IF ( DISTB < DMINB ) THEN 
              CHAIN(ISEC)%NPAIR(1)=I
              DMINB=DISTB
            ENDIF
            IF ( DISTE < DMINE ) THEN 
              CHAIN(ISEC)%NPAIR(2)=I
              DMINE=DISTE 
            ENDIF 
          END DO
          WRITE(LU,'(A,3(1X,I9))') 
     &          ' -> SECTION, TERMINAL NODES: ', 
     &          ISEC, CHAIN(ISEC)%NPAIR(:)
        ENDDO  
      ELSE
        DO ISEC=1,NSEC
          WRITE(LU,'(A,1X,I9,4(1X,1PG13.6))') 
     &          ' -> SECTION, TERMINAL COORDINATES: ', ISEC, 
     &          CHAIN(ISEC)%XYBEG, CHAIN(ISEC)%XYEND
        ENDDO 
      ENDIF 
!
      WRITE(LU,*) 'NSEC,IHOWSEC: ',NSEC,IHOWSEC
      WRITE(LU,*) 'ANTICIPATED SECTIONS SUMMARY:'
      DO ISEC=1,NSEC
        WRITE(LU,*) CHAIN(ISEC)%DESCR
        WRITE(LU,*) CHAIN(ISEC)%XYBEG(:), CHAIN(ISEC)%XYEND(:)
        WRITE(LU,*) CHAIN(ISEC)%NPAIR(:)
      ENDDO  
!
!     NOW FOLLOW THE FLUSEC SUBROUTINE IN BIEF TO FIND SECTIONS 
!     IN THE GLOBAL MESH -> FILL THE FIELD LISTE
!
      ALLOCATE(LISTE(NSEMAX,2),STAT=ERR) ! WORKHORSE 
      CALL CHECK_ALLOCATE(ERR, 'LISTE')
!
      DO ISEC =1,NSEC
!
        DEP = CHAIN(ISEC)%NPAIR(1) 
        ARR = CHAIN(ISEC)%NPAIR(2)
!
        PT = DEP
        ISEG = 0
        DIST=(F(DEP,1)-F(ARR,1))**2+(F(DEP,2)-F(ARR,2))**2
!
 1010   CONTINUE ! A JUMP POINT 
!
        DO IELEM =1,NELEM
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
          IF (PT.EQ.I1.OR.PT.EQ.I2.OR.PT.EQ.I3) THEN
            DIST1 = (F(I1,1)-F(ARR,1))**2 + (F(I1,2)-F(ARR,2))**2
            DIST2 = (F(I2,1)-F(ARR,1))**2 + (F(I2,2)-F(ARR,2))**2
            DIST3 = (F(I3,1)-F(ARR,1))**2 + (F(I3,2)-F(ARR,2))**2
            IF (DIST1.LT.DIST) THEN
              DIST = DIST1
              ELBEST = IELEM
              IGBEST = I1
              ILBEST = 1
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF (DIST2.LT.DIST) THEN 
              DIST = DIST2
              ELBEST = IELEM
              IGBEST = I2
              ILBEST = 2
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF(DIST3.LT.DIST) THEN
              DIST = DIST3
              ELBEST = IELEM
              IGBEST = I3
              ILBEST = 3
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
          ENDIF
!
        END DO ! OVER ELEMENTS 
!
        IF (IGBEST.EQ.PT) THEN
          WRITE(LU,*)'FLUSEC : ALGORITHM FAILED'
          CALL PLANTE(1)
          STOP
        ELSE
          PT = IGBEST
          ISEG = ISEG + 1
          IF (ISEG.GT.NSEMAX) THEN
            WRITE(LU,*) 'TOO MANY SEGMENTS IN A   '
            WRITE(LU,*) 'SECTION. INCREASE  NSEMAX'
            CALL PLANTE(1)
            STOP
          ENDIF
          LISTE(ISEG,1) = IKLE(ELBEST,ILPREC)
          LISTE(ISEG,2) = IKLE(ELBEST,ILBEST)
          IF (IGBEST.NE.ARR) GOTO 1010
        ENDIF
        CHAIN(ISEC)%NSEG = ISEG
        ALLOCATE (CHAIN(ISEC)%LISTE(CHAIN(ISEC)%NSEG,3), STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'CHAIN(ISEC)%LISTE') 
        DO ISEG=1,CHAIN(ISEC)%NSEG
          CHAIN(ISEC)%LISTE(ISEG,1)=LISTE(ISEG,1) 
          CHAIN(ISEC)%LISTE(ISEG,2)=LISTE(ISEG,2) 
          CHAIN(ISEC)%LISTE(ISEG,3)=-1 ! INITIALISE... FOR DEVEL 
        END DO 
      ENDDO ! OVER SECTIONS 
      DEALLOCATE (LISTE) 
!
      ALLOCATE (ANPBEG(NBMAXNSHARE), STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'ANPBEG') 
      ALLOCATE (ANPEND(NBMAXNSHARE), STAT=ERR) 
      CALL CHECK_ALLOCATE(ERR, 'ANPEND') 
!
      DO ISEC=1,NSEC 
        DO ISEG=1,CHAIN(ISEC)%NSEG 
!
          NPBEG=COUNT( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),:)>0 )
          NPEND=COUNT( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),:)>0 )          
!
          IF (NPBEG>NBMAXNSHARE .OR. NPEND>NBMAXNSHARE) THEN 
            WRITE(LU,*) 'NPBEG OR NPEND: ',NPBEG,NPEND
            WRITE(LU,*) 'ARE LARGER THAN NBMAXNSHARE: ',NBMAXNSHARE
            CALL PLANTE(1) 
            STOP
          ENDIF 
!
          ! THE NICE AND USUAL CASE WHEN BOTH SEGMENT ENDS 
          ! BELONG TO ONE SUBDOMAIN - ONLY ONE POSITION IN KNOGL 
          IF ( NPBEG==1 .AND. NPEND==1) THEN  
             IM(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),:) ) 
             IN(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),:) )
             IF (IM(1)==IN(1)) THEN  
               CHAIN(ISEC)%LISTE(ISEG,3)=IM(1) 
             ELSE ! THEY BELONG TO DIFFERENT SUBDOMAINS? HOW COME?
               WRITE(LU,*) 'IMPOSSIBLE CASE (1) BY SECTIONS'
               CALL PLANTE(1)
               STOP
             ENDIF 
          ! AT LEAST ONE OF THE TERMINAL NODES IS ON THE INTERFACE
          ! TAKE THE LARGEST COMMON PARTITION NUMBER THEY BOTH BELONG TO
          ELSE 
            IF (NPBEG==1 .AND. NPEND>1) THEN ! THE SEGMENT'S END TOUCHES THE INTERFACE 
              IM(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),:) )
              IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),IM(1))>0 ) THEN  
                CHAIN(ISEC)%LISTE(ISEG,3) = IM(1) 
              ELSE 
                WRITE(LU,*) 'IMPOSSIBLE CASE (2) BY SECTIONS'
                CALL PLANTE(1)
                STOP
              ENDIF 
            ELSE IF (NPBEG>1 .AND. NPEND==1) THEN ! THE SEGMENT'S BEG. TOUCHES THE INTERFACE
              IN(:) = MAXLOC ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),:) )
              IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),IN(1))>0 ) THEN  
                CHAIN(ISEC)%LISTE(ISEG,3) = IN(1) 
              ELSE 
                WRITE(LU,*) 'IMPOSSIBLE CASE (3) BY SECTIONS'
                CALL PLANTE(1)
                STOP
              ENDIF 
            ELSE ! I.E. (NPBEG>1 .AND. NPEND>1) - LIES JUST ON THE INTERFACE OR "A SHORTCUT" 
              ANPBEG=0
              ANPEND=0 
              I=0 
              DO N=1,NPARTS
                IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),N)>0 ) THEN
                  I=I+1
                  ANPBEG(I)=N 
                ENDIF  
              END DO 
              IF (I/=NPBEG) WRITE(LU,*) 'OH! I/=NPBEG'
              I=0 
              DO N=1,NPARTS
                IF ( KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),N)>0 ) THEN
                  I=I+1
                  ANPEND(I)=N 
                ENDIF  
              END DO 
              IF (I/=NPEND) WRITE(LU,*) 'OH! I/=NPEND'
!
              WRITE(LU,*) 'ANPBEG: ',ANPBEG
              WRITE(LU,*) 'ANPEND: ',ANPEND
!
              FOUND=.FALSE.
              DO I=NPBEG,1,-1
                DO J=NPEND,1,-1
                  IF (ANPBEG(I)==ANPEND(J)) THEN 
                     CHAIN(ISEC)%LISTE(ISEG,3) = ANPBEG(I)
                    FOUND=.TRUE.
                    EXIT
                  ENDIF 
                END DO 
                IF (FOUND) EXIT 
              END DO 
              IF (.NOT.FOUND) THEN 
                WRITE(LU,*) 'BY SECTION WITH NODES: ',
     &            CHAIN(ISEC)%LISTE(ISEG,1),CHAIN(ISEC)%LISTE(ISEG,2)
                WRITE(LU,*) 'IMPOSSIBLE CASE (4) BY SECTIONS'
                CALL PLANTE(1)
                STOP
              ENDIF 

            ENDIF
          ENDIF 
        ENDDO 
      ENDDO 
!
      DEALLOCATE (ANPBEG,ANPEND) 
!
! WRITE FILES 
!
      DO N=1,NPARTS
        NAMEOUT=TRIM(NAMESEC)//EXTENS(NPARTS-1,N-1)

        WRITE(LU,*) 'WRITING: ', TRIM(NAMEOUT) 

        OPEN (NOUT,FILE=TRIM(NAMEOUT),FORM='FORMATTED',STATUS='UNKNOWN')
        REWIND(NOUT) 
        WRITE(NOUT,*) '# SECTIONS PARTITIONED FOR ',
     &   EXTENS(NPARTS-1,N-1)
        WRITE(NOUT,*) NSEC, 1
        DO ISEC=1,NSEC
          WRITE(NOUT,*) TRIM(CHAIN(ISEC)%DESCR)
          I=COUNT(CHAIN(ISEC)%LISTE(:,3)==N) 
          WRITE(NOUT,*) I
          DO ISEG=1,CHAIN(ISEC)%NSEG
            IF (CHAIN(ISEC)%LISTE(ISEG,3)==N) THEN 
              WRITE(NOUT,*) 
     &          KNOGL(CHAIN(ISEC)%LISTE(ISEG,1),N),
     &          KNOGL(CHAIN(ISEC)%LISTE(ISEG,2),N)
            ENDIF
          END DO 
        END DO
        CLOSE(NOUT) 
      END DO 
!
      WRITE(LU,*) 'FINISHED DEALING WITH SECTIONS'
      ENDIF ! NPLAN==0

! !YA #### DEAL WITH ZONES 
!
!----------------------------------------------------------------------
!
      ! TODO: Handle 3d 
      IF (NPLAN.NE.0) WITH_ZONES = .FALSE.
      IF(WITH_ZONES) THEN 
!
      WRITE(LU,*) 'DEALING WITH ZONES WITH FILE ',TRIM(NAMEZFI)
!     Reading the friction informations
      OPEN(NZFI,FILE=TRIM(NAMEZFI),FORM='FORMATTED',STATUS='OLD') 
      ALLOCATE (FRICTION(NPOIN), STAT=ERR) 
      CALL CHECK_ALLOCATE(ERR, 'FRICTION') 
      FRICTION(:) = 0
      DO J=1,NPOIN
        READ(NZFI,*,END=304,ERR=303) I, VAL_ZFI
        FRICTION(I) = VAL_ZFI 
      ENDDO
      CLOSE(NZFI)

      DO N=1,NPARTS
        NAMEOUT=TRIM(NAMEZFI)//EXTENS(NPARTS-1,N-1)

        WRITE(LU,*) 'WRITING: ', TRIM(NAMEOUT) 

        OPEN (NOUT,FILE=TRIM(NAMEOUT),FORM='FORMATTED',STATUS='NEW')
        DO I=1,NPOIN_P(N)
          I_GLOB = KNOLG(I,N)
          WRITE(NOUT,*) I, FRICTION(I_GLOB)
        END DO
        CLOSE(NOUT) 
      END DO 
      DEALLOCATE(FRICTION)

      WRITE(LU,*) 'FINISHED DEALING WITH ZONES'
      ENDIF ! WITH_ZONES
!
!----------------------------------------------------------------------
!
!     NOTE BY J-M HERVOUET : DEALLOCATE CAUSES ERRORS ON HP
!     (POSSIBLE REMAINING BUG ?)
!     NOTE BY JAJ: DEALLOCATE(HP) ,^)
!
!     NOTE JMH: AS THE PROGRAM ENDS, IS DEALLOCATING USEFUL ?????
!
      DEALLOCATE (IKLE) ! #### MOVED FROM FAR ABOVE 
      DEALLOCATE(NPART)
      DEALLOCATE(EPART)
      DEALLOCATE(NPOIN_P)
      DEALLOCATE(NELEM_P)
      DEALLOCATE(NPTFR_P)
      DEALLOCATE(NPTIR_P)
!
      DEALLOCATE(IKLES)
      IF(NPLAN.GT.1) THEN
        DEALLOCATE(IKLES3D)
        DEALLOCATE(IKLES3D_P)
      ENDIF
      DEALLOCATE(IKLES_P)
      DEALLOCATE(IRAND)
      DEALLOCATE(F)
!
      DEALLOCATE(KNOLG)
      DEALLOCATE(KNOGL)
      DEALLOCATE(ELELG)
      DEALLOCATE(KP1BOR)
!
!----------------------------------------------------------------------      
!
 299  IF (TIMECOUNT) THEN 
        CALL SYSTEM_CLOCK (COUNT=TEMPS, COUNT_RATE=PARSEC)
        TFIN = TEMPS
        WRITE(LU,*) 'OVERALL TIMING: ',
     &    (1.0*(TFIN-TDEB))/(1.0*PARSEC),' SECONDS'
        WRITE(LU,*) ' '
      ENDIF
!
      WRITE(LU,*) '+---- PARTEL: NORMAL TERMINATION ----+'
      WRITE(LU,*) ' '
!
      GO TO 999
!
 290  WRITE(LU,*) 'ERROR WHEN READING THE TITLE'
      GO TO 998
 297  WRITE(LU,*) 'ERROR WHEN READING X AND Y'
      GO TO 998
 298  WRITE(LU,*) 'ERROR WHEN READING TIME IN FIRST LOOP'
      GO TO 998
 300  WRITE(LU,*) 'ERROR WHEN READING TIME IN SECOND LOOP'
      GO TO 998
 301  WRITE(LU,*) 'ERROR WHEN READING VARIABLES IN FIRST LOOP'
      GO TO 998
 302  WRITE(LU,*) 'ERROR WHEN READING VARIABLES IN SECOND LOOP'
      GO TO 998
! Error handling for zones
 303  WRITE(LU,*) 'ERROR WITH ZONE FILE FORMAT'
      GO TO 999
 304  WRITE(LU,*) 'ABNORMAL END OF FILE'
      GO TO 999
      
!
!     CASE WITH ERROR
!
 998  CONTINUE
      WRITE(LU,*) 'TITLE WAS ',TITLE
      WRITE(LU,*) 'SERAFIND WAS ',SERAFIND
      WRITE(LU,*) 'NPOIN WAS ',NPOIN
      IF(SERAFIND) THEN
        WRITE(LU,*) 'LAST TIME READ WAS ',TIMES_D,' ILOOP=',ILOOP
      ELSE
        WRITE(LU,*) 'LAST TIME READ WAS ',TIMES,' ILOOP=',ILOOP
      ENDIF  
      CALL PLANTE(1)
!
 999  STOP  
!
!----------------------------------------------------------------------      
!
      END PROGRAM PARTEL
