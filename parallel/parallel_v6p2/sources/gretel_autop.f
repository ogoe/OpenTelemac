!                    **************
                     PROGRAM GRETEL_AUTOP
!                    **************
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION
!+                TO WRITE A SINGLE FILE IN SELAFIN FORMAT.
!
!note     JMH 28/04/2009: THERE IS A STRANGE TREATMENT HERE OF
!+                            NPLAN=0 WHICH SEEMS TO CORRESPOND TO 2D
!+                            WHEREAS NPLAN.NE.0 WOULD BE 3D. IT CORRESPONDS
!+                            TO A SPECIAL PROGRAMMING OF ECRGEO AND
!+                            WRITE_MESH_SERAFIN. THIS IS TO BE CHANGED.
!
!bug      JMH,08/08/2007 : THERE IS A CALL EXIT(ICODE) WHICH IS A
!+        FORTRAN EXTENSION. IT WILL NOT WORK WITH SOME COMPILERS,
!+        LIKE NAG
!
!history  JAJ
!+        2001/2
!+
!+   SLIGHTLY CHANGED TO DEAL WITH:
!
!history  HW, BAW-HAMBURG
!+        20/02/2003
!+
!+   IMPROVED READING OF DATASETS
!
!history  JAJ
!+        14/03/2003
!+
!+   ADDED EXIT CODES
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
!
      CHARACTER(LEN=30) GEO
!
!-------------------------------------------------------------------------
!
      LI=5
      LU=6
      LNG=2
!HW
!JAJ INTRODUCE YOURSELF WITH THE VERSION DATE
!
      WRITE(LU,*) 'I AM GRETEL FROM BAW HAMBURG'
      WRITE(LU,*) 'REINCARNATED BY HOLGER WEILBEER'
      WRITE(LU,*) 'ON 20TH FEBRUARY 2003'
      WRITE(LU,*)
        !!!!FABS: DETERMINES WHICH CONFIGURATION IT IS:
        !!!!      ** IF THE 1ST NAME IS T2DGEO, THEN ===> TELEMAC-2D DOMAIN DECOMPOSITION
        !!!!      OR
        !!!!      ** IF THE 1ST NAME IS E2DSERA OR E2DVOL OR E2DSCAL, THEN
        !!!!      ===> ESTEL-2D PARTICULATE DECOMPOSITION
        !!!!FABS: SHOULD PERFORM SPEED TESTS TO INVESTIGATE THE DIFFERENCE
        !!!!    : BETWEEN SCALAR AND PARALLEL MODES... SCALAR MODE APPEARS
        !!!!    : TO BE QUICKER AND TAKE LESS SPACE.
        !!!!FABS: THE PARTICULATE DECOMPOSITION DOES NOT MODIFY THE SERAPHIN,
        !!!!FABS: IT IS THEREFORE BENEFICIAL TO DECLARE IN SCALAR RATHER THAN
        !!!!FABS: IN SELAFIN: SAVES MEMORY AND TIME WHEN MERGING THE RESULTS.
!
!
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
!      REWIND(LI)
      READ(LI,*) GEO
 !       WRITE(LU,*) GEO
!
      IF ((GEO.EQ.'E2DSERA').OR.(GEO.EQ.'E2DVOL')
     &       .OR.(GEO.EQ.'E2DSCAL')) THEN
!
!|=======================================================================/
!|     	                                                                 /
!| START: MERGES FILES RESULTING FROM THE PARTICULATE DECOMPOSITION      /
!|                                                                       /
!| SERAFIN  = INCHANGE => MERE COPY OF ONE OF THE NPROC FILES            /
!| VOLFIN   = CHANGE => SUM OF THE RESULTS PART_INS,PART_CUM             /
!| SCALAIRE = CHANGE => SUM OF THE RESULTS NBPART_LOST...                /
!|                                                                       /
!|=======================================================================/
!
!
      CALL RECOMPOSITION_PARTICULAIRE(GEO)
!
!
!|==================================================================|
!|     	                                                            |
!| END: MERGES FILES RESULTING FROM THE PARTICULATE DECOMPOSITION   |
!|                                                                  |
!|==================================================================|
!
!
        ELSE
!
!
!|==================================================================|
!|     	                                                            |
!| START: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION      |
!|                                                                  |
!|==================================================================|
!
        CALL RECOMPOSITION_DECOMP_DOMAINE (GEO)
!
!|==================================================================|
!|     	                                                            |
!| END: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION        |
!|                                                                  |
!|==================================================================|
!
!
      ENDIF
      STOP
      END PROGRAM GRETEL_AUTOP
!
!
!         *******************************************
          SUBROUTINE RECOMPOSITION_PARTICULAIRE (GEO)
!         *******************************************
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief       MERGES THE RESULTS FROM COMPUTATION USING PARTICULATE
!+                DECOMPOSITION IN ESTEL-2D. ONLY MERGES THE FOLLOWING FILES:
!+  1/ SERAFIN (E2DSERA): NO MODIFICATION.
!+         MERE COPY OF A SINGLE FILE.
!+  2/ VOLFIN (E2DVOL): PARTICULATE VARIABLES ARE MODIFIED.
!+         THE VALUES STORED IN EACH PROCESSOR'S FILES ARE SUMMED UP.
!+  3/ SCALAR RESULTS (E2DSCAL): PARTICULATE VARIABLES ARE MODIFIED.
!+         THE VALUES ARE SUMMED UP.
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
!history  FABIEN DECUNG (STAGIAIRE MATMECA)
!+        18/07/2005
!+        V4P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| GEO            |---|
!| IKLE           |<->| TABLEAU DES CONNECTIVITES
!| NELEM          |-->| NOMBRE D'ELEMENTS
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
!=>FABS
      CHARACTER(LEN=30), INTENT(IN) :: GEO
!<=FABS
      INTEGER IPID,ERR,FU
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC
      INTEGER NPROC,NRESU,NPOINMAX
      INTEGER I_S, I_SP, I_LEN
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,IPOBO,VERIF,IPOBO3D
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA,IKLE3D
!
      !FABS------------------------------------------------------!
      INTEGER, DIMENSION(:)    , ALLOCATABLE   :: PART
!=>FABS : NAG BUG
      INTEGER, DIMENSION(:)    , ALLOCATABLE   :: PART_REP
!<=FABS
      REAL   , DIMENSION(:,:)  , ALLOCATABLE   :: GLOBAL_VALUE
      REAL   , DIMENSION(:,:)  , ALLOCATABLE   :: LOCAL_VALUE
      REAL   , DIMENSION(:)    , ALLOCATABLE   :: XORIG,YORIG
      REAL   , DIMENSION(:)    , ALLOCATABLE   :: SOMMEPART
      REAL   , DIMENSION(:,:,:), ALLOCATABLE   :: LOCAL_VALUELEM
      DOUBLE PRECISION, DIMENSION (:)    , ALLOCATABLE :: SOMMERESU
      DOUBLE PRECISION, DIMENSION (:,:,:), ALLOCATABLE :: VALUESCP
      !FABS------------------------------------------------------!
!
      !FABS---------------------!
      DOUBLE PRECISION SOMME
      !!!!FABS: BEWARE: DO NOT DECLARE AS DOUBLE PRECISION, OTHERWISE BUUUUUG!!!
      REAL AT
      INTEGER NUM_PROC
      INTEGER NBVAR,LINE,NBLINE,TEMPS
      LOGICAL IS,ENDE,ENDEOFFILE
      !FABS---------------------!
!
!      CHARACTER*32 GEO,GEOM
!=>FABS
      CHARACTER*32 GEOM
!<=FABS
!
      CHARACTER*32 RUB,RUBENS
      CHARACTER*32 TEXTLU(200)
      CHARACTER*72 TITRE
      CHARACTER*72 PTEXCL, TITLE
      CHARACTER*80 TITSEL
!
      CHARACTER*11 EXTENS
      EXTERNAL    EXTENS
      INTEGER, INTRINSIC ::  MAXVAL
!
!
!
!
        !!!!FABS: LI = NUMBER OF THE FILE WHERE FILENAMES AND NUMBER OF
        !!!!    : PROCESSORS ARE READ => GRETEL.PAR (TEMPORARY DIRECTORY)
        LI = 5
!
        !!!!FABS: REQUIRED BECAUSE IN PARTICULATE DECOMPOSITION, THE GEOMETRY FILE
        !!!!    : IS NOT DECLARED IN "SELAFIN-GEOM" BUT IN "PARAL"; IT IS THEREFORE NOT WRITTEN IN GRETEL.PAR
!
        WRITE(LU, ADVANCE='NO', FMT=
     &         '(/,''  PARTIAL RESULT SELAFIN FILE: <INPUT_NAME>: '')')
        READ(LI,*) RUBENS
!
        IF (RUBENS.EQ.' ') THEN
          WRITE (LU,'('' NO FILENAME'')')
        ELSE
          WRITE(LU,*) 'INPUT: ',RUBENS
        END IF
!
        WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
        READ (LI,*) NPROC
        WRITE (LU,*) NPROC
!
        !!!!FABS: NEED TO CHECK  WHETHER THE FILES ARE SERAPHIN OR VOLFIN OR SCP
        !!!!    : BECAUSE THE MERGING METHODS DEPEND ON THE TYPE OF FILES.
!
        IF ((RUBENS.EQ.'E2DSERA').OR.(RUBENS.EQ.'E2DVOL')) THEN
!
!     COMPUTATION GEOMETRY FILE, READ UNTIL THE 10 PARAMETERS:
!
!
!!!!FABS: ONLY IF GEO FILE IS DECLARED IN PARAL IN THE DICTIONARY
!FABS-----------------------------------------!
! I_S  = LEN (GEO)
!        I_SP = I_S + 1
!        DO I=1,I_S
!         IF(GEO(I_SP-I:I_SP-I) .NE. ' ') EXIT
!        ENDDO
!        I_LEN=I_SP - I
!
! GEOM=GEO(1:I_LEN) // EXTENS(NPROC-1,0)
!!!!FABS: OTHERWISE TAKE THE ROOT E2DGEO
        GEOM = GEO
!FABS-----------------------------------------!
!
      OPEN(2,FILE=GEOM,FORM='UNFORMATTED',STATUS='OLD',ERR=9990)
      READ(2,ERR=9990)
      READ(2,ERR=9990) NBV1,NBV2
      DO 110 I=1,NBV1+NBV2
        READ(2,ERR=9990)
110    CONTINUE
      GO TO 9992
9990   WRITE(LU,*) 'ERROR WHEN OPENING OR READING FILE: ',GEOM
      CALL PLANTE(-1)
      STOP
9992   CONTINUE
!     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
!
!     RESULTS FILE:
!
      OPEN(3,FILE=RUBENS,FORM='UNFORMATTED',ERR=9991)
      GO TO 9993
9991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUBENS
      CALL PLANTE(-1)
      STOP
9993   CONTINUE
!
!     1) STARTS READING THE 1ST RESULT FILE
!
!
        I_S  = LEN (RUBENS)
        I_SP = I_S + 1
        DO I=1,I_S
         IF(RUBENS(I_SP-I:I_SP-I) .NE. ' ') EXIT
        ENDDO
        I_LEN=I_SP - I
!
        RUB=RUBENS(1:I_LEN) // EXTENS(NPROC-1,0)
!
      !!!!FABS: CHECKS THAT AT LEAST THE FIRST PARALLEL TEMPORARY FILE EXISTS
      INQUIRE (FILE=RUB,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RUB
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL PLANTE(-1)
        STOP
      END IF
!
      OPEN(4,FILE=RUB,FORM='UNFORMATTED',ERR=9994)
      GO TO 9995
9994  WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUB
      CALL PLANTE(-1)
      STOP
9995   CONTINUE
!
!
!
!
!  1 : TITLE
!
      READ(4) TITRE
      WRITE(LU,*) 'TITLE=',TITRE
      TITSEL=TITRE // 'SERAPHIN'
      WRITE(3) TITSEL
!
!  2 : NBV1,NBV2
!
      READ(4) NBV1,NBV2
      WRITE(LU,*) 'NBV1=',NBV1,'   NBV2=',NBV2
      WRITE(3) NBV1,NBV2
!
!  3 : NAMES AND UNITS OF THE VARIABLES
!
!
      !!!!FABS: ALLOCATES THE PARTICULATE POINTER
!
      ALLOCATE (PART(1:2))
      PART = 0
!
      DO 5500 I=1,NBV1
        READ(4) TEXTLU(I)
        WRITE(LU,*) 'VARIABLE ',I,' : ',TEXTLU(I)
!
        !!!!FABS: IDENTIFIES THE NUMBER OF THE VARIABLES TO SUM UP
        !!!!      FOR PART_INS => PART(1)
        !!!!      FOR PART_CUM => PART(2)
!
      IF (RUBENS.EQ.'E2DVOL') THEN
        IF ( (TEXTLU(I).EQ.'PARTICULES INST. -')
     &  .OR.(TEXTLU(I).EQ.'PARTICLES INST. -') ) THEN
           PART(1) = I
        ELSE
           IF ( (TEXTLU(I).EQ.'PARTICULES CUM.  -' )
     &  .OR.(TEXTLU(I).EQ.'PARTICLES CUM.  -') ) THEN
              PART(2) = I
           ENDIF
       ENDIF
      ENDIF
      WRITE(3) TEXTLU(I)
!
5500  CONTINUE
!
!      IF ((RUBENS.EQ.'E2DVOL').AND.
!     &   ((PART(1).EQ.0).OR.(PART(2).EQ.0))) THEN
!       WRITE(LU,*) 'PAS RESULTATS PART INS OU CUM'
! WRITE(LU,*) 'VERIFIER VOS SORTIES PARTICULAIRES VOLFIN'
! CALL PLANTE(-1)
!      ENDIF
!
!  4 : 10 PARAMETERS
!
      READ(4) (PARAM(I),I=1,10)
      WRITE(LU,*) '10 PARAMETERS : ',PARAM
      PARAM(9)=0
      PARAM(8)=0
      NPLAN=PARAM(7)
      WRITE(3) (PARAM(I),I=1,10)
! READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(4)  (PARAM(I),I=1,6)
        WRITE(3) (PARAM(I),I=1,6)
      ENDIF
!
      CLOSE(4)
!
!
!  5: READS THE VARIABLES NELEM: 4 PARAMETERS
!
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',  NELEM
      WRITE(LU,*) 'NPOIN2=', NPOIN2
      WRITE(LU,*) 'ECKEN=',  ECKEN
      WRITE(LU,*) 'NDUM=',   NDUM
!
      IF(NPLAN.EQ.0) THEN
        WRITE(3) NELEM,NPOIN2,ECKEN,NDUM
      ELSE
        WRITE(3) NELEM*(NPLAN-1),NPOIN2*NPLAN,6,NDUM
      ENDIF
!
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(NPOIN(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NPOIN')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLESA')
      ALLOCATE(IPOBO(NPOIN2)      ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO')
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(VERIF(NPOIN2)    ,STAT=ERR)
      ELSE
        ALLOCATE(VERIF(NPOIN2*NPLAN)    ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'VERIF')
!  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      IF(NPLAN.EQ.0) THEN
       ! ALLOCATE(GLOBAL_VALUE(NPOIN2,NBV1)       ,STAT=ERR)
       ALLOCATE(GLOBAL_VALUE(NELEM,NBV1))
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'GLOBAL_VALUE')
!  X AND Y
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'YORIG')
!  RELATED TO A 3D CASE
      IF(NPLAN.NE.0) THEN
      ALLOCATE(IKLE3D(NELEM*(NPLAN-1),6),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLE3D')
      ALLOCATE(IPOBO3D(NPOIN2*NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO3D')
      ENDIF
!
!  END OF ALLOCATION ...
!
!  6 : IKLE
!
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      WRITE(LU,*) 'WRITING IKLE'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      ELSE
!       WRITES HERE IKLE3D (WITH INVERSION OF DIMENSIONS)
        CALL CPIKLE2(IKLE3D,IKLESA,NELEM,NELEM,NPOIN2,NPLAN)
        WRITE(3) ((IKLE3D(I,J),J=1,6),I=1,NELEM*(NPLAN-1))
      ENDIF
!
!  7 : IPOBO
!
      READ(2)  (IPOBO(I),I=1,NPOIN2)
      WRITE(LU,*) 'WRITING IPOBO'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) (IPOBO(I),I=1,NPOIN2)
      ELSE
!       DUMMY VALUES
        DO I=1,NPOIN2*NPLAN
          IPOBO3D(I) = 0
        ENDDO
        WRITE(3) (IPOBO3D(I),I=1,NPOIN2*NPLAN)
      ENDIF
!
!  8 : X AND Y, WILL BE CHECKED LATER ...
!
      READ(2)  (XORIG(I),I=1,NPOIN2)
      READ(2)  (YORIG(I),I=1,NPOIN2)
!
!
!------------------------------------------------------------------------------
!
! OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
!
      DO IPID = 0,NPROC-1
         !!!!FABS: FU IS THE NUMBER OF THE TEMPORARY RESULT FILE.
         !!!!    : CAN BE THE CAUSE OF ERRORS IN COMPUTATIONS WITH A LOT
         !!!!    : OF PROCESSORS IF 2 PROCESSORS HAVE THE SAME FU VALUE.
         FU = IPID + 10
         RUB=RUBENS(1:I_LEN) // EXTENS(NPROC-1,IPID)
         OPEN (FU,FILE=RUB,FORM='UNFORMATTED',ERR=9998)
         GO TO 9999
9998     WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUB,
     &                'USING FILE UNIT: ', FU
         CALL PLANTE(-1)
         STOP
9999      REWIND(FU)
         CALL SKIP_HEADER(FU,NPOIN(IPID+1),NBV1,ERR,LU)
         IF(ERR.NE.0) THEN
           WRITE(LU,*) 'ERROR READING FILE'
           CALL PLANTE(-1)
           STOP
         ENDIF
      END DO
!
!
      NPOINMAX = MAXVAL(NPOIN)
! ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'KNOLG')
!  LOCAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
        ALLOCATE(LOCAL_VALUE(NPOINMAX,NBV1)   ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LOCAL_VALUE')
      !!!!FABS: ALLOCATES A NEW TEMPORARY VECTOR TO STORE THE RESULTS IN
        ALLOCATE(LOCAL_VALUELEM(0:NPROC-1,1:NELEM,1:NBV1)   ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LOCAL_VALUELEM')
      !!!!FABS: ALLOCATES A TEMPORARY VECTOR TO STORE THE SUMS IN
        ALLOCATE(SOMMEPART(1:NELEM)   ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'SOMMEPART')
!
      !!!! UNTIL THIS POINT, OK FOR SERAFIN AND VOLFIN
!
! READS KNOLG(NPOIN,NPROC)
!
      !!!!FABS: IS IT USEFUL TO STORE IN THE DECOMPOSITION ?... NO!
!
      DO IPID = 0,NPROC-1
         FU = IPID + 10
         IF(NPLAN.EQ.0) THEN
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1))
         ELSE
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
         ENDIF
      END DO
!
! READS LOCAL X
!
!
!
!
        !!!!FABS: READS ALL THE TEMPORARY FILES...
        !!!!    : READS THE X-COORDINATES
        DO IPID = 0,NPROC-1
        FU = IPID +10
        READ(FU) (LOCAL_VALUE(I,1),I=1,NPOIN(IPID+1))
        ENDDO
!
!
        !!!!FABS: WITH RESPECT TO PROCESSOR 0 (+1)
        !!!!    : WRITES THE X-COORDINATES.
        WRITE(LU,*) 'WRITING X-COORDINATES'
        IF(NPLAN.EQ.0) THEN
          WRITE(3) (LOCAL_VALUE(I,1),I=1,NPOIN(1))
        ENDIF
!
        !!!!FABS: READS ALL THE TEMPORARY FILES...
        !!!!    : READS THE Y-COORDINATES
        DO IPID = 0,NPROC-1
        FU = IPID +10
        READ(FU) (LOCAL_VALUE(I,1),I=1,NPOIN(IPID+1))
        ENDDO
!
        !!!!FABS: WITH RESPECT TO PROCESSOR 0 (+1)
        !!!!    : WRITES THE Y-COORDINATES
        WRITE(LU,*) 'WRITING Y-COORDINATES'
        IF(NPLAN.EQ.0) THEN
          WRITE(3) (LOCAL_VALUE(I,1),I=1,NPOIN(1))
        ENDIF
!
! READS DATASETS
!
      NRESU = 0
!
20000 NRESU = NRESU + 1
!
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          VERIF(I)=0
        ENDDO
      ENDIF
!
      WRITE(LU,*)'TRY TO READ DATASET NO.',NRESU
!
!
      !!!!FABS: CHECKS THE FILE TYPE  :=> SERAFIN = E2DSERA
      !!!!    :                       :=> VOLFIN  = E2DVOL
      !!!!    :                       :=> SCP     = E2DSCAL
!
!
      IF (RUBENS.EQ.'E2DSERA') THEN
!
                !!! FABS: THE FILE IS A SERAFIN FILE.
                !!!     : THE VALUES ARE CONSIDERED AT THE NODES.
                !!!     : ONLY ONE PROCESSOR IS CONSIDERED BECAUSE
                !!!     : THE VALUES ARE THE SAME ON EACH PROCESSOR
!
                !!! READS THE DATA FROM THE LAST OPEN FILE
                CALL READ_DATASET
     &   (LOCAL_VALUE,NPOINMAX,NPOIN(1),NBV1,AT,FU,ENDE)
                IF (ENDE) GOTO 30000
                !!!FABS: WRITES THE RESULTS IN THE FINAL FILE
                WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',AT
                WRITE(3) AT
                DO K = 1,NBV1
                        IF(NPLAN.EQ.0) THEN
                        WRITE(3) (LOCAL_VALUE(I,K),I=1,NPOIN(1))
                        ELSE
                        !FABS: USELESS IN THIS CASE...ISN'T IT?
                        !WRITE(3) (LOCAL_VALUE(I,K),I=1,NPOIN(1)*NPLAN)
                        ENDIF
                END DO
                GO TO 20000
!
        ELSE
          IF (RUBENS.EQ.'E2DVOL') THEN
            !!! FABS:
            !!! THE FILE IS A VOLFIN FILE, PART_CUM AND PART_INS
            !!! ARE MODIFIED BY THE PARALLELISATION.
            !!! THE VALUES ARE CONSIDERED AT THE CELLS.
            !!! DEPENDING ON THE CASE, ONE OR ALL PROCESSORS ARE CONSIDERED.
!
            DO IPID = 0,NPROC-1
              !!!! FABS:
              !!!! READS THE DATA AT EACH TIMESTEP
              !!!! FOR ALL THE VARIABLES AND ALL THE PROCESSORS.
              FU = IPID +10
              CALL READ_DATASET_ELEM
     &   (LOCAL_VALUELEM,NPROC,NELEM,NBV1,AT,FU,IPID,ENDE)
              IF (ENDE) GOTO 30000
            ENDDO
            !!!FABS: WRITES IN THE FINAL RESULT FILE
            WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',AT
            WRITE(3) AT
!
            !!!!FABS: WRITES THE RESULTS IN THE FINAL RESULT FILE
            DO K = 1,NBV1
              IF (NPLAN.EQ.0) THEN
                !!! FABS:
                !!! IN THE CASE OF PARTICULATE DATA RESULTING FROM
                !!! PARALLELISATION, NEED TO SUM THEM UP ON ALL THE
                !!! PROCESSORS
                IF ( (K.NE.PART(1).AND.K.NE.PART(2)).OR.AT.EQ.0) THEN
                !!! FABS: PARAMETER K DOES NOT DEPEND ON PARALLELISATION,
                !!! CAN THEREFORE ONLY TAKE THE VALUES FROM PROCESSOR 0.
                !!!
                  WRITE(3) (LOCAL_VALUELEM(0,I,K),I=1,NELEM)
                ELSE !( (K.NE.PART(1)...
                  !!! FABS: DATA RESULTING FROM PARALLELISATION, THEY ARE
                  !!! SUMMED UP ON THE NUMBER OF PROCESSORS
                  SOMMEPART = 0.
                  DO I= 1, NELEM
                    DO NUM_PROC = 0, NPROC-1
                  SOMMEPART(I)=SOMMEPART(I)+LOCAL_VALUELEM(NUM_PROC,I,K)
                    ENDDO
                  ENDDO
!   IF (K.EQ.PART(2)) THEN
                  !!! FABS:
                  !!! THE FILE IS A CUM PARTICLES FILE, THE PARTICLES
                  !!! INITIALLY INTRODUCED, WHICH ARE NOT PART OF THE
                  !!! PARALLELISATION MUST BE SUBTRACTED.
!   WRITE(3) ( SOMMEPART(I), I=1,NELEM )
!   ELSE
                  WRITE(3) ( SOMMEPART(I), I=1,NELEM )
!   ENDIF
                ENDIF !( (K.NE.PART(1)....
              ENDIF ! (NPLAN.EQ.0)
            ENDDO !(K = 1,NBV1)
          ENDIF
        GO TO 20000
      ENDIF ! (RUBENS.EQ.'E2DSERA')
!
!
30000  WRITE(LU,*) 'END OF PROGRAM, ',NRESU-1,' DATASETS FOUND'
!
      !!!!FABS: CLOSES THE INPUT FILE (2)
      !!!!    : AND THE FINAL MERGED FILE (3)
      !!!!    : AS WELL AS THE TEMPORARY FILES ON EACH PROC.
!
      !FABS--------------!
      CLOSE(2)
      CLOSE(3)
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CLOSE (FU)
      END DO
      !FABS--------------!
!
      DEALLOCATE (PART)
      DEALLOCATE (LOCAL_VALUELEM)
!
!
      ELSE !!!! IF (RUBENS.EQ.'E2DVOL').OR.(RUBENS.EQ.'E2DSERA')
!
      !!!!FABS: THE FILE TO BE READ IS A SCALAR RESULTS FILE.
!
      OPEN(3,FILE=RUBENS,FORM='FORMATTED',ERR=99991)
      GO TO 99993
99991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUBENS
      CALL PLANTE(-1)
      STOP
99993 CONTINUE
!
!     1) STARTS READING THE 1ST RESULT FILE
!
!
        I_S  = LEN (RUBENS)
        I_SP = I_S + 1
        DO I=1,I_S
         IF(RUBENS(I_SP-I:I_SP-I) .NE. ' ') EXIT
        ENDDO
        I_LEN=I_SP - I
!
        RUB=RUBENS(1:I_LEN) // EXTENS(NPROC-1,0)
!
      INQUIRE (FILE=RUB,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RUB
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL PLANTE(-1)
        STOP
      END IF
!
! OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
!
       ALLOCATE (PART_REP(1:8))
       DO IPID = 0,NPROC-1
         FU = IPID + 10
         RUB=RUBENS(1:I_LEN) // EXTENS(NPROC-1,IPID)
         OPEN (FU,FILE=RUB,FORM='FORMATTED',ERR=99998)
         GO TO 99999
99998      WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUB,
     &                 'USING FILE UNIT: ', FU
         CALL PLANTE(-1)
         STOP
99999    REWIND(FU)
         !!!FABS: THIS ARRAY IS ALLOCATED FROM 1 TO 8 BECAUSE THERE ARE
         !!!      8 PARTICULATES VALUES TO RECONSTITUTE
         !!!!!! HOT !!!!!!
         ENDEOFFILE = .FALSE.
         NBVAR  = 0
         NBLINE = 0
         NBVAR  = 0
         DO WHILE (.NOT.ENDEOFFILE)
           READ(FU,1981,END = 9799) PTEXCL,TITLE
           IF (PTEXCL.EQ."'") THEN
             IF(IPID.EQ.0) WRITE(3,1981) "'",TITLE
             NBLINE = NBLINE + 1
             NBVAR  = NBVAR  + 1
             !!!! FABS: NEED TO IDENTIFY WHERE THE PARTICULATE VARIABLES ARE
             IF (TITLE.EQ."NBPART          -'" ) PART_REP(1)=NBVAR-3
             IF (TITLE.EQ."NBPART_OUT      -'" ) PART_REP(2)=NBVAR-3
             IF (TITLE.EQ."NBPART_NEW      -'" ) PART_REP(3)=NBVAR-3
             IF (TITLE.EQ."NBPART_LOST     -'" ) PART_REP(4)=NBVAR-3
             IF (TITLE.EQ."NBPART_AT       -'" ) PART_REP(5)=NBVAR-3
             IF (TITLE.EQ."NBPART_OUT_AT   -'" ) PART_REP(6)=NBVAR-3
             IF (TITLE.EQ."NBPART_NEW_AT   -'" ) PART_REP(7)=NBVAR-3
             IF (TITLE.EQ."NBPART_LOST_AT  -'" ) PART_REP(8)=NBVAR-3
!
           ELSE
             !!!!FABS: FOUND THE NUMBER OF VARIABLES TO COPY
             !!!!      GOES BACK TO THE TOP OF THE FILE TO WRITE THEM OUT
             !!!!      NBVAR - 3: ALWAYS 3 TITLE LINES
             !!!!      COULD JUST AS WELL USE NBLINE - 3...
             WRITE(LU,*) 'NUMBER OF VARIABLES', NBVAR-3
             IF ((PART_REP(1).NE.0.).AND.(PART_REP(8).NE.0.)) THEN
               WRITE(LU,*) 'PARTICULAR VARIABLES FOUND'
             ELSE
               WRITE(LU,*) 'PARTICULAR VARIABLES NOT FOUND'
               GO TO 9799
             ENDIF
             REWIND(FU)
             GO TO 10190
           ENDIF
         END DO
         !!!!! FABS: MODIFY? MAYBE YES, MAYBE NO
         !!!!! THERE ARE 3 TITLE LINES !!!!
         !!!!! MAYBE MODIFY TO CONSIDER FROM TIME?
         !!!!! MIGHT NOT NEED IT SINCE THE 3 LINES ARE IN THE CODE: H2D_RESSCP.F
!
         !!!! HAS TO STORE DEPENDING ON THE TIMESTEP
!
10190    TEMPS = 0
         !!!! COUNTS THE NUMBER OF TIMESTEPS
         DO WHILE (.NOT.ENDEOFFILE)
           READ(FU,*,END=6996) TITLE
           TEMPS = TEMPS + 1
         ENDDO
!
6996     TEMPS = TEMPS - NBLINE
         WRITE(LU,*) 'NUMBER OF TIME STEPS', TEMPS
!
         IF (IPID.EQ.0) THEN
         ALLOCATE (VALUESCP(1:TEMPS+1,0:NPROC-1,1:NBVAR-3))
         ALLOCATE (SOMMERESU(1:8))
         VALUESCP=0.
         ENDIF
!
         !!!!TO PASS THE TITLE LINES AND COME TO THE SCALAR VALUES
         REWIND(FU)
         DO LINE=1,NBLINE
           READ(FU,*) TITLE
         ENDDO
         !!!!READS ALL THE SCALAR VALUES BY PROCESSOR AND TIMESTEP
         TEMPS = 0
         DO WHILE (.NOT.ENDEOFFILE)
           READ(FU,*,END=6969) (VALUESCP(TEMPS+1,IPID,I),I=1,NBVAR-3)
           TEMPS = TEMPS + 1
         ENDDO
6969     CONTINUE
       ENDDO
       !!!! FABS: SHOULD NOW RECONSTITUTE THE CORRECT SUMS!!!
       !!!! FABS: HOT !!!!
       !!!! COPIES NON PARTICULATE DATA BACK FROM PROCESSOR 0
       DO LINE=1,TEMPS
         SOMMERESU = 0.
         DO IPID=0,NPROC-1
           !!!! SCALAR VARIABLES 1,3,5,7 DO NOT CHANGE
           !!!! WHEREAS MUST SUM UP VARIABLES 2,4,6,8
           DO I=1,7,2
             SOMMERESU(I) =  VALUESCP(LINE,0,PART_REP(I))
           ENDDO
           DO I=2,8,2
             SOMMERESU(I) = SOMMERESU(I)+VALUESCP(LINE,IPID,PART_REP(I))
           ENDDO
         ENDDO
         !!!! REQUIRED IF VARIABLES ARE LEFT OTHER THAN THOSE RESULTING FROM PARTICULATE, ELSE...
         IF ((PART_REP(8)+1).LE.NBVAR-3) THEN
         !!!! TEST !!!!
         WRITE(3,1010) (VALUESCP(LINE,0,I),I=1,PART_REP(1)-1),
     &   (SOMMERESU(I),I=1,8),
     &   (VALUESCP(LINE,0,I),I=PART_REP(8)+1,SIZE(VALUESCP,3))
         ELSE
         WRITE(3,1010) (VALUESCP(LINE,0,I),I=1,PART_REP(1)-1),
     &   (SOMMERESU(I),I=1,8)
         ENDIF
       ENDDO
       DO LINE=1,TEMPS
       WRITE(LU,*) 'TIME',LINE
       DO IPID=0,NPROC-1
       ENDDO
       ENDDO
       GO TO 97909
!
9799    PRINT*, 'ERROR'
        CALL PLANTE(-1)
        STOP
!
97909   PRINT*, 'DATA SETS FOUND'
        DO IPID = 0,NPROC-1
        FU = IPID +10
        CLOSE (FU)
        END DO
        CLOSE (3)
        DEALLOCATE (VALUESCP)
!
1981    FORMAT (A1,A70)
1010    FORMAT(E14.6,1X,30(E14.6,1X))
!
      END IF !!!! IF (RUBENS.EQ.'E2DVOL').OR.(RUBENS.EQ.'E2DSERA')
!
      STOP
!
      END SUBROUTINE RECOMPOSITION_PARTICULAIRE
!
!
!
         SUBROUTINE RECOMPOSITION_DECOMP_DOMAINE (GEO)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| GEO            |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
!=>FABS
      CHARACTER(LEN=30), INTENT(IN) :: GEO
!<=FABS
!
      INTEGER IPID,ERR,FU
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC
      INTEGER NPROC,NRESU,NPOINMAX
      INTEGER I_S, I_SP, I_LEN
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,IPOBO,VERIF,IPOBO3D
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA,IKLE3D
!
!
      REAL   , DIMENSION(:,:), ALLOCATABLE :: GLOBAL_VALUE
      REAL   , DIMENSION(:,:), ALLOCATABLE :: LOCAL_VALUE
      REAL   , DIMENSION(:)  , ALLOCATABLE :: XORIG,YORIG
!
      REAL AT
!
      LOGICAL IS,ENDE
!
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*72 TITRE
      CHARACTER*80 TITSEL
      CHARACTER*32 TEXTLU(200)
      CHARACTER*11 EXTENS
      EXTERNAL    EXTENS
      INTEGER, INTRINSIC ::  MAXVAL
!
!
        LI = 5
!
! READS FILE NAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
!=>FABS
!      READ(LI,*) GEO
!        WRITE(LU,*) GEO
!      WRITE (LU, ADVANCE='NO',
!     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
!<=FABS
!
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE (LU,*) ' '
!
      INQUIRE (FILE=GEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL PLANTE (-1)
        STOP
      END IF
!
      I_S  = LEN (RES)
      I_SP = I_S + 1
      DO I=1,I_S
         IF(RES(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
!
!
!     COMPUTATION GEOMETRY FILE, READ UNTIL THE 10 PARAMETERS:
!
      OPEN(2,FILE=GEO,FORM='UNFORMATTED',STATUS='OLD',ERR=990)
      READ(2,ERR=990)
      READ(2,ERR=990) NBV1,NBV2
      DO 10 I=1,NBV1+NBV2
        READ(2,ERR=990)
10    CONTINUE
      GO TO 992
990   WRITE(LU,*) 'ERROR WHEN OPENING OR READING FILE: ',GEO
      CALL PLANTE(-1)
      STOP
992   CONTINUE
!     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
!
!     RESULTS FILE:
!
      OPEN(3,FILE=RES,FORM='UNFORMATTED',ERR=991)
      GO TO 993
991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RES
      CALL PLANTE(-1)
      STOP
993   CONTINUE
!
!     1) STARTS READING THE 1ST RESULT FILE
!
!CC      RESPAR=RES // EXTENS(2**IDIMS-1,0)
!
      RESPAR=RES(1:I_LEN) // EXTENS(NPROC-1,0)
!
      INQUIRE (FILE=RESPAR,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RESPAR
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL PLANTE(-1)
        STOP
      END IF
!
      OPEN(4,FILE=RESPAR,FORM='UNFORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(-1)
      STOP
995   CONTINUE
!
!  1 : TITLE
!
      READ(4) TITRE
      WRITE(LU,*) 'TITLE=',TITRE
      TITSEL=TITRE // 'SERAFIN'
      WRITE(3) TITSEL
!
!  2 : NBV1,NBV2
!
      READ(4) NBV1,NBV2
      WRITE(LU,*) 'NBV1=',NBV1,'   NBV2=',NBV2
      WRITE(3) NBV1,NBV2
!
!  3 : NAMES AND UNITS OF THE VARIABLES
!
      DO 500 I=1,NBV1
        READ(4) TEXTLU(I)
        WRITE(LU,*) 'VARIABLE ',I,' : ',TEXTLU(I)
        WRITE(3) TEXTLU(I)
500   CONTINUE
!
!  4 : 10 PARAMETERS
!
      READ(4) (PARAM(I),I=1,10)
      WRITE(LU,*) '10 PARAMETERS : ',PARAM
      PARAM(9)=0
      PARAM(8)=0
      NPLAN=PARAM(7)
      WRITE(3) (PARAM(I),I=1,10)
! READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(4)  (PARAM(I),I=1,6)
        WRITE(3) (PARAM(I),I=1,6)
      ENDIF
      CLOSE(4)
!
!  2) READS THE GEOMETRY FILE
!
!  5 : 4 PARAMETERS
!
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',NELEM
      WRITE(LU,*) 'NPOIN2=',NPOIN2
      WRITE(LU,*) 'ECKEN=',ECKEN
      WRITE(LU,*) 'NDUM=',NDUM
!
      IF(NPLAN.EQ.0) THEN
        WRITE(3) NELEM,NPOIN2,ECKEN,NDUM
      ELSE
        WRITE(3) NELEM*(NPLAN-1),NPOIN2*NPLAN,6,NDUM
      ENDIF
!
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(NPOIN(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NPOIN')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLESA')
      ALLOCATE(IPOBO(NPOIN2)      ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO')
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(VERIF(NPOIN2)    ,STAT=ERR)
      ELSE
        ALLOCATE(VERIF(NPOIN2*NPLAN)    ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'VERIF')
!  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(GLOBAL_VALUE(NPOIN2,NBV1)       ,STAT=ERR)
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'GLOBAL_VALUE')
!  X AND Y
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'YORIG')
!  3D
      IF(NPLAN.NE.0) THEN
      ALLOCATE(IKLE3D(NELEM*(NPLAN-1),6),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLE3D')
      ALLOCATE(IPOBO3D(NPOIN2*NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO3D')
      ENDIF
!
!  END OF ALLOCATION ...
!
!  6 : IKLE
!
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      WRITE(LU,*) 'WRITING IKLE'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      ELSE
!       WRITES HERE IKLE3D (WITH INVERSION OF DIMENSIONS)
        CALL CPIKLE2(IKLE3D,IKLESA,NELEM,NELEM,NPOIN2,NPLAN)
        WRITE(3) ((IKLE3D(I,J),J=1,6),I=1,NELEM*(NPLAN-1))
      ENDIF
!
!  7 : IPOBO
!
      READ(2)  (IPOBO(I),I=1,NPOIN2)
      WRITE(LU,*) 'WRITING IPOBO'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) (IPOBO(I),I=1,NPOIN2)
      ELSE
!       DUMMY VALUES
        DO I=1,NPOIN2*NPLAN
          IPOBO3D(I) = 0
        ENDDO
        WRITE(3) (IPOBO3D(I),I=1,NPOIN2*NPLAN)
      ENDIF
!
!  8 : X AND Y, WILL BE CHECKED LATER ...
!
      READ(2)  (XORIG(I),I=1,NPOIN2)
      READ(2)  (YORIG(I),I=1,NPOIN2)
!
!------------------------------------------------------------------------------
!
! OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         RESPAR=RES(1:I_LEN) // EXTENS(NPROC-1,IPID)
         OPEN (FU,FILE=RESPAR,FORM='UNFORMATTED',ERR=998)
         GO TO 999
998      WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR,
     &                      ' USING FILE UNIT: ', FU
         CALL PLANTE(-1)
         STOP
999      REWIND(FU)
         CALL SKIP_HEADER(FU,NPOIN(IPID+1),NBV1,ERR,LU)
         IF(ERR.NE.0) THEN
           WRITE(LU,*) 'ERROR READING FILE '
           CALL PLANTE(-1)
           STOP
         ENDIF
      END DO
!
      NPOINMAX = MAXVAL(NPOIN)
! ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'KNOLG')
!  LOCAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
        ALLOCATE(LOCAL_VALUE(NPOINMAX,NBV1)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LOCAL_VALUE')
!
! READS KNOLG(NPOIN,NPROC)
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         IF(NPLAN.EQ.0) THEN
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1))
         ELSE
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
         ENDIF
      END DO
!
! READS LOCAL X
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         READ(FU) (LOCAL_VALUE(I,1),I=1,NPOIN(IPID+1))
         IF(NPLAN.EQ.0) THEN
          DO I=1,NPOIN(IPID+1)
            GLOBAL_VALUE(KNOLG(I,IPID+1),1) =
     &       LOCAL_VALUE(      I        ,1)
                   VERIF(KNOLG(I,IPID+1))   = 1
          ENDDO
         ELSE
          NPOIN2LOC = NPOIN(IPID+1)/NPLAN
          DO I=1,NPOIN2LOC
          DO J=1,NPLAN
            GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , 1)=
     &       LOCAL_VALUE(      I         + NPOIN2LOC*(J-1) , 1)
                   VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1))  = 1
          ENDDO
          ENDDO
         ENDIF
      END DO
!
! COMPARISON WITH GLOBAL VALUES
!
!     IN 3D, CHECKS THE FIRST PLANE ONLY
      DO I=1,NPOIN2
         IF(ABS(XORIG(I)-GLOBAL_VALUE(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' XORIG=',XORIG(I),
     &                ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
         ENDIF
      ENDDO
! FURTHER CHECKS
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR X-COORDINATES'
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR X-COORDINATES'
          ENDIF
        ENDDO
      ENDIF
! WRITES X
      WRITE(LU,*) 'WRITING X-COORDINATES'
      IF(NPLAN.EQ.0) THEN
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2)
      ELSE
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2*NPLAN)
      ENDIF
!
! READS LOCAL Y
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         READ(FU) (LOCAL_VALUE(I,1),I=1,NPOIN(IPID+1))
         IF(NPLAN.EQ.0) THEN
          DO I=1,NPOIN(IPID+1)
            GLOBAL_VALUE(KNOLG(I,IPID+1),1) =
     &       LOCAL_VALUE(      I        ,1)
                   VERIF(KNOLG(I,IPID+1))   = 1
          ENDDO
         ELSE
          NPOIN2LOC = NPOIN(IPID+1)/NPLAN
          DO I=1,NPOIN2LOC
          DO J=1,NPLAN
            GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , 1)=
     &       LOCAL_VALUE(      I         + NPOIN2LOC*(J-1) , 1)
                   VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1))  = 1
          ENDDO
          ENDDO
         ENDIF
      END DO
!
! COMPARISON WITH GLOBAL VALUES
!
! IN 3D, CHECKS THE FIRST PLANE ONLY
      DO I=1,NPOIN2
         IF(ABS(YORIG(I)-GLOBAL_VALUE(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' YORIG=',YORIG(I),
     &                      ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
         ENDIF
      END DO
! FURTHER CHECKS
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR Y-COORDINATES'
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR Y-COORDINATES'
          ENDIF
        ENDDO
      ENDIF
! WRITES Y
      WRITE(LU,*) 'WRITING Y-COORDINATES'
      IF(NPLAN.EQ.0) THEN
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2)
      ELSE
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2*NPLAN)
      ENDIF
!
! READS DATASETS
!
      NRESU = 0
!
2000  NRESU = NRESU + 1
!
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          VERIF(I)=0
        ENDDO
      ENDIF
!
      WRITE(LU,*)'TRY TO READ DATASET NO.',NRESU
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CALL READ_DATASET
     &   (LOCAL_VALUE,NPOINMAX,NPOIN(IPID+1),NBV1,AT,FU,ENDE)
         IF (ENDE) GOTO 3000
! STORES EACH DATASET
         IF(NPLAN.EQ.0) THEN
            DO I=1,NPOIN(IPID+1)
            DO K=1,NBV1
              GLOBAL_VALUE(KNOLG(I,IPID+1),K) = LOCAL_VALUE(I,K)
            END DO
              VERIF(KNOLG(I,IPID+1))   = 1
            END DO
         ELSE
            NPOIN2LOC = NPOIN(IPID+1)/NPLAN
            DO I=1,NPOIN2LOC
            DO J=1,NPLAN
            DO K=1,NBV1
            GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , K)=
     &       LOCAL_VALUE(      I         + NPOIN2LOC*(J-1) , K)
            END DO
                   VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1)) = 1
            END DO
            END DO
         ENDIF
      END DO
! WRITES GLOBAL DATASET
      WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',AT
!
      WRITE(3) AT
      DO K = 1,NBV1
         IF(NPLAN.EQ.0) THEN
            WRITE(3) (GLOBAL_VALUE(I,K),I=1,NPOIN2)
         ELSE
            WRITE(3) (GLOBAL_VALUE(I,K),I=1,NPOIN2*NPLAN)
         ENDIF
      END DO
! CHECKS ...
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        END DO
      ELSE
        DO I=1,NPOIN2*NPLAN
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        END DO
      ENDIF
!
      GO TO 2000
!
3000  WRITE(LU,*) 'END OF PROGRAM, ',NRESU-1,' DATASETS FOUND'
!
      CLOSE(2)
      CLOSE(3)
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CLOSE (FU)
      END DO
!
        !!!FABS
!
      END SUBROUTINE RECOMPOSITION_DECOMP_DOMAINE
!
!
!
!                       ****************************
                        CHARACTER*11 FUNCTION EXTENS
!                       ****************************
     &(N,IPID)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief       EXTENSION OF THE FILES ON EACH PROCESSOR.
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
!history  J-M HERVOUET (LNH)
!+        08/01/1997
!+        V4P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IPID           |-->| NUMERO DU PROCESSEUR
!| N              |-->| NOMBRE DE PROCESSEURS MOINS UN = NCSIZE-1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER IPID,N
!
!-----------------------------------------------------------------------
!
      IF(N.GT.0) THEN
!
        EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(EXTENS(07:11),'(I5)') IPID
        ENDIF
!
      ELSE
!
        EXTENS='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!
!        **********************
         SUBROUTINE SKIP_HEADER
!        **********************
     &(FU,NPOIN,NVALUE,ERR,LU)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| ERR            |---|
!| FU             |---|
!| LU             |---|
!| NPOIN          |---|
!| NVALUE         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NPOIN,NELEM,ECKEN,NDUM,NBV1,NVALUE,I,NPLAN
      INTEGER FU,ERR,LU
      INTEGER PARAM(10)
!
!  1 : SKIPS TITLE
!
      READ(FU,ERR=999)
!
!  2 : READS NBV1
!
      READ(FU,ERR=999) NBV1
      IF (NBV1.NE.NVALUE) THEN
        WRITE(LU,*)  'NBV1.NE.NVALUE! CHECK OUTPUT FILES ...'
        CALL PLANTE(-1)
        STOP
      ENDIF
!
!  3 : SKIPS NAMES AND UNITS OF THE VARIABLES
!
      DO I=1,NBV1
        READ(FU,ERR=999)
      END DO
!
!  4 : 10 PARAMETERS
!
      READ(FU,ERR=999) (PARAM(I),I=1,10)
      NPLAN=PARAM(7)
!  READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(FU,ERR=999)  (PARAM(I),I=1,6)
      ENDIF
!
!  5 : 4 PARAMETERS
!
      READ(FU,ERR=999) NELEM,NPOIN,ECKEN,NDUM
!
!  6 : IKLE
!
      READ(FU,ERR=999)
!
 999  RETURN
      END
!
!
!                         ***********************
                          SUBROUTINE READ_DATASET
!                         ***********************
     &(LOCAL_VALUE,NPOINMAX,NPOIN,NVALUE,AT,FU,ENDE)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| AT             |---|
!| ENDE           |---|
!| FU             |---|
!| LOCAL_VALUE    |---|
!| NPOIN          |---|
!| NPOINMAX       |---|
!| NVALUE         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NPOINMAX,NPOIN,NVALUE,FU
      INTEGER IPOIN,IVALUE
!
      REAL AT
      REAL LOCAL_VALUE(NPOINMAX,NVALUE)
!
      LOGICAL ENDE
!
      ENDE = .TRUE.
!
      READ(FU,END=999) AT
      DO IVALUE = 1,NVALUE
         READ(FU,END=999) (LOCAL_VALUE(IPOIN,IVALUE),IPOIN=1,NPOIN)
      END DO
!
      ENDE = .FALSE.
!
 999  RETURN
      END
!
!
!
!                         ****************************
                          SUBROUTINE READ_DATASET_ELEM
!                         ****************************
     &(LOCAL_VALUELEM,NPROC,NELEM,NBV1,AT,FU,IPID,ENDE)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| AT             |---|
!| ENDE           |---|
!| FU             |---|
!| IPID           |---|
!| LOCAL_VALUELEM |---|
!| NBV1           |---|
!| NELEM          |---|
!| NPROC          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NPROC,NELEM,NBV1,FU,IPID
      INTEGER IELEM,IVALUE
!
      REAL AT
      REAL LOCAL_VALUELEM(0:NPROC-1,1:NELEM,1:NBV1)
!
      LOGICAL ENDE
!
      ENDE = .TRUE.
!
      READ(FU,END=9099) AT
      DO IVALUE = 1,NBV1
         READ(FU,END=9099) (LOCAL_VALUELEM(IPID,IELEM,IVALUE)
     &   ,IELEM=1,NELEM)
      END DO
!
      ENDE = .FALSE.
!
 9099  RETURN
      END
!
!
!                       ******************
                        SUBROUTINE CPIKLE2
!                       ******************
     &(IKLE3,IKLES,NELEM2,NELMAX2,NPOIN2,NPLAN)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief       EXTENSION OF THE CONNECTIVITY TABLE.
!+                CASE OF EXTENSION TO A QUASI-BUBBLE ELEMENT.
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
!history  J-M HERVOUET (LNH)
!+        23/08/1999
!+        V5P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |<->| TABLEAU DES CONNECTIVITES
!| IKLE3          |---|
!| IKLES          |---|
!| NELEM          |-->| NOMBRE D'ELEMENTS
!| NELEM2         |---|
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NELMAX2        |---|
!| NPLAN          |---|
!| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
!| NPOIN2         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NELEM2,NELMAX2,NPOIN2,NPLAN
      INTEGER, INTENT(INOUT) :: IKLES(3,NELEM2)
      INTEGER, INTENT(INOUT) :: IKLE3(NELMAX2,NPLAN-1,6)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I
!
!-----------------------------------------------------------------------
!
!     BOTTOM AND TOP OF ALL LAYERS
!
      IF(NPLAN.GE.2) THEN
        DO I = 1,NPLAN-1
          DO IELEM = 1,NELEM2
            IKLE3(IELEM,I,1) = IKLES(1,IELEM) + (I-1)*NPOIN2
            IKLE3(IELEM,I,2) = IKLES(2,IELEM) + (I-1)*NPOIN2
            IKLE3(IELEM,I,3) = IKLES(3,IELEM) + (I-1)*NPOIN2
            IKLE3(IELEM,I,4) = IKLES(1,IELEM) +  I   *NPOIN2
            IKLE3(IELEM,I,5) = IKLES(2,IELEM) +  I   *NPOIN2
            IKLE3(IELEM,I,6) = IKLES(3,IELEM) +  I   *NPOIN2
          ENDDO
        ENDDO
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'CPIKLE2 : IL FAUT AU MOINS 2 PLANS'
        IF(LNG.EQ.2) WRITE(LU,*) 'CPIKLE2 : MINIMUM OF 2 PLANES NEEDED'
        CALL PLANTE(-1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!
!     *****************************
      SUBROUTINE ALLOER (N, CHFILE)
!     *****************************
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| CHFILE         |---|
!| N             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      CHARACTER*(*), INTENT(IN) :: CHFILE
      WRITE(N,*) 'ERROR BY ALLOCATION OF ',CHFILE
      CALL PLANTE(-1)
      STOP
      END SUBROUTINE ALLOER
!
!
!
!     ***********************
      SUBROUTINE PLANTE(IVAL)
!     ***********************
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
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
!| IVAL           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IVAL
      INTEGER ICODE
      IF (IVAL < 0) THEN ! THIS INDICATES A CONTROLLED ERROR
        ICODE = 1
      ELSE IF (IVAL==0) THEN  ! THIS INDICATES A PROGRAM FAILURE
        ICODE = -1
      ELSE                    ! THIS INDICATES A NORMAL STOP
        ICODE = 0
      ENDIF
      !!! WRITE(*,*) 'RETURNING EXIT CODE: ', ICODE
      CALL EXIT(ICODE)
      STOP    ! WHICH IS USUALLY EQUIVALENT TO CALL EXIT(0)
      END SUBROUTINE PLANTE
