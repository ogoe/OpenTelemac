!                    ********************
                     PROGRAM GRETEL_AUTOP
!                    ********************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
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
!history  J-M HERVOUET (LNHE)
!+        23/07/2012
!+        V6P2
!+   Double precision SERAFIN files taken into account. They are
!+   recognised by a title ending with SERAFIND
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
      CHARACTER(LEN=30) GEO
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
      REAL   , DIMENSION(:,:), ALLOCATABLE :: GLOBAL_VALUE
      REAL   , DIMENSION(:,:), ALLOCATABLE :: LOCAL_VALUE
      REAL   , DIMENSION(:)  , ALLOCATABLE :: XORIG,YORIG
!
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: GLOBAL_VALUE_D
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: LOCAL_VALUE_D
      DOUBLE PRECISION, DIMENSION(:)  , ALLOCATABLE :: XORIG_D,YORIG_D
!
      REAL AT
      DOUBLE PRECISION AT_D
!
      LOGICAL IS,ENDE,SERAFIND_GEO,SERAFIND_RES
!
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*80 TITSEL
      CHARACTER*32 TEXTLU(200)
      CHARACTER*11 EXTENS
      EXTERNAL  EXTENS
      INTRINSIC REAL
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
!
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
      READ(LI,*) GEO
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
      WRITE(LU,*) 'THE OPTION GEO=',GEO,' HAS BEEN REMOVED'
      STOP
!     CALL RECOMPOSITION_PARTICULAIRE(GEO)
!
!
!|==================================================================|
!|     	                                                            |
!| END: MERGES FILES RESULTING FROM THE PARTICULATE DECOMPOSITION   |
!|                                                                  |
!|==================================================================|
!
!
      ENDIF
!
!
!|==================================================================|
!|     	                                                            |
!| START: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION      |
!|                                                                  |
!|==================================================================|
!
! READS FILE NAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
      WRITE(LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE (LU,*) ' '
!
      INQUIRE (FILE=GEO,EXIST=IS)
      IF(.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL PLANTE (-1)
        STOP
      ENDIF
!
      I_S  = LEN (RES)
      I_SP = I_S + 1
      DO I=1,I_S
         IF(RES(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I
!
!     COMPUTATION GEOMETRY FILE, READ UNTIL THE 10 PARAMETERS:
!
      OPEN(2,FILE=GEO,FORM='UNFORMATTED',STATUS='OLD',ERR=990)
      READ(2,ERR=990) TITSEL
!
      SERAFIND_GEO=.FALSE.
      IF(TITSEL(73:80).EQ.'SERAFIND') SERAFIND_GEO=.TRUE. 
!
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
      RESPAR=RES(1:I_LEN) // extens(NPROC-1,0)
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
      READ(4) TITSEL
      WRITE(LU,*) 'TITLE=',TITSEL
      SERAFIND_RES=.FALSE.
      IF(TITSEL(73:80).EQ.'SERAFIND') SERAFIND_RES=.TRUE.       
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
      CALL CHECK_ALLOCATE(ERR, 'NPOIN')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLESA')
      ALLOCATE(IPOBO(NPOIN2)      ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IPOBO')
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(VERIF(NPOIN2)    ,STAT=ERR)
      ELSE
        ALLOCATE(VERIF(NPOIN2*NPLAN)    ,STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'VERIF')
!
!     GLOBAL_VALUES IN SINGLE PRECISION, STORES NBV1 VALUES
!
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(GLOBAL_VALUE(NPOIN2,NBV1)       ,STAT=ERR)
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'GLOBAL_VALUE')
!
!     GLOBAL_VALUES IN DOUBLE PRECISION, STORES NBV1 VALUES
!
      IF(SERAFIND_GEO.OR.SERAFIND_RES) THEN  
        IF(NPLAN.EQ.0) THEN
          ALLOCATE(GLOBAL_VALUE_D(NPOIN2,NBV1)       ,STAT=ERR)
        ELSE
          ALLOCATE(GLOBAL_VALUE_D(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
        ENDIF
        CALL CHECK_ALLOCATE(ERR, 'GLOBAL_VALUE_D')
      ENDIF
!
!     X AND Y SINGLE PRECISION
!
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'YORIG')
!
!     X AND Y DOUBLE PRECISION
!
      IF(SERAFIND_GEO.OR.SERAFIND_RES) THEN
        ALLOCATE(XORIG_D(NPOIN2)    ,STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'XORIG_D')
        ALLOCATE(YORIG_D(NPOIN2)    ,STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'YORIG_D')
      ENDIF
!
!  3D
!
      IF(NPLAN.NE.0) THEN
      ALLOCATE(IKLE3D(NELEM*(NPLAN-1),6),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IKLE3D')
      ALLOCATE(IPOBO3D(NPOIN2*NPLAN),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'IPOBO3D')
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
        CALL GRETEL_CPIKLE2
     &  (IKLE3D,IKLESA,NELEM,NELEM,NPOIN2,NPLAN)
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
      IF(SERAFIND_GEO) THEN
        READ(2)  (XORIG_D(I),I=1,NPOIN2)
        READ(2)  (YORIG_D(I),I=1,NPOIN2)
        DO I=1,NPOIN2
          XORIG(I)=REAL(XORIG_D(I))
          YORIG(I)=REAL(YORIG_D(I))
        ENDDO
      ELSE
        READ(2)  (XORIG(I),I=1,NPOIN2)
        READ(2)  (YORIG(I),I=1,NPOIN2)
      ENDIF
!
!------------------------------------------------------------------------------
!
! OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         RESPAR=RES(1:I_LEN) // extens(NPROC-1,IPID)
         OPEN (FU,FILE=RESPAR,FORM='UNFORMATTED',ERR=998)
         GO TO 999
998      WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR,
     &                      ' USING FILE UNIT: ', FU
         CALL PLANTE(-1)
         STOP
999      REWIND(FU)
         CALL GRETEL_SKIP_HEADER(FU,NPOIN(IPID+1),NBV1,ERR,LU)
         IF(ERR.NE.0) THEN
           WRITE(LU,*) 'ERROR READING FILE '
           CALL PLANTE(-1)
           STOP
         ENDIF
      ENDDO
!
      NPOINMAX = MAXVAL(NPOIN)
!
!     ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
!
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
      ENDIF
      CALL CHECK_ALLOCATE(ERR, 'KNOLG')
!
!     LOCAL_VALUES IN SINGLE PRECISION, STORES NBV1 VALUES
!
      ALLOCATE(LOCAL_VALUE(NPOINMAX,NBV1)    ,STAT=ERR)
      CALL CHECK_ALLOCATE(ERR, 'LOCAL_VALUE')
!
!     LOCAL_VALUES IN DOUBLE PRECISION, STORES NBV1 VALUES
!
      IF(SERAFIND_GEO.OR.SERAFIND_RES) THEN
        ALLOCATE(LOCAL_VALUE_D(NPOINMAX,NBV1)    ,STAT=ERR)
        CALL CHECK_ALLOCATE(ERR, 'LOCAL_VALUE_D')
      ENDIF
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
      ENDDO
!
! READS LOCAL X
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         IF(SERAFIND_RES) THEN
           READ(FU) (LOCAL_VALUE_D(I,1),I=1,NPOIN(IPID+1))
         ELSE
           READ(FU) (LOCAL_VALUE(I,1),I=1,NPOIN(IPID+1))
         ENDIF
         IF(NPLAN.EQ.0) THEN
           IF(SERAFIND_RES) THEN
             DO I=1,NPOIN(IPID+1)
               GLOBAL_VALUE_D(KNOLG(I,IPID+1),1)=LOCAL_VALUE_D(I,1)
               VERIF(KNOLG(I,IPID+1))   = 1
             ENDDO
           ELSE
             DO I=1,NPOIN(IPID+1)
               GLOBAL_VALUE(KNOLG(I,IPID+1),1)=LOCAL_VALUE(I,1)
               VERIF(KNOLG(I,IPID+1))   = 1
             ENDDO
           ENDIF
         ELSE
           NPOIN2LOC = NPOIN(IPID+1)/NPLAN
           IF(SERAFIND_RES) THEN
             DO I=1,NPOIN2LOC
             DO J=1,NPLAN
               GLOBAL_VALUE_D(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , 1)=
     &         LOCAL_VALUE_D(      I         + NPOIN2LOC*(J-1) , 1)
               VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1))  = 1
             ENDDO
             ENDDO
           ELSE
             DO I=1,NPOIN2LOC
             DO J=1,NPLAN
               GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , 1)=
     &         LOCAL_VALUE(      I         + NPOIN2LOC*(J-1) , 1)
               VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1))  = 1
             ENDDO
             ENDDO
          ENDIF
         ENDIF
      ENDDO
!
! COMPARISON WITH GLOBAL VALUES (ON SINGLE PRECISION VALUES)
!
!     IN 3D, CHECKS THE FIRST PLANE ONLY
!
      IF(SERAFIND_RES) THEN
        DO I=1,NPOIN2
          IF(ABS(XORIG(I)-GLOBAL_VALUE_D(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' XORIG=',XORIG(I),
     &                ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPOIN2
          IF(ABS(XORIG(I)-GLOBAL_VALUE(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' XORIG=',XORIG(I),
     &                ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
          ENDIF
        ENDDO
      ENDIF
!
! FURTHER CHECKS
!
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
!
! WRITES X
!
      WRITE(LU,*) 'WRITING X-COORDINATES'
      IF(NPLAN.EQ.0) THEN
        IF(SERAFIND_RES) THEN
          WRITE(3) (GLOBAL_VALUE_D(I,1),I=1,NPOIN2)
        ELSE
          WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2)
        ENDIF
      ELSE
        IF(SERAFIND_RES) THEN
          WRITE(3) (GLOBAL_VALUE_D(I,1),I=1,NPOIN2*NPLAN)
        ELSE
          WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2*NPLAN)
        ENDIF
      ENDIF
!
! READS LOCAL Y (EXACTLY LIKE READS LOCAL X, COULD BE A LOOP...)
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         IF(SERAFIND_RES) THEN
           READ(FU) (LOCAL_VALUE_D(I,1),I=1,NPOIN(IPID+1))
         ELSE
           READ(FU) (LOCAL_VALUE(I,1),I=1,NPOIN(IPID+1))
         ENDIF
         IF(NPLAN.EQ.0) THEN
           IF(SERAFIND_RES) THEN
             DO I=1,NPOIN(IPID+1)
               GLOBAL_VALUE_D(KNOLG(I,IPID+1),1)=LOCAL_VALUE_D(I,1)
               VERIF(KNOLG(I,IPID+1))   = 1
             ENDDO
           ELSE
             DO I=1,NPOIN(IPID+1)
               GLOBAL_VALUE(KNOLG(I,IPID+1),1)=LOCAL_VALUE(I,1)
               VERIF(KNOLG(I,IPID+1))   = 1
             ENDDO
           ENDIF
         ELSE
           NPOIN2LOC = NPOIN(IPID+1)/NPLAN
           IF(SERAFIND_RES) THEN
             DO I=1,NPOIN2LOC
             DO J=1,NPLAN
               GLOBAL_VALUE_D(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , 1)=
     &         LOCAL_VALUE_D(      I         + NPOIN2LOC*(J-1) , 1)
               VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1))  = 1
             ENDDO
             ENDDO
           ELSE
             DO I=1,NPOIN2LOC
             DO J=1,NPLAN
               GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1) , 1)=
     &         LOCAL_VALUE(      I         + NPOIN2LOC*(J-1) , 1)
               VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1))  = 1
             ENDDO
             ENDDO
          ENDIF
         ENDIF
      ENDDO
!
! COMPARISON WITH GLOBAL VALUES
!
! IN 3D, CHECKS THE FIRST PLANE ONLY
!
      IF(SERAFIND_RES) THEN
        DO I=1,NPOIN2
          IF(ABS(YORIG(I)-GLOBAL_VALUE_D(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' YORIG=',YORIG(I),
     &                      ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPOIN2
          IF(ABS(YORIG(I)-GLOBAL_VALUE(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' YORIG=',YORIG(I),
     &                      ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
          ENDIF
        ENDDO
      ENDIF
!
! FURTHER CHECKS
!
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
!
! WRITES Y
!
      WRITE(LU,*) 'WRITING Y-COORDINATES'
      IF(NPLAN.EQ.0) THEN
        IF(SERAFIND_RES) THEN
          WRITE(3) (GLOBAL_VALUE_D(I,1),I=1,NPOIN2)
        ELSE
          WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2)
        ENDIF
      ELSE
        IF(SERAFIND_RES) THEN
          WRITE(3) (GLOBAL_VALUE_D(I,1),I=1,NPOIN2*NPLAN)
        ELSE
          WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2*NPLAN)
        ENDIF
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
      WRITE(LU,*) 'TRY TO READ DATASET NO.',NRESU
!
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CALL GRETEL_READ_DATASET(LOCAL_VALUE,LOCAL_VALUE_D,
     &                            SERAFIND_RES,
     &                            NPOINMAX,NPOIN(IPID+1),
     &                            NBV1,AT,AT_D,FU,ENDE)
         IF(ENDE) GOTO 3000
!
!        STORES EACH DATASET
!
         IF(NPLAN.EQ.0) THEN
            IF(SERAFIND_RES) THEN
              DO I=1,NPOIN(IPID+1)
              DO K=1,NBV1
                GLOBAL_VALUE_D(KNOLG(I,IPID+1),K)=LOCAL_VALUE_D(I,K)
              ENDDO
              VERIF(KNOLG(I,IPID+1)) = 1
              ENDDO
            ELSE
              DO I=1,NPOIN(IPID+1)
              DO K=1,NBV1
                GLOBAL_VALUE(KNOLG(I,IPID+1),K) = LOCAL_VALUE(I,K)
              ENDDO
              VERIF(KNOLG(I,IPID+1)) = 1
              ENDDO
            ENDIF
         ELSE
            NPOIN2LOC = NPOIN(IPID+1)/NPLAN
            IF(SERAFIND_RES) THEN
              DO I=1,NPOIN2LOC
                DO J=1,NPLAN
                  DO K=1,NBV1
              GLOBAL_VALUE_D(KNOLG(I,IPID+1)+NPOIN2   *(J-1),K)=
     &         LOCAL_VALUE_D(      I        +NPOIN2LOC*(J-1),K)
                  ENDDO
              VERIF(KNOLG(I,IPID+1) + NPOIN2*(J-1)) = 1
                ENDDO
              ENDDO
            ELSE
              DO I=1,NPOIN2LOC
                DO J=1,NPLAN
                  DO K=1,NBV1
              GLOBAL_VALUE(KNOLG(I,IPID+1)+NPOIN2   *(J-1),K)=
     &         LOCAL_VALUE(      I        +NPOIN2LOC*(J-1),K)
                  ENDDO
              VERIF(KNOLG(I,IPID+1) + NPOIN2*(J-1)) = 1
                ENDDO
              ENDDO
            ENDIF
         ENDIF
      ENDDO
!
! WRITES GLOBAL DATASET
!
      WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',AT
!
!     TIME
!
      IF(SERAFIND_RES) THEN
        WRITE(3) AT_D
      ELSE
        WRITE(3) AT
      ENDIF
!
!     VARIABLES
!
      DO K = 1,NBV1
         IF(NPLAN.EQ.0) THEN
            IF(SERAFIND_RES) THEN
              WRITE(3) (GLOBAL_VALUE_D(I,K),I=1,NPOIN2)
            ELSE
              WRITE(3) (GLOBAL_VALUE(I,K),I=1,NPOIN2)
            ENDIF
         ELSE
            IF(SERAFIND_RES) THEN
              WRITE(3) (GLOBAL_VALUE_D(I,K),I=1,NPOIN2*NPLAN)
            ELSE
              WRITE(3) (GLOBAL_VALUE(I,K),I=1,NPOIN2*NPLAN)
            ENDIF
         ENDIF
      ENDDO
!
! CHECKS ...
!
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, POINT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        ENDDO
      ENDIF
!
      GO TO 2000
!
3000  WRITE(LU,*) 'END OF PROGRAM, ',NRESU-1,' DATASETS FOUND'
!
      CLOSE(2)
      CLOSE(3)
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CLOSE (FU)
      ENDDO
!
!|==================================================================|
!|     	                                                            |
!| END: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION        |
!|                                                                  |
!|==================================================================|
!
!
      STOP
      END PROGRAM GRETEL_AUTOP
