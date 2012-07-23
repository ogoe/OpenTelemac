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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
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
      CHARACTER(LEN=30), INTENT(IN) :: GEO
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
      CHARACTER*72 TITRE
      CHARACTER*80 TITSEL
      CHARACTER*32 TEXTLU(200)
      CHARACTER*11 GRETEL_EXTENS
      EXTERNAL    GRETEL_EXTENS
      INTEGER, INTRINSIC ::  MAXVAL
      INTRINSIC REAL,DBLE
!
!-----------------------------------------------------------------------
!
      LI = 5
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
        CALL GRETEL_PLANTE (-1)
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
      CALL GRETEL_PLANTE(-1)
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
      CALL GRETEL_PLANTE(-1)
      STOP
993   CONTINUE
!
!     1) STARTS READING THE 1ST RESULT FILE
!
      RESPAR=RES(1:I_LEN) // GRETEL_EXTENS(NPROC-1,0)
!
      INQUIRE (FILE=RESPAR,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RESPAR
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL GRETEL_PLANTE(-1)
        STOP
      END IF
!
      OPEN(4,FILE=RESPAR,FORM='UNFORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL GRETEL_PLANTE(-1)
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
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'NPOIN')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'IKLESA')
      ALLOCATE(IPOBO(NPOIN2)      ,STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'IPOBO')
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(VERIF(NPOIN2)    ,STAT=ERR)
      ELSE
        ALLOCATE(VERIF(NPOIN2*NPLAN)    ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'VERIF')
!
!     GLOBAL_VALUES IN SINGLE PRECISION, STORES NBV1 VALUES
!
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(GLOBAL_VALUE(NPOIN2,NBV1)       ,STAT=ERR)
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'GLOBAL_VALUE')
!
!     GLOBAL_VALUES IN DOUBLE PRECISION, STORES NBV1 VALUES
!
      IF(SERAFIND_GEO.OR.SERAFIND_RES) THEN  
        IF(NPLAN.EQ.0) THEN
          ALLOCATE(GLOBAL_VALUE_D(NPOIN2,NBV1)       ,STAT=ERR)
        ELSE
          ALLOCATE(GLOBAL_VALUE_D(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
        ENDIF
        IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'GLOBAL_VALUE_D')
      ENDIF
!
!     X AND Y SINGLE PRECISION
!
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'YORIG')
!
!     X AND Y DOUBLE PRECISION
!
      IF(SERAFIND_GEO.OR.SERAFIND_RES) THEN
        ALLOCATE(XORIG_D(NPOIN2)    ,STAT=ERR)
        IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'XORIG_D')
        ALLOCATE(YORIG_D(NPOIN2)    ,STAT=ERR)
        IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'YORIG_D')
      ENDIF
!
!  3D
!
      IF(NPLAN.NE.0) THEN
      ALLOCATE(IKLE3D(NELEM*(NPLAN-1),6),STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'IKLE3D')
      ALLOCATE(IPOBO3D(NPOIN2*NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'IPOBO3D')
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
         RESPAR=RES(1:I_LEN) // GRETEL_EXTENS(NPROC-1,IPID)
         OPEN (FU,FILE=RESPAR,FORM='UNFORMATTED',ERR=998)
         GO TO 999
998      WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR,
     &                      ' USING FILE UNIT: ', FU
         CALL GRETEL_PLANTE(-1)
         STOP
999      REWIND(FU)
         CALL GRETEL_SKIP_HEADER(FU,NPOIN(IPID+1),NBV1,ERR,LU)
         IF(ERR.NE.0) THEN
           WRITE(LU,*) 'ERROR READING FILE '
           CALL GRETEL_PLANTE(-1)
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
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'KNOLG')
!
!     LOCAL_VALUES IN SINGLE PRECISION, STORES NBV1 VALUES
!
      ALLOCATE(LOCAL_VALUE(NPOINMAX,NBV1)    ,STAT=ERR)
      IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'LOCAL_VALUE')
!
!     LOCAL_VALUES IN DOUBLE PRECISION, STORES NBV1 VALUES
!
      IF(SERAFIND_GEO.OR.SERAFIND_RES) THEN
        ALLOCATE(LOCAL_VALUE_D(NPOINMAX,NBV1)    ,STAT=ERR)
        IF(ERR.NE.0) CALL GRETEL_ALLOER (LU, 'LOCAL_VALUE_D')
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
      END SUBROUTINE RECOMPOSITION_DECOMP_DOMAINE
!                       ***********************************
                        CHARACTER*11 FUNCTION GRETEL_EXTENS
!                       ***********************************
     &(N,IPID)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief       GRETEL_EXTENSION OF THE FILES ON EACH PROCESSOR.
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
        GRETEL_EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(GRETEL_EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(GRETEL_EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(GRETEL_EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(GRETEL_EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(GRETEL_EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(GRETEL_EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(GRETEL_EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(GRETEL_EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(GRETEL_EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(GRETEL_EXTENS(07:11),'(I5)') IPID
        ENDIF
!
      ELSE
!
        GRETEL_EXTENS='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************************
                        SUBROUTINE GRETEL_SKIP_HEADER
!                       *****************************
!
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
        CALL GRETEL_PLANTE(-1)
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
!                         ******************************
                          SUBROUTINE GRETEL_READ_DATASET
!                         ******************************
!
     &(LOCAL_VALUE,LOCAL_VALUE_D,SERAFIND,
     & NPOINMAX,NPOIN,NVALUE,AT,AT_D,FU,ENDE)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
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
      LOGICAL SERAFIND
!
      REAL AT
      DOUBLE PRECISION AT_D
      REAL LOCAL_VALUE(NPOINMAX,NVALUE)
      DOUBLE PRECISION LOCAL_VALUE_D(NPOINMAX,NVALUE)
!
      LOGICAL ENDE
!
      ENDE = .TRUE.
!
      IF(SERAFIND) THEN
        READ(FU,END=999) AT_D
        DO IVALUE = 1,NVALUE
          READ(FU,END=999) (LOCAL_VALUE_D(IPOIN,IVALUE),IPOIN=1,NPOIN)
        ENDDO
      ELSE
        READ (FU,END=999) AT
        DO IVALUE = 1,NVALUE
          READ(FU,END=999) (LOCAL_VALUE(IPOIN,IVALUE),IPOIN=1,NPOIN)
        ENDDO
      ENDIF
!
      ENDE = .FALSE.
!
!-----------------------------------------------------------------------
!
 999  RETURN
      END
!
!
!
!                         *****************************************
                          SUBROUTINE GRETEL_READ_DATASET_ELEM
!                         *****************************************
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
!                       *******************************
                        SUBROUTINE GRETEL_CPIKLE2
!                       *******************************
     &(IKLE3,IKLES,NELEM2,NELMAX2,NPOIN2,NPLAN)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief       GRETEL_EXTENSION OF THE CONNECTIVITY TABLE.
!+                CASE OF GRETEL_EXTENSION TO A QUASI-BUBBLE ELEMENT.
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
        IF(LNG.EQ.1) WRITE(LU,*) 
     &   'GRETEL_CPIKLE2 : IL FAUT AU MOINS 2 PLANS'
        IF(LNG.EQ.2) WRITE(LU,*) 
     &   'GRETEL_CPIKLE2 : MINIMUM OF 2 PLANES NEEDED'
        CALL GRETEL_PLANTE(-1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!
!     ******************************************
      SUBROUTINE GRETEL_ALLOER (N, CHFILE)
!     ******************************************
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
      CALL GRETEL_PLANTE(-1)
      STOP
      END SUBROUTINE GRETEL_ALLOER
!
!
!
!     ************************************
      SUBROUTINE GRETEL_PLANTE(IVAL)
!     ************************************
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
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: IVAL
      INTEGER ICODE
!     STANDARD F90 :  STOP [n] WHERE N IS A STRING OF NOT MORE
!     THAN FIVE DIGITS OR IS A CHARACTER CONSTANT.
!     HOWEVER, CODE IS NOT ALWAYS SENT TO STDERR
!     (COMPILER DEPENDENT, NAG DOESN'T FOR INSTANCE)
!     ICODE MIGHT BE USED IN A POSSIBLE SYSTEM DEPENDENT EXIT PROCEDURE
!     EXAMPLE : STOP 1 ; STOP '    1'
      IF(IVAL.LT.0) THEN
        ICODE = 0      ! JUST ASSUMED FOR NON-ERROR STOP
      ELSEIF(IVAL.EQ.0.OR.IVAL.EQ.1) THEN
        ICODE = 2      ! EXIT IVAL 0 OR 1 INDICATING A "CONTROLLED" ERROR
        STOP 2
      ELSE
        ICODE = 1     ! SOMETHING ELSE? BUT AN ERROR!
        STOP 1
      ENDIF
      WRITE(LU,*) 'RETURNING EXIT CODE: ', ICODE
      STOP 0    !WHICH IS USUALLY EQUIVALENT TO CALL EXIT(0)

!     JMH 30/09/2011 WHAT IS THIS (NAG COMPILER DOES NOT KNOW)
!     CALL EXIT(ICODE)
      END SUBROUTINE GRETEL_PLANTE
