C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MERGES THE RESULTS OF A PARALLEL COMPUTATION
!>                TO WRITE A SINGLE FILE IN SELAFIN FORMAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     JMH 28/04/2009: THERE IS A STRANGE TREATMENT HERE OF
!>                            NPLAN=0 WHICH SEEMS TO CORRESPOND TO 2D
!>                            WHEREAS NPLAN.NE.0 WOULD BE 3D. IT CORRESPONDS
!>                            TO A SPECIAL PROGRAMMING OF ECRGEO AND
!>                            WRITE_MESH_SERAFIN. THIS IS TO BE CHANGED.

!>  @bug  JMH,08/08/2007 : THERE IS A CALL EXIT(ICODE) WHICH IS A
!>        FORTRAN EXTENSION. IT WILL NOT WORK WITH SOME COMPILERS,
!>        LIKE NAG

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> GEO, LI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE()
!>   </td></tr>
!>     </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 14/03/2003                                              </td>
!>    <td> JAJ                                                     </td>
!>    <td> ADDED EXIT CODES                                        </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 20/02/2003                                              </td>
!>    <td> HW, BAW-HAMBURG                                         </td>
!>    <td> IMPROVED READING OF DATASETS                            </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 2001/2                                                  </td>
!>    <td> JAJ                                                     </td>
!>    <td> SLIGHTLY CHANGED TO DEAL WITH:
!>         (1) ARBITRARY NUMBER OF SUB-DOMAINS
!>         (2) ARBITRARY NAMES OF THE GEOMETRY AND RESULT FILES
!>         (3) AUTOMATIC PARALLEL RUNS                             </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      PROGRAM GRETEL
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
C
C
      CHARACTER(LEN=30) GEO
C
C-------------------------------------------------------------------------
C
      LI=5
      LU=6
      LNG=2
CHW
CJAJ INTRODUCE YOURSELF WITH THE VERSION DATE
C
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
C
C
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
C      REWIND(LI)
      READ(LI,*) GEO
 !       WRITE(LU,*) GEO
C
      IF ((GEO.EQ.'E2DSERA').OR.(GEO.EQ.'E2DVOL')
     &       .OR.(GEO.EQ.'E2DSCAL')) THEN
C
C|=======================================================================/
C|     	                                                                 /
C| START: MERGES FILES RESULTING FROM THE PARTICULATE DECOMPOSITION      /
C|                                                                       /
C| SERAFIN  = INCHANGE => MERE COPY OF ONE OF THE NPROC FILES            /
C| VOLFIN   = CHANGE => SUM OF THE RESULTS PART_INS,PART_CUM             /
C| SCALAIRE = CHANGE => SUM OF THE RESULTS NBPART_LOST...                /
C|                                                                       /
C|=======================================================================/
C
C
      CALL RECOMPOSITION_PARTICULAIRE(GEO)
C
C
C|==================================================================|
C|     	                                                            |
C| END: MERGES FILES RESULTING FROM THE PARTICULATE DECOMPOSITION   |
C|                                                                  |
C|==================================================================|
C
C
        ELSE
C
C
C|==================================================================|
C|     	                                                            |
C| START: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION      |
C|                                                                  |
C|==================================================================|
C
        CALL RECOMPOSITION_DECOMP_DOMAINE (GEO)
C
C|==================================================================|
C|     	                                                            |
C| END: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION        |
C|                                                                  |
C|==================================================================|
C


      ENDIF

      STOP

      END PROGRAM GRETEL
!
!=============================================================================
!



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MERGES THE RESULTS FROM COMPUTATION USING PARTICULATE
!>                DECOMPOSITION IN ESTEL-2D. ONLY MERGES THE FOLLOWING FILES:<br>
!><br>  1/ SERAFIN (E2DSERA): NO MODIFICATION.
!>         MERE COPY OF A SINGLE FILE.
!><br>  2/ VOLFIN (E2DVOL): PARTICULATE VARIABLES ARE MODIFIED.
!>         THE VALUES STORED IN EACH PROCESSOR'S FILES ARE SUMMED UP.
!><br>  3/ SCALAR RESULTS (E2DSCAL): PARTICULATE VARIABLES ARE MODIFIED.
!>         THE VALUES ARE SUMMED UP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GEO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AT, ECKEN, ENDE, ENDEOFFILE, ERR, FU, GEOM, GLOBAL_VALUE, I, IKLE3D, IKLESA, IPID, IPOBO, IPOBO3D, IS, I_LEN, I_S, I_SP, J, K, KNOLG, LI, LINE, LOCAL_VALUE, LOCAL_VALUELEM, MAXVAL, NBLINE, NBV1, NBV2, NBVAR, NDUM, NELEM, NPLAN, NPOIN, NPOIN2, NPOIN2LOC, NPOINMAX, NPROC, NRESU, NUM_PROC, PARAM, PART, PART_REP, PTEXCL, RUB, RUBENS, SOMME, SOMMEPART, SOMMERESU, TEMPS, TEXTLU, TITLE, TITRE, TITSEL, VALUESCP, VERIF, XORIG, YORIG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOER(), CPIKLE2(), EXTENS(), PLANTE(), READ_DATASET(), READ_DATASET_ELEM(), SKIP_HEADER()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GRETEL()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td>                                                         </td>
!>    <td> 18/07/2005                                              </td>
!>    <td> FABIEN DECUNG (STAGIAIRE MATMECA)                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td><-></td><td>TABLEAU DES CONNECTIVITES
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE SOMMETS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE RECOMPOSITION_PARTICULAIRE (GEO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GEO            |---| 
C| IKLE           |<->| TABLEAU DES CONNECTIVITES
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
C=>FABS
      CHARACTER(LEN=30), INTENT(IN) :: GEO
C<=FABS
      INTEGER IPID,ERR,FU
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC
      INTEGER NPROC,NRESU,NPOINMAX
      INTEGER I_S, I_SP, I_LEN
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,IPOBO,VERIF,IPOBO3D
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA,IKLE3D

      !FABS------------------------------------------------------!
      INTEGER, DIMENSION(:)    , ALLOCATABLE   :: PART
C=>FABS : NAG BUG
      INTEGER, DIMENSION(:)    , ALLOCATABLE   :: PART_REP
C<=FABS
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
C      CHARACTER*32 GEO,GEOM
C=>FABS
      CHARACTER*32 GEOM
C<=FABS

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

        !!!!FABS: NEED TO CHECK  WHETHER THE FILES ARE SERAPHIN OR VOLFIN OR SCP
        !!!!    : BECAUSE THE MERGING METHODS DEPEND ON THE TYPE OF FILES.

        IF ((RUBENS.EQ.'E2DSERA').OR.(RUBENS.EQ.'E2DVOL')) THEN
C
C     COMPUTATION GEOMETRY FILE, READ UNTIL THE 10 PARAMETERS:
C

C!!!FABS: ONLY IF GEO FILE IS DECLARED IN PARAL IN THE DICTIONARY
!FABS-----------------------------------------!
C I_S  = LEN (GEO)
C        I_SP = I_S + 1
C        DO I=1,I_S
C         IF(GEO(I_SP-I:I_SP-I) .NE. ' ') EXIT
C        ENDDO
C        I_LEN=I_SP - I
!
C GEOM=GEO(1:I_LEN) // EXTENS(NPROC-1,0)
C!!!FABS: OTHERWISE TAKE THE ROOT E2DGEO
        GEOM = GEO
!FABS-----------------------------------------!

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
C     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
C
C     RESULTS FILE:
C
      OPEN(3,FILE=RUBENS,FORM='UNFORMATTED',ERR=9991)
      GO TO 9993
9991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUBENS
      CALL PLANTE(-1)
      STOP
9993   CONTINUE
C
C     1) STARTS READING THE 1ST RESULT FILE
C
C
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
C
C  1 : TITLE
C
      READ(4) TITRE
      WRITE(LU,*) 'TITLE=',TITRE
      TITSEL=TITRE // 'SERAPHIN'
      WRITE(3) TITSEL
C
C  2 : NBV1,NBV2
C
      READ(4) NBV1,NBV2
      WRITE(LU,*) 'NBV1=',NBV1,'   NBV2=',NBV2
      WRITE(3) NBV1,NBV2
C
C  3 : NAMES AND UNITS OF THE VARIABLES
C
!
      !!!!FABS: ALLOCATES THE PARTICULATE POINTER
!
      ALLOCATE (PART(1:2))
      PART = 0
!
      DO 5500 I=1,NBV1
        READ(4) TEXTLU(I)
        WRITE(LU,*) 'VARIABLE ',I,' : ',TEXTLU(I)

        !!!!FABS: IDENTIFIES THE NUMBER OF THE VARIABLES TO SUM UP
        !!!!      FOR PART_INS => PART(1)
        !!!!      FOR PART_CUM => PART(2)

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

5500  CONTINUE
!
C      IF ((RUBENS.EQ.'E2DVOL').AND.
C     &   ((PART(1).EQ.0).OR.(PART(2).EQ.0))) THEN
C       WRITE(LU,*) 'PAS RESULTATS PART INS OU CUM'
C WRITE(LU,*) 'VERIFIER VOS SORTIES PARTICULAIRES VOLFIN'
C CALL PLANTE(-1)
C      ENDIF
C
C  4 : 10 PARAMETERS
C
      READ(4) (PARAM(I),I=1,10)
      WRITE(LU,*) '10 PARAMETERS : ',PARAM
      PARAM(9)=0
      PARAM(8)=0
      NPLAN=PARAM(7)
      WRITE(3) (PARAM(I),I=1,10)
C READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(4)  (PARAM(I),I=1,6)
        WRITE(3) (PARAM(I),I=1,6)
      ENDIF

      CLOSE(4)

C
C  5: READS THE VARIABLES NELEM: 4 PARAMETERS
C
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',  NELEM
      WRITE(LU,*) 'NPOIN2=', NPOIN2
      WRITE(LU,*) 'ECKEN=',  ECKEN
      WRITE(LU,*) 'NDUM=',   NDUM
C
      IF(NPLAN.EQ.0) THEN
        WRITE(3) NELEM,NPOIN2,ECKEN,NDUM
      ELSE
        WRITE(3) NELEM*(NPLAN-1),NPOIN2*NPLAN,6,NDUM
      ENDIF
C
C  DYNAMICALLY ALLOCATES THE ARRAYS
C
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
C  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      IF(NPLAN.EQ.0) THEN
       ! ALLOCATE(GLOBAL_VALUE(NPOIN2,NBV1)       ,STAT=ERR)
       ALLOCATE(GLOBAL_VALUE(NELEM,NBV1))
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'GLOBAL_VALUE')
C  X AND Y
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'YORIG')
C  RELATED TO A 3D CASE
      IF(NPLAN.NE.0) THEN
      ALLOCATE(IKLE3D(NELEM*(NPLAN-1),6),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLE3D')
      ALLOCATE(IPOBO3D(NPOIN2*NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO3D')
      ENDIF
C
C  END OF ALLOCATION ...
C
C  6 : IKLE
C
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      WRITE(LU,*) 'WRITING IKLE'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      ELSE
C       WRITES HERE IKLE3D (WITH INVERSION OF DIMENSIONS)
        CALL CPIKLE2(IKLE3D,IKLESA,NELEM,NELEM,NPOIN2,NPLAN)
        WRITE(3) ((IKLE3D(I,J),J=1,6),I=1,NELEM*(NPLAN-1))
      ENDIF
C
C  7 : IPOBO
C
      READ(2)  (IPOBO(I),I=1,NPOIN2)
      WRITE(LU,*) 'WRITING IPOBO'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) (IPOBO(I),I=1,NPOIN2)
      ELSE
C       DUMMY VALUES
        DO I=1,NPOIN2*NPLAN
          IPOBO3D(I) = 0
        ENDDO
        WRITE(3) (IPOBO3D(I),I=1,NPOIN2*NPLAN)
      ENDIF
C
C  8 : X AND Y, WILL BE CHECKED LATER ...
C
      READ(2)  (XORIG(I),I=1,NPOIN2)
      READ(2)  (YORIG(I),I=1,NPOIN2)

C
C------------------------------------------------------------------------------
C
C OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
C
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
C
      NPOINMAX = MAXVAL(NPOIN)
C ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'KNOLG')
C  LOCAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
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
C
C READS KNOLG(NPOIN,NPROC)
C
      !!!!FABS: IS IT USEFUL TO STORE IN THE DECOMPOSITION ?... NO!
C
      DO IPID = 0,NPROC-1
         FU = IPID + 10
         IF(NPLAN.EQ.0) THEN
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1))
         ELSE
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
         ENDIF
      END DO
C
C READS LOCAL X
C
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
C
C READS DATASETS
C
      NRESU = 0
C
20000 NRESU = NRESU + 1
C
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          VERIF(I)=0
        ENDDO
      ENDIF
C
      WRITE(LU,*)'TRY TO READ DATASET NO.',NRESU
C
!
      !!!!FABS: CHECKS THE FILE TYPE  :=> SERAFIN = E2DSERA
      !!!!    :                       :=> VOLFIN  = E2DVOL
      !!!!    :                       :=> SCP     = E2DSCAL
!
!
      IF (RUBENS.EQ.'E2DSERA') THEN

                !!! FABS: THE FILE IS A SERAFIN FILE.
                !!!     : THE VALUES ARE CONSIDERED AT THE NODES.
                !!!     : ONLY ONE PROCESSOR IS CONSIDERED BECAUSE
                !!!     : THE VALUES ARE THE SAME ON EACH PROCESSOR

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

        ELSE
          IF (RUBENS.EQ.'E2DVOL') THEN
            !!! FABS:
            !!! THE FILE IS A VOLFIN FILE, PART_CUM AND PART_INS
            !!! ARE MODIFIED BY THE PARALLELISATION.
            !!! THE VALUES ARE CONSIDERED AT THE CELLS.
            !!! DEPENDING ON THE CASE, ONE OR ALL PROCESSORS ARE CONSIDERED.

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
C   IF (K.EQ.PART(2)) THEN
                  !!! FABS:
                  !!! THE FILE IS A CUM PARTICLES FILE, THE PARTICLES
                  !!! INITIALLY INTRODUCED, WHICH ARE NOT PART OF THE
                  !!! PARALLELISATION MUST BE SUBTRACTED.
C   WRITE(3) ( SOMMEPART(I), I=1,NELEM )
C   ELSE
                  WRITE(3) ( SOMMEPART(I), I=1,NELEM )
C   ENDIF
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

      ELSE !!!! IF (RUBENS.EQ.'E2DVOL').OR.(RUBENS.EQ.'E2DSERA')

      !!!!FABS: THE FILE TO BE READ IS A SCALAR RESULTS FILE.
!
      OPEN(3,FILE=RUBENS,FORM='FORMATTED',ERR=99991)
      GO TO 99993
99991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RUBENS
      CALL PLANTE(-1)
      STOP
99993 CONTINUE
C
C     1) STARTS READING THE 1ST RESULT FILE
C
C
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
C
C OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
C
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

         !!!! HAS TO STORE DEPENDING ON THE TIMESTEP

10190    TEMPS = 0
         !!!! COUNTS THE NUMBER OF TIMESTEPS
         DO WHILE (.NOT.ENDEOFFILE)
           READ(FU,*,END=6996) TITLE
           TEMPS = TEMPS + 1
         ENDDO

6996     TEMPS = TEMPS - NBLINE
         WRITE(LU,*) 'NUMBER OF TIME STEPS', TEMPS

         IF (IPID.EQ.0) THEN
         ALLOCATE (VALUESCP(1:TEMPS+1,0:NPROC-1,1:NBVAR-3))
         ALLOCATE (SOMMERESU(1:8))
         VALUESCP=0.
         ENDIF

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

9799    PRINT*, 'ERROR'
        CALL PLANTE(-1)
        STOP

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

      END IF !!!! IF (RUBENS.EQ.'E2DVOL').OR.(RUBENS.EQ.'E2DSERA')
!
      STOP
!
      END SUBROUTINE RECOMPOSITION_PARTICULAIRE
!
!=============================================================================
!
!=============================================================================
!
!=============================================================================
!



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       {text}

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GEO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AT, ECKEN, ENDE, ERR, FU, GLOBAL_VALUE, I, IKLE3D, IKLESA, IPID, IPOBO, IPOBO3D, IS, I_LEN, I_S, I_SP, J, K, KNOLG, LI, LOCAL_VALUE, MAXVAL, NBV1, NBV2, NDUM, NELEM, NPLAN, NPOIN, NPOIN2, NPOIN2LOC, NPOINMAX, NPROC, NRESU, PARAM, RES, RESPAR, TEXTLU, TITRE, TITSEL, VERIF, XORIG, YORIG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOER(), CPIKLE2(), EXTENS(), PLANTE(), READ_DATASET(), SKIP_HEADER()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GRETEL()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GEO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE RECOMPOSITION_DECOMP_DOMAINE (GEO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GEO            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
C
C=>FABS
      CHARACTER(LEN=30), INTENT(IN) :: GEO
C<=FABS

      INTEGER IPID,ERR,FU
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC
      INTEGER NPROC,NRESU,NPOINMAX
      INTEGER I_S, I_SP, I_LEN
C
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,IPOBO,VERIF,IPOBO3D
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA,IKLE3D
C
C
      REAL   , DIMENSION(:,:), ALLOCATABLE :: GLOBAL_VALUE
      REAL   , DIMENSION(:,:), ALLOCATABLE :: LOCAL_VALUE
      REAL   , DIMENSION(:)  , ALLOCATABLE :: XORIG,YORIG
C
      REAL AT
C
      LOGICAL IS,ENDE
C
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
C
C READS FILE NAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
C
C=>FABS
C      READ(LI,*) GEO
C        WRITE(LU,*) GEO
C      WRITE (LU, ADVANCE='NO',
C     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
C<=FABS
C
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
C
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE (LU,*) ' '

      INQUIRE (FILE=GEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL PLANTE (-1)
        STOP
      END IF
C
      I_S  = LEN (RES)
      I_SP = I_S + 1
      DO I=1,I_S
         IF(RES(I_SP-I:I_SP-I) .NE. ' ') EXIT
      ENDDO
      I_LEN=I_SP - I

C
C     COMPUTATION GEOMETRY FILE, READ UNTIL THE 10 PARAMETERS:
C
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
C     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
C
C     RESULTS FILE:
C
      OPEN(3,FILE=RES,FORM='UNFORMATTED',ERR=991)
      GO TO 993
991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RES
      CALL PLANTE(-1)
      STOP
993   CONTINUE
C
C     1) STARTS READING THE 1ST RESULT FILE
C
CCC      RESPAR=RES // EXTENS(2**IDIMS-1,0)
C
      RESPAR=RES(1:I_LEN) // EXTENS(NPROC-1,0)
C
      INQUIRE (FILE=RESPAR,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RESPAR
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL PLANTE(-1)
        STOP
      END IF
C
      OPEN(4,FILE=RESPAR,FORM='UNFORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(-1)
      STOP
995   CONTINUE
C
C  1 : TITLE
C
      READ(4) TITRE
      WRITE(LU,*) 'TITLE=',TITRE
      TITSEL=TITRE // 'SERAFIN'
      WRITE(3) TITSEL
C
C  2 : NBV1,NBV2
C
      READ(4) NBV1,NBV2
      WRITE(LU,*) 'NBV1=',NBV1,'   NBV2=',NBV2
      WRITE(3) NBV1,NBV2
C
C  3 : NAMES AND UNITS OF THE VARIABLES
C
      DO 500 I=1,NBV1
        READ(4) TEXTLU(I)
        WRITE(LU,*) 'VARIABLE ',I,' : ',TEXTLU(I)
        WRITE(3) TEXTLU(I)
500   CONTINUE
C
C  4 : 10 PARAMETERS
C
      READ(4) (PARAM(I),I=1,10)
      WRITE(LU,*) '10 PARAMETERS : ',PARAM
      PARAM(9)=0
      PARAM(8)=0
      NPLAN=PARAM(7)
      WRITE(3) (PARAM(I),I=1,10)
C READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(4)  (PARAM(I),I=1,6)
        WRITE(3) (PARAM(I),I=1,6)
      ENDIF
      CLOSE(4)
C
C  2) READS THE GEOMETRY FILE
C
C  5 : 4 PARAMETERS
C
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',NELEM
      WRITE(LU,*) 'NPOIN2=',NPOIN2
      WRITE(LU,*) 'ECKEN=',ECKEN
      WRITE(LU,*) 'NDUM=',NDUM
C
      IF(NPLAN.EQ.0) THEN
        WRITE(3) NELEM,NPOIN2,ECKEN,NDUM
      ELSE
        WRITE(3) NELEM*(NPLAN-1),NPOIN2*NPLAN,6,NDUM
      ENDIF
C
C  DYNAMICALLY ALLOCATES THE ARRAYS
C
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
C  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(GLOBAL_VALUE(NPOIN2,NBV1)       ,STAT=ERR)
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN,NBV1) ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'GLOBAL_VALUE')
C  X AND Y
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'YORIG')
C  3D
      IF(NPLAN.NE.0) THEN
      ALLOCATE(IKLE3D(NELEM*(NPLAN-1),6),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLE3D')
      ALLOCATE(IPOBO3D(NPOIN2*NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO3D')
      ENDIF
C
C  END OF ALLOCATION ...
C
C  6 : IKLE
C
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      WRITE(LU,*) 'WRITING IKLE'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
      ELSE
C       WRITES HERE IKLE3D (WITH INVERSION OF DIMENSIONS)
        CALL CPIKLE2(IKLE3D,IKLESA,NELEM,NELEM,NPOIN2,NPLAN)
        WRITE(3) ((IKLE3D(I,J),J=1,6),I=1,NELEM*(NPLAN-1))
      ENDIF
C
C  7 : IPOBO
C
      READ(2)  (IPOBO(I),I=1,NPOIN2)
      WRITE(LU,*) 'WRITING IPOBO'
      IF(NPLAN.EQ.0) THEN
        WRITE(3) (IPOBO(I),I=1,NPOIN2)
      ELSE
C       DUMMY VALUES
        DO I=1,NPOIN2*NPLAN
          IPOBO3D(I) = 0
        ENDDO
        WRITE(3) (IPOBO3D(I),I=1,NPOIN2*NPLAN)
      ENDIF
C
C  8 : X AND Y, WILL BE CHECKED LATER ...
C
      READ(2)  (XORIG(I),I=1,NPOIN2)
      READ(2)  (YORIG(I),I=1,NPOIN2)
C
C------------------------------------------------------------------------------
C
C OPENS FILES AND READS/SKIPS HEADERS -> NPOIN(NPROC), NPOINMAX
C
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
C
      NPOINMAX = MAXVAL(NPOIN)
C ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'KNOLG')
C  LOCAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
        ALLOCATE(LOCAL_VALUE(NPOINMAX,NBV1)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LOCAL_VALUE')
C
C READS KNOLG(NPOIN,NPROC)
C
      DO IPID = 0,NPROC-1
         FU = IPID +10
         IF(NPLAN.EQ.0) THEN
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1))
         ELSE
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
         ENDIF
      END DO
C
C READS LOCAL X
C
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
C
C COMPARISON WITH GLOBAL VALUES
C
C     IN 3D, CHECKS THE FIRST PLANE ONLY
      DO I=1,NPOIN2
         IF(ABS(XORIG(I)-GLOBAL_VALUE(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' XORIG=',XORIG(I),
     &                ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
         ENDIF
      ENDDO
C FURTHER CHECKS
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
C WRITES X
      WRITE(LU,*) 'WRITING X-COORDINATES'
      IF(NPLAN.EQ.0) THEN
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2)
      ELSE
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2*NPLAN)
      ENDIF
C
C READS LOCAL Y
C
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
C
C COMPARISON WITH GLOBAL VALUES
C
C IN 3D, CHECKS THE FIRST PLANE ONLY
      DO I=1,NPOIN2
         IF(ABS(YORIG(I)-GLOBAL_VALUE(I,1)).GT.0.1) THEN
            WRITE(LU,*) 'POINT ',I,' YORIG=',YORIG(I),
     &                      ' GLOBAL_VALUE=',GLOBAL_VALUE(I,1)
            WRITE(LU,*) 'GEO IS PROBABLY NOT THE RIGHT ORIGINAL FILE'
         ENDIF
      END DO
C FURTHER CHECKS
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
C WRITES Y
      WRITE(LU,*) 'WRITING Y-COORDINATES'
      IF(NPLAN.EQ.0) THEN
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2)
      ELSE
         WRITE(3) (GLOBAL_VALUE(I,1),I=1,NPOIN2*NPLAN)
      ENDIF
C
C READS DATASETS
C
      NRESU = 0
C
2000  NRESU = NRESU + 1
C
      IF(NPLAN.EQ.0) THEN
        DO I=1,NPOIN2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NPOIN2*NPLAN
          VERIF(I)=0
        ENDDO
      ENDIF
C
      WRITE(LU,*)'TRY TO READ DATASET NO.',NRESU
C
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CALL READ_DATASET
     &   (LOCAL_VALUE,NPOINMAX,NPOIN(IPID+1),NBV1,AT,FU,ENDE)
         IF (ENDE) GOTO 3000
C STORES EACH DATASET
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
C WRITES GLOBAL DATASET
      WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',AT
C
      WRITE(3) AT
      DO K = 1,NBV1
         IF(NPLAN.EQ.0) THEN
            WRITE(3) (GLOBAL_VALUE(I,K),I=1,NPOIN2)
         ELSE
            WRITE(3) (GLOBAL_VALUE(I,K),I=1,NPOIN2*NPLAN)
         ENDIF
      END DO
C CHECKS ...
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
C
      GO TO 2000
C
3000  WRITE(LU,*) 'END OF PROGRAM, ',NRESU-1,' DATASETS FOUND'
C
      CLOSE(2)
      CLOSE(3)
C
      DO IPID = 0,NPROC-1
         FU = IPID +10
         CLOSE (FU)
      END DO
C
        !!!FABS

      END SUBROUTINE RECOMPOSITION_DECOMP_DOMAINE



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTENSION OF THE FILES ON EACH PROCESSOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IPID, N
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_OPEN_FILES(), DREDGESIM_INTERFACE(), GREDELHYD(), GREDELMET(), GREDELPTS(), GREDELSEG(), RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td> 4.0                                                     </td>
!>    <td> 08/01/1997                                              </td>
!>    <td> J-M HERVOUET (LNH)                                      </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IPID
!></td><td>--></td><td>NUMERO DU PROCESSEUR
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NOMBRE DE PROCESSEURS MOINS UN = NCSIZE-1
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        CHARACTER*11 FUNCTION EXTENS
     &(N,IPID)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IPID           |-->| NUMERO DU PROCESSEUR
C| N             |-->| NOMBRE DE PROCESSEURS MOINS UN = NCSIZE-1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER IPID,N
C
C-----------------------------------------------------------------------
C
      IF(N.GT.0) THEN
C
        EXTENS='00000-00000'
C
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
C
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
C
      ELSE
C
        EXTENS='       '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       {text}

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ERR, FU, LU, NPOIN, NVALUE
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ECKEN, I, NBV1, NDUM, NELEM, NPLAN, PARAM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ERR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVALUE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE SKIP_HEADER
     &(FU,NPOIN,NVALUE,ERR,LU)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ERR            |---| 
C| FU             |---| 
C| LU             |---| 
C| NPOIN          |---| 
C| NVALUE         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER NPOIN,NELEM,ECKEN,NDUM,NBV1,NVALUE,I,NPLAN
      INTEGER FU,ERR,LU
      INTEGER PARAM(10)
C
C  1 : SKIPS TITLE
C
      READ(FU,ERR=999)
C
C  2 : READS NBV1
C
      READ(FU,ERR=999) NBV1
      IF (NBV1.NE.NVALUE) THEN
        WRITE(LU,*)  'NBV1.NE.NVALUE! CHECK OUTPUT FILES ...'
        CALL PLANTE(-1)
        STOP
      ENDIF
C
C  3 : SKIPS NAMES AND UNITS OF THE VARIABLES
C
      DO I=1,NBV1
        READ(FU,ERR=999)
      END DO
C
C  4 : 10 PARAMETERS
C
      READ(FU,ERR=999) (PARAM(I),I=1,10)
      NPLAN=PARAM(7)
C  READS THE DATE (OPTIONAL) AND WRITES IT OUT
      IF(PARAM(10).EQ.1) THEN
        READ(FU,ERR=999)  (PARAM(I),I=1,6)
      ENDIF
C
C  5 : 4 PARAMETERS
C
      READ(FU,ERR=999) NELEM,NPOIN,ECKEN,NDUM
C
C  6 : IKLE
C
      READ(FU,ERR=999)
C
 999  RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, ENDE, FU, LOCAL_VALUE, NPOIN, NPOINMAX, NVALUE
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPOIN, IVALUE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELPTS(), GREDELSEG(), RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENDE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LOCAL_VALUE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOINMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVALUE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE READ_DATASET
     &(LOCAL_VALUE,NPOINMAX,NPOIN,NVALUE,AT,FU,ENDE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |---| 
C| ENDE           |---| 
C| FU             |---| 
C| LOCAL_VALUE    |---| 
C| NPOIN          |---| 
C| NPOINMAX       |---| 
C| NVALUE         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER NPOINMAX,NPOIN,NVALUE,FU
      INTEGER IPOIN,IVALUE
C
      REAL AT
      REAL LOCAL_VALUE(NPOINMAX,NVALUE)
C
      LOGICAL ENDE
C
      ENDE = .TRUE.
C
      READ(FU,END=999) AT
      DO IVALUE = 1,NVALUE
         READ(FU,END=999) (LOCAL_VALUE(IPOIN,IVALUE),IPOIN=1,NPOIN)
      END DO
C
      ENDE = .FALSE.
C
 999  RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       {text}

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, ENDE, FU, IPID, LOCAL_VALUELEM, NBV1, NELEM, NPROC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IVALUE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RECOMPOSITION_PARTICULAIRE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td>                                                         </td>
!>    <td>                                                         </td>
!>    <td> FABS                                                    </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENDE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IPID
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LOCAL_VALUELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBV1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPROC
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE READ_DATASET_ELEM
     &(LOCAL_VALUELEM,NPROC,NELEM,NBV1,AT,FU,IPID,ENDE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |---| 
C| ENDE           |---| 
C| FU             |---| 
C| IPID           |---| 
C| LOCAL_VALUELEM |---| 
C| NBV1           |---| 
C| NELEM          |---| 
C| NPROC          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER NPROC,NELEM,NBV1,FU,IPID
      INTEGER IELEM,IVALUE
C
      REAL AT
      REAL LOCAL_VALUELEM(0:NPROC-1,1:NELEM,1:NBV1)
C
      LOGICAL ENDE
C
      ENDE = .TRUE.
C
      READ(FU,END=9099) AT
      DO IVALUE = 1,NBV1
         READ(FU,END=9099) (LOCAL_VALUELEM(IPID,IELEM,IVALUE)
     &   ,IELEM=1,NELEM)
      END DO
C
      ENDE = .FALSE.
C
 9099  RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTENSION OF THE CONNECTIVITY TABLE.
!>                CASE OF EXTENSION TO A QUASI-BUBBLE ELEMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE3, IKLES, NELEM2, NELMAX2, NPLAN, NPOIN2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IELEM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALMESH(), RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td> 5.1                                                     </td>
!>    <td> 23/08/1999                                              </td>
!>    <td> J-M HERVOUET (LNH) 30 87 80 18                          </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td><-></td><td>TABLEAU DES CONNECTIVITES
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE SOMMETS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CPIKLE2
     &(IKLE3,IKLES,NELEM2,NELMAX2,NPOIN2,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |<->| TABLEAU DES CONNECTIVITES
C| IKLE3          |---| 
C| IKLES          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELEM2         |---| 
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NELMAX2        |---| 
C| NPLAN          |---| 
C| NPOIN          |-->| NOMBRE DE SOMMETS DU MAILLAGE
C| NPOIN2         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM2,NELMAX2,NPOIN2,NPLAN
      INTEGER, INTENT(INOUT) :: IKLES(3,NELEM2)
      INTEGER, INTENT(INOUT) :: IKLE3(NELMAX2,NPLAN-1,6)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I
C
C-----------------------------------------------------------------------
C
C     BOTTOM AND TOP OF ALL LAYERS
C
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
C
C-----------------------------------------------------------------------
C
      RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHFILE, N
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELHYD(), GREDELMET(), GREDELPTS(), GREDELSEG(), PARES3D(), RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHFILE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE ALLOER (N, CHFILE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHFILE         |---| 
C| N             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      CHARACTER*(*), INTENT(IN) :: CHFILE
      WRITE(N,*) 'ERROR BY ALLOCATION OF ',CHFILE
      CALL PLANTE(-1)
      STOP
      END SUBROUTINE ALLOER



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IVAL
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ICODE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> EXIT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>AKEPIN(), ALMESH(), AS3_1313_Q(), AS3_1313_S(), AS3_4141_Q(), AS3_4141_S(), ASSEX3(), BEDLOAD_BAILARD(), BEDLOAD_DIFFIN(), BEDLOAD_FORMULA(), BEDLOAD_HIDING_FACTOR(), BIEF_SUITE(), BIEF_SUM(), BISSEL(), BORD3D(), BUILD_GLOBAL_FRONT(), BYPASS_CRUSHED_POINTS_SEG(), CALCOT(), CARACT(), CARAFR(), CFLPSI(), CHAR13(), CHARAC(), CHECK(), CHECK_DIGITS(), CHGDIS(), CHGELM(), CLHUVT(), CMPOBJ(), COEFRO(), COEFRO_SISYPHE(), COMPLIM(), COMP_IKLE(), COMP_NH_COM_SEG(), COMP_SEG(), CONDIH(), CONDIM(), CONDIN(), CONDIW(), CORRSL(), COST_FUNCTION(), COUUTI(), CPIKLE2(), CPIKLE2(), CPIKLE3(), CPSTMT(), CPSTVC(), CREATE_DATASET(), CVDFTR(), CVTRVF(), DCPLDU(), DEBIMP(), DEBIMP3D(), DEBSCE(), DECLDU(), DECVRT(), DERI3D(), DERIVE(), DESCEN(), DESSED(), DESSEG(), DIFFIN(), DIM1_EXT(), DIM2_EXT(), DIMENS(), DIRICH(), DLDU11(), DLDU21(), DLDU41(), DLDUSEG(), DOTS(), DOWNUP(), DRAGFO(), DREDGESIM_INTERFACE(), DRIUTI(), DRSURR(), ECRI2(), ELEB3D(), ELEB3DT(), ELEBD(), ELEBD(), ELEBD31(), ERODC(), ERODE(), EXTMSK(), FILPOL(), FLUCIN(), FLUSEC(), FLUSEC_SISYPHE(), FLUSEC_TELEMAC2D(), FLUXPR_SISYPHE(), FLUXPR_TELEMAC2D(), FLUX_EF_VF(), FLUX_EF_VF_3D(), FRICTI(), FRICTION_CALC(), FRICTION_CHOICE(), FRICTION_INIT(), FRICTION_READ(), FRICTION_SCAN(), FRONT2(), FROPRO(), GEOELT(), GETTRI(), GODOWN(), GODWN1(), GOUP(), GOUP1(), GREDELHYD(), GREDELMET(), GREDELPTS(), GREDELSEG(), GSEBE(), HOMERE_ADJ_T2D(), HOMERE_TELEMAC2D(), IELBOR(), IFAB3D(), INBIEF(), INIT_AVAI(), INIT_MIXTE(), INIVEN(), INTEG(), INTERP(), INTERPOL(), INVMTX(), KEPCL3(), KEPSCL(), KEPSIL(), KEPSIN(), KOMCL3(), KSUPG(), LAGRAN(), LAYER(), LECDOI(), LECDON(), LECDON_ARTEMIS(), LECDON_SISYPHE(), LECDON_TELEMAC2D(), LECDON_TELEMAC3D(), LECHAM(), LECLIM(), LECLIM_ARTEMIS(), LECLIM_TOMAWAC(), LECLIS(), LECSIP(), LECSNG(), LECSUI(), LECUTI(), LICHEK(), LIT(), LITENR(), LONGMB(), LONGML(), LUDCMP(), LUMP(), MAJTRAC(), MARUTI(), MASKTO(), MATRBL(), MATRIX(), MATRIY(), MATVCT(), MATVEC(), MESURES(), METGRA(), MT02AA(), MT02AA_2(), MT02BB(), MT02CC(), MT02PP(), MT02PT(), MT02TT(), MT03AA(), MT03BB(), MT03CC(), MT04AA(), MT04BB(), MT04CC(), MT04PP(), MT04TT(), MT05AA(), MT05BB(), MT05CC(), MT05PP(), MT05TT(), MT06AA(), MT06CC(), MT06FF(), MT06FT(), MT06FT2(), MT06OC(), MT06OO(), MT06PP(), MT06TT(), MT07AA(), MT08AA(), MT08AB(), MT08AC(), MT08BA(), MT08BB(), MT08PP(), MT08TT(), MT11AA(), MT11AB(), MT11AC(), MT11BA(), MT11BB(), MT12AA(), MT12AB(), MT12AC(), MT12BA(), MT12BB(), MT13AA(), MT13AB(), MT13BA(), MT13BB(), MT13CA(), MT13CC(), MT99AA(), MT99BB(), MURD3D(), MURD3D_POS(), MV0202(), MV0303(), MV0304(), MV0306(), MV0403(), MV0404(), MV0603(), MV0606(), MVSEG(), MW0303(), M_MED(), NBFEL(), NBMPTS(), NBPEL(), NBPTS(), NBSEG(), NBSEGEL(), NOMVAR_SISYPHE(), NOUDON(), NOUMAR(), OM(), OM0101(), OM1101(), OM1111(), OM1112(), OM1113(), OM1201(), OM1211(), OM1302(), OM1311(), OM2121(), OM3181(), OM4111(), OM4121(), OM4141(), OM5111(), OM5161(), OMSEG(), OMSEGBOR(), OPASS(), OS(), OSBD(), OSDB(), OSDBIF(), OV(), OVBD(), OVD(), OVDB(), PARACO(), PARCOM(), PARINI(), PARMOY(), POINT_TELEMAC2D(), POINT_TELEMAC3D(), POROS(), PRE4_MUMPS(), PREBD4(), PREBD9(), PREBDT(), PRECDT(), PRECON(), PREDIV(), PREVEREBE(), PREVERSEG(), PROPAG(), PROPAG_ADJ(), PROPIN_TELEMAC2D(), PROSOU(), PROXIM(), PTTOEL(), PUOG(), P_DOTS(), P_LSUM(), Q(), Q3(), QSFORM(), READGEO1(), READ_FIC_CURVES(), READ_FIC_FRLIQ(), READ_FIC_SOURCES(), READ_SECTIONS_SISYPHE(), READ_SECTIONS_TELEMAC2D(), READ_SUBMIT(), RECOMPOSITION_DECOMP_DOMAINE(), RECOMPOSITION_PARTICULAIRE(), REMONT(), REMSEG(), RESCUE(), RESCUE_SISYPHE(), RESCUE_SISYPHE_NOTPERMA(), RESOLU(), SD_SOLVE_1(), SEGBOR(), SISYPHE(), SIS_ARRET(), SKIP_HEADER(), SL(), SL3(), SLOPES(), SMOOTHING_FLUX(), SOLAUX(), SOLVE(), SOLVE_MUMPS(), SORTIE(), SPECTRE(), STOSEG(), STOSEG(), STOSEG41(), STREAMLINE(), STREAMLINE_TOMAWAC(), SUISED(), SUITE_SERAFIN(), SURVOL(), SUSPENSION_BIJKER(), SUSPENSION_DISPERSION(), SUSPENSION_EROSION_COH(), T3D_DEBSCE(), T3D_READ_FIC_CURVES(), T3D_TRSCE(), TBORD(), TELEMAC2D(), TESTEUR(), TFOND(), THOMPS(), TNOMER(), TOMAWAC_MPI(), TOPOGR(), TR(), TR3(), TRACVF(), TRANSF_ZCHAR(), TRA_PROF_Z(), TRID3D(), TRISOU(), TRSCE(), TVF(), UM1X(), UPWIND(), UPWINDEBE(), UPWINDSEG(), VC01AA(), VC01BB(), VC01FF(), VC01FT(), VC01FT2(), VC01OO(), VC01PP(), VC01TT(), VC01TT0(), VC03AA(), VC03BB(), VC04AA(), VC04PP(), VC04TT(), VC05AA(), VC05FF(), VC05FT(), VC05OO(), VC08AA(), VC08BB(), VC08CC(), VC08PP(), VC08TT(), VC09AA(), VC10OO(), VC11AA(), VC11AA2(), VC11BB(), VC11PP(), VC11TT(), VC11TT0(), VC13AA(), VC13BB(), VC13CC(), VC13PP(), VC13PP2(), VC13TT(), VC14AA(), VC15AA(), VC16AA(), VC18PP(), VC19AA(), VECLEN(), VECTOR(), VECTOS(), VEL_PROF_Z(), VENUTI(), VERMOY(), VGFPSI(), VISCLM(), VIT(), VIT3(), VOISIN(), VOISIN(), VOISIN31(), WAC(), WAITFOR(), WAVE_EQUATION(), WRITE_DATA(), WRITE_MESH(), WRITE_MESH_SERAFIN()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IVAL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE PLANTE(IVAL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IVAL           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
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

C
C#######################################################################
C