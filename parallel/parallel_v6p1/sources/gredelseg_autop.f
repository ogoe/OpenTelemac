C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MERGES THE RESULTS OF A PARALLEL COMPUTATION (COUPLING
!>                WITH DELWAQ) TO WRITE A SINGLE FILE IN DELWAQ FORMAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CONLIM, ECKEN, ELTSEG, ENDE, ERR, FILETYPE, FU, GEO, GLOBAL_VALUE, GLOSEG, GLOSEGLOC, I, IADR, IDUM, IELM, IFABOR, IG1, IG2, IGTEMP, IKLBOR, IKLE, IKLESA, IL1, IL2, IPID, IS, ISEG, IT, IVOIS, I_LEN, I_S, I_SP, J, K, KLOG, KNOLG, KP1BOR, KSEGLG, LI, LIHBOR, LIHBOR0, LIHBORLOC, LOCAL_VALUE, MAXNVOIS, MBND, MBND2, MBNDLOC, NBOR, NBOR0, NBORLOC, NBV1, NBV2, NDUM, NELBOR, NELEM, NELEM2, NELMAX2, NODENRS, NODENRSLOC, NOQ, NOQ2, NOQ2LOC, NOQMAX, NPLAN, NPLANLOC, NPOIN, NPOIN2, NPOIN2LOC, NPOINMAX, NPROC, NPTFR, NPTFR2, NPTFRL, NPTFRLOC, NPTFRMAX, NRESU, NSEG, NSEG2, NSEG2LOC, NSEGMAX, NULONE, NVOIS, ORISEG, PARAM, RDUM, RES, RESPAR, SEGMENT, T3, VERIF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOER(), ELEBD(), EXTENS(), PLANTE(), READ_DATASET(), STOSEG(), VOISIN()
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
      PROGRAM GREDELSEG
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
C
      CHARACTER(LEN=30) GEO
C
      INTEGER IPID,ERR,FU
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC,NOQ2,NPLANLOC,NSEG2LOC,NOQ2LOC
      INTEGER MBNDLOC,NPTFRLOC
      INTEGER NPROC,NRESU,NPOINMAX,NSEGMAX,NOQMAX,NPTFRMAX
      INTEGER I_S, I_SP, I_LEN
      INTEGER IT
      INTEGER IDUM, NPTFR
      INTEGER IELM,NELEM2,NELMAX2,NPTFR2,NSEG2,KLOG,MBND2
      INTEGER MAXNVOIS,ISEG,IG1,IG2,IGTEMP,IVOIS,IL1,IL2
C
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,VERIF,NOQ,NSEG
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: MBND,NODENRS,NPTFRL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG,KSEGLG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NODENRSLOC,NBORLOC
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: LIHBORLOC
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA
C
C
      REAL   , DIMENSION(:)  , ALLOCATABLE :: GLOBAL_VALUE
      REAL   , DIMENSION(:)  , ALLOCATABLE :: LOCAL_VALUE
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE       ! IKLE(SIZIKL,*) OU IKLE(NELMAX,*)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IFABOR     ! IFABOR(NELMAX,*) OU IFABOR(NELMAX2,*)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NVOIS,IADR ! NVOIS(NPOIN),IADR(NPOIN)
C
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NELBOR,LIHBOR      ! NELBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NULONE             ! NULONE(NPTFR,2) OU NULONE(NPTFR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KP1BOR             ! KP1BOR(NPTFR,2) OU KP1BOR(NPTFR)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR               ! NBOR(*)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLBOR             ! IKLBOR(NPTFR,2)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: T3                 ! T3(NPOIN)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR0,LIHBOR0      ! NBOR0(NPTFR),LIHBOR0(NPTFR)
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOSEG         ! GLOSEG(MAXSEG,2)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ELTSEG,ORISEG  ! ELTSEG(NELMAX,*),ORISEG(NELMAX,3)
C
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: GLOSEGLOC
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: SEGMENT
C
      REAL RDUM
C
      LOGICAL IS,ENDE
C
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*11 EXTENS
      CHARACTER*30 CONLIM
      CHARACTER*7  FILETYPE
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL


      LI=5
      LU=6
      LNG=2
CHW
CJAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
C
      WRITE(LU,*) 'I AM GREDELSEG, COUSIN OF GRETEL FROM BAW HAMBURG'
      WRITE(LU,*)
C
C READS FILENAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
C
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
C      REWIND(LI)
      READ(LI,*) GEO
      WRITE(LU,*) GEO
C
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
      WRITE(LU,*) RES
C
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE(LU,*) NPROC
C
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
C     GEOMETRY FILE, READ UNTIL 10 PARAMETERS:
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
C     1) READS THE BEGINNING OF THE FIRST RESULTS FILE
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
      READ(4) FILETYPE
      READ(4) NPOIN2
      READ(4) NSEG2LOC
      READ(4) MBNDLOC
      READ(4) NOQ2LOC
      READ(4) NPLAN
      IF(NPLAN.EQ.1) NPLAN = 0
C
      CLOSE(4)
C
C  5 : 4 PARAMETERS
C
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',  NELEM
      WRITE(LU,*) 'NPOIN2=', NPOIN2
      WRITE(LU,*) 'ECKEN=',  ECKEN
      WRITE(LU,*) 'NDUM=',   NDUM
C
C  DYNAMICALLY ALLOCATES THE ARRAYS
C
      ALLOCATE(NPOIN(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NPOIN')
      ALLOCATE(NOQ(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NOQ')
      ALLOCATE(NSEG(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NSEG')
      ALLOCATE(MBND(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'MBND')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLESA')
      ALLOCATE(NODENRS(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NODENRS')
      ALLOCATE(NPTFRL(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NPTFR2LOC')
C
      ALLOCATE(IFABOR(NELEM,3),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IFABOR')
      ALLOCATE(IKLE(NELEM,3),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLE')
      ALLOCATE(IADR(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IADR')
      ALLOCATE(NVOIS(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NVOIS')
      ALLOCATE(T3(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'T3')
C
C  END OF ALLOCATION ...
C
C  6 : IKLE
C
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
C
C----------------------------------------------------------------------
C
C
      IF(NPLAN.LE.1) THEN
        CONLIM = "T2DCLI"
      ELSE
        CONLIM = "T3DCLI"
      ENDIF
C
      OPEN(4,FILE=CONLIM,FORM='FORMATTED',ERR=996)
      GO TO 997
 996  WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',CONLIM
      CALL PLANTE(-1)
      STOP
 997  CONTINUE
C
      ALLOCATE(LIHBOR0(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LIHBOR')
      ALLOCATE(NBOR0(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NBOR')
      DO I=1,NPOIN2
        READ(4,*,END=989) LIHBOR0(I),IDUM,IDUM,RDUM,RDUM,RDUM,RDUM,
     &                    IDUM,RDUM,RDUM,RDUM,NBOR0(I),IDUM
      ENDDO
C
      CLOSE(4)
 989  NPTFR=I-1
C
      ALLOCATE(LIHBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LIHBOR')
      ALLOCATE(NBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NBOR')
      ALLOCATE(NELBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NELBOR')
      ALLOCATE(NULONE(NPTFR,2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NULONE')
      ALLOCATE(KP1BOR(NPTFR,2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'KP1BOR')
      ALLOCATE(IKLBOR(NPTFR,2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLBOR')
      ALLOCATE(ELTSEG(NELEM,3),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'ELTSEG')
      ALLOCATE(ORISEG(NELEM,3),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'ORISEG')
C
      MBND2=0
C
      DO I=1,NPOIN2
        NODENRS(I) = I
      ENDDO
C
      DO I=1,NPTFR
        NBOR(I)   = NBOR0(I)
        LIHBOR(I) = LIHBOR0(I)
        IF (LIHBOR(I).NE.2) THEN
          MBND2 = MBND2 + 1
          NODENRS(NBOR(I)) = -MBND2
        ENDIF
      ENDDO
C
C------------------------------------------------------------------------------
C
C LOCAL CONSTRUCTION OF GLOSEG
C
C------------------------------------------------------------------------------
C
C     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
C
      IELM = 11 ! WARNING: IS HARD-CODED !!!
        NELEM2  =NELEM
        NELMAX2 =NELEM
        NPTFR2  =NPTFR
C
C     NEIGHBOURS OF THE BOUNDARY SIDES FOR TRIANGULAR MESH
C
        DO J=1,NELEM
          DO I=1,3
            IKLE(J,I)=IKLESA(I,J)
          ENDDO
        ENDDO

      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        CALL VOISIN(IFABOR,NELEM2,NELEM,IELM,IKLE,
     &              NELEM,
     &              NPOIN2,IADR,NVOIS)
        MAXNVOIS = MAXVAL(NVOIS)/2
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF

      KLOG = 2 ! SOLID BOUNDARY CONDITION: IS HARD-CODED !!!

      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
        CALL ELEBD(NELBOR,NULONE,KP1BOR,
     &             IFABOR,NBOR,IKLE,NELEM,
     &             IKLBOR,NELEM2,NELMAX2,
     &             NPOIN2,NPTFR2,IELM,
     &             LIHBOR,KLOG,
     &             IADR,NVOIS,T3)
      ELSE
        WRITE(LU,*) 'UNEXPECTED ELEMENT IN INBIEF:',IELM
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  DATA STRUCTURE FOR EDGE-BASED STORAGE (FROM 5.9 ON ALWAYS DONE IN 2D)
C  SEE CALL TO COMP_SEG BELOW FOR COMPLETING THE STRUCTURE
C
      IF(IELM.EQ.11) THEN
C
         NSEG2 = (3*NELEM+NPTFR)/2
         NOQ2=NPLAN*(NSEG2+MBND2)+(NPLAN-1)*NPOIN2
         IF(NPLAN.EQ.0) THEN
           ALLOCATE(VERIF(NSEG2+MBND2),STAT=ERR)
         ELSE
           ALLOCATE(VERIF(NOQ2) ,STAT=ERR)
         ENDIF
         IF(ERR.NE.0) CALL ALLOER (LU, 'VERIFSEG')
C
C  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
         IF(NPLAN.EQ.0) THEN
           ALLOCATE(GLOBAL_VALUE(NSEG2+MBND2),STAT=ERR)
         ELSE
           ALLOCATE(GLOBAL_VALUE(NOQ2),STAT=ERR)
         ENDIF
         IF(ERR.NE.0) CALL ALLOER (LU, 'GLOBAL_VALUE')
C
         ALLOCATE(GLOSEG(NSEG2,2),STAT=ERR)
         IF(ERR.NE.0) CALL ALLOER (LU, 'GLOSEG')
C
      CALL STOSEG(IFABOR,NELEM,NELMAX2,NELMAX2,IELM,IKLE,
     &            NBOR,NPTFR,
     &            GLOSEG,NSEG2,    ! GLOSEG%MAXDIM1,
     &            ELTSEG,ORISEG,NSEG2,
     &            KP1BOR,NELBOR,NULONE)
      ENDIF
C
      ALLOCATE(SEGMENT(NPOIN2,MAXNVOIS,2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'SEGMENT')
C
C INITIALISES SEGMENT
      DO K=1,2
        DO J=1,MAXNVOIS
          DO I=1,NPOIN2
            SEGMENT(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
      DO ISEG=1,NSEG2
        IG1 = GLOSEG(ISEG,1)
        IG2 = GLOSEG(ISEG,2)
C GLOBAL NUMBERS IN INCREASING ORDER
        IF(IG1.GT.IG2) THEN
          IGTEMP = IG1
          IG1 = IG2
          IG2 = IGTEMP
        ENDIF
        IVOIS=1
        DO WHILE ((SEGMENT(IG1,IVOIS,1).NE.0).AND.(IVOIS.LE.MAXNVOIS))
          IVOIS = IVOIS + 1
        ENDDO
        SEGMENT(IG1,IVOIS,1) = IG2
        SEGMENT(IG1,IVOIS,2) = ISEG
      ENDDO
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
         READ(FU) FILETYPE
         READ(FU) NPOIN(IPID+1)
         READ(FU) NSEG(IPID+1)
         READ(FU) MBND(IPID+1)
         READ(FU) NOQ(IPID+1)
         READ(FU) NPLANLOC
         READ(FU) NPTFRL(IPID+1)
      END DO
C
      NPOINMAX = MAXVAL(NPOIN)
      NSEGMAX = MAXVAL(NSEG)
      NOQMAX = MAXVAL(NOQ)
      NPTFRMAX = MAXVAL(NPTFRL)
C ARRAY FOR LOCAL-GLOBAL NUMBERS, 2D-FIELD
      ALLOCATE (GLOSEGLOC(NSEGMAX,2,NPROC),STAT=ERR)
      IF(NPLAN.EQ.0) THEN
         ALLOCATE (KNOLG(NPOINMAX,NPROC),STAT=ERR)
         ALLOCATE (KSEGLG(NSEGMAX,NPROC),STAT=ERR)
         ALLOCATE (NODENRSLOC(NPOINMAX,NPROC),STAT=ERR)
         ALLOCATE (NBORLOC(NPTFRMAX,NPROC),STAT=ERR)
         ALLOCATE (LIHBORLOC(NPTFRMAX,NPROC),STAT=ERR)
      ELSE
         ALLOCATE (KNOLG(NPOINMAX/NPLAN,NPROC),STAT=ERR)
         ALLOCATE (KSEGLG(NOQMAX,NPROC),STAT=ERR)
         ALLOCATE (NODENRSLOC(NPOINMAX/NPLAN,NPROC),STAT=ERR)
         ALLOCATE (NBORLOC(NPTFRMAX,NPROC),STAT=ERR)
         ALLOCATE (LIHBORLOC(NPTFRMAX,NPROC),STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'KNOLG')
      IF(ERR.NE.0) CALL ALLOER (LU, 'KSEGLG')
      IF(ERR.NE.0) CALL ALLOER (LU, 'NODENRSLOC')
      IF(ERR.NE.0) CALL ALLOER (LU, 'NBORLOC')
C  LOCAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      ALLOCATE(LOCAL_VALUE(NOQMAX),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LOCAL_VALUE')
C
C READS KNOLG(NPOIN,NPROC)
C
C
      IF(NPLAN.EQ.0) THEN
        DO I=1,NSEG2+MBND2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NOQ2
          VERIF(I)=0
        ENDDO
      ENDIF
C
      DO IPID = 0,NPROC-1
         FU = IPID +10
C CHECKS
         IF(NPLAN.EQ.0) THEN
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1))
            READ(FU) ((GLOSEGLOC(I,J,IPID+1),J=1,2),I=1,NSEG(IPID+1))
            READ(FU) (NODENRSLOC(I,IPID+1),I=1,NPOIN(IPID+1))
            READ(FU) (NBORLOC(I,IPID+1),I=1,NPTFRL(IPID+1))
            READ(FU) (LIHBORLOC(I,IPID+1),I=1,NPTFRL(IPID+1))
         ELSE
            READ(FU) (KNOLG(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
            READ(FU) ((GLOSEGLOC(I,J,IPID+1),J=1,2),I=1,NSEG(IPID+1))
            READ(FU) (NODENRSLOC(I,IPID+1),I=1,NPOIN(IPID+1)/NPLAN)
            READ(FU) (NBORLOC(I,IPID+1),I=1,NPTFRL(IPID+1))
            READ(FU) (LIHBORLOC(I,IPID+1),I=1,NPTFRL(IPID+1))
         ENDIF
C
C INITIALISES SEGMENT
C
         DO ISEG=1,NSEG(IPID+1)
           IL1 = GLOSEGLOC(ISEG,1,IPID+1)
           IL2 = GLOSEGLOC(ISEG,2,IPID+1)
           IG1 = KNOLG(IL1,IPID+1)
           IG2 = KNOLG(IL2,IPID+1)
C GLOBAL NUMBER IN INCREASING ORDER
           IF(IG1.GT.IG2) THEN
             IGTEMP = IG1
             IG1 = IG2
             IG2 = IGTEMP
           ENDIF
           IVOIS=1
           DO WHILE ((SEGMENT(IG1,IVOIS,1).NE.IG2)
     &               .AND.(IVOIS.LE.MAXNVOIS))
             IVOIS = IVOIS + 1
           ENDDO
           IF(IVOIS.LE.MAXNVOIS) THEN
             KSEGLG(ISEG,IPID+1) = SEGMENT(IG1,IVOIS,2)
           ENDIF
         ENDDO
C
      END DO
C
C FURTHER VERIFICATIONS
C
C READS DATASETS
C
      NRESU = 0
C
2000  NRESU = NRESU + 1
C
      IF(NPLAN.EQ.0) THEN
        DO I=1,NSEG2+MBND2
          VERIF(I)=0
        ENDDO
      ELSE
        DO I=1,NOQ2
          VERIF(I)=0
        ENDDO
      ENDIF
C
      WRITE(LU,*)'TRY TO READ DATASET NO.',NRESU
C
      IF(NPLAN.EQ.0) THEN
        DO I=1,NSEG2+MBND2
          GLOBAL_VALUE(I) = 0.D0
        ENDDO
      ELSE
        DO I=1,NOQ2
          GLOBAL_VALUE(I) = 0.D0
        ENDDO
      ENDIF
C
      DO IPID = 0,NPROC-1
         FU = IPID +10
C READS LOCAL X INSTEAD OF READ_DATASET
         CALL READ_DATASET
     &   (LOCAL_VALUE,NOQMAX,NOQ(IPID+1),IT,FU,ENDE)
         IF (ENDE) GOTO 3000
C STORES EACH DATASET
         IF(NPLAN.EQ.0) THEN
            NSEG2LOC  = NSEG(IPID+1)
            NPTFRLOC  = NPTFRL(IPID+1)
            DO I=1,NSEG2LOC
              GLOBAL_VALUE(KSEGLG(I,IPID+1)) =
     &        GLOBAL_VALUE(KSEGLG(I,IPID+1)) + LOCAL_VALUE(I)
              VERIF(KSEGLG(I,IPID+1)) =   VERIF(KSEGLG(I,IPID+1))
     &                                     + 1
            END DO
C
           DO I=1,NPTFRLOC
             IF(LIHBORLOC(I,IPID+1).NE.2) THEN
               IF(FILETYPE(1:7).EQ.'SUMAREA') THEN
                 GLOBAL_VALUE(-NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                        + NSEG2) =
     &           LOCAL_VALUE(-NODENRSLOC(NBORLOC(I,IPID+1),IPID+1)
     &                       + NSEG2LOC)
                 VERIF( -NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                 + NSEG2) = 1
               ELSEIF(FILETYPE(1:7).EQ.'SUMFLOW') THEN
                 GLOBAL_VALUE(-NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                        + NSEG2) =
     &           GLOBAL_VALUE(-NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                        + NSEG2) +
     &           LOCAL_VALUE(-NODENRSLOC(NBORLOC(I,IPID+1),IPID+1)
     &                       + NSEG2LOC)
                 VERIF( -NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                 + NSEG2) =
     &           VERIF( -NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                 + NSEG2) + 1
               ELSE
                 WRITE(LU,*) 'CAS NON PREVU'
                 STOP
               ENDIF
             ENDIF
           END DO
C
         ELSE
           NPOIN2LOC = NPOIN(IPID+1)/NPLAN
           NSEG2LOC  = NSEG(IPID+1)
           MBNDLOC   = MBND(IPID+1)
           NPTFRLOC  = NPTFRL(IPID+1)
           DO I=1,NSEG2LOC
             DO J=1,NPLAN
               GLOBAL_VALUE(KSEGLG(I,IPID+1) + (NSEG2+MBND2)*(J-1)) =
     &         GLOBAL_VALUE(KSEGLG(I,IPID+1) + (NSEG2+MBND2)*(J-1)) +
     &         LOCAL_VALUE(       I      + (NSEG2LOC+MBNDLOC)*(J-1))
               VERIF(KSEGLG(I,IPID+1) + (NSEG2+MBND2)*(J-1)) =
     &       + VERIF(KSEGLG(I,IPID+1) + (NSEG2+MBND2)*(J-1)) + 1
             END DO
           END DO
C
           DO I=1,NPTFRLOC
             IF(LIHBORLOC(I,IPID+1).NE.2) THEN
               DO J=1,NPLAN
                 IF(FILETYPE(1:7).EQ.'SUMAREA') THEN
                  GLOBAL_VALUE(-NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                         + NSEG2 + (NSEG2+MBND2)*(J-1)) =
     &             LOCAL_VALUE(-NODENRSLOC(NBORLOC(I,IPID+1),IPID+1)
     &                         + NSEG2LOC + (NSEG2LOC+MBNDLOC)*(J-1))
                   VERIF( -NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                   + NSEG2 + (NSEG2+MBND2)*(J-1)) = 1
                 ELSEIF(FILETYPE(1:7).EQ.'SUMFLOW') THEN
                  GLOBAL_VALUE(-NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                         + NSEG2 + (NSEG2+MBND2)*(J-1)) =
     &            GLOBAL_VALUE(-NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                         + NSEG2 + (NSEG2+MBND2)*(J-1)) +
     &             LOCAL_VALUE(-NODENRSLOC(NBORLOC(I,IPID+1),IPID+1)
     &                         + NSEG2LOC + (NSEG2LOC+MBNDLOC)*(J-1))
                   VERIF( -NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                   + NSEG2 + (NSEG2+MBND2)*(J-1)) =
     &             VERIF( -NODENRS(KNOLG(NBORLOC(I,IPID+1),IPID+1))
     &                   + NSEG2 + (NSEG2+MBND2)*(J-1)) + 1
                 ELSE
                   WRITE(LU,*) 'CAS NON PREVU'
                   STOP
                 ENDIF
               END DO
             ENDIF
           END DO
C
           DO I=1,NPOIN2LOC
             DO J=1,NPLAN-1
               IF(FILETYPE(1:7).EQ.'SUMAREA') THEN
                 GLOBAL_VALUE(  KNOLG(I,IPID+1) + NPOIN2*(J-1)
     &                        + (NSEG2+MBND2)*NPLAN) =
     &         LOCAL_VALUE(I+NPOIN2LOC*(J-1)+(NSEG2LOC+MBNDLOC)*NPLAN)
                 VERIF( KNOLG(I,IPID+1) + NPOIN2*(J-1)
     &               + (NSEG2+MBND2)*NPLAN) = 1
               ELSEIF(FILETYPE(1:7).EQ.'SUMFLOW') THEN
                 GLOBAL_VALUE( KNOLG(I,IPID+1) + NPOIN2*(J-1)
     &                        + (NSEG2+MBND2)*NPLAN) =
     &           GLOBAL_VALUE( KNOLG(I,IPID+1) + NPOIN2*(J-1)
     &                        + (NSEG2+MBND2)*NPLAN) +
     &         LOCAL_VALUE(I+NPOIN2LOC*(J-1)+(NSEG2LOC+MBNDLOC)*NPLAN)
                 VERIF( KNOLG(I,IPID+1) + NPOIN2*(J-1)
     &               + (NSEG2+MBND2)*NPLAN) =
     &           VERIF( KNOLG(I,IPID+1) + NPOIN2*(J-1)
     &               + (NSEG2+MBND2)*NPLAN) + 1
               ELSE
                 WRITE(LU,*) 'CAS NON PREVU'
                 STOP
               ENDIF
             END DO
           END DO
         ENDIF
      END DO
C WRITES GLOBAL DATASET
      WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',IT
C
      IF(NPLAN.EQ.0) THEN
         WRITE(3) IT, (GLOBAL_VALUE(I),I=1,NSEG2+MBND2)
      ELSE
         WRITE(3) IT, (GLOBAL_VALUE(I),I=1,NOQ2)
      ENDIF
C CHECKS ...
      IF(NPLAN.EQ.0) THEN
        DO I=1,NSEG2+MBND2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, SEGMENT I=',I,' FALSE FOR NRESU=',NRESU
          ENDIF
        END DO
      ELSE
        DO I=1,NOQ2
          IF(VERIF(I).EQ.0) THEN
            WRITE(LU,*) 'ERROR, SEGMENT I=',I,' FALSE FOR NRESU=',NRESU
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

      STOP

      END PROGRAM GREDELSEG



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
     &(LOCAL_VALUE,NPOINMAX,NPOIN,IT,FU,ENDE)
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
      INTEGER NPOINMAX,NPOIN,FU
      INTEGER IPOIN
      INTEGER IT
C
      REAL LOCAL_VALUE(NPOINMAX)
C
      LOGICAL ENDE
C
      ENDE = .TRUE.
C
      READ(FU,END=999) IT, (LOCAL_VALUE(IPOIN),IPOIN=1,NPOIN)
C
      ENDE = .FALSE.
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
      CALL EXIT(ICODE)
      STOP    ! WHICH IS USUALLY EQUIVALENT TO CALL EXIT(0)
      END SUBROUTINE PLANTE



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE ARRAY IFABOR, WHERE IFABOR (IELEM, IFACE)
!>                IS THE GLOBAL NUMBER OF THE NEIGHBOUR OF SIDE IFACE
!>                OF ELEMENT IELEM (IF THIS NEIGHBOUR EXISTS) AND 0 IF
!>                THE SIDE IS ON THE DOMAIN BOUNDARY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IADR, IELM, IFABOR, IKLE, NELEM, NELMAX, NPOIN, NVOIS, SIZIKL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR, I, I1, I2, IDIMAT, IELEM, IELEM2, IFACE, IFACE2, IMAX, IV, KEL, M1, M2, MAT1, MAT2, MAT3, NDP, NFACE, SOMFAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELMET(), GREDELSEG(), INBIEF()

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
!>    <td> 5.9                                                     </td>
!>    <td> 19/02/2008                                              </td>
!>    <td> J-M HERVOUET (LNHE) 01 30 87 80 18                      </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IADR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>11: TRIANGLES
!>                  21: QUADRILATERES
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td><--</td><td>TABLEAU DES VOISINS DES FACES.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!>                  (CAS DES MAILLAGES ADAPTATIFS)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU DOMAINE
!>    </td></tr>
!>          <tr><td>NVOIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZIKL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VOISIN
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,SIZIKL,
     & NPOIN,IADR,NVOIS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IADR           |---| 
C| IELM           |-->| 11: TRIANGLES
C|                |   | 21: QUADRILATERES
C| IFABOR         |<--| TABLEAU DES VOISINS DES FACES.
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C|                |   | (CAS DES MAILLAGES ADAPTATIFS)
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE
C| NVOIS          |---| 
C| SIZIKL         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER SIZIKL,NELEM,NELMAX,IELM,NPOIN
      INTEGER IKLE(SIZIKL,*)
      INTEGER IFABOR(NELMAX,*)
      INTEGER NVOIS(NPOIN),IADR(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NFACE,NDP,KEL,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER I,ERR,I1,I2,IDIMAT
C
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C     DYNAMICALLY ALLOCATES THE WORKING ARRAYS
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.21) THEN
C       QUADRILATERALS
        NFACE = 4
C       NUMBER OF POINTS PER ELEMENT
        NDP = 4
C       ADDRESS IN SOMFAC
        KEL = 2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
C       TRIANGLES
        NFACE = 3
C       NUMBER OF POINTS PER ELEMENT
        NDP = 3
C       ADDRESS IN SOMFAC
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     IDIMAT IS BIGGER THAN THE SUM OF THE NUMBER OF NEIGHBOURS OF
C     ALL THE POINTS (NEIGHBOUR = CONNECTED BY A SEGMENT)
C
      IDIMAT = NDP*2*NELEM
C
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'VOISIN : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'VOISIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  ARRAY NVOIS FOR EACH POINT
C  BEWARE : NVOIS IS BIGGER THAN THE ACTUAL NUMBER OF NEIGHBOURS
C           THE SUM OF NVOIS WILL GIVE IDIMAT
C
      DO I=1,NPOIN
        NVOIS(I) = 0
      ENDDO
C
      DO IFACE = 1,NFACE
        DO IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
C  ADDRESSES OF EACH POINT IN A STRUCTURE OF TYPE COMPACT MATRIX
C
C
      IADR(1) = 1
      DO 50 I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
50    CONTINUE
C
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        IF(LNG.EQ.1) WRITE(LU,51) IDIMAT,IMAX
        IF(LNG.EQ.2) WRITE(LU,52) IDIMAT,IMAX
51      FORMAT(1X,'VOISIN: TAILLE DE MAT1,2,3 (',1I9,') INSUFFISANTE',/,
     &         1X,'IL FAUT AU MOINS : ',1I9)
52      FORMAT(1X,'VOISIN: SIZE OF MAT1,2,3 (',1I9,') TOO SHORT',/,
     &         1X,'MINIMUM SIZE: ',1I9)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INITIALISES THE COMPACT MATRIX TO 0
C
      DO I=1,IMAX
        MAT1(I) = 0
      ENDDO
C
C-----------------------------------------------------------------------
C
C  LOOP ON THE SIDES OF EACH ELEMENT:
C
      DO 60 IFACE = 1 , NFACE
      DO 70 IELEM = 1 , NELEM
C
      IFABOR(IELEM,IFACE) = -1
C
C        GLOBAL NODE NUMBERS FOR THE SIDE:
C
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
C        ORDERED GLOBAL NUMBERS:
C
         M1 = MIN0(I1,I2)
         M2 = MAX0(I1,I2)
C
         DO 80 IV = 1,NVOIS(M1)
C
           IF(MAT1(IADR(M1)+IV-1).EQ.0) THEN
              MAT1(IADR(M1)+IV-1)=M2
              MAT2(IADR(M1)+IV-1)=IELEM
              MAT3(IADR(M1)+IV-1)=IFACE
              GO TO 81
           ELSEIF(MAT1(IADR(M1)+IV-1).EQ.M2) THEN
              IELEM2 = MAT2(IADR(M1)+IV-1)
              IFACE2 = MAT3(IADR(M1)+IV-1)
              IFABOR(IELEM,IFACE) = IELEM2
              IFABOR(IELEM2,IFACE2) = IELEM
              GO TO 81
           ENDIF
C
80       CONTINUE
C
         IF(LNG.EQ.1) WRITE(LU,82)
         IF(LNG.EQ.2) WRITE(LU,83)
82       FORMAT(1X,'VOISIN : ERREUR DANS LE MAILLAGE       ',/,1X,
     &             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     &             '         MAYBE SUPERIMPOSED POINTS     ')
         CALL PLANTE(1)
         STOP
C
81       CONTINUE
C
70    CONTINUE
60    CONTINUE
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
C
C-----------------------------------------------------------------------
C
      RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PRISMS SPLIT IN TETRAHEDRONS :<br>
!><br>           1) BUILDS THE ARRAYS NELBOR AND NULONE,
!><br>           2) BUILDS THE ARRAY KP1BOR,
!><br>           3) IFABOR DISTINGUISHES BETWEEN SOLID BOUNDARY,
!>                  SIDES AND LIQUID SIDES,
!><br>           4) COMPUTES IKLBOR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IELM, IFABOR, IKLBOR, IKLE, KLOG, KP1BOR, LIHBOR, NBOR, NELBOR, NELEM, NELMAX, NPOIN, NPTFR, NULONE, SIZIKL, T1, T2, T3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, I1, I2, IEL, IELEM, IFACE, IPOIN, IPT, K, K1, K2, KEL, N1, N2, NFACE, NPT, SOMFAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELMET(), GREDELSEG(), INBIEF()

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
!>    <td> 5.9                                                     </td>
!>    <td> 20/03/2008                                              </td>
!>    <td> J-M HERVOUET (LNHE) 01 30 87 80 18                      </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
!>                  11 : TRIANGLES.
!>                  21 : QUADRILATERES.
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>TABLEAU DES VOISINS DES FACES.
!>    </td></tr>
!>          <tr><td>IKLBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR LA CONDITION LIMITE DE PAROI
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td><--</td><td>NUMERO DU POINT SUIVANT LE POINT DE BORD K.
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>MXELVS
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS AUTOUR D'UN POINT
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td>--></td><td>NOMBRE MAXIMUM DE VOISINS D'UN POINT
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAL DU POINT DE BORD K.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td><--</td><td>NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU DOMAINE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td><--</td><td>NUMERO LOCAL D'UN POINT DE BORD DANS
!>                  L'ELEMENT ADJACENT DONNE PAR NELBOR
!>    </td></tr>
!>          <tr><td>SIZIKL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1,2,3
!></td><td>--></td><td>TABLEAUX DE TRAVAIL ENTIERS.
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ELEBD
     &(NELBOR,NULONE,KP1BOR,IFABOR,NBOR,IKLE,SIZIKL,IKLBOR,NELEM,NELMAX,
     & NPOIN,NPTFR,IELM,LIHBOR,KLOG,T1,T2,T3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT.
C|                |   | 11 : TRIANGLES.
C|                |   | 21 : QUADRILATERES.
C| IFABOR         |-->| TABLEAU DES VOISINS DES FACES.
C| IKLBOR         |---| 
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| KLOG           |-->| CONVENTION POUR LA CONDITION LIMITE DE PAROI
C| KP1BOR         |<--| NUMERO DU POINT SUIVANT LE POINT DE BORD K.
C| LIHBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR H
C| MXELVS         |-->| NOMBRE MAXIMUM D'ELEMENTS AUTOUR D'UN POINT
C| MXPTVS         |-->| NOMBRE MAXIMUM DE VOISINS D'UN POINT
C| NBOR           |-->| NUMERO GLOBAL DU POINT DE BORD K.
C| NELBOR         |<--| NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
C| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |---| 
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NULONE         |<--| NUMERO LOCAL D'UN POINT DE BORD DANS
C|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR
C| SIZIKL         |---| 
C| T1,2,3         |-->| TABLEAUX DE TRAVAIL ENTIERS.
C| T2             |---| 
C| T3             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER KLOG,NELMAX,NELEM,SIZIKL
      INTEGER NPOIN,NPTFR,IELM
      INTEGER NELBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER NULONE(NPTFR,2),KP1BOR(NPTFR,2)
      INTEGER NBOR(NPTFR)
      INTEGER IFABOR(NELMAX,3)
      INTEGER IKLE(SIZIKL,3)
      INTEGER IKLBOR(NPTFR,2)
      INTEGER T1(NPOIN),T2(NPOIN),T3(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,NFACE,NPT,KEL,IPOIN
      INTEGER K,IFACE,I1,I2,N1,N2,IPT,IEL,I,K1,K2
C
      INTEGER SOMFAC(2,4,2)
C
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
C       TRIANGLES
        NFACE = 3
        NPT = 3
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,900) IELM
        IF(LNG.EQ.2) WRITE(LU,901) IELM
900     FORMAT(1X,'ELEBD : IELM=',1I6,' TYPE D''ELEMENT INCONNU')
901     FORMAT(1X,'ELEBD: IELM=',1I6,' UNKNOWN TYPE OF ELEMENT')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C  INITIALISES T1,2,3 TO 0
C
      DO IPOIN=1,NPOIN
        T1(IPOIN) = 0
        T2(IPOIN) = 0
        T3(IPOIN) = 0
      ENDDO
C
C  STORES K IN TRAV(*,3) WITH ADDRESS NBOR(K)
C  ALLOWS TO GO FROM GLOBAL NODE NUMBER TO BOUNDARY NODE NUMBER
C
      DO K = 1, NPTFR
         T3(NBOR(K)) = K
      ENDDO
C
C  LOOP ON ALL THE SIDES OF ALL THE ELEMENTS:
C
      DO 20 IFACE = 1 , NFACE
      DO 10 IELEM = 1 , NELEM
C
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
C
C      THIS IS A TRUE BOUNDARY SIDE (INTERNAL SIDES IN PARALLEL MODE
C                                    ARE INDICATED WITH -2)
C      GLOBAL NODE NUMBERS FOR THE SIDE :
C
       I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
       I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
C      STORES IN T1 AND T2 WITH ADDRESS I1 : I2 AND IELEM
C
       T1(I1) = I2
       T2(I1) = IELEM
C
C      A LIQUID SIDE IS RECOGNIZED BY BOUNDARY CONDITION ON H
C
C      07/02/03 IF(NPTFR...  COURTESY OLIVER GOETHEL, HANNOVER UNIVERSITY
       IF(NPTFR.GT.0) THEN
       IF(LIHBOR(T3(I1)).NE.KLOG.AND.LIHBOR(T3(I2)).NE.KLOG) THEN
C        LIQUID SIDE : IFABOR=0  SOLID SIDE : IFABOR=-1
         IFABOR(IELEM,IFACE)=0
       ENDIF
       ENDIF
C
      ENDIF
C
10    CONTINUE
20    CONTINUE
C
CPARA
C
C NELBOR IS 0 WHEN THE ELEMENT BELONGS TO ANOTHER
C SUB-DOMAIN
C
C
C      ALREADY DONE
C      IF(NCSIZE.GT.1) THEN
C        DO 39 K1=1,NPTFR
C          NELBOR(K)=0
C39      CONTINUE
C      ENDIF
C
CPARAFIN
C
C  LOOP ON ALL THE POINTS:
C
C     07/02/03 IF(NPTFR...  CORRECTION BY OLIVER GOETHELS, HANNOVER
      IF(NPTFR.GT.0) THEN
      DO I = 1 , NPOIN
         IF(T1(I).NE.0) THEN
C          NEXT POINT
           KP1BOR(T3(I),1)=T3(T1(I))
C          PREVIOUS POINT
           KP1BOR(T3(T1(I)),2)=T3(I)
           NELBOR(T3(I))=T2(I)
         ENDIF
      ENDDO
      ENDIF
C
CPARAFIN
C
C COMPUTES NULONE
C
      DO 50 K1=1,NPTFR
C
CPARAFIN
      K2=KP1BOR(K1,1)
      IEL = NELBOR(K1)
      N1  = NBOR(K1)
      N2  = NBOR(K2)
C
      I1 = 0
      I2 = 0
C
      DO 60 IPT=1,NPT
C
        IF(IKLE(IEL,IPT).EQ.N1) THEN
          NULONE(K1,1) = IPT
          I1 = 1
        ENDIF
        IF(IKLE(IEL,IPT).EQ.N2) THEN
          NULONE(K1,2) = IPT
          I2 = 1
        ENDIF
C
60    CONTINUE
C
      IF(I1.EQ.0.OR.I2.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,810) IEL
        IF(LNG.EQ.2) WRITE(LU,811) IEL
810     FORMAT(1X,'ELEBD: ERREUR DE NUMEROTATION DANS L''ELEMENT:',I6,/,
     &         1X,'       CAUSE POSSIBLE :                       '   ,/,
     &         1X,'       LE FICHIER DES CONDITIONS AUX LIMITES NE'  ,/,
     &         1X,'       CORRESPOND PAS AU FICHIER DE GEOMETRIE  ')
811     FORMAT(1X,'ELEBD: ERROR OF NUMBERING IN THE ELEMENT:',I6,
     &         1X,'       POSSIBLE REASON:                       '   ,/,
     &         1X,'       THE BOUNDARY CONDITION FILE IS NOT      '  ,/,
     &         1X,'       RELEVANT TO THE GEOMETRY FILE           ')
        CALL PLANTE(1)
        STOP
      ENDIF
C
50    CONTINUE
C
C  COMPUTES IKLBOR : LIKE IKLE FOR BOUNDARY POINTS, WITH BOUNDARY
C                    NODES NUMBERING
C
      DO K=1,NPTFR
        IKLBOR(K,1) = K
        IKLBOR(K,2) = KP1BOR(K,1)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END



C
C#######################################################################
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, GLOSEG, IELM, IFABOR, IKLE, KP1BOR, MAXSEG, NBOR, NELBOR, NELEM, NELMAX, NELMAX2, NPTFR, NSEG, NULONE, ORISEG
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, IELEM, IELEM1, IELEM2, IFA, IFACE, IG1, IG2, IPTFR, J1, J2, JFACE, NEL, NEXT, NSE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELMET(), GREDELSEG(), INBIEF(), STOSEG41()

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
!>    <td> 5.9                                                     </td>
!>    <td> 02/10/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01 30 87 80 18                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td><--</td><td>SEGMENTS OF EVERY TRIANGLE.
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td><--</td><td>GLOBAL NUMBERS OF POINTS OF SEGMENTS.
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>11: TRIANGLES.
!>                  21: QUADRILATERES.
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td><--</td><td>TABLEAU DES VOISINS DES FACES.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMBER OF POINT FOLLOWING BOUNDARY POINT K
!>                  (I.E. K+1 MOST OF THE TIME BUT NOT ALWAYS).
!>    </td></tr>
!>          <tr><td>MAXSEG
!></td><td><--</td><td>1st DIMENSION OF MAXSEG.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>GLOBAL NUMBERS OF BOUNDARY POINTS.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMBER OF ELEMENT CONTAINING SEGMENT K OF
!>                  THE BOUNDARY.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!>                  (CAS DES MAILLAGES ADAPTATIFS)
!>    </td></tr>
!>          <tr><td>NELMAX2
!></td><td>--></td><td>PREMIERE DIMENSION DE IFABOR
!>                  (EN 3D LE NOMBRE D'ELEMENTS 2D)
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS.
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td><--</td><td>NUMBER OF SEGMENTS OF THE MESH.
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
!>                  ELEMENT.
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td><--</td><td>ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE STOSEG
     &(IFABOR,NELEM,NELMAX,NELMAX2,IELM,IKLE,NBOR,NPTFR,
     & GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG,KP1BOR,NELBOR,NULONE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
C| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
C| IELM           |-->| 11: TRIANGLES.
C|                |   | 21: QUADRILATERES.
C| IFABOR         |<--| TABLEAU DES VOISINS DES FACES.
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
C| KP1BOR         |-->| NUMBER OF POINT FOLLOWING BOUNDARY POINT K
C|                |   | (I.E. K+1 MOST OF THE TIME BUT NOT ALWAYS).
C| MAXSEG         |<--| 1st DIMENSION OF MAXSEG.
C| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS.
C| NELBOR         |-->| NUMBER OF ELEMENT CONTAINING SEGMENT K OF
C|                |   | THE BOUNDARY.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C|                |   | (CAS DES MAILLAGES ADAPTATIFS)
C| NELMAX2        |-->| PREMIERE DIMENSION DE IFABOR
C|                |   | (EN 3D LE NOMBRE D'ELEMENTS 2D)
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS.
C| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
C| NULONE         |-->| LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
C|                |   | ELEMENT.
C| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NELMAX,NELMAX2,NPTFR,NSEG,MAXSEG,IELM,NELEM
      INTEGER NBOR(NPTFR),KP1BOR(NPTFR)
      INTEGER IFABOR(NELMAX2,3),IKLE(NELMAX,3)
      INTEGER NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER GLOSEG(MAXSEG,2)
      INTEGER ELTSEG(NELMAX,3),ORISEG(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPTFR,NSE
C
      INTEGER NEL,IFA,I1,I2,J1,J2,IFACE,JFACE,IG1,IG2
      INTEGER IELEM,IELEM1,IELEM2
C
      INTEGER NEXT(3)
      DATA NEXT / 2,3,1 /
C
C-----------------------------------------------------------------------
C
      IF(IELM.NE.11.AND.IELM.NE.12.AND.IELM.NE.13.AND.IELM.NE.14) THEN
        IF (LNG.EQ.1) WRITE(LU,500) IELM
        IF (LNG.EQ.2) WRITE(LU,501) IELM
500     FORMAT(1X,'STOSEG (BIEF) : ELEMENT NON PREVU : ',1I6)
501     FORMAT(1X,'STOSEG (BIEF) : UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     INITIALISES ELTSEG
C
      DO IELEM = 1 , NELEM
        ELTSEG(IELEM,1) = 0
        ELTSEG(IELEM,2) = 0
        ELTSEG(IELEM,3) = 0
      ENDDO
C
C-----------------------------------------------------------------------
C
C     LOOP ON BOUNDARY NODES :
C
      NSE = 0
      DO IPTFR = 1 , NPTFR
C
C       IN PARALLEL MODE, IF THE BOUNDARY POINT FOLLOWING IPTFR
C       IS IN  ANOTHER SUB-DOMAIN, KP1BOR(IPTFR)=IPTFR
C       IN THIS CASE THE SEGMENT
C       BASED ON IPTFR AND THIS POINT IS NOT IN THE LOCAL DOMAIN
C       A CONSEQUENCE IS THAT NSE IS NOT EQUAL TO IPTFR
C
        IF(KP1BOR(IPTFR).NE.IPTFR) THEN
C
          NSE = NSE + 1
          GLOSEG(NSE,1) = NBOR(IPTFR)
          GLOSEG(NSE,2) = NBOR(KP1BOR(IPTFR))
          NEL = NELBOR(IPTFR)
          IFA = NULONE(IPTFR)
          ELTSEG(NEL,IFA) = NSE
          ORISEG(NEL,IFA) = 1
C
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     LOOP ON ELEMENTS FOR NUMBERING INTERNAL SEGMENTS AND FILLING:
C     GLOSEG, ELTSEG, ORISEG
C
      DO IELEM1 = 1 , NELEM
        DO IFACE = 1 , 3
          IF(ELTSEG(IELEM1,IFACE).EQ.0) THEN
C           NEW SEGMENT (HENCE INTERNAL SO IFABOR0)
            NSE = NSE + 1
C           BOTH NEIGHBOURING ELEMENTS ARE TREATED FOR THIS SEGMENT
            I1 = IKLE(IELEM1,     IFACE)
            I2 = IKLE(IELEM1,NEXT(IFACE))
            IF(I1.EQ.I2) THEN
              IF(LNG.EQ.1) THEN
               WRITE(LU,*) 'STOSEG : SEGMENT AVEC UN SEUL POINT'
               WRITE(LU,*) '         ELEMENT ',IELEM1,' FACE ',IFACE
              ENDIF
              IF(LNG.EQ.2) THEN
               WRITE(LU,*) 'STOSEG: EDGE MADE OF ONLY ONE POINT'
               WRITE(LU,*) '        ELEMENT ',IELEM1,' FACE ',IFACE
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
            ELTSEG(IELEM1,IFACE) = NSE
            IG1=I1
            IG2=I2
C           SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
            IF(IG1.LT.IG2) THEN
              GLOSEG(NSE,1) = I1
              GLOSEG(NSE,2) = I2
              ORISEG(IELEM1,IFACE) = 1
            ELSE
              GLOSEG(NSE,1) = I2
              GLOSEG(NSE,2) = I1
              ORISEG(IELEM1,IFACE) = 2
            ENDIF
C           OTHER ELEMENT NEIGHBOURING THIS SEGMENT
            IELEM2 = IFABOR(IELEM1,IFACE)
C           IELEM2 = 0 OR -1 MAY OCCUR IN PARALLELISM
            IF(IELEM2.GT.0) THEN
C             LOOKS FOR THE RIGHT SIDE OF ELEMENT IELEM2
              DO JFACE = 1,3
                J1 = IKLE(IELEM2,     JFACE)
                J2 = IKLE(IELEM2,NEXT(JFACE))
C               ALL ELEMENTS HAVE A COUNTER-CLOCKWISE NUMBERING
                IF(I1.EQ.J2.AND.I2.EQ.J1) THEN
                  ELTSEG(IELEM2,JFACE) = NSE
                  ORISEG(IELEM2,JFACE) = 3-ORISEG(IELEM1,IFACE)
C                 SIDE FOUND, NO NEED TO GO ON
                  GO TO 1000
                ELSEIF(I1.EQ.J1.AND.I2.EQ.J2) THEN
C                 SIDE BADLY ORIENTED
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'STOSEG : MAILLAGE DEFECTUEUX'
                    WRITE(LU,*) '         LA FACE ',JFACE
                    WRITE(LU,*) '         DE L''ELEMENT ',IELEM2
                    WRITE(LU,*) '         EST MAL ORIENTEE'
                    WRITE(LU,*) '         (POINTS ',I1,' ET ',I2,')'
                  ENDIF
                  IF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'STOSEG: WRONG MESH'
                    WRITE(LU,*) '        FACE ',JFACE
                    WRITE(LU,*) '        OF ELEMENT ',IELEM2
                    WRITE(LU,*) '        IS NOT WELL ORIENTED'
                    WRITE(LU,*) '         (POINTS ',I1,' AND ',I2,')'
                  ENDIF
                  CALL PLANTE(1)
                  STOP
                ENDIF
              ENDDO
C             SIDE NOT FOUND, THIS IS AN ERROR
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'STOSEG : MAILLAGE DEFECTUEUX'
                WRITE(LU,*) '         ELEMENTS ',IELEM1,' ET ',IELEM2
                WRITE(LU,*) '         LIES PAR LES POINTS ',I1,' ET ',I2
                WRITE(LU,*) '         MAIS CES POINTS NE FONT PAS UNE'
                WRITE(LU,*) '         FACE DE L''ELEMENT ',IELEM2
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'STOSEG: WRONG MESH'
                WRITE(LU,*) '        ELEMENTS ',IELEM1,' AND ',IELEM2
                WRITE(LU,*) '        LINKED BY POINTS ',I1,' AND ',I2
                WRITE(LU,*) '        BUT THESE POINTS ARE NOT AN EDGE'
                WRITE(LU,*) '        OF ELEMENT ',IELEM2
              ENDIF
              CALL PLANTE(1)
              STOP
            ENDIF
1000        CONTINUE
          ENDIF
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
C     CHECKS
C
      IF(NSEG.NE.NSE) THEN
        IF (LNG.EQ.1) WRITE(LU,502) NSE,NSEG
        IF (LNG.EQ.2) WRITE(LU,503) NSE,NSEG
502     FORMAT(1X,'STOSEG (BIEF) : MAUVAIS NOMBRE DE SEGMENTS : ',1I6,
     &            '                AU LIEU DE ',1I6,' ATTENDUS')
503     FORMAT(1X,'STOSEG (BIEF): WRONG NUMBER OF SEGMENTS : ',1I6,
     &            '               INSTEAD OF ',1I6,' EXPECTED')
        CALL PLANTE(1)
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