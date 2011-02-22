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
!>    </th><td> ECKEN, ENDE, ERR, FU, GEO, GLOBAL_VALUE, I, IPID, IS, IT, I_LEN, I_S, I_SP, J, KNOLG, LI, LOCAL_VALUE, NBV1, NBV2, NDUM, NELEM, NPLAN, NPLANLOC, NPOIN, NPOIN2, NPOIN2LOC, NPOINMAX, NPROC, NRESU, PARAM, RES, RESPAR, VERIF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOER(), EXTENS(), PLANTE(), READ_DATASET()
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
      PROGRAM GREDELPTS
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
      INTEGER NELEM,ECKEN,NDUM,I,J,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPOIN2LOC,NPLANLOC
      INTEGER NPROC,NRESU,NPOINMAX
      INTEGER I_S, I_SP, I_LEN
      INTEGER IT
C
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,VERIF
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KNOLG
C
C
      REAL   , DIMENSION(:)  , ALLOCATABLE :: GLOBAL_VALUE
      REAL   , DIMENSION(:)  , ALLOCATABLE :: LOCAL_VALUE
C
      LOGICAL IS,ENDE
C
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*11 EXTENS
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL


C-------------------------------------------------------------------------
C
      LI=5
      LU=6
      LNG=2
CHW
CJAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
C
      WRITE(LU,*) 'I AM GREDELPTS, COUSIN OF GRETEL FROM BAW HAMBURG'
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
      READ(4) NPOIN2
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
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(VERIF(NPOIN2)    ,STAT=ERR)
      ELSE
        ALLOCATE(VERIF(NPOIN2*NPLAN)    ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'VERIF')
C  GLOBAL_VALUES, STORES THE WHOLE DATASET (NBV1-VALUES)
      IF(NPLAN.EQ.0) THEN
        ALLOCATE(GLOBAL_VALUE(NPOIN2)       ,STAT=ERR)
      ELSE
        ALLOCATE(GLOBAL_VALUE(NPOIN2*NPLAN) ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) CALL ALLOER (LU, 'GLOBAL_VALUE')
C
C  END OF ALLOCATION ...
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
         READ(FU) NPOIN(IPID+1)
         READ(FU) NPLANLOC
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
      ALLOCATE(LOCAL_VALUE(NPOINMAX),STAT=ERR)
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
     &   (LOCAL_VALUE,NPOINMAX,NPOIN(IPID+1),IT,FU,ENDE)
         IF (ENDE) GOTO 3000
C STORES EACH DATASET
         IF(NPLAN.EQ.0) THEN
            DO I=1,NPOIN(IPID+1)
              GLOBAL_VALUE(KNOLG(I,IPID+1)) = LOCAL_VALUE(I)
              VERIF(KNOLG(I,IPID+1))   = 1
            END DO
         ELSE
            NPOIN2LOC = NPOIN(IPID+1)/NPLAN
            DO I=1,NPOIN2LOC
            DO J=1,NPLAN
            GLOBAL_VALUE(KNOLG(I,IPID+1) + NPOIN2   *(J-1)) =
     &       LOCAL_VALUE(      I         + NPOIN2LOC*(J-1))
                   VERIF(KNOLG(I,IPID+1) + NPOIN2   *(J-1)) = 1
            END DO
            END DO
         ENDIF
      END DO
C WRITES GLOBAL DATASET
      WRITE(LU,*)'WRITING DATASET NO.',NRESU,' TIME =',IT
C
      IF(NPLAN.EQ.0) THEN
         WRITE(3) IT, (GLOBAL_VALUE(I),I=1,NPOIN2)
      ELSE
         WRITE(3) IT, (GLOBAL_VALUE(I),I=1,NPOIN2*NPLAN)
      ENDIF
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
      STOP

      END PROGRAM GREDELPTS



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