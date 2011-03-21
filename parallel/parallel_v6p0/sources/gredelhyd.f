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
!>    </th><td> CONLIM, DIFF_DEL, ECKEN, ERR, F, GEO, I, IDUM, IS, ITSTEP, ITSTOP, ITSTRT, I_LEN, I_S, I_SP, J, LI, LIHBOR, LIHBOR0, MARDAT, MARTIM, MBND, NBOR, NBOR0, NBV1, NBV2, NDUM, NELEM, NOMCOU, NOMGEO, NOMINI, NOMLIM, NOMMAB, NOMMAF, NOMSAL, NOMSOU, NOMTEM, NOMVEB, NOMVEL, NOMVIS, NPLAN, NPOIN2, NPROC, NPTFR, NSEG2, NSTEPA, PARAM, RDUM, RES, RESPAR, SALI_DEL, TEMP_DEL, TITRE, VELO_DEL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLOER(), EXTENS(), PLANTE()
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
      PROGRAM GREDELHYD
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER LI
C
      CHARACTER(LEN=30) GEO
C
      INTEGER ERR
      INTEGER NELEM,ECKEN,NDUM,I,J,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2
      INTEGER NPROC
      INTEGER I_S, I_SP, I_LEN
      INTEGER IDUM, NPTFR,NSEG2,MBND
C
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: LIHBOR             ! LIHBOR(NPTFR)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR               ! NBOR(*)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR0,LIHBOR0      ! NBOR0(NPTFR),LIHBOR0(NPTFR)
C
      REAL RDUM
      REAL,    DIMENSION(:)  , ALLOCATABLE :: F
C
      LOGICAL IS
C
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*11 EXTENS
      CHARACTER*30 CONLIM
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL
C
      INTEGER ITSTRT,ITSTOP,ITSTEP,NSTEPA
      INTEGER MARDAT(3),MARTIM(3)
      CHARACTER*72  TITRE
      CHARACTER*144 NOMGEO,NOMLIM
      CHARACTER*144 NOMSOU,NOMMAB,NOMCOU,NOMSAL,NOMTEM
      CHARACTER*144 NOMINI,NOMVEB,NOMMAF,NOMVEL,NOMVIS
      LOGICAL SALI_DEL,TEMP_DEL
      LOGICAL VELO_DEL,DIFF_DEL
C
      LI=5
      LU=6
      LNG=2
CHW
CJAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
C
      WRITE(LU,*) 'I AM GREDELHYD, COUSIN OF GRETEL FROM BAW HAMBURG'
      WRITE(LU,*)
C
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
C      REWIND(LI)
      READ(LI,*) GEO
      WRITE(LU,*) GEO
C
C READS FILENAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
C
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
      WRITE(LU,*) RES
C
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE(LU,*) NPROC

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
      OPEN(3,FILE=RES,FORM='FORMATTED',ERR=991)
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
      OPEN(4,FILE=RESPAR,FORM='FORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(-1)
      STOP
995   CONTINUE
C
      READ(4,'(I6)')NPLAN
      CLOSE(4)
C
      ALLOCATE(F(NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'F')
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
C----------------------------------------------------------------------
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
C
      MBND=0
C
      DO I=1,NPTFR
        NBOR(I)   = NBOR0(I)
        LIHBOR(I) = LIHBOR0(I)
        IF (LIHBOR(I).NE.2) THEN
          MBND = MBND + 1
        ENDIF
      ENDDO
C
C     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
C
      NSEG2 = (3*NELEM+NPTFR)/2
C
C
      OPEN(4,FILE=RESPAR,FORM='FORMATTED',ERR=984)
      GO TO 985
984   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(-1)
      STOP
985   CONTINUE
C
      READ(4,'(I6)')NPLAN
      READ(4,'(I3)')J
      READ(4,'(A)')TITRE(1:J)
      READ(4,'(I4)')MARDAT(1)
      READ(4,'(I2)')MARDAT(2)
      READ(4,'(I2)')MARDAT(3)
      READ(4,'(I2)')MARTIM(1)
      READ(4,'(I2)')MARTIM(2)
      READ(4,'(I2)')MARTIM(3)
      READ(4,'(I14)')ITSTRT
      READ(4,'(I14)')ITSTOP
      READ(4,'(I14)')NSTEPA
      READ(4,'(I6)')NPLAN
C
      WRITE(3, '(A)' )
     &    "task      full-coupling                              "
      WRITE(3, '(A)' )
     &    "                                                     "
      WRITE(3, '(A)' )
     &    "#                                                    "
      WRITE(3, '(A)' )
     &    "# telemac data                                       "
      WRITE(3, '(A)' )
     &    "#                                                    "
      WRITE(3, '(A)' )
     &    "                                                     "
      WRITE(3, '(A)' )
     &    "geometry  finite-elements                            "
      WRITE(3, '(A)' )
     &    "                                                     "
      WRITE(3, '(A)' )
     &    "horizontal-aggregation       no                      "
      WRITE(3, '(A)' )
     &    "minimum-vert-diffusion-used  no                      "
      WRITE(3, '(A)' )
     &    "vertical-diffusion           calculated              "
      WRITE(3, '(A)' )
     &    "description                                          "
      WRITE(3, '(A,A,A)' )
     &    "   '",TITRE(1:J),"'"
      WRITE(3, '(A)' )
     &    "   '                                    '            "
      WRITE(3, '(A)' )
     &    "   '                                    '            "
      WRITE(3, '(A)' )
     &    "end-description                                      "
      WRITE(3, '(A,I4,I2,I2,I2,I2,I2,A)' )
     &"reference-time           '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                             MARTIM(1),MARTIM(2),MARTIM(3),"'"
      WRITE(3, '(A,I14,A)' )
     &    "hydrodynamic-start-time  '",ITSTRT,"'"
      WRITE(3, '(A,I14,A)' )
     &    "hydrodynamic-stop-time   '",ITSTOP,"'"
      WRITE(3, '(A,I14,A)' )
     &    "hydrodynamic-timestep    '",NSTEPA,"'"
      WRITE(3, '(A,I14,A)' )
     &    "conversion-ref-time      '",ITSTRT,"'"
      WRITE(3, '(A,I14,A)' )
     &    "conversion-start-time    '",ITSTRT,"'"
      WRITE(3, '(A,I14,A)' )
     &    "conversion-stop-time     '",ITSTOP,"'"
      WRITE(3, '(A,I14,A)' )
     &    "conversion-timestep      '",NSTEPA,"'"
      WRITE(3, '(A,I6)'  )
     &    "grid-cells-first-direction ",NPOIN2
      WRITE(3, '(A,I6,A)')
     &    "grid-cells-second-direction",NSEG2+MBND," # nr of exchanges!"
      WRITE(3, '(A,I6)' )
     &    "number-hydrodynamic-layers ",NPLAN
      WRITE(3, '(A,I6)' )
     &    "number-water-quality-layers",NPLAN
      READ(4,'(I3)')J
      READ(4,'(A)')NOMGEO(1:J)
      WRITE(3, '(A,A,A)' )
     &    "hydrodynamic-file        '",NOMGEO(1:J),"'"
      WRITE(3, '(A)' )
     &    "aggregation-file         none                        "
      WRITE(3, '(A,A,A)' )
     &    "grid-indices-file        '",NOMGEO(1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMLIM(1:J)
      WRITE(3, '(A,A,A)' )
     &    "grid-coordinates-file    '",NOMLIM(1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMSOU(1:J)
      WRITE(3, '(A,A,A)' )
     &    "volumes-file             '",NOMSOU(1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMMAB(1:J)
      WRITE(3, '(A,A,A)' )
     &    "areas-file               '",NOMMAB(1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMCOU(1:J)
      WRITE(3, '(A,A,A)' )
     &    "flows-file               '",NOMCOU(1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMVEB(1:J)
      WRITE(3, '(A,A,A)' )
     &    "pointers-file            '",NOMVEB(1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMMAF(1:J)
      WRITE(3, '(A,A,A)' )
     &    "lengths-file             '",NOMMAF(1:J),"'"
      READ(4,'(L)')SALI_DEL
      IF(SALI_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)')NOMSAL(1:J)
        WRITE(3, '(A,A,A)' )
     &    "salinity-file            '",NOMSAL(1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "salinity-file            none                        "
      ENDIF
      READ(4,'(L)')TEMP_DEL
      IF(TEMP_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)')NOMTEM(1:J)
        WRITE(3, '(A,A,A)' )
     &    "temperature-file         '",NOMTEM(1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "temperature-file         none                        "
      ENDIF
      READ(4,'(L)')DIFF_DEL
      IF(DIFF_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)')NOMVIS(1:J)
        WRITE(3, '(A,A,A)' )
     &    "vert-diffusion-file      '",NOMVIS(1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "vert-diffusion-file      none                        "
      ENDIF
      READ(4,'(L)')VELO_DEL
      IF(VELO_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)')NOMVEL(1:J)
        WRITE(3, '(A,A,A)' )
     &    "velocity-file            '",NOMVEL(1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "velocity-file            none                        "
      ENDIF
      READ(4,'(I3)')J
      READ(4,'(A)')NOMINI(1:J)
      WRITE(3, '(A,A,A)' )
     &    "surfaces-file            '",NOMINI(1:J),"'"
!
      WRITE(3, '(A)' )
     &    "total-grid-file          none                        "
      WRITE(3, '(A)' )
     &    "discharges-file          none                        "
      WRITE(3, '(A)' )
     &    "chezy-coefficients-file  none                        "
      WRITE(3, '(A)' )
     &    "shear-stresses-file      none                        "
      WRITE(3, '(A)' )
     &    "walking-discharges-file  none                        "
      IF ( NPLAN .GT. 1 ) THEN
         WRITE(3, '(A)' )
     &       "minimum-vert-diffusion                            "
         WRITE(3, '(A)' )
     &       "   upper-layer       0.0000E+00                   "
         WRITE(3, '(A)' )
     &       "   lower-layer       0.0000E+00                   "
         WRITE(3, '(A)' )
     &       "   interface-depth   0.0000E+00                   "
         WRITE(3, '(A)' )
     &       "end-minimum-vert-diffusion                        "
      ENDIF
      WRITE(3, '(A)' )
     &    "constant-dispersion                                  "
      WRITE(3, '(A)' )
     &    "   first-direction    0.0000                         "
      WRITE(3, '(A)' )
     &    "   second-direction   0.0000                         "
      WRITE(3, '(A)' )
     &    "   third-direction    0.0000                         "
      WRITE(3, '(A)' )
     &    "end-constant-dispersion                              "
      WRITE(3, '(A)' )
     &    "hydrodynamic-layers                               "
      DO I=1,NPLAN
        READ(4,'(F10.4)')F(I)
      ENDDO
      DO I=1,NPLAN
         WRITE(3, '(F10.4)' ) F(I)
      ENDDO
      WRITE(3, '(A)' )
     &    "end-hydrodynamic-layers                           "
      WRITE(3, '(A)' )
     &    "water-quality-layers                              "
      DO I=1,NPLAN
         WRITE(3, '(F10.4)' ) 1.0
      ENDDO
      WRITE(3, '(A)' )
     &    "end-water-quality-layers                          "
      WRITE(3, '(A)' )
     &    "discharges                                           "
      WRITE(3, '(A)' )
     &    "end-discharges                                       "
C
      WRITE(LU,*) 'END OF PROGRAM '
C
      CLOSE(2)
      CLOSE(3)
      CLOSE(4)
C
      STOP

      END PROGRAM GREDELHYD



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
      IF (IVAL < 0) THEN      ! THIS INDICATES A CONTROLLED ERROR
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