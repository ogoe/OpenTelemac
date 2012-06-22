!                    ***********************
                     PROGRAM GREDELHYD_AUTOP
!                    ***********************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION (COUPLING
!+                WITH DELWAQ) TO WRITE A SINGLE FILE IN DELWAQ FORMAT.
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
      COMMON/INFO/LNG,LU
      INTEGER LI
!
      CHARACTER(LEN=30) GEO
!
      INTEGER ERR,NELEM,ECKEN,NDUM,I,J,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2,NPROC,I_S,I_SP,I_LEN,IDUM,NPTFR,NSEG2,MBND
      INTEGER IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEC
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: LIHBOR             ! LIHBOR(NPTFR)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR               ! NBOR(*)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR0,LIHBOR0      ! NBOR0(NPTFR),LIHBOR0(NPTFR)
!
      REAL RDUM
      REAL,    DIMENSION(:)  , ALLOCATABLE :: F
!
      DOUBLE PRECISION REFER_DAY,JULIAN_DAY
      DOUBLE PRECISION GREDELHYD_JULTIM
      EXTERNAL         GREDELHYD_JULTIM
!
      LOGICAL IS
!
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*11 GREDELHYD_EXTENS
      CHARACTER*30 CONLIM
      EXTERNAL    GREDELHYD_EXTENS
      INTRINSIC MAXVAL
!
      INTEGER ITSTRT,ITSTOP,ITSTEP,NSTEPA
      INTEGER MARDAT(3),MARTIM(3)
      CHARACTER*144 TITRE
      CHARACTER*144 NOMGEO,NOMLIM
      CHARACTER*144 NOMSOU,NOMMAB,NOMCOU,NOMSAL,NOMTEM
      CHARACTER*144 NOMINI,NOMVEB,NOMMAF,NOMVEL,NOMVIS
      LOGICAL SALI_DEL,TEMP_DEL
      LOGICAL VELO_DEL,DIFF_DEL
!
      LI=5
      LU=6
      LNG=2
!HW
!JAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
!
      WRITE(LU,*) 'I AM GREDELHYD, COUSIN OF GRETEL FROM BAW HAMBURG'
      WRITE(LU,*)
!
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
!      REWIND(LI)
      READ(LI,*) GEO
      WRITE(LU,*) GEO
!
! READS FILENAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
      WRITE (LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
      WRITE(LU,*) RES
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE(LU,*) NPROC
      INQUIRE (FILE=GEO,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL GREDELHYD_PLANTE (-1)
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
!     GEOMETRY FILE, READ UNTIL 10 PARAMETERS:
!
      OPEN(2,FILE=GEO,FORM='UNFORMATTED',STATUS='OLD',ERR=990)
      READ(2,ERR=990)
      READ(2,ERR=990) NBV1,NBV2
      DO 10 I=1,NBV1+NBV2
        READ(2,ERR=990)
10    CONTINUE
      GO TO 992
990   WRITE(LU,*) 'ERROR WHEN OPENING OR READING FILE: ',GEO
      CALL GREDELHYD_PLANTE(-1)
      STOP
992   CONTINUE
!     READS THE 10 PARAMETERS AND THE DATE
      READ(2) (PARAM(I),I=1,10)
      IF(PARAM(10).EQ.1) READ(2) (PARAM(I),I=1,6)
!
!     RESULTS FILE:
!
      OPEN(3,FILE=RES,FORM='FORMATTED',ERR=991)
      GO TO 993
991   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RES
      CALL GREDELHYD_PLANTE(-1)
      STOP
993   CONTINUE
!
!     1) READS THE BEGINNING OF THE FIRST RESULTS FILE
!
!CC      RESPAR=RES // GREDELHYD_EXTENS(2**IDIMS-1,0)
!
      RESPAR=RES(1:I_LEN) // GREDELHYD_EXTENS(NPROC-1,0)
!
      INQUIRE (FILE=RESPAR,EXIST=IS)
      IF (.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', RESPAR
        WRITE (LU,*) 'CHECK THE NUMBER OF PROCESSORS'
        WRITE (LU,*) 'AND THE RESULT FILE CORE NAME'
        CALL GREDELHYD_PLANTE(-1)
        STOP
      END IF
!
      OPEN(4,FILE=RESPAR,FORM='FORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL GREDELHYD_PLANTE(-1)
      STOP
995   CONTINUE
!
      READ(4,'(I6)')NPLAN
      CLOSE(4)
!
      ALLOCATE(F(NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL GREDELHYD_ALLOER (LU, 'F')
!
!  5 : 4 PARAMETERS
!
      READ(2) NELEM,NPOIN2,ECKEN,NDUM
      WRITE(LU,*) '4 PARAMETERS IN GEOMETRY FILE'
      WRITE(LU,*) 'NELEM=',  NELEM
      WRITE(LU,*) 'NPOIN2=', NPOIN2
      WRITE(LU,*) 'ECKEN=',  ECKEN
      WRITE(LU,*) 'NDUM=',   NDUM
!
!----------------------------------------------------------------------
!
      IF(NPLAN.LE.1) THEN
        CONLIM = "T2DCLI"
      ELSE
        CONLIM = "T3DCLI"
      ENDIF
!
      OPEN(4,FILE=CONLIM,FORM='FORMATTED',ERR=996)
      GO TO 997
 996  WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',CONLIM
      CALL GREDELHYD_PLANTE(-1)
      STOP
 997  CONTINUE
!
      ALLOCATE(LIHBOR0(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL GREDELHYD_ALLOER (LU, 'LIHBOR')
      ALLOCATE(NBOR0(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL GREDELHYD_ALLOER (LU, 'NBOR')
      DO I=1,NPOIN2
        READ(4,*,END=989) LIHBOR0(I),IDUM,IDUM,RDUM,RDUM,RDUM,RDUM,
     &                    IDUM,RDUM,RDUM,RDUM,NBOR0(I),IDUM
      ENDDO
!
      CLOSE(4)
 989  NPTFR=I-1
!
      ALLOCATE(LIHBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL GREDELHYD_ALLOER (LU, 'LIHBOR')
      ALLOCATE(NBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL GREDELHYD_ALLOER (LU, 'NBOR')
!
      MBND=0
!
      DO I=1,NPTFR
        NBOR(I)   = NBOR0(I)
        LIHBOR(I) = LIHBOR0(I)
        IF (LIHBOR(I).NE.2) THEN
          MBND = MBND + 1
        ENDIF
      ENDDO
!
!     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
!
      NSEG2 = (3*NELEM+NPTFR)/2
!
!
      OPEN(4,FILE=RESPAR,FORM='FORMATTED',ERR=984)
      GO TO 985
984   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL GREDELHYD_PLANTE(-1)
      STOP
985   CONTINUE
!
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
!
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
!      J = LEN_TRIM(TITRE)
      IF ( J .GT. 40 ) THEN
         WRITE (3, '(A,A,A)' ) "   '",TITRE(1:40),"'"
         IF ( J .GT. 80 ) THEN
            WRITE (3, '(A,A,A)' ) "   '",TITRE(41:80),"'"
            IF ( J .GT. 120 ) THEN
               WRITE (3, '(A,A,A)' ) "   '",TITRE(81:120),"'"
            ELSE
               WRITE (3, '(A,A,A)' ) "   '",TITRE(81:J),"'"
            ENDIF
         ELSE
            WRITE (3, '(A,A,A)' ) "   '",TITRE(41:J),"'"
            WRITE (3, '(A)' )
     &    "   '                                    '            "
         ENDIF
      ELSE
         WRITE (3, '(A,A,A)' ) "   '",TITRE(1:J),"'"
         WRITE (3, '(A)' )
     &    "   '                                    '            "
         WRITE (3, '(A)' )
     &    "   '                                    '            "
      ENDIF
!      WRITE(3, '(A,A,A)' )
!     &    "   '",TITRE(1:J),"'"
!      WRITE(3, '(A)' )
!     &    "   '                                    '            "
!      WRITE(3, '(A)' )
!     &    "   '                                    '            "
      WRITE(3, '(A)' )
     &    "end-description                                      "
      WRITE(3, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &"reference-time           '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                             MARTIM(1),MARTIM(2),MARTIM(3),"'"
      REFER_DAY  = GREDELHYD_JULTIM(MARDAT(1),MARDAT(2),MARDAT(3),
     &                              MARTIM(1),MARTIM(2),MARTIM(3),0.D0)
      JULIAN_DAY = REFER_DAY + DBLE(ITSTRT)/(86400.D0*36525.D0)
      CALL GREDELHYD_GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY,
     &                                    IHOUR, IMIN,   ISEC )
      WRITE(3, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "hydrodynamic-start-time  '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
      JULIAN_DAY = REFER_DAY + DBLE(ITSTOP)/(86400.D0*36525.D0)
      CALL GREDELHYD_GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY,
     &                                    IHOUR, IMIN,   ISEC )
      WRITE(3, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "hydrodynamic-stop-time   '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
      WRITE(3, '(A,I14,A)' )
     &    "hydrodynamic-timestep    '",NSTEPA,"'"
      WRITE(3, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "conversion-ref-time      '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                                 MARTIM(1),MARTIM(2),MARTIM(3),"'"
      JULIAN_DAY = REFER_DAY + DBLE(ITSTRT)/(86400.D0*36525.D0)
      CALL GREDELHYD_GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY,
     &                                    IHOUR, IMIN,   ISEC )
      WRITE(3, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "conversion-start-time    '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
      JULIAN_DAY = REFER_DAY + DBLE(ITSTOP)/(86400.D0*36525.D0)
      CALL GREDELHYD_GREGTIM( JULIAN_DAY, IYEAR, IMONTH, IDAY,
     &                                    IHOUR, IMIN,   ISEC )
      WRITE(3, '(A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)' )
     &    "conversion-stop-time     '",IYEAR,IMONTH,IDAY,
     &                                 IHOUR,IMIN  ,ISEC, "'"
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
      READ(4,'(A)') NOMSOU(1:J)
      I = J
      DO WHILE((NOMSOU(I:I).NE.'/').AND.(NOMSOU(I:I).NE.'\')
     &                             .AND.(I.GE.1))
        I = I-1
      ENDDO
      WRITE(3, '(A,A,A)' )
     &    "volumes-file             '",NOMSOU(I+1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)') NOMMAB(1:J)
      I = J
      DO WHILE((NOMMAB(I:I).NE.'/').AND.(NOMMAB(I:I).NE.'\')
     &                             .AND.(I.GE.1))
        I = I-1
      ENDDO
      WRITE(3, '(A,A,A)' )
     &    "areas-file               '",NOMMAB(I+1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)') NOMCOU(1:J)
      I = J
      DO WHILE((NOMCOU(I:I).NE.'/').AND.(NOMCOU(I:I).NE.'\')
     &                             .AND.(I.GE.1))
        I = I-1
      ENDDO
      WRITE(3, '(A,A,A)' )
     &    "flows-file               '",NOMCOU(I+1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)') NOMVEB(1:J)
      I = J
      DO WHILE((NOMVEB(I:I).NE.'/').AND.(NOMVEB(I:I).NE.'\')
     &                             .AND.(I.GE.1))
        I = I-1
      ENDDO
      WRITE(3, '(A,A,A)' )
     &    "pointers-file            '",NOMVEB(I+1:J),"'"
      READ(4,'(I3)')J
      READ(4,'(A)')NOMMAF(1:J)
      I = J
      DO WHILE((NOMMAF(I:I).NE.'/').AND.(NOMMAF(I:I).NE.'\')
     &                             .AND.(I.GE.1))
        I = I-1
      ENDDO
      WRITE(3, '(A,A,A)' )
     &    "lengths-file             '",NOMMAF(I+1:J),"'"
      READ(4,'(L1)') SALI_DEL
      IF(SALI_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)') NOMSAL(1:J)
        I = J
        DO WHILE((NOMSAL(I:I).NE.'/').AND.(NOMSAL(I:I).NE.'\')
     &                               .AND.(I.GE.1))
          I = I-1
        ENDDO
        WRITE(3, '(A,A,A)' )
     &    "salinity-file            '",NOMSAL(I+1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "salinity-file            none                        "
      ENDIF
      READ(4,'(L1)') TEMP_DEL
      IF(TEMP_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)') NOMTEM(1:J)
        I = J
        DO WHILE((NOMTEM(I:I).NE.'/').AND.(NOMTEM(I:I).NE.'\')
     &                               .AND.(I.GE.1))
          I = I-1
        ENDDO
        WRITE(3, '(A,A,A)' )
     &    "temperature-file         '",NOMTEM(I+1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "temperature-file         none                        "
      ENDIF
      READ(4,'(L1)') DIFF_DEL
      IF(DIFF_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)') NOMVIS(1:J)
        I = J
        DO WHILE((NOMVIS(I:I).NE.'/').AND.(NOMVIS(I:I).NE.'\')
     &                               .AND.(I.GE.1))
          I = I-1
        ENDDO
        WRITE(3, '(A,A,A)' )
     &    "vert-diffusion-file      '",NOMVIS(I+1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "vert-diffusion-file      none                        "
      ENDIF
      READ(4,'(L1)') VELO_DEL
      IF(VELO_DEL) THEN
        READ(4,'(I3)')J
        READ(4,'(A)') NOMVEL(1:J)
        I = J
        DO WHILE((NOMVEL(I:I).NE.'/').AND.(NOMVEL(I:I).NE.'\')
     &                               .AND.(I.GE.1))
          I = I-1
        ENDDO
        WRITE(3, '(A,A,A)' )
     &    "velocity-file            '",NOMVEL(I+1:J),"'"
      ELSE
        WRITE(3, '(A)' )
     &    "velocity-file            none                        "
      ENDIF
      READ(4,'(I3)')J
      READ(4,'(A)') NOMINI(1:J)
      I = J
      DO WHILE((NOMINI(I:I).NE.'/').AND.(NOMINI(I:I).NE.'\')
     &                             .AND.(I.GE.1))
        I = I-1
      ENDDO
      WRITE(3, '(A,A,A)' )
     &    "surfaces-file            '",NOMINI(I+1:J),"'"
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
!
      WRITE(LU,*) 'END OF PROGRAM '
!
      CLOSE(2)
      CLOSE(3)
      CLOSE(4)
!
      STOP
      END PROGRAM GREDELHYD_AUTOP
!
!
!                       ************************************
                        CHARACTER*11 FUNCTION GREDELHYD_EXTENS
!                       ************************************
     &(N,IPID)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief       GREDELHYD_EXTENSION OF THE FILES ON EACH PROCESSOR.
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
        GREDELHYD_EXTENS='00000-00000'
!
        IF(N.LT.10) THEN
          WRITE(GREDELHYD_EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(GREDELHYD_EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(GREDELHYD_EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(GREDELHYD_EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(GREDELHYD_EXTENS(01:05),'(I5)') N
        ENDIF
!
        IF(IPID.LT.10) THEN
          WRITE(GREDELHYD_EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(GREDELHYD_EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(GREDELHYD_EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(GREDELHYD_EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(GREDELHYD_EXTENS(07:11),'(I5)') IPID
        ENDIF
!
      ELSE
!
        GREDELHYD_EXTENS='       '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!
!     *************************************
      SUBROUTINE GREDELHYD_ALLOER (N, CHFILE)
!     *************************************
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
!| CHFILE         |---|
!| N              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: N
      CHARACTER*(*), INTENT(IN) :: CHFILE
      WRITE(N,*) 'ERROR BY ALLOCATION OF ',CHFILE
      CALL GREDELHYD_PLANTE(-1)
      STOP
      END SUBROUTINE GREDELHYD_ALLOER
!
!
!     *******************************
      SUBROUTINE GREDELHYD_PLANTE(IVAL)
!     *******************************
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
      STOP 0   !WHICH IS USUALLY EQUIVALENT TO CALL EXIT(0)

!     JMH 30/09/2011 WHAT IS THIS (NAG COMPILER DOES NOT KNOW)
!     CALL EXIT(ICODE)
      END SUBROUTINE GREDELHYD_PLANTE
!
!               ******************************************
                DOUBLE PRECISION FUNCTION GREDELHYD_JULTIM
!               ******************************************
!
     &(YEAR,MONTH,DAY,HOUR,MIN,SEC,AT)
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TIME ELAPSED SINCE 31/12/1899.
!+                EXPRESSES IT IN JULIAN CENTURIES.
!
!history  E. DAVID (LHF)
!+        12/07/1995
!+        V5P1
!+
!
!history  JMH (EDF-LNHE)
!+        03/09/2010
!+        V6P0
!+   For consistency, YEAR is now INTENT(IN) only
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
!| AT             |-->| TIME IN SECONDS
!| DAY            |-->| DAY
!| HOUR           |-->| HOUR IN UNIVERSAL TIME
!| MIN            |-->| MINUTE IN UNIVERSAL TIME
!| MONTH          |-->| MONTH
!| SEC            |-->| SECOND IN UNIVERSAL TIME
!| YEAR           |-->| YEAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: MONTH,DAY,HOUR,MIN,SEC,YEAR
      DOUBLE PRECISION, INTENT(IN) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER GREG,Y,M,YEAR2
      DOUBLE PRECISION J
!
      INTRINSIC INT
!
      PARAMETER (GREG=15+31*(10+12*1582))
!
!-----------------------------------------------------------------------
!
      YEAR2=YEAR
      IF(YEAR2.EQ.0) THEN
        IF (LNG.EQ.1) WRITE (LU,100)
        IF(LNG.EQ.2)  WRITE (LU,101)
        STOP
      ENDIF
      IF(YEAR2.LT.0) YEAR2=YEAR2+1
!
      IF (MONTH.GT.2) THEN
       Y=YEAR2
       M=MONTH+1
      ELSE
       Y=YEAR2-1
       M=MONTH+13
      ENDIF
!
      J=INT(365.25D0*Y)+INT(30.6001D0*M)+DAY+1720995.D0
      IF(DAY+31*(MONTH+12*YEAR2).GE.GREG) THEN
        J=J+2-INT(0.01D0*Y)+INT(0.25D0*INT(0.01D0*Y))
      ENDIF
      J=J-2415020.5D0
      GREDELHYD_JULTIM = (J+(HOUR+(MIN+(SEC+AT)/60.D0)/60.D0)/24.D0)
     &                   / 36525.D0
!
!---------------------------------------------------------------
!
100   FORMAT (//,10X,'**********************************',
     &         /,10X,'       FONCTION JULTIM',
     &         /,10X,' LA VALEUR DE L''ANNEE EST NULLE',
     &         /,10X,' CALCUL IMPOSSIBLE ...'
     &         /,10X,'**********************************')
101   FORMAT (//,10X,'**********************************',
     &         /,10X,'       JULTIM FUNCTION',
     &         /,10X,' THE VALUE FOR THE YEAR IS ZERO',
     &         /,10X,' COMPUTATION NOT POSSIBLE ...'
     &         /,10X,'**********************************')
!
!---------------------------------------------------------------
!
      RETURN
      END
!
!                       ****************************
                        SUBROUTINE GREDELHYD_GREGTIM
!                       ****************************
     &(JULTIM,YEAR,MONTH,DAY,HOUR,MIN,SEC)
!
!***********************************************************************
! PARALLEL   V6P2                                   31/08/2011
!***********************************************************************
!
!brief    COMPUTES THE GREGORIAN CALENDAR DATE
!+        (YEAR,MONTH,DAY,HOUR,MIN,SEC)
!+        GIVEN THE JULIAN DATE (JD) IN CENTURY
!
!history  C.-T. PHAM (EDF-LNHE)
!+        31/08/2011
!+        V6P2
!+        FROM http://aa.usno.navy.mil/faq/docs/JD_Formula.php :
!+        GDATE ALGORITHM
!+        ORIGINAL ARTICLE: FLIEGEL AND VAN FLANDERN (1968),
!+        A MACHINE ALGORITHM FOR PROCESSING CALENDAR DATES
!+        FOR YEAR, MONTH AND DAY;
!+        AND FROM DELTARES, INITIALLY WL, SECTOR WATERBEHEER & MILIEU
!+        PROJET T0467 OR T1234.56, ANDRE HENDRIKS, V 1.01 (930429)
!+        FOR HOUR,MIN,SEC
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DAY            |<->| DAY    (1-28, 29, 30 OR 31)
!| HOUR           |<->| HOUR   (0-23) IN UNIVERSAL TIME
!| JULTIM         |-->| JULIAN DAY IN CENTURY
!| MIN            |<->| MINUTE (0-59) IN UNIVERSAL TIME
!| MONTH          |<->| MONTH  (1-12)
!| SEC            |<->| SECOND (0-59) IN UNIVERSAL TIME
!| YEAR           |<->| YEAR   (-4713-..)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(INOUT) :: YEAR,MONTH,DAY,HOUR,MIN,SEC
      DOUBLE PRECISION, INTENT(IN)    :: JULTIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,L,N
      DOUBLE PRECISION JD,JDR
!
      INTRINSIC INT
!
!-----------------------------------------------------------------------
!
!  JULTIM UNIT: CENTURY
!  JD UNIT    : DAY
!
!  2415020 <=> 31/12/1899: DUE TO THE SHIFT IN JULTIM IN TELEMAC/BIEF
!
      JD=JULTIM*36525.D0+2415020.D0
!
      JDR=MOD(JD,1.D0)
!
      IF (JDR.LT.0.5D0) THEN
        JDR = JDR+0.5D0
      ELSE
        JDR = JDR-0.5D0
        JD  = JD+1.D0
      ENDIF
!
      L = INT(JD)+68569
      N = 4*L/146097
      L = L-(146097*N+3)/4
      I = 4000*(L+1)/1461001
      L = L-1461*I/4+31
      J = 80*L/2447
      K = L-2447*J/80
      L = J/11
      J = J+2-12*L
      I = 100*(N-49)+I+L
!
      YEAR  = I
      MONTH = J
      DAY   = K
!
      HOUR = INT(JDR*24.D0)
      MIN  = INT(JDR*1440.D0)-60*HOUR
      SEC  = NINT(JDR*86400.D0)-3600*HOUR-60*MIN
!
!  TO AVOID SEC = 60
!
      IF(SEC.EQ.60) THEN
        SEC = 0
        MIN = MIN + 1
      ENDIF
!
      IF(MIN.GE.60) THEN
        MIN  = MIN  - 60
        HOUR = HOUR + 1
      ENDIF
!
      IF(HOUR.GE.24) THEN
        HOUR = HOUR - 24
        DAY  = DAY  + 1
      ENDIF
!
      RETURN
      END
