!                    *****************
                     PROGRAM GREDELHYD_AUTOP
!                    *****************
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
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
      INTEGER ERR
      INTEGER NELEM,ECKEN,NDUM,I,J,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2
      INTEGER NPROC
      INTEGER I_S, I_SP, I_LEN
      INTEGER IDUM, NPTFR,NSEG2,MBND
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: LIHBOR             ! LIHBOR(NPTFR)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR               ! NBOR(*)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR0,LIHBOR0      ! NBOR0(NPTFR),LIHBOR0(NPTFR)
!
      REAL RDUM
      REAL,    DIMENSION(:)  , ALLOCATABLE :: F
!
      LOGICAL IS
!
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*11 EXTENS
      CHARACTER*30 CONLIM
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL
!
      INTEGER ITSTRT,ITSTOP,ITSTEP,NSTEPA
      INTEGER MARDAT(3),MARTIM(3)
      CHARACTER*72  TITRE
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
      CALL PLANTE(-1)
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
      CALL PLANTE(-1)
      STOP
993   CONTINUE
!
!     1) READS THE BEGINNING OF THE FIRST RESULTS FILE
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
      OPEN(4,FILE=RESPAR,FORM='FORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(-1)
      STOP
995   CONTINUE
!
      READ(4,'(I6)')NPLAN
      CLOSE(4)
!
      ALLOCATE(F(NPLAN),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'F')
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
      CALL PLANTE(-1)
      STOP
 997  CONTINUE
!
      ALLOCATE(LIHBOR0(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LIHBOR')
      ALLOCATE(NBOR0(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NBOR')
      DO I=1,NPOIN2
        READ(4,*,END=989) LIHBOR0(I),IDUM,IDUM,RDUM,RDUM,RDUM,RDUM,
     &                    IDUM,RDUM,RDUM,RDUM,NBOR0(I),IDUM
      ENDDO
!
      CLOSE(4)
 989  NPTFR=I-1
!
      ALLOCATE(LIHBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'LIHBOR')
      ALLOCATE(NBOR(NPTFR),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NBOR')
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
      CALL PLANTE(-1)
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
!| N              |---|
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
