!                    *****************
                     PROGRAM GREDELMET_AUTOP
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
      INTEGER NELEM,ECKEN,NDUM,I,J,K,NBV1,NBV2,PARAM(10)
      INTEGER NPLAN,NPOIN2
      INTEGER NPROC
      INTEGER I_S, I_SP, I_LEN
      INTEGER IDUM, NPTFR
      INTEGER IELM,NELEM2,NELMAX2,NPTFR2,NSEG2,KLOG
      INTEGER MAXNVOIS,ISEG
      INTEGER IELEM,ND1,ND2,ND3,MBND,IFROM,ITO,IFRM1,ITOP1
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NPOIN,IPOBO,NOQ,NSEG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLESA
!
!
      REAL   , DIMENSION(:)  , ALLOCATABLE :: XORIG,YORIG
      REAL   , DIMENSION(:)  , ALLOCATABLE :: AREA
      REAL   , DIMENSION(:,:), ALLOCATABLE :: LENGTH
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLE       ! IKLE(SIZIKL,*) OU IKLE(NELMAX,*)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IFABOR     ! IFABOR(NELMAX,*) OU IFABOR(NELMAX2,*)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NVOIS,IADR ! NVOIS(NPOIN),IADR(NPOIN)
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NELBOR,LIHBOR      ! NELBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NULONE             ! NULONE(NPTFR,2) OU NULONE(NPTFR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: KP1BOR             ! KP1BOR(NPTFR,2) OU KP1BOR(NPTFR)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR               ! NBOR(*)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IKLBOR             ! IKLBOR(NPTFR,2)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: T3                 ! T3(NPOIN)
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NBOR0,LIHBOR0      ! NBOR0(NPTFR),LIHBOR0(NPTFR)
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOSEG         ! GLOSEG(MAXSEG,2)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ELTSEG,ORISEG  ! ELTSEG(NELMAX,*),ORISEG(NELMAX,3)
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: NODENRS
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: IFROM1,ITOPL1
!
      REAL RDUM
      REAL X2,X3,Y2,Y3,SURFACC,DX,DY
!
      LOGICAL IS
!
      CHARACTER*30 RES
      CHARACTER*50 RESPAR
      CHARACTER*11 EXTENS
      CHARACTER*30 CONLIM
      CHARACTER*7  FILETYPE
      EXTERNAL    EXTENS
      INTRINSIC MAXVAL
!
      LI=5
      LU=6
      LNG=2
!HW
!JAJ INTRODUCE YOURSELF WITH THE RELEASE DATE
!
      WRITE(LU,*) 'I AM GREDELMET, COUSIN OF GRETEL FROM BAW HAMBURG'
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
      OPEN(3,FILE=RES,FORM='UNFORMATTED',ERR=991)
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
      OPEN(4,FILE=RESPAR,FORM='UNFORMATTED',ERR=994)
      GO TO 995
994   WRITE(LU,*) 'ERROR WHEN OPENING FILE: ',RESPAR
      CALL PLANTE(-1)
      STOP
995   CONTINUE
!
      READ(4) FILETYPE
      READ(4) NPLAN
      CLOSE(4)
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
!  DYNAMICALLY ALLOCATES THE ARRAYS
!
      ALLOCATE(NPOIN(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NPOIN')
      ALLOCATE(NOQ(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NOQ')
      ALLOCATE(NSEG(NPROC),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NSEG')
      ALLOCATE(IKLESA(3,NELEM),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IKLESA')
      ALLOCATE(IPOBO(NPOIN2)      ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'IPOBO')
!  X AND Y
      ALLOCATE(XORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'XORIG')
      ALLOCATE(YORIG(NPOIN2)    ,STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'YORIG')
!
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
      ALLOCATE(AREA(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'AREA')
      ALLOCATE(NODENRS(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) CALL ALLOER (LU, 'NODENRS')
!
!  END OF ALLOCATION ...
!
!  6 : IKLE
!
      READ(2)  ((IKLESA(I,J),I=1,ECKEN),J=1,NELEM)
!
!  7 : IPOBO
!
      READ(2)  (IPOBO(I),I=1,NPOIN2)
!
!  8 : X AND Y, WILL BE CHECKED LATER ...
!
      READ(2)  (XORIG(I),I=1,NPOIN2)
      READ(2)  (YORIG(I),I=1,NPOIN2)
!
!----------------------------------------------------------------------
!
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
!
      MBND=0
!
      DO I=1,NPOIN2
        NODENRS(I) = I
      ENDDO
!
      DO I=1,NPTFR
        NBOR(I)   = NBOR0(I)
        LIHBOR(I) = LIHBOR0(I)
        IF (LIHBOR(I).NE.2) THEN
          MBND = MBND + 1
          NODENRS(NBOR(I)) = -MBND
        ENDIF
      ENDDO
!
!------------------------------------------------------------------------------
!
! LOCAL CONSTRUCTION OF GLOSEG
!
!------------------------------------------------------------------------------
!
!     WITH PRISMS, DIFFERENT FROM 2D VALUES, OTHERWISE
!
      IELM = 11 ! WARNING: IS HARD-CODED !!!
        NELEM2  =NELEM
        NELMAX2 =NELEM
        NPTFR2  =NPTFR
!
!     NEIGHBOURS OF THE BOUNDARY SIDES FOR TRIANGULAR MESH
!
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
!
!-----------------------------------------------------------------------
!
!  DATA STRUCTURE FOR EDGE-BASED STORAGE (FROM 5.9 ON ALWAYS DONE IN 2D)
!  SEE CALL TO COMP_SEG BELOW FOR COMPLETING THE STRUCTURE
!
      IF(IELM.EQ.11) THEN
!
         NSEG2 = (3*NELEM+NPTFR)/2
         ALLOCATE(LENGTH(2,NSEG2+MBND),STAT=ERR)
         IF(ERR.NE.0) CALL ALLOER (LU, 'LENGTH')
         ALLOCATE(GLOSEG(NSEG2,2),STAT=ERR)
         IF(ERR.NE.0) CALL ALLOER (LU, 'GLOSEG')
         ALLOCATE(IFROM1(NSEG2),STAT=ERR)
         IF(ERR.NE.0) CALL ALLOER (LU, 'IFROM1')
         ALLOCATE(ITOPL1(NSEG2),STAT=ERR)
         IF(ERR.NE.0) CALL ALLOER (LU, 'ITOPL1')
!
      CALL STOSEG(IFABOR,NELEM,NELMAX2,NELMAX2,IELM,IKLE,
     &            NBOR,NPTFR,
     &            GLOSEG,NSEG2,    ! GLOSEG%MAXDIM1,
     &            ELTSEG,ORISEG,NSEG2,
     &            KP1BOR,NELBOR,NULONE)
      ENDIF
!
      IF(FILETYPE(1:6).EQ.'AREA2D') THEN
        DO I=1,NPOIN2
          AREA(I)=0.D0
        ENDDO
        DO IELEM=1,NELEM2
          ND1 = IKLE(IELEM,1)
          ND2 = IKLE(IELEM,2)
          ND3 = IKLE(IELEM,3)
          X2=XORIG(ND2)-XORIG(ND1)
          X3=XORIG(ND3)-XORIG(ND1)
          Y2=YORIG(ND2)-YORIG(ND1)
          Y3=YORIG(ND3)-YORIG(ND1)
          SURFACC=0.5D0*(X2*Y3-X3*Y2)
          AREA(ND1)=AREA(ND1)+SURFACC/3.D0
          AREA(ND2)=AREA(ND2)+SURFACC/3.D0
          AREA(ND3)=AREA(ND3)+SURFACC/3.D0
        ENDDO
      ELSEIF(FILETYPE(1:6).EQ.'LENGTH') THEN
        DO ISEG=1,NSEG2
          DX = XORIG(GLOSEG(ISEG,1)) - XORIG(GLOSEG(ISEG,2))
          DY = YORIG(GLOSEG(ISEG,1)) - YORIG(GLOSEG(ISEG,2))
          LENGTH(1,ISEG) = SQRT(DX**2+DY**2)*0.5D0
          LENGTH(2,ISEG) = LENGTH(1,ISEG)
        ENDDO
        DO I = 1, NPTFR2                    ! LP 05/04/2009
          IF (LIHBOR(I).NE.2 ) THEN         ! OPEN BOUNDARY
            IFROM = NODENRS(NBOR(I))        ! EXCHANGES ADDED
            LENGTH(1,NSEG2-IFROM) = 10.0D0  ! DUMMY LENGTH
            LENGTH(2,NSEG2-IFROM) = 10.0D0
          ENDIF
        ENDDO
      ENDIF
!
      IF(FILETYPE(1:6).EQ.'AREA2D') THEN
        WRITE(3) NPOIN2,0,NPOIN2,NPOIN2,NPOIN2,0
        WRITE(3) (REAL(AREA(I)),I=1,NPOIN2)
      ELSEIF(FILETYPE(1:6).EQ.'LENGTH') THEN
!        WRITE(3) 0
!        DO K=1,NPLAN
!          WRITE(3) ((REAL(LENGTH(I,J)),I=1,2),J=1,NSEG2+MBND)
!        ENDDO
!        DO K=1,NPLAN-1
!          WRITE(3) (1.0, I=1,NPOIN2*2)
!        ENDDO
        WRITE(3) 0,(((REAL(LENGTH(I,J)),I=1,2),J=1,NSEG2+MBND),     ! LP 27/02/2011
     &                K=1,NPLAN), ((1.0,1.0), K=1,(NPLAN-1)*NPOIN2) ! BECAUSE OF
!                                                                   ! UNFORMATTED FILES
!                                                                   ! ALL NOW IN 1 RECORD
      ELSEIF(FILETYPE(1:6).EQ.'IFRMTO') THEN
        DO K=1,NPLAN
          DO ISEG=1,NSEG2
            IFROM = GLOSEG(ISEG,1)
            ITO   = GLOSEG(ISEG,2)
              IF ( K.EQ.1 ) THEN
                CALL FDNRST(IFROM,ITO,XORIG,YORIG,NODENRS,NPOIN2,
     &                                IFROM1(ISEG),ITOPL1(ISEG))
                IF ( IFROM1(ISEG) .LT. 0 .AND.              !  *START*  LP 24/04/2009
     &               IFROM1(ISEG) .NE. NODENRS(IFROM) ) THEN
                  DO I = 1,NPOIN2
                    IF ( NODENRS(I) .EQ. IFROM1(ISEG) ) THEN
                         IFROM1(ISEG) = I
                       EXIT
                    ENDIF
                  ENDDO
                ENDIF
                IF ( ITOPL1(ISEG) .LT. 0 .AND.
     &               ITOPL1(ISEG) .NE. NODENRS(ITO  ) ) THEN
                  DO I = 1,NPOIN2
                    IF ( NODENRS(I) .EQ. ITOPL1(ISEG) ) THEN
                         ITOPL1(ISEG) = I
                       EXIT
                    ENDIF
                  ENDDO
                ENDIF                                       !  **END**  LP 24/04/2009
              ENDIF
              IFRM1 = IFROM1(ISEG)
              ITOP1 = ITOPL1(ISEG)
              IFROM = IFROM + (K-1)*NPOIN2
              IF ( IFRM1 .GT. 0 ) THEN
                IFRM1 = IFRM1 + (K-1)*NPOIN2
              ELSE
                IFRM1 = IFRM1 - (K-1)*MBND                      ! LP 24/04/2009
              ENDIF
              ITO   = ITO   + (K-1)*NPOIN2
              IF ( ITOP1 .GT. 0 ) THEN
                ITOP1 = ITOP1 + (K-1)*NPOIN2
              ELSE
                ITOP1 = ITOP1 - (K-1)*MBND                      ! LP 24/04/2009
              ENDIF
              WRITE(3) IFROM,ITO,IFRM1,ITOP1
            ENDDO
            DO I=1,NPTFR2                                      ! LP 05/04/2009
              IF ( LIHBOR(I) .NE. 2 ) THEN                       ! OPEN BOUNDARY
                 IFROM = NODENRS(NBOR(I))                        ! EXCHANGES ADDED
                 ITO   = NBOR(I)
                 IFRM1 = IFROM
                 ITOP1 = ITO
                 IFROM = IFROM - (K-1)*MBND
                 IFRM1 = IFRM1 - (K-1)*MBND
                 ITO   = ITO   + (K-1)*NPOIN2
                 ITOP1 = ITOP1 + (K-1)*NPOIN2
                 WRITE(3)IFROM,ITO,IFRM1,ITOP1
              ENDIF
            ENDDO
!        THE WRITING OF EXCHANGE POINTERS IS CHANGED       **END**     LP 05/04/2009
         ENDDO
!
!        DERIVE THE FROM-TO EXCHANGE TABLE FOR COMPUTATIONAL ELEMENTS
!        VERTICALLY FOR ALL LAYERS. THE LAYERS DIFFER NPOIN2 IN
!        COMPUTATIONAL ELEMENT NUMBER. BOUNDARY NODES HAVE NO VERTICAL FLOW
!        WRITE 1.0 FOR THE VERTICAL 'FROM' AND 'TO' HALFDISTANCES
!        THEY ARE UPDATED BY WAQ TO BECOME VOLUME/AREA/2.0 DURING
!        SIMULATION TIME, SINCE VERTICAL DISTANCES CHANGE WITH VOLUME.
!
         DO K=1,NPLAN-1
           DO I=1,NPOIN2
!        THE WRITING OF EXCHANGE POINTERS IS CHANGED       *START*     LP 05/04/2009
             IFROM = I
             IFRM1 = IFROM +  MAX(K-2,   0   )*NPOIN2
             ITOP1 = IFROM +  MIN(K+1,NPLAN-1)*NPOIN2
             IFROM = IFROM + (    K-1        )*NPOIN2
             ITO   = IFROM +                      NPOIN2
             WRITE (3) IFROM,ITO,IFRM1,ITOP1
!        THE WRITING OF EXCHANGE POINTERS IS CHANGED       **END**     LP 05/04/2009
           ENDDO
         ENDDO                  ! WAQ COMPUTES THEM ON THE FLY FROM VOLUMES
      ENDIF
!
      WRITE(LU,*) 'END OF PROGRAM '
!
      CLOSE(2)
      CLOSE(3)
!
      STOP
      END PROGRAM GREDELMET_AUTOP
!
!                       ****************************
                        CHARACTER*11 FUNCTION EXTENS
!                       ****************************
     &(N,IPID)
!
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    EXTENSION OF THE FILES ON EACH PROCESSOR.
!+
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
!
!
!                       *****************
                        SUBROUTINE VOISIN
!                       *****************
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,SIZIKL,
     & NPOIN,IADR,NVOIS)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE ARRAY IFABOR, WHERE IFABOR (IELEM, IFACE)
!+                IS THE GLOBAL NUMBER OF THE NEIGHBOUR OF SIDE IFACE
!+                OF ELEMENT IELEM (IF THIS NEIGHBOUR EXISTS) AND 0 IF
!+                THE SIDE IS ON THE DOMAIN BOUNDARY.
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
!+        19/02/2008
!+        V5P9
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IADR           |---|
!| IELM           |-->| 11: TRIANGLES
!|                |   | 21: QUADRILATERES
!| IFABOR         |<--| TABLEAU DES VOISINS DES FACES.
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!|                |   | (CAS DES MAILLAGES ADAPTATIFS)
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE
!| NVOIS          |---|
!| SIZIKL         |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SIZIKL,NELEM,NELMAX,IELM,NPOIN
      INTEGER IKLE(SIZIKL,*)
      INTEGER IFABOR(NELMAX,*)
      INTEGER NVOIS(NPOIN),IADR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NFACE,NDP,KEL,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER I,ERR,I1,I2,IDIMAT
!
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
!
!     DYNAMICALLY ALLOCATES THE WORKING ARRAYS
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.21) THEN
!       QUADRILATERALS
        NFACE = 4
!       NUMBER OF POINTS PER ELEMENT
        NDP = 4
!       ADDRESS IN SOMFAC
        KEL = 2
      ELSEIF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!       TRIANGLES
        NFACE = 3
!       NUMBER OF POINTS PER ELEMENT
        NDP = 3
!       ADDRESS IN SOMFAC
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     IDIMAT IS BIGGER THAN THE SUM OF THE NUMBER OF NEIGHBOURS OF
!     ALL THE POINTS (NEIGHBOUR = CONNECTED BY A SEGMENT)
!
      IDIMAT = NDP*2*NELEM
!
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
!
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
!
!-----------------------------------------------------------------------
!
!  ARRAY NVOIS FOR EACH POINT
!  BEWARE : NVOIS IS BIGGER THAN THE ACTUAL NUMBER OF NEIGHBOURS
!           THE SUM OF NVOIS WILL GIVE IDIMAT
!
      DO I=1,NPOIN
        NVOIS(I) = 0
      ENDDO
!
      DO IFACE = 1,NFACE
        DO IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!  ADDRESSES OF EACH POINT IN A STRUCTURE OF TYPE COMPACT MATRIX
!
!
      IADR(1) = 1
      DO 50 I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
50    CONTINUE
!
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
!
!-----------------------------------------------------------------------
!
!  INITIALISES THE COMPACT MATRIX TO 0
!
      DO I=1,IMAX
        MAT1(I) = 0
      ENDDO
!
!-----------------------------------------------------------------------
!
!  LOOP ON THE SIDES OF EACH ELEMENT:
!
      DO 60 IFACE = 1 , NFACE
      DO 70 IELEM = 1 , NELEM
!
      IFABOR(IELEM,IFACE) = -1
!
!        GLOBAL NODE NUMBERS FOR THE SIDE:
!
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
!        ORDERED GLOBAL NUMBERS:
!
         M1 = MIN0(I1,I2)
         M2 = MAX0(I1,I2)
!
         DO 80 IV = 1,NVOIS(M1)
!
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
!
80       CONTINUE
!
         IF(LNG.EQ.1) WRITE(LU,82)
         IF(LNG.EQ.2) WRITE(LU,83)
82       FORMAT(1X,'VOISIN : ERREUR DANS LE MAILLAGE       ',/,1X,
     &             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     &             '         MAYBE SUPERIMPOSED POINTS     ')
         CALL PLANTE(1)
         STOP
!
81       CONTINUE
!
70    CONTINUE
60    CONTINUE
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!                       ****************
                        SUBROUTINE ELEBD
!                       ****************
     &(NELBOR,NULONE,KP1BOR,IFABOR,NBOR,IKLE,SIZIKL,IKLBOR,NELEM,NELMAX,
     & NPOIN,NPTFR,IELM,LIHBOR,KLOG,T1,T2,T3)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    PRISMS SPLIT IN TETRAHEDRONS:
!>           1) BUILDS THE ARRAYS NELBOR AND NULONE,
!>           2) BUILDS THE ARRAY KP1BOR,
!>           3) IFABOR DISTINGUISHES BETWEEN SOLID BOUNDARY,
!>                  SIDES AND LIQUID SIDES,
!>           4) COMPUTES IKLBOR.
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
!+        20/03/2008
!+        V5P9
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IELM           |-->| TYPE D'ELEMENT.
!|                |   | 11 : TRIANGLES.
!|                |   | 21 : QUADRILATERES.
!| IFABOR         |-->| TABLEAU DES VOISINS DES FACES.
!| IKLBOR         |---|
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!| KLOG           |-->| CONVENTION POUR LA CONDITION LIMITE DE PAROI
!| KP1BOR         |<--| NUMERO DU POINT SUIVANT LE POINT DE BORD K.
!| LIHBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR H
!| MXELVS         |-->| NOMBRE MAXIMUM D'ELEMENTS AUTOUR D'UN POINT
!| MXPTVS         |-->| NOMBRE MAXIMUM DE VOISINS D'UN POINT
!| NBOR           |-->| NUMERO GLOBAL DU POINT DE BORD K.
!| NELBOR         |<--| NUMERO DE L'ELEMENT ADJACENT AU KIEME SEGMENT
!| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |---|
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE.
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
!| NULONE         |<--| NUMERO LOCAL D'UN POINT DE BORD DANS
!|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR
!| SIZIKL         |---|
!| T1,2,3         |-->| TABLEAUX DE TRAVAIL ENTIERS.
!| T2             |---|
!| T3             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER KLOG,NELMAX,NELEM,SIZIKL
      INTEGER NPOIN,NPTFR,IELM
      INTEGER NELBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER NULONE(NPTFR,2),KP1BOR(NPTFR,2)
      INTEGER NBOR(NPTFR)
      INTEGER IFABOR(NELMAX,3)
      INTEGER IKLE(SIZIKL,3)
      INTEGER IKLBOR(NPTFR,2)
      INTEGER T1(NPOIN),T2(NPOIN),T3(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,NFACE,NPT,KEL,IPOIN
      INTEGER K,IFACE,I1,I2,N1,N2,IPT,IEL,I,K1,K2
!
      INTEGER SOMFAC(2,4,2)
!
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11.OR.IELM.EQ.41.OR.IELM.EQ.51) THEN
!       TRIANGLES
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
!
!  INITIALISES T1,2,3 TO 0
!
      DO IPOIN=1,NPOIN
        T1(IPOIN) = 0
        T2(IPOIN) = 0
        T3(IPOIN) = 0
      ENDDO
!
!  STORES K IN TRAV(*,3) WITH ADDRESS NBOR(K)
!  ALLOWS TO GO FROM GLOBAL NODE NUMBER TO BOUNDARY NODE NUMBER
!
      DO K = 1, NPTFR
         T3(NBOR(K)) = K
      ENDDO
!
!  LOOP ON ALL THE SIDES OF ALL THE ELEMENTS:
!
      DO 20 IFACE = 1 , NFACE
      DO 10 IELEM = 1 , NELEM
!
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!
!      THIS IS A TRUE BOUNDARY SIDE (INTERNAL SIDES IN PARALLEL MODE
!                                    ARE INDICATED WITH -2)
!      GLOBAL NODE NUMBERS FOR THE SIDE :
!
       I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
       I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
!      STORES IN T1 AND T2 WITH ADDRESS I1 : I2 AND IELEM
!
       T1(I1) = I2
       T2(I1) = IELEM
!
!      A LIQUID SIDE IS RECOGNIZED BY BOUNDARY CONDITION ON H
!
!      07/02/03 IF(NPTFR...  COURTESY OLIVER GOETHEL, HANNOVER UNIVERSITY
       IF(NPTFR.GT.0) THEN
       IF(LIHBOR(T3(I1)).NE.KLOG.AND.LIHBOR(T3(I2)).NE.KLOG) THEN
!        LIQUID SIDE : IFABOR=0  SOLID SIDE : IFABOR=-1
         IFABOR(IELEM,IFACE)=0
       ENDIF
       ENDIF
!
      ENDIF
!
10    CONTINUE
20    CONTINUE
!
!PARA
!
! NELBOR IS 0 WHEN THE ELEMENT BELONGS TO ANOTHER
! SUB-DOMAIN
!
!
!      ALREADY DONE
!      IF(NCSIZE.GT.1) THEN
!        DO 39 K1=1,NPTFR
!          NELBOR(K)=0
!39      CONTINUE
!      ENDIF
!
!PARAFIN
!
!  LOOP ON ALL THE POINTS:
!
!     07/02/03 IF(NPTFR...  CORRECTION BY OLIVER GOETHELS, HANNOVER
      IF(NPTFR.GT.0) THEN
      DO I = 1 , NPOIN
         IF(T1(I).NE.0) THEN
!          NEXT POINT
           KP1BOR(T3(I),1)=T3(T1(I))
!          PREVIOUS POINT
           KP1BOR(T3(T1(I)),2)=T3(I)
           NELBOR(T3(I))=T2(I)
         ENDIF
      ENDDO
      ENDIF
!
!PARAFIN
!
! COMPUTES NULONE
!
      DO 50 K1=1,NPTFR
!
!PARAFIN
      K2=KP1BOR(K1,1)
      IEL = NELBOR(K1)
      N1  = NBOR(K1)
      N2  = NBOR(K2)
!
      I1 = 0
      I2 = 0
!
      DO 60 IPT=1,NPT
!
        IF(IKLE(IEL,IPT).EQ.N1) THEN
          NULONE(K1,1) = IPT
          I1 = 1
        ENDIF
        IF(IKLE(IEL,IPT).EQ.N2) THEN
          NULONE(K1,2) = IPT
          I2 = 1
        ENDIF
!
60    CONTINUE
!
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
!
50    CONTINUE
!
!  COMPUTES IKLBOR : LIKE IKLE FOR BOUNDARY POINTS, WITH BOUNDARY
!                    NODES NUMBERING
!
      DO K=1,NPTFR
        IKLBOR(K,1) = K
        IKLBOR(K,2) = KP1BOR(K,1)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!                       *****************
                        SUBROUTINE STOSEG
!                       *****************
     &(IFABOR,NELEM,NELMAX,NELMAX2,IELM,IKLE,NBOR,NPTFR,
     & GLOSEG,MAXSEG,ELTSEG,ORISEG,NSEG,KP1BOR,NELBOR,NULONE)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE DATA STRUCTURE FOR EDGE-BASED STORAGE.
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
!+        02/10/2008
!+        V5P9
!+
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTSEG         |<--| SEGMENTS OF EVERY TRIANGLE.
!| GLOSEG         |<--| GLOBAL NUMBERS OF POINTS OF SEGMENTS.
!| IELM           |-->| 11: TRIANGLES.
!|                |   | 21: QUADRILATERES.
!| IFABOR         |<--| TABLEAU DES VOISINS DES FACES.
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT.
!| KP1BOR         |-->| NUMBER OF POINT FOLLOWING BOUNDARY POINT K
!|                |   | (I.E. K+1 MOST OF THE TIME BUT NOT ALWAYS).
!| MAXSEG         |<--| 1st DIMENSION OF MAXSEG.
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS.
!| NELBOR         |-->| NUMBER OF ELEMENT CONTAINING SEGMENT K OF
!|                |   | THE BOUNDARY.
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
!|                |   | (CAS DES MAILLAGES ADAPTATIFS)
!| NELMAX2        |-->| PREMIERE DIMENSION DE IFABOR
!|                |   | (EN 3D LE NOMBRE D'ELEMENTS 2D)
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS.
!| NSEG           |<--| NUMBER OF SEGMENTS OF THE MESH.
!| NULONE         |-->| LOCAL NUMBER OF BOUNDARY POINTS IN A BOUNDARY
!|                |   | ELEMENT.
!| ORISEG         |<--| ORIENTATION OF SEGMENTS OF EVERY TRIANGLE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NELMAX,NELMAX2,NPTFR,NSEG,MAXSEG,IELM,NELEM
      INTEGER NBOR(NPTFR),KP1BOR(NPTFR)
      INTEGER IFABOR(NELMAX2,3),IKLE(NELMAX,3)
      INTEGER NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER GLOSEG(MAXSEG,2)
      INTEGER ELTSEG(NELMAX,3),ORISEG(NELMAX,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR,NSE
!
      INTEGER NEL,IFA,I1,I2,J1,J2,IFACE,JFACE,IG1,IG2
      INTEGER IELEM,IELEM1,IELEM2
!
      INTEGER NEXT(3)
      DATA NEXT / 2,3,1 /
!
!-----------------------------------------------------------------------
!
      IF(IELM.NE.11.AND.IELM.NE.12.AND.IELM.NE.13.AND.IELM.NE.14) THEN
        IF (LNG.EQ.1) WRITE(LU,500) IELM
        IF (LNG.EQ.2) WRITE(LU,501) IELM
500     FORMAT(1X,'STOSEG (BIEF) : ELEMENT NON PREVU : ',1I6)
501     FORMAT(1X,'STOSEG (BIEF) : UNEXPECTED ELEMENT: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     INITIALISES ELTSEG
!
      DO IELEM = 1 , NELEM
        ELTSEG(IELEM,1) = 0
        ELTSEG(IELEM,2) = 0
        ELTSEG(IELEM,3) = 0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP ON BOUNDARY NODES :
!
      NSE = 0
      DO IPTFR = 1 , NPTFR
!
!       IN PARALLEL MODE, IF THE BOUNDARY POINT FOLLOWING IPTFR
!       IS IN  ANOTHER SUB-DOMAIN, KP1BOR(IPTFR)=IPTFR
!       IN THIS CASE THE SEGMENT
!       BASED ON IPTFR AND THIS POINT IS NOT IN THE LOCAL DOMAIN
!       A CONSEQUENCE IS THAT NSE IS NOT EQUAL TO IPTFR
!
        IF(KP1BOR(IPTFR).NE.IPTFR) THEN
!
          NSE = NSE + 1
          GLOSEG(NSE,1) = NBOR(IPTFR)
          GLOSEG(NSE,2) = NBOR(KP1BOR(IPTFR))
          NEL = NELBOR(IPTFR)
          IFA = NULONE(IPTFR)
          ELTSEG(NEL,IFA) = NSE
          ORISEG(NEL,IFA) = 1
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP ON ELEMENTS FOR NUMBERING INTERNAL SEGMENTS AND FILLING:
!     GLOSEG, ELTSEG, ORISEG
!
      DO IELEM1 = 1 , NELEM
        DO IFACE = 1 , 3
          IF(ELTSEG(IELEM1,IFACE).EQ.0) THEN
!           NEW SEGMENT (HENCE INTERNAL SO IFABOR<>0)
            NSE = NSE + 1
!           BOTH NEIGHBOURING ELEMENTS ARE TREATED FOR THIS SEGMENT
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
!           SEGMENT ORIENTED LOWER RANK TO HIGHER RANK
            IF(IG1.LT.IG2) THEN
              GLOSEG(NSE,1) = I1
              GLOSEG(NSE,2) = I2
              ORISEG(IELEM1,IFACE) = 1
            ELSE
              GLOSEG(NSE,1) = I2
              GLOSEG(NSE,2) = I1
              ORISEG(IELEM1,IFACE) = 2
            ENDIF
!           OTHER ELEMENT NEIGHBOURING THIS SEGMENT
            IELEM2 = IFABOR(IELEM1,IFACE)
!           IELEM2 = 0 OR -1 MAY OCCUR IN PARALLEL MODE
            IF(IELEM2.GT.0) THEN
!             LOOKS FOR THE RIGHT SIDE OF ELEMENT IELEM2
              DO JFACE = 1,3
                J1 = IKLE(IELEM2,     JFACE)
                J2 = IKLE(IELEM2,NEXT(JFACE))
!               ALL ELEMENTS HAVE A COUNTER-CLOCKWISE NUMBERING
                IF(I1.EQ.J2.AND.I2.EQ.J1) THEN
                  ELTSEG(IELEM2,JFACE) = NSE
                  ORISEG(IELEM2,JFACE) = 3-ORISEG(IELEM1,IFACE)
!                 SIDE FOUND, NO NEED TO GO ON
                  GO TO 1000
                ELSEIF(I1.EQ.J1.AND.I2.EQ.J2) THEN
!                 SIDE BADLY ORIENTED
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
!             SIDE NOT FOUND, THIS IS AN ERROR
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
!
!-----------------------------------------------------------------------
!
!     CHECKS
!
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!                       *****************
                        SUBROUTINE FDNRST
!                       *****************
     &(IFRM,ITO,X,Y,NODENRS,NPOIN2,IFRM1,ITOP1)
!
!***********************************************************************
! PARALLEL   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FINDS THE NEAREST FROM -1 AND TO +1 POINTER.
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
!history  LEO POSTMA (DELFT HYDRAULICS)
!+        03/04/2007
!+        V5P7
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFRM           |-->|
!| IFRM1          |---|
!| ITO            |-->|
!| ITOP1          |---|
!| NODENRS        |-->| IF > 0 : NODE NUMBER
!|                |   | IF
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
!| X,Y            |-->| NODE COORDINATES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: IFRM,ITO,NPOIN2
      INTEGER, INTENT(IN)          :: NODENRS(NPOIN2)
      INTEGER, INTENT(INOUT)       :: IFRM1,ITOP1
      REAL, INTENT(IN) :: X(NPOIN2), Y(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN
      REAL XFRM1,XTOP1,YFRM1,YTOP1,DISFRM,DISTO,DX,DY
!
!-----------------------------------------------------------------------
!
      XFRM1  = X(IFRM)
      YFRM1  = Y(IFRM)
      XTOP1  = X(ITO )
      YTOP1  = Y(ITO )
      DISFRM = XFRM1-XTOP1
      XFRM1  = XFRM1 + DISFRM
      XTOP1  = XTOP1 - DISFRM
      DISFRM = YFRM1-YTOP1
      YFRM1  = YFRM1 + DISFRM
      YTOP1  = YTOP1 - DISFRM
!
      DX     = XFRM1-X(1)
      DY     = YFRM1-Y(1)
      DISFRM = DX*DX + DY*DY
      DX     = XTOP1-X(1)
      DY     = YTOP1-Y(1)
      DISTO  = DX*DX + DY*DY
      IFRM1  = 1
      ITOP1  = 1
      DO IPOIN = 2, NPOIN2
         DX     = XFRM1-X(IPOIN)
         DY     = YFRM1-Y(IPOIN)
         DX     = DX*DX + DY*DY
         IF(DX.LT.DISFRM) THEN
           DISFRM = DX
           IFRM1  = IPOIN
         ENDIF
         DX     = XTOP1-X(IPOIN)
         DY     = YTOP1-Y(IPOIN)
         DX     = DX*DX + DY*DY
         IF(DX.LT.DISTO) THEN
           DISTO  = DX
           ITOP1  = IPOIN
         ENDIF
      ENDDO
      IFRM1 = NODENRS(IFRM1)
      ITOP1 = NODENRS(ITOP1)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
