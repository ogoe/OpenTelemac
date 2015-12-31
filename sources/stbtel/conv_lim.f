      MODULE CONV_LIM
      CONTAINS
!                       *****************
                        SUBROUTINE READ_LIM
!                       *****************
     &(LIMFILE)
!***********************************************************************
! PROGICIEL : STBTEL  6.0           19/05/11    Y. AUDOUIN
!***********************************************************************
!
!     FONCTION  : READ MED FILE AND FILL MESH_OBJ
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | INFILE         |--> | NAME OF THE BOUNDARY FILE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : CONVERTER
! APPEL DE :
!***********************************************************************
      USE DECLARATIONS_STBTEL
!
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      CHARACTER(LEN=MAXLENHARD),INTENT(IN) :: LIMFILE
!
      INTEGER :: I
      INTEGER :: IERR,IPTFR,IDUM,INBOR,IHBOR,IUBOR,IVBOR
      INTEGER :: VALUES(MAXFAM)
      DOUBLE PRECISION :: DDUM
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1)WRITE(LU,*)
     &        '------DEBUT LECTURE FICHIER DES CONDITIONS LIMITES'
      IF(LNG.EQ.2)WRITE(LU,*) '------BEGINNING READING OF BOUNDARY FILE'
      WRITE(LU,*) '----------------------------------------------------'
!
!-----------------------------------------------------------------------
!
      DDUM=0.0
      IDUM=0
      IF(DEBUG) WRITE(LU,*) 'LIMFILE : ',TRIM(LIMFILE)
      ! READING THE BOUNDARY LIMIT FILE
      OPEN(NLIM,IOSTAT=IERR,FILE=LIMFILE,FORM='FORMATTED')
      CALL FNCT_CHECK(IERR,'OPEN '//TRIM(LIMFILE))
      ! TEST IF NPTFR IS KNOWN (NOT WHEN COMMING FROM MED)
      IF(MESH2%NPTFR.EQ.0) THEN
        IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) 'CALCUL DE NPTFR'
        IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'CALCULING NPTFR'
        DO WHILE(.TRUE.)
        ! COUNT THE NUMBER OF LINE IN THE FILE
        READ(NLIM,*,END=666) IDUM, IDUM,IDUM,
     &             DDUM, DDUM, DDUM,
     &             DDUM,IDUM,DDUM,DDUM,DDUM,
     &             IDUM,IDUM
        MESH2%NPTFR = MESH2%NPTFR + 1
        ENDDO
        ! REWIND OF THE FILE
666     CLOSE(NLIM,IOSTAT=IERR)
        CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(LIMFILE))
        OPEN(NLIM,IOSTAT=IERR,FILE=LIMFILE,FORM='FORMATTED')
        CALL FNCT_CHECK(IERR,'OPEN '//TRIM(LIMFILE))
      ENDIF

      ALLOCATE(MESH2%LIHBOR(MESH2%NPTFR),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE NESH2%LIHBOR')
      ALLOCATE(MESH2%NBOR(MESH2%NPTFR),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%NBOR')
      IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) 'LECTURE DU FICHIER'
      IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'READING BOUNDARY FILE'
      ! BUILDING THE BOUNDARY FAMILIES
      VALUES(:) = 0
      ! WE CONSIDER THAT IF WE READ THE BOUNDARY FILE
      ! WE REMOVE THE FAMILIES IN THE MED FILE
      IF(MESH2%NFAM.NE.0) THEN
        DEALLOCATE(MESH2%IDFAM)
        DEALLOCATE(MESH2%NAMEFAM)
        DEALLOCATE(MESH2%VALFAM)
        DEALLOCATE(MESH2%NGROUPFAM)
        DEALLOCATE(MESH2%GROUPFAM)
        MESH2%NFAM = 0
      ENDIF
      DO I=1,MESH2%NPTFR
        READ(NLIM,*) IHBOR, IUBOR, IVBOR,
     &             DDUM, DDUM, DDUM,
     &             DDUM,IDUM,DDUM,DDUM,DDUM,
     &             INBOR,IPTFR
        IF(IPTFR.NE.I) THEN
          WRITE(LU,*) 'WARNING : THERE IS AN ERROR AT LINE ',I,
     &                'OF THE BOUNDARY FILES I'
          WRITE(LU,*) 'THE LAST COLUMN NUMBER',
     &                ' SHOULD BE ',I,'AND IT IS ',IPTFR
        ENDIF
        MESH2%NBOR(I) = INBOR
        MESH2%LIHBOR(I) = IHBOR*100+IUBOR*10+IVBOR
        IF(COUNT(VALUES.EQ.MESH2%LIHBOR(I)).EQ.0) THEN
          MESH2%NFAM = MESH2%NFAM + 1
          VALUES(MESH2%NFAM) = MESH2%LIHBOR(I)
        ENDIF
      ENDDO
      IF(DEBUG) WRITE(LU,*) 'NFAM :',MESH2%NFAM
      IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) 'CONSTRUCTION DES FAMILLES'
      IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'CONSTRUCTING FAMILIES'
      !FILLING MED VARIABLES
      ALLOCATE(MESH2%IDFAM(MESH2%NFAM),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%IDFAM')
      ALLOCATE(MESH2%NAMEFAM(MESH2%NFAM),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%NAMEFAM')
      ALLOCATE(MESH2%VALFAM(MESH2%NFAM),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%VALFAM')
      ALLOCATE(MESH2%NGROUPFAM(MESH2%NFAM),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%NGROUPEFAM')
      ALLOCATE(MESH2%GROUPFAM(MESH2%NFAM,1),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%GROUPFAM')
      DO I=1,MESH2%NFAM
        MESH2%IDFAM(I) = I
        MESH2%NAMEFAM(I) = 'FAM_CONLIM_'//TRIM(I2CHAR(VALUES(I)))
        MESH2%VALFAM(I) = VALUES(I)
        MESH2%NGROUPFAM(I) = 1
        MESH2%GROUPFAM(I,1) = 'CONLIM_'//TRIM(I2CHAR(VALUES(I)))
        IF(DEBUG) WRITE(LU,*) 'NAME  : ',MESH2%NAMEFAM(I)
        IF(DEBUG) WRITE(LU,*) 'ID    : ',MESH2%IDFAM(I)
        IF(DEBUG) WRITE(LU,*) 'VALUE : ',MESH2%VALFAM(I)
        IF(DEBUG) WRITE(LU,*) 'NGROUP : ',MESH2%NGROUPFAM(I)
        IF(DEBUG) WRITE(LU,*) 'VALUE : ',MESH2%GROUPFAM(I,1)

      ENDDO
      CLOSE(NLIM,IOSTAT=IERR)
      CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(LIMFILE))
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------FIN LECTURE DU FICHIER DES ',
     &              'CONDITIONS LIMITES'
      IF(LNG.EQ.2) WRITE(LU,*) '------ENDING READING OF BOUNDARY FILE'
      WRITE(LU,*) '----------------------------------------------------'
      END SUBROUTINE

!                       *****************
                        SUBROUTINE WRITE_LIM
!                       *****************
     &(LIMFILE)
!***********************************************************************
! PROGICIEL : STBTEL  6.0           19/05/11    Y. AUDOUIN
!***********************************************************************
!
!     FONCTION  : READ MED FILE AND FILL MESH_OBJ
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | INFILE         |--> | COORDONNEES DES POINTS DU MAILLAGE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : CONVERTER
! APPEL DE :
!***********************************************************************
      USE DECLARATIONS_STBTEL
!
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      CHARACTER(LEN=MAXLENHARD),INTENT(IN) :: LIMFILE
!
      INTEGER :: IDUM,I,IERR,IHBOR,IUBOR,IVBOR
      DOUBLE PRECISION :: DDUM
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*)
     &             '------BEGINNING WRITTING OF BOUNDARY FILE'
      IF(LNG.EQ.2) WRITE(LU,*)
     &           '------DEBUT LECTURE DU FICHIER DES CONDITIONS LIMITES'
      WRITE(LU,*) '----------------------------------------------------'
!
!-----------------------------------------------------------------------
!
      OPEN(NBND,IOSTAT=IERR,FILE=LIMFILE,STATUS='NEW',FORM='FORMATTED')
      CALL FNCT_CHECK(IERR,'OPEN '//TRIM(LIMFILE))
      IDUM = 0
      DDUM = 0.0
      DO I=1,MESH2%NPTFR
        IHBOR = MESH2%LIHBOR(I)/100
        IUBOR = (MESH2%LIHBOR(I) - 100*IHBOR)/10
        IVBOR = MESH2%LIHBOR(I) - 100*IHBOR - 10*IUBOR
        WRITE(NBND,*)
     &              IHBOR, IUBOR, IVBOR,
     &              DDUM, DDUM, DDUM,
     &              DDUM, IHBOR, DDUM, DDUM, DDUM,
     &              MESH2%NBOR(I), I
      ENDDO
      CLOSE(NLIM,IOSTAT=IERR)
      CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(LIMFILE))
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*)
     &    '------FIN LECTURE DU FICHIER DES CONDITIONS LIMITES'
      IF(LNG.EQ.2) WRITE(LU,*) '------ENDING WRITTING OF BOUNDARY FILE'
      WRITE(LU,*) '----------------------------------------------------'
      END SUBROUTINE
      END MODULE
