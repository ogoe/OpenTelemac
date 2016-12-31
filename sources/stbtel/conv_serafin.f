      MODULE CONV_SERAFIN
      CONTAINS
!                       ***********************
                        SUBROUTINE READ_SERAFIN
!                       ***********************
!
     &(SLFFILE,LIMFILE,SERAFIND)
!
!***********************************************************************
! STBTEL   V6P1                                   11/07/2011
!***********************************************************************
!
!BRIEF    READS A FILE OF SERAFIN FORMAT AND FILL THE MESH OBJECT
!
!HISTORY  Y.AUDOUIN (EDF)
!+        11/07/2011
!+        V6P1
!+   CREATION OF THE FILE
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SLFFILE        |-->| NAME OF THE SERAFIN FILE IN THE TEMPORARY FOLDER
!| LIMFILE        |-->| NAME OF THE BOUNDARY FILE IN THE TEMPORARY FOLDER
!| SERAFIND       |-->| TRUE IF SERAFIN IS IN DOUBLE PRECISION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_STBTEL
      USE CONV_LIM
      USE INTERFACE_HERMES
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: SLFFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LIMFILE
      LOGICAL, INTENT(IN)                   :: SERAFIND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I,J,K,IERR,IDUM
      DOUBLE PRECISION, ALLOCATABLE :: TMP(:)
      CHARACTER(LEN=8) :: FFORMAT
      CHARACTER(LEN=80) :: TITLE
      INTEGER :: TYP_BND_ELEM,NELEBD
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------DEBUT LECTURE DU FICHIER SERAFIN'
      IF(LNG.EQ.2) WRITE(LU,*) '------BEGINNING READING OF SERAFIN FILE'
      WRITE(LU,*) '----------------------------------------------------'
!
!-----------------------------------------------------------------------
!
      ! IF THE OPTION SERAFIND IS TRUE WE READ IN DOUBLE
      ! PRECISION SINGLE OTHERWISE
      IF(SERAFIND) THEN
        WRITE(LU,*) 'DOUBLE PRECISION'
        FFORMAT='SERAFIND'
      ELSE
        FFORMAT='SERAFIN '
      ENDIF
      CALL OPEN_MESH(FFORMAT, SLFFILE, NINP, 'READ     ', ierr)
      CALL FNCT_CHECK(IERR,'OPEN '//TRIM(SLFFILE))
      ! READING NAME OF THE MESH
      CALL GET_MESH_TITLE(FFORMAT, NINP, MESH2%TITLE, IERR)
      ! SET THE DESCRIPTION  TO NO DESCRIPTION
      MESH2%DESCRIPTION = 'NO DESCRIPTION'//CHAR(0)
      ! GET THE NUMBER OF VARIABLES
      CALL GET_DATA_NVAR(FFORMAT, NINP, MESH2%NVAR, IERR)
!
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR LES VARIABLES'
      IF(LNG.EQ.2) WRITE(LU,*) '---VARIABLES INFORMATIONS'
      IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) 'NOMBRE DE VARIABLES :',
     &               MESH2%NVAR
      IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'NUMBER OF VARIABLES :',
     &               MESH2%NVAR
!
      IF(MESH2%NVAR.NE.0) THEN
        ALLOCATE(MESH2%NAMEVAR(MESH2%NVAR),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%NAMEVAR')
        ALLOCATE(MESH2%UNITVAR(MESH2%NVAR),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%UNITVAR')
        ! GET THE NAME AND UNIT OF EACH VARIABLE
        CALL GET_DATA_VAR_LIST(FFORMAT, NINP, MESH2%NVAR,
     &                         MESH2%NAMEVAR, MESH2%UNITVAR, IERR)
        DO I=1,MESH2%NVAR
          IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) 'NOM DE LA VARIABLE : ',
     &                  MESH2%NAMEVAR(I)
          IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*)'UNITE DE LA VARIABLE : ',
     &                  MESH2%UNITVAR(I)
          IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'VARIABLE NAME: ',
     &                  MESH2%NAMEVAR(I)
          IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'VARIABLE UNIT: ',
     &                  MESH2%UNITVAR(I)
        ENDDO
      ENDIF
!
      ! READING THE NUMBER ELEMENTS, POINT, ...
      CALL READ_MESH_INFO(FFORMAT,NINP,TITLE,IDUM,MESH2%NPOIN,
     &                    MESH2%TYPE_ELEM, MESH2%NELEM, MESH2%NPTFR,
     &                    MESH2%IB(9), MESH2%NDP, MESH2%IB(7),
     &                    TYP_BND_ELEM,NELEBD)
      MESH2%IB(8) = MESH2%NPTFR
      ! IF WE ARE IN 3D
      IF(MESH2%IB(7).GT.1) THEN
        MESH2%NDIM=3
      ELSE
        MESH2%NDIM=2
      ENDIF
      IF(MESH2%TYPE_ELEM.LT.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TYPE D ELEMENT INCONNU'
          WRITE(LU,*) 'NOMBRE DE POINT PAR ELEMENT :',MESH2%NDP
          WRITE(LU,*) 'DIMENSION DU MAILLAGE :',MESH2%NDIM
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'UNKNOWN ELEMENT TYPE'
          WRITE(LU,*) 'NUMBER OF POINT PER ELEMENT:',MESH2%NDP
          WRITE(LU,*) 'DIMENSION OF THE MESH:',MESH2%NDIM
        ENDIF
        CALL PLANTE(-1)
      ENDIF
!
      ! READING IPOBO AND IKLES
      ALLOCATE(MESH2%IKLES(MESH2%NELEM*MESH2%NDP),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%IKLES')
      ALLOCATE(MESH2%IPOBO(MESH2%NPOIN),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH%IPOBO')
!
      CALL READ_MESH_CONN(FFORMAT,NINP,MESH2%NPOIN, MESH2%TYPE_ELEM,
     &                    MESH2%NELEM, MESH2%NDP,
     &                    TYP_BND_ELEM, NELEBD,
     &                    MESH2%IKLES, MESH2%IPOBO)
      IF( (MESH2%IB(8).NE.0) .OR. (MESH2%IB(9).NE.0) ) THEN
        ALLOCATE(MESH2%KNOLG(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%KNOLG')
        CALL GET_MESH_L2G_NUMBERING(FFORMAT,NINP,MESH2%KNOLG,
     &                              MESH2%NPOIN,IERR)
      ENDIF
      IF(DEBUG) WRITE(LU,*) 'NPTFR:',MESH2%NPTFR
      IF(DEBUG) WRITE(LU,*) 'NDIM:',MESH2%NDIM
      IF(DEBUG) WRITE(LU,*) 'NDP:',MESH2%NDP
      IF(DEBUG) WRITE(LU,*) 'IB:',MESH2%IB
!
      ! READING COORDINATES AND KNOLG IF IN PARALLEL
      ALLOCATE(MESH2%X(MESH2%NPOIN),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%X')
      ALLOCATE(MESH2%Y(MESH2%NPOIN),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%Y')
      IF(MESH2%NDIM.EQ.3) THEN
        ALLOCATE(MESH2%Z(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%Z')
      ENDIF
!
      !READING THE COORDINATES

!     PROJECTION
      IDUM=1
!
      CALL READ_MESH_COORD(FFORMAT,NINP,MESH2%X,MESH2%Y,MESH2%NPOIN,
     &              IDUM,0.D0,0.D0)

!
      ! COMPLETING COORDINATES NAME AND UNIT
      ALLOCATE(MESH2%NAMECOO(MESH2%NDIM),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%NAMECOO')
      ALLOCATE(MESH2%UNITCOO(MESH2%NDIM),STAT=IERR)
      CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%UNITCOO')
      MESH2%NAMECOO(1) = 'X'
      MESH2%UNITCOO(1) = 'M'
      MESH2%NAMECOO(2) = 'Y'
      MESH2%UNITCOO(2) = 'M'
      IF(MESH2%NDIM.EQ.3) THEN
        MESH2%NAMECOO(3) = 'Z'
        MESH2%UNITCOO(3) = 'M'
      ENDIF
      DO I=1,MESH2%NDIM
        CALL BLANC2USCORE(MESH2%NAMECOO(I),16)
        CALL BLANC2USCORE(MESH2%UNITCOO(I),16)
      ENDDO
!
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR LES RESUTATS'
      IF(LNG.EQ.2) WRITE(LU,*) '---RESULTS INFORMATIONS'
      ! WE DO A FIRST READ TO COUNT THE NUMBER OF TIMESTEPS
      MESH2%TIMESTEP = 0
      CALL GET_DATA_NTIMESTEP(FFORMAT,NINP,MESH2%TIMESTEP,IERR)
!
      IF(MESH2%TIMESTEP .NE. 0) THEN
        ! IF WE HAVE RESULTS WE GO BACK TO BEGINNING OF THE FILE
        CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(SLFFILE))
        OPEN(NINP,FILE=SLFFILE, FORM='UNFORMATTED', IOSTAT=IERR)
        CALL FNCT_CHECK(IERR,'OPEN '//TRIM(SLFFILE))
        ! ALLOCATING RESULTS TABLES
        ALLOCATE(MESH2%TIMES(MESH2%TIMESTEP),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%TIMES')
        ALLOCATE(MESH2%RESULTS(MESH2%TIMESTEP,MESH2%NVAR,
     &                             MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%RESULTS')
        IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*)
     &            'NOMBRE DE PAS DE TEMPS:',MESH2%TIMESTEP
        IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*)
     &            'NUMBER OF TIME STEP:',MESH2%TIMESTEP
        ! THEN READ THE RESULTS FOR ALL VARIALBLES AND ALL TIMESTEPS
        ALLOCATE(TMP(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE TMP')
        DO I=1,MESH2%TIMESTEP
          CALL GET_DATA_TIME(FFORMAT,NINP,I-1,MESH2%TIMES(I),IERR)
          IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) '--POUR TEMPS : ',
     &                  REAL(MESH2%TIMES(I))
          IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) '--FOR TIME: ',
     &                  REAL(MESH2%TIMES(I))
          DO J=1,MESH2%NVAR
            IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) '-POUR VARIABLE : ',
     &             MESH2%NAMEVAR(J)
            IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) '-FOR VARIABLE: ',
     &             MESH2%NAMEVAR(J)
            CALL GET_DATA_VALUE(FFORMAT, NINP, I-1, MESH2%NAMEVAR(J),
     &                          TMP, MESH2%NPOIN, IERR)
            DO K=1,MESH2%NPOIN
              MESH2%RESULTS(I,J,K) = TMP(K)
            ENDDO
          ENDDO
        ENDDO
        DEALLOCATE(TMP)
      ENDIF
      ! IF WE ARE IN 3D THE FIRST VARIABLE IS THE Z COORDINATES
      IF(MESH2%NDIM.EQ.3) THEN
        IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) 'COPIE COORDONEES Z'
        IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) 'COPY Z COORDINATES'
        DO I=1,MESH2%NPOIN
          MESH2%Z(I) = MESH2%RESULTS(1,1,I)
        ENDDO
      ENDIF
!
      CALL CLOSE_MESH(FFORMAT,NINP,IERR)
      CALL CHECK_CALL(IERR,'READ_SERAFIN:CLOSE_MESH')
!
      IF(LNG.EQ.1) WRITE(LU,*)
     &       '---INFORMATIONS SUR LES CONDITIONS LIMITES'
      IF(LNG.EQ.2) WRITE(LU,*) '---BOUNDARY INFORMATIONS'
      IF(LIMFILE(1:1).EQ.' ') THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'PAS DE FICHIER DE CONDITIONS LIMITES'
        IF(LNG.EQ.2) WRITE(LU,*) 'NO BOUNDARY FILE'
        MESH2%NPTFR = 0
      ELSE
        ! READING THE BOUNDARY LIMIT FILE
        CALL READ_LIM(LIMFILE)
      ENDIF
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------FIN LECTURE DU FICHIER SERAFIN'
      IF(LNG.EQ.2) WRITE(LU,*) '------ENDING READING OF SERAFIN FILE'
      WRITE(LU,*) '----------------------------------------------------'
      END SUBROUTINE
!                       *****************
                        SUBROUTINE WRITE_SERAFIN
!                       *****************
     &(SLFFILE,LIMFILE,SERAFIND)
!
!***********************************************************************
! STBTEL   V6P1                                   11/07/2011
!***********************************************************************
!
!BRIEF    WRITE A FILE OF SERAFIN FORMAT WITH THE MESH OBJECT
!+        INFORMATIONS
!
!HISTORY  Y.AUDOUIN (EDF)
!+        11/07/2011
!+        V6P1
!+   CREATION OF THE FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SLFFILE        |-->| NAME OF THE SERAFIN FILE
!| LIMFILE        |-->| NAME OF THE BOUNDARY FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_STBTEL
      USE BIEF
      USE CONV_LIM
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: SLFFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LIMFILE
      LOGICAL, INTENT(IN)                   :: SERAFIND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I,J,K,IERR,IELEM
      CHARACTER(LEN=SNAME_SIZE*2),ALLOCATABLE ::VARI(:)
      CHARACTER*80 :: TITLE
      CHARACTER*8 :: FFORMAT
      DOUBLE PRECISION, ALLOCATABLE :: TMP(:)
      INTEGER, ALLOCATABLE :: TMP2(:)
      INTEGER :: DATE(3), TIME(3)
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*)'------DEBUT ECRITURE DU FICHIER SERAFIN'
      IF(LNG.EQ.2) WRITE(LU,*)'------BEGINNING WRITTING OF SERAFIN FILE'
      WRITE(LU,*) '----------------------------------------------------'
!
!-----------------------------------------------------------------------
!
      ! IF THE OPTION DOUBLE PRECISION IS TRUE WE WRITE IN DOUBLE
      ! PRECISION SINGLE OTHERWISE
      IF(SERAFIND) THEN
        FFORMAT = 'SERAFIND'
      ELSE
        FFORMAT = 'SERAFIN '
      ENDIF
      CALL OPEN_MESH(FFORMAT,SLFFILE,NOUT,'WRITE    ',IERR)
      CALL CHECK_CALL(IERR,'WRITE_SERAFIN:OPEN_MESH')
      ! TITLE AND VARIABLES
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR LES VARIABLES'
      IF(LNG.EQ.2) WRITE(LU,*) '---VARIABLES INFORMATIONS'
      TITLE = MESH2%TITLE
      ALLOCATE(VARI(MESH2%NVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'WRITE_SERAFIN:VARI')
      DO I=1,MESH2%NVAR
        VARI(I)(1:16) = MESH2%NAMEVAR(I)
        VARI(I)(17:32) = MESH2%UNITVAR(I)
        CALL USCORE2BLANC(VARI(I),32)
        IF(DEBUG) WRITE(LU,*) '------',VARI(I)
      ENDDO
      CALL SET_HEADER(FFORMAT,NOUT,TITLE,MESH2%NVAR,VARI,IERR)
      CALL CHECK_CALL(IERR,'WRITE_SERAFIN:SET_HEADER')

      ! MESH INFORMATIONS
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR MAILLAGE'
      IF(LNG.EQ.2) WRITE(LU,*) '---MESH INFORMATIONS'

      DATE = (/0,0,0/)
      TIME = (/0,0,0/)

      ALLOCATE(TMP2(MESH2%NELEM*MESH2%NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'WRITE_SERAFIN:TMP0')
      DO I = 1,MESH2%NDP
        DO IELEM = 1,MESH2%NELEM
          TMP2((I-1)*MESH2%NELEM + IELEM) =
     &            MESH2%IKLES((IELEM-1)*MESH2%NDP+I)
        ENDDO
      ENDDO
      CALL SET_MESH(FFORMAT,NOUT,MESH2%NDIM,MESH2%TYPE_ELEM,MESH2%NDP,
     &              MESH2%IB(8),MESH2%IB(9),MESH2%NELEM,MESH2%NPOIN,
     &              TMP2,MESH2%IPOBO,MESH2%KNOLG,MESH2%X,
     &              MESH2%Y,MESH2%IB(7),DATE,TIME,IERR)
      CALL CHECK_CALL(IERR,'WRITE_SERAFIN:SET_MESH')
      DEALLOCATE(TMP2)

      ! RESULTS INFORMATIONS
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR LES RESUTATS'
      IF(LNG.EQ.2) WRITE(LU,*) '---RESULTS INFORMATIONS'
      IF(MESH2%TIMESTEP.NE.0) THEN
        ALLOCATE(TMP(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE TMP')
        DO I=1,MESH2%TIMESTEP
          IF(DEBUG) WRITE(LU,*) '------','TIME',MESH2%TIMES(I)
          DO J=1,MESH2%NVAR
            IF(DEBUG) WRITE(LU,*) '---------','VAR',VARI(J)
            DO K=1,MESH2%NPOIN
              TMP(K) = MESH2%RESULTS(I,J,K)
            ENDDO
            CALL ADD_DATA(FFORMAT,NOUT,VARI(J),MESH2%TIMES(I),I-1,
     &                    J.EQ.1,TMP,MESH2%NPOIN,IERR)
            CALL CHECK_CALL(IERR,'WRITE_SERAFIN:ADD_DATA')
          ENDDO
        ENDDO
        DEALLOCATE(TMP)
      ENDIF
!
      CALL CLOSE_MESH(FFORMAT,NOUT,IERR)
      CALL CHECK_CALL(IERR,'WRITE_SERAFIN:CLOSE_MESH')

      DEALLOCATE(VARI)
      IF(LNG.EQ.1) WRITE(LU,*)
     &           '---INFORMATIONS SUR LES CONDITIONS LIMITES'
      IF(LNG.EQ.2) WRITE(LU,*) '---BOUNDARY INFORMATIONS'
      ! WRITTING THE BOUNFARY FILE
      IF(MESH2%NPTFR.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*)
     &          'PAS D INFORMATIONS SUR LES CONDITIONS LIMITES'
        IF(LNG.EQ.2) WRITE(LU,*) 'NO BOUNDARY INFORMATIONS'
      ELSE
        CALL WRITE_LIM(LIMFILE)
      ENDIF
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------FIN ECRITURE DU FICHIER SERAFIN'
      IF(LNG.EQ.2) WRITE(LU,*) '------ENDING WRITTING OF SERAFIN FILE'
      WRITE(LU,*) '----------------------------------------------------'
      END SUBROUTINE
      END MODULE
