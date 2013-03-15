      MODULE CONV_SERAFIN
      CONTAINS
!                       *********************** 
                        SUBROUTINE READ_SERAFIN 
!                       ***********************
!
     &(SLFFILE,LIMFILE)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| SLFFILE        |-->| NAME OF THE SERAFIN FILE IN THE TEMPORARY FOLDER
!| LIMFILE        |-->| NAME OF THE BOUNDARY FILE IN THE TEMPORARY FOLDER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_STBTEL
      USE CONV_LIM
      USE BIEF
!      
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: SLFFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LIMFILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I,J,K,IERR,IDUM
      INTEGER :: TIMESTEP
      CHARACTER(LEN=32) :: VARI
      REAL :: TIME
      DOUBLE PRECISION :: DDUMMY      
      INTEGER :: IB(6)
      DOUBLE PRECISION XB(1)
      REAL :: RB(1)
      CHARACTER(LEN=1) :: CB
      INTEGER :: ISTAT
      REAL, ALLOCATABLE :: TMP(:)
      CHARACTER*2 :: RF
      CHARACTER*8 :: FFORMAT
      DOUBLE PRECISION :: MIN_DIST,MAX_DIST,DIST
      INTEGER I1,I2,I3,I_MIN
      DOUBLE PRECISION :: X,Y,KM
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------DEBUT LECTURE DU FICHIER SERAFIN'
      IF(LNG.EQ.2) WRITE(LU,*) '------BEGINNING READING OF SERAFIN FILE'
      WRITE(LU,*) '----------------------------------------------------'
!      
!-----------------------------------------------------------------------
!
      ! IF THE OPTION DOUBLE PRECISION IS TRUE WE READ IN DOUBLE
      ! PRECISION SINGLE OTHERWISE
      IF(SERAFIN_DOUBLE) THEN
        RF='R8'
        WRITE(LU,*) 'DOUBLE PRECISION'
        FFORMAT='SERAFIND'
      ELSE
        RF='R4'
        FFORMAT='        '
      ENDIF
      OPEN(NINP,IOSTAT=IERR,FILE=SLFFILE,FORM='UNFORMATTED') 
      CALL FNCT_CHECK(IERR,'OPEN '//TRIM(SLFFILE))
      ! READING NAME OF THE MESH
      CALL LIT(XB,RB,IB,MESH2%TITLE,72,'CH',NINP,'STD',ISTAT)
      ! SET THE DESCRIPTION  TO NO DESCRIPTION
      MESH2%DESCRIPTION = 'NO DESCRIPTION'//CHAR(0)
      ! GET THE NUMBER OF VARIABLES
      CALL LIT(XB,RB,IB,CB,2,'I ',NINP,'STD',ISTAT)
      IF(DEBUG) WRITE(LU,*) IB(1),IB(2)
      MESH2%NVAR = IB(1) + IB(2)
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
        DO I=1,MESH2%NVAR
          CALL LIT(XB,RB,IB,VARI,32,'CH',NINP,'STD',ISTAT)
          MESH2%NAMEVAR(I) = VARI(1:16)
          MESH2%UNITVAR(I) = VARI(17:32)
          CALL BLANC2USCORE(MESH2%NAMEVAR(I),16)
          CALL BLANC2USCORE(MESH2%UNITVAR(I),16)
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
      CALL LIT(XB,RB,MESH2%IB,CB,10,'I ',NINP,'STD',ISTAT)
!
      ! GO BACK TO THE BEGINNING OF THE FILE
      CLOSE(NINP,IOSTAT=IERR)
      CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(SLFFILE))
      OPEN(NINP,IOSTAT=IERR,FILE=SLFFILE,FORM='UNFORMATTED') 
      CALL FNCT_CHECK(IERR,'OPEN '//TRIM(SLFFILE))
!
      ! CANCELLING READGEO OUTPUT INFORMATIONS
      ! READING THE NUMBER ELEMENTS, POINT, ...
      CALL READGEO1(MESH2%NPOIN, MESH2%NELEM, MESH2%NPTFR, 
     &              MESH2%NDP, MESH2%IB, NINP,IDUM)
      ! IF WE ARE IN 3D
      IF(MESH2%IB(7).GT.1) THEN
        MESH2%NDIM=3
      ELSE
        MESH2%NDIM=2
      ENDIF
      ! DEFINE THE TYPE OF ELEMENT
      IF(MESH2%NDIM.EQ.2) THEN
        IF(MESH2%NDP.EQ.3) MESH2%TYPE_ELEM = TYPE_TRIA3
        IF(MESH2%NDP.EQ.4) MESH2%TYPE_ELEM = TYPE_QUAD4
      ELSE
        IF(MESH2%NDP.EQ.4) MESH2%TYPE_ELEM = TYPE_TETRA4
        IF(MESH2%NDP.EQ.6) MESH2%TYPE_ELEM = TYPE_PRISM6
      ENDIF
      IF(MESH2%TYPE_ELEM.EQ.0) THEN
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
      CALL READGEO2(MESH2%NPOIN, MESH2%NELEM, MESH2%NPTFR, 
     &              MESH2%NDP, MESH2%IKLES, MESH2%IPOBO,
     &              MESH2%IB, NINP)
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
      IF( (MESH2%IB(8).NE.0) .OR. (MESH2%IB(9).NE.0) ) THEN
        ALLOCATE(MESH2%KNOLG(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%KNOLG')
      ENDIF
!
      !READING THE COORDINATES

!     PROJECTION
      IDUM=1
!
      CALL READGEO3(MESH2%KNOLG,MESH2%X,MESH2%Y,MESH2%NPOIN,
     &              NINP,MESH2%IB,FFORMAT,IDUM,0.D0,0.D0)
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
      DO WHILE(.TRUE.)
        ! IF WE ARE AT THE END OF THE FILE WE GO TO HELL (666)
        ! CANNOT USE LIT BECAUSE LIT CALL PLANTE IF END
        READ(NINP,END=666) TIME
        MESH2%TIMESTEP = MESH2%TIMESTEP + 1
        DO I=1,MESH2%NVAR
          CALL LIT(XB,RB,IB,CB,1,RF,NINP,'STD',ISTAT)
        ENDDO
      ENDDO
!     
666   IF(MESH2%TIMESTEP .NE. 0) THEN
        ! IF WE HAVE RESULTS WE GO BACK TO BEGINNING OF THE FILE
        CLOSE(NINP,IOSTAT=IERR)
        CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(SLFFILE))
        OPEN(NINP,FILE=SLFFILE, FORM='UNFORMATTED', IOSTAT=IERR) 
        CALL FNCT_CHECK(IERR,'OPEN '//TRIM(SLFFILE))
        ! ADVANCING IN THE FILE TO THE RESULTS
        CALL QUICKREAD(NINP)
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
          CALL LIT(MESH2%TIMES(I),RB,IB,CB,1,RF,NINP,'STD',ISTAT)
          IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) '--POUR TEMPS TIME : ',
     &                  REAL(MESH2%TIMES(I))
          IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) '--FOR TIME: ',
     &                  REAL(MESH2%TIMES(I))
          DO J=1,MESH2%NVAR
            IF(DEBUG.AND.(LNG.EQ.1)) WRITE(LU,*) '-POUR VARIABLE : ',
     &             MESH2%NAMEVAR(J)
            IF(DEBUG.AND.(LNG.EQ.2)) WRITE(LU,*) '-FOR VARIABLE: ',
     &             MESH2%NAMEVAR(J)
            CALL LIT(MESH2%RESULTS(I,J,:),TMP,IB,CB,MESH2%NPOIN,
     &               RF,NINP,'STD',ISTAT)
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
      CLOSE(NINP,IOSTAT=IERR)
      CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(SLFFILE))
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
     &(SLFFILE,LIMFILE)
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
!      
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=MAXLENHARD) :: SLFFILE
      CHARACTER(LEN=MAXLENHARD) :: LIMFILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I,J,K,IERR,ISTAT
      CHARACTER(LEN=SNAME_SIZE*2) ::VARI
      INTEGER :: IB(6)
      DOUBLE PRECISION :: XB(1)
      CHARACTER*1 :: CB
      CHARACTER*2 :: RF
      CHARACTER*80 :: TITLE
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
      IF(SERAFIN_DOUBLE) THEN
        RF='R8'
      ELSE
        RF='R4'
      ENDIF
      OPEN(NOUT,IOSTAT=IERR,FILE=SLFFILE,STATUS='NEW',
     &     FORM='UNFORMATTED')
      CALL FNCT_CHECK(IERR,'OPEN '//TRIM(SLFFILE))
      ! TITLE AND NUMBER OF VARIABLES
      TITLE = MESH2%TITLE
      CALL ECRI2(XB,IB,TITLE,80,'CH',NOUT,'STD',ISTAT)
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR LES VARIABLES'
      IF(LNG.EQ.2) WRITE(LU,*) '---VARIABLES INFORMATIONS'
      IB(1) = MESH2%NVAR
      IB(2) = 0
      CALL ECRI2(XB,IB,CB,2,'I ',NOUT,'STD',ISTAT)
      ! NAME OF THE VARIABLES
      DO I=1,MESH2%NVAR
        VARI(1:16) = MESH2%NAMEVAR(I) 
        VARI(17:32) = MESH2%UNITVAR(I)
        CALL ECRI2(XB,IB,VARI,32,'CH',NOUT,'STD',ISTAT)
      ENDDO
      ! GEO1 INFORMATIONS
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR MAILLAGE'
      IF(LNG.EQ.2) WRITE(LU,*) '---MESH INFORMATIONS'
      CALL ECRI2(XB,MESH2%IB,CB,10,'I ',NOUT,'STD',ISTAT)
      IF(MESH2%IB(10) .EQ. 1) THEN
        IB(:) = 1
        CALL ECRI2(XB,IB,CB,6,'I ',NOUT,'STD',ISTAT)
      ENDIF
      IB(1) = MESH2%NELEM
      IB(2) = MESH2%NPOIN
      IB(3) = MESH2%NDP
      IB(4) = 1
      CALL ECRI2(XB,IB,CB,4,'I ',NOUT,'STD',ISTAT)
      ! IKLES AND IPOBO
      CALL ECRI2(XB,MESH2%IKLES,CB,MESH2%NELEM*MESH2%NDP,'I ',
     &           NOUT,'STD',ISTAT)
      IF(MESH2%IB(8).EQ.0 .AND. MESH2%IB(9).EQ.0) THEN
        CALL ECRI2(XB,MESH2%IPOBO,CB,MESH2%NPOIN,'I ',NOUT,'STD',
     &             ISTAT)
      ELSE
        CALL ECRI2(XB,MESH2%KNOLG,CB,MESH2%NPOIN,'I ',NOUT,'STD',
     &             ISTAT)
      ENDIF
      ! COORDONATES
      CALL ECRI2(MESH2%X,IB,CB,MESH2%NPOIN,RF,NOUT,'STD',ISTAT)
      CALL ECRI2(MESH2%Y,IB,CB,MESH2%NPOIN,RF,NOUT,'STD',ISTAT)
      ! RESULTS INFORMATIONS 
      IF(LNG.EQ.1) WRITE(LU,*) '---INFORMATIONS SUR LES RESUTATS'
      IF(LNG.EQ.2) WRITE(LU,*) '---RESULTS INFORMATIONS'
      IF(MESH2%TIMESTEP.NE.0) THEN
        DO I=1,MESH2%TIMESTEP
          CALL ECRI2(MESH2%TIMES(I),IB,CB,1,RF,NOUT,'STD',ISTAT)
          DO J=1,MESH2%NVAR
             CALL ECRI2(MESH2%RESULTS(I,J,:),IB,CB,MESH2%NPOIN,
     &                  RF,NOUT,'STD',ISTAT)
          ENDDO
        ENDDO
      ENDIF
!
      CLOSE(NOUT,IOSTAT=IERR)
      CALL FNCT_CHECK(IERR,'CLOSE '//TRIM(SLFFILE))
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
!                       ***************** 
                        SUBROUTINE QUICKREAD 
!                       *****************
     &(IDFILE)
!
!***********************************************************************
! STBTEL   V6P1                                   11/07/2011
!***********************************************************************
!
!BRIEF    MAKE A QUICK READ OF THE SERAFIN FILE TO REACH THE RESULTS
!                        
!HISTORY  Y.AUDOUIN (EDF)
!+        11/07/2011
!+        V6P1
!+   CREATION OF THE FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IDFILE        |-->| ID OF THE SERAFIN FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_STBTEL
      USE BIEF
!
      IMPLICIT NONE
!      
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: IDFILE
!      
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: I
      INTEGER :: IB(10)
      DOUBLE PRECISION XB(1)
      REAL :: RB(1)
      CHARACTER(LEN=1) :: CB
      INTEGER :: ISTAT
      CHARACTER*2 :: RF
!      
!-----------------------------------------------------------------------
!
      ! IF THE OPTION DOUBLE PRECISION IS TRUE WE WRITE IN DOUBLE
      ! PRECISION SINGLE OTHERWISE
      IF(SERAFIN_DOUBLE) THEN
        RF='R8'
      ELSE
        RF='R4'
      ENDIF
      ! TITLE
      CALL LIT(XB,RB,IB,CB,1,'CH',IDFILE,'STD',ISTAT)
      ! NUMBER OF VARIABLES
      CALL LIT(XB,RB,IB,CB,2,'I ',IDFILE,'STD',ISTAT)
      ! NAME AND UNIT
      DO I=1,IB(1)+IB(2)
        CALL LIT(XB,RB,IB,CB,1,'CH',IDFILE,'STD',ISTAT)
      ENDDO
      ! 10 INTEGERS
      CALL LIT(XB,RB,IB,CB,10,'I ',IDFILE,'STD',ISTAT)
      ! CASE WHERE DATE AND TIME ARE IN THE FILE
      IF(IB(10).EQ.1) CALL LIT(XB,RB,IB,CB,6,'I ',IDFILE,'STD',ISTAT)
      ! 4 INTEGERS
      CALL LIT(XB,RB,IB,CB,4,'I ',IDFILE,'STD',ISTAT)
      ! IKLES
      CALL LIT(XB,RB,IB,CB,1,'I ',IDFILE,'STD',ISTAT)
      ! IPOBO OR KNOLG
      CALL LIT(XB,RB,IB,CB,1,'I ',IDFILE,'STD',ISTAT)
      ! X AND Y
      CALL LIT(XB,RB,IB,CB,1,RF,IDFILE,'STD',ISTAT)
      CALL LIT(XB,RB,IB,CB,1,RF,IDFILE,'STD',ISTAT)
!      
!-----------------------------------------------------------------------
!
      END SUBROUTINE
      END MODULE
