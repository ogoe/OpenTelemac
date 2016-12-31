      MODULE CONV_VTK
      CONTAINS
!                       *****************
                        SUBROUTINE WRITE_VTK
!                       *****************
     &(VTKFILE)
!
!***********************************************************************
! STBTEL   V6P1                                   11/07/2011
!***********************************************************************
!
!BRIEF    WRITE A FILE OF SELAFIN FORMAT WITH THE MESH OBJECT
!+        INFORMATIONS
!
!HISTORY  Y.AUDOUIN (EDF)
!+        11/07/2011
!+        V6P1
!+   CREATION OF THE FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| VTKFILE        |-->| NAME OF THE VTK FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_STBTEL
      USE LIB_VTK_IO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: VTKFILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
#if defined(HAVE_VTK)
      INTEGER :: IERR,ITIME,I,J
      INTEGER, ALLOCATABLE :: CELL_TYPE(:), CONNECT(:)
      DOUBLE PRECISION, ALLOCATABLE :: TMP(:)
      CHARACTER(LEN=MAXLENHARD) :: FILENAME
      CHARACTER*8 :: TIMESTR
      INTEGER :: LENGTH
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------DEBUT ECRITURE FICHIER VTK'
      IF(LNG.EQ.2) WRITE(LU,*) '------BEGINNING WRITTING OF VTK FILE'
      WRITE(LU,*) '----------------------------------------------------'
!
!-----------------------------------------------------------------------
!
      ! IF WE ARE IN 2D ADDING Z COORDINATES TO 0
      IF(MESH2%NDIM.EQ.2) THEN
        ALLOCATE(MESH2%Z(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE MESH2%Z')
        MESH2%Z(:) = 0.D0
      ENDIF
      ! MAKING A FILE FOR EACH TIME STEP
      DO ITIME=1,MESH2%TIMESTEP
        CALL NUMBER2STRING(TIMESTR,ITIME)
        FILENAME = TRIM(VTKFILE)//TIMESTR
        IF(LNG.EQ.1) WRITE(LU,*) '-- ECRITURE DU FICHIER : ',FILENAME
        IF(LNG.EQ.2) WRITE(LU,*) '-- WRITTING FILE: ',FILENAME
        ! INITIALISING THE VTK FILE
        IERR=VTK_INI('ASCII',FILENAME,MESH2%TITLE,'UNSTRUCTURED_GRID')
        CALL FNCT_CHECK(IERR,'VTK_INI')
        ! WRITTING THE NODES COORDINATES
        IERR=VTK_GEO(MESH2%NPOIN,MESH2%X,MESH2%Y,MESH2%Z)
        CALL FNCT_CHECK(IERR,'VTK_GEO')
        ! DEFINING THE CELLS TYPE
        ALLOCATE(CELL_TYPE(MESH2%NELEM),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE CELL_TYPE')
        ! VALUE CHOSEN IN PDF VTK FILE FORMAT (GOOGLE)
        SELECT CASE(MESH2%TYPE_ELEM)
        CASE(TRIANGLE_ELT_TYPE)
          CELL_TYPE(:) = 5
        CASE(QUADRANGLE_ELT_TYPE)
          CELL_TYPE(:) = 9
        CASE(TETRAHEDRON_ELT_TYPE)
          CELL_TYPE(:) = 10
        CASE(PRISM_ELT_TYPE)
          CELL_TYPE(:) = 13
        END SELECT
        ! BUILDING THE CONNECTION TABLE FOLLOWING THE VTK STANDARD
        ! FOR EACH CELL :
        !   FIRST THE NUMBER OF NODES COMPOSING THE CELL
        !   THEN THE NUMBER OF EACH NODE
        ! WE NEED TO RENUMBER TO NODES 0 TO NELEM-1
        ALLOCATE(CONNECT(MESH2%NELEM*(MESH2%NDP+1)),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE CONNECT')
        DO I=1,MESH2%NELEM
          CONNECT((I-1)*(MESH2%NDP+1)+1) = MESH2%NDP
          DO J=1,MESH2%NDP
            CONNECT((I-1)*(MESH2%NDP+1)+J+1) =
     &              MESH2%IKLES((I-1)*MESH2%NDP+J)-1
          ENDDO
        ENDDO
        IERR=VTK_CON(MESH2%NELEM,CONNECT,CELL_TYPE)
        CALL FNCT_CHECK(IERR,'VTK_CON')
        ! ADDING THE NUMBER OF NODES ON WHICH THERE IS GONNA BE A
        ! VARAIBLE HERE EVERY ONE
        IERR=VTK_DAT(MESH2%NPOIN,'node')
        CALL FNCT_CHECK(IERR,'VTK_DAT')
        ! ADDING EACH VARAIBLE
        ALLOCATE(TMP(MESH2%NPOIN),STAT=IERR)
        CALL FNCT_CHECK(IERR,'ALLOCATE TMP')
        DO I=1,MESH2%NVAR
          DO J=1,MESH2%NPOIN
            TMP(J)= MESH2%RESULTS(ITIME,I,J)
          ENDDO
          ! REMOVE WHITE SPACES IN VAR NAMES
          ! TO BE READABLE IN PARAVIEW 5.0
          CALL BLANC2USCORE(MESH2%NAMEVAR(I),LEN(MESH2%NAMEVAR(I)))
          IERR=VTK_VAR(MESH2%NPOIN,MESH2%NAMEVAR(I),
     &                 TMP)
          CALL FNCT_CHECK(IERR,'VTK_VAR '//MESH2%NAMEVAR(I))
        ENDDO
        DEALLOCATE(TMP)
        ! CLOSING THE FILE
        IERR=VTK_END()
        CALL FNCT_CHECK(IERR,'VTK_END')
        DEALLOCATE(CELL_TYPE,CONNECT)
      ENDDO
      IF(MESH2%NDIM.EQ.2) DEALLOCATE(MESH2%Z)

!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) '----------------------------------------------------'
      IF(LNG.EQ.1) WRITE(LU,*) '------FIN ECRITURE DU FICHIER VTK'
      IF(LNG.EQ.2) WRITE(LU,*) '------ENDING WRITTING OF VTK FILE'
      WRITE(LU,*) '----------------------------------------------------'
#else
      IF(LNG.EQ.1) WRITE(LU,*)
     &       'ERREUR : TENTATIVE D ECRITURE D UN FICHIER VTK ',
     &       'SANS LA BIBLIOTHEQUE'
      IF(LNG.EQ.2) WRITE(LU,*)
     &       'ERROR : TRYING TO WRITE VTK FILE WITHOUT VTK LIBRARY'
      CALL PLANTE(1)

#endif
      END SUBROUTINE
      END MODULE
