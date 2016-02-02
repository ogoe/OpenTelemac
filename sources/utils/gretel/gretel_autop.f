!                    ********************
                     SUBROUTINE GRETEL_AUTOP
!                    ********************
     &(GEO,GEOFORMAT,RES,RESFORMAT,NPROC,NPLAN_RES)
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION
!+                TO WRITE A SINGLE FILE IN A GIVEN FORMAT.
!
!
!history  Y. Audouin
!+        02/09/2014
!+
!+   Creation of the file
!+   This version of gretel now handles multiple format and has a better memory organisation
!+   It also uses the hermes module for I/O
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      CHARACTER(LEN=250), INTENT(IN) :: GEO
      CHARACTER(LEN=250), INTENT(IN) :: RES
      CHARACTER(LEN=8),   INTENT(INOUT) :: GEOFORMAT,RESFORMAT
      INTEGER,            INTENT(IN) :: NPROC
      INTEGER,            INTENT(INOUT) :: NPLAN_RES
!
      INTEGER IPID
      INTEGER I,J,IELEM
      INTEGER NPLAN_GEO,NELEM_GEO,NDP,NELEBD, NPTFR, NPTIR
      INTEGER :: NDIM
!
      INTEGER, DIMENSION(:)  , ALLOCATABLE :: IPOBO_GEO,IPOBO3D
      INTEGER, DIMENSION(:), ALLOCATABLE :: KNOLG, TMP2
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKLE_GEO,IKLE3D
!
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RESDATA
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TMP, X, Y
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: X3D, Y3D
!
      DOUBLE PRECISION AT
!
      LOGICAL IS
!
      CHARACTER(LEN=300) :: RESPAR
!
      CHARACTER*80 TITSEL
      CHARACTER*32,ALLOCATABLE :: TEXTELU(:)
      CHARACTER*16,ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      CHARACTER*11 EXTENS
      EXTERNAL  EXTENS
      INTRINSIC REAL
!
      INTEGER IERR, NRES, NRESPAR, NGEO
      INTEGER TYP_ELEM, TYP_BND_ELEM
      INTEGER DATE(3), TIME(3)
      INTEGER NTIMESTEP_RES
      INTEGER NPOIN_GEO, NPOIN_RES, NPOIN_PAR
      INTEGER NVAR_RES,NVAR_GEO
      INTEGER IVAR, ITIME
      INTEGER NPOIN3D, NELEM3D
!
!-------------------------------------------------------------------------
!
      LI=5
      LU=6
      LNG=2
      NGEO = 2
      NRES = 3
      NRESPAR = 4

!
!|==================================================================|
!|                                                                  |
!| START: MERGES FILES RESULTING FROM THE DOMAIN DECOMPOSITION      |
!|                                                                  |
!|==================================================================|
!
! READS FILE NAMES AND THE NUMBER OF PROCESSORS / PARTITIONS
!
!
      CALL OPEN_MESH(RESFORMAT,RES,NRES,'READWRITE',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RES")
!
!     Header information
!
      RESPAR = TRIM(RES) // EXTENS(NPROC-1,0)
      CALL OPEN_MESH(RESFORMAT,RESPAR,NRESPAR,'READ     ',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RESPAR")
!
      ! GET THE MESH TITLE
      CALL GET_MESH_TITLE(RESFORMAT,NRESPAR,TITSEL,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_MESH_TITLE")

!     Get the number of variable
      CALL GET_DATA_NVAR(RESFORMAT,NRESPAR,NVAR_RES,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_DATA_NVAR")
!
      ALLOCATE(TEXTELU(NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:TEXTELU')
      ALLOCATE(VAR_NAME(NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:VAR_NAME')
      ALLOCATE(VAR_UNIT(NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:VAR_UNIT')
!
      CALL GET_DATA_VAR_LIST(RESFORMAT,NRESPAR,NVAR_RES,VAR_NAME,
     &                       VAR_UNIT,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_DATA_VAR_LIST")
!
      WRITE(LU,*) 'TITLE=',TITSEL
      WRITE(LU,*) 'NBVAR=',NVAR_RES
      DO I=1,NVAR_RES
        TEXTELU(I)(1:16) = VAR_NAME(I)
        TEXTELU(I)(17:32) = VAR_UNIT(I)
        WRITE(LU,*) 'VARIABLE ',I,' : ',TEXTELU(I)
      ENDDO ! I
!
      DEALLOCATE(VAR_NAME)
      DEALLOCATE(VAR_UNIT)
!
!     WE NEED TO GET THE NUMBER OF PLANES IN THE PARTITIONNED FILE
!     TO KNOW IF WE NEED TO TRANFORM THE GEOMETRY IN 3D GEOMETRY
      CALL GET_MESH_NPLAN(RESFORMAT,NRESPAR,NPLAN_RES,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:MESH_NPLAN")
!
!     Get the number of timestep
      CALL GET_DATA_NTIMESTEP(RESFORMAT,NRESPAR,NTIMESTEP_RES,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:GET_DATA_NTIMESTEP")

      CALL CLOSE_MESH(RESFORMAT,NRESPAR,IERR)
      CALL CHECK_CALL(IERR,"GRETEL:CLOSE_MESH:RESPAR")

      CALL SET_HEADER(RESFORMAT,NRES,TITSEL,NVAR_RES,TEXTELU,IERR)
!
!     Geometry information
!
      CALL OPEN_MESH(GEOFORMAT,GEO,NGEO,'READ     ',IERR)
      CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:GEO")
!
      ! TODO: HANDLE BOUNDARY CONDITION AS WELL FOR MED ?
      CALL READ_MESH_INFO(GEOFORMAT,NGEO,TITSEL,NVAR_GEO,NPOIN_GEO,
     &                    TYP_ELEM,NELEM_GEO,NPTFR,NPTIR,NDP,NPLAN_GEO,
     &                    TYP_BND_ELEM,NELEBD)
      WRITE(LU,*) 'MESH INFORMATIONS:'
      WRITE(LU,*) 'NELEM=',NELEM_GEO
      WRITE(LU,*) 'NPOIN=',NPOIN_GEO
      WRITE(LU,*) 'NDP=',NDP
      WRITE(LU,*) 'TYP_ELEM=',TYP_ELEM
!
      ALLOCATE(IKLE_GEO(NELEM_GEO*NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:IKLE_GEO')
      ALLOCATE(IPOBO_GEO(NPOIN_GEO),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:IPOBO')
!
      CALL READ_MESH_CONN(GEOFORMAT,NGEO,NPOIN_GEO,TYP_ELEM,NELEM_GEO,
     &                    NDP,TYP_BND_ELEM,NELEBD,IKLE_GEO, IPOBO_GEO)

      ! GET 2D COORDINATES
      ALLOCATE(X(NPOIN_GEO),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:X')
      ALLOCATE(Y(NPOIN_GEO),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:Y')
      ! GET MESH COORDINATES FROM THE GEO MESH
      CALL GET_MESH_COORD(GEOFORMAT,NGEO,1,2,NPOIN_GEO,X,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:X:GEO')
      CALL GET_MESH_COORD(GEOFORMAT,NGEO,2,2,NPOIN_GEO,Y,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:Y:GEO')

      CALL CLOSE_MESH(GEOFORMAT,NGEO,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:CLOSE_MESH:GEO')

      !
      ! Update coordiantes with coordinates from the partitionned files
      ! as they could have been modified by corrxy
      IF(NPLAN_RES.EQ.0) THEN
        NDIM = 2
      ELSE
        NDIM = 3
      ENDIF
      DO IPID = 0, NPROC-1
!
        RESPAR = TRIM(RES) // EXTENS(NPROC-1,IPID)
        CALL OPEN_MESH(RESFORMAT,RESPAR,NRESPAR,'READ     ',IERR)
        CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RESPAR2")
!
        CALL GET_MESH_NPOIN(RESFORMAT,NRESPAR,TYP_ELEM,NPOIN_PAR,IERR)
        CALL CHECK_CALL(IERR,"GRETEL:GET_MESH_NPOIN:RESPAR")
!
        ALLOCATE(KNOLG(NPOIN_PAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:KNOLG')
        ALLOCATE(TMP(NPOIN_PAR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:TMP')
!
        CALL GET_MESH_L2G_NUMBERING(RESFORMAT,NRESPAR,KNOLG,
     &                              NPOIN_PAR,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_L2G_NUMBERING:RESPAR')
!
        CALL GET_MESH_COORD(RESFORMAT,NRESPAR,1,NDIM,NPOIN_PAR,TMP,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:X:RESPAR')
        DO I=1,NPOIN_PAR/(MAX(NPLAN_RES,1))
          X(KNOLG(I)) = TMP(I)
        ENDDO
        CALL GET_MESH_COORD(RESFORMAT,NRESPAR,2,NDIM,NPOIN_PAR,TMP,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_COORD:Y:RESPAR')
        DO I=1,NPOIN_PAR/(MAX(NPLAN_RES,1))
          Y(KNOLG(I)) = TMP(I)
        ENDDO
        CALL CLOSE_MESH(RESFORMAT,NRESPAR,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:CLOSEMESH:RESPAR')
        DEALLOCATE(TMP)
        DEALLOCATE(KNOLG)
      ENDDO ! IPID


      !
      ! IF WE HAVE A 3D RESULT WE NEED TO TRANSFORM THE MESH IN 3D
      ! WRITES THE MESH INFORMATION TO THE MERGED FILE
      WRITE(LU,*) 'WRITING MESH'
      IF(NPLAN_RES.EQ.0) THEN
        ! 2D
        ALLOCATE(TMP2(NELEM_GEO*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:TMP0')
        DO I = 1,NDP
          DO IELEM = 1,NELEM_GEO
            TMP2((I-1)*NELEM_GEO + IELEM) = IKLE_GEO((IELEM-1)*NDP+I)
          ENDDO
        ENDDO

        CALL SET_MESH(RESFORMAT,NRES,2,TYP_ELEM,NDP,NPTFR,NPTIR,
     &                NELEM_GEO,NPOIN_GEO,TMP2,IPOBO_GEO,IPOBO_GEO,X,Y,
     &                NPLAN_RES,DATE,TIME,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:SET_MESH:RES')
        DEALLOCATE(IKLE_GEO)
        DEALLOCATE(TMP2)
        DEALLOCATE(IPOBO_GEO)
        DEALLOCATE(X)
        DEALLOCATE(Y)
        NPOIN_RES = NPOIN_GEO
      ELSE
        ! 3D
        NDP = 6
        TYP_ELEM = PRISM_ELT_TYPE
        NPOIN3D = NPOIN_GEO * NPLAN_RES
        NELEM3D = NELEM_GEO * (NPLAN_RES-1)
        NPTFR = 0
        NPTIR = 0
        NPOIN_RES = NPOIN3D
        ALLOCATE(IKLE3D(NELEM3D*NDP),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'GRETEL:IKLE3D')
        ALLOCATE(IPOBO3D(NPOIN3D),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR, 'GRETEL:IPOBO3D')
!
        ! BUILDING 3D IKLE
        DO I=1,NPLAN_RES-1
          DO J=1,NELEM_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (0*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+1) + (I-1)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (1*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+2) + (I-1)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (2*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+3) + (I-1)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (3*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+1) + (I)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (4*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+2) + (I)*NPOIN_GEO
            IKLE3D(J+(I-1)*NELEM_GEO + (5*NELEM3D)) =
     &                    IKLE_GEO((J-1)*3+3) + (I)*NPOIN_GEO
          ENDDO
        ENDDO
        ! TODO: BOUNDARY TO HANDLE FOR MED ?
        ! DEFAULT IPOBO
        DO I=1,NPOIN3D
          IPOBO3D(I) = 0
        ENDDO
        ! WE DONT NEED THEM ANYMORE
        DEALLOCATE(IKLE_GEO)
        DEALLOCATE(IPOBO_GEO)
        ! BUILD THE COORDINATES FROM THE 2D COORDINATES
        ALLOCATE(X3D(NPOIN_GEO*NPLAN_RES),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:X3D')
        ALLOCATE(Y3D(NPOIN_GEO*NPLAN_RES),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'GRETEL:Y3D')
        !
        DO I=1,NPOIN_GEO
          DO J=1,NPLAN_RES
            X3D(I+(J-1)*NPOIN_GEO) = X(I)
            Y3D(I+(J-1)*NPOIN_GEO) = Y(I)
          ENDDO
        ENDDO
        DEALLOCATE(X)
        DEALLOCATE(Y)
        DATE = (/0,0,0/)
        TIME = (/0,0,0/)
        CALL SET_MESH(RESFORMAT,NRES,3,TYP_ELEM,NDP,NPTFR,NPTIR,NELEM3D,
     &                NPOIN3D,IKLE3D,IPOBO3D,IPOBO3D,X3D,Y3D,
     &                NPLAN_RES,DATE,TIME,IERR)
        CALL CHECK_CALL(IERR,'GRETEL:SET_MESH:RES')
        DEALLOCATE(IKLE3D)
        DEALLOCATE(IPOBO3D)
        DEALLOCATE(X3D)
        DEALLOCATE(Y3D)
      ENDIF
!
!     Read results informations from partitioned files
!
      ! TODO: SEE IF LOOP ON ALL FILE OR LOOP ON TIMESTEPS
      ALLOCATE(RESDATA(NPOIN_RES,NVAR_RES),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'GRETEL:RES')
      ! LOOP ON ALL THE PARTITIONNED FILES
      DO ITIME=0,NTIMESTEP_RES-1
        DO IPID = 0, NPROC-1
!
          RESPAR = TRIM(RES) // EXTENS(NPROC-1,IPID)
          CALL OPEN_MESH(RESFORMAT,RESPAR,NRESPAR,'READ     ',IERR)
          CALL CHECK_CALL(IERR,"GRETEL:OPEN_MESH:RESPAR2")
!
          CALL GET_MESH_NPOIN(RESFORMAT,NRESPAR,TYP_ELEM,NPOIN_PAR,IERR)
          CALL CHECK_CALL(IERR,"GRETEL:GET_MESH_NPOIN:RESPAR")
!
          ALLOCATE(KNOLG(NPOIN_PAR),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'GRETEL:KNOLG')
          ALLOCATE(TMP(NPOIN_PAR),STAT=IERR)
          CALL CHECK_ALLOCATE(IERR,'GRETEL:TMP')
          ! GET THE TIME OF THE TIMSTEP
          IF(IPID.EQ.0) THEN
            CALL GET_DATA_TIME(RESFORMAT,NRESPAR,ITIME,AT,IERR)
            CALL CHECK_CALL(IERR,'GRETEL:GET_DATA_TIME:RESPAR')
          ENDIF
!
          CALL GET_MESH_L2G_NUMBERING(RESFORMAT,NRESPAR,KNOLG,
     &                                NPOIN_PAR,IERR)
          CALL CHECK_CALL(IERR,'GRETEL:GET_MESH_L2G_NUMBERING:RESPAR')
!
          ! LOOP ON ALL THE VARIABLE FOR THE TIMESTEP ITIME
          DO IVAR=1,NVAR_RES
            CALL GET_DATA_VALUE(RESFORMAT,NRESPAR,ITIME,
     &                          TEXTELU(IVAR)(1:16),TMP,
     &                          NPOIN_PAR,IERR)
            CALL CHECK_CALL(IERR,'GRETEL:GET_DATA_VALUE')
            IF(NPLAN_RES.EQ.0) THEN
              ! 2D
              DO I=1,NPOIN_PAR
                RESDATA(KNOLG(I),IVAR) = TMP(I)
              ENDDO
            ELSE
              ! 3D
              DO I=1,NPOIN_PAR/NPLAN_RES
                DO J=1,NPLAN_RES
                  RESDATA(KNOLG(I) + (J-1)*NPOIN_GEO, IVAR) =
     &                       TMP(I + (J-1)*NPOIN_PAR/NPLAN_RES)
                ENDDO
              ENDDO
            ENDIF
          ENDDO ! IVAR
          CALL CLOSE_MESH(RESFORMAT,NRESPAR,IERR)
          CALL CHECK_CALL(IERR,'GRETEL:CLOSEMESH:RESPAR')
          DEALLOCATE(TMP)
          DEALLOCATE(KNOLG)
        ENDDO ! IPID
        ! WRITING TIME STEP
        WRITE(LU,*)'WRITING DATASET NO.',ITIME,' TIME =',REAL(AT)
        !
        DO I=1,NVAR_RES
          CALL ADD_DATA(RESFORMAT,NRES,TEXTELU(I),AT,ITIME,I.EQ.1,
     &                  RESDATA(:,I),NPOIN_RES,IERR)
          CALL CHECK_CALL(IERR,'GRETEL:ADD_DATA:RES')
        ENDDO

      ENDDO ! ITIME

      ! DONE
      CALL CLOSE_MESH(RESFORMAT,NRES,IERR)
      CALL CHECK_CALL(IERR,'GRETEL:CLOSEMESH:RES')

      DEALLOCATE(TEXTELU)
      DEALLOCATE(RESDATA)
      WRITE(LU,*) 'END OF PROGRAM, ',NTIMESTEP_RES,' DATASETS FOUND'

      END SUBROUTINE GRETEL_AUTOP
