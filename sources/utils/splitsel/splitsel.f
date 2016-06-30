!
!     ************************
      PROGRAM SPLITSEL
!     ************************
!
!***********************************************************************
! SPLITSEL   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    Refine a mesh by splitting the triangle in 4
!
!history  C. MOULINEC (STFC)
!+        10/11/2011
!+        V7P0
!+        First version of the file
!
!history  Y. AUDOUIN (EDF)
!+        01/01/2016
!+        V7P1
!+        Integration in the source code
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE PAIR
!
      IMPLICIT NONE
!
      INTEGER          :: NFIC   ! FILE TO READ
      CHARACTER(LEN=144) :: INPUT_FILE
      CHARACTER(LEN=144) :: INPUT_CLI_FILE
      CHARACTER(LEN=8) :: INFORMAT
      CHARACTER(LEN=8) :: OUTFORMAT
      INTEGER          :: NOUT   ! FILE TO WRITE
      CHARACTER(LEN=144) :: OUTPUT_FILE
      CHARACTER(LEN=144) :: OUTPUT_CLI_FILE
      INTEGER          :: ISORT
!CCCC ORIGINAL MESH
!
      INTEGER          :: NPOIN  ! NUMBER OF MESH NODES
      INTEGER          :: NELEM  ! NUMBER OF ELEMENTS
      INTEGER          :: NDP    ! NUMBER OF ELEMENT FACES
      INTEGER          :: IB(10) ! INTEGER ARRAY
      INTEGER          :: NPTFR  ! NUMBER OF BORDER NODES
      INTEGER          :: NELEBD ! NUMBER OF BORDER ELEMENTS
      INTEGER          :: NPLAN ! NUMBER OF PLANES (3D ONLY)
      INTEGER :: TYP_BND_ELEM, TYP_ELEM
      INTEGER :: DATE(3),TIME(3)
!
      INTEGER PROJEC
      DOUBLE PRECISION :: LATI,LONGI
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IKLES,IKLE
      INTEGER, ALLOCATABLE, DIMENSION(:)  :: IPOBO
!
      INTEGER, ALLOCATABLE, DIMENSION(:)          :: KNOLG
!
      ! NAME TAKEN FROM UTILS/BIEF/FONDSTR.F
      CHARACTER(LEN=16) :: FOND_NAME(5) =
     &  (/ 'FOND            ','BOTTOM          ','ALTIMETRIE      ',
     &     'COTE_DU_FOND    ','BOTTOM_LEVEL    '/)
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X, Y, ZF
!
!CCCC BOUNDARY FILE
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: LIHBOR, LIUBOR, LIVBOR
      INTEGER, ALLOCATABLE, DIMENSION(:) :: LITBOR
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NBOR
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NUMLIQ, NUMSOL, CHECK
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: HBOR
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: UBOR, VBOR, AUBOR
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: TBOR, ATBOR, BTBOR
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: I_END_BC
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: LIHBOR_REF
      INTEGER, ALLOCATABLE, DIMENSION(:) :: LIUBOR_REF
      INTEGER, ALLOCATABLE, DIMENSION(:) :: LIVBOR_REF
      INTEGER, ALLOCATABLE, DIMENSION(:) :: LITBOR_REF
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NBOR_REF
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: CHECK_REF
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: HBOR_REF
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: UBOR_REF
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: VBOR_REF
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: AUBOR_REF
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: TBOR_REF
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: ATBOR_REF
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:)  :: BTBOR_REF
!
      INTEGER :: NCLI
!
!CCCC REFINED MESH
!
      INTEGER          :: NPOIN_REF  ! NUMBER OF MESH NODES
      INTEGER          :: NELEM_REF ! NUMBER OF ELEMENTS
      INTEGER          :: NDP_REF   ! NUMBER OF ELEMENT FACES
      INTEGER          :: IB_REF(10) ! INTEGER ARRAY
      INTEGER          :: NPTFR_REF ! NUMBER OF BORDER NODES
      INTEGER          :: NFIC_REF  ! FILE TO READ
      INTEGER          :: NELEBD_REF! NUMBER OF BORDER ELEMENTS
!
      INTEGER          :: NPOIN_REF1
      INTEGER          :: NPOIN_REF2_X, NPOIN_REF3_X
      INTEGER          :: NPOIN_REF2_Y, NPOIN_REF3_Y
!
!
      INTEGER          :: I_BC_SING
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: IKLES_REF
      INTEGER, ALLOCATABLE, DIMENSION(:)  :: IPOBO_REF
!
      INTEGER, ALLOCATABLE, DIMENSION(:)          :: KNOLG_REF
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X_REF, Y_REF,
     &                                               ZF_REF
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X_REF_BC, Y_REF_BC
      INTEGER, ALLOCATABLE, DIMENSION(:)          :: I_REF_BC
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X_REF_X_SORTED,
     &     Y_REF_X_SORTED, ZF_REF_X_SORTED
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X_REF_Y_SORTED,
     &     Y_REF_Y_SORTED
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NBOR_REF_X_SORTED
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: I_X_REF_SORTED2ORI
      INTEGER, ALLOCATABLE, DIMENSION(:) :: I_X_REF_ORI2SORTED
!
      TYPE(TYPE_PAIR), ALLOCATABLE, DIMENSION(:) :: R_XQ_REF
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: X_REFINED_X
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: Y_REFINED_X
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: ZF_REFINED_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NBOR_REFINED_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IKLES_RAW
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IKLES_REF_X_SORTED
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IPOBO_REF_X_SORTED
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IKLES_REFINED_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: IKLE_REFINED_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: I_ORI2MERGED_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: I_MERGED2ORI_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: I_MIDDLE_SEG_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IPOBO_REFINED_X
!
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: I_ELTS_BOUNDARY
!
      INTEGER :: NPOIN_BOR
!
      INTEGER :: IERR, I, J, K, L, M
!
      INTEGER :: IKLES1, IKLES2, IKLES3
!
      DOUBLE PRECISION :: X1, X2, X3, Y1, Y2, Y3, ZF1, ZF2, ZF3
!
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: XCEN, YCEN
!
      DOUBLE PRECISION :: XDIFF, YDIFF, XYDIFF, TOL
!
      DOUBLE PRECISION :: TIME1, TIME2
      DOUBLE PRECISION :: TIME3, TIME4
      DOUBLE PRECISION :: TIME5, TIME6
      DOUBLE PRECISION :: TIME7, TIME8
      DOUBLE PRECISION :: TIME9, TIME10
      DOUBLE PRECISION :: TIME11, TIME12
      DOUBLE PRECISION :: TIME13, TIME14
      DOUBLE PRECISION :: TIME15, TIME16
!
      DOUBLE PRECISION :: XYTOL, AMIN_BC
!
      CHARACTER(LEN=80) ::      TITLE
      CHARACTER(LEN=32), DIMENSION (:), ALLOCATABLE :: NOMVAR
!
      INTEGER :: NBVAR, NBVAR2
!
      WRITE(LU,*) 'ENTER GEO FILE FORMAT:'
      READ(LI,*) INFORMAT
      WRITE(LU,*) 'ENTER GEO FILE:'
      READ(LI,*) INPUT_FILE
      WRITE(LU,*) 'ENTER CLI FILE:'
      READ(LI,*) INPUT_CLI_FILE
      WRITE(LU,*) 'ENTER OUTPUT FILE FORMAT:'
      READ(LI,*) OUTFORMAT
      WRITE(LU,*) 'ENTER OUTPUT FILE:'
      READ(LI,*) OUTPUT_FILE
      OUTPUT_CLI_FILE = TRIM(OUTPUT_FILE)//'.CLI'
      OUTPUT_FILE = TRIM(OUTPUT_FILE)//'.SLF'
      WRITE(LU,*) 'INPUT_FILE FORMAT: ',INFORMAT
      WRITE(LU,*) 'INPUT_FILE: ',INPUT_FILE
      WRITE(LU,*) 'INPUT_CLI_FILE: ',INPUT_CLI_FILE
      WRITE(LU,*) 'OUTPUT_FILE FORMAT: ',OUTFORMAT
      WRITE(LU,*) 'OUTPUT_FILE: ',OUTPUT_FILE
      WRITE(LU,*) 'OUTPUT_CLI_FILE: ',OUTPUT_CLI_FILE
!
      CALL CPU_TIME( TIME1 )
!
      TOL=1.D-15
!
      XYTOL=1.D6
!
      NCSIZE=1
!
!     READING ORIGINAL MESH
!
      CALL CPU_TIME ( TIME3 )
!
      WRITE(LU,*) 'BEGINNING THE ORIGINAL MESH'
!
      NCSIZE=1
      NFIC=15
      CALL OPEN_MESH(INFORMAT,INPUT_FILE,NFIC,'READ     ',IERR)
      CALL CHECK_CALL(IERR,'OPEN_MESH:NFIC')
!
      TYP_ELEM = TRIANGLE_ELT_TYPE
      TYP_BND_ELEM = POINT_ELT_TYPE
      CALL READ_MESH_INFO(INFORMAT,NFIC,TITLE,NBVAR,NPOIN,TYP_ELEM,
     &  NELEM,NPTFR,NPTIR,NDP,NPLAN,TYP_BND_ELEM,NELEBD)
!
      ALLOCATE (IKLES(NELEM*NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLES')
!
      ALLOCATE (IPOBO(NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IPOBO')
!
      CALL READ_MESH_CONN(INFORMAT,NFIC,NPOIN,TYP_ELEM,NELEM,NDP,
     & TYP_BND_ELEM,NELEBD,IKLES,IPOBO)
!
      WRITE(LU,*) "NPOIN ",NPOIN, "NELEM ", NELEM
!
      ALLOCATE (X(NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLES')
!
      ALLOCATE (Y(NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLES')
!
      ALLOCATE (ZF(NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLES')
!
      PROJEC = 1
      LATI = 0.D0
      LONGI = 0.D0
      CALL READ_MESH_COORD(INFORMAT,NFIC,X,Y,NPOIN,
     &                     PROJEC,LATI,LONGI)
!
      ! READING THE "FOND"
      DO I=1,5
        CALL FIND_VARIABLE(INFORMAT,NFIC,FOND_NAME(I),ZF,NPOIN,IERR)
        IF(IERR.EQ.0) EXIT
      ENDDO
      IF(IERR.NE.0.AND.I.EQ.5) THEN
        WRITE(LU,*) 'NO BOTTOM IN FILE'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      CALL CPU_TIME ( TIME4 )
!
      WRITE(LU,*) 'END READING THE ORIGINAL MESH'
!
      WRITE(LU,*) 'TIME FOR READING THE ORIGINAL MESH:',
     &            TIME4 - TIME3,'S'
!
!CCCC END READING ORIGINAL MESH
!
!CCCC BEGINNING READING BOUNDARY CONDITIONS
!
      CALL CPU_TIME ( TIME5 )
!
      WRITE(LU,*) 'READING THE BOUNDARY CONDITION FILE'
!
      ALLOCATE (LIHBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIHBOR')
      ALLOCATE (LIUBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIUBOR')
      ALLOCATE (LIVBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIVBOR')
      ALLOCATE (HBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'HBOR')
      ALLOCATE (UBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'UBOR')
      ALLOCATE (VBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VBOR')
      ALLOCATE (AUBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'AUBOR')
      ALLOCATE (TBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TBOR')
      ALLOCATE (ATBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ATBOR')
      ALLOCATE (BTBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'BTBOR')
      ALLOCATE (LITBOR(NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LITBOR')
      ALLOCATE (NBOR(2*NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIHBOR')
!
      CALL OPEN_BND(INFORMAT,INPUT_CLI_FILE,NFIC,'READ     ',IERR)
!     READING NBOR
      CALL GET_BND_NUMBERING(INFORMAT,NFIC,TYP_BND_ELEM,NPTFR,
     &                  NBOR,IERR)
      CALL CHECK_CALL(IERR,'GET_BND_NUMBERING')
!     READING ALL THE BOUNDARY VALUES
      CALL GET_BND_VALUE(INFORMAT,NFIC,TYP_BND_ELEM,NELEBD,
     & LIHBOR,LIUBOR,LIVBOR,HBOR,UBOR,VBOR,AUBOR,.TRUE.,
     & LITBOR,TBOR,ATBOR,BTBOR,NPTFR,NBOR, IERR)
      CALL CHECK_CALL(IERR,'GET_BND_VALUE')

!
      ! CLOSING INPUT FILES
      CALL CLOSE_BND(INFORMAT,NFIC,IERR)
      CALL CHECK_CALL(IERR,'CLOSE_BND:NFIC')
      CALL CLOSE_MESH(INFORMAT,NFIC,IERR)
      CALL CHECK_CALL(IERR,'CLOSE_MESH:NFIC')
!
      CALL CPU_TIME ( TIME6 )
!
      WRITE(LU,*) 'END READING THE BOUNDARY CONDITION FILE'
!
      WRITE(LU,*) 'TIME FOR READING THE BOUNDARY CONDITION FILE:',
     &       TIME6 - TIME5, 'S'
!
!CCCC END READING BOUNDARY CONDITIONS
!
!CCCC CONSTRUCTION OF THE FINER MESH (WITH MIDDLE OF SEGMENTS COUNTED TWICE)
!
      CALL CPU_TIME ( TIME7 )
!
      WRITE(LU,*) 'BEGINNING OF CELL CENTRE AND EDGE CENTRE GENERATION'
!
      ALLOCATE (X_REF(3*NELEM+NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'X_REF')
!
      ALLOCATE (Y_REF(3*NELEM+NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'Y_REF')
!
      ALLOCATE (ZF_REF(3*NELEM+NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ZF_REF')
!
      ALLOCATE (IKLES_REF(2*NELEM*NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLES_REF')
!
      ALLOCATE (XCEN(NELEM),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'XCEN')
!
      ALLOCATE (YCEN(NELEM),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'YCEN')
!
      ALLOCATE (IPOBO_REF(3*NELEM+NPOIN),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IPOBO_REF')
!
      ALLOCATE (I_ELTS_BOUNDARY(2*NPTFR,4),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'I_ELTS_BOUNDARY')
!
      ALLOCATE (NBOR_REF(2*NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'NBOR_REF')
!
      ALLOCATE (I_END_BC(2*NPTFR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'I_END_BC')
!
      NPOIN_BOR=0
!
      DO I=1,NPOIN
!
        X_REF(I)=X(I)
        Y_REF(I)=Y(I)
        ZF_REF(I)=ZF(I)
        IPOBO_REF(I)=IPOBO(I)
!
        IF (IPOBO_REF(I).NE.0) IPOBO_REF(I)=1
!
      ENDDO
!
      DO I=1,NPOIN
!
        IF (IPOBO(I).NE.0) THEN
          NPOIN_BOR=NPOIN_BOR+1
        ENDIF
!
      ENDDO
!
      WRITE(LU,*) 'CHECK NPOIN_BOR = NPTFR',NPOIN_BOR,NPTFR
!
      DO K=1,2*NPTFR
!
        DO L=1,4
!
          I_ELTS_BOUNDARY(K,L)=-9999
!
        ENDDO
!
      ENDDO
!
      DO K=1,NPOIN_BOR
!
        I_ELTS_BOUNDARY(K,1)=NBOR(K)
!
      ENDDO
!
      NPOIN_REF=NPOIN
      NELEM_REF=2*NELEM*NDP
!
      DO I=1,NELEM
!
        IKLES1=IKLES((I-1)*NDP+1)
        IKLES2=IKLES((I-1)*NDP+2)
        IKLES3=IKLES((I-1)*NDP+3)
!
        IKLES_REF((I-1)*NDP+1)=IKLES1
        IKLES_REF((I-1)*NDP+2)=IKLES2
        IKLES_REF((I-1)*NDP+3)=IKLES3
!
        X1=X(IKLES1)
        X2=X(IKLES2)
        X3=X(IKLES3)
!
        Y1=Y(IKLES1)
        Y2=Y(IKLES2)
        Y3=Y(IKLES3)
!
        ZF1=ZF(IKLES1)
        ZF2=ZF(IKLES2)
        ZF3=ZF(IKLES3)
!
!CCCCCC CELL CENTRES
!
        XCEN(I)=(X1+X2+X3)/3.D0
        YCEN(I)=(Y1+Y2+Y3)/3.D0
!
!CCCCCC EGDE MIDDLE
!
        NPOIN_REF=NPOIN_REF+1
!
        X_REF(NPOIN_REF)=0.5D0*(X1+X2)
        Y_REF(NPOIN_REF)=0.5D0*(Y1+Y2)
        ZF_REF(NPOIN_REF)=0.5D0*(ZF1+ZF2)
!
        IKLES_REF(NDP*NELEM+(I-1)*NDP+1)=NPOIN_REF
!
        IF (IPOBO(IKLES1).NE.0.AND.IPOBO(IKLES2).NE.0) THEN
          NPOIN_BOR=NPOIN_BOR+1
          I_ELTS_BOUNDARY(NPOIN_BOR,1)=IKLES_REF(NDP*NELEM+(I-1)*NDP+1)
          I_ELTS_BOUNDARY(NPOIN_BOR,2)=IKLES1
          I_ELTS_BOUNDARY(NPOIN_BOR,3)=IKLES2
          I_ELTS_BOUNDARY(NPOIN_BOR,4)=I
          IPOBO_REF(IKLES_REF(NDP*NELEM+(I-1)*NDP+1))=1
        ENDIF
!
        NPOIN_REF=NPOIN_REF+1
!
        X_REF(NPOIN_REF)=0.5D0*(X2+X3)
        Y_REF(NPOIN_REF)=0.5D0*(Y2+Y3)
        ZF_REF(NPOIN_REF)=0.5D0*(ZF2+ZF3)
!
        IKLES_REF(NDP*NELEM+(I-1)*NDP+2)=NPOIN_REF
!
        IF (IPOBO(IKLES2).NE.0.AND.IPOBO(IKLES3).NE.0) THEN
          NPOIN_BOR=NPOIN_BOR+1
          I_ELTS_BOUNDARY(NPOIN_BOR,1)=IKLES_REF(NDP*NELEM+(I-1)*NDP+2)
          I_ELTS_BOUNDARY(NPOIN_BOR,2)=IKLES2
          I_ELTS_BOUNDARY(NPOIN_BOR,3)=IKLES3
          I_ELTS_BOUNDARY(NPOIN_BOR,4)=I
          IPOBO_REF(IKLES_REF(NDP*NELEM+(I-1)*NDP+2))=1
        ENDIF
!
        NPOIN_REF=NPOIN_REF+1
!
        X_REF(NPOIN_REF)=0.5D0*(X3+X1)
        Y_REF(NPOIN_REF)=0.5D0*(Y3+Y1)
        ZF_REF(NPOIN_REF)=0.5D0*(ZF3+ZF1)
!
        IKLES_REF(NDP*NELEM+(I-1)*NDP+3)=NPOIN_REF
!
        IF (IPOBO(IKLES3).NE.0.AND.IPOBO(IKLES1).NE.0) THEN
          NPOIN_BOR=NPOIN_BOR+1
          I_ELTS_BOUNDARY(NPOIN_BOR,1)=IKLES_REF(NDP*NELEM+(I-1)*NDP+3)
          I_ELTS_BOUNDARY(NPOIN_BOR,2)=IKLES3
          I_ELTS_BOUNDARY(NPOIN_BOR,3)=IKLES1
          I_ELTS_BOUNDARY(NPOIN_BOR,4)=I
          IPOBO_REF(IKLES_REF(NDP*NELEM+(I-1)*NDP+3))=1
        ENDIF
!
      ENDDO
!
      WRITE(LU,*) 'NUMBER OF BOUNDARY NODES OLD/NEW ',NPTFR,NPOIN_BOR
!
!
!!!!! I_ELTS_BOUNDARY : 1     MIDDLE OF A SEGMENT
!!!!! I_ELTS_BOUNDARY : 2, 3  END OF THE SEGMENT
!!!!! I_ELTS_BOUNDARY : 4     ELEMENT INDEX
!
!!!!! UP TO HERE, 6550 GIVES THE RIGHT BOUNDARY NODES, BUT NOT PROPERLY ORDERED.
!
!!!!! NBOR_REF IS NOW BUILT
!
      DO I=1,NPOIN_BOR
!
        NBOR_REF(I)=-9999
!
      ENDDO
!
      IKLES1=0
!
      DO I=1,NPOIN_BOR/2
!
        NBOR_REF(2*I-1)=NBOR(I)
!
        DO J=1,NPOIN_BOR
!
          IF (NBOR_REF(2*I-1).EQ.I_ELTS_BOUNDARY(I,1)) THEN
!
            IKLES1=IKLES1+1
!
            GOTO 1001
!
          ENDIF
!
        ENDDO
!
 1001 CONTINUE
!
      ENDDO
!
      WRITE(LU,*) 'MATCHING NODES ', IKLES1, NPOIN_BOR
!
!CCCC
!
      DO I=1,NPOIN_BOR/2-1
!
        DO J=NPOIN_BOR/2+1,NPOIN_BOR
!
          IF ((I_ELTS_BOUNDARY(J,2).EQ.NBOR_REF(2*I-1)).AND.
     &        (I_ELTS_BOUNDARY(J,3).EQ.NBOR_REF(2*I+1))) THEN
!
            NBOR_REF(2*I)=I_ELTS_BOUNDARY(J,1)
!
          ENDIF
!
          IF ((I_ELTS_BOUNDARY(J,3).EQ.NBOR_REF(2*I-1)).AND.
     &        (I_ELTS_BOUNDARY(J,2).EQ.NBOR_REF(2*I+1))) THEN
!
            NBOR_REF(2*I)=I_ELTS_BOUNDARY(J,1)
!
          ENDIF
!
        ENDDO
!
      ENDDO
!
!CCCC AT THIS STAGE, SOME BOUNDARY NODES ARE NOT TAKEN INTO ACCOUNT
!CCCC THEY ARE LOCATED AT THE END OF THE "BOUNDARY LINES".
!
      I_BC_SING=0
      DO I=1,NPOIN_BOR
!
        IF (NBOR_REF(I).EQ.-9999) THEN
!
          I_BC_SING=I_BC_SING+1
!
        ENDIF
!
      ENDDO
!
      WRITE(LU,*) 'PB IN ',I_BC_SING,'BOUNDARIES'
!
!CCCC SORTIR LES COORDONNEES DES POINTS QUI POSENT PROBLEMES
!CCCC EN COMPARANT X_REF(NBOR_REF(I)) ET X_REF(I_ELTS_BOUNDARY(J,1))
!CCCC CEUX QUI RESTENT SONT LES POINTS A PROBLEMES !!!
!
      ALLOCATE (X_REF_BC(I_BC_SING),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'X_REF_BC')
!
      ALLOCATE (Y_REF_BC(I_BC_SING),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'Y_REF_BC')
!
      ALLOCATE (I_REF_BC(I_BC_SING),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'I_REF_BC')
!
      DO I=1,NPOIN_BOR
!
        I_END_BC(I)=I_ELTS_BOUNDARY(I,1)
!
        DO J=1,NPOIN_BOR
!
          IF (NBOR_REF(J).NE.-9999) THEN
!
            XDIFF=ABS(X_REF(NBOR_REF(J))-
     &                X_REF(I_ELTS_BOUNDARY(I,1)))
            YDIFF=ABS(Y_REF(NBOR_REF(J))-
     &                Y_REF(I_ELTS_BOUNDARY(I,1)))
!
            IF (XDIFF.LE.TOL.AND.YDIFF.LE.TOL) THEN
!
              I_END_BC(I)=-9999
!
              GOTO 9876
!
            ENDIF
!
          ENDIF
!
        ENDDO
!
 9876   CONTINUE
!
      ENDDO
!
      L=0
      DO I=1,NPOIN_BOR
!
        IF (I_END_BC(I).NE.-9999) THEN
!
          L=L+1
!
          I_REF_BC(L)=I_END_BC(I)
          X_REF_BC(L)=X_REF(I_END_BC(I))
          Y_REF_BC(L)=Y_REF(I_END_BC(I))
!
        ENDIF
!
      ENDDO
!
!!!!! 1:X1+X2, 2:X2+X3, 3=X3+X1
!
      DO I=1,NPOIN_BOR
!
        IF (NBOR_REF(I).EQ.-9999) THEN
!
          XYTOL=1.D99
!
          DO J=1,I_BC_SING
!
            XDIFF=ABS(X_REF_BC(J)-X_REF(NBOR_REF(I-1)))
            YDIFF=ABS(Y_REF_BC(J)-Y_REF(NBOR_REF(I-1)))
!
            XYDIFF=XDIFF*XDIFF+YDIFF*YDIFF
!
            AMIN_BC=MIN(XYDIFF,XYTOL)
!
            IF (ABS(XYDIFF-AMIN_BC).LE.TOL) THEN
!
              XYTOL=AMIN_BC
!
              M=I_REF_BC(J)
!
            ENDIF
!
          ENDDO
!
          NBOR_REF(I)=M
!
        ENDIF
!
      ENDDO
!
      CALL CPU_TIME ( TIME8 )
!
      WRITE(LU,*) 'END OF CELL CENTRE AND EDGE CENTRE GENERATION'
!
      WRITE(LU,*) 'TIME FOR CREATING CELL CENTRES AND EDGE CENTRES: ',
     &      TIME8 - TIME7,'S'
!
      CALL CPU_TIME ( TIME9 )
!
      WRITE(LU,*) 'BEGINNING OF RAW FINE GRID GENERATION'
!
      ALLOCATE (IKLES_RAW(4*NDP*NELEM),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLES_RAW')
!
!CCCCC FIRST RAW MESH
!
      DO I=1,NELEM
!
        J=4*I-3
        IKLES_RAW((J-1)*NDP+1)=IKLES_REF((I-1)*NDP+1)
        IKLES_RAW((J-1)*NDP+2)=IKLES_REF(NDP*NELEM+(I-1)*NDP+1)
        IKLES_RAW((J-1)*NDP+3)=IKLES_REF(NDP*NELEM+(I-1)*NDP+3)
!
        J=4*I-2
        IKLES_RAW((J-1)*NDP+1)=IKLES_REF((I-1)*NDP+2)
        IKLES_RAW((J-1)*NDP+2)=IKLES_REF(NDP*NELEM+(I-1)*NDP+2)
        IKLES_RAW((J-1)*NDP+3)=IKLES_REF(NDP*NELEM+(I-1)*NDP+1)
!
        J=4*I-1
        IKLES_RAW((J-1)*NDP+1)=IKLES_REF((I-1)*NDP+3)
        IKLES_RAW((J-1)*NDP+2)=IKLES_REF(NDP*NELEM+(I-1)*NDP+3)
        IKLES_RAW((J-1)*NDP+3)=IKLES_REF(NDP*NELEM+(I-1)*NDP+2)
!
        J=4*I
        IKLES_RAW((J-1)*NDP+1)=IKLES_REF(NDP*NELEM+(I-1)*NDP+1)
        IKLES_RAW((J-1)*NDP+2)=IKLES_REF(NDP*NELEM+(I-1)*NDP+2)
        IKLES_RAW((J-1)*NDP+3)=IKLES_REF(NDP*NELEM+(I-1)*NDP+3)
!
      ENDDO
!
      CALL CPU_TIME ( TIME10 )
!
      WRITE(LU,*) 'END OF RAW FINE GRID GENERATION'
!
      WRITE(LU,*) 'TIME FOR GENERATING THE RAW FINE GRID: ',
     &      TIME10 - TIME9,'S'
!
!CCCC NEEDS SOME SORTING
!
      CALL CPU_TIME ( TIME11 )
!
      WRITE(LU,*) 'BEGINNING OF SORTING STAGE'
!
      ALLOCATE (R_XQ_REF(NPOIN_REF),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'R_XQ_REF')
!
      ALLOCATE (I_X_REF_SORTED2ORI(NPOIN_REF),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'I_X_REF_SORTED2ORI')
!
      DO I=1,NPOIN_REF
!
        R_XQ_REF(I)%R1=X_REF(I)
        R_XQ_REF(I)%R2=Y_REF(I)
!
      ENDDO
!
!
      CALL SHELL(NPOIN_REF,R_XQ_REF,I_X_REF_SORTED2ORI)
!
      CALL CPU_TIME( TIME12 )
!
      WRITE(LU,*) 'TIME IN SHELL (X) ',TIME12-TIME11,'S'
!
      CALL CPU_TIME( TIME13 )
!
!
      WRITE(LU,*) 'END OF SORTING STAGE'
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      CALL CPU_TIME( TIME13 )
!
      WRITE(LU,*) 'BEGINNING OF RAW FINE GRID SORTING'
!
      ALLOCATE (X_REF_X_SORTED(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING X_REF_X_SORTED'
!
      ALLOCATE (Y_REF_X_SORTED(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING Y_REF_X_SORTED'
!
      ALLOCATE (ZF_REF_X_SORTED(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING ZF_REF_X_SORTED'
!
      ALLOCATE (IPOBO_REF_X_SORTED(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING IPOBO_REF_X_SORTED'
!
      ALLOCATE (I_X_REF_ORI2SORTED(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING I_X_REF_ORI2SORTED'
!
      ALLOCATE (NBOR_REF_X_SORTED(NPOIN_BOR),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING NBOR_REF_X_SORTED'
!
      DO I=1,NPOIN_REF
!
        X_REF_X_SORTED(I)=X_REF(I_X_REF_SORTED2ORI(I))
        Y_REF_X_SORTED(I)=Y_REF(I_X_REF_SORTED2ORI(I))
        ZF_REF_X_SORTED(I)=ZF_REF(I_X_REF_SORTED2ORI(I))
        IPOBO_REF_X_SORTED(I)=IPOBO_REF(I_X_REF_SORTED2ORI(I))
!
      ENDDO
!
      DO I=1,NPOIN_REF
!
        I_X_REF_ORI2SORTED(I_X_REF_SORTED2ORI(I))=I
!
      ENDDO
!
      DO I=1,NPOIN_BOR
!
        NBOR_REF_X_SORTED(I)=I_X_REF_ORI2SORTED(NBOR_REF(I))
!
      ENDDO
!
      ALLOCATE (IKLES_REF_X_SORTED(4*NDP*NELEM),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING IKLES_REF_X_SORTED'
!
      DO I=1,4*NELEM
!
        IKLES1=IKLES_RAW((I-1)*NDP+1)
        IKLES_REF_X_SORTED((I-1)*NDP+1)=I_X_REF_ORI2SORTED(IKLES1)
!
        IKLES2=IKLES_RAW((I-1)*NDP+2)
        IKLES_REF_X_SORTED((I-1)*NDP+2)=I_X_REF_ORI2SORTED(IKLES2)
!
        IKLES3=IKLES_RAW((I-1)*NDP+3)
        IKLES_REF_X_SORTED((I-1)*NDP+3)=I_X_REF_ORI2SORTED(IKLES3)
!
      ENDDO
!
      WRITE(LU,*) 'BEGINNING X_REF_X_SORTED(I)<X_REF_X_SORTED(I-1)'
!
      DO I=2,NPOIN_REF
!
        IF (X_REF_X_SORTED(I).LT.X_REF_X_SORTED(I-1)) THEN
          WRITE(LU,*) 'PB HERE X_REF_X_SORTED(I)<X_REF_X_SORTED(I-1)',I
          WRITE(LU,*) X_REF_X_SORTED(I-1),I-1
          WRITE(LU,*) X_REF_X_SORTED(I),I
        ENDIF
!
      ENDDO
!
      WRITE(LU,*) 'END X_REF_X_SORTED(I)<X_REF_X_SORTED(I-1)'
!
      WRITE(LU,*) 'END OF RAW FINE GRID SORTING'
!
      CALL CPU_TIME ( TIME14 )
!
      WRITE(LU,*)
     & 'TIME TO GENERATE THE SORTED FINE GRID (NO COMPRESSION)',
     &        TIME14 - TIME13,'S'
!
!CCCC
!
      CALL CPU_TIME( TIME15 )
!
      WRITE(LU,*) 'BEGINNING OF FINE SORTED COMPRESSED GRID GENERATION'
!
      ALLOCATE (I_ORI2MERGED_X(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING I_ORI2MERGED_X'
!
      ALLOCATE (I_MERGED2ORI_X(NPOIN_REF),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING I_MERGED2ORI_X'
!
      DO I=1,NPOIN_REF
!
        I_ORI2MERGED_X(I)=-9999
!
        I_MERGED2ORI_X(I)=-9999
!
      ENDDO
!
      NPOIN_REF2_X=0
      NPOIN_REF3_X=0
!
      DO I=2,NPOIN_REF
!
        XDIFF=X_REF_X_SORTED(I)-X_REF_X_SORTED(I-1)
        YDIFF=Y_REF_X_SORTED(I)-Y_REF_X_SORTED(I-1)
!
        IF (XDIFF.LE.TOL.AND.YDIFF.LE.TOL) THEN
!
          NPOIN_REF2_X=NPOIN_REF2_X+1
!
        ELSE
!
          NPOIN_REF3_X=NPOIN_REF3_X+1
!
          I_ORI2MERGED_X(NPOIN_REF3_X)=I_X_REF_SORTED2ORI(I-1)
!
          I_MERGED2ORI_X(I-1)=NPOIN_REF3_X
!
        ENDIF
!
      ENDDO
!
      I=NPOIN_REF
      XDIFF=X_REF_X_SORTED(I)-X_REF_X_SORTED(I-1)
      YDIFF=Y_REF_X_SORTED(I)-Y_REF_X_SORTED(I-1)
!
      IF (XDIFF.LE.TOL.AND.YDIFF.LE.TOL) THEN
!
      ELSE
!
        NPOIN_REF3_X=NPOIN_REF3_X+1
!
        I_ORI2MERGED_X(NPOIN_REF3_X)=I_X_REF_SORTED2ORI(I)
!
        I_MERGED2ORI_X(I)=NPOIN_REF3_X
!
      ENDIF
!
      DO I=1,NPOIN_REF
        IF (I_MERGED2ORI_X(I).EQ.-9999) THEN
          I_MERGED2ORI_X(I)=I_MERGED2ORI_X(I+1)
        ENDIF
      ENDDO
!
      WRITE(LU,*) 'NPOIN_REF2 X_REFINED I-1 :',NPOIN_REF2_X,NPOIN_REF,
     &       NPOIN_REF3_X,NPOIN_REF2_X+NPOIN_REF3_X
!
!CCCC IKLES
!
      ALLOCATE (IKLES_REFINED_X(4*NDP*NELEM),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING IKLES_REFINED_X'
!
      DO I=1,4*NELEM
!
        IKLES1=IKLES_REF_X_SORTED((I-1)*NDP+1)
        IKLES_REFINED_X((I-1)*NDP+1)=I_MERGED2ORI_X(IKLES1)
!
        IKLES2=IKLES_REF_X_SORTED((I-1)*NDP+2)
        IKLES_REFINED_X((I-1)*NDP+2)=I_MERGED2ORI_X(IKLES2)
!
        IKLES3=IKLES_REF_X_SORTED((I-1)*NDP+3)
        IKLES_REFINED_X((I-1)*NDP+3)=I_MERGED2ORI_X(IKLES3)
!
      ENDDO
!
!CCCC OTHER QUANTITIES
!
      ALLOCATE (X_REFINED_X(NPOIN_REF3_X),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING X_REFINED_X'
!
      ALLOCATE (Y_REFINED_X(NPOIN_REF3_X),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING Y_REFINED_X'
!
      ALLOCATE (ZF_REFINED_X(NPOIN_REF3_X),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING ZF_REFINED_X'
!
      ALLOCATE (IPOBO_REFINED_X(NPOIN_REF3_X),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING IPOBO_REFINED_X'
!
      DO I=1,NPOIN_REF
!
        J=I_MERGED2ORI_X(I)
!
        X_REFINED_X(J)=X_REF_X_SORTED(I)
        Y_REFINED_X(J)=Y_REF_X_SORTED(I)
        ZF_REFINED_X(J)=ZF_REF_X_SORTED(I)
        IPOBO_REFINED_X(J)=IPOBO_REF_X_SORTED(I)
!
      ENDDO
!
      ALLOCATE (NBOR_REFINED_X(NPOIN_BOR),STAT=IERR)
      IF (IERR.GT.0) WRITE(LU,*) 'PB ALLOCATING NBOR_REFINED_X'
!
      DO I=1,NPOIN_BOR
!
        NBOR_REFINED_X(I)=I_MERGED2ORI_X(NBOR_REF_X_SORTED(I))
!
      ENDDO
!
      CALL CPU_TIME( TIME16 )
!
      WRITE(LU,*) 'END OF FINE SORTED COMPRESSED GRID GENERATION'
!
      WRITE(LU,*) 'TIME FOR FINE SORTED COMPRESSED GRID GENERATION',
     &   TIME16 - TIME15, 'S'
!
!CCCC BOUNDARY_FILE
!
      ALLOCATE (LIHBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIHBOR_REF')
      ALLOCATE (LIUBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIUBOR_REF')
      ALLOCATE (LIVBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LIVBOR_REF')
      ALLOCATE (HBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'HBOR_REF')
      ALLOCATE (UBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'UBOR_REF')
      ALLOCATE (VBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VBOR_REF')
      ALLOCATE (AUBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'AUBOR_REF')
      ALLOCATE (TBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'TBOR_REF')
      ALLOCATE (ATBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'ATBOR_REF')
      ALLOCATE (BTBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'BTBOR_REF')
      ALLOCATE (LITBOR_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'LITBOR_REF')
      ALLOCATE (CHECK_REF(NPOIN_BOR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'CHECK_REF')
!
      WRITE(LU,*) 'PASSING THROW HERE6'
      NCLI=18
      OPEN(UNIT=NCLI,FILE=OUTPUT_CLI_FILE)
!
      DO I=1,NPTFR
!
        LIHBOR_REF(2*I-1)=LIHBOR(I)
        LIUBOR_REF(2*I-1)=LIUBOR(I)
        LIVBOR_REF(2*I-1)=LIVBOR(I)
!
        HBOR_REF(2*I-1)=HBOR(I)
        UBOR_REF(2*I-1)=UBOR(I)
        VBOR_REF(2*I-1)=VBOR(I)
!
        HBOR_REF(2*I)=HBOR(I)
        UBOR_REF(2*I)=UBOR(I)
        VBOR_REF(2*I)=VBOR(I)
!
        AUBOR_REF(2*I-1)=AUBOR(I)
!
        AUBOR_REF(2*I)=AUBOR(I)
!
        LITBOR_REF(2*I-1)=LITBOR(I)
!
        TBOR_REF(2*I-1)=TBOR(I)
        ATBOR_REF(2*I-1)=ATBOR(I)
        BTBOR_REF(2*I-1)=BTBOR(I)
!
        TBOR_REF(2*I)=TBOR(I)
        ATBOR_REF(2*I)=ATBOR(I)
        BTBOR_REF(2*I)=BTBOR(I)
!
      ENDDO
      WRITE(LU,*) 'PASSING THROW HERE5'
!
      DO I=1,NPTFR-1
!
        IF (LIHBOR_REF(2*I-1).EQ.5.AND.LIHBOR_REF(2*I+1).EQ.5) THEN
!
          IF ((LIUBOR_REF(2*I-1).EQ.4.AND.LIUBOR_REF(2*I+1).EQ.4).AND.
     &        (LIVBOR_REF(2*I-1).EQ.4.AND.LIVBOR_REF(2*I+1).EQ.4)) THEN
!
            LIHBOR_REF(2*I)=5
            LIUBOR_REF(2*I)=4
            LIVBOR_REF(2*I)=4
!
            LITBOR_REF(2*I)=4
!
          ENDIF
!
        ELSEIF (LIHBOR_REF(2*I-1).EQ.4.AND.LIHBOR_REF(2*I+1).EQ.4) THEN
!
          IF ((LIUBOR_REF(2*I-1).EQ.5.AND.LIUBOR_REF(2*I+1).EQ.5).AND.
     &        (LIVBOR_REF(2*I-1).EQ.5.AND.LIVBOR_REF(2*I+1).EQ.5)) THEN
!
            LIHBOR_REF(2*I)=4
            LIUBOR_REF(2*I)=5
            LIVBOR_REF(2*I)=5
!
            LITBOR_REF(2*I)=4
!
          ENDIF
!
        ELSE
!
          LIHBOR_REF(2*I)=2
          LIUBOR_REF(2*I)=2
          LIVBOR_REF(2*I)=2
!
          LITBOR_REF(2*I)=2
!
        ENDIF
!
      ENDDO
      WRITE(LU,*) 'PASSING THROW HERE4'
!
      I=NPTFR
      LIHBOR_REF(2*I)=2
      LIUBOR_REF(2*I)=2
      LIVBOR_REF(2*I)=2
!
      LITBOR_REF(2*I)=2
!
      WRITE(LU,*) 'PASSING THROW HERE3'
      DO K=1,NPOIN_BOR
!
        CHECK_REF(K)=K
!
        WRITE(NCLI,4000) LIHBOR_REF(K),LIUBOR_REF(K),
     &                LIVBOR_REF(K),
     &                HBOR_REF(K),UBOR_REF(K),VBOR_REF(K),
     &                AUBOR_REF(K),LITBOR_REF(K),
     &                TBOR_REF(K),ATBOR_REF(K),BTBOR_REF(K),
     &                NBOR_REFINED_X(K),CHECK_REF(K)
!
      ENDDO
      WRITE(LU,*) 'PASSING THROW HERE2'
!
 4000       FORMAT (1X,I2,1X,2(I1,1X),3(F12.6,1X),1X,
     &           F12.6,3X,I1,1X,3(F12.6,1X),1I15,1X,1I15)
!CCCC     &           1X,I7,1X,2(F27.15,1X),I6)
!
      CLOSE(UNIT=NCLI)
      WRITE(LU,*) 'PASSING THROW HERE1'
!
!CCCC SORTIR LE FICHIER AU FORMAT GEO.
!
!! OPENING SELAFIN_FILE
      NOUT = 666
      CALL OPEN_MESH(OUTFORMAT,OUTPUT_FILE,NOUT,'WRITE    ',IERR)
      CALL CHECK_CALL(IERR,'OPEN_MESH:NOUT')

!!----------------------------------------------------------------------
!! HEADER OF THE OUTPUT FILE
!!----------------------------------------------------------------------
      TITLE(73:80)='SERAFIND'
      NBVAR = 1
      ALLOCATE (NOMVAR(NBVAR),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'NOMVAR')
      NOMVAR(1)='BOTTOM'      !!! ARTIFICIAL BATHYMETRY
!
      CALL SET_HEADER(OUTFORMAT,NOUT,TITLE,NBVAR,NOMVAR,IERR)
      CALL CHECK_CALL(IERR,'SET_HEADER')
!
!!----------------------------------------------------------------------
!! MESH OF THE OUTPUT FILE
!!----------------------------------------------------------------------

      DATE = (/0,0,0/)
      TIME = (/0,0,0/)
      ALLOCATE(IKLE(4*NELEM*NDP),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'IKLE')
      DO I=1,4*NELEM
        DO J=1,NDP
          IKLE((J-1)*NELEM*4+I) = IKLES_REFINED_X((I-1)*NDP+J)
        ENDDO
      ENDDO
      CALL SET_MESH(OUTFORMAT,NOUT,2,TYP_ELEM,NDP,NPOIN_BOR,0,4*NELEM,
     &              NPOIN_REF3_X,IKLE,IPOBO_REFINED_X,
     &              IPOBO_REFINED_X,X_REFINED_X,Y_REFINED_X,NPLAN,
     &              DATE,TIME,IERR)
      CALL CHECK_CALL(IERR,'SET_MESH')
      DEALLOCATE(IKLE)

!!----------------------------------------------------------------------
!! ADDING BOTTOM
!!----------------------------------------------------------------------
      CALL ADD_DATA(OUTFORMAT,NOUT,NOMVAR(1),0.D0,0,.TRUE.,ZF_REFINED_X,
     &              NPOIN_REF3_X,IERR)
      CALL CHECK_CALL(IERR,'ADD_DATA')
!!----------------------------------------------------------------------
!! FINISH THE GEO FILE
!!----------------------------------------------------------------------
      CALL CLOSE_MESH(OUTFORMAT,NOUT,IERR)
      CALL CHECK_CALL(IERR,'CLOSE_MESH:NOUT')
      DEALLOCATE (NOMVAR)
!
      CALL CPU_TIME ( TIME2 )
!
      WRITE(LU,*) 'END OF THE PROGRAM'
      WRITE(LU,*) 'THE PROGRAM RAN IN',TIME2-TIME1,'S'
!
      STOP
      END
