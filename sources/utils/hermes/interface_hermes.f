!                    ***********************
                     MODULE INTERFACE_HERMES
!                    ***********************
!
!
!***********************************************************************
! HERMES VERSION 7.0                                     26/12/2013
!***********************************************************************
!
!brief    INTERFACES OF HERMES PUBLIC SUBROUTINES
!+
!
!history  Y. AUDOUIN (EDF R&D, LNHE)
!+        24/03/2014
!+        V7P0
!+     Creation of the file.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      INTERFACE
!
!-----------------------------------------------------------------------
!
!     DEFINITION OF INTERFACES
!
      SUBROUTINE OPEN_MESH(FFORMAT,FNAME,FID,OPENMODE,IERR)
      CHARACTER(LEN=8), INTENT(INOUT) :: FFORMAT
      INTEGER, INTENT(OUT) :: FID
      CHARACTER(LEN=*), INTENT(IN) :: FNAME
      CHARACTER(LEN=9), INTENT(IN)     :: OPENMODE
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE CLOSE_MESH(FFORMAT,FID,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE OPEN_BND(FFORMAT,FNAME,FID,OPENMODE,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      CHARACTER(LEN=*), INTENT(IN) :: FNAME
      CHARACTER(LEN=9), INTENT(IN)     :: OPENMODE
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE CLOSE_BND(FFORMAT,FID,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Reading section
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     --Mesh
!
      SUBROUTINE GET_MESH_TITLE(FFORMAT,FID,TITLE,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      CHARACTER(LEN=80), INTENT(OUT) :: TITLE
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_DATE(FFORMAT,FID,DATE,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(INOUT) :: DATE(6)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE

      SUBROUTINE GET_MESH_NELEM(FFORMAT,FID,TYP_ELEM,NELEM,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYP_ELEM
      INTEGER, INTENT(OUT) :: NELEM
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_NPOIN_PER_ELEMENT(FFORMAT,FID,TYP_ELEM,
     &                                      NDP,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYP_ELEM
      INTEGER, INTENT(OUT) :: NDP
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_CONNECTIVITY(FFORMAT,FID,TYP_ELEM,IKLE,
     &                                 NELEM,NDP,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYP_ELEM
      INTEGER, INTENT(IN) :: NELEM
      INTEGER, INTENT(IN) :: NDP
      INTEGER, INTENT(INOUT) :: IKLE(NELEM*NDP)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE

      SUBROUTINE GET_MESH_NPOIN(FFORMAT,FID,TYP_ELEM,NPOIN,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYP_ELEM
      INTEGER, INTENT(OUT) :: NPOIN
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_NPLAN(FFORMAT,FID,NPLAN,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: NPLAN
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_DIMENSION(FFORMAT,FID,NDIM,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: NDIM
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_COORD(FFORMAT,FID,JDIM,NDIM,NPOIN,COORD,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID,JDIM,NDIM,NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: COORD(NPOIN)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE GET_MESH_L2G_NUMBERING(FFORMAT,FID,KNOLG,NPOIN,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(INOUT) :: KNOLG(NPOIN)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_MESH_NPTIR(FFORMAT,FID,NPTIR,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: NPTIR
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
!     --Boundary conditions
!
      SUBROUTINE GET_BND_IPOBO(FFORMAT,FID,NPOIN,NELEBD,TYP_ELEM_BND,
     & IPOBO,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID,NPOIN,NELEBD,TYP_ELEM_BND
      INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE GET_BND_NUMBERING(FFORMAT,FID,TYP_ELEM_BND,NPTFR,
     & NBOR,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID,NPTFR,TYP_ELEM_BND
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE GET_BND_CONNECTIVITY(FFORMAT,FID,TYP_BND_ELEM,NELEBD,
     &                                NDP,IKLE_BND,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID,TYP_BND_ELEM,NELEBD,NDP
      INTEGER, INTENT(INOUT) :: IKLE_BND(NDP*NELEBD)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE GET_BND_NPOIN(FFORMAT,FID,TYPE_BND_ELEM,NPTFR,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYPE_BND_ELEM
      INTEGER, INTENT(OUT) :: NPTFR
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE GET_BND_NELEM(FFORMAT,FID,TYPE_BND_ELEM,NELEM,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYPE_BND_ELEM
      INTEGER, INTENT(OUT) :: NELEM
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE GET_BND_VALUE
     &(FFORMAT,FID,TYP_BND_ELEM,NELEBD,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,TRAC,
     & LITBOR,TBOR,ATBOR,BTBOR,NPTFR,NBOR, IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: TYP_BND_ELEM
      INTEGER, INTENT(IN) :: NELEBD
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER,          INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER,          INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR),ATBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(NPTFR)
      LOGICAL,          INTENT(IN)    :: TRAC
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
!     --Data
!
      SUBROUTINE GET_DATA_NVAR(FFORMAT,FID,NVAR,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: NVAR
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_DATA_VAR_LIST(FFORMAT,FID,NVAR,VARLIST,UNITLIST,
     &                             IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: NVAR
      CHARACTER(LEN=16), INTENT(INOUT) :: VARLIST(NVAR)
      CHARACTER(LEN=16), INTENT(INOUT) :: UNITLIST(NVAR)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_DATA_NTIMESTEP(FFORMAT,FID,NTIMESTEP,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(OUT) :: NTIMESTEP
      INTEGER, INTENT(INOUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_DATA_TIME(FFORMAT,FID,RECORD,TIME,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: RECORD
      DOUBLE PRECISION, INTENT(OUT) :: TIME
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
      SUBROUTINE GET_DATA_VALUE(FFORMAT,FID,RECORD,VAR_NAME,
     &                          RES_VALUE,N,IERR)
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN) :: FID
      INTEGER, INTENT(IN) :: RECORD
      CHARACTER(LEN=16), INTENT(IN) :: VAR_NAME
      INTEGER, INTENT(IN) :: N
      DOUBLE PRECISION, INTENT(INOUT) :: RES_VALUE(N)
      INTEGER, INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE LIT
     &( X , W , I , C , NVAL , TYPE , CANAL , STD2 , ISTAT )
      INTEGER, INTENT(IN)             :: NVAL,CANAL
      INTEGER, INTENT(INOUT)          :: ISTAT
      CHARACTER(LEN=*), INTENT(IN)       :: TYPE,STD2
      INTEGER, INTENT(INOUT)          :: I(NVAL)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NVAL)
      REAL, INTENT(INOUT)             :: W(NVAL)
      CHARACTER(LEN=*), INTENT(INOUT)    :: C
      END SUBROUTINE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     Writing section
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      SUBROUTINE SET_HEADER
     &(FFORMAT,FILE_ID,TITLE,NVAR,VAR_NAME,IERR)
      CHARACTER(LEN=8),                 INTENT(IN)  :: FFORMAT
      INTEGER,                          INTENT(IN)  :: FILE_ID
      CHARACTER(LEN=80),                INTENT(IN)  :: TITLE
      INTEGER,                          INTENT(IN)  :: NVAR
      CHARACTER(LEN=32),                INTENT(IN)  :: VAR_NAME(NVAR)
      INTEGER,                          INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE SET_MESH(FFORMAT,FILE_ID,MESH_DIM,TYPELM,NDP,NPTFR,
     &                    NPTIR,NELEM,NPOIN,IKLES,IPOBO,
     &                    KNOLG,X,Y,NPLAN,DATE,TIME,
     &                    IERR,Z)
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FILE_ID,NPLAN
      INTEGER,          INTENT(IN)  :: DATE(3)
      INTEGER,          INTENT(IN)  :: TIME(3)
      INTEGER,          INTENT(IN)  :: MESH_DIM
      INTEGER,          INTENT(IN)  :: TYPELM
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: NPTFR
      INTEGER,          INTENT(IN)  :: NPTIR
      INTEGER,          INTENT(IN)  :: NELEM
      INTEGER,          INTENT(IN)  :: NPOIN
      INTEGER,          INTENT(IN)  :: IKLES(NELEM*NDP)
      INTEGER,          INTENT(IN)  :: IPOBO(*)
      INTEGER,          INTENT(IN)  :: KNOLG(*)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN),Y(NPOIN)
      INTEGER,          INTENT(OUT) :: IERR
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: Z(*)
      END SUBROUTINE
!
      SUBROUTINE ADD_DATA(FFORMAT,FILE_ID,VAR_NAME,TIME,RECORD,
     &                    FIRST_VAR,VAR_VALUE,N,IERR)
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FILE_ID,N
      CHARACTER(LEN=32), INTENT(IN)  :: VAR_NAME
      DOUBLE PRECISION,  INTENT(IN)  :: TIME
      INTEGER,           INTENT(IN)  :: RECORD
      LOGICAL,           INTENT(IN)  :: FIRST_VAR
      DOUBLE PRECISION,  INTENT(IN)  :: VAR_VALUE(N)
      INTEGER,           INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE UPDATE_DATA_MESH(FFORMAT,FILE_ID,TIME,RECORD,
     &                            NB_DIM_MESH,NPOIN,COORD,IERR)
      CHARACTER(LEN=8),  INTENT(IN)  :: FFORMAT
      INTEGER,           INTENT(IN)  :: FILE_ID
      DOUBLE PRECISION,  INTENT(IN)  :: TIME
      INTEGER,           INTENT(IN)  :: RECORD
      INTEGER,           INTENT(IN)  :: NB_DIM_MESH, NPOIN
      DOUBLE PRECISION,  INTENT(IN)  :: COORD(NB_DIM_MESH*NPOIN)
      INTEGER,           INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE SET_BND
     &(FFORMAT,FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     & LITBOR,TBOR,ATBOR,BTBOR,IERR)
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE(NELEBD*NDP)
      INTEGER,          INTENT(IN)  :: LIUBOR(NELEBD),LIVBOR(NELEBD)
      INTEGER,          INTENT(IN)  :: LIHBOR(NELEBD),LITBOR(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: UBOR(NELEBD),VBOR(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: HBOR(NELEBD),CHBORD(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: TBOR(NELEBD),ATBOR(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: BTBOR(NELEBD)
      INTEGER,          INTENT(OUT) :: IERR
      END SUBROUTINE
!
      SUBROUTINE ECRI2
     &(X , I , C , NVAL , TYPE , CANAL , STD , ISTAT)
      INTEGER, INTENT(IN) :: NVAL,CANAL
      DOUBLE PRECISION, INTENT(IN) :: X(NVAL)
      INTEGER, INTENT(IN) :: I(NVAL)
      CHARACTER(LEN=*), INTENT(IN) :: TYPE,STD,C
      INTEGER, INTENT(OUT) :: ISTAT
      END SUBROUTINE
!
      END INTERFACE
!
!=======================================================================
!
      END MODULE INTERFACE_HERMES
