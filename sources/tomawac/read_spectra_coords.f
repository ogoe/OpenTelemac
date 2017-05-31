!                    ******************************
                     SUBROUTINE READ_SPECTRA_COORDS
!                    ******************************
!     
     &     (FID,NP,IP,XP,YP)
!     
!***********************************************************************
!     TOMAWAC   V7P3                                   21/02/2017
!***********************************************************************
!
!     brief    READS A LIST OF COORDINATES FROM AN EXTERNAL TEXT FILE
!     +        AND ADDS THEM TO THE EXISTING COORDINATES
!
!     history  A. JOLY (EDF - LNHE)
!     +        21/02/2017
!     +        V5P0
!     +   CREATED

!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     | AT             |-->| COMPUTATION TIME
!     | AUXIL          |<->| DIRECTIONAL SPECTRUM WORK TABLE
!     | BINSCO         |-->| SPECTRUM FILE FORMAT
!     | DATE           |-->| START DATE
!     | DEBRES         |-->| LOGICAL INDICATING THE FIRST TIME STEP TO PRINT
!     | F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!     | FREQ           |-->| DISCRETIZED FREQUENCIES
!     | INUTIL         |<->| WORK TABLE
!     | ISLEO          |-->| ARRAY OF LOGICAL
!     | KNOLG          |-->| ARRAY LINKING LOCAL TO GLOBAL INDEXES IN PARALL
!     | NF             |-->| NUMBER OF FREQUENCIES
!     | NK             |-->| DUMMY VARIABLE
!     | NLEO           |-->| NUMBER OF SPECTRUM PRINTOUT POINTS
!     | NOLEO          |-->| INDEX ARRAY OF SPECTRUM PRINTOUT POINTS
!     | NPLAN          |-->| NUMBER OF DIRECTIONS
!     | NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!     | NSCO           |-->| LOGICAL UNIT NUMBER OF THE PUNCTUAL RESULTS FILE
!     | TETA           |-->| DISRETIZED DIRECTION
!     | TIME           |-->| START TIME
!     | TITCAS         |-->| TITLE
!     | TISPEF         |-->| NAME OF THE 1D SPECTRA RESULTS FILE
!     | NSPE           |-->| LOGICAL UNIT NUMBER FOR THE 1D SPECTRA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!     
!     
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      INTEGER, INTENT(IN)                         :: FID
      INTEGER, INTENT(INOUT)                      :: NP
      INTEGER, ALLOCATABLE, INTENT(INOUT)         :: IP(:)
      DOUBLE PRECISION,ALLOCATABLE, INTENT(INOUT) :: XP(:),YP(:)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!      INTEGER  ISTAT , II    , JF    , K    , II_ALL
!      INTEGER  KAMP1 , KAMP2 , KAMP3 , KAMP4 , KAMP5 , KAMP6 , ILEO
!      INTEGER  IBID(1), NELEM, NPSPE
!      CHARACTER(LEN=72) C
!      CHARACTER(LEN=32) TEXTE(NLEO)
!      CHARACTER(LEN=6)  NUM
!      CHARACTER(LEN=2)  CC
!      CHARACTER(LEN=1)  C1,C2,C3,C4,C5,C6
!      TYPE(BIEF_MESH) MESHF
!      LOGICAL         SORLEO(NLEO)
!      DOUBLE PRECISION DTETAR
!      REAL W(1)
!      CHARACTER(LEN=11) EXTENS
!      EXTERNAL          EXTENS
!      INTEGER :: ID
!!
!      INTEGER  P_IMAX,P_ISUM
!      EXTERNAL P_IMAX,P_ISUM
!!
!      DOUBLE PRECISION, ALLOCATABLE :: F_INTF(:,:)
       INTEGER I
       INTEGER I_GLO
       DOUBLE PRECISION NE,ZP
!     
!-----------------------------------------------------------------------
!
! CHECK TO SEE IF THE PRINTOUT POINTS ARE ONLY DEFINED ONE WAY
!
      IF(NP.GT.0)THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) ''
          WRITE(LU,*) '**************************************'
          WRITE(LU,*) ' ERREUR : POINTS DE SORTIES DU SPECTRE'
          WRITE(LU,*) ' LES POINTS DE SORTIES SONT DEFINIS A'
          WRITE(LU,*) ' TRAVERS DES MOTS-CLEFS ET UN FICHIER'
          WRITE(LU,*) '**************************************'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) ''
          WRITE(LU,*) '**************************************'
          WRITE(LU,*) '   ERROR : SPECTRA PRINTOUT POINTS'
          WRITE(LU,*) '   THE PRINTOUT POINTS ARE DEFINED'
          WRITE(LU,*) '   USING BOTH KEYWORDS AND A FILE'
          WRITE(LU,*) '**************************************'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
! READ THE NUMBER OF POINTS
      READ(FID,*) NP,NE
! ALLOCATE THE MEMORY FOR THE POINTS
      IF(ALLOCATED(IP))DEALLOCATE(IP)
      ALLOCATE(IP(NP))
      IF(ALLOCATED(XP))DEALLOCATE(XP)
      ALLOCATE(XP(NP))
      IF(ALLOCATED(YP))DEALLOCATE(YP)
      ALLOCATE(YP(NP))
! READ ALL THE POINTS
      DO I=1,NP
        READ(FID,*) I_GLO,XP(I),YP(I),ZP
      ENDDO
!
      RETURN
      END
