!                    ****************
                     SUBROUTINE SOR3D
!                    ****************
!
     &(F,NPLAN,NF,NPOIN2,AT,U,V,UV,VV,DEPTH,VENT,
     & COURAN,MAREE,TITRE,NR3D,BINR3D,TRA01,MESH3D)
!
!***********************************************************************
! TOMAWAC   V6P3                                   28/06/2011
!***********************************************************************
!
!brief    WRITES DATA NECESSARY TO RESUME COMPUTATION
!+                AT A LATER DATE.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        26/11/2012
!+        V6P3
!+   Correction of bugs and double precision.
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINR3D         |-->| GLOBAL RESULT FILE BINARY
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| MAREE          |-->| LOGICAL INDICATING CONSIDERATION OF TIDE
!| MESH3D         |-->| MESH STRUCTURE IN 3D (I.E. INCLUDING DIRECTIONS)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NR3D           |-->| LOGICAL UNIT NUMBER OF GLOBAL RESULT FILE
!| TITRE          |-->| TITLE
!| TRA01          |-->| DOUBLE PRECISION WORK TABLE OF SIZE NPOIN2*NPLAN
!| U              |-->| CURRENT SPEED ALONG X
!| UV             |-->| WIND SPEED ALONG X
!| V              |-->| CURRENT SPEED ALONG Y
!| VV             |-->| WIND SPEED ALONG Y
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NR3D,NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF),AT
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2),V(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: DEPTH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2*NPLAN)
      LOGICAL, INTENT(IN)             :: COURAN,VENT,MAREE
      CHARACTER(LEN=8), INTENT(IN)    :: BINR3D
      CHARACTER(LEN=80), INTENT(IN)   :: TITRE
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISTAT,I,IIF
!
      INTEGER, PARAMETER :: NFMAX = 200
      LOGICAL SORLEO(NFMAX+2)
      CHARACTER(LEN=32) TEXTE(NFMAX+2)
!
      INTEGER :: DATE(3),TIME(3)
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA DATE/0,0,0/
!      DATA TIME/0,0,0/
      PARAMETER ( DATE = (/ 0,0,0 /) )
      PARAMETER ( TIME = (/ 0,0,0 /) )
!##< SEB @ HRW
!
!***********************************************************************
!
      DO I=1,NF
        SORLEO(I)=.TRUE.
        TEXTE(I)='FREQUENCY 000                   '
        IF(I.LT.10) THEN
          WRITE(TEXTE(I)(13:13),'(I1)') I
        ELSEIF(I.LT.100) THEN
          WRITE(TEXTE(I)(12:13),'(I2)') I
        ELSEIF(I.LT.NFMAX+1) THEN
          WRITE(TEXTE(I)(11:13),'(I3)') I
        ELSE
          WRITE(LU,*) 'SOR3D: PARAMETER NFMAX MUST BE'
          WRITE(LU,*) '       INCREASED TO AT LEAST ',NF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
      SORLEO(NF+1)=.TRUE.
      TEXTE(NF+1)='DEPTH                           '
      TEXTE(NF+2)='CURRENT-WIND                    '
      IF(COURAN.OR.VENT) THEN
        SORLEO(NF+2)=.TRUE.
      ELSE
        SORLEO(NF+2)=.FALSE.
      ENDIF
!
!     CREATES THE DATA FILE USING A GIVEN FILE FORMAT
!     THE DATA ARE CREATED IN THE FILE: NRES, AND IS
!     CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
!     CONTAINED IN THE FILE.
!
      CALL WRITE_HEADER(BINR3D, ! RESULTS FILE FORMAT
     &                  NR3D,       ! LU FOR RESULTS FILE
     &                  TITRE,      ! TITLE
     &                  NF+2,       ! MAX NUMBER OF OUTPUT VARIABLES
     &                  TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                  SORLEO)     ! PRINT TO FILE OR NOT
!
!     WRITES THE MESH IN THE OUTPUT FILE
!
      CALL WRITE_MESH(BINR3D,     ! RESULTS FILE FORMAT
     &                NR3D,       ! LU FOR RESULTS FILE
     &                MESH3D,
     &                NPLAN,      ! NUMBER OF PLANES
     &                DATE,       ! START DATE
     &                TIME)       ! START TIME
!
! WRITES TIME
!
!     WRITES DATA INFORMATION
!
! WRITES F
!
      DO IIF=1,NF
        CALL ADD_DATA(BINR3D,NR3D,TEXTE(IIF),AT,0,IIF.EQ.1,F(1,1,IIF),
     &                NPOIN2*NPLAN,ISTAT)
      ENDDO
!
!     WRITES DEPTH
!
      IF(MAREE) THEN
        DO I=1,NPOIN2
          TRA01(I)=DEPTH(I)
        ENDDO
      ENDIF
!
!     HERE TRA01 MAY BE WRITTEN FOR NOTHING (AND NOT INITIALISED)
!     THIS IS NECESSARY TO HAVE A REAL SERAFIN FORMAT
!
      CALL ADD_DATA(BINR3D,NR3D,TEXTE(NF+1),AT,0,.FALSE.,TRA01,
     &                NPOIN2*NPLAN,ISTAT)
!
!     WRITES U,V,UV,VV (IF HAS TO)
!
      IF(VENT.AND.NPLAN.LT.4) THEN
        WRITE(LU,*) 'SOR3D: NPLAN MUST BE GREATER THAN 3'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(COURAN.AND.NPLAN.LT.2) THEN
        WRITE(LU,*) 'SOR3D: NPLAN MUST BE GREATER THAN 1'
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(COURAN) THEN
        DO I=1,NPOIN2
          TRA01(I)=U(I)
          TRA01(I+NPOIN2)=V(I)
        ENDDO
      ENDIF
!
      IF(VENT) THEN
        DO I=1,NPOIN2
          TRA01(I+2*NPOIN2)=UV(I)
          TRA01(I+3*NPOIN2)=VV(I)
        ENDDO
      ENDIF
!
      IF(COURAN.OR.VENT) THEN
        CALL ADD_DATA(BINR3D,NR3D,TEXTE(NF+2),AT,0,.FALSE.,TRA01,
     &                NPOIN2*NPLAN,ISTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
