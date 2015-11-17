!                    ***************************
                     SUBROUTINE READ_PARTEL_INFO
!                    ***************************
!
     &(NAMEPAR,NPTFR,NUMLIQ,BOUNDARY_COLOUR,MESH)
!
!***********************************************************************
! BIEF   V7P1
!***********************************************************************
!
!brief    READS IFAPAR AND NACHB
!
!history  Y AUDOUIN (LNHE)
!+        25/05/2015
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NAMEPAR        |-->| NAME OF THE PARTEL INFO FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NUMLIQ         |<->| NUMBER OF LIQUID BOUNDARY
!| BOUNDARY_COLOUR|<->| COLOUR OF BOUNDARY POINTS
!| MESH           |<->| MESH STRUCTURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=144), INTENT(IN)    :: NAMEPAR
      INTEGER,            INTENT(IN) :: NPTFR
      INTEGER,            INTENT(INOUT) :: NUMLIQ(NPTFR)
      INTEGER,            INTENT(INOUT) :: BOUNDARY_COLOUR(NPTFR)
      TYPE(BIEF_MESH),    INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER PTIR,I,K,IF1,IF2,IF3,IF4,IF5,IF6,IF7
      INTEGER NPTFR_BND
      INTEGER NPAR
      LOGICAL IS_USED
!
!-----------------------------------------------------------------------
!
!  PARALLEL MODE : READS NPTIR AND NACHB
!
!-----------------------------------------------------------------------
!
      ! Look for an available unit
      NPAR = 1000
      DO
        INQUIRE(UNIT=NPAR,OPENED=IS_USED)
        IF(.NOT.IS_USED) EXIT
        NPAR = NPAR + 1
      ENDDO
      OPEN(NPAR,FILE=NAMEPAR,STATUS='OLD',FORM='FORMATTED')
      REWIND(NPAR)
!
      READ(NPAR,*) NPTFR_BND
!
      WRITE(LU,*) NPTFR_BND,NPTFR
      IF(NPTFR.NE.NPTFR_BND) THEN
        IF(LNG.EQ.1) WRITE(LU,23) NPTFR_BND,NPTFR
        IF(LNG.EQ.2) WRITE(LU,24) NPTFR_BND,NPTFR
23      FORMAT(1X,'READ_PARTEL_INFO : ERREUR DANS LE FICHIER PARALLEL',
     &       /,1X,'         ',1I5,' LIGNES',
     &       /,1X,'         AU LIEU DE ',I5)
24      FORMAT(1X,'READ_PARTEL_INFO: ERROR IN THE PARALLEL FILE,',
     &       /,9X,1I5,' LINES INSTEAD OF ',I5,' REQUESTED')
        CALL PLANTE(1)
        STOP
      ENDIF
      DO K=1,NPTFR_BND
        READ(NPAR,*) BOUNDARY_COLOUR(K),MESH%NBOR%I(K),MESH%ISEG%I(K),
     &               MESH%XSEG%R(K),MESH%YSEG%R(K),NUMLIQ(K)
      ENDDO
!
      READ(NPAR,*) PTIR
      IF(NPTIR.NE.PTIR) THEN
        IF(LNG.EQ.1) WRITE(LU,151) NPTIR,PTIR
        IF(LNG.EQ.2) WRITE(LU,152) NPTIR,PTIR
151     FORMAT(1X,'READ_PARTEL_INFO : INCOHERENCE ENTRE GEOMETRIE ',/,
     &         1X,'         ET CONDITIONS AUX LIMITES'   ,/,1X,I6,
     &  ' POINTS INTERFACE DANS LA GEOMETRIE',/,1X,I6,
     &  ' POINTS INTERFACE DANS LE FICHIER CONPAR')
152     FORMAT(1X,'READ_PARTEL_INFO : DIFFERENCE BETWEEN GEOMETRY ',/,
     &         1X,'         AND BOUNDARY CONDITIONS'   ,/,1X,I6,
     &  ' INTERFACE POINTS IN GEOMETRY',/,1X,I6,
     &  ' INTERFACE POINTS IN CONPAR FILE')
      ENDIF
!     NACHB(NBMAXNSHARE,NPTIR), HERE NACHB(I,K)
!     HERE NACHB IS IN LOCAL NUMBERING
      IF(NPTIR.GT.0) THEN
        DO K=1,NPTIR
          READ(NPAR,*) (MESH%NACHB%I((K-1)*NBMAXNSHARE+I),
     &                          I=1,NBMAXNSHARE)
        ENDDO
      ENDIF
!
!     JAJ //// READS THE NEIGHBOURHOODS FOR HALO CELLS ALONG THE INTERFACES
!     FILLING PATTERN: IFAPAR(1:7,K), K=1:NHALO
!                      -> NHALO: NUMBER OF HALO CELLS IN THIS PARTITION
!
!     IFAPAR(1,K)   : HALO ELEMENT -LOCAL- NUMBER IN THIS PARTITION
!     IFAPAR(2:4,K) : PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
!                     NUMBER FROM 0 TO NCSIZE-1
!     IFAPAR(5:7,K) : -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
!                     IN THE NUMBERING OF PARTITIONS THEY BELONG TO
!     ACTUALLY, NOT ALL OF THAT IS REQUIRED AND CAN BE OPTIMISED
!     AFTER THE DEVELOPMENT STAGE IS OVER
!
!     IN TELEMAC, IFAPAR IS REORGANISED IN IFAPAR(6,NELEM2)
!                 AND INITIALISED TO 0 IN ALMESH
!
      READ(NPAR,*) NHALO
      IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIB, SUBROUTINE ALMESH
        WRITE(LU,*) ' => NHALO>2*NPTIR DETECTED IN BC FILE'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(NHALO.GT.0) THEN
        DO K=1,NHALO
          READ(NPAR,*) IF1,IF2,IF3,IF4,IF5,IF6,IF7
!      
!         CORRECTS A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
!                         AND LIQUID BOUNDARY BUT
!                         IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
!                         IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
!                         HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE SET AT -1
!      
          IF(IF5.EQ.0) IF2=-1
          IF(IF6.EQ.0) IF3=-1
          IF(IF7.EQ.0) IF4=-1
!      
          MESH%IFAPAR%I(6*(IF1-1)+1)=IF2
          MESH%IFAPAR%I(6*(IF1-1)+2)=IF3
          MESH%IFAPAR%I(6*(IF1-1)+3)=IF4
          MESH%IFAPAR%I(6*(IF1-1)+4)=IF5
          MESH%IFAPAR%I(6*(IF1-1)+5)=IF6
          MESH%IFAPAR%I(6*(IF1-1)+6)=IF7
        ENDDO
      ENDIF
!
      CLOSE(NPAR)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
