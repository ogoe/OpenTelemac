!                    ******************************
                     SUBROUTINE T3D_READ_FIC_CURVES
!                    ******************************
!
     &(NFIC,NFRLIQ,STA_DIS_CURVES,PTS_CURVES)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS STAGE-DISCHARGE CURVES IN THEIR FILE.
!
!note     THIS IS A MERE COPY OF READ_FIC_CURVES IN TELEMAC2D LIBRARY
!+         ONLY THE USE DECLARATIONS IS CHANGED FROM 2D TO 3D, TO REACH
!+         ALLOCATABLE ARRAY QZ.
!
!history  J-M HERVOUET (LNHE)
!+        09/04/08
!+        V5P9
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NFIC           |-->| NUMBER OF CHANNEL OF STAGE-DISCHARGE CURVES FILE
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| PTS_CURVES     |<->| NUMBER OF POINTS GIVEN
!|                |---| FOR EACH DISCHARGE-ELEVATIONS CURVES
!| STA_DIS_CURVES |-->| STAGE-DISCHARGE CURVES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC3D, ONLY : QZ
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NFIC,NFRLIQ
      INTEGER, INTENT(IN)    :: STA_DIS_CURVES(NFRLIQ)
      INTEGER, INTENT(INOUT) :: PTS_CURVES(NFRLIQ)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NMAXPTS,IDEB,IFIN,ICURVE,PASS,I,OK
!
      CHARACTER(LEN=144) :: LIGNE
      CHARACTER(LEN=1)   :: WHAT
!
      INTRINSIC CHAR
!
!-----------------------------------------------------------------------
!
      NMAXPTS=0
!     FILE WILL BE READ TWICE, THE FIRST TIME (PASS=0) TO COUNT DATA
!                              THE SECOND TIME (PASS=1) TO READ THEM
      PASS=0
!
10    CONTINUE
      REWIND(NFIC)
!     SKIPS COMMENTS
1     READ(NFIC,FMT='(A)',END=1000,ERR=999) LIGNE
      IF(LIGNE(1:1).EQ.'#') GO TO 1
!
!     NOW A LINE ANNOUNCING Q(??) OR Z(??)
!
!     IDENTIFIES FIRST CHARACTER OF NAME
2     CONTINUE
      IDEB=1
!     SKIPPING SPACES OR TABS
      IF(LIGNE(IDEB:IDEB).EQ.' '.OR.LIGNE(IDEB:IDEB).EQ.CHAR(9)) THEN
        IDEB=IDEB+1
        IF(IDEB.EQ.145) THEN
          READ(NFIC,FMT='(A)') LIGNE
          IDEB=1
        ENDIF
        GO TO 2
      ENDIF
      IF(LIGNE(IDEB:IDEB+1).EQ.'Q('.OR.LIGNE(IDEB:IDEB+1).EQ.'Z(') THEN
        WHAT=LIGNE(IDEB:IDEB)
!       WHICH BOUNDARY NUMBER ?
        IDEB=IDEB+2
        IFIN=IDEB+1
3       IF(LIGNE(IFIN:IFIN).NE.')') THEN
          IFIN=IFIN+1
          IF(IFIN.GT.144) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
              WRITE(LU,*) 'MANQUE PARENTHESE DANS LA LIGNE :',LIGNE
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
              WRITE(LU,*) 'MISSING PARENTHESIS IN LINE:',LIGNE
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          GO TO 3
        ENDIF
        READ(LIGNE(IDEB:IFIN-1),*) ICURVE
!       SKIPS UNITS (UNITS NOT CHECKED)
        READ(NFIC,FMT='(A)',END=1000,ERR=999) LIGNE
        PTS_CURVES(ICURVE)=0
4       READ(NFIC,FMT='(A)',END=1001,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#') THEN
          PTS_CURVES(ICURVE)=PTS_CURVES(ICURVE)+1
          IF(PASS.EQ.1) THEN
!           READS AND STORES
            IF(WHAT.EQ.'Q') THEN
            READ(LIGNE,*,ERR=999) QZ(1,ICURVE,PTS_CURVES(ICURVE)),
     &                            QZ(2,ICURVE,PTS_CURVES(ICURVE))
            ENDIF
            IF(WHAT.EQ.'Z') THEN
            READ(LIGNE,*,ERR=999) QZ(2,ICURVE,PTS_CURVES(ICURVE)),
     &                            QZ(1,ICURVE,PTS_CURVES(ICURVE))
            ENDIF
          ENDIF
          GO TO 4
        ENDIF
!       END OF BLOCK FOR CURVE ICURVE
1001    NMAXPTS=MAX(NMAXPTS,PTS_CURVES(ICURVE))
!       TREATS THE NEXT CURVE
        GO TO 1
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
          WRITE(LU,*) 'LA PREMIERE LIGNE APRES COMMENTAIRES :',LIGNE
          WRITE(LU,*) 'DOIT ANNONCER Q(..) ET Z(..)'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
          WRITE(LU,*) 'THE FIRST LINE AFTER COMMENTS:',LIGNE
          WRITE(LU,*) 'MUST ANNOUNCE Q(..) AND Z(..)'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
999   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
      ENDIF
      CALL PLANTE(1)
      STOP
1000  CONTINUE
!
!     CHECKING
!
      DO ICURVE=1,NFRLIQ
        IF(STA_DIS_CURVES(ICURVE).GT.0.AND.PTS_CURVES(ICURVE).EQ.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ERREUR DANS LE FICHIER DES COURBES DE TARAGE'
            WRITE(LU,*) 'COURBE :',ICURVE,' MANQUANTE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'ERROR IN THE STAGE-DISCHARGE CURVES FILE'
            WRITE(LU,*) 'CURVE:',ICURVE,' MISSING'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     DYNAMICALLY ALLOCATES QZ
!
      IF(PASS.EQ.0) THEN
        ALLOCATE(QZ(2,NFRLIQ,NMAXPTS),STAT=OK)
        IF(OK.NE.0) THEN
          WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR QZ'
          CALL PLANTE(1)
          STOP
        ENDIF
        PASS=1
!       SHOOT AGAIN
        GO TO 10
      ENDIF
!
!     REPORTS IN LISTING
!
      DO ICURVE=1,NFRLIQ
        IF(PTS_CURVES(ICURVE).GT.0) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) WRITE(LU,*) 'COURBE DE TARAGE :',ICURVE
          IF(LNG.EQ.2) WRITE(LU,*) 'STAGE-DISCHARGE CURVE:',ICURVE
          WRITE(LU,*) ' '
          DO I=1,PTS_CURVES(ICURVE)
            WRITE(LU,*) 'Q=',QZ(1,ICURVE,I),' Z=',QZ(2,ICURVE,I)
          ENDDO
        ENDIF
        WRITE(LU,*) ' '
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
