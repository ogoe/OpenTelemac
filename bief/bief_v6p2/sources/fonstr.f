!                    *****************
                     SUBROUTINE FONSTR
!                    *****************
!
     &(H,ZF,Z,CHESTR,NGEO,NFON,NOMFON,MESH,FFON,LISTIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR 'BOTTOM' IN THE GEOMETRY FILE.
!+
!+            LOOKS FOR 'BOTTOM FRICTION' (COEFFICIENTS).
!
!note     THE NAMES OF THE VARIABLES HAVE BEEN DIRECTLY
!+         WRITTEN OUT AND ARE NOT READ FROM 'TEXTE'.
!+         THIS MAKES IT POSSIBLE TO HAVE A GEOMETRY FILE
!+         COMPILED IN ANOTHER LANGUAGE.
!
!history  J-M HERVOUET (LNH)
!+        17/08/94
!+        V5P6
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
!| CHESTR         |<--| FRICTION COEFFICIENT (DEPENDING ON FRICTION LAW)
!| FFON           |-->| FRICTION COEFFICIENT IF CONSTANT
!| H              |<--| WATER DEPTH
!| LISTIN         |-->| IF YES, WILL GIVE A REPORT
!| MESH           |-->| MESH STRUCTURE
!| NFON           |-->| LOGICAL UNIT OF BOTTOM FILE
!| NGEO           |-->| LOGICAL UNIT OF GEOMETRY FILE
!| NOMFON         |-->| NAME OF BOTTOM FILE
!| Z              |<--| FREE SURFACE ELEVATION
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FONSTR => FONSTR
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: H,ZF,Z,CHESTR
      CHARACTER(LEN=72), INTENT(IN) :: NOMFON
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      DOUBLE PRECISION, INTENT(IN)  :: FFON
      LOGICAL, INTENT(IN)           :: LISTIN
      INTEGER, INTENT(IN)           :: NGEO,NFON
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ERR
!
      DOUBLE PRECISION BID
      REAL, ALLOCATABLE :: W(:)
!
      LOGICAL CALFON,CALFRO,OK,LUZF,LUH,LUZ
!
!-----------------------------------------------------------------------
!
      ALLOCATE(W(MESH%NPOIN),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
        IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!    ASSUMES THAT THE FILE HEADER LINES HAVE ALREADY BEEN READ
!    WILL START READING THE RESULT RECORDS
!
!-----------------------------------------------------------------------
!
!    INITIALISES
!
      LUH  =  .FALSE.
      LUZ  =  .FALSE.
      LUZF =  .FALSE.
      CALFRO = .TRUE.
!
!-----------------------------------------------------------------------
!
!     LOOKS FOR THE FRICTION COEFFICIENT IN THE FILE
!
      IF(LNG.EQ.1) CALL FIND_IN_SEL(CHESTR,'FROTTEMENT      ',NGEO,W,OK,
     &                              TIME=BID)
      IF(LNG.EQ.2) CALL FIND_IN_SEL(CHESTR,'BOTTOM FRICTION ',NGEO,W,OK,
     &                              TIME=BID)
!     CASE OF A GEOMETRY FILE IN ANOTHER LANGUAGE
      IF(.NOT.OK.AND.LNG.EQ.1) THEN
        CALL FIND_IN_SEL(CHESTR,'BOTTOM FRICTION ',NGEO,W,OK,TIME=BID)
      ENDIF
      IF(.NOT.OK.AND.LNG.EQ.2) THEN
        CALL FIND_IN_SEL(CHESTR,'FROTTEMENT      ',NGEO,W,OK,TIME=BID)
      ENDIF
      IF(OK) THEN
        CALFRO = .FALSE.
        IF(LNG.EQ.1) WRITE(LU,5)
        IF(LNG.EQ.2) WRITE(LU,6)
5       FORMAT(1X,'FONSTR : COEFFICIENTS DE FROTTEMENT LUS DANS',/,
     &         1X,'         LE FICHIER DE GEOMETRIE')
6       FORMAT(1X,'FONSTR : FRICTION COEFFICIENTS READ IN THE',/,
     &         1X,'         GEOMETRY FILE')
      ENDIF
!
!     LOOKS FOR THE BOTTOM ELEVATION IN THE FILE
!
      IF(LNG.EQ.1) CALL FIND_IN_SEL(ZF,'FOND            ',NGEO,W,OK,
     &                              TIME=BID)
      IF(LNG.EQ.2) CALL FIND_IN_SEL(ZF,'BOTTOM          ',NGEO,W,OK,
     &                              TIME=BID)
      IF(.NOT.OK.AND.LNG.EQ.1) THEN
        CALL FIND_IN_SEL(ZF,'BOTTOM          ',NGEO,W,OK,TIME=BID)
      ENDIF
      IF(.NOT.OK.AND.LNG.EQ.2) THEN
        CALL FIND_IN_SEL(ZF,'FOND            ',NGEO,W,OK,TIME=BID)
      ENDIF
!     MESHES FROM BALMAT ?
      IF(.NOT.OK) CALL FIND_IN_SEL(ZF,'ALTIMETRIE      ',NGEO,W,OK,
     &                             TIME=BID)
!     TOMAWAC IN FRENCH ?
      IF(.NOT.OK) CALL FIND_IN_SEL(ZF,'COTE_DU_FOND    ',NGEO,W,OK,
     &                             TIME=BID)
!     TOMAWAC IN ENGLISH ?
      IF(.NOT.OK) CALL FIND_IN_SEL(ZF,'BOTTOM_LEVEL    ',NGEO,W,OK,
     &                             TIME=BID)
      LUZF = OK
!
      IF(.NOT.LUZF) THEN
!       LOOKS FOR WATER DEPTH AND FREE SURFACE ELEVATION
        IF(LNG.EQ.1) CALL FIND_IN_SEL(H,'HAUTEUR D''EAU   ',NGEO,W,OK,
     &                                TIME=BID)
        IF(LNG.EQ.2) CALL FIND_IN_SEL(H,'WATER DEPTH     ',NGEO,W,OK,
     &                                TIME=BID)
        IF(.NOT.OK.AND.LNG.EQ.1) THEN
          CALL FIND_IN_SEL(H,'WATER DEPTH     ',NGEO,W,OK,TIME=BID)
        ENDIF
        IF(.NOT.OK.AND.LNG.EQ.2) THEN
          CALL FIND_IN_SEL(H,'HAUTEUR D''EAU   ',NGEO,W,OK,TIME=BID)
        ENDIF
        LUH = OK
        IF(LNG.EQ.1) CALL FIND_IN_SEL(Z,'SURFACE LIBRE   ',NGEO,W,OK,
     &                                TIME=BID)
        IF(LNG.EQ.2) CALL FIND_IN_SEL(Z,'FREE SURFACE    ',NGEO,W,OK,
     &                                TIME=BID)
        IF(.NOT.OK.AND.LNG.EQ.1) THEN
          CALL FIND_IN_SEL(Z,'FREE SURFACE    ',NGEO,W,OK,TIME=BID)
        ENDIF
        IF(.NOT.OK.AND.LNG.EQ.2) THEN
          CALL FIND_IN_SEL(Z,'SURFACE LIBRE   ',NGEO,W,OK,TIME=BID)
        ENDIF
        LUZ = OK
      ENDIF
!
!     INITIALISES THE BOTTOM ELEVATION
!
      IF(LUZF) THEN
!
         CALFON = .FALSE.
!
      ELSE
!
         IF (LUZ.AND.LUH) THEN
!
            CALL OS( 'X=Y-Z   ' , ZF , Z , H , BID )
            IF(LNG.EQ.1) WRITE(LU,24)
            IF(LNG.EQ.2) WRITE(LU,25)
24          FORMAT(1X,'FONSTR (BIEF) : ATTENTION, FOND CALCULE AVEC',/,
     &                '                PROFONDEUR ET SURFACE LIBRE',/,
     &                '                DU FICHIER DE GEOMETRIE')
25          FORMAT(1X,'FONSTR (BIEF): ATTENTION, THE BOTTOM RESULTS',/,
     &                '               FROM DEPTH AND SURFACE ELEVATION',
     &              /,'               FOUND IN THE GEOMETRY FILE')
            CALFON = .FALSE.
!
         ELSE
!
            CALFON = .TRUE.
!
         ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! BUILDS THE BOTTOM IF IT WAS NOT IN THE GEOMETRY FILE
!
      IF(NOMFON(1:1).NE.' ') THEN
!       A BOTTOM FILE WAS GIVEN, (RE)COMPUTES THE BOTTOM ELEVATION
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,2223) NOMFON
          IF(LNG.EQ.2) WRITE(LU,2224) NOMFON
          IF(.NOT.CALFON) THEN
            IF(LNG.EQ.1) WRITE(LU,2225)
            IF(LNG.EQ.2) WRITE(LU,2226)
          ENDIF
        ENDIF
2223    FORMAT(/,1X,'FONSTR (BIEF) : FOND DANS LE FICHIER : ',A72)
2224    FORMAT(/,1X,'FONSTR (BIEF): BATHYMETRY GIVEN IN FILE : ',A72)
2225    FORMAT(  1X,'                LE FOND TROUVE DANS LE FICHIER',/,
     &           1X,'                DE GEOMETRIE EST IGNORE',/)
2226    FORMAT(  1X,'               BATHYMETRY FOUND IN THE',/,
     &           1X,'               GEOMETRY FILE IS IGNORED',/)
!
        CALL FOND(ZF%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,NFON,
     &            MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
!
      ELSEIF(CALFON) THEN
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,2227)
          IF(LNG.EQ.2) WRITE(LU,2228)
        ENDIF
2227    FORMAT(/,1X,'FONSTR (BIEF) : PAS DE FOND DANS LE FICHIER DE',
     &         /,1X,'                GEOMETRIE ET PAS DE FICHIER DES',
     &         /,1X,'                FONDS. LE FOND EST INITIALISE A'
     &         /,1X,'                ZERO MAIS PEUT ENCORE ETRE MODIFIE'
     &         /,1X,'                DANS CORFON.',
     &         /,1X)
2228    FORMAT(/,1X,'FONSTR (BIEF): NO BATHYMETRY IN THE GEOMETRY FILE',
     &         /,1X,'               AND NO BATHYMETRY FILE. THE BOTTOM',
     &         /,1X,'               LEVEL IS FIXED TO ZERO BUT STILL',
     &         /,1X,'               CAN BE MODIFIED IN CORFON.',
     &         /,1X)
        CALL OS( 'X=C     ' , ZF , ZF , ZF , 0.D0 )
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE BOTTOM FRICTION COEFFICIENT
!
      IF(CALFRO) THEN
        CALL OS( 'X=C     ' , CHESTR , CHESTR , CHESTR , FFON )
      ENDIF
      CALL STRCHE
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(W)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
