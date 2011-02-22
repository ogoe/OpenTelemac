C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       LOOKS FOR 'BOTTOM' IN THE GEOMETRY FILE.
!><br>            LOOKS FOR 'BOTTOM FRICTION' (COEFFICIENTS).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE NAMES OF THE VARIABLES HAVE BEEN DIRECTLY
!>         WRITTEN OUT AND ARE NOT READ FROM 'TEXTE'.
!>         THIS MAKES IT POSSIBLE TO HAVE A GEOMETRY FILE
!>         COMPILED IN ANOTHER LANGUAGE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHESTR, FFON, H, LISTIN, MESH, NFON, NGEO, NOMFON, Z, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, CALFON, CALFRO, ERR, LUH, LUZ, LUZF, OK, W
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FONSTR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FIND_IN_SEL(), FOND(), OS(), STRCHE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), SISYPHE(), TELEMAC2D(), TELEMAC3D(), WAC()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.6                                       </center>
!> </td><td> 17/08/94
!> </td><td> J-M HERVOUET (LNH) 30 71 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHESTR
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT.
!>    </td></tr>
!>          <tr><td>FFON
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td><--</td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NFON
!></td><td>--></td><td>NUMERO DU CANAL DU FICHIER DES FONDS
!>    </td></tr>
!>          <tr><td>NGEO
!></td><td>--></td><td>NUMERO DU CANAL DU FICHIER DE GEOMETRIE
!>    </td></tr>
!>          <tr><td>NOMFON
!></td><td>--></td><td>NOM DU FICHIER DES FONDS
!>    </td></tr>
!>          <tr><td>Z
!></td><td><--</td><td>COTE DE LA SURFACE LIBRE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><--</td><td>FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FONSTR
     &(H,ZF,Z,CHESTR,NGEO,NFON,NOMFON,MESH,FFON,LISTIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |<--| COEFFICIENT DE FROTTEMENT.
C| FFON           |-->| 
C| H             |<--| HAUTEUR D'EAU
C| LISTIN         |-->| 
C| MESH           |-->| 
C| NFON           |-->| NUMERO DU CANAL DU FICHIER DES FONDS
C| NGEO           |-->| NUMERO DU CANAL DU FICHIER DE GEOMETRIE
C| NOMFON         |-->| NOM DU FICHIER DES FONDS
C| Z             |<--| COTE DE LA SURFACE LIBRE
C| ZF             |<--| FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FONSTR => FONSTR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: H,ZF,Z,CHESTR
      CHARACTER(LEN=72), INTENT(IN) :: NOMFON
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
      DOUBLE PRECISION, INTENT(IN)  :: FFON
      LOGICAL, INTENT(IN)           :: LISTIN
      INTEGER, INTENT(IN)           :: NGEO,NFON
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ERR
C
      DOUBLE PRECISION BID
      REAL, ALLOCATABLE :: W(:)
C
      LOGICAL CALFON,CALFRO,OK,LUZF,LUH,LUZ
C
C-----------------------------------------------------------------------
C
      ALLOCATE(W(MESH%NPOIN),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
        IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C    ASSUMES THAT THE FILE HEADER LINES HAVE ALREADY BEEN READ
C    WILL START READING THE RESULT RECORDS
C
C-----------------------------------------------------------------------
C
C    INITIALISES
C
      LUH  =  .FALSE.
      LUZ  =  .FALSE.
      LUZF =  .FALSE.
      CALFRO = .TRUE.
C
C-----------------------------------------------------------------------
C
C     LOOKS FOR THE FRICTION COEFFICIENT IN THE FILE
C
      IF(LNG.EQ.1) CALL FIND_IN_SEL(CHESTR,'FROTTEMENT      ',NGEO,W,OK,
     &                              TIME=BID)
      IF(LNG.EQ.2) CALL FIND_IN_SEL(CHESTR,'BOTTOM FRICTION ',NGEO,W,OK,
     &                              TIME=BID)
C     CASE OF A GEOMETRY FILE IN ANOTHER LANGUAGE
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
C
C     LOOKS FOR THE BOTTOM ELEVATION IN THE FILE
C
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
C     MESHES FROM BALMAT ?
      IF(.NOT.OK) CALL FIND_IN_SEL(ZF,'ALTIMETRIE      ',NGEO,W,OK,
     &                             TIME=BID)
C     TOMAWAC IN FRENCH ?
      IF(.NOT.OK) CALL FIND_IN_SEL(ZF,'COTE_DU_FOND    ',NGEO,W,OK,
     &                             TIME=BID)
C     TOMAWAC IN ENGLISH ?
      IF(.NOT.OK) CALL FIND_IN_SEL(ZF,'BOTTOM_LEVEL    ',NGEO,W,OK,
     &                             TIME=BID)
      LUZF = OK
C
      IF(.NOT.LUZF) THEN
C       LOOKS FOR WATER DEPTH AND FREE SURFACE ELEVATION
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
C
C     INITIALISES THE BOTTOM ELEVATION
C
      IF(LUZF) THEN
C
         CALFON = .FALSE.
C
      ELSE
C
         IF (LUZ.AND.LUH) THEN
C
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
C
         ELSE
C
            CALFON = .TRUE.
C
         ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C BUILDS THE BOTTOM IF IT WAS NOT IN THE GEOMETRY FILE
C
      IF(NOMFON(1:1).NE.' ') THEN
C       A BOTTOM FILE WAS GIVEN, (RE)COMPUTES THE BOTTOM ELEVATION
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
C
        CALL FOND(ZF%R,MESH%X%R,MESH%Y%R,MESH%NPOIN,NFON,
     &            MESH%NBOR%I,MESH%KP1BOR%I,MESH%NPTFR)
C
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
C
C-----------------------------------------------------------------------
C
C COMPUTES THE BOTTOM FRICTION COEFFICIENT
C
      IF(CALFRO) THEN
        CALL OS( 'X=C     ' , CHESTR , CHESTR , CHESTR , FFON )
      ENDIF
      CALL STRCHE
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(W)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C