C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE BOUNDARY CONDITIONS FILE AND
!>                STORES IN ARRAYS THE DATA READ.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ATBOR, CFBOR, BOUNDARY_COLOUR, BTBOR, HBOR, KADH, KENT, KENTU, KINC, KLOG, KSORT, LIHBOR, LITBOR, LIUBOR, LIVBOR, MESH, NLIM, NPTFR, NPTFR2, NUMLIQ, STDGEO, TBOR, TRAC, UBOR, VBOR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NBMAXNSHARE NBMAXNSHARE@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NHALO NHALO@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, DIMUBOR, I, IBID, IF1, IF2, IF3, IF4, IF5, IF6, IF7, K, KFICH, NLIG, PTIR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_LECLIM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 09/07/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ATBOR,BTBOR
!></td><td><--</td><td>COEFFICIENTS D'ECHANGE THERMIQUE.
!>    </td></tr>
!>          <tr><td>AUBOR
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT AU BORD
!>    </td></tr>
!>          <tr><td>BOUNDARY_COLOUR
!></td><td><--</td><td>COULEUR DU POINT DE BORD
!>                  (PAR DEFAUT SON NUMERO)
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td><--</td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE PAROI (ADHERENCE)
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>TYPE DE CONDITION LIMITE D'ENTREE.
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>--></td><td>TYPE DE CONDITION LIMITE : VITESSES IMPOSEES
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>--></td><td>TYPE DE CONDITION LIMITE D'ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE PAROI (PAROI)
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE SORTIE LIBRE
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES EN HAUTEUR
!>                  POUR LES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES EN TEMPERA-
!>                  TURE POUR LES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>LIUBOR,LIVBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES POUR LES
!>                  POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NLIM
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES CONDITIONS LIM.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NPTFR2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STDGEO
!></td><td>--></td><td>STANDARD DU FICHIER DE GEOMETRIE.
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td><--</td><td>TRACEUR AUX BORDS
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>INDICATEUR DE TRACEUR .
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td><--</td><td>CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td><--</td><td>CONDITIONS AUX LIMITES SUR V
!>                  (COEFFICIENTS DE LA LOI LOG)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECLIM
     &(LIHBOR,LIUBOR,LIVBOR,LITBOR,HBOR,UBOR,VBOR,TBOR,
     & CHBORD,ATBOR,BTBOR,NPTFR,STDGEO,TRAC,NLIM,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,NUMLIQ,MESH,BOUNDARY_COLOUR,
     & NPTFR2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ATBOR,BTBOR    |<--| COEFFICIENTS D'ECHANGE THERMIQUE.
C| BOUNDARY_COLOUR|<--| COULEUR DU POINT DE BORD
C|                |   | (PAR DEFAUT SON NUMERO)
C| CHBORD         |<--| COEFFICIENT DE FROTTEMENT AU BORD
C| HBOR           |<--| CONDITIONS AUX LIMITES SUR H
C| KADH           |-->| TYPE DE CONDITION LIMITE DE PAROI (ADHERENCE)
C| KENT           |-->| TYPE DE CONDITION LIMITE D'ENTREE.
C| KENTU          |-->| TYPE DE CONDITION LIMITE : VITESSES IMPOSEES
C| KINC           |-->| TYPE DE CONDITION LIMITE D'ONDE INCIDENTE
C| KLOG           |-->| TYPE DE CONDITION LIMITE DE PAROI (PAROI)
C| KSORT          |-->| TYPE DE CONDITION LIMITE DE SORTIE LIBRE
C| LIHBOR         |<--| TYPES DE CONDITIONS AUX LIMITES EN HAUTEUR
C|                |   | POUR LES POINTS DE BORD.
C| LITBOR         |<--| TYPES DE CONDITIONS AUX LIMITES EN TEMPERA-
C|                |   | TURE POUR LES POINTS DE BORD.
C| LIUBOR,LIVBOR  |<--| TYPES DE CONDITIONS AUX LIMITES POUR LES
C|                |   | POINTS DE BORD.
C| MESH           |---| 
C| NLIM           |-->| NUMERO DE CANAL DU FICHIER DES CONDITIONS LIM.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NPTFR2         |---| 
C| NUMLIQ         |---| 
C| STDGEO         |-->| STANDARD DU FICHIER DE GEOMETRIE.
C| TBOR           |<--| TRACEUR AUX BORDS
C| TRAC           |-->| INDICATEUR DE TRACEUR .
C| UBOR           |<--| CONDITIONS AUX LIMITES SUR U
C| VBOR           |<--| CONDITIONS AUX LIMITES SUR V
C|                |   | (COEFFICIENTS DE LA LOI LOG)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_LECLIM => LECLIM
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NLIM,KENT,KSORT,KADH,KLOG,KINC,KENTU
      INTEGER, INTENT(IN)    :: STDGEO,NPTFR
      LOGICAL, INTENT(IN)    :: TRAC
      INTEGER, INTENT(INOUT) :: NUMLIQ(*)
      INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(NPTFR)
      DOUBLE PRECISION,  INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
      DOUBLE PRECISION,  INTENT(INOUT) :: TBOR(NPTFR),ATBOR(NPTFR)
      DOUBLE PRECISION,  INTENT(INOUT) :: BTBOR(NPTFR)
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
      INTEGER, OPTIONAL, INTENT(INOUT) :: BOUNDARY_COLOUR(NPTFR)
      INTEGER, OPTIONAL, INTENT(IN)    :: NPTFR2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER PTIR,I,IBID,KFICH,K,NLIG,IF1,IF2,IF3,IF4,IF5,IF6,IF7
      INTEGER DIMUBOR
C
      DOUBLE PRECISION BID
C
C-----------------------------------------------------------------------
C
      DIMUBOR=NPTFR
      IF(PRESENT(NPTFR2)) DIMUBOR=NPTFR2
C
C-----------------------------------------------------------------------
C
      REWIND NLIM
C
C-----------------------------------------------------------------------
C
C READS EACH LINE OF FILE DYNAM
C
C
C 1) WITHOUT TRACER
C
      NLIG=1
      IF(.NOT.TRAC) THEN
C
        DO K=1,NPTFR
C
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
C
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K)  ,
     &                       CHBORD(K) ,IBID,BID,BID,BID,
     &                       MESH%NBOR%I(K)  ,KFICH
        NLIG=NLIG+1
C
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
C
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K)  ,
     &                       CHBORD(K),IBID,BID,BID,BID,
     &                       MESH%NBOR%I(K),KFICH,
     &                       MESH%ISEG%I(K),
     &                       MESH%XSEG%R(K),MESH%YSEG%R(K),
     &                       NUMLIQ(K)
        NLIG=NLIG+1
C
        ELSE
          IF(LNG.EQ.1) WRITE(LU,21) STDGEO
          IF(LNG.EQ.2) WRITE(LU,22) STDGEO
21        FORMAT(1X,'LECLIM : STANDARD DU FICHIER DE GEOMETRIE : ',1I6,/
     &           1X,'         CETTE VALEUR EST INCONNUE ET       ',    /
     &           1X,'         LE FICHIER DES CONDITIONS LIMITES  ',    /
     &           1X,'         EN DEPEND |')
22        FORMAT(1X,'LECLIM : GEOMETRY FILE STANDARD : ',I6   ,/
     &           1X,'         UNKNOWN PARAMETER AND BOUNDARY ',/
     &           1X,'         CONDITIONS FILE DEPENDS ON IT !')
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          BOUNDARY_COLOUR(K)=KFICH
        ENDIF
C
        ENDDO
C
      ENDIF
C
C 2) WITH TRACER
C
      IF(TRAC) THEN
C
        DO K=1,NPTFR
C
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
C
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K),
     &                       CHBORD(K) ,LITBOR(K),TBOR(K),
     &                       ATBOR(K),BTBOR(K),
     &                       MESH%NBOR%I(K),KFICH
        NLIG=NLIG+1
C
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
C
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K),
     &                       CHBORD(K) ,LITBOR(K),TBOR(K),
     &                       ATBOR(K) ,BTBOR(K) ,MESH%NBOR%I(K),
     &                       KFICH,
     &                       MESH%ISEG%I(K),
     &                       MESH%XSEG%R(K),MESH%YSEG%R(K),
     &                       NUMLIQ(K)
        NLIG=NLIG+1
C
        ELSE
          IF(LNG.EQ.1) WRITE(LU,21) STDGEO
          IF(LNG.EQ.2) WRITE(LU,22) STDGEO
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          BOUNDARY_COLOUR(K)=KFICH
        ENDIF
C
        ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  CHECKS, CORRECTS AND STORES:
C
      IF(NLIG.NE.NPTFR+1) THEN
        IF(LNG.EQ.1) WRITE(LU,23) NLIG-1,NPTFR
        IF(LNG.EQ.2) WRITE(LU,24) NLIG-1,NPTFR
23      FORMAT(1X,'LECLIM : ERREUR DANS LE FICHIER DES',
     &       /,1X,'         CONDITIONS AUX LIMITES, ',1I5,' LIGNES',
     &       /,1X,'         AU LIEU DE ',I5)
24      FORMAT(1X,'LECLIM: ERROR IN THE BOUNDARY CONDITIONS FILE,',
     &       /,9X,1I5,' LINES INSTEAD OF ',I5,' REQUESTED')
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DO 50 K=1,NPTFR
C
C     CHECKING FRICTION COEFFICIENT
C
!     IF(CHBORD(K).LT.0.D0) THEN
!        IF(LNG.EQ.1) WRITE(LU,48) K
!        IF(LNG.EQ.2) WRITE(LU,49) K
!48      FORMAT(1X,'LECLIM : CHBORD DOIT ETRE POSITIF OU NUL',/,1X,
!     &            '         IL VAUT ',F10.3,' AU POINT DE BORD ',1I6)
!49      FORMAT(1X,'LECLIM : CHBORD MUST BE POSITIVE OR ZERO',/,1X,
!     &            '         IT IS ',F10.3,' AT BOUNDARY POINT ',1I6)
!        CALL PLANTE(1)
!        STOP
!      ENDIF
C
C     ADHERENCE FOR H CHANGED AT THE WALL
C
      IF(LIHBOR(K).EQ.KADH) THEN
        LIHBOR(K)=KLOG
        IF(LNG.EQ.1) WRITE(LU,51) K
        IF(LNG.EQ.2) WRITE(LU,52) K
51      FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &            '         CHANGEE EN CONDITION DE PAROI')
52      FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &            '         CHANGEE EN CONDITION DE PAROI')
      ENDIF
C
C     INCIDENT WAVE FOR H TREATED LIKE A FREE EXIT
C
      IF(LIHBOR(K).EQ.KINC) THEN
        LIHBOR(K)=KSORT
      ENDIF
C
C     CANCELS DIRICHLET VALUES WHEN THE POINT IS NOT DIRICHLET
C     FOR POINTS WITH ADHERENCE, NEEDS UBOR OR VBOR =0
C
C     IF(LIHBOR(K).NE.KENT) HBOR(K)=0.D0
      IF(LIUBOR(K).NE.KENT.AND.LIUBOR(K).NE.KENTU) UBOR(K)=0.D0
      IF(LIVBOR(K).NE.KENT.AND.LIVBOR(K).NE.KENTU) VBOR(K)=0.D0
C
C     BACKS UP UBOR AND VBOR ON THEIR SECOND DIMENSION
C
      UBOR(K+DIMUBOR) = UBOR(K)
      VBOR(K+DIMUBOR) = VBOR(K)
C
50    CONTINUE
C
      IF(TRAC) THEN
        DO K=1,NPTFR
          IF(LITBOR(K).NE.KENT) TBOR(K)=0.D0
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C  PARALLEL MODE : READS NPTIR AND NACHB
C
C  NOTE: NACHB IS HERE IN GLOBAL NUMBERING OF POINTS, IT WILL BE REDUCED
C        TO THE LOCAL NUMBERING OF THIS SUB-DOMAIN IN SUBROUTINE PARAGL
C
      IF(NCSIZE.GT.1) THEN
        READ(NLIM,*,ERR=900) PTIR
        NLIG=NLIG+1
        IF(NPTIR.NE.PTIR) THEN
          IF(LNG.EQ.1) WRITE(LU,151) NPTIR,PTIR
          IF(LNG.EQ.2) WRITE(LU,152) NPTIR,PTIR
151       FORMAT(1X,'LECLIM : INCOHERENCE ENTRE GEOMETRIE ',/,1X,
     &              '         ET CONDITIONS AUX LIMITES'   ,/,1X,I6,
     &    ' POINTS INTERFACE DANS LA GEOMETRIE',/,1X,I6,
     &    ' POINTS INTERFACE DANS LE FICHIER CONLIM')
152       FORMAT(1X,'LECLIM : DIFFERENCE BETWEEN GEOMETRY ',/,1X,
     &              '         AND BOUNDARY CONDITIONS'   ,/,1X,I6,
     &    ' INTERFACE POINTS IN GEOMETRY',/,1X,I6,
     &    ' INTERFACE POINTS IN CONLIM FILE')
        ENDIF
C       NACHB(NBMAXNSHARE,NPTIR), HERE NACHB(I,K)
C       HERE NACHB IS IN GLOBAL NUMBERING, IT WILL BE CHANGED INTO
C       LOCAL NUMBERING IN PARAGL
        DO K=1,NPTIR
          READ(NLIM,*,ERR=900) (MESH%NACHB%I((K-1)*NBMAXNSHARE+I),
     &                          I=1,NBMAXNSHARE)
          NLIG=NLIG+1
        ENDDO
C
C      JAJ //// READS THE NEIGHBOURHOODS FOR HALO CELLS ALONG THE INTERFACES
C      FILLING PATTERN: IFAPAR(1:7,K), K=1:NHALO
C                       -> NHALO: NUMBER OF HALO CELLS IN THIS PARTITION
!
C      IFAPAR(1,K)   : HALO ELEMENT -LOCAL- NUMBER IN THIS PARTITION
C      IFAPAR(2:4,K) : PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
C                      NUMBER FROM 0 TO NCSIZE-1
C      IFAPAR(5:7,K) : -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
C                      IN THE NUMBERING OF PARTITIONS THEY BELONG TO
C      ACTUALLY, NOT ALL OF THAT IS REQUIRED AND CAN BE OPTIMISED
C      AFTER THE DEVELOPMENT STAGE IS OVER
!
C      IN TELEMAC, IFAPAR IS REORGANISED IN IFAPAR(6,NELEM2)
C                  AND INITIALISED TO 0 IN ALMESH
!
!
        READ(NLIM,*,ERR=901) NHALO
        IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIB, SUBROUTINE ALMESH
          WRITE(LU,*) ' => NHALO>2*NPTIR DETECTED IN BC FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO K=1,NHALO
C         READ(NLIM,*,ERR=901) (MESH%IFAPAR%I(7*(K-1)+I),I=1,7)
          READ(NLIM,*,ERR=901) IF1,IF2,IF3,IF4,IF5,IF6,IF7
!
C         CORRECTS A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
C                         AND LIQUID BOUNDARY BUT
C                         IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
C                         IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
C                         HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE SET AT -1
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
C
      ENDIF
C
      GO TO 1000
C
C-----------------------------------------------------------------------
C
900   CONTINUE
C
C     READS ERRORS
C
      IF(LNG.EQ.1) WRITE(LU,251) NLIG
      IF(LNG.EQ.2) WRITE(LU,252) NLIG
251   FORMAT(1X,'LECLIM : ERREUR DE LECTURE DANS LE FICHIER',/,1X,
     &          '         DES CONDITIONS AUX LIMITES'   ,/,1X,
     &          '         A LA LIGNE '   ,I6,//,1X,
     &          'CAUSES POSSIBLES :',//,1X,
     &          '1) RETOURS CHARRIOTS WINDOWS SUR UNIX ?',/,1X,
     &          '2) INFORMATIONS POUR LE PARALLELISME MANQUANTES ?')
252   FORMAT(1X,'LECLIM : READING ERROR IN THE BOUNDARY',/,1X,
     &          '         CONDITIONS FILE'   ,/,1X,
     &          '         AT LINE '   ,I6,//,1X,
     &          'POSSIBLE CAUSES:',//,1X,
     &          '1) WINDOWS CARRIAGE RETURN ON UNIX ?',/,1X,
     &          '2) INFORMATIONS ON PARALLELISM MISSING ?')
      CALL PLANTE(1)
      STOP
CJAJ //// BE PRECISE IN THE CASE OF THE BC FILE APPENDIX
901   CONTINUE
      WRITE (LU,*) 'LECLIM: ',
     &             'ERROR IN READING IFAPAR IN THE BC CONDITIONS FILE'
      CALL PLANTE(1)
      STOP
C
C-----------------------------------------------------------------------
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
