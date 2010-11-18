C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE BOUNDARY CONDITION FILE AND
!>                STORES THE DATA READ IN ARRAYS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BOUNDARY_COLOUR, HBOR, ISEG, LIHBOR, MESH, NACHB, NBOR, NLIM, NPTFR, STDGEO, XSEG, YSEG
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
!>    </th><td> BID, I, IBID, IF1, IF2, IF3, IF4, IF5, IF6, IF7, K, KFICH, NLIG, NUMLIQ, PTIR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 5.0                                       </center>
!> </td><td> 25/08/00
!> </td><td> OPTIMER   02 98 44 24 51
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 24/04/97
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BOUNDARY_COLOUR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td><--</td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>ISEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES EN HAUTEUR
!>                  POUR LES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NACHB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td><--</td><td>ADRESSES DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NLIM
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES CONDITIONS LIM.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>STDGEO
!></td><td>--></td><td>STANDARD DU FICHIER DE GEOMETRIE.
!>    </td></tr>
!>          <tr><td>XSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YSEG
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECLIM_TOMAWAC
     &(LIHBOR, HBOR , NPTFR, NBOR  , STDGEO, NLIM,
     & ISEG  , XSEG , YSEG , NACHB , MESH,BOUNDARY_COLOUR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BOUNDARY_COLOUR|---| 
C| HBOR           |<--| CONDITIONS AUX LIMITES SUR H
C| ISEG           |---| 
C| LIHBOR         |<--| TYPES DE CONDITIONS AUX LIMITES EN HAUTEUR
C|                |   | POUR LES POINTS DE BORD.
C| MESH           |---| 
C| NACHB          |---| 
C| NBOR           |<--| ADRESSES DES POINTS DE BORD.
C| NLIM           |-->| NUMERO DE CANAL DU FICHIER DES CONDITIONS LIM.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| STDGEO         |-->| STANDARD DU FICHIER DE GEOMETRIE.
C| XSEG           |---| 
C| YSEG           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPTFR
!BD_INCKA MODIFICATION FOR PARALLEL MODE
C      INTEGER ISEG(NPTFR),NACHB(5,*),PTIR,I
      INTEGER ISEG(NPTFR),PTIR,I
      INTEGER NACHB(NBMAXNSHARE*NPTIR)
      INTEGER IF1,IF2,IF3,IF4,IF5,IF6,IF7
      TYPE(BIEF_MESH)  MESH
!BD_INCKA END OF MODIFICATION
      DOUBLE PRECISION XSEG(NPTFR),YSEG(NPTFR)
C
      INTEGER IBID,KFICH
      INTEGER STDGEO,NLIM,K
      INTEGER LIHBOR(NPTFR)
      INTEGER NBOR(NPTFR)
C
      DOUBLE PRECISION HBOR(NPTFR)
      DOUBLE PRECISION BID
!BD_INCKA TO READ NUMLIQ
      INTEGER NUMLIQ
      INTEGER NLIG
      INTEGER BOUNDARY_COLOUR(NPTFR)

!BD_INCKA END
C
C-----------------------------------------------------------------------
C
      REWIND NLIM
C
C-----------------------------------------------------------------------
C
C READS ALL THE LINES IN THE FILE DYNAM.
C
C NO TRACER IN DYNAM
C
      DO 20 K=1,NPTFR
C
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
C
        READ(NLIM,*) LIHBOR(K), IBID, IBID,
     &                 HBOR(K), BID , BID , BID ,
     &                 IBID   , BID , BID , BID ,
     &                 NBOR(K)            , KFICH
C
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
C
        READ(NLIM,*) LIHBOR(K), IBID, IBID,
     &                 HBOR(K), BID , BID , BID  ,
     &                 IBID   , BID , BID , BID  ,
     &                 NBOR(K)            , KFICH,
     &                 MESH%ISEG%I(K),MESH%XSEG%R(K),
     &                 MESH%YSEG%R(K),IBID
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
          STOP
        ENDIF
C
          BOUNDARY_COLOUR(K)=KFICH
C
C
!BD_INCKA
        IF (NCSIZE.LE.1) THEN
!BD_INCKA
       IF(KFICH.NE.K) THEN
          IF(LNG.EQ.1) WRITE(LU,23) K,KFICH,K
          IF(LNG.EQ.2) WRITE(LU,24) K,KFICH,K
23        FORMAT(1X,'LECLIM : ERREUR LIGNE ',I5,' DANS LE FICHIER DES',
     &         /,1X,'         CONDITIONS AUX LIMITES, POINT DE BORD',
     &         /,1X,'         NUMERO ',I5,' AU LIEU DE ',I5)
24        FORMAT(1X,'LECLIM : ERROR LINE ',I5,' IN BOUNDARY CONDITIONS',
     &         /,1X,'         FILE, BOUNDARY POINT NUMBER ',I5,
     &         /,1X,'         INSTEAD OF ',I5)
          STOP
        ENDIF
!BD_INCKA
      ENDIF
!BD_INCKA
C
20    CONTINUE
C
C-----------------------------------------------------------------------
C
C  IN PARALLEL MODE : READS NPTIR AND NACHB
C
      IF(NCSIZE.GT.1) THEN
        READ(NLIM,*) PTIR
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
        DO 153 K=1,NPTIR
!BD_INCKA MODIFICATION FOR PARALLEL MODE
C          READ(NLIM,*) (NACHB(I,K),I=1,5)
          READ(NLIM,*,ERR=900) (MESH%NACHB%I((K-1)*NBMAXNSHARE+I),
     &                          I=1,NBMAXNSHARE)
!BD_INCKA END OF MODIFICATION
153     CONTINUE
!BD_INCKA MODIFICATION FOR PARALLEL MODE
        READ(NLIM,*,ERR=901) NHALO
        IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIBRARY, SUBROUTINE ALMESH
          WRITE(LU,*) ' => NHALO>2*NPTIR DETECTED IN BC FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
         DO K=1,NHALO
C !         READ(NLIM,*,ERR=901) (MESH%IFAPAR%I(7*(K-1)+I),I=1,7)
           READ(NLIM,*,ERR=901) IF1,IF2,IF3,IF4,IF5,IF6,IF7
! !
C          CORRECTING A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
C                            AND LIQUID BOUNDARY BUT
C                            IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
C                            IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
C                           HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE PUT AT -1
! !
           IF(IF5.EQ.0) IF2=-1
           IF(IF6.EQ.0) IF3=-1
           IF(IF7.EQ.0) IF4=-1
! !
           MESH%IFAPAR%I(6*(IF1-1)+1)=IF2
           MESH%IFAPAR%I(6*(IF1-1)+2)=IF3
           MESH%IFAPAR%I(6*(IF1-1)+3)=IF4
           MESH%IFAPAR%I(6*(IF1-1)+4)=IF5
           MESH%IFAPAR%I(6*(IF1-1)+5)=IF6
           MESH%IFAPAR%I(6*(IF1-1)+6)=IF7
         ENDDO
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      ENDIF
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      GO TO 1000
C
C-----------------------------------------------------------------------
C
900   CONTINUE
C
C     READS ERRORS
C
      NLIG=1
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