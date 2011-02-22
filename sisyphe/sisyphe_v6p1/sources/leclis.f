C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE BOUNDARY CONDITION FILE AND
!>                STORES THE DATA READ IN ARRAYS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  JMH : SHOULD REALLY CALL LECLIM INSTEAD
!>               IDENTIFYING THE DIFFERENCES : COPY FOR THE CLASSES ?

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AFBOR, BFBOR, BOUNDARY_COLOUR, EBOR, ISEG, KENT, LIEBOR, MESH, NACHB, NBOR, NLIM, NPTFR, NSICLA, NUMLIQ, STDGEO, XSEG, YSEG
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
!>    </th><td> BID, I, IBID, IF1, IF2, IF3, IF4, IF5, IF6, IF7, K, PTIR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 01/10/2008
!> </td><td> JMH
!> </td><td> CORRECTED IFAPAR (FINITE VOLUME IN PARALLEL MODE)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 12/08/2008
!> </td><td> JMH
!> </td><td> READS NHALO AND IFAPAR (CHARACTERISTICS IN PARALLEL MODE)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/06/2008
!> </td><td> JMH
!> </td><td> ADDED ARGUMENT BOUNDARY_COLOUR
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> C. LENORMANT; C. MACHET; JACEK.JANKOWSKI@BAW.DE
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BOUNDARY_COLOUR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td><--</td><td>EVOLUTION AUX BORDS
!>    </td></tr>
!>          <tr><td>ISEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE PAROI (ADHERENCE)
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>TYPE DE CONDITION LIMITE D'ENTREE.
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
!>          <tr><td>LIEBOR
!></td><td><--</td><td>TYPES DE CONDITIONS AUX LIMITES EN TEMPERA-
!>                  TURE POUR LES POINTS DE BORD.
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
!>          <tr><td>NSICLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
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
                        SUBROUTINE LECLIS
     &(LIEBOR,EBOR,NPTFR,NBOR,STDGEO,NLIM,KENT,ISEG,XSEG,YSEG,
     & NACHB,NUMLIQ,NSICLA,AFBOR,BFBOR,BOUNDARY_COLOUR,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AFBOR          |---| 
C| BFBOR          |---| 
C| BOUNDARY_COLOUR|---| 
C| EBOR           |<--| EVOLUTION AUX BORDS
C| ISEG           |---| 
C| KADH           |-->| TYPE DE CONDITION LIMITE DE PAROI (ADHERENCE)
C| KENT           |-->| TYPE DE CONDITION LIMITE D'ENTREE.
C| KINC           |-->| TYPE DE CONDITION LIMITE D'ONDE INCIDENTE
C| KLOG           |-->| TYPE DE CONDITION LIMITE DE PAROI (PAROI)
C| KSORT          |-->| TYPE DE CONDITION LIMITE DE SORTIE LIBRE
C| LIEBOR         |<--| TYPES DE CONDITIONS AUX LIMITES EN TEMPERA-
C|                |   | TURE POUR LES POINTS DE BORD.
C| MESH           |---| 
C| NACHB          |---| 
C| NBOR           |<--| ADRESSES DES POINTS DE BORD.
C| NLIM           |-->| NUMERO DE CANAL DU FICHIER DES CONDITIONS LIM.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NSICLA         |---| 
C| NUMLIQ         |---| 
C| STDGEO         |-->| STANDARD DU FICHIER DE GEOMETRIE.
C| XSEG           |---| 
C| YSEG           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPTFR
      INTEGER, INTENT(INOUT)         :: LIEBOR(NPTFR)
      TYPE(BIEF_OBJ),INTENT(INOUT)   :: EBOR
      INTEGER, INTENT(INOUT)         :: NBOR(NPTFR)
      INTEGER, INTENT(INOUT)         :: BOUNDARY_COLOUR(NPTFR)
      INTEGER, INTENT(IN)            :: STDGEO,NLIM,KENT,NSICLA
      DOUBLE PRECISION, INTENT(INOUT):: XSEG(NPTFR),YSEG(NPTFR)
      INTEGER, INTENT(INOUT)         :: ISEG(NPTFR),NACHB(NBMAXNSHARE,*)
      INTEGER, INTENT(INOUT)         :: NUMLIQ(*)
      DOUBLE PRECISION, INTENT(INOUT):: AFBOR(NPTFR),BFBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IBID,PTIR,I,K,IF1,IF2,IF3,IF4,IF5,IF6,IF7
      DOUBLE PRECISION BID
C
C-----------------------------------------------------------------------
C
      REWIND NLIM
C
C-----------------------------------------------------------------------
C
C READS ALL THE LINES IN THE BOUNDARY CONDITION FILE
C
C
        DO 40 K=1,NPTFR
C
        IF (STDGEO.EQ.3 .AND. NCSIZE.LE.1) THEN

          READ(NLIM,*) IBID,IBID,IBID,BID,BID,BID,BID,
     &                 LIEBOR(K),EBOR%ADR(1)%P%R(K),AFBOR(K),BFBOR(K),
     &                 NBOR(K),BOUNDARY_COLOUR(K)

        ELSEIF (STDGEO.EQ.3 .AND. NCSIZE.GT.1) THEN

          READ(NLIM,*) IBID,IBID,IBID,BID,BID,BID,BID,
     &                 LIEBOR(K),EBOR%ADR(1)%P%R(K),AFBOR(K),BFBOR(K),
     &                 NBOR(K),BOUNDARY_COLOUR(K),
     &                 ISEG(K),XSEG(K),YSEG(K),NUMLIQ(K)
C
        ELSE
          IF(LNG.EQ.1) WRITE(LU,21) STDGEO
          IF(LNG.EQ.2) WRITE(LU,22) STDGEO
          CALL PLANTE(1)
          STOP
        ENDIF
C
40      CONTINUE
C
21        FORMAT(1X,'LECLIS : STANDARD DU FICHIER DE GEOMETRIE : ',1I6,/
     &           1X,'         CETTE VALEUR EST INCONNUE ET       ',    /
     &           1X,'         LE FICHIER DES CONDITIONS LIMITES  ',    /
     &           1X,'         EN DEPEND |')
22        FORMAT(1X,'LECLIS : GEOMETRY FILE STANDARD : ',I6   ,/
     &           1X,'         UNKNOWN PARAMETER AND BOUNDARY ',/
     &           1X,'         CONDITIONS FILE DEPENDS ON IT !')
23        FORMAT(1X,'LECLIS : ERREUR LIGNE ',I5,' DANS LE FICHIER DES',
     &         /,1X,'         CONDITIONS AUX LIMITES, POINT DE BORD',
     &         /,1X,'         NUMERO ',I5,' AU LIEU DE ',I5)
24        FORMAT(1X,'LECLIS : ERROR LINE ',I5,' IN BOUNDARY CONDITIONS',
     &         /,1X,'         FILE, BOUNDARY POINT NUMBER ',I5,
     &         /,1X,'         INSTEAD OF ',I5)
C
C-----------------------------------------------------------------------
C
C  CHECKS, CORRECTS AND STORES :
C
        DO K=1,NPTFR
C
          IF(LIEBOR(K).NE.KENT) EBOR%ADR(1)%P%R(K)=0.D0
C
C         COPIES THE SAME EBOR FOR ALL CLASSES
          IF(NSICLA.GE.2) THEN
            DO I=2,NSICLA
              EBOR%ADR(I)%P%R(K)=EBOR%ADR(1)%P%R(K)
            ENDDO
          ENDIF
C
        ENDDO
C
C-----------------------------------------------------------------------
C
C  IN PARALLEL MODE : READS NPTIR AND NACHB
C
      IF(NCSIZE.GT.1) THEN
        READ(NLIM,*) PTIR
        IF(NPTIR.NE.PTIR) THEN
          IF(LNG==1) WRITE(LU,151) NPTIR,PTIR
          IF(LNG==2) WRITE(LU,152) NPTIR,PTIR
151       FORMAT(1X,'LECLIS : INCOHERENCE ENTRE GEOMETRIE ',/,1X,
     &              '         ET CONDITIONS AUX LIMITES'   ,/,1X,I6,
     &    ' POINTS INTERFACE DANS LA GEOMETRIE',/,1X,I6,
     &    ' POINTS INTERFACE DANS LE FICHIER CONLIM')
152       FORMAT(1X,'LECLIS : DIFFERENCE BETWEEN GEOMETRY ',/,1X,
     &              '         AND BOUNDARY CONDITIONS'   ,/,1X,I6,
     &    ' INTERFACE POINTS IN GEOMETRY',/,1X,I6,
     &    ' INTERFACE POINTS IN CONLIM FILE')
        ENDIF
        DO K=1,NPTIR
          READ(NLIM,*) (NACHB(I,K),I=1,NBMAXNSHARE)
        ENDDO
C
C       JAJ //// READ THE NEIGHBOURHOODS FOR HALO CELLS ALONG THE INTERFACES
C       FILLING PATTERN: IFAPAR(1:7,K), K=1:NHALO
C                        -> NHALO: NUMBER OF HALO CELLS IN THIS PARTITION
!
C       IFAPAR(1,K)   : HALO ELEMENT -LOCAL- NUMBER IN THIS PARTITION
C       IFAPAR(2:4,K) : PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
C       IFAPAR(5:7,K) : -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
C                       IN THE NUMBERING OF PARTITIONS THEY BELONG TO
C       ACTUALLY, NOT ALL OF THAT IS REQUIRED AND CAN BE OPTIMISED
C       AFTER THE DEVELOPMENT STAGE IS OVER
!
        READ(NLIM,*,ERR=901) NHALO
        IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIBRARY, SUBROUTINE ALMESH
          WRITE(LU,*) ' => NHALO>2*NPTIR DETECTED IN BC FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO K=1,NHALO
C         READ(NLIM,*,ERR=901) (MESH%IFAPAR%I(7*(K-1)+I),I=1,7)
          READ(NLIM,*,ERR=901) IF1,IF2,IF3,IF4,IF5,IF6,IF7
!
C         CORRECTING A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
C                           AND LIQUID BOUNDARY BUT
C                           IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
C                           IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
C                           HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE PUT AT -1
!
          IF(IF5.EQ.0) IF2=-1
          IF(IF6.EQ.0) IF3=-1
          IF(IF7.EQ.0) IF4=-1
          MESH%IFAPAR%I(6*(IF1-1)+1)=IF2
          MESH%IFAPAR%I(6*(IF1-1)+2)=IF3
          MESH%IFAPAR%I(6*(IF1-1)+3)=IF4
          MESH%IFAPAR%I(6*(IF1-1)+4)=IF5
          MESH%IFAPAR%I(6*(IF1-1)+5)=IF6
          MESH%IFAPAR%I(6*(IF1-1)+6)=IF7
        ENDDO
      ENDIF
C
      GO TO 1000
C
C-----------------------------------------------------------------------
C
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