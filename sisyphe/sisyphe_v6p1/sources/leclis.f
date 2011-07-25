!                    *****************
                     SUBROUTINE LECLIS
!                    *****************
!
     &(LIEBOR,EBOR,NPTFR,NBOR,STDGEO,NLIM,KENT,ISEG,XSEG,YSEG,
     & NACHB,NUMLIQ,NSICLA,AFBOR,BFBOR,BOUNDARY_COLOUR,MESH)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITION FILE AND
!+                STORES THE DATA READ IN ARRAYS.
!
!note     JMH : SHOULD REALLY CALL LECLIM INSTEAD
!+               IDENTIFYING THE DIFFERENCES : COPY FOR THE CLASSES ?
!
!history  C. LENORMANT; C. MACHET; JACEK.JANKOWSKI@BAW.DE
!+
!+
!+
!
!history  JMH
!+        16/06/2008
!+
!+   ADDED ARGUMENT BOUNDARY_COLOUR
!
!history  JMH
!+        12/08/2008
!+
!+   READS NHALO AND IFAPAR (CHARACTERISTICS IN PARALLEL MODE)
!
!history  JMH
!+        01/10/2008
!+        V5P9
!+   CORRECTED IFAPAR (FINITE VOLUME IN PARALLEL MODE)
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
!| AFBOR          |<->| BOUNDARY CONDITION ON F: NU*DF/DN=AFBOR*F+BFBOR
!| BFBOR          |<->| BOUNDARY CONDITION ON F: NU*DF/DN=AFBOR*F+BFBOR
!| BOUNDARY_COLOUR|<->| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| EBOR           |<->| PRESCRIBED EVOLUTION ON THE BOUNDARY 
!| ISEG           |<->| NUMBER OF SEGMENTS ON THE BOUNDARY 
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| LIEBOR         |<->| TYPE OF BOUNDARY CONDITIONS FOR BED EVOLUTION
!| MESH           |<->| MESH STRUCTURE
!| NACHB          |<->| NUMBER OF NEIGHBOUR POINT **** 
!| NBOR           |<->| ADRESSES DES POINTS DE BORD.
!| NLIM           |-->| NUMERO DE CANAL DU FICHIER DES CONDITIONS LIM.
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| STDGEO         |-->| STANDARD OF GEOMETRY FILE
!| XSEG           |---| A SUPPRIMER
!| YSEG           |---| A SUPPRIMER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IBID,PTIR,I,K,IF1,IF2,IF3,IF4,IF5,IF6,IF7
      DOUBLE PRECISION BID
!
!-----------------------------------------------------------------------
!
      REWIND NLIM
!
!-----------------------------------------------------------------------
!
! READS ALL THE LINES IN THE BOUNDARY CONDITION FILE
!
!
        DO 40 K=1,NPTFR
!
        IF (STDGEO.EQ.3 .AND. NCSIZE.LE.1) THEN
          READ(NLIM,*) IBID,IBID,IBID,BID,BID,BID,BID,
     &                 LIEBOR(K),EBOR%ADR(1)%P%R(K),AFBOR(K),BFBOR(K),
     &                 NBOR(K),BOUNDARY_COLOUR(K)
        ELSEIF (STDGEO.EQ.3 .AND. NCSIZE.GT.1) THEN
          READ(NLIM,*) IBID,IBID,IBID,BID,BID,BID,BID,
     &                 LIEBOR(K),EBOR%ADR(1)%P%R(K),AFBOR(K),BFBOR(K),
     &                 NBOR(K),BOUNDARY_COLOUR(K),
     &                 ISEG(K),XSEG(K),YSEG(K),NUMLIQ(K)
!
        ELSE
          IF(LNG.EQ.1) WRITE(LU,21) STDGEO
          IF(LNG.EQ.2) WRITE(LU,22) STDGEO
          CALL PLANTE(1)
          STOP
        ENDIF
!
40      CONTINUE
!
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
!
!-----------------------------------------------------------------------
!
!  CHECKS, CORRECTS AND STORES :
!
        DO K=1,NPTFR
!
          IF(LIEBOR(K).NE.KENT) EBOR%ADR(1)%P%R(K)=0.D0
!
!         COPIES THE SAME EBOR FOR ALL CLASSES
          IF(NSICLA.GE.2) THEN
            DO I=2,NSICLA
              EBOR%ADR(I)%P%R(K)=EBOR%ADR(1)%P%R(K)
            ENDDO
          ENDIF
!
        ENDDO
!
!-----------------------------------------------------------------------
!
!  IN PARALLEL MODE : READS NPTIR AND NACHB
!
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
!
!       JAJ //// READ THE NEIGHBOURHOODS FOR HALO CELLS ALONG THE INTERFACES
!       FILLING PATTERN: IFAPAR(1:7,K), K=1:NHALO
!                        -> NHALO: NUMBER OF HALO CELLS IN THIS PARTITION
!
!       IFAPAR(1,K)   : HALO ELEMENT -LOCAL- NUMBER IN THIS PARTITION
!       IFAPAR(2:4,K) : PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
!       IFAPAR(5:7,K) : -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
!                       IN THE NUMBERING OF PARTITIONS THEY BELONG TO
!       ACTUALLY, NOT ALL OF THAT IS REQUIRED AND CAN BE OPTIMISED
!       AFTER THE DEVELOPMENT STAGE IS OVER
!
        READ(NLIM,*,ERR=901) NHALO
        IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIBRARY, SUBROUTINE ALMESH
          WRITE(LU,*) ' => NHALO>2*NPTIR DETECTED IN BC FILE'
          CALL PLANTE(1)
          STOP
        ENDIF
        DO K=1,NHALO
!         READ(NLIM,*,ERR=901) (MESH%IFAPAR%I(7*(K-1)+I),I=1,7)
          READ(NLIM,*,ERR=901) IF1,IF2,IF3,IF4,IF5,IF6,IF7
!
!         CORRECTING A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
!                           AND LIQUID BOUNDARY BUT
!                           IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
!                           IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
!                           HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE PUT AT -1
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
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!
!JAJ //// BE PRECISE IN THE CASE OF THE BC FILE APPENDIX
901   CONTINUE
      WRITE (LU,*) 'LECLIM: ',
     &             'ERROR IN READING IFAPAR IN THE BC CONDITIONS FILE'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
