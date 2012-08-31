!                    *************************
                     SUBROUTINE LECLIM_TOMAWAC
!                    *************************
!
     &(LIHBOR, HBOR , NPTFR, NBOR  , STDGEO, NLIM,
     & ISEG  , XSEG , YSEG , NACHB , MESH,BOUNDARY_COLOUR)
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/06/2011
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITION FILE AND
!+                STORES THE DATA READ IN ARRAYS.
!
!history  J-M HERVOUET (LNH)
!+        24/04/97
!+
!+
!
!history  OPTIMER
!+        25/08/00
!+        V5P0
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BOUNDARY_COLOUR|<--| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| ISEG           |<--| GLOBAL NUMBER OF FOLLOWING OR PRECEDING POINT
!|                |   | IN THE BOUNDARY IF IT IS IN ANOTHER SUB-DOMAIN
!| LIHBOR         |<--| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| MESH           |<--| BIEF OBJECT
!| NACHB          |<--| NUMBERS OF PROCESSORS CONTAINING A GIVEN POINT
!| NBOR           |<--| GLOBAL NUMBER OF BOUNDARY POINTS
!| NLIM           |-->| LOGICAL UNIT NUMBER OF BOUNDARY CONDITIONS FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| STDGEO         |-->| STANDARD DU FICHIER DE GEOMETRIE.
!| XSEG           |<--| X COORDINATE OF FOLLOWING OR PRECEDING POINT
!|                |   | IN THE BOUNDARY IF IT IS IN ANOTHER SUB-DOMAIN
!| YSEG           |<--| Y COORDINATE OF FOLLOWING OR PRECEDING POINT
!|                |   | IN THE BOUNDARY IF IT IS IN ANOTHER SUB-DOMAIN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPTFR
!BD_INCKA MODIFICATION FOR PARALLEL MODE
!      INTEGER ISEG(NPTFR),NACHB(5,*),PTIR,I
      INTEGER ISEG(NPTFR),PTIR,I
      INTEGER NACHB(NBMAXNSHARE*NPTIR)
      INTEGER IF1,IF2,IF3,IF4,IF5,IF6,IF7
      TYPE(BIEF_MESH)  MESH
!BD_INCKA END OF MODIFICATION
      DOUBLE PRECISION XSEG(NPTFR),YSEG(NPTFR)
!
      INTEGER IBID,KFICH
      INTEGER STDGEO,NLIM,K
      INTEGER LIHBOR(NPTFR)
      INTEGER NBOR(NPTFR)
!
      DOUBLE PRECISION HBOR(NPTFR)
      DOUBLE PRECISION BID
!BD_INCKA TO READ NUMLIQ
      INTEGER NUMLIQ
      INTEGER NLIG
      INTEGER BOUNDARY_COLOUR(NPTFR)
!BD_INCKA END
!
!-----------------------------------------------------------------------
!
      REWIND NLIM
!
!-----------------------------------------------------------------------
!
! READS ALL THE LINES IN THE FILE DYNAM.
!
! NO TRACER IN DYNAM
!
      DO 20 K=1,NPTFR
!
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
!
        READ(NLIM,*) LIHBOR(K), IBID, IBID,
     &                 HBOR(K), BID , BID , BID ,
     &                 IBID   , BID , BID , BID ,
     &                 NBOR(K)            , KFICH
!
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
!
        READ(NLIM,*) LIHBOR(K), IBID, IBID,
     &                 HBOR(K), BID , BID , BID  ,
     &                 IBID   , BID , BID , BID  ,
     &                 NBOR(K)            , KFICH,
     &                 MESH%ISEG%I(K),MESH%XSEG%R(K),
     &                 MESH%YSEG%R(K),IBID
!
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
!
          BOUNDARY_COLOUR(K)=KFICH
!
!
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
!
20    CONTINUE
!
!-----------------------------------------------------------------------
!
!  IN PARALLEL MODE : READS NPTIR AND NACHB
!
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
!          READ(NLIM,*) (NACHB(I,K),I=1,5)
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
! !         READ(NLIM,*,ERR=901) (MESH%IFAPAR%I(7*(K-1)+I),I=1,7)
           READ(NLIM,*,ERR=901) IF1,IF2,IF3,IF4,IF5,IF6,IF7
! !
!          CORRECTING A BUG (IN IFAPAR THERE IS A CONFUSION BETWEEN PROCESSOR 0
!                            AND LIQUID BOUNDARY BUT
!                            IN CASE OF LIQUID BOUNDARY, THE ELEMENT BEHIND
!                            IS GIVEN AS 0, SO BOTH CASES MAY BE DISTINGUISHED
!                           HERE ALL BOUNDARIES (LIQUID OR SOLID) ARE PUT AT -1
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
!
!-----------------------------------------------------------------------
!
900   CONTINUE
!
!     READS ERRORS
!
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
