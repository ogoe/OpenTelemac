!                    *************************
                     SUBROUTINE LECLIM_ARTEMIS
!                    *************************
!
     &(LIHBOR,LIUBOR,NPTFR,NBOR,STDGEO,NLIM,
     & ISEG , XSEG , YSEG , NACHB , NUMLIQ,IFAPAR)
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITION FILE AND
!+                STORES THE DATA READ IN ARRAYS.
!
!history  J-M HERVOUET (LNH)
!+        17/08/1994
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
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
!| IFAPAR         |---| TABLE (1: HALO ELEMENT NUMBER, 
!|                |   |  2-4 : PROCESSOR NUMBER, 5-7 : ELEMENT NUMBER)
!| ISEG           |---| BOUNDARY SEGMENT NUMBER
!| LIHBOR         |<--| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| NACHB          |-->| NACHB(1,I) : GLOBAL (INPUT) OR LOCAL (OUTPUT)
!|                |   | NUMBER OF INTERFACE POINT.
!|                |   | NACHB(2 TO 5,I) : NUMBER OF OTHER SUB-DOMAINS
!|                |   | CONTAINING THE POINT I.
!|                |   | I IS A NUMBERING OF INTERFACE POINTS.
!| NBOR           |<--| GLOBAL NUMBER OF BOUNDARY POINTS
!| NLIM           |-->| NUMBER OF THE BOUDARY CONDITION FILE.
!| NPTFR          |-->| NUMBER OF BOURDARY POINTS.
!| NUMLIQ         |-->| BOUNDARY NUMBER OF BOUNDARY POINTS
!| STDGEO         |-->| STANDARD FOR GEOMETRY FILE.
!| XSEG, YSEG     |---| COORDINATES OF THE BOUNDARY SEGMENT 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_ARTEMIS, EX_LECLIM_ARTEMIS => LECLIM_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPTFR
      INTEGER ISEG(NPTFR),NACHB(NBMAXNSHARE,*),NUMLIQ(*)
      DOUBLE PRECISION XSEG(NPTFR),YSEG(NPTFR)
      INTEGER :: IFAPAR(6,*)
!
      INTEGER IBID,KFICH,NLIG
      INTEGER STDGEO,NLIM,K,LIHBOR(NPTFR),LIUBOR(NPTFR),NBOR(NPTFR)
      INTEGER PTIR,I
      INTEGER IF1,IF2,IF3,IF4,IF5,IF6,IF7
!
      DOUBLE PRECISION BID
!
!-----------------------------------------------------------------------
!
      REWIND NLIM
!
!-----------------------------------------------------------------------
!
! READS ALL THE LINES FROM THE CONLIM FILE
!
!
      NLIG=1
!
      DO 20 K=1,NPTFR
!
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
!
        READ(NLIM,*) LIHBOR(K), LIUBOR(K) , IBID ,
     &                 BID  , BID  , BID  ,
     &                 BID ,
     &                 IBID     ,BID      ,BID      ,BID      ,
     &                 NBOR(K)  ,KFICH
        NLIG=NLIG+1
!
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
!
        READ(NLIM,*) LIHBOR(K), LIUBOR(K) , IBID ,
     &                 BID  , BID  , BID ,
     &                 BID ,
     &                 IBID     ,BID      ,BID      ,BID      ,
     &                 NBOR(K)  ,KFICH    ,
     &                 ISEG(K),XSEG(K),YSEG(K),NUMLIQ(K)
!
        NLIG=NLIG+1
        ELSE
          IF(LNG.EQ.1) WRITE(LU,21) STDGEO
          IF(LNG.EQ.2) WRITE(LU,22) STDGEO
21        FORMAT(1X,'LECLIM_ARTEMIS : STD DU FICHIER DE GEOM.  : ',1I6,/
     &           1X,'         CETTE VALEUR EST INCONNUE ET       ',    /
     &           1X,'         LE FICHIER DES CONDITIONS LIMITES  ',    /
     &           1X,'         EN DEPEND |')
22        FORMAT(1X,'LECLIM_ARTEMIS : GEOM. FILE STD : ',I6   ,/
     &           1X,'         UNKNOWN PARAMETER AND BOUNDARY ',/
     &           1X,'         CONDITIONS FILE DEPENDS ON IT !')
          STOP
        ENDIF
!
 20     CONTINUE
!
        IF(NLIG.NE.NPTFR+1) THEN
           IF(LNG.EQ.1) WRITE(LU,23) NLIG-1,NPTFR
           IF(LNG.EQ.2) WRITE(LU,24) NLIG-1,NPTFR
 23        FORMAT(1X,'LECLIM_ARTEMIS : ERREUR DANS LE FICHIER DES',
     &          /,1X,'         CONDITIONS AUX LIMITES, ',1I5,' LIGNES',
     &          /,1X,'         AU LIEU DE ',I5)
 24        FORMAT(1X,'LECLIM_ARTEMIS : ERROR IN THE BOUNDARY
     & CONDITIONS FILE,',
     &          /,9X,1I5,' LINES INSTEAD OF ',I5,' REQUESTED')
           CALL PLANTE(1)
           STOP
        ENDIF
!
        NLIG=1
        IF(NCSIZE.GT.1) THEN
           READ(NLIM,*,ERR=900) PTIR
           NLIG=NLIG+1
           IF(NPTIR.NE.PTIR) THEN
              IF(LNG.EQ.1) WRITE(LU,151) NPTIR,PTIR
              IF(LNG.EQ.2) WRITE(LU,152) NPTIR,PTIR
 151          FORMAT(1X,'LECLIM : INCOHERENCE ENTRE GEOMETRIE ',/,1X,
     &              '         ET CONDITIONS AUX LIMITES'   ,/,1X,I6,
     &             ' POINTS INTERFACE DANS LA GEOMETRIE',/,1X,I6,
     &             ' POINTS INTERFACE DANS LE FICHIER CONLIM')
 152          FORMAT(1X,'LECLIM : DIFFERENCE BETWEEN GEOMETRY ',/,1X,
     &             '         AND BOUNDARY CONDITIONS'   ,/,1X,I6,
     &             ' INTERFACE POINTS IN GEOMETRY',/,1X,I6,
     &             ' INTERFACE POINTS IN CONLIM FILE')
           ENDIF
!     NACHB(NBMAXNSHARE,NPTIR), HERE NACHB(I,K)
           DO K=1,NPTIR
                 READ(NLIM,*,ERR=900) (NACHB(I,K), I=1,NBMAXNSHARE)
           ENDDO
            READ(NLIM,*,ERR=901) NHALO
        IF(NHALO.GT.2*NPTIR) THEN ! SEE BIEF LIB, SUBROUTINE ALMESH
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
!
          IFAPAR(1,IF1)=IF2
          IFAPAR(2,IF1)=IF3
          IFAPAR(3,IF1)=IF4
          IFAPAR(4,IF1)=IF5
          IFAPAR(5,IF1)=IF6
          IFAPAR(6,IF1)=IF7
        ENDDO
!
      ENDIF
!
!
!
!      JAJ //// READS THE NEIGHBOURHOODS FOR HALO CELLS ALONG THE INTERFACES
!      FILLING PATTERN: IFAPAR(1:7,K), K=1:NHALO
!                       -> NHALO: NUMBER OF HALO CELLS IN THIS PARTITION
!
!      IFAPAR(1,K)   : HALO ELEMENT -LOCAL- NUMBER IN THIS PARTITION
!      IFAPAR(2:4,K) : PROCESSOR NUMBERS BEHIND THE 3 ELEMENT EDGES
!                      NUMBER FROM 0 TO NCSIZE-1
!      IFAPAR(5:7,K) : -LOCAL- ELEMENT NUMBERS BEHIND THE 3 EDGES
!                      IN THE NUMBERING OF PARTITIONS THEY BELONG TO
!      ACTUALLY, NOT ALL OF THAT IS REQUIRED AND CAN BE OPTIMISED
!      AFTER THE DEVELOPMENT STAGE IS OVER
!
!      IN TELEMAC, IFAPAR IS REORGANISED IN IFAPAR(6,NELEM2)
!                  AND INITIALISED TO 0 IN ALMESH
!
!
!
      GOTO 1000
900   CONTINUE
!
!     READS ERRORS
!
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
!
!
      STOP
!JAJ //// BE PRECISE IN THE CASE OF THE BC FILE APPENDIX
901   CONTINUE
      WRITE (LU,*) 'LECLIM:
     & ERROR IN READING IFAPAR IN THE BC CONDITIONS FILE'
      CALL PLANTE(1)
      STOP
!
!-----------------------------------------------------------------------
!
1000  CONTINUE
!
      RETURN
      END
