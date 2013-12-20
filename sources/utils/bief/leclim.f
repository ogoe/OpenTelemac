!                    *****************
                     SUBROUTINE LECLIM
!                    *****************
!
     &(LIHBOR,LIUBOR,LIVBOR,LITBOR,HBOR,UBOR,VBOR,TBOR,
     & CHBORD,ATBOR,BTBOR,NPTFR,STDGEO,TRAC,NLIM,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,NUMLIQ,MESH,BOUNDARY_COLOUR,
     & NPTFR2)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITIONS FILE AND
!+                STORES IN ARRAYS THE DATA READ.
!
!history  J-M HERVOUET (LNHE)
!+        09/07/2009
!+        V6P0
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
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| BOUNDARY_COLOUR|<--| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| MESH           |-->| MESH STRUCTURE
!| NLIM           |-->| LOGICAL UNIT OF BOUNDARY CONDITIONS FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| STDGEO         |-->| STANDARD OF GEOMETRY FILE.
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_LECLIM => LECLIM
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER PTIR,I,IBID,KFICH,K,NLIG,IF1,IF2,IF3,IF4,IF5,IF6,IF7
      INTEGER DIMUBOR
!
      DOUBLE PRECISION BID
!
!-----------------------------------------------------------------------
!
      DIMUBOR=NPTFR
      IF(PRESENT(NPTFR2)) DIMUBOR=NPTFR2
!
!-----------------------------------------------------------------------
!
      REWIND NLIM
!
!-----------------------------------------------------------------------
!
! READS EACH LINE OF FILE DYNAM
!
!
! 1) WITHOUT TRACER
!
      NLIG=1
      IF(.NOT.TRAC) THEN
!
        DO K=1,NPTFR
!
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
!
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K)  ,
     &                       CHBORD(K) ,IBID,BID,BID,BID,
     &                       MESH%NBOR%I(K)  ,KFICH
        NLIG=NLIG+1
!
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
!
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K)  ,
     &                       CHBORD(K),IBID,BID,BID,BID,
     &                       MESH%NBOR%I(K),KFICH,
     &                       MESH%ISEG%I(K),
     &                       MESH%XSEG%R(K),MESH%YSEG%R(K),
     &                       NUMLIQ(K)
        NLIG=NLIG+1
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
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          BOUNDARY_COLOUR(K)=KFICH
        ENDIF
!
        ENDDO
!
      ENDIF
!
! 2) WITH TRACER
!
      IF(TRAC) THEN
!
        DO K=1,NPTFR
!
        IF(STDGEO.EQ.3.AND.NCSIZE.LE.1) THEN
!
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K),
     &                       CHBORD(K) ,LITBOR(K),TBOR(K),
     &                       ATBOR(K),BTBOR(K),
     &                       MESH%NBOR%I(K),KFICH
        NLIG=NLIG+1
!
        ELSEIF(STDGEO.EQ.3.AND.NCSIZE.GT.1) THEN
!
        READ(NLIM,*,ERR=900) LIHBOR(K),LIUBOR(K),LIVBOR(K),
     &                       HBOR(K)  ,UBOR(K)  ,VBOR(K),
     &                       CHBORD(K) ,LITBOR(K),TBOR(K),
     &                       ATBOR(K) ,BTBOR(K) ,MESH%NBOR%I(K),
     &                       KFICH,
     &                       MESH%ISEG%I(K),
     &                       MESH%XSEG%R(K),MESH%YSEG%R(K),
     &                       NUMLIQ(K)
        NLIG=NLIG+1
!
        ELSE
          IF(LNG.EQ.1) WRITE(LU,21) STDGEO
          IF(LNG.EQ.2) WRITE(LU,22) STDGEO
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          BOUNDARY_COLOUR(K)=KFICH
        ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CHECKS, CORRECTS AND STORES:
!
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
!
      DO 50 K=1,NPTFR
!
!     CHECKING FRICTION COEFFICIENT
!
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
!
!     ADHERENCE FOR H CHANGED AT THE WALL
!
      IF(LIHBOR(K).EQ.KADH) THEN
        LIHBOR(K)=KLOG
        IF(LNG.EQ.1) WRITE(LU,51) K
        IF(LNG.EQ.2) WRITE(LU,52) K
51      FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &            '         CHANGEE EN CONDITION DE PAROI')
52      FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &            '         CHANGEE EN CONDITION DE PAROI')
      ENDIF
!
!     INCIDENT WAVE FOR H TREATED LIKE A FREE EXIT
!
      IF(LIHBOR(K).EQ.KINC) THEN
        LIHBOR(K)=KSORT
      ENDIF
!
!     CANCELS DIRICHLET VALUES WHEN THE POINT IS NOT DIRICHLET
!     FOR POINTS WITH ADHERENCE, NEEDS UBOR OR VBOR =0
!
!     IF(LIHBOR(K).NE.KENT) HBOR(K)=0.D0
      IF(LIUBOR(K).NE.KENT.AND.LIUBOR(K).NE.KENTU) UBOR(K)=0.D0
      IF(LIVBOR(K).NE.KENT.AND.LIVBOR(K).NE.KENTU) VBOR(K)=0.D0
!
!     BACKS UP UBOR AND VBOR ON THEIR SECOND DIMENSION
!
      UBOR(K+DIMUBOR) = UBOR(K)
      VBOR(K+DIMUBOR) = VBOR(K)
!
50    CONTINUE
!
      IF(TRAC) THEN
        DO K=1,NPTFR
          IF(LITBOR(K).NE.KENT) TBOR(K)=0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  PARALLEL MODE : READS NPTIR AND NACHB
!
!  NOTE: NACHB IS HERE IN GLOBAL NUMBERING OF POINTS, IT WILL BE REDUCED
!        TO THE LOCAL NUMBERING OF THIS SUB-DOMAIN IN SUBROUTINE PARAGL
!
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
!       NACHB(NBMAXNSHARE,NPTIR), HERE NACHB(I,K)
!       HERE NACHB IS IN GLOBAL NUMBERING, IT WILL BE CHANGED INTO
!       LOCAL NUMBERING IN PARAGL
        DO K=1,NPTIR
          READ(NLIM,*,ERR=900) (MESH%NACHB%I((K-1)*NBMAXNSHARE+I),
     &                          I=1,NBMAXNSHARE)
          NLIG=NLIG+1
        ENDDO
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
!
      ENDIF
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!
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
