!                    *****************
                     SUBROUTINE SORFLO
!                    *****************
!
     &(XFLOT,YFLOT,IKLFLO,DEBFLO,FINFLO,NFLOT,NITFLO,FLOPRD,
     & NRBI,TITCAS,BINRES,NOMRBI,NIT,MAXVAR,DATE,TIME,MESH,
     & I_ORIG,J_ORIG)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT THE BINARY RESULTS FILE (SELAFIN FORMAT) WITH
!+                THE DATA ON TRAJECTORY OF THE FLOATING BODIES.
!
!history  J-M JANIN (LNH)
!+        08/03/2007
!+        V5P7
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
!| BINRES         |-->| TYPE DE BINAIRE DU FICHIER DE RESULTATS
!| DATE           |---|
!| DEBFLO         |-->| NUMEROS DES PAS DE TEMPS DE LARGAGE DE
!|                |   | CHAQUE FLOTTEUR.
!| FINFLO         |-->| NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DE
!|                |   | DERIVE POUR CHAQUE FLOTTEUR.
!| FLOPRD         |-->| NOMBRE DE PAS DE TEMPS ENTRE 2 ENREGITREMENTS
!|                |   | DES POSITIONS SUCCESSIVES DES FLOTTEURS.
!| IKLFLO         |---| TABLE DE CONNECTIVITE BIDON UTILISEE POUR LA
!|                |   | SORTIE DES TRAJECTOIRES SOUS FORME DE MAILLAGE
!| I_ORIG         |---|
!| J_ORIG         |---|
!| MAXVAR         |---|
!| MESH           |---|
!| NFLOT          |-->| NOMBRE DE FLOTTEURS.
!| NIT            |-->| NOMBRE DE PAS DE TEMPS
!| NITFLO         |-->| NOMBRE MAXIMAL D'ENREGISTREMENTS DES
!|                |   | POSITIONS SUCCESSIVES DES FLOTTEURS.
!| NOMRBI         |-->| NOM DU FICHIER DE RESULTATS BINAIRE SUP.
!| NRBI           |-->| FICHIER DE RESULTATS BINAIRE SUPPLEMENTAIRE
!|                |   | POUR STOCKER LES TRAJECTOIRES DE FLOTTEURS
!| TIME           |---|
!| TITCAS         |-->| TITRE DU FICHIER CAS
!| XFLOT,YFLOT    |-->| POSITIONS SUCCESSIVES DES FLOTTEURS.
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
      INTEGER, INTENT(IN)             :: I_ORIG,J_ORIG,FLOPRD,NITFLO
      INTEGER, INTENT(IN)             :: NFLOT,NRBI,NIT,MAXVAR
      INTEGER, INTENT(IN)             :: DATE(3),TIME(3)
      INTEGER, INTENT(INOUT)          :: IKLFLO(3*NITFLO*NFLOT)
      INTEGER, INTENT(IN)             :: DEBFLO(NFLOT),FINFLO(NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO*NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO*NFLOT)
      CHARACTER(LEN=72), INTENT(IN)   :: TITCAS,NOMRBI
      CHARACTER(LEN=3), INTENT(IN)    :: BINRES
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IIT,NVAR,ISTAT,IDEB,IFIN,IFLOT,NELFLO,IELFLO
!
      INTEGER BIDI(1)
!
      DOUBLE PRECISION A(2)
!
      LOGICAL DRAPO(26)
!
      CHARACTER*32 TEXTE(26),BID32(26)
      CHARACTER*3  BIDC
!
      DATA DRAPO /.TRUE.,25*.FALSE./
      DATA TEXTE(1) /'MAILLAGE                        '/
!
!-----------------------------------------------------------------------
!
      IF(NOMRBI(1:1).NE.' ') THEN
!
         NELFLO= 0
!
         DO 10 IFLOT=1,NFLOT
            IF (DEBFLO(IFLOT).LT.NIT) THEN
               IDEB=(DEBFLO(IFLOT)-1)/FLOPRD + 1
               IFIN=(MIN0(FINFLO(IFLOT),NIT)-1)/FLOPRD + 1
               DO 20 IIT=IDEB,IFIN
                  NELFLO = NELFLO + 1
                  XFLOT (NELFLO) = XFLOT (NITFLO*(IFLOT-1)+IIT)
                  YFLOT (NELFLO) = YFLOT (NITFLO*(IFLOT-1)+IIT)
                  IKLFLO(NELFLO) = NELFLO + 1
20             CONTINUE
               IKLFLO(NELFLO) = NELFLO - 1
            ENDIF
10       CONTINUE
!
         IF(NELFLO.NE.0) THEN
!
            DO 30 IELFLO=1,NELFLO
               IKLFLO(  NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(2*NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(IELFLO) = IELFLO
30          CONTINUE
!
!           SELAFIN FORMAT
!
            CALL ECRGEO(XFLOT,YFLOT,NELFLO,IKLFLO,
     &                  NRBI,NVAR,TEXTE,BID32,0,TITCAS,DRAPO,26,
     &                  IKLFLO,NELFLO,NELFLO,3,DATE,TIME,
     &                  NCSIZE,NPTIR,MESH%KNOLG%I,
     &                  I3=I_ORIG,I4=J_ORIG)
!
!           WRITES OUT THE TIME STEP
!
            A(1) = 0.D0
            CALL ECRI2(A,BIDI,BIDC,1,'R4',NRBI,BINRES,ISTAT)
!
!           WRITES OUT DUMMY RECORDS IN PLACE OF SIMULATION RESULTS
!           (USES XFLOT BECAUSE NEEDS AN ARRAY OF REALS)
!
            CALL ECRI2 (XFLOT,BIDI,BIDC,NELFLO,'R4',NRBI,BINRES,ISTAT)
!
         ELSE
!
            IF(LNG.EQ.1) WRITE(LU,11) NFLOT
            IF(LNG.EQ.2) WRITE(LU,12) NFLOT
11          FORMAT(1X,'ATTENTION : VOUS AVEZ PREVU',I4,' FLOTTEURS',/,
     &             1X,'MAIS AUCUN N''A ETE LARGUE',/,
     &             1X,'AVANT LE DERNIER PAS DE TEMPS')
12          FORMAT(1X,'ATTENTION : YOU ASKED FOR',I4,' FLOATS',/,
     &             1X,'BUT NONE OF THEM HAS BEEN RELEASED',/,
     &             1X,'SINCE THE LAST TIME STEP')
!
         ENDIF
!
      ELSE
!
         IF(LNG.EQ.1) WRITE(LU,21)
         IF(LNG.EQ.2) WRITE(LU,22)
21       FORMAT(1X,'LE FICHIER DE DERIVES DE FLOTTEURS',/,
     &          1X,'N''A PU ETRE CONSTITUE,',/,
     &          1X,'IL FAUT FOURNIR DANS LE FICHIER DES PARAMETRES',/,
     &          1X,'UN NOM DE FICHIER DE RESULTATS BINAIRE')
22       FORMAT(1X,'THE FILE OF FLOAT DRIFTS COULD NOT BE FIELD,',/,
     &          1X,'YOU SHOULD INCLUDE IN THE STEERING FILE',/,
     &          1X,'A NAME OF BINARY RESULTS FILE')
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END