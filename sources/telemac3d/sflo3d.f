!                    *****************
                     SUBROUTINE SFLO3D
!                    *****************
!
     &(XFLOT,YFLOT,ZFLOT,IKLFLO,TRAFLO,DEBFLO,FINFLO,NFLOT,NITFLO,
     & FLOPRD,NREBI,LISTIN,TITCAS,BINRES,NOMRBI,NIT,
     & I_ORIG,J_ORIG)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES THE BINARY RESULTS FILE TO SELAFIN FORMAT
!+                WITH INFORMATION ON THE TRAJECTORIES OF THE
!+                FLOATS.
!
!history  J-M JANIN (LNH)
!+        27/11/92
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  JMH
!+        26/08/99
!+
!+   CALL TO FMTSEL REPLACED BY ECRGEO (NOT TESTED)
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
!| BINRES         |-->| TYPE OF BINARY FOR RESULTS FILE
!| DEBFLO         |<->| NUMBERS OF TIME STEPS FOR DROPPING OF EACH DROGUE
!| FINFLO         |<->| NUMBERS OF TIME STEPS OF END OF COMPUTATION OF
!|                |   | DRIFT FOR EACH DROGUE
!| FLOPRD         |-->| NUMBER OF TIME STEPS BETWEEN 2 RECORDINGS
!|                |   | OF SUCCESSIVE POSITIONS OF DROGUES
!| IKLFLO         |<->| FAKE CONNECTIVY TABLE BIDON USED FOR PATHS 
!|                |   | UNDER MESH FORMAT
!| I_ORIG         |-->| COORDINATE OF THE ORIGIN
!| J_ORIG         |-->| COORDINATE OF THE ORIGIN
!| LISTIN         |-->| LISTING PRINTOUT OR NOT
!| NFLOT          |-->| NUMBER OF DROGUES
!| NIT            |-->| NUMBER OF TIME STEPS
!| NITFLO         |-->| MAXIMUM NUMBER OF RECORDINGS OF SUCCESSIVE
!|                |   | POSITIONS OF DROGUES
!| NOMRBI         |-->| NAME OF BINARY RESULTS FILE SUP.
!| NREBI          |-->| EXTRA BINARY RESULT FILE TO STORE
!|                |---| TRAJECTORIES OF DROGUES
!| TITCAS         |-->| TITLE OF TEST CASE
!| TRAFLO         |<->| WORK ARRAY USED IN FMTSEL
!| XFLOT          |<->| SUCCESSIVE X POSITIONS OF DROGUES
!| YFLOT          |<->| SUCCESSIVE Y POSITIONS OF DROGUES
!| ZFLOT          |<->| SUCCESSIVE Z POSITIONS OF DROGUES
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
      INTEGER, INTENT(IN) :: NFLOT,NIT,NITFLO,NREBI,FLOPRD
      INTEGER, INTENT(IN) :: I_ORIG,J_ORIG
!
      INTEGER, INTENT(INOUT) :: IKLFLO(3*NITFLO*NFLOT)
      INTEGER, INTENT(INOUT) :: TRAFLO(3*NITFLO*NFLOT)
      INTEGER, INTENT(INOUT) :: DEBFLO(NFLOT), FINFLO(NFLOT)
!
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO*NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO*NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: ZFLOT(NITFLO*NFLOT)
!
      CHARACTER(LEN=72), INTENT(IN) :: TITCAS, NOMRBI
      CHARACTER(LEN=3),  INTENT(IN) :: BINRES
      LOGICAL, INTENT(IN) :: LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=32) TEXTE(26)
      LOGICAL DRAPO(26), DEPAR, SUIT, ECRI
!
      INTEGER IIT,NVAR,ISTAT,BIDI(1),DATE(3),TIME(3)
      INTEGER IDEB,IFIN,IFLOT,NELFLO,IELFLO
      DOUBLE PRECISION A(2)
      CHARACTER(LEN=32) BIDC2,BIDC(1)
!
      DATA DRAPO /.TRUE.,25*.FALSE./
      DATA TEXTE(1) /'COTE_RELATIVE                   '/
!
!-----------------------------------------------------------------------
!
      IF(NOMRBI(1:1).NE.' ') THEN
!
         DEPAR = .FALSE.
         SUIT  = .FALSE.
         ECRI  = .TRUE.
         NELFLO= 0
!
         DO IFLOT=1,NFLOT
           IF(DEBFLO(IFLOT).LT.NIT) THEN
             IDEB=(DEBFLO(IFLOT)-1)/FLOPRD + 1
             IFIN=(MIN0(FINFLO(IFLOT),NIT)-1)/FLOPRD + 1
             DO IIT=IDEB,IFIN
               NELFLO = NELFLO + 1
               XFLOT (NELFLO) = XFLOT (NITFLO*(IFLOT-1)+IIT)
               YFLOT (NELFLO) = YFLOT (NITFLO*(IFLOT-1)+IIT)
               ZFLOT (NELFLO) = ZFLOT (NITFLO*(IFLOT-1)+IIT)
               IKLFLO(NELFLO) = NELFLO + 1
             ENDDO
             IKLFLO(NELFLO) = NELFLO - 1
           ENDIF
         ENDDO
!
         IF(NELFLO.NE.0) THEN
!
            DO IELFLO=1,NELFLO
               IKLFLO(  NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(2*NELFLO+IELFLO) = IKLFLO(IELFLO)
               IKLFLO(IELFLO) = IELFLO
            ENDDO
!
!  STANDARD SELAFIN
!
            DATE(1) = 0
            DATE(2) = 0
            DATE(3) = 0
            TIME(1) = 0
            TIME(2) = 0
            TIME(3) = 0
!
!   NOTE JMH: THERE IS CONFUSION BETWEEN NBOR AND IKLE, BOTH GIVEN AS
!   IKLFLO. NBOR IS ACTUALLY ONLY USED TO BUILD IPOBO FOR SELAFIN FORMAT,
!   NOT USED BY RUBENS. IE MUST WORK BY LUCK.
!   AND NELFLO,NELFLO REPRESENTS NELEM,NPTFR !!!
            CALL ECRGEO(XFLOT,YFLOT,NELFLO,IKLFLO,
     &                  NREBI,NVAR,TEXTE,BIDC,0,
     &                  TITCAS,DRAPO,26,IKLFLO,NELFLO,
     &                  NELFLO,3,DATE,TIME,
     &                  0     ,0     ,BIDI,
!    *                  NCSIZE,NPTIR,KNOLG)  PARALLELISM NOT IMPLEMENTED
     &                  I3=I_ORIG,I4=J_ORIG)
!
! WRITES THE TIME
!
            A(1) = 0.D0
            CALL ECRI2(A,BIDI,BIDC2,1,'R4',NREBI,BINRES,ISTAT)
!
! WRITES THE ELEVATION
!
            CALL ECRI2(ZFLOT,BIDI,BIDC2,NELFLO,'R4',NREBI,BINRES,ISTAT)
!
         ELSE
!
            IF (LNG.EQ.1) WRITE(LU,11) NFLOT
            IF (LNG.EQ.2) WRITE(LU,12) NFLOT
!
         ENDIF
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,21)
         IF (LNG.EQ.2) WRITE(LU,22)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(' ATTENTION : VOUS AVEZ PREVU',I4,' FLOTTEURS',/,
     &       ' MAIS AUCUN N''A ETE LARGUE',/,
     &       ' AVANT LE DERNIER PAS DE TEMPS')
12    FORMAT(' ATTENTION : YOU ASKED FOR',I4,' FLOATS',/,
     &       ' BUT NONE OF THEM HAS BEEN RELEASED',/,
     &       ' SINCE THE LAST TIME STEP')
!
21    FORMAT(' LE FICHIER DE DERIVES DE FLOTTEURS N''A PU ETRE CREE',/,
     &       ' IL FAUT FOURNIR DANS LE FICHIER DES PARAMETRES',/,
     &       ' UN NOM DE FICHIER DE RESULTATS BINAIRE')
22    FORMAT(' THE FILE OF FLOAT DRIFTS COULD NOT BE FIELD,',/,
     &       ' YOU SHOULD INCLUDE IN THE STEERING FILE',/,
     &       ' A NAME OF BINARY RESULTS FILE')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
