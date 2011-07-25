!                    *****************
                     SUBROUTINE LECSIP
!                    *****************
!
     &(RELAXS,NSIPH,ENTSIP,SORSIP,SECSCE,
     & ALTSCE,CSSCE,CESCE,DELSCE,ANGSCE,LSCE,MAXSCE,IFIC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE DATA FOR SIPHONS.
!
!history  V. GUINOT   (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        03/10/1996
!+        V5P2
!+   MODIFIED
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
!| ALTSCE         |<--| ELEVATION OF ENTRY AND EXIT OF PIPES
!| ANGSCE         |<--| ANGLE OF PIPES WITH AXIS OX.
!| CESCE          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CSSCE          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| DELSCE         |<--| ANGLE OF PIPES WITH VERTICAL
!| ENTSIP         |<--| INDICES OF ENTRY OF PIPE IN POINT SOURCES NUMBERING
!| IFIC           |-->| LOGICAL UNIT OF FORMATTED DATA FILE 1
!| LSCE           |<--| LINEAR HEAD LOSS OF PIPE
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NSIPH          |<--| NUMBER OF CULVERTS
!| RELAXS         |<--| RELAXATION COEFFICIENT.
!| SECSCE         |<--| CROSS SECTION OF CULVERTS (NUMBERED AS SOURCES)
!| SORSIP         |<--| INDICES OF PIPES EXITS IN SOURCES NUMBERING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: MAXSCE,IFIC
      INTEGER, INTENT(INOUT) :: ENTSIP(*),SORSIP(*),NSIPH
      DOUBLE PRECISION, INTENT(INOUT) :: RELAXS
      DOUBLE PRECISION, INTENT(INOUT) :: SECSCE(*),ALTSCE(*),CSSCE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DELSCE(*),ANGSCE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: CESCE(*),LSCE(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
!
      DOUBLE PRECISION DELTA1,DELTA2,S12,ALT1,ALT2
      DOUBLE PRECISION ANG1,ANG2,CS1,CS2,CE1,CE2,L12
!
      DOUBLE PRECISION PI
      PARAMETER(PI=3.141592653589D0)
!
!-----------------------------------------------------------------------
!
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=998) RELAXS,N
      READ(IFIC,*,END=900)
!
!     CHECKS SIZES
!
      IF(N.GT.MAXSCE/2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSIP : NOMBRE DE SIPHONS : ',N
          WRITE(LU,*) '         TROP GRAND'
          WRITE(LU,*) '         LE MAXIMUM EST :',MAXSCE/2
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSIP : NUMBER OF CULVERTS:',N
          WRITE(LU,*) '         EXCEEDIND THE MAXIMUM OF: ',MAXSCE/2
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COHERENCE WITH THE STEERING FILE
!
      IF(N.NE.NSIPH) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSIP : NOMBRE DE SIPHONS : ',N
          WRITE(LU,*) '         DIFFERENT DE LA VALEUR DONNEE DANS LE'
          WRITE(LU,*) '         FICHIER DES PARAMETRES :',NSIPH
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSIP : NUMBER OF CULVERTS:',N
          WRITE(LU,*) '         DIFFERENT FROM THE ONE GIVEN IN THE'
          WRITE(LU,*) '         PARAMETER FILE: ',NSIPH
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      DO 10 N=1,NSIPH
        READ(IFIC,*,ERR=997) ENTSIP(N),SORSIP(N),DELTA1,DELTA2,
     &                       CE1,CE2,CS1,CS2,S12,L12,ALT1,ALT2,ANG1,ANG2
        DELSCE(ENTSIP(N)) = DELTA1*PI/180.D0
        DELSCE(SORSIP(N)) = DELTA2*PI/180.D0
        CESCE(ENTSIP(N))  = CE1
        CESCE(SORSIP(N))  = CE2
        CSSCE(ENTSIP(N))  = CS1
        CSSCE(SORSIP(N))  = CS2
        SECSCE(ENTSIP(N)) = S12
        SECSCE(SORSIP(N)) = S12
        LSCE(ENTSIP(N))   = L12
        LSCE(SORSIP(N))   = L12
        ALTSCE(ENTSIP(N)) = ALT1
        ALTSCE(SORSIP(N)) = ALT2
        ANGSCE(ENTSIP(N)) = ANG1*PI/180.D0
        ANGSCE(SORSIP(N)) = ANG2*PI/180.D0
10    CONTINUE
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         AT LINE 2'
      ENDIF
      GO TO 2000
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LE SIPHON ',N
        WRITE(LU,*) '         DONNEES ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR CULVERT NUMBER ',N
        WRITE(LU,*) '         THE DATA CANNOT BE READ'
      ENDIF
      GO TO 2000
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         UNEXPECTED END OF FILE'
      ENDIF
!
2000  CONTINUE
!
      NSIPH = 0
!
1000  CONTINUE
!
      IF(NSIPH.EQ.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSIP : ERREUR DE LECTURE'
          WRITE(LU,*)'         AUCUN SIPHON NE SERA'
          WRITE(LU,*)'         PRIS EN COMPTE.'
          WRITE(LU,*)
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSIP : READ ERROR'
          WRITE(LU,*)'         NO CULVERT WILL BE TAKEN'
          WRITE(LU,*)'         INTO ACCOUNT'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
