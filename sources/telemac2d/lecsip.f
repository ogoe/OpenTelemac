!                    *****************
                     SUBROUTINE LECSIP
!                    *****************
!
     &(RELAXS,NSIPH,ENTSIP,SORSIP,SECSIP,
     & ALTSIP,CSSIP,CESIP,DELSIP,ANGSIP,LSIP,IFIC,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    READS THE DATA FOR SIPHONS.
!
!history  V. GUINOT   (LHF)
!+        19/04/1996
!+        V5P2
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
!history  C.COULET (ARTELIA)
!+        10/05/2012
!+        V6P2
!+   Modification of culvert file
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        25/01/2013
!+        V6P3
!+   Bug corrected in parallel.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALTSIP         |<--| ELEVATION OF ENTRY AND EXIT OF PIPES
!| ANGSIP         |<--| ANGLE OF PIPES WITH AXIS OX.
!| CESIP          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CSSIP          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| DELSIP         |<--| ANGLE OF PIPES WITH VERTICAL
!| ENTSIP         |<--| INDICES OF ENTRY OF PIPE IN GLOBAL NUMBERING
!| IFIC           |-->| LOGICAL UNIT OF FORMATTED DATA FILE 1
!| LSIP           |<--| LINEAR HEAD LOSS OF PIPE
!| NSIPH          |<--| NUMBER OF CULVERTS
!| RELAXS         |<--| RELAXATION COEFFICIENT.
!| SECSIP         |<--| CROSS SECTION OF CULVERTS
!| SORSIP         |<--| INDICES OF PIPES EXITS IN GLOBAL MESH NUMBERING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: IFIC,NSIPH
      INTEGER, INTENT(INOUT)          :: ENTSIP(NSIPH),SORSIP(NSIPH)
      DOUBLE PRECISION, INTENT(INOUT) :: RELAXS
      DOUBLE PRECISION, INTENT(INOUT) :: SECSIP(NSIPH),ALTSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(INOUT) :: DELSIP(NSIPH,2),ANGSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(INOUT) :: CESIP(NSIPH,2),CSSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(INOUT) :: LSIP(NSIPH)
      TYPE(BIEF_MESH), INTENT(IN)     :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N
!
      DOUBLE PRECISION DELTA1,DELTA2,ALT1,ALT2
      DOUBLE PRECISION ANG1,ANG2,CS1,CS2,CE1,CE2
!
      DOUBLE PRECISION PI
!
!-----------------------------------------------------------------------
!
!> SEB @ HRW: ALGORITHMIC DIFFERENTIATION
      PI = 4.D0 * ATAN( 1.D0 )
!      PARAMETER(PI=3.141592653589D0)
!< SEB @ HRW
!
!-----------------------------------------------------------------------
!
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=998) RELAXS
      READ(IFIC,*,END=900)
!
      DO N=1,NSIPH
        READ(IFIC,*,ERR=997) ENTSIP(N),SORSIP(N),
     &                       DELTA1,DELTA2,CE1,CE2,
     &                       CS1,CS2,SECSIP(N),LSIP(N),
     &                       ALT1,ALT2,ANG1,ANG2
        DELSIP(N,1) = DELTA1*PI/180.D0
        DELSIP(N,2) = DELTA2*PI/180.D0
        CESIP(N,1)  = CE1
        CESIP(N,2)  = CE2
        CSSIP(N,1)  = CS1
        CSSIP(N,2)  = CS2
        ALTSIP(N,1) = ALT1
        ALTSIP(N,2) = ALT2
        ANGSIP(N,1) = ANG1*PI/180.D0
        ANGSIP(N,2) = ANG2*PI/180.D0
      ENDDO ! N
!
!     IN // THE POINTS ARE GIVEN THEIR LOCAL NUMBER
!           OR 0 IF NOT IN THE SUB-DOMAIN
!
      IF(NCSIZE.GT.1) THEN
        DO N=1,NSIPH
          ENTSIP(N)=GLOBAL_TO_LOCAL_POINT(ENTSIP(N),MESH)
          SORSIP(N)=GLOBAL_TO_LOCAL_POINT(SORSIP(N),MESH)
        ENDDO
      ENDIF
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
        WRITE(LU,*) '         FICHIER DE DONNEES DES SIPHONS'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         CULVERT DATA FILE'
        WRITE(LU,*) '         AT LINE 2'
      ENDIF
      CALL PLANTE(1)
      STOP
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SIPHONS'
        WRITE(LU,*) '         POUR LE SIPHON ',N
        WRITE(LU,*) '         DONNEES ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         CULVERT DATA FILE'
        WRITE(LU,*) '         FOR CULVERT NUMBER ',N
        WRITE(LU,*) '         THE DATA CANNOT BE READ'
      ENDIF
      CALL PLANTE(1)
      STOP
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSIP : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SIPHONS'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSIP : READ ERROR ON THE'
        WRITE(LU,*) '         CULVERT DATA FILE'
        WRITE(LU,*) '         UNEXPECTED END OF FILE'
      ENDIF
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
