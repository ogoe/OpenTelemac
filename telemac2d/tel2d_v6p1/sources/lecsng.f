!                    *****************
                     SUBROUTINE LECSNG
!                    *****************
!
     &(NWEIRS,NWRMAX,NPSING,NUMDIG,ZDIG,PHIDIG,IOPTAN,NPSMAX,NPOIN,IFIC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE DATA DEFINING SINGULARITIES
!+                FROM FORMATTED FILE 1.
!
!history  V. GUINOT (LHF)
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
!| IFIC           |-->| LOGICAL UNIT OF FORMATED DATA FILE 1
!| IOPTAN         |<--| OPTION FOR TANGENTIAL VELOCITIES
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!|                |   | ZDIG, NUMDIG AND PHIDIG ARE ALLOCATED ASSUMING:
!|                |   | NPOIN .GE. NWEIRS * NPSMAX.
!| NPSING         |<--| NUMBER OF POINTS FOR ONE SIDE OF A SINGULARITY
!| NPSMAX         |-->| MAXIMUM FOR NPSING
!| NWEIRS         |-->| NUMBER OF LINEAR SINGULARITIES (WEIRS, ETC.)
!| NWRMAX         |-->| MAXIMUM FOR NWEIRS
!| PHIDIG         |<--| DISCHARGE COEFFICIENT FOR POINTS ON WEIRS
!| ZDIG           |<--| ELEVATION OF POINTS ON WEIRS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NWRMAX,NPOIN,IFIC,NWEIRS
      INTEGER, INTENT(INOUT) :: NPSMAX,IOPTAN
!                                                              NPSMAX
      INTEGER, INTENT(INOUT) :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,*     )
!                                                    NPSMAX
      DOUBLE PRECISION, INTENT(INOUT) :: ZDIG(NWEIRS,*     )
!                                                      NPSMAX
      DOUBLE PRECISION, INTENT(INOUT) :: PHIDIG(NWEIRS,*     )
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I,NNWEIRS
!
!-----------------------------------------------------------------------
!
      NNWEIRS=NWEIRS
!
      READ(IFIC,*,END=900)
      READ(IFIC,*,ERR=998) N,IOPTAN
!
!     CHECKS SIZES
!
      IF(N.GT.NWRMAX) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSNG : NOMBRE DE SEUILS : ',N
          WRITE(LU,*) '         TROP GRAND'
          WRITE(LU,*) '         LE MAXIMUM EST :',NWRMAX
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSNG : NUMBER OF WEIRS:',N
          WRITE(LU,*) '         EXCEEDIND THE MAXIMUM OF: ',NWRMAX
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COHERENCE WITH THE STEERING FILE
!
      IF(N.NE.NWEIRS) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSNG : NOMBRE DE SEUILS : ',N
          WRITE(LU,*) '         DIFFERENT DE LA VALEUR DONNEE DANS LE'
          WRITE(LU,*) '         FICHIER DES PARAMETRES :',NWEIRS
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSNG : NUMBER OF WEIRS:',N
          WRITE(LU,*) '         DIFFERENT FROM THE ONE GIVEN IN THE'
          WRITE(LU,*) '         PARAMETER FILE: ',NWEIRS
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'LECSNG : NOMBRE DE DIGUES :',NWEIRS
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'LECSNG : NUMBER OF WEIRS :',NWEIRS
      ENDIF
!
      NPSMAX = 0
!
      DO 10 N=1,NWEIRS
        READ(IFIC,*,END=900)
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=997) NPSING(N)
        NPSMAX=MAX(NPSING(N),NPSMAX)
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=996) (NUMDIG(1,N,I),I=1,NPSING(N))
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=994) (NUMDIG(2,N,I),I=1,NPSING(N))
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=992) (ZDIG(N,I),I=1,NPSING(N))
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=991) (PHIDIG(N,I),I=1,NPSING(N))
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
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         AT LINE 2'
      ENDIF
      GO TO 2000
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         NOMBRE DE POINTS ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         THE NUMBER OF POINTS CANNOT BE READ'
      ENDIF
      GO TO 2000
!
996   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         NUMDIGS DES POINTS ILLISIBLE'
        WRITE(LU,*) '         POUR LE COTE 1'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         THE NUMBER OF THE POINTS CANNOT BE READ'
        WRITE(LU,*) '         FOR SIDE NUMBER 1'
      ENDIF
      GO TO 2000
!
994   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         NUMDIGS DES POINTS ILLISIBLE'
        WRITE(LU,*) '         POUR LE COTE 2'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         THE NUMBER OF THE POINTS CANNOT BE READ'
        WRITE(LU,*) '         FOR SIDE NUMBER 2'
      ENDIF
      GO TO 2000
!
992   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         COTES SUR LA DIGUE ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         ELEVATIONS ON THE WEIR CANNOT BE READ'
      ENDIF
      GO TO 1000
!
991   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         COEFFICIENTS DE DEBIT ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         DISCHARGE COEFFICIENTS CANNOT BE READ'
      ENDIF
      GO TO 2000
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES FORMATE 1'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         FORMATTED DATA FILE 1'
        WRITE(LU,*) '         UNEXPECTED END OF FILE'
      ENDIF
!
2000  CONTINUE
!
      NNWEIRS = 0
!
1000  CONTINUE
!
      IF(NNWEIRS.EQ.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSNG : ERREUR DE LECTURE'
          WRITE(LU,*)'         AUCUNE SINGULARITE NE SERA'
          WRITE(LU,*)'         PRISE EN COMPTE.'
          WRITE(LU,*)
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSNG : READ ERROR'
          WRITE(LU,*)'         NO SINGULARITY WILL BE TAKEN'
          WRITE(LU,*)'         INTO ACCOUNT'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CHECKS SIZE OF ARRAYS NUMDIG, PHIDIG, ZDIG
!
      IF(NPOIN.LT.NWEIRS*NPSMAX) THEN
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSNG : TROP DE POINTS SUR LES SINGULARITES'
          WRITE(LU,*)'         UN CHANGEMENT DES POINTEURS EST'
          WRITE(LU,*)'         NECESSAIRE'
          WRITE(LU,*)
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSNG : TOO MANY POINTS IN THE SINGULARITIES'
          WRITE(LU,*)'         IT IS NECESSARY TO CHANGE THE MEMORY'
          WRITE(LU,*)'         POINTERS'
          WRITE(LU,*)
        ENDIF
!
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
