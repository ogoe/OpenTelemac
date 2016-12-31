!                    ************************
                     SUBROUTINE FRICTION_READ
!                    ************************
!
     &(NCOF,NZONMX,ITURB,LISRUG,LINDNER,NOMCOF,NZONES,FRTAB)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    FRICTION FILE READ.
!
!history  F. HUVELIN
!+        20/04/2004
!+        V5P4
!+   First version
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
!history  J-M HERVOUET (LNHE)
!+        11/05/2015
!+        V7P1
!+   Data on lateral friction are now asked as soon as LISRUG=2
!+   and not only in case of k-epsilon model.
!
!history  J-M HERVOUET (LNHE)
!+        29/09/2015
!+        V7P1
!+   Bug corrected when the number of zones is the maximum.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FRTAB          |-->| FRICTION_OBJ STRUCTURE WITH DATA ON FRICTION
!| ITURB          |-->| TURBULENCE MODEL
!| LINDNER        |-->| IF YES, THERE IS NON-SUBMERGED VEGETATION FRICTION
!| LISRUG         |-->| TURBULENCE REGIME (1: SMOOTH 2: ROUGH)
!| NCOF           |-->| LOGICAL UNIT OF FRICTION FILE
!| NOMCOF         |-->| NAME OF FRICTION FILE
!| NZONES         |<--| NUMBER OF FRICTION ZONES
!| NZONMX         |-->| MAXIMUM NUMBER OF FRICTION ZONES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE FRICTION_DEF
      USE INTERFACE_TELEMAC2D, EX_FRICTION_READ => FRICTION_READ
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,            INTENT(IN)    :: NCOF, NZONMX
      INTEGER,            INTENT(IN)    :: ITURB, LISRUG
      LOGICAL,            INTENT(IN)    :: LINDNER
      CHARACTER(LEN=144), INTENT(IN)    :: NOMCOF
      INTEGER,            INTENT(OUT)   :: NZONES
      TYPE(FRICTION_OBJ), INTENT(INOUT) :: FRTAB
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER           :: I, J, LOOP, NLOOP, N, IZ1, IZ2
      INTEGER           :: IZONE, TYP, LINE
      DOUBLE PRECISION  :: R1, R2
      CHARACTER(LEN=4)  :: LAW
      CHARACTER(LEN=20) :: CHAINE(10)
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      ! CHECK THAT THERE IS A FRICTION FILE
      ! -----------------------------------
      IF(NOMCOF(1:1) == ' ') THEN
        IF(LNG == 1) WRITE(LU,1)
        IF(LNG == 2) WRITE(LU,2)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      REWIND NCOF
!
      DO I=1,NZONMX
        DO J = 1, 2
          FRTAB%ADR(I)%P%GNUMB(J) = 0
          FRTAB%ADR(I)%P%RTYPE(J) = 0
          FRTAB%ADR(I)%P%RCOEF(J) = 0.D0
          FRTAB%ADR(I)%P%NDEF (J) = 0.D0
        ENDDO
        FRTAB%ADR(I)%P%DP          = 0.D0
        FRTAB%ADR(I)%P%SP          = 0.D0
      ENDDO
!
!     ROUGH REGIME  : READ PARAMETERS FOR 2 LAWS (BOTTOM AND BOUNDARY CONDITIONS)
!     SMOOTH REGIME : READ PARAMETERS FOR 1 LAW  (BOTTOM)
!
      IF(LISRUG.EQ.2) THEN
        NLOOP=2
      ELSE
        NLOOP=1
      ENDIF
!
!     LISTING
!
      WRITE(LU,3)
      WRITE(LU,4)
!
!     READ DATA FOR EACH ZONE
!
      IZONE = 0
      LINE = 0
1000  CONTINUE
!
!     FIND THE NEXT ZONE INDEX AND LAW
!
      CALL FRICTION_SCAN(NCOF,NOMCOF,TYP,LINE)
!
!     END OF FILE => EXIT
      IF(TYP == 3) THEN
        NZONES = IZONE
        GO TO 1001
      ELSE
        IZONE = IZONE + 1
        IF(IZONE.GT.NZONMX) THEN
          IF (LNG == 1) WRITE(LU,5)
          IF (LNG == 2) WRITE(LU,6)
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     READ AND SAVE PARAMETERS OF THE ZONE
      IF (TYP == 1) N = 1
      IF (TYP == 2) N = 4
!
      DO LOOP = 1, NLOOP
!
        IF(LOOP == 2) BACKSPACE(NCOF)
!       READ THE NAME OF THE LAW AND NUMBERING OF THE ZONE
        CHAINE(1) = ' '
        IF(TYP.EQ.1) THEN
          READ(NCOF,*,END=999,ERR=998) IZ1,(CHAINE(J),J=2,N),LAW
          IZ2 = IZ1
        ELSEIF(TYP.EQ.2) THEN
           READ(NCOF,*,END=999,ERR=998) CHAINE(1),IZ1,CHAINE(3),IZ2,
     &                                   (CHAINE(J),J=5,N),LAW
        ENDIF
!
!       LOCAL-GLOBAL NUMBER OF THE ZONE
!
        IF (LOOP == 1) FRTAB%ADR(IZONE)%P%GNUMB(1) = IZ1
        IF (LOOP == 1) FRTAB%ADR(IZONE)%P%GNUMB(2) = IZ2
!
        BACKSPACE(NCOF)
        CALL MAJUS(LAW)
!
!       FIND THE LAW AND THE NUMBER OF PARAMETERS TO READ
!
        SELECT CASE (LAW)
!
        CASE('NOFR')
          READ(NCOF,*,END=999,ERR=900) (CHAINE(J),J=1,N),LAW
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 0
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = 0.D0
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
          N = N + 1
        CASE('HAAL')
          READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 1
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
!
!         THE BOUNDARY COEFFICIENT COEF MUST BE THE SAME AS THE BOTTOM
!
          IF( ( (N.GT.1.AND.TYP.EQ.1).OR.(N.GT.4.AND.TYP.EQ.2) )
     &        .AND.
     &     (FRTAB%ADR(IZONE)%P%RCOEF(1).NE.FRTAB%ADR(IZONE)%P%RCOEF(2))
     &      ) THEN
            IF (LNG==1) WRITE(LU,15) NOMCOF,IZONE,
     &                               FRTAB%ADR(IZONE)%P%RCOEF(1)
            IF (LNG==2) WRITE(LU,16) NOMCOF,IZONE,
     &                               FRTAB%ADR(IZONE)%P%RCOEF(1)
            CALL PLANTE(1)
            STOP
          ENDIF
          N = N + 2
        CASE('CHEZ')
          READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 2
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
          N = N + 2
        CASE('STRI')
          READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 3
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
          N = N + 2
        CASE('MANN')
          READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 4
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
          N = N + 2
        CASE('NIKU')
          READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 5
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
          N = N + 2
        CASE('LOGW')
          IF (((N == 1).AND.(TYP == 1))  .OR.
     &        ((N == 4).AND.(TYP == 2))) THEN
            IF (LNG == 1) WRITE(LU,7) NOMCOF, IZONE
            IF (LNG == 2) WRITE(LU,8) NOMCOF, IZONE
            CALL PLANTE(1)
            STOP
          ENDIF
          READ(NCOF,*,END=999,ERR=901) (CHAINE(J),J=1,N),LAW,R1
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 6
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = 0.D0
          N = N + 2
        CASE('COWH')
          READ(NCOF,*,END=999,ERR=907) (CHAINE(J),J=1,N),LAW,R1,R2
          FRTAB%ADR(IZONE)%P%RTYPE(LOOP) = 7
          FRTAB%ADR(IZONE)%P%RCOEF(LOOP) = R1
          FRTAB%ADR(IZONE)%P%NDEF (LOOP) = R2
          N = N + 3
        CASE DEFAULT
          IF (LNG==1) WRITE(LU, 9) LINE, IZ1, IZ2, LAW
          IF (LNG==2) WRITE(LU,10) LINE, IZ1, IZ2, LAW
          CALL PLANTE(1)
          STOP
        END SELECT
      ENDDO
!
!       READ NON-SUBMERGED COEFFICIENT IF NEEDED
!
      IF(LINDNER) THEN
        BACKSPACE(NCOF)
        READ(NCOF,*,END=999,ERR=888) (CHAINE(J),J=1,N),R1,R2
        FRTAB%ADR(IZONE)%P%DP = R1
        FRTAB%ADR(IZONE)%P%SP = R2
      ELSE
        FRTAB%ADR(IZONE)%P%DP = 0.D0
        FRTAB%ADR(IZONE)%P%SP = 0.D0
      ENDIF
      WRITE(LU,11) FRTAB%ADR(IZONE)%P%GNUMB(1),
     &             FRTAB%ADR(IZONE)%P%GNUMB(2),
     &             FRTAB%ADR(IZONE)%P%RTYPE(1),
     &             FRTAB%ADR(IZONE)%P%RCOEF(1),
     &             FRTAB%ADR(IZONE)%P%NDEF (1),
     &             FRTAB%ADR(IZONE)%P%RTYPE(2),
     &             FRTAB%ADR(IZONE)%P%RCOEF(2),
     &             FRTAB%ADR(IZONE)%P%NDEF (2),
     &             FRTAB%ADR(IZONE)%P%DP,
     &             FRTAB%ADR(IZONE)%P%SP
!
      GO TO 1000
1001  CONTINUE
!
!     END
!
      WRITE(LU,3)
!
      IF (LNG == 1) WRITE(LU,13) NZONES
      IF (LNG == 2) WRITE(LU,14) NZONES
!
      GOTO 997
!
      ! ============ !
      ! ERROR FORMAT !
      ! ============ !
!
1     FORMAT('PAS DE FICHIER DE DONNEES POUR LE FROTTEMENT')
2     FORMAT('NO FRICTION DATA FILE')
3     FORMAT('-------------------------------------------------------'
     &     , '------------------------------------------------------')
4     FORMAT('                      BOTTOM                         '
     &     , 'BOUNDARY CONDITION              NON-SUBMERGED VEGETATION'
     &     ,/'NO                    LAW   RCOEF        NDEF        '
     &     , 'LAW   RCOEF        NDEF         DP           SP')
5     FORMAT('NOMBRE DE ZONES DE FROTTEMENT DEFINI TROP NOMBREUSES'
     &     ,/'AUGMENTER LE NOMBRE DE ZONES MAXIMALES AVEC LE MOT-CLE :'
     &     ,/'NOMBRE MAXIMALE DE ZONES POUR LE FROTTEMENT')
6     FORMAT('TOO MANY NUMBER OF FRICTION ZONES DEFINED'
     &     ,/'INCREASED THE NUMBER OF MAXIMAL ZONES WITH THE KEYWORD :'
     &     ,/'MAXIMUM NUMBER OF ZONES FOR THE FRICTION')
7     FORMAT('FICHIER DE DONNEES POUR LE FROTTEMENT : ',A144
     &      ,/'ZONE : ',I9
     &      ,/'LA LOI LOG NE PEUT ETRE UTILISEE SUR LE FOND')
8     FORMAT('FRICTION DATA FILE : ',A144
     &      ,/'ZONE : ',I9
     &      ,/'LOG LAW CAN''T BE USED FOR THE BOTTOM')
9     FORMAT('FICHIER DE DONNEES POUR LE FROTTEMENT'
     &     ,/'ERREUR DE LECTURE LIGNE : ',I10
     &     ,/'ZONE DE ',I10,' A ',I10
     &     ,/'LOI ',A4)
10    FORMAT('FRICTION DATA FILE'
     &     ,/'READ ERROR LINE',I10
     &     ,/'ZONE FROM ',I10,' TO ',I10
     &     ,/'LAW ',A4)
11    FORMAT(2(1X,I9),1X,I4,2(1X,E12.4),1X,I4,4(1X,E12.4))
!
13    FORMAT(I5,' TYPES DE ZONES DEFINIES')
14    FORMAT(I5,' ZONES TYPE SPECIFICATIONS')
15    FORMAT('FICHIER DE DONNEES POUR LE FROTTEMENT : ',A144
     &      ,/'ZONE : ',I9
     &      ,/'LE COEFFICIENT DE FROTTEMENT DE LA LOI DE HAALAND POUR'
     &      ,/'LES CONDITIONS LIMITES DOIT ETER EGALE A CELUI DU FOND :'
     &      , E12.4)
16    FORMAT('FRICTION DATA FILE : ',A144
     &      ,/'ZONE : ',I9
     &      ,/'FRICTION COEFFICIENT OF HAALAND LAW FOR'
     &      ,/'BOUNDARY CONDITION MUST BE THE SAME AS THE BOTTOM :'
     &      , E12.4)
!
      ! END OF FILE
      ! -----------
999   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*) 'FICHIER DE DONNEES POUR LE FROTTEMENT : ',NOMCOF
        WRITE(LU,*) 'FIN DE FICHIER ANORMALE'
        WRITE(LU,*) 'VERIFIER QUE TOUTES LES VALEURS SONT ENTREES'
      ENDIF
      IF (LNG.EQ.2) THEN
        WRITE(LU,*) 'FRICTION DATA FILE : ',NOMCOF
        WRITE(LU,*) 'ABNORMAL END OF FILE'
        WRITE(LU,*) 'CHECK ALL VALUE HAVE BEEN WRITTEN'
      ENDIF
      CALL PLANTE(1)
      STOP
!
      ! INDEX AND FIRST LAW OF THE ZONE
      ! -------------------------------
998   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'FICHIER DE DONNEES POUR LE FROTTEMENT : ',NOMCOF
        WRITE(LU,*)'ERREUR DE LECTURE ZONE : ',CHAINE(1)
      ENDIF
      IF (LNG.EQ.2) THEN
        WRITE(LU,*) 'FRICTION DATA FILE : ',NOMCOF
        WRITE(LU,*) 'READ ERROR ZONE : ',CHAINE(1)
      ENDIF
      CALL PLANTE(0)
      STOP
      ! NON-SUBMERGED VEGETATION PARAMETER
      ! ----------------------------------
888   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*) 'FICHIER DE DONNEES POUR LE FROTTEMENT'
        WRITE(LU,*)'ERREUR DE LECTURE POUR DP ET SP, ZONE : ',CHAINE(1)
      ENDIF
      IF (LNG.EQ.2) THEN
        WRITE(LU,*) 'FRICTION DATA FILE'
        WRITE(LU,*) 'READ ERROR FOR DP AND SP, ZONE : ',CHAINE(1)
      ENDIF
      CALL PLANTE(0)
      STOP
!
      ! NO FRICTION LAW
      ! ---------------
900   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'FICHIER DE DONNEES POUR LE FROTTEMENT'
        WRITE(LU,*) 'ERREUR DE LECTURE ZONE  : ',CHAINE(1)
        IF ((ITURB==3).AND.(LISRUG==2)) THEN
          IF (LOOP==1) WRITE(LU,*) 'POUR LA 1ERE LOI DEFINIR '//
     &                             'SEULEMENT LE NOM DE LA LOI : NOFR'
          IF (LOOP==2) WRITE(LU,*) 'POUR LA 2NDE LOI DEFINIR'//
     &                             'SEULEMENT LE NOM DE LA LOI : NOFR'
        ELSE
          WRITE(LU,*) 'DEFINIR SEULEMENT LE NOM DE LA LOI'
        ENDIF
      ENDIF
      IF (LNG.EQ.2) THEN
        WRITE(LU,*) 'FRICTION DATA FILE'
        WRITE(LU,*) 'READ ERROR ZONE : ',CHAINE(1)
        IF ((ITURB==3).AND.(LISRUG==2)) THEN
          IF (LOOP==1) WRITE(LU,*) 'FOR THE 1ST LAW DEFINE '//
     &                             'ONLY THE NAME OF THE LAW : NOFR'
          IF (LOOP==2) WRITE(LU,*) 'FOR THE 2ND LAW DEFINE '//
     &                             'ONLY THE NAME OF THE LAW : NOFR'
        ELSE
           WRITE(LU,*) 'DEFINE ONLY THE NAME OF THE LAW : NOFR'
        ENDIF
      ENDIF
      CALL PLANTE(1)
      STOP
!
      ! HAALAND-CHEZY-STRICKLER-MANNING-NIKURADSE-LOG WALL LAWS
      ! -------------------------------------------------------
901   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'FICHIER DE DONNEES POUR LE FROTTEMENT : ',NOMCOF
        WRITE(LU,*) 'ERREUR DE LECTURE ZONE: ',CHAINE(1)
        IF ((ITURB==3).AND.(LISRUG==2)) THEN
          IF (LOOP==1) WRITE(LU,*) 'POUR LA 1ERE LOI DEFINIR ' //
     &                             'LE NOM DE LA LOI : ',LAW,' ET'//
     &                             ' LE COEFFICIENT DE FROTTEMENT'
          IF (LOOP==1) WRITE(LU,*) 'POUR LA 2NDE LOI DEFINIR ' //
     &                             'LE NOM DE LA LOI : ',LAW,' ET'//
     &                             ' LE COEFFICIENT DE FROTTEMENT'
        ELSE
          WRITE(LU,*) 'DEFINIR LE NOM DE LA LOI : ',LAW,' ET'//
     &                ' LE COEFFICIENT DE FROTTEMENT'
        ENDIF
      ENDIF
!
      IF (LNG.EQ.2) THEN
        WRITE(LU,*) 'FRICTION DATA FILE : ',NOMCOF
        WRITE(LU,*) 'READ ERROR ZONE : ',CHAINE(1)
        IF ((ITURB==3).AND.(LISRUG==2)) THEN
          IF (LOOP==1) WRITE(LU,*) 'FOR THE 1ST LAW DEFINE '//
     &                             'THE NAME OF THE LAW : ',LAW//
     &                             ' AND THE FRICTION COEFFICIENT'
          IF (LOOP==2) WRITE(LU,*) 'FOR THE 2ND LAW DEFINE '//
     &                             'THE NAME OF THE LAW : ',LAW//
     &                             ' AND THE FRICTION COEFFICIENT'
        ELSE
          WRITE(LU,*) 'DEFINE THE NAME OF THE LAW : ',LAW//
     &                ' AND THE FRICTION COEFFICIENT'
        ENDIF
      ENDIF
      CALL PLANTE(1)
      STOP
!
      ! COLEBROOK WHITE LAW
      ! -------------------
907   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'FICHIER DE DONNEES POUR LE FROTTEMENT : ',NOMCOF
        WRITE(LU,*) 'ERREUR DE LECTURE ZONE : ',CHAINE(1)
        IF ((ITURB==3).AND.(LISRUG==2)) THEN
          IF (LOOP==1) WRITE(LU,*) 'POUR LA 1ERE LOI DEFINIR ' //
     &                             'LE NOM DE LA LOI : ',LAW,' ET'//
     &                             ' LE COEFFICIENT DE FROTTEMENT'//
     &                             ' ET LE MANNING'
          IF (LOOP==1) WRITE(LU,*) 'POUR LA 2NDE LOI DEFINIR ' //
     &                             'LE NOM DE LA LOI : ',LAW,' ET'//
     &                             ' LE COEFFICIENT DE FROTTEMENT'//
     &                             ' ET LE MANNING'
        ELSE
          WRITE(LU,*) 'DEFINIR LE NOM DE LA LOI : ',LAW,' ET'//
     &                ' LE COEFFICIENT DE FROTTEMENT ET LE MANNING'
        ENDIF
      ENDIF
      IF (LNG.EQ.2) THEN
        WRITE(LU,*) 'FRICTION DATA FILE : ',NOMCOF
        WRITE(LU,*) 'READ ERROR ZONE : ',CHAINE(1)
        IF ((ITURB==3).AND.(LISRUG==2)) THEN
          IF (LOOP==1) WRITE(LU,*) 'FOR THE 1ST LAW DEFINE '//
     &                             'THE NAME OF THE LAW : ',LAW//
     &                             ' AND THE FRICTION COEFFICIENT'//
     &                             ' AND DEFAULT MANNING'
          IF (LOOP==2) WRITE(LU,*) 'FOR THE 2ND LAW DEFINE '//
     &                             'THE NAME OF THE LAW : ',LAW//
     &                             ' AND THE FRICTION COEFFICIENT'//
     &                             ' AND DEFAULT MANNING'
        ELSE
          WRITE(LU,*) 'DEFINE THE NAME OF THE LAW : ',LAW//
     &                ' AND THE FRICTION COEFFICIENT'//
     &                ' AND DEFAULT MANNING'
        ENDIF
      ENDIF
      CALL PLANTE(1)
      STOP
!
997   CONTINUE
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END

