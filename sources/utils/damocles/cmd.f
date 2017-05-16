!                    **************
                     SUBROUTINE CMD
!                    **************
!
     &(ICOL   , LIGNE  , ADRESS , DIMENS , TROUVE , MOTCLE , NMOT2 ,
     & MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT , INDIC  , SIZE ,
     & UTINDX , DYNAM  , VUCMD  , EXECMD , NFICDA , NMAXR  )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CARRIES OUT A COMMAND PROVIDED IN THE DICTIONARY AND
!+             STEERING FILES : COMMAND = '&' + 3 LETTERS.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!note      DOCUMENTATION : COMMANDS &LIS, &ETA, &IND, &STO, &FIN
!+                             ARE ONLY CARRIED OUT IF EXECMD=.TRUE.
!+                             AND VUCMD(NB_CMB)=.TRUE.
!+                             COMMAND &DYN IS IGNORED IN THE STEERING FILE
!
!history  O. QUIQUEMPOIX (LNH)
!+        14/12/1993
!+
!+
!
!history  J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE
!+        15/01/2008
!+        V5P8
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
!| ADRESS         |-->| TABLEAU DES ADRESSES DES MOTS CLES
!| DIMENS         |-->| TABLEAU DES DIMENSIONS DES MOTS CLES
!| DYNAM          |<->| LOGIQUE POUR LE MODE DYNAMIQUE
!| EXECMD         |-->| LOGIQUE D'ACTIVATION DES COMMANDES MEMORISEES
!| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
!| INDIC          |-->| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
!|                |   | = 1 : PAS DE SUBMIT & TABLEAU
!|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
!|                |   | = 3 : AVEC   SUBMIT & NON TABLEAU
!| LIGNE          |-->| LIGNE EN COURS DE DECODAGE.
!| MOTATT         |-->| TABLEAU DES SUBMITS
!| MOTCAR         |-->| TABLEAU DES VALEURS CARACTERES
!| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
!| MOTINT         |-->| TABLEAU DES VALEURS ENTIERES
!| MOTLOG         |-->| TABLEAU DES VALEURS LOGIQUES
!| MOTREA         |-->| TABLEAU DES VALEURS REELLES
!| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
!| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
!| NMOT2           |-->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
!| TROUVE         |-->| INDICATEUR D'ETAT DES MOTS CLES
!|                |   | = 0 : AUCUNE VALEUR TROUVEE
!|                |   | = 1 : VALEUR PAR DEFAUT TROUVEE
!|                |   | = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)
!|                |   | = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)
!|                |   | = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE
!|                |   | = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE
!|                |   | = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)
!|                |   | = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)
!|                |   | = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR
!|                |   | =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR
!| UTINDX         |-->| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!| VUCMD          |<->| TABLEAU DE LOGIQUES (MEMORISATION DES CMDES)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_DAMOCLES
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER          ICOL,NMOT2(4),ADRESS(4,*),DIMENS(4,*),TROUVE(4,*)
      INTEGER          SIZE(4,*),INDIC(4,*),MOTINT(*),NFICDA,NMAXR(4)
      LOGICAL          MOTLOG(*),DYNAM,UTINDX(4,*),VUCMD(5),EXECMD
      CHARACTER(LEN=*)    MOTCLE(4,*),LIGNE
      CHARACTER(LEN=144)    MOTATT(4,*),MOTCAR(*)
      DOUBLE PRECISION MOTREA(*)
!
      INTEGER  PREVAL,LONGLU
      EXTERNAL PREVAL,LONGLU
!
!-----------------------------------------------------------------------
!
      INTEGER          I1,IAD,L1,L2,TRANS,ISIZE,K,I,N
      CHARACTER(LEN=72)  :: FMT0, FMT1, FMT2, FMT3, FMT4, FMT5, FMT6,
     &     FMT7, FMT8, FMT9, FMT10, FMT11, FMT12, FMT13, FMT14, FMT15,
     &     FMT16, FMT17, FMT18, FMT19, FMT20, FMT21, FMT22, FMT23,
     &     FMT24, FMT25, FMT26, FMT27, FMT28, FMT29, FMT30, FMT31,
     &     FMT32, FMT33, FMT34, FMT35
      CHARACTER(LEN=6)  ::  TYP(4)
      CHARACTER(LEN=1)      TABUL
!
!-----------------------------------------------------------------------
!
      INTRINSIC CHAR
!
!-----------------------------------------------------------------------
!
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA TYP/'MOTINT','MOTREA','MOTLOG','MOTCAR'/
      PARAMETER ( TYP = (/ 'MOTINT','MOTREA','MOTLOG','MOTCAR' /) )
!##< SEB @ HRW
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
      TABUL = CHAR(9)
      I1 = ICOL + 1
!     ADDED BY JMH 15/01/2008: CASE WHERE LIGNE='NUL'
      IF(I1+2.GT.LONGLI) I1=1
!
! *********************** COMMAND &FIN **************************
!
      IF(LIGNE(I1:I1+2).EQ.'FIN'.OR.(EXECMD.AND.VUCMD(5))) THEN
        IF (.NOT.(EXECMD)) THEN
          VUCMD(5) = .TRUE.
          RETOUR = .TRUE.
          GO TO 1000
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE (LU,10)
 10       FORMAT(1X,/,1X,'FIN DU FICHIER POUR DAMOCLES',/)
        ELSEIF(LNG.EQ.2) THEN
          WRITE (LU,12)
 12       FORMAT(1X,/,1X,'END OF FILE FOR DAMOCLES',/)
        ENDIF
!
! *********************** COMMAND &ETA **************************
!
      ELSE IF (LIGNE(I1:I1+2).EQ.'ETA'.OR.(EXECMD.AND.VUCMD(2))) THEN
        IF (.NOT.(EXECMD)) THEN
          VUCMD(2) = .TRUE.
          GO TO 1000
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE (LU,11)
 11       FORMAT(1X,/,1X,'VALEUR DES MOTS-CLES :',/)
        ELSEIF(LNG.EQ.2) THEN
          WRITE (LU,13)
 13       FORMAT(1X,/,1X,'VALUES OF THE KEY-WORDS:',/)
        ENDIF
!
        FMT1 ="(1X,A,/,1X,'MOTINT(',1I3,')=',A,I9   ,/)"
        FMT2 ="(1X,A,/,1X,'MOTREA(',1I3,')=',A,G16.7,/)"
        FMT3 ="(1X,A,/,1X,'MOTLOG(',1I3,')=',A,L1   ,/)"
        FMT4 ="(1X,A,/,1X,'MOTCAR(',1I3,')=',A,A    ,/)"
        FMT5 ="(1X,A,/,1X,'MOTINT(',1I3,') = ',A,' ; ',I9   ,/)"
        FMT6 ="(1X,A,/,1X,'MOTREA(',1I3,') = ',A,' ; ',G16.7,/)"
        FMT7 ="(1X,A,/,1X,'MOTLOG(',1I3,') = ',A,' ; ',L1   ,/)"
        FMT8 ="(1X,A,/,1X,'MOTCAR(',1I3,') = ',A,' ; ',A    ,/)"
!
        DO N =1,4
          DO I = 1 , NMAXR(N)
            IF(UTINDX(N,I)) THEN
              ISIZE = SIZE(N,I)
              IF(TROUVE(N,I).GE.1) THEN
                DO K=1,DIMENS(N,I)
                  IAD = ADRESS(N,I) + K - 1
                  IF (INDIC(N,I).LT.2) THEN
                    TRANS=0
                    MOTATT(N,IAD)=' '
                    L1=1
                  ELSE
                    TRANS=4
                    L1=LONGLU(MOTATT(N,IAD))
                  ENDIF
!                 IF (TROUVE(N,I).NE.3) THEN
                  ! Array as format not accepted in fortran 95
                  SELECT CASE (N+TRANS)
                  CASE(1)
                    FMT0 = FMT1
                  CASE(2)
                    FMT0 = FMT2
                  CASE(3)
                    FMT0 = FMT3
                  CASE(4)
                    FMT0 = FMT4
                  CASE(5)
                    FMT0 = FMT5
                  CASE(6)
                    FMT0 = FMT6
                  CASE(7)
                    FMT0 = FMT7
                  CASE(8)
                    FMT0 = FMT8
                  END SELECT
                  IF(N.EQ.1) THEN
                    WRITE(LU,FMT0)
     &              MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),
     &              MOTINT(IAD)
                  ELSE IF (N.EQ.2) THEN
                    WRITE(LU,FMT0)
     &              MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),
     &              MOTREA(IAD)
                  ELSE IF (N.EQ.3) THEN
                    WRITE(LU,FMT0)
     &              MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),
     &              MOTLOG(IAD)
                  ELSE IF (N.EQ.4) THEN
                    L2 = LONGLU(MOTCAR(IAD))
                    WRITE(LU,FMT0)
     &              MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),
     &              MOTCAR(IAD)(1:L2)
                  ENDIF
!                 ENDIF
                ENDDO ! K
              ELSE
                IF(LNG.EQ.1) THEN
                  WRITE(LU,212) MOTCLE(N,I)(1:ISIZE)
212               FORMAT(1X,A,/,1X,'VALEUR NON TROUVEE',/,1X)
                ELSEIF(LNG.EQ.2) THEN
                  WRITE(LU,213) MOTCLE(N,I)(1:ISIZE)
213               FORMAT(1X,A,/,1X,'VALUE NOT FOUND',/,1X)
                ENDIF
              ENDIF
!
            ENDIF
          ENDDO ! I
        ENDDO ! N
!
! *********************** COMMAND &IND **************************
!
      ELSE IF (LIGNE(I1:I1+2).EQ.'IND'.OR.(EXECMD.AND.VUCMD(3))) THEN
        IF (.NOT.(EXECMD)) THEN
          VUCMD(3) = .TRUE.
          GOTO 1000
        ENDIF
!
! DEFINITION OF THE FORMATS USED
!
        FMT1 ="(1X,'MOTINT(',1I3,') =',A,I9   )"
        FMT2 ="(1X,'MOTREA(',1I3,') =',A,G16.7)"
        FMT3 ="(1X,'MOTLOG(',1I3,') =',A,L1   )"
        FMT4 ="(1X,'MOTCAR(',1I3,') =',A,A    )"
        FMT5 ="(1X,'MOTINT(',1I3,') = ',A,' ; ',I9   )"
        FMT6 ="(1X,'MOTREA(',1I3,') = ',A,' ; ',G16.7)"
        FMT7 ="(1X,'MOTLOG(',1I3,') = ',A,' ; ',L1   )"
        FMT8 ="(1X,'MOTCAR(',1I3,') = ',A,' ; ',A    )"
        FMT9 ="(1X,'!!! TABLEAU COMPACTE !!!')"
        FMT10="(1X,'!!! COMPACTED ARRAY !!!')"
        FMT11="(1X,'ATTENTION ! TAILLE EN SORTIE = 0')"
        FMT12="(1X,'WARNING ! OUTPUT SIZE = 0')"
        FMT13="(1X,'TAILLE = ',I4)"
        FMT14="(1X,'SIZE  = ',I4)"
        FMT15="(1X,'VALEUR OPTIONNELLE NON TROUVEE')"
        FMT16="(1X,'OPTIONAL VALUE NOT FOUND')"
        FMT17="(1X,'VALEUR FORCEE NON TROUVEE')"
        FMT18="(1X,'FORCED VALUE NOT FOUND')"
        FMT19="(1X,'INDEX  = ',I4)"
        FMT20="(1X,'INDEX = ',I4)"
        FMT21="(1X,'VALEUR NON TROUVEE')"
        FMT22="(1X,'VALUE NOT FOUND')"
        FMT23="(/,1X,'VALEUR DES MOTS-CLES :',/)"
        FMT24="(/,1X,'VALUES OF THE KEY-WORDS :',/)"
        FMT25="(1X,'NOMBRE DE MOTS ENTIERS     = ',I4,)"//
     &        "10X,'(DERNIER INDEX :',I4,')')"
        FMT26="(1X,'NUMBER OF INTEGER   KEY WORDS = ',I4,"//
     &        "10X,'(LAST INDEX :',I4,')')"
        FMT27="(1X,'NOMBRE DE MOTS REELS       = ',I4,"//
     &        "10X,'(DERNIER INDEX :',I4,')')"
        FMT28="(1X,'NUMBER OF REAL      KEY WORDS = ',I4,"//
     &        "10X,'(LAST INDEX :',I4,')')"
        FMT29="(1X,'NOMBRE DE MOTS LOGIQUES    = ',I4,"//
     &        "10X,'(DERNIER INDEX :',I4,')')"
        FMT30="(1X,'NUMBER OF LOGICAL   KEY WORDS = ',I4,"//
     &        "10X,'(LAST INDEX :',I4,')')"
        FMT31="(1X,'NOMBRE DE MOTS CARACTERES  = ',I4,"//
     &        "10X,'(DERNIER INDEX :',I4,')')"
        FMT32="(1X,'NUMBER OF CHARACTER KEY WORDS = ',I4,"//
     &        "10X,'(LAST INDEX :',I4,')')"
        FMT33="(1X,'NOMBRE TOTAL DE MOTS CLES  = ',I4)"
        FMT34="(1X,'TOTAL NUMBER OF KEY WORDS     = ',I4)"
        FMT35="(/,1X,70('-'),/,1X,A,/,1X,70('-'))"
!
! TITLE
        IF(LNG.EQ.1) THEN
          WRITE(LU,FMT23)
!
          WRITE(LU,*)' '
          WRITE(LU,*)'====================================='
          WRITE(LU,FMT25) NMOT2(1),NMAXR(1)
          WRITE(LU,FMT27) NMOT2(2),NMAXR(2)
          WRITE(LU,FMT29) NMOT2(3),NMAXR(3)
          WRITE(LU,FMT25) NMOT2(4),NMAXR(4)
          WRITE(LU,*)'-------------------------------------'
          WRITE(LU,FMT33) NMOT2(1)+NMOT2(2)+NMOT2(3)+NMOT2(4)
          WRITE(LU,*)'====================================='
          WRITE(LU,*)' '
        ELSE
          WRITE(LU,FMT24)
!
          WRITE(LU,*)' '
          WRITE(LU,*)'====================================='
          WRITE(LU,FMT26) NMOT2(1),NMAXR(1)
          WRITE(LU,FMT28) NMOT2(2),NMAXR(2)
          WRITE(LU,FMT30) NMOT2(3),NMAXR(3)
          WRITE(LU,FMT26) NMOT2(4),NMAXR(4)
          WRITE(LU,*)'-------------------------------------'
          WRITE(LU,FMT34) NMOT2(1)+NMOT2(2)+NMOT2(3)+NMOT2(4)
          WRITE(LU,*)'====================================='
          WRITE(LU,*)' '

        ENDIF
!
        DO N =1,4
          DO I = 1 , NMAXR(N)
            IF(UTINDX(N,I)) THEN
              IF(TROUVE(N,I).GE.1.OR.DIMENS(N,I).GT.1) THEN
                WRITE(LU,FMT35) MOTCLE(N,I)(1:SIZE(N,I))
! COMPACTED ?
                IF (TROUVE(N,I).EQ.5) THEN
                  IF(LNG.EQ.1) WRITE(LU,FMT9)
                  IF(LNG.EQ.2) WRITE(LU,FMT10)
                ENDIF
! INDEX
                IF(LNG.EQ.1) WRITE(LU,FMT19) I
                IF(LNG.EQ.2) WRITE(LU,FMT20) I
! SIZE
                IF(LNG.EQ.1) WRITE(LU,FMT13) DIMENS(N,I)
                IF(LNG.EQ.2) WRITE(LU,FMT14) DIMENS(N,I)
                IF (DIMENS(N,I).GT.1.AND.
     &              TROUVE(N,I).EQ.0.AND.DYNAM) THEN
                  IF(LNG.EQ.1) WRITE(LU,FMT11)
                  IF(LNG.EQ.2) WRITE(LU,FMT12)
                ENDIF
!
! TROUVE ?
                IF (TROUVE(N,I).EQ.3) THEN
                  IF(LNG.EQ.1) WRITE(LU,FMT15)
                  IF(LNG.EQ.2) WRITE(LU,FMT16)
                ENDIF
                IF (TROUVE(N,I).EQ.6) THEN
                  IF(LNG.EQ.1) WRITE(LU,FMT17)
                  IF(LNG.EQ.2) WRITE(LU,FMT18)
                ENDIF
!
! LINEFEED FOR PRESENTATION PURPOSES
                IF (DIMENS(N,I).GT.1) WRITE(LU,*) ' '
!
                DO K=1,DIMENS(N,I)
                  IAD = ADRESS(N,I) + K - 1
                  IF (INDIC(N,I).GE.2) THEN
                    TRANS = 4
                    L1=LONGLU(MOTATT(N,IAD))
                  ELSE
                    TRANS = 0
                    MOTATT(N,IAD)=' '
                    L1 =1
                  ENDIF
!
!                 IF (TROUVE(N,I).NE.3) THEN
                  SELECT CASE (N+TRANS)
                  CASE(1)
                    FMT0 = FMT1
                  CASE(2)
                    FMT0 = FMT2
                  CASE(3)
                    FMT0 = FMT3
                  CASE(4)
                    FMT0 = FMT4
                  CASE(5)
                    FMT0 = FMT5
                  CASE(6)
                    FMT0 = FMT6
                  CASE(7)
                    FMT0 = FMT7
                  CASE(8)
                    FMT0 = FMT8
                  END SELECT
                  IF(N.EQ.1) THEN
                    WRITE(LU,FMT0)
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTINT(IAD)
                  ELSE IF (N.EQ.2) THEN
                    WRITE(LU,FMT0)
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTREA(IAD)
                  ELSE IF (N.EQ.3) THEN
                    WRITE(LU,FMT0)
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTLOG(IAD)
                  ELSE IF (N.EQ.4) THEN
                    L2 = LONGLU(MOTCAR(IAD))
                    WRITE(LU,FMT0)
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTCAR(IAD)(1:L2)
                  ENDIF
!                 ENDIF
                ENDDO ! K
              ELSE
                WRITE(LU,FMT35) MOTCLE(N,I)(1:SIZE(N,I))
                IF(LNG.EQ.1) THEN
                  WRITE(LU,FMT21)
                  WRITE(LU,FMT19) I
                  WRITE(LU,FMT13) DIMENS(N,I)
                ELSE
                  WRITE(LU,FMT22)
                  WRITE(LU,FMT20) I
                  WRITE(LU,FMT14) DIMENS(N,I)
                ENDIF
                WRITE(LU,*)' '
              ENDIF
!
            ENDIF
          ENDDO ! I
        ENDDO ! N
!
! *********************** COMMAND &LIS **************************
!
      ELSE IF (LIGNE(I1:I1+2).EQ.'LIS'.OR.(EXECMD.AND.VUCMD(1))) THEN
        IF (.NOT.(EXECMD)) THEN
          VUCMD(1) = .TRUE.
          GO TO 1000
        ENDIF
! FORMATS
        FMT1 = "(/,1X,'LISTE DES MOTS-CLES :',/)"
        FMT2 = "(/,1X,'KEY-WORDS LIST :',/)"
        FMT3 = "(1X,'DIMENSION : ',I3,5X,'ADRESSE DANS ',A,"//
     &              "1X,':',1X,I3)"
        FMT4 = "(1X,'SIZE : ',I3,5X,'ADRESS IN ',A,"//
     &              "1X,':',1X,I3)"
        FMT5 = "(1X,/,1X,A)"
! TITLE
        IF(LNG.EQ.1) WRITE (LU,FMT1)
        IF(LNG.EQ.2) WRITE (LU,FMT2)
!
        DO N = 1 , 4
          DO I = 1 , NMAXR(N)
!
            IF(UTINDX(N,I)) THEN
              IAD = ADRESS(N,I)
              WRITE (LU,FMT5) MOTCLE(N,I)(1:SIZE(N,I))
              IF (DIMENS(N,I).GT.1.AND.TROUVE(N,I).EQ.0.AND.DYNAM) THEN
                IF(LNG.EQ.1) WRITE (LU,FMT3) 0,TYP(N),IAD
                IF(LNG.EQ.2) WRITE (LU,FMT4) 0,TYP(N),IAD
              ELSE
                IF(LNG.EQ.1) WRITE (LU,FMT3) DIMENS(N,I),TYP(N),IAD
                IF(LNG.EQ.2) WRITE (LU,FMT4) DIMENS(N,I),TYP(N),IAD
              ENDIF
            ENDIF
          ENDDO ! I
        ENDDO ! N
!
! *********************** COMMAND &DOC **************************
!
      ELSE IF ( LIGNE(I1:I1+2).EQ.'DOC' ) THEN
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'COMMANDE &DOC SUPPRIMEE DANS CETTE VERSION'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'COMMAND &DOC HAS BEEN SUPPRESSED IN THIS RELEASE'
        ENDIF
!
! *********************** COMMAND &STO **************************
!
      ELSE IF (LIGNE(I1:I1+2).EQ.'STO'.OR.(EXECMD.AND.VUCMD(4))) THEN
        IF (.NOT.(EXECMD)) THEN
          VUCMD(4) = .TRUE.
          RETOUR=.TRUE.
          GO TO 1000
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE (LU,1113)
1113      FORMAT(1X,/,1X,'ARRET DE DAMOCLES PAR LA COMMANDE &STO')
        ELSEIF(LNG.EQ.2) THEN
          WRITE (LU,1114)
1114      FORMAT(1X,/,1X,'DAMOCLES STOPPED BY COMMAND &STO')
        ENDIF
        CALL PLANTE(1)
        STOP
!
! *********************** COMMAND &DYN **************************
!
      ELSEIF ( LIGNE(I1:I1+2).EQ.'DYN' ) THEN
        IF (NFIC.EQ.NFICDA) THEN
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'COMMANDE &DYN DU FICHIER CAS IGNOREE !!'
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'WARNING : INSTRUCTION &DYN FROM STEERING ',
     &                 'FILE HAS BEEN IGNORED !!'
          ENDIF
        ELSE
          DYNAM=.TRUE.
        ENDIF
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,'(1X,A)') LIGNE(1:LONGLI)
          WRITE(LU,'(1X,A6,I4,A)') 'LIGNE: ',NLIGN,' COMMANDE INCONNUE'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,'(1X,A)') LIGNE(1:LONGLI)
          WRITE(LU,'(1X,A6,I4,A)') 'LINE: ',NLIGN,' UNKNOWN COMMAND'
        ENDIF
      ENDIF
!
!     //// SEEKS THE FIRST WHITE CHARACTER FOLLOWING & ////
!
 1000 CONTINUE
      ICOL = PREVAL (I1+1,LIGNE,' ',TABUL,' ')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
