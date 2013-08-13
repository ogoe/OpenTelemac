!                    ***************
                     SUBROUTINE DICO
!                    ***************
!
     &( ITYP   , NUMERO , ILONG  , CHAINE , MOTCLE , NMOT   , MOTPRO ,
     &  LONPRO , SIZE   , UTINDX , LANGUE , AIDLNG , MOTIGN , NIGN   ,
     &  LUIGN  , TYPIGN , LONIGN , NFICDA , NBLANG , NMAXR )
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    LOOKS FOR A CHARACTER STRING IN THE DICTIONARY.
!+
!+             FOR THE DICTIONARY FILE, LOOKS AMONGST THE RESERVED
!+             WORDS.
!+
!+             FOR THE STEERING FILE, LOOKS AMONGST THE ACTIVE KEYWORDS
!+             AND AMONGST THE WORDS IGNORED IN THE DICTIONARY BUT
!+             WRITTEN BY EDAMOX.
!
!note     PORTABILITY : IBM,CRAY,HP,SUN
!
!warning  ADDING A LANGUAGE MUST RESULT IN AN INCREASE IN THE
!+            NUMBER OF LOOPS AGAINST AVAILABLE LANGUAGES
!+           (2 LANGUAGES IN THIS CASE: F ANF GB)
!
!history  O. QUIQUEMPOIX (LNH)
!+        15/12/1993
!+
!+
!
!history  J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE
!+        14/01/2008
!+        V5P8
!+   JMH: DECOMPOSITION OF IF TO AVOID STRINGS LONGER THAN THEIR SIZE
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
!| AIDLNG         |<--| LOGIQUE .TRUE. SI L'AIDE EST CELLE DE LNG
!| CHAINE         |-->| CHAINE A ANALYSER
!| ILONG          |-->| LONGUEUR DE LA CHAINE A ANALYSER
!| ITYP           |<--| TYPE DU MOT-CLE  :    1  ENTIER
!|                |   | 2  REEL
!|                |   | 3  LOGIQUE
!|                |   | 4  CARACTERES
!|                |   | 5  MOT RESERVE
!|                |   | 0  MOT INCONNU
!| LANGUE         |<--| LOGIQUE=.TRUE. SI LA CHAINE EST RECONNUE
!| LONIGN         |-->| TABLEAU DES LONGUEURS DES MOTS DE MOTIGN
!| LONPRO         |-->| LONGUEURS DES MOTS CLES DE MOTPRO
!| LUIGN          |-->| LOGIQUE POUR LES MOTS A NE PAS CLASSER
!| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
!| MOTIGN         |-->| TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
!| MOTPRO         |-->| TABLEAU DES MOTS CLES RESERVES AU PROGRAMME
!| NBLANG         |-->| NOMBRE DE LANGUES CONNUES
!| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
!| NIGN           |-->| NOMBRE DE MOTS CLES DUS A EDAMOX A IGNORER
!| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
!| NMOT           |<->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!|                |   | NMOT(1) ENTIERS
!|                |   | NMOT(2) REELS
!|                |   | NMOT(3) LOGIQUES
!|                |   | NMOT(4) CARACTERES
!| NUMERO         |<--| ORDRE DU MOT-CLE PARMI CEUX DE SON TYPE
!| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
!| TYPIGN         |-->| TABLEAU DES TYPES DES MOTS EDAMOX A IGNORER
!| UTINDX         |-->| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER       NMOT(4),SIZE(4,*),ITYP,NUMERO,ILONG,NBLANG,NMAXR(4)
      INTEGER       NIGN,NFICDA,TYPIGN(100),LONIGN(100),LONPRO(15)
      LOGICAL       UTINDX(4,*),LANGUE,LUIGN,AIDLNG
      CHARACTER*(*) MOTCLE(4,*),MOTPRO(*),CHAINE
      CHARACTER*1   LNGPRO(9)
      CHARACTER*9   RUBPRO(5),MOTLNG
      CHARACTER*72  MOTIGN(100)
!
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
!
!-----------------------------------------------------------------------
!
      INTEGER       INDX,LGRUB(5),I,K,LNGINT,VALNUM(5)
!
!-----------------------------------------------------------------------
!
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
!
!-----------------------------------------------------------------------
!
      DATA LNGPRO /'1','2','3','4','5','6','7','8','9'/
      DATA RUBPRO /'NOM','DEFAUT','AIDE','CHOIX','RUBRIQUE'/
! NUMBER OF LETTERS IN THE RUBPRO NAMES
      DATA LGRUB  /3,6,4,5,8/
! CORRESPONDENCES BETWEEN RUBPRO AND MOTPRO
      DATA VALNUM /1,5,6,7,8/
!
!***********************************************************************
!                                    RCS AND SCCS MARKING
!
!***********************************************************************
!
! LANGUE IS ONLY USED WHEN READING THE DICTIONARY.
! IT IS NOT USED WHEN READING THE USER FILE.
!
      LANGUE = .FALSE.
      AIDLNG = .FALSE.
!
      LNGINT = LNG - 1
!
!*******************************************************
!  1) SEARCHES THROUGH THE USER KEYWORDS:
!*******************************************************
!
      IF (NFIC.EQ.NFICDA) THEN
        DO 100 ITYP = 1,4
          DO 110 INDX=1,NMAXR(ITYP)
            IF (UTINDX(ITYP,INDX)) THEN
              K=SIZE(ITYP,INDX)
              IF(K.EQ.ILONG) THEN
                IF(CHAINE(1:K).EQ.MOTCLE(ITYP,INDX)(1:K)) THEN
                  NUMERO=INDX
                  GO TO 1000
                ENDIF
              ENDIF
            ENDIF
 110      CONTINUE
 100    CONTINUE
!
! IF NOT, DETERMINES IF ITS AN EDAMOX KEYWORD OF INDEX = -1
!
        DO 900 I=1,NIGN
          IF(LONIGN(I).EQ.ILONG) THEN
            IF(CHAINE(1:ILONG).EQ.MOTIGN(I)(1:ILONG)) THEN
              ITYP = TYPIGN(I)
               LUIGN = .TRUE.
              GO TO 1000
            ENDIF
          ENDIF
900     CONTINUE
!
! END OF SEARCH THROUGH THE USER KEYWORDS
       GO TO 910
      ENDIF
!
!
!*********************************************
!  2) SEARCHES THROUGH THE RESERVED WORDS:
!*********************************************
!
!  AIDLNG (LOGICAL) IS TRUE IF THE HELP IS THAT OF THE SELECTED LANGUAGE
!
! IF IT IS AN ENGLISH WORD: NO NEED TO LOOK FOR IT AMONG THE FR
! THAT SAVES 50 TESTS PER WORD FOR TELEMAC FOR EXAMPLE
! (ESTIMATED 6500 TESTS FOR TELEMAC)
      IF (CHAINE(ILONG:ILONG).EQ.'1') GOTO 125
!
      DO 120 I=1,15
       IF (ILONG.EQ.LONPRO(I)) THEN
         IF (CHAINE(1:ILONG).EQ.MOTPRO(I)(1:ILONG)) THEN
!           IF 'AIDE' AND LNG=FRANCAIS, WILL EDIT THE HELP IF DOC
            IF (I.EQ.6 .AND. LNGINT .EQ. 0) AIDLNG = .TRUE.
            LANGUE = .TRUE.
            NUMERO = I
            ITYP   = 5
            GO TO 1000
         ENDIF
       ENDIF
120   CONTINUE
!
!  IF NOT: LOOKS FOR IT AMONG THE RESERVED WORDS FOR LANGUAGES
!          OTHER THAN FRENCH. (MAX NBLANG LANGUAGES AND NBLANG<=10)
!
! LNG IS THE EXTERNAL LANGUAGE PARAMETER (1 = FRENCH, 2 = ENGLISH ...)
! LNGINT IS THE LANGUAGE PARAMETER INTERNAL TO DAMOCLE
! (I.E. 0 = FRENCH, 1 = ENGLISH ...)
!
!  AIDLNG = NUMBER OF THE HELP LINE IN REQUESTED LANGUAGE
!
125   CONTINUE
      IF (NBLANG.GE.2) THEN
      DO 130 I=1,5
      DO 131 K=1,NBLANG-1
        IF (LGRUB(I)+1.EQ.ILONG) THEN
        MOTLNG = RUBPRO(I)(1:LGRUB(I))//LNGPRO(K)(1:1)
        IF (CHAINE(1:ILONG).EQ.MOTLNG(1:ILONG)) THEN
          NUMERO=VALNUM(I)
!
          IF (I.EQ.3 .AND. K.EQ.LNGINT) AIDLNG = .TRUE.
!
          ITYP = 5
!
! RETURNS LANGUE = .TRUE. ONLY FOR DAMOCLE KEYWORDS
! EXCEPT FOR 'AIDE', IN WHICH CASE LANGUE IS NOT USED
! NOT THE SAME TREATMENT DEPENDING ON THE SELECTED LANGUAGE
          IF (K.EQ.LNGINT.AND.I.GE.1.AND.I.LE.3) LANGUE = .TRUE.
          GO TO 1000
        ENDIF
        ENDIF
131   CONTINUE
130   CONTINUE
      ENDIF
!
!  6) ERROR : KEYWORD UNKNOWN
!
 910  CONTINUE
      ERREUR = .TRUE.
      ITYP = 0
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'*************************************************'
        WRITE(LU,*)'A LA LIGNE ',NLIGN,' LE MOT CLE SUIVANT : ',
     &              CHAINE(1:ILONG),' EST INCONNU ...'
        WRITE(LU,*)'*************************************************'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'*************************************************'
        WRITE(LU,*)'AT LINE    ',NLIGN,' THE KEY-WORD       : ',
     &              CHAINE(1:ILONG),' IS UNKNOWN...'
        WRITE(LU,*)'*************************************************'
      ENDIF
!
1000  CONTINUE
!
      RETURN
      END