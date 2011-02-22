C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    LOOKS FOR A CHARACTER STRING IN THE DICTIONARY.<br>
!>             FOR THE DICTIONARY FILE, LOOKS AMONGST THE RESERVED
!>             WORDS.<br>
!>             FOR THE STEERING FILE, LOOKS AMONGST THE ACTIVE KEYWORDS
!>             AND AMONGST THE WORDS IGNORED IN THE DICTIONARY BUT
!>             WRITTEN BY EDAMOX.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @warning  ADDING A LANGUAGE MUST RESULT IN AN INCREASE IN THE
!>            NUMBER OF LOOPS AGAINST AVAILABLE LANGUAGES
!>           (2 LANGUAGES IN THIS CASE: F ANF GB)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIDLNG, CHAINE, ILONG, ITYP, LANGUE, LONIGN, LONPRO, LUIGN, MOTCLE, MOTIGN, MOTPRO, NBLANG, NFICDA, NIGN, NMAXR, NMOT, NUMERO, SIZE, TYPIGN, UTINDX
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCCHIE : NFIC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, INDX, K, LGRUB, LNGINT, LNGPRO, MOTLNG, RUBPRO, VALNUM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOC()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td><center> 5.8                                    </center></td>
!>    <td> 14/01/2008                                              </td>
!>    <td> J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE               </td>
!>    <td> JMH: DECOMPOSITION OF IF TO AVOID STRINGS LONGER THAN THEIR SIZE </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 15/12/1993                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH) 30.87.78.70                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIDLNG
!></td><td><--</td><td>LOGIQUE .TRUE. SI L'AIDE EST CELLE DE LNG
!>    </td></tr>
!>          <tr><td>CHAINE
!></td><td>--></td><td>CHAINE A ANALYSER
!>    </td></tr>
!>          <tr><td>ERREUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>ILONG
!></td><td>--></td><td>LONGUEUR DE LA CHAINE A ANALYSER
!>    </td></tr>
!>          <tr><td>ITYP
!></td><td><--</td><td>TYPE DU MOT-CLE  :    1  ENTIER
!>                  2  REEL
!>                  3  LOGIQUE
!>                  4  CARACTERES
!>                  5  MOT RESERVE
!>                  0  MOT INCONNU
!>    </td></tr>
!>          <tr><td>LANGUE
!></td><td><--</td><td>LOGIQUE=.TRUE. SI LA CHAINE EST RECONNUE
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LONIGN
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS DE MOTIGN
!>    </td></tr>
!>          <tr><td>LONPRO
!></td><td>--></td><td>LONGUEURS DES MOTS CLES DE MOTPRO
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>LUIGN
!></td><td>--></td><td>LOGIQUE POUR LES MOTS A NE PAS CLASSER
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td>--></td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTIGN
!></td><td>--></td><td>TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
!>    </td></tr>
!>          <tr><td>MOTPRO
!></td><td>--></td><td>TABLEAU DES MOTS CLES RESERVES AU PROGRAMME
!>    </td></tr>
!>          <tr><td>NBLANG
!></td><td>--></td><td>NOMBRE DE LANGUES CONNUES
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
!>    </td></tr>
!>          <tr><td>NFICDA
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES DONNEES
!>    </td></tr>
!>          <tr><td>NIGN
!></td><td>--></td><td>NOMBRE DE MOTS CLES DUS A EDAMOX A IGNORER
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td>--></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>NMAXR
!></td><td>--></td><td>TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
!>    </td></tr>
!>          <tr><td>NMOT
!></td><td><-></td><td>TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!>                  NMOT(1) ENTIERS
!>                  NMOT(2) REELS
!>                  NMOT(3) LOGIQUES
!>                  NMOT(4) CARACTERES
!>    </td></tr>
!>          <tr><td>NUMERO
!></td><td><--</td><td>ORDRE DU MOT-CLE PARMI CEUX DE SON TYPE
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>TYPIGN
!></td><td>--></td><td>TABLEAU DES TYPES DES MOTS EDAMOX A IGNORER
!>    </td></tr>
!>          <tr><td>UTINDX
!></td><td>--></td><td>TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DICO
     &( ITYP   , NUMERO , ILONG  , CHAINE , MOTCLE , NMOT   , MOTPRO ,
     &  LONPRO , SIZE   , UTINDX , LANGUE , AIDLNG , MOTIGN , NIGN   ,
     &  LUIGN  , TYPIGN , LONIGN , NFICDA , NBLANG , NMAXR )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIDLNG         |<--| LOGIQUE .TRUE. SI L'AIDE EST CELLE DE LNG
C| CHAINE         |-->| CHAINE A ANALYSER
C| ERREUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| ILONG          |-->| LONGUEUR DE LA CHAINE A ANALYSER
C| ITYP           |<--| TYPE DU MOT-CLE  :    1  ENTIER
C|                |   | 2  REEL
C|                |   | 3  LOGIQUE
C|                |   | 4  CARACTERES
C|                |   | 5  MOT RESERVE
C|                |   | 0  MOT INCONNU
C| LANGUE         |<--| LOGIQUE=.TRUE. SI LA CHAINE EST RECONNUE
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LONIGN         |-->| TABLEAU DES LONGUEURS DES MOTS DE MOTIGN
C| LONPRO         |-->| LONGUEURS DES MOTS CLES DE MOTPRO
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| LUIGN          |-->| LOGIQUE POUR LES MOTS A NE PAS CLASSER
C| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
C| MOTIGN         |-->| TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
C| MOTPRO         |-->| TABLEAU DES MOTS CLES RESERVES AU PROGRAMME
C| NBLANG         |-->| NOMBRE DE LANGUES CONNUES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
C| NIGN           |-->| NOMBRE DE MOTS CLES DUS A EDAMOX A IGNORER
C| NLIGN          |-->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
C| NMOT           |<->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
C|                |   | NMOT(1) ENTIERS
C|                |   | NMOT(2) REELS
C|                |   | NMOT(3) LOGIQUES
C|                |   | NMOT(4) CARACTERES
C| NUMERO         |<--| ORDRE DU MOT-CLE PARMI CEUX DE SON TYPE
C| RETOUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
C| TYPIGN         |-->| TABLEAU DES TYPES DES MOTS EDAMOX A IGNORER
C| UTINDX         |-->| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER       NMOT(4),SIZE(4,*),ITYP,NUMERO,ILONG,NBLANG,NMAXR(4)
      INTEGER       NIGN,NFICDA,TYPIGN(100),LONIGN(100),LONPRO(15)
      LOGICAL       UTINDX(4,*),LANGUE,LUIGN,AIDLNG
      CHARACTER*(*) MOTCLE(4,*),MOTPRO(*),CHAINE
      CHARACTER*1   LNGPRO(9)
      CHARACTER*9   RUBPRO(5),MOTLNG
      CHARACTER*72  MOTIGN(100)
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR,RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       INDX,LGRUB(5),I,K,LNGINT,VALNUM(5)
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
C
C-----------------------------------------------------------------------
C
      DATA LNGPRO /'1','2','3','4','5','6','7','8','9'/
      DATA RUBPRO /'NOM','DEFAUT','AIDE','CHOIX','RUBRIQUE'/
C NUMBER OF LETTERS IN THE RUBPRO NAMES
      DATA LGRUB  /3,6,4,5,8/
C CORRESPONDENCES BETWEEN RUBPRO AND MOTPRO
      DATA VALNUM /1,5,6,7,8/
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
C LANGUE IS ONLY USED WHEN READING THE DICTIONARY.
C IT IS NOT USED WHEN READING THE USER FILE.
C
      LANGUE = .FALSE.
      AIDLNG = .FALSE.
C
      LNGINT = LNG - 1
C
C*******************************************************
C  1) SEARCHES THROUGH THE USER KEYWORDS:
C*******************************************************
C
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
C
C IF NOT, DETERMINES IF ITS AN EDAMOX KEYWORD OF INDEX = -1
C
        DO 900 I=1,NIGN
          IF(LONIGN(I).EQ.ILONG) THEN
            IF(CHAINE(1:ILONG).EQ.MOTIGN(I)(1:ILONG)) THEN
              ITYP = TYPIGN(I)
               LUIGN = .TRUE.
              GO TO 1000
            ENDIF
          ENDIF
900     CONTINUE
C
C END OF SEARCH THROUGH THE USER KEYWORDS
       GO TO 910
      ENDIF
C
C
C*********************************************
C  2) SEARCHES THROUGH THE RESERVED WORDS:
C*********************************************
C
C  AIDLNG (LOGICAL) IS TRUE IF THE HELP IS THAT OF THE SELECTED LANGUAGE
C
C IF IT IS AN ENGLISH WORD: NO NEED TO LOOK FOR IT AMONG THE FR
C THAT SAVES 50 TESTS PER WORD FOR TELEMAC FOR EXAMPLE
C (ESTIMATED 6500 TESTS FOR TELEMAC)
      IF (CHAINE(ILONG:ILONG).EQ.'1') GOTO 125
C
      DO 120 I=1,15
       IF (ILONG.EQ.LONPRO(I)) THEN
         IF (CHAINE(1:ILONG).EQ.MOTPRO(I)(1:ILONG)) THEN
C           IF 'AIDE' AND LNG=FRANCAIS, WILL EDIT THE HELP IF DOC
            IF (I.EQ.6 .AND. LNGINT .EQ. 0) AIDLNG = .TRUE.
            LANGUE = .TRUE.
            NUMERO = I
            ITYP   = 5
            GO TO 1000
         ENDIF
       ENDIF
120   CONTINUE
C
C  IF NOT: LOOKS FOR IT AMONG THE RESERVED WORDS FOR LANGUAGES
C          OTHER THAN FRENCH. (MAX NBLANG LANGUAGES AND NBLANG<=10)
C
C LNG IS THE EXTERNAL LANGUAGE PARAMETER (1 = FRENCH, 2 = ENGLISH ...)
C LNGINT IS THE LANGUAGE PARAMETER INTERNAL TO DAMOCLE
C (I.E. 0 = FRENCH, 1 = ENGLISH ...)
C
C  AIDLNG = NUMBER OF THE HELP LINE IN REQUESTED LANGUAGE
C
125   CONTINUE
      IF (NBLANG.GE.2) THEN
      DO 130 I=1,5
      DO 131 K=1,NBLANG-1
        IF (LGRUB(I)+1.EQ.ILONG) THEN
        MOTLNG = RUBPRO(I)(1:LGRUB(I))//LNGPRO(K)(1:1)
        IF (CHAINE(1:ILONG).EQ.MOTLNG(1:ILONG)) THEN
          NUMERO=VALNUM(I)
C
          IF (I.EQ.3 .AND. K.EQ.LNGINT) AIDLNG = .TRUE.
C
          ITYP = 5
C
C RETURNS LANGUE = .TRUE. ONLY FOR DAMOCLE KEYWORDS
C EXCEPT FOR 'AIDE', IN WHICH CASE LANGUE IS NOT USED
C NOT THE SAME TREATMENT DEPENDING ON THE SELECTED LANGUAGE
          IF (K.EQ.LNGINT.AND.I.GE.1.AND.I.LE.3) LANGUE = .TRUE.
          GO TO 1000
        ENDIF
        ENDIF
131   CONTINUE
130   CONTINUE
      ENDIF
C
C  6) ERROR : KEYWORD UNKNOWN
C
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
C
1000  CONTINUE
C
      RETURN
      END
C
C#######################################################################
C