C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    STORES IN ARRAYS MOTINT, MOTREA, MOTLOG, MOTCAR AND
!>             MOTATT THE VALUES READ FOR A KEYWORD.<br>
!>             DISCARDS THE WORDS RETURNED BY EDAMOX IN THE DATA FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ADRESS, DEFATT, DEFCAR, DEFINT, DEFLOG, DEFREA, DIMENS, INDIC, LUIGN, MOTATT, MOTCAR, MOTCLE, MOTINT, MOTLOG, MOTREA, NMAX, OFFSET, SIZE, UTINDX
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCNGE : INDX, NTYP, ITAI, LONGU, NMOT, DEFLU<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCNGEC : PARAM
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
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
!>    <td><center> 5.1                                    </center></td>
!>    <td> 16/08/1994                                              </td>
!>    <td> L. LEGUE                                                </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 14/12/1993                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH) 30.87.78.70                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ADRESS
!></td><td><-></td><td>TABLEAU DES ADRESSES DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DEFATT
!></td><td><-></td><td>TABLEAU DES SUBMITS PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFCAR
!></td><td><-></td><td>TABLEAU DES VALEURS CARACTERES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFINT
!></td><td><-></td><td>TABLEAU DES VALEURS ENTIERES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLOG
!></td><td><-></td><td>TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLU
!></td><td><-></td><td>NOMBRE DE VALEURS LUES POUR LE MOT CLE
!>    </td></tr>
!>          <tr><td>DEFREA
!></td><td><-></td><td>TABLEAU DES VALEURS REELLES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DIMENS
!></td><td><-></td><td>TABLEAU DES DIMENSIONS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td><-></td><td>TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!>                  = 0 : PAS DE SUBMIT & NON TABLEAU
!>                  = 1 : PAS DE SUBMIT & TABLEAU
!>                  = 2 : AVEC   SUBMIT & NON TABLEAU
!>                  = 3 : AVEC   SUBMIT & NON TABLEAU
!>    </td></tr>
!>          <tr><td>INDX
!></td><td><-></td><td>INDEX DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>ITAI
!></td><td><-></td><td>TAILLE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LONGU
!></td><td><-></td><td>LONGUEUR DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>LUIGN
!></td><td>--></td><td>LOGIQUE POUR LES MOTS A NE PAS CLASSER
!>    </td></tr>
!>          <tr><td>MOTATT
!></td><td><-></td><td>TABLEAU DES SUBMITS
!>    </td></tr>
!>          <tr><td>MOTCAR
!></td><td><-></td><td>TABLEAU DES VALEURS CARACTERES
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td>--></td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTINT
!></td><td><-></td><td>TABLEAU DES VALEURS ENTIERES
!>    </td></tr>
!>          <tr><td>MOTLOG
!></td><td><-></td><td>TABLEAU DES VALEURS LOGIQUES
!>    </td></tr>
!>          <tr><td>MOTREA
!></td><td><-></td><td>TABLEAU DES VALEURS REELLES
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td>--></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>NMAX
!></td><td>--></td><td>TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
!>    </td></tr>
!>          <tr><td>NMOT
!></td><td><-></td><td>TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!>    </td></tr>
!>          <tr><td>NTYP
!></td><td><-></td><td>TYPE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>OFFSET
!></td><td><-></td><td>TABLEAUX DES PROCHAINES ADRESSES LIBRES
!>    </td></tr>
!>          <tr><td>PARAM
!></td><td><-></td><td>NOM DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td><-></td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>UTINDX
!></td><td><-></td><td>TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CLASSE
     &(DIMENS , SIZE   , MOTCLE , UTINDX , NMAX   ,
     & OFFSET , ADRESS , INDIC  , LUIGN  ,
     & MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     & DEFCAR , DEFINT , DEFLOG , DEFREA , DEFATT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ADRESS         |<->| TABLEAU DES ADRESSES DES MOTS CLES
C| DEFATT         |<->| TABLEAU DES SUBMITS PAR DEFAUT
C| DEFCAR         |<->| TABLEAU DES VALEURS CARACTERES PAR DEFAUT
C| DEFINT         |<->| TABLEAU DES VALEURS ENTIERES PAR DEFAUT
C| DEFLOG         |<->| TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
C| DEFLU          |<->| NOMBRE DE VALEURS LUES POUR LE MOT CLE
C| DEFREA         |<->| TABLEAU DES VALEURS REELLES PAR DEFAUT
C| DIMENS         |<->| TABLEAU DES DIMENSIONS DES MOTS CLES
C| INDIC          |<->| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
C|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
C|                |   | = 1 : PAS DE SUBMIT & TABLEAU
C|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
C|                |   | = 3 : AVEC   SUBMIT & NON TABLEAU
C| INDX           |<->| INDEX DU MOT CLE EN COURS
C| ITAI           |<->| TAILLE DU MOT CLE EN COURS
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LONGU          |<->| LONGUEUR DU MOT CLE EN COURS
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| LUIGN          |-->| LOGIQUE POUR LES MOTS A NE PAS CLASSER
C| MOTATT         |<->| TABLEAU DES SUBMITS
C| MOTCAR         |<->| TABLEAU DES VALEURS CARACTERES
C| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
C| MOTINT         |<->| TABLEAU DES VALEURS ENTIERES
C| MOTLOG         |<->| TABLEAU DES VALEURS LOGIQUES
C| MOTREA         |<->| TABLEAU DES VALEURS REELLES
C| NLIGN          |-->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAX           |-->| TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
C| NMOT           |<->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
C| NTYP           |<->| TYPE DU MOT CLE EN COURS
C| OFFSET         |<->| TABLEAUX DES PROCHAINES ADRESSES LIBRES
C| PARAM          |<->| NOM DU MOT CLE EN COURS
C| SIZE           |<->| TABLEAU DES LONGUEURS DES MOTS CLES
C| UTINDX         |<->| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER          LNG,LU
      INTEGER          INDX,NTYP,ITAI,LONGU,NMOT(4),DEFLU
      INTEGER          NLIGN,LONGLI
      CHARACTER*72     PARAM
C
      INTEGER          NMAX,MOTINT(*),ADRESS(4,*),DIMENS(4,*)
      INTEGER          SIZE(4,*),OFFSET(4),DEFINT(*),INDIC(4,*)
      LOGICAL          UTINDX(4,*),DEFLOG(*),MOTLOG(*),LUIGN
      CHARACTER*72     MOTCLE(4,*)
      CHARACTER*144    MOTCAR(*),DEFCAR(*)
      CHARACTER*144    MOTATT(4,*),DEFATT(*)
      DOUBLE PRECISION MOTREA(*),DEFREA(*)
C
C-----------------------------------------------------------------------
C
      INTEGER          I
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCNGE  / INDX,NTYP,ITAI,LONGU,NMOT,DEFLU
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCNGEC / PARAM
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
                IF (LUIGN) GO TO 1600
C
C               GLOBAL TREATMENT OF THE KEYWORD
C
                IF (INDX .GT. NMAX) THEN
                  WRITE(LU,*) '****************************************'
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'ERREUR A LA LIGNE :',NLIGN,
     &                          ' DU DICTIONNAIRE'
                    WRITE(LU,*) 'INDEX INVALIDE : ',INDX,' MAX = ',NMAX
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'ERROR AT LINE:',NLIGN,
     &                          ' OF THE DICTIONARY'
                    WRITE(LU,*) 'INVALID INDEX: ',INDX,' MAX = ',NMAX
                  ENDIF
                  WRITE(LU,*) '****************************************'
                  STOP 'ERREUR DAMOCLES 6'
                ENDIF
C
                IF (NMOT(NTYP) .GT. NMAX) THEN
                  WRITE(LU,*)'*****************************************'
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'ERREUR A LA LIGNE :',NLIGN,
     &                          ' DU DICTIONNAIRE'
                    WRITE(LU,*)'TROP DE MOTS CLES. MAXIMUM : ',NMAX
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'ERROR AT LINE:',NLIGN,
     &                          ' OF THE DICTIONARY'
                    WRITE(LU,*) 'TOO MANY KEY-WORDS, MAXIMUM : ',NMAX
                  ENDIF
                  WRITE(LU,*)'*****************************************'
                  STOP 'ERREUR DAMOCLES 7'
                ENDIF
C
C REDUNDANT WITH LUIGN? KEPT BY DEFAULT - TO BE CHECKED
                IF (INDX .LE. 0) GO TO 1600
C
                IF (UTINDX(NTYP,INDX)) THEN
                  WRITE(LU,*)'*****************************'
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'ERREUR A LA LIGNE : ',NLIGN
                    WRITE(LU,*) 'L''INDEX  : ',INDX,
     &             ' A DEJA ETE UTILISE POUR LE TYPE : ',NTYP
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'ERROR AT LINE: ',NLIGN
                    WRITE(LU,*) 'THE INDEX: ',INDX,
     &             ' IS USED TWO TIMES FOR THE TYPE : ',NTYP
                  ENDIF
                  WRITE(LU,*)'*****************************'
                  STOP 'ERREUR DAMOCLES 8'
                ELSE
                   UTINDX(NTYP,INDX) = .TRUE.
                ENDIF
C
                IF (ITAI .LE. 0) THEN
                   ITAI = 1
                ELSE
C PREVENTS DYNAMIC ALLOCATION FOR SOMETHING ELSE THAN AN ARRAY
                   INDIC(NTYP,INDX)=INDIC(NTYP,INDX)+1
                ENDIF
C
C ADDITION CF JMH - ISSUES A WARNING FOR ESTET - N3S DICO FILES
C WHEN THE DEFAULT VALUES ARE DEFINED IN INSUFFICIENT NUMBER
C COMPARED TO THE DIMENSIONS
C
               IF(DEFLU.GT.0.AND.DEFLU.NE.ITAI) THEN
                  WRITE(LU,*) ' '
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*)'ATTENTION ! A LA LIGNE ',NLIGN,
     &                         ' DU DICTIONNAIRE :'
                    WRITE(LU,*)'LE NOMBRE DE VALEURS PAR DEFAUT ',
     &                          DEFLU,' EST DIFFERENT DE LA TAILLE ',
     &                          'ANNONCEE ',ITAI
                  ELSEIF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'WARNING !  AT LINE ',NLIGN,
     &                          ' OF THE DICTIONARY :'
                    WRITE(LU,*) 'NUMBER OF DEFAULT VALUES ',DEFLU,
     &                           ' IS DIFFERENT FROM THE SIZE ',ITAI
                  ENDIF
                  WRITE(LU,*) ' '
               ENDIF
C
                IF (DEFLU .EQ. 0) THEN
                   IF     (NTYP .EQ. 1) THEN
                      DEFINT(1) = 0
                   ELSEIF (NTYP .EQ. 2) THEN
                      DEFREA(1) = 0.0
                   ELSEIF (NTYP .EQ. 3)THEN
                      DEFLOG(1) = .FALSE.
                   ELSEIF (NTYP .EQ. 4) THEN
                      DEFCAR(1) = ' '
                   ENDIF
                ENDIF
C
                IF (ITAI .NE. DEFLU) THEN
                   IF (ITAI .GT. DEFLU) THEN
                      DO 100 I = DEFLU + 1 , ITAI
                         IF     (NTYP .EQ. 1) THEN
                            DEFINT(I) = DEFINT(MAX(1,DEFLU))
                         ELSEIF (NTYP .EQ. 2) THEN
                            DEFREA(I) = DEFREA(MAX(1,DEFLU))
                         ELSEIF (NTYP .EQ. 3) THEN
                            DEFLOG(I) = DEFLOG(MAX(1,DEFLU))
                         ELSEIF (NTYP .EQ. 4) THEN
                            DEFCAR(I) = DEFCAR(MAX(1,DEFLU))
                         ENDIF
C                        DEFATT(NYTP,I) = DEFATT(NYTP,MAX(1,DEFLU))
 100                  CONTINUE
                   ENDIF
                   DEFLU = ITAI
                ENDIF
C
C   STORES THE KEYWORD ATTRIBUTES IN THE ARRAYS
C   NUMBER OF KEYWORDS OF TYPE NTYP
C
                NMOT(NTYP) = NMOT(NTYP) + 1
C
C   NEXT FREE ADDRESS FOR THE KEYWORD OF TYPE NTYP
C
                ADRESS(NTYP,INDX) = OFFSET(NTYP)
C
C   STORED KEYWORD
C
                MOTCLE(NTYP,INDX) = PARAM(1:LONGU)
C
C   NUNBER OF VALUES ASSOCIATED WITH THE KEYWORD OF TYPE NTYP
C
                DIMENS(NTYP,INDX) = ITAI
C
C   LENGTH OF THE KEYWORD (CHARACTERS)
C
                SIZE(NTYP,INDX) = LONGU
C
C   STORES THE VALUES IN THE ARRAYS
C
                IF (((ADRESS(NTYP,INDX)+ITAI-1) .GT. NMAX)
     &             .OR. (OFFSET(NTYP) .GT. NMAX)) THEN
                     IF(LNG.EQ.1) THEN
                       WRITE(LU,*) 'ADRESSE SUPERIEURE A NMAX = ',NMAX
                       WRITE(LU,*) 'TROP DE VALEURS DE TYPE : ',NTYP
     &                             ,' DECLAREES.'
                       WRITE(LU,*) 'ARRET AU MOT CLE D''INDEX : ',INDX
                     ELSEIF(LNG.EQ.2) THEN
                       WRITE(LU,*) 'ADRESS GREATER THAN NMAX = ',NMAX
                       WRITE(LU,*) 'TOO MANY VALUES OF TYPE : ',NTYP
     &                             ,' DECLARED.'
                       WRITE(LU,*) 'STOP AT KEY-WORD OF INDEX: ',INDX
                     ENDIF
                     STOP 'ERREUR DAMOCLES 9'
                   ENDIF
C
                DO 200 I = 1 , ITAI
                   IF (NTYP .EQ. 1) THEN
                      MOTINT(ADRESS(NTYP,INDX)+I-1) = DEFINT(I)
                   ELSE IF (NTYP .EQ. 2) THEN
                      MOTREA(ADRESS(NTYP,INDX)+I-1) = DEFREA(I)
                   ELSE IF (NTYP .EQ. 3) THEN
                      MOTLOG(ADRESS(NTYP,INDX)+I-1) = DEFLOG(I)
                   ELSE IF (NTYP .EQ. 4) THEN
                      MOTCAR(ADRESS(NTYP,INDX)+I-1) = DEFCAR(I)
                   ENDIF
                   IF (INDIC(NTYP,INDX).GE.2)
     &                 MOTATT(NTYP,ADRESS(NTYP,INDX)+I-1) = DEFATT(I)
 200            CONTINUE
C
C   UPDATES THE NEXT FREE ADDRESS
C
                OFFSET(NTYP) = OFFSET(NTYP) + ITAI
C
C   INITIALISES THE TEMPORARY VARIABLES
C
1600            CONTINUE
                PARAM  = ' '
                LONGU  = 0
                NTYP   = -100
                INDX   = 123456
                ITAI   = -100
                DEFLU  = 0
C
C-----------------------------------------------------------------------
C
       RETURN
       END

C
C#######################################################################
C