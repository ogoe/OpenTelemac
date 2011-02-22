C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    MAIN ROUTINE OF THE DAMOCLES LIBRARY
!>             CALLED BY THE DAMOCLES EXECUTABLE (DAMOCLE.F)
!>             CALLED BY THE LNH COMPUTATIONAL CODES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ADRESS, DIMENS, DOC, GESTD, LLNG, LLU, MOTCAR, MOTCLE, MOTINT, MOTLOG, MOTREA, NFICDA, NFICMO, NMAX, TROUVE
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEFATT, DEFCAR, DEFINT, DEFLOG, DEFREA, I, INDIC, MOTATT, NBLANG, RETRY, SIZE, USRATT, USRCAR, USRINT, USRLOG, USRREA, UTINDX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DAMOC()
!>   </td></tr>
!>     </table>

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
!>    <td><center>                                        </center></td>
!>    <td> 01/05/1998                                              </td>
!>    <td> A. DESITTER (NAG)                                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 16/08/1994                                              </td>
!>    <td> J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE               </td>
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
!></td><td><--</td><td>TABLEAU DES ADRESSES DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DEFATT
!></td><td><--</td><td>TABLEAU DES SUBMITS PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFCAR
!></td><td><--</td><td>TABLEAU DES VALEURS CARACTERES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFINT
!></td><td><--</td><td>TABLEAU DES VALEURS ENTIERES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLOG
!></td><td><--</td><td>TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLU
!></td><td><-></td><td>NOMBRE DE VALEURS LUES POUR LE MOT CLE
!>    </td></tr>
!>          <tr><td>DEFREA
!></td><td><--</td><td>TABLEAU DES VALEURS REELLES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DIMENS
!></td><td><--</td><td>TABLEAU DES DIMENSIONS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DOC
!></td><td>--></td><td>LOGIQUE DE DOCUMENTATION DE LA SORTIE
!>                  = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
!>                  = FAUX : N'IMPRIME PAS L'AIDE
!>    </td></tr>
!>          <tr><td>ERREUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>GESTD
!></td><td>--></td><td>LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td><--</td><td>TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!>                  = 0 : PAS DE SUBMIT & NON TABLEAU
!>                  = 1 : PAS DE SUBMIT & TABLEAU
!>                  = 2 : AVEC   SUBMIT & NON TABLEAU
!>                  = 3 : AVEC   SUBMIT & TABLEAU
!>    </td></tr>
!>          <tr><td>INDX
!></td><td><-></td><td>INDEX DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>ITAI
!></td><td><-></td><td>TAILLE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LLNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LLU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
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
!>          <tr><td>MOTATT
!></td><td><--</td><td>TABLEAU DES SUBMITS
!>    </td></tr>
!>          <tr><td>MOTCAR
!></td><td><--</td><td>TABLEAU DES VALEURS CARACTERES
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td><--</td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTINT
!></td><td><--</td><td>TABLEAU DES VALEURS ENTIERES
!>    </td></tr>
!>          <tr><td>MOTLOG
!></td><td><--</td><td>TABLEAU DES VALEURS LOGIQUES
!>    </td></tr>
!>          <tr><td>MOTREA
!></td><td><--</td><td>TABLEAU DES VALEURS REELLES
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
!>          <tr><td>NFICMO
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES MOTS-CLES
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td><-></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
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
!>          <tr><td>PARAM
!></td><td><-></td><td>NOM DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td><--</td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td><--</td><td>INDICATEUR D'ETAT DES MOTS CLES
!>                  = 0 : AUCUNE VALEUR TROUVEE
!>                  = 1 : VALEUR PAR DEFAUT TROUVEE
!>                  = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)
!>                  = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)
!>                  = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE
!>                  = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE
!>                  = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)
!>                  = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)
!>                  = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR
!>                  =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR
!>    </td></tr>
!>          <tr><td>USRATT
!></td><td><--</td><td>TABLEAU DES SUBMITS A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRCAR
!></td><td><--</td><td>TABLEAU DES VALEURS CARACTERES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRINT
!></td><td><--</td><td>TABLEAU DES VALEURS ENTIERES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRLOG
!></td><td><--</td><td>TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRREA
!></td><td><--</td><td>TABLEAU DES VALEURS REELLES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>UTINDX
!></td><td><--</td><td>TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DAMOCLES
     &( ADRESS , DIMENS , NMAX   , DOC    , LLNG , LLU ,
     &  MOTINT , MOTREA , MOTLOG , MOTCAR ,
     &  MOTCLE , TROUVE , NFICMO , NFICDA , GESTD  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ADRESS         |<--| TABLEAU DES ADRESSES DES MOTS CLES
C| DEFATT         |<--| TABLEAU DES SUBMITS PAR DEFAUT
C| DEFCAR         |<--| TABLEAU DES VALEURS CARACTERES PAR DEFAUT
C| DEFINT         |<--| TABLEAU DES VALEURS ENTIERES PAR DEFAUT
C| DEFLOG         |<--| TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
C| DEFLU          |<->| NOMBRE DE VALEURS LUES POUR LE MOT CLE
C| DEFREA         |<--| TABLEAU DES VALEURS REELLES PAR DEFAUT
C| DIMENS         |<--| TABLEAU DES DIMENSIONS DES MOTS CLES
C| DOC            |-->| LOGIQUE DE DOCUMENTATION DE LA SORTIE
C|                |   | = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
C|                |   | = FAUX : N'IMPRIME PAS L'AIDE
C| ERREUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| GESTD          |-->| LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
C| INDIC          |<--| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
C|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
C|                |   | = 1 : PAS DE SUBMIT & TABLEAU
C|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
C|                |   | = 3 : AVEC   SUBMIT & TABLEAU
C| INDX           |<->| INDEX DU MOT CLE EN COURS
C| ITAI           |<->| TAILLE DU MOT CLE EN COURS
C| LLNG           |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LLU            |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LONGU          |<->| LONGUEUR DU MOT CLE EN COURS
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| MOTATT         |<--| TABLEAU DES SUBMITS
C| MOTCAR         |<--| TABLEAU DES VALEURS CARACTERES
C| MOTCLE         |<--| TABLEAU DES MOTS CLES ACTIFS
C| MOTINT         |<--| TABLEAU DES VALEURS ENTIERES
C| MOTLOG         |<--| TABLEAU DES VALEURS LOGIQUES
C| MOTREA         |<--| TABLEAU DES VALEURS REELLES
C| NBLANG         |-->| NOMBRE DE LANGUES CONNUES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
C| NFICMO         |-->| NUMERO DE CANAL DU FICHIER DES MOTS-CLES
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAX           |-->| TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
C| NMOT           |<->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
C| NTYP           |<->| TYPE DU MOT CLE EN COURS
C| PARAM          |<->| NOM DU MOT CLE EN COURS
C| RETOUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C| SIZE           |<--| TABLEAU DES LONGUEURS DES MOTS CLES
C| TROUVE         |<--| INDICATEUR D'ETAT DES MOTS CLES
C|                |   | = 0 : AUCUNE VALEUR TROUVEE
C|                |   | = 1 : VALEUR PAR DEFAUT TROUVEE
C|                |   | = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)
C|                |   | = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)
C|                |   | = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE
C|                |   | = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE
C|                |   | = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)
C|                |   | = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)
C|                |   | = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR
C|                |   | =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR
C| USRATT         |<--| TABLEAU DES SUBMITS A USAGE LOCAL
C| USRCAR         |<--| TABLEAU DES VALEURS CARACTERES A USAGE LOCAL
C| USRINT         |<--| TABLEAU DES VALEURS ENTIERES A USAGE LOCAL
C| USRLOG         |<--| TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL
C| USRREA         |<--| TABLEAU DES VALEURS REELLES A USAGE LOCAL
C| UTINDX         |<--| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER            ,INTENT(IN)    :: NMAX,LLU,NFICMO,NFICDA
      INTEGER            ,INTENT(INOUT) :: LLNG
      INTEGER            ,INTENT(OUT)   :: MOTINT(*),ADRESS(4,*)
      INTEGER            ,INTENT(OUT)   :: DIMENS(4,*),TROUVE(4,*)
      LOGICAL            ,INTENT(OUT)   :: MOTLOG(*)
      LOGICAL            ,INTENT(IN)    :: DOC, GESTD
      CHARACTER(LEN=72)  ,INTENT(OUT)   :: MOTCLE(4,*)
      CHARACTER(LEN=144) ,INTENT(OUT)   :: MOTCAR(*)
      DOUBLE PRECISION   ,INTENT(OUT)   :: MOTREA(*)
C
C     AUTOMATIC ARRAYS
C
      INTEGER            :: DEFINT(NMAX),USRINT(NMAX)
      INTEGER            :: SIZE(4,NMAX)
      INTEGER            :: INDIC(4,NMAX)
      LOGICAL            :: DEFLOG(NMAX),USRLOG(NMAX),UTINDX(4,NMAX)
      CHARACTER(LEN=144) :: MOTATT(4,NMAX),DEFATT(NMAX),USRATT(NMAX)
      CHARACTER(LEN=144) :: DEFCAR(NMAX),USRCAR(NMAX)
      DOUBLE PRECISION   :: DEFREA(NMAX),USRREA(NMAX)
C
      INTEGER,PARAMETER :: NBLANG = 2
      INTEGER :: RETRY,I
      RETRY = 0
C
C-----------------------------------------------------------------------
C
C     CALLS DAMOC
C
      CALL DAMOC( ADRESS , DIMENS , NMAX   , DOC    , LLNG   , LLU  ,
     &            MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     &            DEFINT , DEFREA , DEFLOG , DEFCAR , DEFATT ,
     &            USRINT , USRREA , USRLOG , USRCAR , USRATT ,
     &            MOTCLE , SIZE   , TROUVE , UTINDX , NFICMO , NFICDA ,
     &            INDIC  , GESTD  , NBLANG , RETRY )
C
      IF(RETRY.EQ.1) THEN
        REWIND(NFICMO)
        REWIND(NFICDA)
        DO I=1,10
          WRITE(LLU,*)
          WRITE(LLU,*) 'DAMOCLE: TRYING ANOTHER LANGUAGE'
        ENDDO
        LLNG=3-LLNG
        CALL DAMOC( ADRESS, DIMENS, NMAX   , DOC    , LLNG   , LLU,
     &              MOTINT, MOTREA, MOTLOG , MOTCAR , MOTATT ,
     &              DEFINT, DEFREA, DEFLOG , DEFCAR , DEFATT ,
     &              USRINT, USRREA, USRLOG , USRCAR , USRATT ,
     &              MOTCLE, SIZE  , TROUVE , UTINDX , NFICMO , NFICDA,
     &              INDIC , GESTD , NBLANG , RETRY )
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C