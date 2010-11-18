C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    CARRIES OUT A COMMAND PROVIDED IN THE DICTIONARY AND
!>             STEERING FILES : COMMAND = '&' + 3 LETTERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @note      DOCUMENTATION : COMMANDS &LIS, &ETA, &IND, &STO, &FIN
!>                             ARE ONLY CARRIED OUT IF EXECMD=.TRUE.
!>                             AND VUCMD(NB_CMB)=.TRUE.
!>                             COMMAND &DYN IS IGNORED IN THE STEERING FILE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ADRESS, DIMENS, DYNAM, EXECMD, ICOL, INDIC, LIGNE, MOTATT, MOTCAR, MOTCLE, MOTINT, MOTLOG, MOTREA, NFICDA, NMAXR, NMOT, SIZE, TROUVE, UTINDX, VUCMD
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCCHIE : NFIC<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCRARE : ERREUR, RETOUR
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORMA1, I, I1, IAD, ISIZE, K, L1, L2, N, TABUL, TRANS, TYP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LONGLU(), PREVAL()
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
!>    <td> 15/01/2008                                              </td>
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
!></td><td>--></td><td>TABLEAU DES ADRESSES DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DIMENS
!></td><td>--></td><td>TABLEAU DES DIMENSIONS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DYNAM
!></td><td><-></td><td>LOGIQUE POUR LE MODE DYNAMIQUE
!>    </td></tr>
!>          <tr><td>ERREUR
!></td><td>--></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>EXECMD
!></td><td>--></td><td>LOGIQUE D'ACTIVATION DES COMMANDES MEMORISEES
!>    </td></tr>
!>          <tr><td>ICOL
!></td><td><-></td><td>POSITION COURANTE DU POINTEUR DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!>                  = 0 : PAS DE SUBMIT & NON TABLEAU
!>                  = 1 : PAS DE SUBMIT & TABLEAU
!>                  = 2 : AVEC   SUBMIT & NON TABLEAU
!>                  = 3 : AVEC   SUBMIT & NON TABLEAU
!>    </td></tr>
!>          <tr><td>LIGNE
!></td><td>--></td><td>LIGNE EN COURS DE DECODAGE.
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>MOTATT
!></td><td>--></td><td>TABLEAU DES SUBMITS
!>    </td></tr>
!>          <tr><td>MOTCAR
!></td><td>--></td><td>TABLEAU DES VALEURS CARACTERES
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td>--></td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTINT
!></td><td>--></td><td>TABLEAU DES VALEURS ENTIERES
!>    </td></tr>
!>          <tr><td>MOTLOG
!></td><td>--></td><td>TABLEAU DES VALEURS LOGIQUES
!>    </td></tr>
!>          <tr><td>MOTREA
!></td><td>--></td><td>TABLEAU DES VALEURS REELLES
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
!>    </td></tr>
!>          <tr><td>NFICDA
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES DONNEES
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td>--></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>NMAXR
!></td><td>--></td><td>TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
!>    </td></tr>
!>          <tr><td>NMOT
!></td><td>--></td><td>TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td>--></td><td>INDICATEUR D'ETAT DES MOTS CLES
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
!>          <tr><td>UTINDX
!></td><td>--></td><td>TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!>    </td></tr>
!>          <tr><td>VUCMD
!></td><td><-></td><td>TABLEAU DE LOGIQUES (MEMORISATION DES CMDES)
!>    </td></tr>
!>          <tr><td>VUCMD(1)
!></td><td>---</td><td>LOGIQUE POUR &LIS
!>    </td></tr>
!>          <tr><td>VUCMD(2)
!></td><td>---</td><td>LOGIQUE POUR &ETA
!>    </td></tr>
!>          <tr><td>VUCMD(3)
!></td><td>---</td><td>LOGIQUE POUR &IND
!>    </td></tr>
!>          <tr><td>VUCMD(4)
!></td><td>---</td><td>LOGIQUE POUR &STO
!>    </td></tr>
!>          <tr><td>VUCMD(5)
!></td><td>---</td><td>LOGIQUE POUR &FIN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CMD
     &(ICOL   , LIGNE  , ADRESS , DIMENS , TROUVE , MOTCLE , NMOT ,
     & MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT , INDIC  , SIZE ,
     & UTINDX , DYNAM  , VUCMD  , EXECMD , NFICDA , NMAXR  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ADRESS         |-->| TABLEAU DES ADRESSES DES MOTS CLES
C| DIMENS         |-->| TABLEAU DES DIMENSIONS DES MOTS CLES
C| DYNAM          |<->| LOGIQUE POUR LE MODE DYNAMIQUE
C| ERREUR         |-->| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| EXECMD         |-->| LOGIQUE D'ACTIVATION DES COMMANDES MEMORISEES
C| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| INDIC          |-->| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
C|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
C|                |   | = 1 : PAS DE SUBMIT & TABLEAU
C|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
C|                |   | = 3 : AVEC   SUBMIT & NON TABLEAU
C| LIGNE          |-->| LIGNE EN COURS DE DECODAGE.
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| MOTATT         |-->| TABLEAU DES SUBMITS
C| MOTCAR         |-->| TABLEAU DES VALEURS CARACTERES
C| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
C| MOTINT         |-->| TABLEAU DES VALEURS ENTIERES
C| MOTLOG         |-->| TABLEAU DES VALEURS LOGIQUES
C| MOTREA         |-->| TABLEAU DES VALEURS REELLES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
C| NLIGN          |-->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
C| NMOT           |-->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
C| RETOUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
C| TROUVE         |-->| INDICATEUR D'ETAT DES MOTS CLES
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
C| UTINDX         |-->| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
C| VUCMD          |<->| TABLEAU DE LOGIQUES (MEMORISATION DES CMDES)
C| VUCMD(1)       |---| LOGIQUE POUR &LIS
C| VUCMD(2)       |---| LOGIQUE POUR &ETA
C| VUCMD(3)       |---| LOGIQUE POUR &IND
C| VUCMD(4)       |---| LOGIQUE POUR &STO
C| VUCMD(5)       |---| LOGIQUE POUR &FIN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C
      INTEGER          ICOL,ADRESS(4,*),DIMENS(4,*),NMOT(4),TROUVE(4,*)
      INTEGER          SIZE(4,*),INDIC(4,*),MOTINT(*),NFICDA,NMAXR(4)
      LOGICAL          MOTLOG(*),DYNAM,UTINDX(4,*),VUCMD(5),EXECMD
      CHARACTER*(*)    MOTCLE(4,*),LIGNE
      CHARACTER*144    MOTATT(4,*),MOTCAR(*)
      DOUBLE PRECISION MOTREA(*)
C
      INTEGER  PREVAL,LONGLU
      EXTERNAL PREVAL,LONGLU
C
      INTEGER          LNG,LU
      INTEGER          NLIGN,LONGLI
      INTEGER          NFIC
      LOGICAL          ERREUR , RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER          I1,IAD,L1,L2,TRANS,ISIZE,K,I,N
      CHARACTER*72     FORMA1(35)
      CHARACTER*6      TYP(4)
      CHARACTER*1      TABUL
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCCHIE / NFIC
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCRARE / ERREUR , RETOUR
C
      INTRINSIC CHAR
C
C-----------------------------------------------------------------------
C
      DATA TYP/'MOTINT','MOTREA','MOTLOG','MOTCAR'/
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
      TABUL = CHAR(9)
      I1 = ICOL + 1
C     ADDED BY JMH 15/01/2008: CASE WHERE LIGNE='NUL'
      IF(I1+2.GT.LONGLI) I1=1
C
C *********************** COMMAND &FIN **************************
C
      IF(LIGNE(I1:I1+2).EQ.'FIN'.OR.(EXECMD.AND.VUCMD(5))) THEN
           IF (.NOT.(EXECMD)) THEN
             VUCMD(5) = .TRUE.
             RETOUR = .TRUE.
             GO TO 1000
           ENDIF
           IF(LNG.EQ.1) THEN
             WRITE (LU,10)
 10          FORMAT(1X,/,1X,'FIN DU FICHIER POUR DAMOCLES',/)
           ELSEIF(LNG.EQ.2) THEN
             WRITE (LU,12)
 12          FORMAT(1X,/,1X,'END OF FILE FOR DAMOCLES',/)
           ENDIF
C
C *********************** COMMAND &ETA **************************
C
      ELSE IF (LIGNE(I1:I1+2).EQ.'ETA'.OR.(EXECMD.AND.VUCMD(2))) THEN
           IF (.NOT.(EXECMD)) THEN
             VUCMD(2) = .TRUE.
             GO TO 1000
           ENDIF
           IF(LNG.EQ.1) THEN
             WRITE (LU,11)
 11          FORMAT(1X,/,1X,'VALEUR DES MOTS-CLES :',/)
           ELSEIF(LNG.EQ.2) THEN
             WRITE (LU,13)
 13          FORMAT(1X,/,1X,'VALUES OF THE KEY-WORDS:',/)
           ENDIF
C
           FORMA1(1)= '(1X,A,/,1X,7HMOTINT(,1I3,2H)=,A,I9   ,/)'
           FORMA1(2)= '(1X,A,/,1X,7HMOTREA(,1I3,2H)=,A,G16.7,/)'
           FORMA1(3)= '(1X,A,/,1X,7HMOTLOG(,1I3,2H)=,A,L1   ,/)'
           FORMA1(4)= '(1X,A,/,1X,7HMOTCAR(,1I3,2H)=,A,A    ,/)'
           FORMA1(5)= '(1X,A,/,1X,7HMOTINT(,1I3,4H) = ,A,3H ; ,I9   ,/)'
           FORMA1(6)= '(1X,A,/,1X,7HMOTREA(,1I3,4H) = ,A,3H ; ,G16.7,/)'
           FORMA1(7)= '(1X,A,/,1X,7HMOTLOG(,1I3,4H) = ,A,3H ; ,L1   ,/)'
           FORMA1(8)= '(1X,A,/,1X,7HMOTCAR(,1I3,4H) = ,A,3H ; ,A    ,/)'
C
           DO 209 N =1,4
           DO 210 I = 1 , NMAXR(N)
           IF(UTINDX(N,I)) THEN
           ISIZE = SIZE(N,I)
           IF(TROUVE(N,I).GE.1) THEN
             DO 211 K=1,DIMENS(N,I)
             IAD = ADRESS(N,I) + K - 1
              IF (INDIC(N,I).LT.2) THEN
                TRANS=0
                MOTATT(N,IAD)=' '
                L1=1
              ELSE
                TRANS=4
                L1=LONGLU(MOTATT(N,IAD))
              ENDIF
C             IF (TROUVE(N,I).NE.3) THEN
               IF(N.EQ.1) THEN
                WRITE(LU,FORMA1(N+TRANS))
     &          MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),MOTINT(IAD)
               ELSE IF (N.EQ.2) THEN
                WRITE(LU,FORMA1(N+TRANS))
     &          MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),MOTREA(IAD)
               ELSE IF (N.EQ.3) THEN
                WRITE(LU,FORMA1(N+TRANS))
     &          MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),MOTLOG(IAD)
               ELSE IF (N.EQ.4) THEN
                L2 = LONGLU(MOTCAR(IAD))
                WRITE(LU,FORMA1(N+TRANS))
     &          MOTCLE(N,I)(1:ISIZE),IAD,MOTATT(N,IAD)(1:L1),
     &          MOTCAR(IAD)(1:L2)
               ENDIF
C             ENDIF
211        CONTINUE
           ELSE
             IF(LNG.EQ.1) THEN
               WRITE(LU,212) MOTCLE(N,I)(1:ISIZE)
212            FORMAT(1X,A,/,1X,'VALEUR NON TROUVEE',/,1X)
             ELSEIF(LNG.EQ.2) THEN
               WRITE(LU,213) MOTCLE(N,I)(1:ISIZE)
213            FORMAT(1X,A,/,1X,'VALUE NOT FOUND',/,1X)
             ENDIF
           ENDIF
C
           ENDIF
210        CONTINUE
209        CONTINUE
C
C *********************** COMMAND &IND **************************
C
      ELSE IF (LIGNE(I1:I1+2).EQ.'IND'.OR.(EXECMD.AND.VUCMD(3))) THEN
           IF (.NOT.(EXECMD)) THEN
             VUCMD(3) = .TRUE.
             GOTO 1000
           ENDIF
C
C DEFINITION OF THE FORMATS USED
C
           FORMA1(1)  = '(1X,7HMOTINT(,1I3,3H) =,A,I9   )'
           FORMA1(2)  = '(1X,7HMOTREA(,1I3,3H) =,A,G16.7)'
           FORMA1(3)  = '(1X,7HMOTLOG(,1I3,3H) =,A,L1   )'
           FORMA1(4)  = '(1X,7HMOTCAR(,1I3,3H) =,A,A    )'
           FORMA1(5)  = '(1X,7HMOTINT(,1I3,4H) = ,A,3H ; ,I9   )'
           FORMA1(6)  = '(1X,7HMOTREA(,1I3,4H) = ,A,3H ; ,G16.7)'
           FORMA1(7)  = '(1X,7HMOTLOG(,1I3,4H) = ,A,3H ; ,L1   )'
           FORMA1(8)  = '(1X,7HMOTCAR(,1I3,4H) = ,A,3H ; ,A    )'
           FORMA1(9)  = '(1X,24H!!! TABLEAU COMPACTE !!!)'
           FORMA1(10) = '(1X,23H!!! COMPACTED ARRAY !!!)'
           FORMA1(11) = '(1X,32HATTENTION ! TAILLE EN SORTIE = 0)'
           FORMA1(12) = '(1X,25HWARNING ! OUTPUT SIZE = 0)'
           FORMA1(13) = '(1X,9HTAILLE = ,I4)'
           FORMA1(14) = '(1X,8HSIZE  = ,I4)'
           FORMA1(15) = '(1X,30HVALEUR OPTIONNELLE NON TROUVEE)'
           FORMA1(16) = '(1X,24HOPTIONAL VALUE NOT FOUND)'
           FORMA1(17) = '(1X,25HVALEUR FORCEE NON TROUVEE)'
           FORMA1(18) = '(1X,22HFORCED VALUE NOT FOUND)'
           FORMA1(19) = '(1X,9HINDEX  = ,I4)'
           FORMA1(20) = '(1X,8HINDEX = ,I4)'
           FORMA1(21) = '(1X,18HVALEUR NON TROUVEE)'
           FORMA1(22) = '(1X,15HVALUE NOT FOUND)'
           FORMA1(23) = '(/,1X,22HVALEUR DES MOTS-CLES :,/)'
           FORMA1(24) = '(/,1X,25HVALUES OF THE KEY-WORDS :,/)'
           FORMA1(25) = '(1X,29HNOMBRE DE MOTS ENTIERS     = ,I4,'//
     &                  '10X,16H(DERNIER INDEX :,I4,1H))'
           FORMA1(26) = '(1X,32HNUMBER OF INTEGER   KEY WORDS = ,I4,'//
     &                  '10X,13H(LAST INDEX :,I4,1H))'
           FORMA1(27) = '(1X,29HNOMBRE DE MOTS REELS       = ,I4,'//
     &                  '10X,16H(DERNIER INDEX :,I4,1H))'
           FORMA1(28) = '(1X,32HNUMBER OF REAL      KEY WORDS = ,I4,'//
     &                  '10X,13H(LAST INDEX :,I4,1H))'
           FORMA1(29) = '(1X,29HNOMBRE DE MOTS LOGIQUES    = ,I4,'//
     &                  '10X,16H(DERNIER INDEX :,I4,1H))'
           FORMA1(30) = '(1X,32HNUMBER OF LOGICAL   KEY WORDS = ,I4,'//
     &                  '10X,13H(LAST INDEX :,I4,1H))'
           FORMA1(31) = '(1X,29HNOMBRE DE MOTS CARACTERES  = ,I4,'//
     &                  '10X,16H(DERNIER INDEX :,I4,1H))'
           FORMA1(32) = '(1X,32HNUMBER OF CHARACTER KEY WORDS = ,I4,'//
     &                  '10X,13H(LAST INDEX :,I4,1H))'
           FORMA1(33) = '(1X,29HNOMBRE TOTAL DE MOTS CLES  = ,I4)'
           FORMA1(34) = '(1X,32HTOTAL NUMBER OF KEY WORDS     = ,I4)'
           FORMA1(35) = '(/,1X,70(1H-),/,1X,A,/,1X,70(1H-))'
C
C TITLE
           WRITE(LU,FORMA1(22+LNG))
C
           WRITE(LU,*)' '
           WRITE(LU,*)'====================================='
           WRITE(LU,FORMA1(24+LNG)) NMOT(1),NMAXR(1)
           WRITE(LU,FORMA1(26+LNG)) NMOT(2),NMAXR(2)
           WRITE(LU,FORMA1(28+LNG)) NMOT(3),NMAXR(3)
           WRITE(LU,FORMA1(30+LNG)) NMOT(4),NMAXR(4)
           WRITE(LU,*)'-------------------------------------'
           WRITE(LU,FORMA1(32+LNG)) NMOT(1)+NMOT(2)+NMOT(3)+NMOT(4)
           WRITE(LU,*)'====================================='
           WRITE(LU,*)' '
C
           DO 409 N =1,4
           DO 410 I = 1 , NMAXR(N)
           IF(UTINDX(N,I)) THEN
           IF(TROUVE(N,I).GE.1.OR.DIMENS(N,I).GT.1) THEN
             WRITE(LU,FORMA1(35)) MOTCLE(N,I)(1:SIZE(N,I))
C COMPACTED ?
             IF (TROUVE(N,I).EQ.5) WRITE(LU,FORMA1(8+LNG))
C INDEX
             WRITE(LU,FORMA1(18+LNG)) I
C SIZE
             WRITE(LU,FORMA1(12+LNG)) DIMENS(N,I)
             IF (DIMENS(N,I).GT.1.AND.TROUVE(N,I).EQ.0.AND.DYNAM)
     &          WRITE(LU,FORMA1(10+LNG))
C
C TROUVE ?
             IF (TROUVE(N,I).EQ.3) WRITE(LU,FORMA1(14+LNG))
             IF (TROUVE(N,I).EQ.6) WRITE(LU,FORMA1(16+LNG))
C
C LINEFEED FOR PRESENTATION PURPOSES
             IF (DIMENS(N,I).GT.1) WRITE(LU,*) ' '
C
             DO 411 K=1,DIMENS(N,I)
              IAD = ADRESS(N,I) + K - 1
              IF (INDIC(N,I).GE.2) THEN
                TRANS = 4
                L1=LONGLU(MOTATT(N,IAD))
              ELSE
                TRANS = 0
                MOTATT(N,IAD)=' '
                L1 =1
              ENDIF
C
C             IF (TROUVE(N,I).NE.3) THEN
               IF(N.EQ.1) THEN
                    WRITE(LU,FORMA1(N+TRANS))
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTINT(IAD)
               ELSE IF (N.EQ.2) THEN
                    WRITE(LU,FORMA1(N+TRANS))
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTREA(IAD)
               ELSE IF (N.EQ.3) THEN
                    WRITE(LU,FORMA1(N+TRANS))
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTLOG(IAD)
               ELSE IF (N.EQ.4) THEN
                    L2 = LONGLU(MOTCAR(IAD))
                    WRITE(LU,FORMA1(N+TRANS))
     &                    IAD,MOTATT(N,IAD)(1:L1),MOTCAR(IAD)(1:L2)
               ENDIF
C             ENDIF
411        CONTINUE
           ELSE
              WRITE(LU,FORMA1(35)) MOTCLE(N,I)(1:SIZE(N,I))
              WRITE(LU,FORMA1(20+LNG))
              WRITE(LU,FORMA1(18+LNG)) I
              WRITE(LU,FORMA1(12+LNG)) DIMENS(N,I)
              WRITE(LU,*)' '
           ENDIF
C
           ENDIF
410        CONTINUE
409        CONTINUE
C
C *********************** COMMAND &LIS **************************
C
      ELSE IF (LIGNE(I1:I1+2).EQ.'LIS'.OR.(EXECMD.AND.VUCMD(1))) THEN
           IF (.NOT.(EXECMD)) THEN
             VUCMD(1) = .TRUE.
             GO TO 1000
           ENDIF
C FORMATS
           FORMA1(1) = '(/,1X,21HLISTE DES MOTS-CLES :,/)'
           FORMA1(2) = '(/,1X,16HKEY-WORDS LIST :,/)'
           FORMA1(3) = '(1X,12HDIMENSION : ,I3,5X,13HADRESSE DANS ,A,'//
     &                 '1X,1H:,1X,I3)'
           FORMA1(4) = '(1X,7HSIZE : ,I3,5X,10HADRESS IN ,A,'//
     &                 '1X,1H:,1X,I3)'
           FORMA1(5) =  '(1X,/,1X,A)'
C TITLE
           WRITE (LU,FORMA1(LNG))
C
           DO 309 N = 1 , 4
           DO 310 I = 1 , NMAXR(N)
C
           IF(UTINDX(N,I)) THEN
             IAD = ADRESS(N,I)
             WRITE (LU,FORMA1(5)) MOTCLE(N,I)(1:SIZE(N,I))
             IF (DIMENS(N,I).GT.1.AND.TROUVE(N,I).EQ.0.AND.DYNAM) THEN
               WRITE (LU,FORMA1(2+LNG)) 0,TYP(N),IAD
             ELSE
               WRITE (LU,FORMA1(2+LNG)) DIMENS(N,I),TYP(N),IAD
             ENDIF
           ENDIF
310        CONTINUE
309        CONTINUE
C
C *********************** COMMAND &DOC **************************
C
      ELSE IF ( LIGNE(I1:I1+2).EQ.'DOC' ) THEN
C
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'COMMANDE &DOC SUPPRIMEE DANS CETTE VERSION'
       ELSEIF(LNG.EQ.2) THEN
         WRITE(LU,*) 'COMMAND &DOC HAS BEEN SUPPRESSED IN THIS RELEASE'
       ENDIF
C
C *********************** COMMAND &STO **************************
C
      ELSE IF (LIGNE(I1:I1+2).EQ.'STO'.OR.(EXECMD.AND.VUCMD(4))) THEN
           IF (.NOT.(EXECMD)) THEN
             VUCMD(4) = .TRUE.
             RETOUR=.TRUE.
             GO TO 1000
           ENDIF
           IF(LNG.EQ.1) THEN
             WRITE (LU,1113)
1113         FORMAT(1X,/,1X,'ARRET DE DAMOCLES PAR LA COMMANDE &STO')
           ELSEIF(LNG.EQ.2) THEN
             WRITE (LU,1114)
1114         FORMAT(1X,/,1X,'DAMOCLES STOPPED BY COMMAND &STO')
           ENDIF
           STOP 'FIN DE DAMOCLES 10'
C
C *********************** COMMAND &DYN **************************
C
      ELSEIF ( LIGNE(I1:I1+2).EQ.'DYN' ) THEN
           IF (NFIC.EQ.NFICDA) THEN
             IF (LNG.EQ.1) THEN
               WRITE(LU,*)'COMMANDE &DYN DU FICHIER CAS IGNOREE !!'
             ELSEIF(LNG.EQ.2) THEN
               WRITE(LU,*)'WARNING : INSTRUCTION &DYN FROM STEERING ',
     &                    'FILE HAS BEEN IGNORED !!'
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
C
C     //// SEEKS THE FIRST WHITE CHARACTER FOLLOWING & ////
C
 1000 CONTINUE
      ICOL = PREVAL (I1+1,LIGNE,' ',TABUL,' ')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C