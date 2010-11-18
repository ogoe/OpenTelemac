C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE INTEGRATION STEP OF THE SOURCE TERMS USING
!>                A SCHEME WITH VARIABLE DEGREE OF IMPLICITATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALFABJ, ALFARO, ALFLTA, ALPHA, BDISPB, BDSSPB, BETA, BETAIH, BETAM, BINVEN, BORETG, CDRAG, CFROT1, CIMPLI, CMOUT1, CMOUT2, COEFHS, COEFNL, COSTET, DDC, DECAL, DEPTH, DFREQ, DF_LIM, DTSI, EM2SIH, F, F1, FMOY, FREQ, GAM2RO, GAMARO, GAMATG, GAMBJ1, GAMBJ2, GRAVIT, IANGNL, IDISRO, IEXPRO, IFRBJ, IFRIH, IFRRO, IFRTG, IHMBJ, INDIC, IQBBJ, IWHTG, KSPB, LIMIT, NBD, NBOR, NDTBRK, NF, NOMVEB, NOMVEF, NP, NPLAN, NPOIN2, NPTFR, NSITS, NVEB, NVEF, PROINF, QINDI, RAISF, RFMLTA, ROAIR, ROEAU, SBREK, SFROT, SINTET, SMOUT, STRIA, STRIF, SVENT, TAILF, TAUWAV, TAUX1, TAUX2, TAUX3, TAUX4, TAUX5, TAUX6, TAUX7, TETA, TNEW, TOLD, TPROP, TRA01, TSDER, TSTOT, TV1, TV2, TWNEW, TWOLD, U1, U2, USNEW, USOLD, V1, V2, VARIAN, VENSTA, VENT, VENTX, VENTY, VX_CTE, VY_CTE, X, XDTBRK, XK, XKAPPA, XKMOY, XRELV, Y, YRELV, Z0NEW, Z0OLD, ZVENT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, AUX2, AUX3, AUX4, AUXI, CHDON, COEF, DEUPI, DFMAX, DTN, FM1, FM2, HM0, HM0MAX, IDT, IFCAR, IFF, IP, ISITS, JP, MF1, MF2, MFMAX, NPOIN3, NPOIN4, SUM, TDEB, TFIN, USMIN, VITMIN, VITVEN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ANAVEN(), FPREAD(), FREM01(), FREM02(), FREMOY(), FREPIC(), KMOYEN(), NOUDON(), QBREK1(), QBREK2(), QBREK3(), QBREK4(), QFROT1(), QMOUT1(), QNLIN1(), QTRIA1(), QTRIA2(), QWIND1(), QWIND2(), STRESS(), TOTNRJ(), USTAR1(), USTAR2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td> 16/12/2008
!> </td><td> JMH
!> </td><td> BETA HAS BEEN ADDED TO THE LIST OF ARGUMENTS AND
!>           IS PASSED LAST IN QBREK1,2,3,4 (INSTEAD OF TAUX1)
!> </td></tr>
!>      <tr>
!>      <td><center> 5.0                                       </center>
!> </td><td> 25/08/2000
!> </td><td>
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.2                                       </center>
!> </td><td> 07/11/96
!> </td><td> M. BENOIT
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 26/03/95
!> </td><td> M. BENOIT
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALFABJ
!></td><td>--></td><td>MODELE DEFERLEMENT BJ : CONSTANTE ALPHA
!>    </td></tr>
!>          <tr><td>ALFARO
!></td><td>--></td><td>MODELE DEFERLEMENT RO : CONSTANTE ALPHA
!>    </td></tr>
!>          <tr><td>ALFLTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ALPHA
!></td><td>--></td><td>CONSTANTE DE LA LOI DE CHARNOCK
!>    </td></tr>
!>          <tr><td>BDISPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BDSSPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETAIH
!></td><td>--></td><td>MODELE DEFERLEMENT IH : CONSTANTE BETA
!>    </td></tr>
!>          <tr><td>BETAM
!></td><td>--></td><td>CONSTANTE BETAMAX DE LA FORMULE DU VENT
!>    </td></tr>
!>          <tr><td>BINVEN
!></td><td>--></td><td>BINAIRE DU FICHIER DE VENT EN ENTREE
!>    </td></tr>
!>          <tr><td>BORETG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : CONSTANTE B
!>    </td></tr>
!>          <tr><td>CDRAG
!></td><td>--></td><td>COEFFICIENT DE TRAINEE
!>    </td></tr>
!>          <tr><td>CFROT1
!></td><td>--></td><td>CONSTANTE POUR LE TERME DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>CIMPLI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CMOUT1
!></td><td>--></td><td>CONSTANTE 1 POUR LE TERME DE MOUTONNEMENT
!>    </td></tr>
!>          <tr><td>CMOUT2
!></td><td>--></td><td>CONSTANTE 2 POUR LE TERME DE MOUTONNEMENT
!>    </td></tr>
!>          <tr><td>COEFHS
!></td><td>--></td><td>COEFFICIENT LIMITATEUR DE LA HAUTEUR HS
!>    </td></tr>
!>          <tr><td>COEFNL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFNL(
!></td><td>--></td><td>COEFFICIENTS POUR QUADRUPLETS DIA
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>VECTEUR DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DE DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>DECAL
!></td><td>--></td><td>CONSTANTE DE DECALAGE DE CROISSANCE VENT
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS (METRES)
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCE
!>    </td></tr>
!>          <tr><td>DF_LIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTSI
!></td><td>--></td><td>PAS DE TEMPS D'INTEGRATION (SECONDES)
!>    </td></tr>
!>          <tr><td>EM2SIH
!></td><td>--></td><td>MODELE DEFERLEMENT IH : CONSTANTE M2*
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>---</td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>F1
!></td><td>--></td><td>PREMIERE FREQUENCE DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>FMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GAM2RO
!></td><td>--></td><td>MODELE DEFERLEMENT RO : CONSTANTE GAMMA2
!>    </td></tr>
!>          <tr><td>GAMARO
!></td><td>--></td><td>MODELE DEFERLEMENT RO : CONSTANTE GAMMA
!>    </td></tr>
!>          <tr><td>GAMATG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : CONSTANTE GAMMA
!>    </td></tr>
!>          <tr><td>GAMBJ1
!></td><td>--></td><td>MODELE DEFERLEMENT BJ : CONSTANTE GAMMA1
!>    </td></tr>
!>          <tr><td>GAMBJ2
!></td><td>--></td><td>MODELE DEFERLEMENT BJ : CONSTANTE GAMMA2
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>IANGNL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IANGNL(
!></td><td>--></td><td>INDICES ANGULAIRES POUR QUADRUPLETS DIA
!>    </td></tr>
!>          <tr><td>IDISRO
!></td><td>--></td><td>MODELE DEFERLEMENT RO : DISTRIBUTION HOULE
!>    </td></tr>
!>          <tr><td>IEXPRO
!></td><td>--></td><td>MODELE DEFERLEMENT RO : EXPOSANT N
!>    </td></tr>
!>          <tr><td>IFRBJ
!></td><td>--></td><td>MODELE DEFERLEMENT BJ : MODE CALCUL DE FREQ
!>    </td></tr>
!>          <tr><td>IFRIH
!></td><td>--></td><td>MODELE DEFERLEMENT IH : MODE CALCUL DE FREQ
!>    </td></tr>
!>          <tr><td>IFRRO
!></td><td>--></td><td>MODELE DEFERLEMENT RO : MODE CALCUL DE FREQ
!>    </td></tr>
!>          <tr><td>IFRTG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : MODE CALCUL DE FREQ
!>    </td></tr>
!>          <tr><td>IHMBJ
!></td><td>--></td><td>MODELE DEFERLEMENT BJ : MODE CALCUL DE HM
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TYPE DE FORMAT DE LECTURE
!>    </td></tr>
!>          <tr><td>IQBBJ
!></td><td>--></td><td>MODELE DEFERLEMENT BJ : MODE CALCUL DE QB
!>    </td></tr>
!>          <tr><td>IWHTG
!></td><td>--></td><td>MODELE DEFERLEMENT TG : MODE CALCUL DE W(H)
!>    </td></tr>
!>          <tr><td>KSPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR(
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>NDTBRK
!></td><td>--></td><td>NOMBRE DE SOUS-PAS DE TEMPS DE DEFERLEMENT
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NOMVEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMVEF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NP
!></td><td>--></td><td>NOMBRE DE POINTS DU CHAMP DE VENT
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>NSITS
!></td><td>--></td><td>NOMBRE DE PAS DE TEMPS D'INTEGRATION
!>    </td></tr>
!>          <tr><td>NVEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVEF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PROINF
!></td><td>--></td><td>INDICATEUR DE PROFONDEUR INFINIE
!>    </td></tr>
!>          <tr><td>QINDI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE POUR DISCRETISATION
!>    </td></tr>
!>          <tr><td>RFMLTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ROAIR
!></td><td>--></td><td>MASSE VOLUMIQUE DE L AIR
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>MASSE VOLUMIQUE DE L EAU
!>    </td></tr>
!>          <tr><td>SBREK
!></td><td>--></td><td>INDICATEUR DE TYPE DE TERME DEFERLEMENT
!>    </td></tr>
!>          <tr><td>SFROT
!></td><td>--></td><td>INDICATEUR DE TYPE DE TERME FROTTEMENT
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>VECTEUR DES   SINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>SMOUT
!></td><td>--></td><td>INDICATEUR DE TYPE DE TERME MOUTONNEMENT
!>    </td></tr>
!>          <tr><td>STRIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STRIF
!></td><td>--></td><td>INDICATEUR DE TYPE DE TERME INTERACTIONS
!>    </td></tr>
!>          <tr><td>SVENT
!></td><td>--></td><td>INDICATEUR DE TYPE DE TERME INPUT PAR VENT
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE DU SPECTRE
!>    </td></tr>
!>          <tr><td>TAUWAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUWAV(
!></td><td>---</td><td>TABLEAU DES CONTRAINTES DUES A LA HOULE
!>    </td></tr>
!>          <tr><td>TAUX1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX2(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX3(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX4(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX5(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX6(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX7(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>VECTEUR DES DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TPROP
!></td><td>--></td><td>DATE DE FIN DE L'ETAPE D'INTEGRATION
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSDER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSTOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TV1
!></td><td><-></td><td>DATE DU CHAMP DE CHAMP 1
!>    </td></tr>
!>          <tr><td>TV2
!></td><td><-></td><td>DATE DU CHAMP DE CHAMP 2
!>    </td></tr>
!>          <tr><td>TWNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TWNEW(
!></td><td>---</td><td>DIRECTION DU VENT A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>TWOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TWOLD(
!></td><td>---</td><td>DIRECTION DU VENT A L'INSTANT N
!>    </td></tr>
!>          <tr><td>U1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U1(
!></td><td>---</td><td>TABLEAU DES COMP. OUEST-EST DE VENT 1
!>    </td></tr>
!>          <tr><td>U2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2(
!></td><td>---</td><td>TABLEAU DES COMP. OUEST-EST DE VENT 2
!>    </td></tr>
!>          <tr><td>USNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USNEW(
!></td><td>---</td><td>VITESSE DE FROTTEMENT A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>USOLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USOLD(
!></td><td>---</td><td>VITESSE DE FROTTEMENT A L'INSTANT N
!>    </td></tr>
!>          <tr><td>V1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V1(
!></td><td>---</td><td>TABLEAU DES COMP. SUD-NORD  DE VENT 1
!>    </td></tr>
!>          <tr><td>V2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2(
!></td><td>---</td><td>TABLEAU DES COMP. SUD-NORD  DE VENT 2
!>    </td></tr>
!>          <tr><td>VARIAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENSTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>INDICATEUR DE PRISE EN COMPTE DE VENT
!>    </td></tr>
!>          <tr><td>VENTX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENTX(
!></td><td>---</td><td>TABLEAU DE VENT (COMP. OUEST-EST)
!>    </td></tr>
!>          <tr><td>VENTY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENTY(
!></td><td>---</td><td>TABLEAU DE VENT (COMP. SUD-NORD)
!>    </td></tr>
!>          <tr><td>VX_CTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VY_CTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X(
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS MAILLAGE
!>    </td></tr>
!>          <tr><td>XDTBRK
!></td><td>--></td><td>PAS DE TEMPS POUR LE DEFERLEMENT
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE
!>    </td></tr>
!>          <tr><td>XKAPPA
!></td><td>--></td><td>CONSTANTE DE VON KARMAN
!>    </td></tr>
!>          <tr><td>XKMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XRELV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XRELV(
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y(
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS MAILLAGE
!>    </td></tr>
!>          <tr><td>YRELV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YRELV(
!></td><td>--></td><td>TABLEAU DES ORDONNEES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>Z0NEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z0NEW(
!></td><td>---</td><td>LONGUEUR DE RUGOSITE L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>Z0OLD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z0OLD(
!></td><td>---</td><td>LONGUEUR DE RUGOSITE L'INSTANT N
!>    </td></tr>
!>          <tr><td>ZVENT
!></td><td>--></td><td>COTE A LAQUELLE EST MESURE LE VENT (M)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SEMIMP
     &( F     , XK    , FREQ  , DFREQ , DEPTH , VENTX , VENTY , X     ,
     &  Y     , NVEB  , NVEF  , NBOR  , NPTFR , DDC   , TV1   , TV2   ,
     &  NP    ,
     &  XRELV , YRELV , U1    , V1    , U2    , V2    , TETA  , SINTET,
     &  COSTET, INDIC , TAILF , RAISF , GRAVIT, CFROT1, CMOUT1, CMOUT2,
     &  TPROP , DTSI  , ROAIR , ROEAU , XKAPPA, BETAM , DECAL , CDRAG ,
     &  ALPHA , ZVENT , NF    , NPLAN , NPOIN2, IANGNL, COEFNL, F1    ,
     &  NSITS , SMOUT , SFROT , SVENT , STRIF , VENT  , VENSTA, VX_CTE,
     &  VY_CTE, SBREK , ALFABJ,
     &  GAMBJ1, GAMBJ2, IQBBJ , IHMBJ , IFRBJ , BORETG, GAMATG, IWHTG ,
     &  IFRTG , ALFARO, GAMARO, GAM2RO, IDISRO, IEXPRO, IFRRO , BETAIH,
     &  EM2SIH, IFRIH , COEFHS, XDTBRK, NDTBRK, STRIA , ALFLTA, RFMLTA,
     &  KSPB  , BDISPB, BDSSPB, PROINF, DF_LIM, LIMIT , CIMPLI,
     &  NOMVEB, NOMVEF, BINVEN, NBD   , QINDI , TAUWAV,
     &  USOLD , TWOLD , Z0OLD , TSTOT , TSDER , TOLD  , TNEW  , VARIAN,
     &  FMOY  , XKMOY , USNEW , Z0NEW , TWNEW , TAUX1 , TAUX2 , TAUX3 ,
     &  TAUX4 , TAUX5 , TAUX6 , TAUX7 , TRA01 , BETA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALFABJ         |-->| MODELE DEFERLEMENT BJ : CONSTANTE ALPHA
C| ALFARO         |-->| MODELE DEFERLEMENT RO : CONSTANTE ALPHA
C| ALFLTA         |---| 
C| ALPHA          |-->| CONSTANTE DE LA LOI DE CHARNOCK
C| BDISPB         |---| 
C| BDSSPB         |---| 
C| BETA           |---| 
C| BETAIH         |-->| MODELE DEFERLEMENT IH : CONSTANTE BETA
C| BETAM          |-->| CONSTANTE BETAMAX DE LA FORMULE DU VENT
C| BINVEN         |-->| BINAIRE DU FICHIER DE VENT EN ENTREE
C| BORETG         |-->| MODELE DEFERLEMENT TG : CONSTANTE B
C| CDRAG          |-->| COEFFICIENT DE TRAINEE
C| CFROT1         |-->| CONSTANTE POUR LE TERME DE FROTTEMENT
C| CIMPLI         |---| 
C| CMOUT1         |-->| CONSTANTE 1 POUR LE TERME DE MOUTONNEMENT
C| CMOUT2         |-->| CONSTANTE 2 POUR LE TERME DE MOUTONNEMENT
C| COEFHS         |-->| COEFFICIENT LIMITATEUR DE LA HAUTEUR HS
C| COEFNL         |---| 
C| COEFNL(        |-->| COEFFICIENTS POUR QUADRUPLETS DIA
C| COSTET         |---| 
C| COSTET(        |-->| VECTEUR DES COSINUS DES DIRECTIONS
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| DECAL          |-->| CONSTANTE DE DECALAGE DE CROISSANCE VENT
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS (METRES)
C| DFREQ          |---| 
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCE
C| DF_LIM         |---| 
C| DTSI           |-->| PAS DE TEMPS D'INTEGRATION (SECONDES)
C| EM2SIH         |-->| MODELE DEFERLEMENT IH : CONSTANTE M2*
C| F             |---| 
C| F(             |---| SPECTRE DIRECTIONNEL DE VARIANCE
C| F1             |-->| PREMIERE FREQUENCE DE DISCRETISATION
C| FMOY           |---| 
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GAM2RO         |-->| MODELE DEFERLEMENT RO : CONSTANTE GAMMA2
C| GAMARO         |-->| MODELE DEFERLEMENT RO : CONSTANTE GAMMA
C| GAMATG         |-->| MODELE DEFERLEMENT TG : CONSTANTE GAMMA
C| GAMBJ1         |-->| MODELE DEFERLEMENT BJ : CONSTANTE GAMMA1
C| GAMBJ2         |-->| MODELE DEFERLEMENT BJ : CONSTANTE GAMMA2
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| IANGNL         |---| 
C| IANGNL(        |-->| INDICES ANGULAIRES POUR QUADRUPLETS DIA
C| IDISRO         |-->| MODELE DEFERLEMENT RO : DISTRIBUTION HOULE
C| IEXPRO         |-->| MODELE DEFERLEMENT RO : EXPOSANT N
C| IFRBJ          |-->| MODELE DEFERLEMENT BJ : MODE CALCUL DE FREQ
C| IFRIH          |-->| MODELE DEFERLEMENT IH : MODE CALCUL DE FREQ
C| IFRRO          |-->| MODELE DEFERLEMENT RO : MODE CALCUL DE FREQ
C| IFRTG          |-->| MODELE DEFERLEMENT TG : MODE CALCUL DE FREQ
C| IHMBJ          |-->| MODELE DEFERLEMENT BJ : MODE CALCUL DE HM
C| INDIC          |-->| TYPE DE FORMAT DE LECTURE
C| IQBBJ          |-->| MODELE DEFERLEMENT BJ : MODE CALCUL DE QB
C| IWHTG          |-->| MODELE DEFERLEMENT TG : MODE CALCUL DE W(H)
C| KSPB           |---| 
C| LIMIT          |---| 
C| NBD            |---| 
C| NBOR           |---| 
C| NBOR(          |-->| NUMEROS GLOBAUX DES POINTS FRONTIERES
C| NDTBRK         |-->| NOMBRE DE SOUS-PAS DE TEMPS DE DEFERLEMENT
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NOMVEB         |---| 
C| NOMVEF         |---| 
C| NP             |-->| NOMBRE DE POINTS DU CHAMP DE VENT
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| NSITS          |-->| NOMBRE DE PAS DE TEMPS D'INTEGRATION
C| NVEB           |---| 
C| NVEF           |---| 
C| PROINF         |-->| INDICATEUR DE PROFONDEUR INFINIE
C| QINDI          |---| 
C| RAISF          |-->| RAISON FREQUENTIELLE POUR DISCRETISATION
C| RFMLTA         |---| 
C| ROAIR          |-->| MASSE VOLUMIQUE DE L AIR
C| ROEAU          |-->| MASSE VOLUMIQUE DE L EAU
C| SBREK          |-->| INDICATEUR DE TYPE DE TERME DEFERLEMENT
C| SFROT          |-->| INDICATEUR DE TYPE DE TERME FROTTEMENT
C| SINTET         |---| 
C| SINTET(        |-->| VECTEUR DES   SINUS DES DIRECTIONS
C| SMOUT          |-->| INDICATEUR DE TYPE DE TERME MOUTONNEMENT
C| STRIA          |---| 
C| STRIF          |-->| INDICATEUR DE TYPE DE TERME INTERACTIONS
C| SVENT          |-->| INDICATEUR DE TYPE DE TERME INPUT PAR VENT
C| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
C| TAUWAV         |---| 
C| TAUWAV(        |---| TABLEAU DES CONTRAINTES DUES A LA HOULE
C| TAUX1          |---| 
C| TAUX1(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX2          |---| 
C| TAUX2(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX3          |---| 
C| TAUX3(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX4          |---| 
C| TAUX4(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX5          |---| 
C| TAUX5(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX6          |---| 
C| TAUX6(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX7          |---| 
C| TAUX7(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TETA           |---| 
C| TETA(          |-->| VECTEUR DES DIRECTIONS DE DISCRETISATION
C| TNEW           |---| 
C| TOLD           |---| 
C| TPROP          |-->| DATE DE FIN DE L'ETAPE D'INTEGRATION
C| TRA01          |---| 
C| TSDER          |---| 
C| TSTOT          |---| 
C| TV1            |<->| DATE DU CHAMP DE CHAMP 1
C| TV2            |<->| DATE DU CHAMP DE CHAMP 2
C| TWNEW          |---| 
C| TWNEW(         |---| DIRECTION DU VENT A L'INSTANT N+1
C| TWOLD          |---| 
C| TWOLD(         |---| DIRECTION DU VENT A L'INSTANT N
C| U1             |---| 
C| U1(            |---| TABLEAU DES COMP. OUEST-EST DE VENT 1
C| U2             |---| 
C| U2(            |---| TABLEAU DES COMP. OUEST-EST DE VENT 2
C| USNEW          |---| 
C| USNEW(         |---| VITESSE DE FROTTEMENT A L'INSTANT N+1
C| USOLD          |---| 
C| USOLD(         |---| VITESSE DE FROTTEMENT A L'INSTANT N
C| V1             |---| 
C| V1(            |---| TABLEAU DES COMP. SUD-NORD  DE VENT 1
C| V2             |---| 
C| V2(            |---| TABLEAU DES COMP. SUD-NORD  DE VENT 2
C| VARIAN         |---| 
C| VENSTA         |---| 
C| VENT           |-->| INDICATEUR DE PRISE EN COMPTE DE VENT
C| VENTX          |---| 
C| VENTX(         |---| TABLEAU DE VENT (COMP. OUEST-EST)
C| VENTY          |---| 
C| VENTY(         |---| TABLEAU DE VENT (COMP. SUD-NORD)
C| VX_CTE         |---| 
C| VY_CTE         |---| 
C| X             |---| 
C| X(             |-->| TABLEAU DES ABSCISSES DES POINTS MAILLAGE
C| XDTBRK         |-->| PAS DE TEMPS POUR LE DEFERLEMENT
C| XK             |---| 
C| XK(            |-->| TABLEAU DES NOMBRES D'ONDE
C| XKAPPA         |-->| CONSTANTE DE VON KARMAN
C| XKMOY          |---| 
C| XRELV          |---| 
C| XRELV(         |-->| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| Y             |---| 
C| Y(             |-->| TABLEAU DES ABSCISSES DES POINTS MAILLAGE
C| YRELV          |---| 
C| YRELV(         |-->| TABLEAU DES ORDONNEES DES POINTS RELEVES
C| Z0NEW          |---| 
C| Z0NEW(         |---| LONGUEUR DE RUGOSITE L'INSTANT N+1
C| Z0OLD          |---| 
C| Z0OLD(         |---| LONGUEUR DE RUGOSITE L'INSTANT N
C| ZVENT          |-->| COTE A LAQUELLE EST MESURE LE VENT (M)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER          NPOIN2, NPLAN , NF    , NSITS , NPTFR , NVEB  ,
     &                 NVEF  , LIMIT ,
     &                 SMOUT , SFROT , SVENT , STRIF , SBREK , INDIC ,
     &                 IQBBJ , IHMBJ , IFRBJ , IWHTG , IFRTG , IFRRO ,
     &                 IEXPRO, IFRIH , NDTBRK, NP    , IDISRO, STRIA ,
     &                 NBOR(NPTFR)   , IANGNL(NPLAN,8)
      INTEGER          NBD   , QINDI(NBD)
      DOUBLE PRECISION TAILF , CFROT1, GRAVIT, RAISF , DTSI  , TPROP ,
     &                 CMOUT1, CMOUT2, DDC   , TV1   , TV2   , ZVENT ,
     &                 ROAIR , ROEAU , XKAPPA, BETAM , DECAL , CDRAG ,
     &                 ALPHA , GAMBJ1, GAMBJ2, ALFABJ, BORETG, GAMATG,
     &                 COEFHS, VX_CTE, VY_CTE, CIMPLI,
     &                 GAMARO, ALFARO, GAM2RO, EM2SIH, BETAIH, XDTBRK,
     &                 ALFLTA, RFMLTA, KSPB  , BDISPB, BDSSPB, F1
      DOUBLE PRECISION  DEPTH(NPOIN2), USNEW(NPOIN2) , USOLD(NPOIN2) ,
     &                 VARIAN(NPOIN2),  FMOY(NPOIN2) , XKMOY(NPOIN2) ,
     &                  TWOLD(NPOIN2), TWNEW(NPOIN2) , Z0OLD(NPOIN2) ,
     &                  Z0NEW(NPOIN2), VENTX(NPOIN2) , VENTY(NPOIN2) ,
     &                     U1(NPOIN2),    U2(NPOIN2) ,    V1(NPOIN2) ,
     &                     V2(NPOIN2),     X(NPOIN2) ,     Y(NPOIN2) ,
     &                 TAUWAV(NPOIN2), TAUX1(NPOIN2) , TAUX2(NPOIN2) ,
     &                  TAUX3(NPOIN2), TAUX4(NPOIN2) , TAUX5(NPOIN2) ,
     &                  TAUX6(NPOIN2), TAUX7(NPOIN2) , COEFNL(16)    ,
     &                    TETA(NPLAN), SINTET(NPLAN) , COSTET(NPLAN) ,
     &                    F(NPOIN2,NPLAN,NF),   XK(NPOIN2,NF)        ,
     &                    DF_LIM(NPOIN2,NF) ,
     &                 TSDER(NPOIN2,NPLAN,NF),TSTOT(NPOIN2,NPLAN,NF) ,
     &                 FREQ(NF), DFREQ(NF), XRELV(NP), YRELV(NP)     ,
     &                 TOLD(NPOIN2,NPLAN), TNEW(NPOIN2,NPLAN)
      DOUBLE PRECISION BETA(NPOIN2)
      DOUBLE PRECISION TRA01(NPOIN2,NPLAN)
      CHARACTER*144 NOMVEB, NOMVEF
      CHARACTER*3 BINVEN
      LOGICAL  PROINF, VENT , VENSTA
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          ISITS , NPOIN3, NPOIN4, IFF   , IP    , JP    ,
     &                 IFCAR , MF1   , MF2   , MFMAX , IDT
      DOUBLE PRECISION AUX1  , AUX2  , AUX3  , AUX4  , COEF  , DFMAX ,
     &                 DEUPI , FM1   , FM2   , TDEB  , TFIN  , VITVEN,
     &                 VITMIN, HM0   , HM0MAX, DTN   , SUM   , AUXI  ,
     &                 USMIN
      CHARACTER*7      CHDON
C
C
      NPOIN3=NPOIN2*NPLAN
      NPOIN4=NPOIN3*NF
      DEUPI=2.D0*3.141592654D0
      VITMIN=1.D-3
C
C
C     -----------------------------------------------------------------
C     CHOPS THE SPECTRUM IN ACCORDANCE WITH THE BATHYMETRY
C     -----------------------------------------------------------------
      IF (.NOT.PROINF) THEN
C
C       0.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
C       --------------------------------------------
        CALL TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
C
C       0.2 COMPUTES THE CORRECTION COEFFICIENT ON THE SPECTRUM
C       -------------------------------------------------------
C
        DO IP=1,NPOIN2
          HM0MAX=COEFHS*DEPTH(IP)
          HM0 =MAX(4.D0*DSQRT(VARIAN(IP)),1.D-20)
          TAUX1(IP)=MIN((HM0MAX/HM0)**2,1.D0)
        ENDDO
C
C       0.3 CORRECTS THE SPECTRUM
C       --------------------------
C
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              F(IP,JP,IFF)=F(IP,JP,IFF)*TAUX1(IP)
            ENDDO
          ENDDO
        ENDDO
C
      ENDIF
C
C     ----------------------------------------------------------------
C     IF THE COMPUTATION INCLUDES STATIONARY WINDS, DUPLICATES THE
C     CONDITIONS AT THE START OF THE TIME STEP TO THE END OF THE TIME
C     STEP. (THIS IS BECAUSE ARRAYS TWNEW, USNEW AND Z0NEW ARE WORKING
C     ARRAYS USED IN DUMP2D BETWEEN 2 CALLS TO SEMIMP).
C     ----------------------------------------------------------------
      IF (VENT.AND.VENSTA) THEN
        DO IP=1,NPOIN2
          TWNEW(IP)=TWOLD(IP)
        ENDDO
        IF (SVENT.EQ.2) THEN
          DO IP=1,NPOIN2
            USNEW(IP)=USOLD(IP)
            Z0NEW(IP)=Z0OLD(IP)
          ENDDO
        ENDIF
      ENDIF
C
C
C     -----------------------------------------------------------------
C     START OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
C     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
C     -----------------------------------------------------------------
      DO 100 ISITS=1,NSITS
C
C
C       1. ASSIGNS THE START AND END DATES OF TIME STEP
C       =========================================================
        TDEB=TPROP-DBLE(NSITS-ISITS+1)*DTSI
        TFIN=TDEB+DTSI
C
C
C       2. UPDATES (IF HAS TO) THE WIND ARRAYS
C       =================================================
        IF (VENT.AND..NOT.VENSTA) THEN
C
C         2.1 UPDATES THE WIND FIELD FOR DATE TFIN
C         ---------------------------------------------------
          CHDON='VENT   '
          IF (NOMVEB(1:1).NE.' ') THEN
            CALL NOUDON
     &( VENTX , VENTY , X     , Y     , NPOIN2, NVEB  , BINVEN, NBOR  ,
     &  NPTFR , TFIN  , DDC   , TV1   , TV2   , NP    , XRELV , YRELV ,
     &  TOLD  , TNEW  , TRA01 , U1    , V1    , U2    , V2    ,
     &  INDIC , CHDON , 2 )
          ELSEIF (NOMVEF(1:1).NE.' ') THEN
            CALL NOUDON
     &( VENTX , VENTY , X     , Y     , NPOIN2, NVEF  , BINVEN, NBOR  ,
     &  NPTFR , TFIN  , DDC   , TV1   , TV2   , NP    , XRELV , YRELV ,
     &  TOLD  , TNEW  , TRA01 , U1    , V1    , U2    , V2    ,
     &  INDIC , CHDON , 2 )
          ELSE
            CALL ANAVEN
     &( VENTX , VENTY , X     , Y     , NPOIN2, TFIN  , DDC   , VX_CTE,
     &  VY_CTE)
          ENDIF
C
C         2.2 COMPUTES THE WIND DIRECTION
C         -----------------------------------
C
          DO IP=1,NPOIN2
            VITVEN=SQRT(VENTX(IP)**2+VENTY(IP)**2)
            IF (VITVEN.GT.VITMIN) THEN
              TWNEW(IP)=ATAN2(VENTX(IP),VENTY(IP))
            ELSE
              TWNEW(IP)=0.D0
            ENDIF
          ENDDO
C
C         2.3 COMPUTES THE FRICTION VELOCITIES AND ROUGHNESS LENGTHS
C         ------------------------------------------------------------
          IF (SVENT.EQ.2) CALL USTAR2
     &( USNEW , VENTX , VENTY , NPOIN2)
C
        ENDIF
C
        IF (VENT) THEN
          IF (SVENT.EQ.1) CALL USTAR1
     &( USNEW , Z0NEW , TAUWAV, VENTX , VENTY , CDRAG , ALPHA , XKAPPA,
     &  ZVENT , GRAVIT, NPOIN2)
        ENDIF
C
C
C       3. COMPUTES MEAN PARAMETERS FOR THE DIRECTIONAL SPECTRUM
C       =========================================================
C
C       3.1 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
C       --------------------------------------------
        CALL TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
C
C       3.2 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
C       ----------------------------------------------
        CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
C
C       3.3 COMPUTES THE MEAN WAVE NUMBER OF THE SPECTRUM
C       ---------------------------------------------
        CALL KMOYEN
     &( XKMOY , XK    , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN ,
     &  NPOIN2, TAUX1 , TAUX2 , TAUX3 )
C
C
C       4. COMPUTES THE CONTRIBUTIONS OF THE SOURCE TERMS FOR GENERATION,
C          WHITECAPPING AND INTERACTIONS BETWEEN QUADRUPLETS
C       =============================================================
C
C       4.1 INITIALISES THE ARRAYS FOR THE SOURCE TERMS
C       ----------------------------------------------------
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              TSTOT(IP,JP,IFF)=0.0D0
              TSDER(IP,JP,IFF)=0.0D0
            ENDDO
          ENDDO
        ENDDO
C
C       4.2 GENERATION BY WIND
C       ---------------------------
        IF (VENT) THEN
          IF (SVENT.EQ.1) THEN
            CALL QWIND1
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , Z0OLD , Z0NEW , TETA  , ROAIR , ROEAU , BETAM , XKAPPA,
     &  DECAL , GRAVIT, NF    , NPLAN , NPOIN2, CIMPLI, TOLD  , TNEW  ,
     &  TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 , TAUX6 , TAUX7 )
            CALL STRESS
     &( TAUWAV, TSTOT , F     , USNEW , TWNEW , Z0NEW , FREQ  , DFREQ ,
     &  TETA  , SINTET, COSTET, ROAIR , ROEAU , XKAPPA, BETAM , DECAL ,
     &  GRAVIT, NPOIN2, NPLAN , NF    , TAUX1 , TAUX2 , TAUX3 )
          ELSE
            CALL QWIND2
     &( TSTOT , TSDER , F     , XK    , FREQ  , USOLD , USNEW , TWOLD ,
     &  TWNEW , TETA  , ROAIR , ROEAU , GRAVIT, NF    , NPLAN , NPOIN2,
     &  CIMPLI, TAUX1 , TAUX2 , TAUX3 , TAUX4 , TAUX5 )
          ENDIF
        ELSE
          DO IP=1,NPOIN2
            USNEW(IP)=0.D0
          ENDDO
        ENDIF
C
C       4.3 NON-LINEAR INTERACTIONS BETWEEN QUADRUPLETS
C       --------------------------------------------------------------
        IF (STRIF.EQ.1) CALL QNLIN1
     &( TSTOT , TSDER , IANGNL, COEFNL, NF    , NPLAN , F1    , RAISF ,
     &  TAILF , PROINF, NPOIN2, F     , DEPTH , XKMOY , TAUX1 , TAUX2 ,
     &  TAUX3 , TAUX4 , TAUX5 , TAUX6 )
C
C
C       4.4 WHITE-CAPPING DISSIPATION
C       -------------------------------------------------
        IF (SMOUT.EQ.1) CALL QMOUT1
     &( TSTOT , TSDER , F     , XK    , VARIAN, FREQ  , FMOY  , XKMOY ,
     &  PROINF, CMOUT1, CMOUT2, GRAVIT, NF    , NPLAN , NPOIN2, TAUX1 ,
     &  TAUX2 )
C
C       4.5 BOTTOM FRICTION DISSIPATION
C       -------------------------------------------
        IF ((SFROT.EQ.1).AND.(.NOT.PROINF)) CALL QFROT1
     &( TSTOT , TSDER , F     , XK    , DEPTH , CFROT1, GRAVIT, NF    ,
     &  NPLAN , NPOIN2, TAUX1 )

C.......4.6 COMPUTES THE LIMITING FACTOR OF GROWTH
C       ------------------------------------
C.......NO LIMITING FACTOR (VERY HIGH VALUE IN ACTUAL FACTS)
        IF (LIMIT.EQ.0) THEN
          AUXI=1.D99
          DO IFF=1,NF
            DO IP=1,NPOIN2
              DF_LIM(IP,IFF)=AUXI
            ENDDO
          ENDDO
C
C.......LIMITING FACTOR TAKEN FROM WAM-CYCLE 4
        ELSEIF (LIMIT.EQ.1) THEN
C          COEF=6.4D-7*GRAVIT**2*DTSI/1200.D0
          COEF=0.62D-4*DTSI/1200.D0
          DO IFF=1,NF
            AUXI=COEF/FREQ(IFF)**5
            DO IP=1,NPOIN2
              DF_LIM(IP,IFF)=AUXI
            ENDDO
          ENDDO
C
C.......LIMITING FACTOR FROM HERSBACH AND JANSSEN (1999), WITHOUT UETOILE
        ELSEIF (LIMIT.EQ.2) THEN
          COEF=3.0D-7*GRAVIT*FREQ(NF)*DTSI
          DO IFF=1,NF
            AUXI=COEF/FREQ(IFF)**4
            USMIN=GRAVIT*5.6D-3/FREQ(IFF)
            DO IP=1,NPOIN2
              DF_LIM(IP,IFF)=AUXI*MAX(USNEW(IP),USMIN)
            ENDDO
          ENDDO
        ENDIF
C
C
C       5. UPDATES THE SPECTRUM - TAKES THE SOURCE TERMS INTO ACCOUNT
C         (GENERATION, WHITECAPPING AND QUADRUPLET INTERACTIONS)
C       ==============================================================
C
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              DFMAX=DF_LIM(IP,IFF)
              AUX1=MAX( 1.D0-DTSI*TSDER(IP,JP,IFF)*CIMPLI , 1.D0 )
              AUX2=DTSI*TSTOT(IP,JP,IFF)/AUX1
              AUX3=MIN( ABS(AUX2) , DFMAX )
              AUX4=SIGN(AUX3,AUX2)
              F(IP,JP,IFF)=MAX( F(IP,JP,IFF)+AUX4 , 0.D0 )
            ENDDO
          ENDDO
        ENDDO
C
C
C       6. TREATS THE HIGH FREQUENCIES DIFFERENTLY
C       =======================================================
C
C       6.1 COMPUTES THE MEAN FREQUENCY OF THE SPECTRUM
C       ----------------------------------------------
        CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
C
        AUX1=GRAVIT/(7.D0*DEUPI*FREQ(1))
        AUX2=2.5D0/FREQ(1)
        AUX3=1.D0/LOG10(RAISF)
C
        DO IP=1,NPOIN2
C
C       6.2 COMPUTES THE LAST FREQUENCY OF THE DISCRETISED SPECTRUM.
C           THIS FREQUENCY IS THE MAXIMUM OF (FM1=4.*FPM ; FM2=2.5*FMOY).
C           ITS INDEX IS MFMAX.
C       -------------------------------------------------------------
          FM1 =AUX1/(USNEW(IP)+1.D-90)
          FM2 =AUX2*FMOY(IP)
          MF1=INT(AUX3*LOG10(FM1)+1.D0)
          MF2=INT(AUX3*LOG10(FM2)+1.D0)
          MFMAX=MIN( MAX(MF1,MF2) , NF )
C
C       6.3 MODIFIES THE HIGH FREQUENCY PART OF THE SPECTRUM
C           A DECREASE IN F**(-TAILF) IS IMPOSED BEYOND
C           FREQ(MFMAX).  (TAILF=5 IN WAM-CYCLE 4)
C       --------------------------------------------------------
          DO IFF=MFMAX+1,NF
            AUX4=(FREQ(MFMAX)/FREQ(IFF))**TAILF
            DO JP=1,NPLAN
              F(IP,JP,IFF)=AUX4*F(IP,JP,MFMAX)
            ENDDO
          ENDDO
        ENDDO
C
C       7. TAKES THE BREAKING SOURCE TERM INTO ACCOUNT
C       =================================================
C
        IF (((SBREK.GT.0).OR.(STRIA.GT.0)).AND.(.NOT.PROINF)) THEN
C
C         7.1 COMPUTES A REPRESENTATIVE FREQUENCY
C         ------------------------------------------
          IF ((SBREK.GT.0).AND.(SBREK.LT.5)) THEN
            IF (SBREK.EQ.1) IFCAR = IFRBJ
            IF (SBREK.EQ.2) IFCAR = IFRTG
            IF (SBREK.GE.3) IFCAR = IFRRO
            IF (SBREK.GE.4) IFCAR = IFRIH
C
            GOTO (751,752,753,754,755,756), IFCAR
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'FREQUENCE DE HOULE NON PREVUE......IFCAR=',
     &                     IFCAR
            ELSE
              WRITE(LU,*) 'WAVE FREQUENCY NOT EXPECTED......IFCAR=',
     &                     IFCAR
            ENDIF
            GOTO 759
C
C           MEAN FREQUENCY FMOY
C           - - - - - - - - - - - -
  751       CONTINUE
            DO IP=1,NPOIN2
              TAUX3(IP)=FMOY(IP)
            ENDDO
            GOTO 759
C
C           MEAN FREQUENCY F01
C           - - - - - - - - - - -
  752       CONTINUE
            CALL FREM01
     &( TAUX3 , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            GOTO 759
C
C           MEAN FREQUENCY F02
C           - - - - - - - - - - -
  753       CONTINUE
            CALL FREM02
     &( TAUX3 , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            GOTO 759
C
C           PEAK FREQUENCY (DISCRETE FREQUENCY WITH MAX VARIANCE)
C           - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  754       CONTINUE
            CALL FREPIC
     &( TAUX3 , F     , FREQ  , NF    , NPLAN , NPOIN2, TAUX1 , TAUX2 )
            GOTO 759
C
C           PEAK FREQUENCY (READ WITH EXPONENT 5)
C           - - - - - - - - - - - - - - - - - - - - - - - - - -
  755       CONTINUE
            CALL FPREAD
     &( TAUX3 , F     , FREQ  , DFREQ , NF    , NPLAN , NPOIN2, 5.D0  ,
     &  TAILF , TAUX1 , TAUX2 )
            GOTO 759
C
C           PEAK FREQUENCY (READ WITH EXPONENT 8)
C           - - - - - - - - - - - - - - - - - - - - - - - - - -
  756       CONTINUE
            CALL FPREAD
     &( TAUX3 , F     , FREQ  , DFREQ , NF    , NPLAN , NPOIN2, 8.D0  ,
     &  TAILF , TAUX1 , TAUX2 )
C
  759       CONTINUE
C
        ENDIF
C
C.........LOOP ON SUB-TIME STEPS FOR BREAKING
C         = = = = = = = = = = = = = = = = = = = = = = = = = = =
          SUM=(XDTBRK**NDTBRK-1.D0)/(XDTBRK-1.D0)
          DTN=DTSI/SUM
C
          DO 782 IDT=1,NDTBRK

C         7.2 INITIALISES THE ARRAYS FOR THE SOURCE-TERMS
C         ----------------------------------------------------
          DO IFF=1,NF
            DO JP=1,NPLAN
              DO IP=1,NPOIN2
                TSTOT(IP,JP,IFF)=0.0D0
              ENDDO
            ENDDO
          ENDDO
C
C         7.3 COMPUTES THE TOTAL VARIANCE OF THE SPECTRUM
C         --------------------------------------------
          CALL TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
C
C         7.4 COMPUTES THE WAVE BREAKING CONTRIBUTION
C         --------------------------------------
          GOTO (761,762,763,764), SBREK
          IF(SBREK.NE.0) THEN
           IF(LNG.EQ.1) THEN
           WRITE(LU,*) 'TYPE DE DEFERLEMENT NON IMPLANTE...SBREK=',SBREK
           WRITE(LU,*) 'PAS DE PRISE EN COMPTE DU DEFERLEMENT'
           ELSE
           WRITE(LU,*) 'BREAKING FORMULATION NOT PROGRAMMED...SBREK=',
     &                  SBREK
           WRITE(LU,*) 'NO CONSIDERATION OF THE DEPTH-INDUCED BREAKING'
           ENDIF
          ENDIF
          GOTO 769
C
C         7.4.1 BREAKING ACCORDING TO BATTJES AND JANSSEN (1978)
C         - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  761     CONTINUE
          CALL QBREK1
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , ALFABJ, GAMBJ1,
     &  GAMBJ2, IQBBJ , IHMBJ , NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
C
C         7.4.2 BREAKING ACCORDING TO THORNTON AND GUZA (1983)
C         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  762     CONTINUE
          CALL QBREK2
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , BORETG, GAMATG,
     &  IWHTG , NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
C
C         7.4.3 BREAKING ACCORDING TO ROELVINK (1993)
C         - - - - - - - - - - - - - - - - - - - - - -
  763     CONTINUE
          CALL QBREK3
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , ALFARO, GAMARO,
     &  GAM2RO, IEXPRO, IDISRO, NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
C
C         7.4.4 BREAKING ACCORDING TO IZUMIYA AND HORIKAWA (1984)
C         - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  764     CONTINUE
          CALL QBREK4
     &( TSTOT , TSDER , F     , TAUX3 , VARIAN, DEPTH , BETAIH, EM2SIH,
     &  GRAVIT, NF    , NPLAN , NPOIN2, BETA )
          GOTO 769
C
  769     CONTINUE
C
C       7.5 NON-LINEAR INTERACTIONS BETWEEN FREQUENCY TRIPLETS
C       -----------------------------------------------------------
          IF (STRIA.EQ.1) THEN
            CALL FREMOY
     &( FMOY  , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2,
     &  TAUX1 , TAUX2 )
            CALL QTRIA1
     &( F     , XK    , FREQ  , DEPTH , RAISF , GRAVIT, ALFLTA, RFMLTA,
     &  NF    , NPLAN , NPOIN2, TSTOT , TSDER , VARIAN, FMOY  )
C
        ELSEIF (STRIA.EQ.2) THEN
            CALL QTRIA2
     &( F     , XK    , FREQ  , DFREQ , DEPTH , TETA  , SINTET, COSTET ,
     &  KSPB  , BDISPB, BDSSPB, RAISF , GRAVIT, NF    , NPLAN , NPOIN2 ,
     &  NBD   , QINDI , TSTOT , TSDER )
        ENDIF
C
C         7.5 UPDATES THE SPECTRUM - TAKES THE BREAKING SOURCE TERM
C             INTO ACCOUNT (EXPLICIT EULER SCHEME)
C         ---------------------------------------------------------
C
        DO IFF=1,NF
          DO JP=1,NPLAN
            DO IP=1,NPOIN2
              F(IP,JP,IFF)=MAX(F(IP,JP,IFF)+DTN*TSTOT(IP,JP,IFF),0.D0)
            ENDDO
          ENDDO
        ENDDO
C
        DTN=DTN*XDTBRK
C
  782   CONTINUE
C
        ENDIF
C
C
C       8. TRANSFERS DATA FROM NEW TO OLD FOR THE NEXT TIME STEP
C       ==============================================================
        IF (VENT) THEN
          DO IP=1,NPOIN2
            USOLD(IP)=USNEW(IP)
            Z0OLD(IP)=Z0NEW(IP)
            TWOLD(IP)=TWNEW(IP)
          ENDDO
        ENDIF
C
C
  100 CONTINUE
C     -----------------------------------------------------------------
C     END OF THE MAIN LOOP ON THE NUMBER OF TIME STEPS (NSITS)
C     FOR INTEGRATION OF THE SOURCE TERMS, BY PROPAGATION TIME STEP
C     -----------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C