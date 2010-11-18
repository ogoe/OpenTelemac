C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE ADVECTION STEP BY COMPUTING THE
!>                PARAMETERS COMMON TO ALL THE VARIABLES TO ADVECT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ISOUSI, LT, WP, WPS, ZPROPS
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::BYPASS BYPASS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CONV CONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DM1 DM1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FC3D FC3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLBOR FLBOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLODEL FLODEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLOPAR FLOPAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLUEXT FLUEXT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLUINT FLUINT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLULIM FLULIM@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLUX FLUX@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FN3D FN3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADEB GRADEB@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAPRD GRAPRD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM2H IELM2H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM2V IELM2V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM3 IELM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IKLE2 IKLE2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IT1 IT1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IT2 IT2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IT3 IT3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IT4 IT4@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ITURBV ITURBV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIMPRO LIMPRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASK MASK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKBR MASKBR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH2D MESH2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MMURD MMURD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSUPG MSUPG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MTRA2 MTRA2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MURD_TF MURD_TF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NELEM3 NELEM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NETAGE NETAGE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NONHYD NONHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLINT NPLINT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR2 NPTFR2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTSUP OPTSUP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPT_HNEG OPT_HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PLUIE PLUIE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RAIN RAIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHCTA SCHCTA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SIGMAG SIGMAG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SVIDE SVIDE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_01 T2_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_01 T3_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_02 T3_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_03 T3_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_04 T3_04@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_05 T3_05@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TA TA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRAV3 TRAV3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UCONV UCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UCONVC UCONVC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VCONV VCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VCONVC VCONVC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLU VOLU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLU2D VOLU2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLUN VOLUN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WSCONV WSCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::Z Z@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZCHAR ZCHAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZCONV ZCONV@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink, 
!> @link DECLARATIONS_TELEMAC::KDIR KDIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORMUL, I, I2, IP, IPLAN, IS, IWS, NSEG3D, OPER, OPTHNEG, SAVEZ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PRECON, SAVEZ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BYPASS_CRUSHED_POINTS_EBE(), BYPASS_CRUSHED_POINTS_SEG(), CHARAC(), DIAG_MURD(), FLUX3D(), MATRIX(), OM(), OS(), OV(), PARCOM2_SEG(), PLANTE(), TRIDW2(), UPWIND(), WSTAR(), WSTARW()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> CONVCONV
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 26/04/2010
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18; JM JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/02/2010
!> </td><td> JMH
!> </td><td> COMPUTES ZCHAR TO CALL CHARAC
!>          (ALLOWS SIMPLIFICATION OF CHAR41 AND STREAMLINE.F)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 18/08/2009
!> </td><td> JMH
!> </td><td> UCONVC AND VCONVC FOR ADVECTION FIELD
!>           GIVEN TO SUPG AND CHARACTERISTICS (DONE IN WAVE_EQUATION)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 29/06/2009
!> </td><td> JMH
!> </td><td> POINT TO POINT FLUXES COMPUTED IN FLODEL
!>           FINITE VOLUMES ADVECTION SCHEMES OR 9 ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 13/08/2008
!> </td><td> JMH
!> </td><td> IMMEDIATE INTERPOLATION IN CHARAC
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 07/08/2008
!> </td><td> JMH
!> </td><td> CALLS CHARAC INSTEAD OF CARACT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AM1,AM2
!></td><td><-></td><td>MATRICES DE TRAVAIL
!>    </td></tr>
!>          <tr><td>CONV
!></td><td>--></td><td>TABLEAU DE LOGIQUES INDIQUANT POUR CHAQUE
!>                  SCHEMA DE CONVECTION SI AU MOINS UNE
!>                  VARIABLE EST TRAITEE PAR CE SCHEMA
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>ELT
!></td><td><--</td><td>NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td><--</td><td>NUMEROS DES ETAGES AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>FLUEXT
!></td><td><--</td><td>FLUX EXTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>FLUINT
!></td><td><--</td><td>FLUX INTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>IBOR
!></td><td>--></td><td>TABLEAU DES ELEMENTS ADJACENTS AUX FACES(3D)
!>    </td></tr>
!>          <tr><td>IELM2H
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DH
!>    </td></tr>
!>          <tr><td>IELM2V
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DV
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>INFORMATIONS SUR LES SOLVEURS
!>    </td></tr>
!>          <tr><td>ISOUSI
!></td><td>--></td><td>RANG DE LA SOUS-ITERATION EN COURS
!>    </td></tr>
!>          <tr><td>ITRAV3
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL D'ENTIERS
!>    </td></tr>
!>          <tr><td>LIWBOF,L,S
!></td><td>--></td><td>TYPE DE CONDITIONS LIMITES POUR WS
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASK
!></td><td><--</td><td>MASQUES POUR LES SEGMENTS 2D
!>                  MASK(NPTFR,1): 1. SI KDIR SUR U 0. SINON
!>                  MASK(NPTFR,2): 1. SI KDIR SUR V 0. SINON
!>                  MASK(NPTFR,3): 1. SI KDDL SUR U 0. SINON
!>                  MASK(NPTFR,4): 1. SI KDDL SUR V 0. SINON
!>                  MASK(NPTFR,5): 1. SI KNEU SUR U 0. SINON
!>                  MASK(NPTFR,6): 1. SI KNEU SUR V 0. SINON
!>                  MASK(NPTFR,7): 1. SI KOND 0. SINON
!>                  (KOND N'EST PAS DEFINI DANS TELEMAC-3D,
!>                  CAR IL N Y A PAS D'ONDE INCIDENTE. EN
!>                  CONSEQUENCE, MASK(*,7)=0)
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>MASQUAGE DES POINTS
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>MURD
!></td><td>--></td><td>MATRICE MURD NON SYMETRIQUE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES GLOBALES DES POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NPLAN - 1
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPLINT
!></td><td>--></td><td>NUMERO DU PLAN INTERMEDIAIRE DE REFERENCE
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NSOUSI
!></td><td>--></td><td>NOMBRE TOTAL DE SOUS-ITERATIONS
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!>                  NUMEROTATION LOCALE 3D
!>    </td></tr>
!>          <tr><td>SHP
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
!>                  DES COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SM
!></td><td><-></td><td>SECOND MEMBRE POUR LA VITESSE VERTICALE
!>    </td></tr>
!>          <tr><td>SUPG
!></td><td>--></td><td>MATRICE SUPG NON SYMETRIQUE
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>TBB
!></td><td>--></td><td>BLOC DE BLOCS DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TE1
!></td><td><-></td><td>TABLEAU DE TRAVAIL PAR ELEMENT 2D
!>    </td></tr>
!>          <tr><td>TETAU
!></td><td>--></td><td>TAUX D'IMPLICITATION SUR U ET V
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 2D
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 3D
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td><--</td><td>COMPOSANTES HORIZONTALES DU CHAMP CONVECTEUR
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!>    </td></tr>
!>          <tr><td>WP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WPS
!></td><td>--></td><td>VITESSE W DANS LE MAILLAGE TRANSFORME
!>    </td></tr>
!>          <tr><td>WSCON2
!></td><td><--</td><td>VITESSE VERTICALE MOYENNEE PAR ETAGE
!>    </td></tr>
!>          <tr><td>WSCONV
!></td><td><--</td><td>VITESSE VERTICALE AUX NOEUDS 3D
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZPROP
!></td><td>--></td><td>COORDONNEE VERTICALE A L'ETAPE DE CONTINUITE
!>    </td></tr>
!>          <tr><td>ZPROPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZSTAR
!></td><td><--</td><td>HAUTEURS RELATIVES DES PLANS HORIZONTAUX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRECON
     &(WP,WPS,ZPROPS,ISOUSI,LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AM1,AM2        |<->| MATRICES DE TRAVAIL
C| CONV           |-->| TABLEAU DE LOGIQUES INDIQUANT POUR CHAQUE
C|                |   | SCHEMA DE CONVECTION SI AU MOINS UNE
C|                |   | VARIABLE EST TRAITEE PAR CE SCHEMA
C| DT             |-->| PAS DE TEMPS
C| ELT            |<--| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| ETA            |<--| NUMEROS DES ETAGES AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| FLUEXT         |<--| FLUX EXTERIEUR PAR NOEUD
C| FLUINT         |<--| FLUX INTERIEUR PAR NOEUD
C| IBOR           |-->| TABLEAU DES ELEMENTS ADJACENTS AUX FACES(3D)
C| IELM2H         |-->| TYPE DE DISCRETISATION 2DH
C| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IKLE2          |-->| CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
C| INFO           |-->| INFORMATIONS SUR LES SOLVEURS
C| ISOUSI         |-->| RANG DE LA SOUS-ITERATION EN COURS
C| ITRAV3         |<->| STRUCTURE DE TABLEAUX DE TRAVAIL D'ENTIERS
C| LIWBOF,L,S     |-->| TYPE DE CONDITIONS LIMITES POUR WS
C| LT             |---| 
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASK           |<--| MASQUES POUR LES SEGMENTS 2D
C|                |   | MASK(NPTFR,1): 1. SI KDIR SUR U 0. SINON
C|                |   | MASK(NPTFR,2): 1. SI KDIR SUR V 0. SINON
C|                |   | MASK(NPTFR,3): 1. SI KDDL SUR U 0. SINON
C|                |   | MASK(NPTFR,4): 1. SI KDDL SUR V 0. SINON
C|                |   | MASK(NPTFR,5): 1. SI KNEU SUR U 0. SINON
C|                |   | MASK(NPTFR,6): 1. SI KNEU SUR V 0. SINON
C|                |   | MASK(NPTFR,7): 1. SI KOND 0. SINON
C|                |   | (KOND N'EST PAS DEFINI DANS TELEMAC-3D,
C|                |   | CAR IL N Y A PAS D'ONDE INCIDENTE. EN
C|                |   | CONSEQUENCE, MASK(*,7)=0)
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MASKPT         |-->| MASQUAGE DES POINTS
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| MURD           |-->| MATRICE MURD NON SYMETRIQUE
C| NBOR           |-->| ADRESSES GLOBALES DES POINTS FRONTIERES.
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPLINT         |-->| NUMERO DU PLAN INTERMEDIAIRE DE REFERENCE
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
C| NSOUSI         |-->| NOMBRE TOTAL DE SOUS-ITERATIONS
C| NULONE         |-->| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C| SHP            |<--| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHZ            |<--| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
C|                |   | DES COURBES CARACTERISTIQUES.
C| SM             |<->| SECOND MEMBRE POUR LA VITESSE VERTICALE
C| SUPG           |-->| MATRICE SUPG NON SYMETRIQUE
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| SVIDE          |-->| STRUCTURE VIDE
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C| TE1            |<->| TABLEAU DE TRAVAIL PAR ELEMENT 2D
C| TETAU          |-->| TAUX D'IMPLICITATION SUR U ET V
C| TRAV2          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 2D
C| TRAV3          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 3D
C| UCONV,VCONV    |<--| COMPOSANTES HORIZONTALES DU CHAMP CONVECTEUR
C| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
C| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
C| WP             |---| 
C| WPS            |-->| VITESSE W DANS LE MAILLAGE TRANSFORME
C| WSCON2         |<--| VITESSE VERTICALE MOYENNEE PAR ETAGE
C| WSCONV         |<--| VITESSE VERTICALE AUX NOEUDS 3D
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C| ZPROP          |-->| COORDONNEE VERTICALE A L'ETAPE DE CONTINUITE
C| ZPROPS         |---| 
C| ZSTAR          |<--| HAUTEURS RELATIVES DES PLANS HORIZONTAUX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PRECON => PRECON
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WP,WPS,ZPROPS
C
      INTEGER, INTENT(IN) :: ISOUSI,LT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ::I,IS,IP,I2,OPTHNEG,IWS,NSEG3D,IPLAN
      CHARACTER(LEN=16) FORMUL
      CHARACTER(LEN=8) OPER
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!=======================================================================
!
C     MESH MODIFIED TO BE EQUIVALENT TO THE DEPTH USED IN THE 2D
C     CONTINUITY EQUATION, TO CALL : VERFIL, FLUX3D, VECTOR, MATRIX
!
C     ZPROPS IS TEMPORARILY PUT IN MESH3D%Z
      SAVEZ=>MESH3D%Z%R
      MESH3D%Z%R=>ZPROPS%R
      NSEG3D=MESH3D%NSEG
!
!=======================================================================
!
C COMPUTES INTERNAL AND EXTERNAL FLUXES AND ADVECTION FIELDS
!
!=======================================================================
!
      OPTHNEG=OPT_HNEG
      IF(LT.EQ.0) OPTHNEG=0
!
      CALL FLUX3D
     & (FLUINT,FLUEXT,UCONV,VCONV,T3_01,T3_02,T3_03,MESH3D%W,
     &  NETAGE,NPLAN,NELEM3,IELM3,IELM2H,IELM2V,SVIDE,MESH3D,
     &  MASK%ADR(8)%P,MSK,MASKEL,MASKBR,
     &  LIMPRO%I,KDIR,NPTFR2,DT,VOLU,VOLUN,MESH2D,
     &  GRAPRD,SIGMAG,T2_01,NPOIN2,NPOIN3,FLUX%R,DM1,ZCONV,FLBOR,
     &  PLUIE,RAIN,FLODEL,FLOPAR,OPTHNEG,FLULIM,
     &  CONV(ADV_LPO).OR.CONV(ADV_LPO_TF),LT,BYPASS)
!
!=======================================================================
C   COMPUTES (DZW*)JH,IV+1/2 AND ACCUMULATES IN WSCONV
!=======================================================================
!
      CALL TRIDW2(WSCONV)
!
!=======================================================================
C     PREPARES ADVECTION BY MURD METHOD
C     STORAGE IS ALWAYS EBE
!=======================================================================
!
      IF(CONV(ADV_NSC).OR.CONV(ADV_PSI)) THEN
!
C       NOTE: THE MATRIX IS THE SAME IN BOTH CASES BUT
C             WITH PSI SCHEME THE DIAGONAL IS NOT ASSEMBLED BECAUSE
C             IT IS ASSEMBLED IN MURD3D
        IF(CONV(ADV_NSC).AND..NOT.(OPT_HNEG.EQ.2.OR.SIGMAG)) THEN
          FORMUL = 'MAMURD 2     N  '
        ELSE
          FORMUL = 'MAMURD 2     PSI'
        ENDIF
        CALL MATRIX
     &  (MMURD,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
C       HERE THE BYPASS IS NOT OPTIONAL, OTHERWISE
C       THE SCHEMES ARE NOT MASS-CONSERVATIVE
C       IF(BYPASS) THEN
        IF(OPT_HNEG.EQ.2.OR.SIGMAG) THEN
          CALL BYPASS_CRUSHED_POINTS_EBE(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   MMURD%X%R,T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,NELEM2,NELEM3,NPLAN,
     &                                   MESH3D%IKLE%I)
          IF(CONV(ADV_NSC)) THEN
            CALL DIAG_MURD(MMURD%D%R,MMURD%X%R,NELEM3,MESH3D%NELMAX,
     &                     NPOIN3,MESH3D%IKLE%I)
          ENDIF
        ENDIF
C       ENDIF
!
      ENDIF
!
!=======================================================================
C     PREPARES ADVECTION BY MURD METHOD IN EDGE-BASED FORM
C     STORAGE IS ALWAYS EDGE-BASED
!=======================================================================
!
      IF(CONV(ADV_NSC_TF)) THEN
!
C       NOTE: THE MATRIX IS THE SAME IN BOTH CASES BUT
C             WITH PSI SCHEME THE DIAGONAL IS NOT ASSEMBLED
C             IT IS WHAT WE WANT HERE
        FORMUL = 'MAMURD 2     PSI'
        CALL MATRIX
     &  (MURD_TF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &   UCONV,VCONV,WSCONV,MESH3D,MSK,MASKEL)
!
C       FROM 30 SEGMENTS WITH POSITIVE FLUXES, WE GO TO 15 WITH
C       POSITIVE OR NEGATIVE FLUXES
        DO I=1,NSEG3D
          MURD_TF%X%R(I) = MURD_TF%X%R(I) - MURD_TF%X%R(I+NSEG3D)
        ENDDO
C       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   MURD_TF%X%R,
     &                                   T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,ADV_NSC_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
        IF(NCSIZE.GT.1) THEN
C         ASSEMBLED FORM OF FLUXES STORED IN SECOND PART
C         OF MATRIX WHICH OTHERWISE IS NOT USED
          CALL OV('X=Y     ',MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                       MURD_TF%X%R(       1:  NSEG3D),
     &                       MURD_TF%X%R(       1:  NSEG3D),
     &                       0.D0,NSEG3D)
          CALL PARCOM2_SEG(MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MURD_TF%X%R(NSEG3D+1:2*NSEG3D),
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,2)
        ENDIF
!
      ENDIF
!
!=======================================================================
C     PREPARES LEO POSTMA ADVECTION SCHEMES
!=======================================================================
!
C     RETRIEVES VERTICAL FLUXES FROM WSCONV
C     VERTICAL FLUXES ARE STORED IN FLODEL AFTER
C     THE HORIZONTAL FLUXES (THERE ARE NSEG*NPLAN HORIZONTAL FLUXES)
C     USEFUL SIZE OF WSCONV IS (NPOIN2,NPLAN-1)
!
      IF(CONV(ADV_LPO).OR.CONV(ADV_LPO_TF)) THEN
        IS=MESH2D%NSEG*NPLAN
        DO IP=1,NPLAN-1
          DO I=1,NPOIN2
            IWS=I+(IP-1)*NPOIN2
C           NOTE 1: WSCONV IS ALREADY ASSEMBLED
C                   USING VOLU2D FLODEL WILL BE THE NON ASSEMBLED FORM
C           NOTE 2: WE COULD KEEP THE ORIGINAL RIGHT HAND SIDE IN
C                   TRIDW2
C           NOTE 3: AGAIN CONVENTION REVERSED, HERE FLOW FROM
C                   POINT 2 (UP) TO POINT 1 (DOWN)
            FLODEL%R(IS+IWS)=-WSCONV%R(IWS)*VOLU2D%R(I)
          ENDDO
        ENDDO
C       CALL BYPASS: OPTIONAL BUT SAVES ITERATIONS
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          CALL BYPASS_CRUSHED_POINTS_SEG(VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                                   FLODEL%R,
     &                                   T3_01,MESH2D,MESH3D,
     &                                   NPOIN3,ADV_LPO_TF,NPOIN2,
     &                                   MESH3D%GLOSEG%I,
     &                                   MESH3D%GLOSEG%DIM1,
     &                                   MESH2D%NSEG,NPLAN)
        ENDIF
      ENDIF
C     FLOPAR = FLODEL ASSEMBLED IN PARALLEL MODE
      IF(OPTHNEG.EQ.2.OR.CONV(ADV_LPO).OR.CONV(ADV_LPO_TF)) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=FLOPAR,Y=FLODEL)
          CALL PARCOM2_SEG(FLOPAR%R,FLOPAR%R,FLOPAR%R,
     &                     MESH2D%NSEG,NPLAN,2,1,MESH2D,1)
        ELSE
          FLOPAR%R=>FLODEL%R
        ENDIF
      ENDIF
!
!=======================================================================
C     PREPARES ADVECTION BY SUPG METHOD
!=======================================================================
!
      IF(CONV(ADV_SUP)) THEN
!
         IF(OPTSUP(1).EQ.2) THEN
C          HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=CFL)
           FORMUL = 'MAUGUG2         '
           CALL MATRIX
     &     (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,0.5D0*DT,SVIDE,SVIDE,
     &      SVIDE,UCONVC,VCONVC,WSCONV,MESH3D,MSK,MASKEL)
C          MSUPG IS SYMMETRICAL
         ELSEIF(OPTSUP(1).EQ.1) THEN
C          HORIZONTAL UPWIND (HERE UPWIND COEFFICIENT=1)
           FORMUL = 'MAUGUG1         '
           CALL MATRIX
     &     (MSUPG,'M=N     ',FORMUL,IELM3,IELM3,1.D0,SVIDE,SVIDE,
     &      SVIDE,UCONVC,VCONVC,WSCONV,MESH3D,MSK,MASKEL)
C          MSUPG IS NOT SYMMETRICAL
         ELSEIF(OPTSUP(1).NE.0) THEN
           CALL PLANTE(1)
           STOP 'UNEXPECTED VALUE OF OPTSUP IN PRECON'
         ENDIF
!
C        MSUPG TRANSFORMED INTO NON SYMMETRICAL MATRIX
         IF(OPTSUP(1).EQ.2) THEN
           CALL OM('M=X(M)  ',MSUPG,MSUPG,SVIDE,0.D0,MESH3D)
           OPER='M=M+N   '
         ELSEIF(OPTSUP(1).EQ.1) THEN
           OPER='M=M+N   '
         ELSE
           OPER='M=N     '
         ENDIF
!
C        ADDS CENTRED ADVECTION TERM
!
         FORMUL = 'MATVGR          '
         FORMUL(8:8) = '2'
         CALL MATRIX
     &   (MSUPG,OPER,FORMUL,IELM3,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &    UCONVC,VCONVC,WSCONV,MESH3D,MSK,MASKEL)
!
C        VERTICAL UPWIND (SUBROUTINE UPWIND EXPECTS SYMMETRICAL MATRICES)
C        HERE UPWIND COEFFICIENT = 1, BUT WSCONV USED INSTEAD OF W
!
         CALL UPWIND(MSUPG,WSCONV,1.D0,MESH2D,MESH3D,NPLAN)
!
      ENDIF
!
!=======================================================================
!
C     RESTORES MESH3D%Z
!
      MESH3D%Z%R=>SAVEZ
!
!=======================================================================
!
C     COMPUTES DELTAZ*WSTAR (IN WPS) AT NODES
!
      CALL WSTAR(WPS,WSCONV,Z,NPOIN2,NPLAN)
!
!=======================================================================
C A.D. MODIF 25/11/04
!
C     COMPUTES W FROM  (DZW*)JH,IV+1/2
!
C        (WITH HYDROSTATIC ASSUMPTION, W IS NEVER USED,
C                  IT IS DONE HERE FOR OUTPUTS)
C        HOWEVER IT IS ALWAYS USED WITH THE K-EPSILON OR K-OMEGA MODELS
!
      IF(.NOT.NONHYD) THEN
        IF(((LT/GRAPRD)*GRAPRD.EQ.LT.AND.LT.GE.GRADEB).OR.
     &      (ITURBV.EQ.3.OR.ITURBV.EQ.7)) THEN
          CALL WSTARW(WP,WSCONV,T3_03%R,T3_04%R,T3_05%R)
        ENDIF
      ENDIF
!
!=======================================================================
!
C ADVECTION BY METHOD OF CHARACTERISTICS
!
!=======================================================================
!
      IF(CONV(ADV_CAR)) THEN
!
C       NOTES: 1) IN BLOCK FN3D THERE IS U,V,W INSTEAD OF UN,VN,WN
C              BECAUSE ADVECTION IS DONE FOR THE NEXT TIME STEP
!
C              2) TRACERS IN BLOCK TAN WILL BE USED AND ARE NOT
C              INITIALISED IN THE FIRST CALL TO PRECON
!
        IF(NTRAC.NE.0.AND.LT.EQ.0.AND.SCHCTA.EQ.ADV_CAR) THEN
          CALL OS ('X=Y     ',X=TAN,Y=TA)
        ENDIF
!
        CALL CHARAC(FN3D,FC3D,FC3D%N,UCONVC,VCONVC,WPS,ZCHAR,
     &              DT,MESH3D%IFABOR,IELM3,NPOIN2,NPLAN,NPLINT,
     &              MSK,MASKEL,MTRA2%X,MTRA2%D,TRAV3,
     &              IT1%I,IT2%I,IT3%I,IT4%I,
     &              MESH3D,NELEM2,MESH2D%NELMAX,IKLE2,MESH2D%SURDET)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C