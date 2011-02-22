C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE RIGHT HAND SIDE OF THE ADJOINT SYSTEM
!>                IN MATRIX FORM.
!>  @code
!>        T      N-1    T     N-1    T     N-1        *
!>         AM1  P     +  CM1 Q     +  CM2 R     =  CV1
!>
!>        T     N-1     T     N-1                     *
!>         BM1 P      +  AM2 Q                  =  CV2
!>
!>        T     N-1                  T     N-1        *
!>         BM2 P                   +  AM3 R     =  CV3
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A23, A32, ADJDIR, ADJO, AGGLOC, AGGLOU, ALIRE, ALPHA1, ALPHA2, ALPHA3, AM1, AM2, AM3, AT, ATMOS, AUBOR, BD, BILMAS, BM1, BM2, C0, CF, CFBOR, CFLMAX, CHBORD, CHESTR, CM1, CM2, CONVV, CORCON, COTOND, CV1, CV2, CV3, DH, DHN, DIFVIT, DIRBOR, DP, DT, DU, DV, EQUA, ESTIME, FU, FV, GRAV, H, H0, HBOR, HD, HFROT, HH, HIT1, HN, HPROP, HTILD, ICONVF, INFOGR, IORDRH, IORDRU, ISOUSI, ITURB, KARMAN, KDIR, KFROT, LIMPRO, LINDNER, LISRUG, LT, MASK, MASKEL, MASKPT, MASSES, MAT, MATADJ, MAXVAR, MBOR, MESH, MSK, NDEF, NFRLIQ, NIT, NREF, NRES, NVARRES, OPDVIT, OPTBAN, OPTCOST, OPTSOU, OPTSUP, OUTINI, PATMOS, PP, PRECCU, PRIVE, QQ, RHS, RO, ROEAU, ROVAR, RR, S, SB, SLVPRO, SMH, SOLSYS, SP, T1, T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, TAM1, TAM2, TAM3, TB, TBM1, TBM2, TCM1, TCM2, TE1, TE2, TE3, TE4, TE5, TETAD, TETAH, TETAHC, TETAU, TEXREF, TEXRES, TEXTE, TM1, TROUVE, U, UBOR, UCONV, UD, UIT1, UN, UNK, UNKADJ, UNSV2D, UTILD, UU, V, VARCL, VARCLA, VARSOR, VBOR, VCONV, VD, VERTIC, VISC, VISC_S, VIT1, VN, VTILD, VV, W, W1, YASMH, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AT1, C, FORMULE, HIST, HOND, I, IELMH, IELMU, ITER, MSKGRA, SL1, SL1U, UDDL, UDIR, UNEU, UNONNEU, VDDL, VDIR, VK, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PROPAG_ADJ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COST_FUNCTION(), CPSTVC(), DIRICH(), FRICTI(), FRICTION_UNIF(), IELBOR(), INCLU2(), LITENR(), MATRIX(), MATVEC(), MESURES(), OM(), OS(), OSDB(), PLANTE(), SLOPES(), SOLVE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center>                                           </center>
!> </td><td> 13/11/2000
!> </td><td>
!> </td><td> COMPLETE VERSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 18/09/2000
!> </td><td> A LEOPARDI (UNINA)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 24/04/1997
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A23,A32
!></td><td><-></td><td>MATRICES
!>    </td></tr>
!>          <tr><td>ADJDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ADJO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AGGLOC
!></td><td>--></td><td>COEFFICIENT DE MASS-LUMPING SUR H
!>    </td></tr>
!>          <tr><td>AGGLOU
!></td><td>--></td><td>COEFFICIENT DE MASS-LUMPING SUR U
!>    </td></tr>
!>          <tr><td>ALIRE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ALPHA1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ALPHA2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ALPHA3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AM1,2,3
!></td><td><-></td><td>MATRICES
!>    </td></tr>
!>          <tr><td>AM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ATMOS
!></td><td>--></td><td>LOGIQUE INDIQUANT SI PATMOS EST REMPLI.
!>    </td></tr>
!>          <tr><td>AUBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR LE FROTTEMENT.
!>    </td></tr>
!>          <tr><td>BD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BILMAS
!></td><td>--></td><td>INDIQUE SI ON FAIT LE BILAN DE MASSE
!>    </td></tr>
!>          <tr><td>BM1,2
!></td><td><-></td><td>MATRICES
!>    </td></tr>
!>          <tr><td>BM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>C0
!></td><td>--></td><td>CELERITE DE REFERENCE
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CFLMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHBORD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT AU FOND.
!>    </td></tr>
!>          <tr><td>CM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CONVV
!></td><td>--></td><td>LOGIQUES INDIQUANT LES VARIABLES QUE L'ON
!>                  VEUT CONVECTER
!>                  CONVV(1):U,V CONVV(2):H
!>    </td></tr>
!>          <tr><td>CORCON
!></td><td>--></td><td>CORRECTION DE CONTINUITE SUR LES POINTS A
!>                  HAUTEUR IMPOSEE (ON CORRIGE LES VITESSES)
!>    </td></tr>
!>          <tr><td>COSUPG
!></td><td>--></td><td>COEFFICIENTS DE DECENTREMENT POUR S.U.P.G
!>                  TABLEAU DE 4 REELS COMPRIS ENTRE 0 ET 1
!>                  COSUPG(1) : U,V
!>    </td></tr>
!>          <tr><td>COTOND
!></td><td><--</td><td>EXPRESSION DE CU/G DANS LA THEORIE DE L'ONDE
!>    </td></tr>
!>          <tr><td>CV1,CV2,CV3
!></td><td><-></td><td>SECONDS MEMBRES DU SYSTEME.
!>    </td></tr>
!>          <tr><td>DH,DHN
!></td><td><--</td><td>STOCKAGE DE LA VARIABLE DH  (DHN AU TEMPS N)
!>    </td></tr>
!>          <tr><td>DIFVIT
!></td><td>--></td><td>INDIQUE S'IL FAUT FAIRE LA DIFFUSION DE U,V
!>    </td></tr>
!>          <tr><td>DIRBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DU,DV
!></td><td><--</td><td>STOCKAGE DES QCCROISSEMENTS EN U ET V
!>    </td></tr>
!>          <tr><td>EQUA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ESTIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FU,FV
!></td><td><-></td><td>TERMES SOURCES TRAITES EN P1
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>CONSTANTE DE GRAVITE .
!>    </td></tr>
!>          <tr><td>H0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HAULIN
!></td><td>--></td><td>HAUTEUR DE REFERENCE.
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR H.
!>    </td></tr>
!>          <tr><td>HD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>--></td><td>HAUTEUR DE PROPAGATION
!>    </td></tr>
!>          <tr><td>HTILD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICONVF
!></td><td>--></td><td>FORME DE LA CONVECTION
!>                  TABLEAU DE 4 VALEURS ENTIERES POUR :
!>                  ICONVF(1) : U ET V
!>                  ICONVF(2) : H
!>                  ICONVF(3) : TRACEUR
!>                  ICONVF(4) : K ET EPSILON
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>INFORMATIONS SUR LE GRADIENT (LOGIQUE)
!>    </td></tr>
!>          <tr><td>IORDRH
!></td><td>--></td><td>ORDRE DU TIR INITIAL POUR H
!>    </td></tr>
!>          <tr><td>IORDRU
!></td><td>--></td><td>ORDRE DU TIR INITIAL POUR U
!>    </td></tr>
!>          <tr><td>ISOUSI
!></td><td>--></td><td>NUMERO DE LA SOUS-ITERATION DANS LE PAS
!>                  DE TEMPS.
!>    </td></tr>
!>          <tr><td>ITURB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>CONSTANTE DE KARMAN.
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONDITION A LA LIMITE DE TYPE DIRICHLET
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT SUR LE FOND
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES
!>    </td></tr>
!>          <tr><td>LINDNER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LISRUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT,AT,DT
!></td><td>--></td><td>NUMERO D'ITERATION, TEMPS, PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>--></td><td>BLOC DE MASQUES POUR LES SEGMENTS :
!>                  MASK(MSK1): 1. SI KDIR SUR U 0. SINON
!>                  MASK(MSK2): 1. SI KDIR SUR V 0. SINON
!>                  MASK(MSK3): 1. SI KDDL SUR U 0. SINON
!>                  MASK(MSK4): 1. SI KDDL SUR V 0. SINON
!>                  MASK(MSK6): 1. SI KNEU SUR V 0. SINON
!>                  MASK(MSK7): 1. SI KOND 0. SINON
!>                  MASK(MSK9): 1. SI KDIR SUR H (POINT)
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>MASQUES PAR POINTS.
!>    </td></tr>
!>          <tr><td>MASSES
!></td><td>--></td><td>MASSE CREEE PAR TERME SOURCE PENDANT
!>                  LE PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>MAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MATADJ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NDEF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>NREF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVARRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPDVIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>--></td><td>OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
!>                  NON UTILISE POUR L'INSTANT :
!>    </td></tr>
!>          <tr><td>OPTCOST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTHYB
!></td><td>--></td><td>OPTION DU SCHEMA HYBRIDE
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTSUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OUTINI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PATMOS
!></td><td>--></td><td>TABLEAU DE VALEURS DE LA PRESSION ATMOSPHER.
!>    </td></tr>
!>          <tr><td>PP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PRECCU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU DE TRAVAIL DEFINI DANS PRINCI
!>    </td></tr>
!>          <tr><td>PROLIN
!></td><td>--></td><td>INDIQUE SI LA PROPAGATION EST LINEARISEE
!>    </td></tr>
!>          <tr><td>QQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RHS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RO
!></td><td>--></td><td>MASSE VOLUMIQUE SI ELLE VARIABLE
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU.
!>    </td></tr>
!>          <tr><td>ROVAR
!></td><td>--></td><td>OUI SI LA MASSE VOLUMIQUE EST VARIABLE.
!>    </td></tr>
!>          <tr><td>RR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>STRUCTURE BIDON
!>    </td></tr>
!>          <tr><td>SB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLVPRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERMES SOURCES DE L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>SOLSYS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T10
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T11
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T9
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TBM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TBM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TCM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TCM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETAD
!></td><td>--></td><td>IMPLICITATION SUR LA DIFFUSION (=1.)
!>    </td></tr>
!>          <tr><td>TETAH
!></td><td>--></td><td>IMPLICITATION SUR H DANS L'EQUATION SUR U
!>    </td></tr>
!>          <tr><td>TETAHC
!></td><td>--></td><td>IMPLICITATION SUR H DANS LA CONTINUITE
!>    </td></tr>
!>          <tr><td>TETAU
!></td><td>--></td><td>IMPLICITATION SUR U ET V
!>    </td></tr>
!>          <tr><td>TEXREF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TM1
!></td><td><-></td><td>MATRICE
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U ,V ,H
!></td><td><--</td><td>VALEURS A L' ETAPE N+1 (System direct) OR
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR U.
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>CHAMP CONVECTEUR
!>    </td></tr>
!>          <tr><td>UD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UIT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UN,VN,HN
!></td><td>--></td><td>VALEURS A L' ETAPE N.
!>    </td></tr>
!>          <tr><td>UNK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNKADJ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UTILD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARCL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARCLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARSOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR V.
!>    </td></tr>
!>          <tr><td>VD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VERTIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>--></td><td>VISCOSITE TURBULENTE .
!>    </td></tr>
!>          <tr><td>VISC_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VIT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VTILD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>INDIQUE SI ON PREND EN COMPTE SMH
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FONT AU NOEUD DE MAILLAGE .
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PROPAG_ADJ
     &(UCONV,VCONV,CONVV,H0,C0,COTOND,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     & TETAH,TETAHC,TETAU,TETAD,
     & AGGLOC,AGGLOU,KDIR,INFOGR,KFROT,ICONVF,
     & PRIVE,ISOUSI,BILMAS,MASSES,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,BD,PRECCU,SOLSYS,CFLMAX,OPDVIT,OPTSOU,
     & NFRLIQ,SLVPRO,EQUA,VERTIC,
     & ADJO,UD,VD,HD,U,V,H,UU,VV,HH,UIT1,VIT1,HIT1,PP,QQ,RR,
     & TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,
     & TCM2,MATADJ,UNKADJ,ALPHA1,ALPHA2,ALPHA3,ADJDIR,ESTIME,OPTCOST,
     & NIT,NVARRES,VARSOR,
     & NRES,NREF,ALIRE,TROUVE,MAXVAR,VARCL,VARCLA,
     & TEXTE,TEXREF,TEXRES,W,OUTINI,CHESTR,KARMAN,NDEF,
     & ITURB,LISRUG,LINDNER,SB,DP,SP,CHBORD,CFBOR,HFROT,UNSV2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A23,A32        |<->| MATRICES
C| ADJDIR         |---| 
C| ADJO           |---| 
C| AGGLOC         |-->| COEFFICIENT DE MASS-LUMPING SUR H
C| AGGLOU         |-->| COEFFICIENT DE MASS-LUMPING SUR U
C| ALIRE          |---| 
C| ALPHA1         |---| 
C| ALPHA2         |---| 
C| ALPHA3         |---| 
C| AM1,2,3        |<->| MATRICES
C| AM2            |---| 
C| AM3            |---| 
C| ATMOS          |-->| LOGIQUE INDIQUANT SI PATMOS EST REMPLI.
C| AUBOR          |-->| CONDITIONS AUX LIMITES SUR LE FROTTEMENT.
C| BD             |---| 
C| BILMAS         |-->| INDIQUE SI ON FAIT LE BILAN DE MASSE
C| BM1,2          |<->| MATRICES
C| BM2            |---| 
C| C0             |-->| CELERITE DE REFERENCE
C| CF             |---| 
C| CFBOR          |---| 
C| CFLMAX         |---| 
C| CHBORD         |---| 
C| CHESTR         |-->| COEFFICIENT DE FROTTEMENT AU FOND.
C| CM1            |---| 
C| CM2            |---| 
C| CONVV          |-->| LOGIQUES INDIQUANT LES VARIABLES QUE L'ON
C|                |   | VEUT CONVECTER
C|                |   | CONVV(1):U,V CONVV(2):H
C| CORCON         |-->| CORRECTION DE CONTINUITE SUR LES POINTS A
C|                |   | HAUTEUR IMPOSEE (ON CORRIGE LES VITESSES)
C| COSUPG         |-->| COEFFICIENTS DE DECENTREMENT POUR S.U.P.G
C|                |   | TABLEAU DE 4 REELS COMPRIS ENTRE 0 ET 1
C|                |   | COSUPG(1) : U,V
C| COTOND         |<--| EXPRESSION DE CU/G DANS LA THEORIE DE L'ONDE
C| CV1,CV2,CV3    |<->| SECONDS MEMBRES DU SYSTEME.
C| DH,DHN         |<--| STOCKAGE DE LA VARIABLE DH  (DHN AU TEMPS N)
C| DIFVIT         |-->| INDIQUE S'IL FAUT FAIRE LA DIFFUSION DE U,V
C| DIRBOR         |---| 
C| DP             |---| 
C| DU,DV          |<--| STOCKAGE DES QCCROISSEMENTS EN U ET V
C| EQUA           |---| 
C| ESTIME         |---| 
C| FU,FV          |<->| TERMES SOURCES TRAITES EN P1
C| GRAV           |-->| CONSTANTE DE GRAVITE .
C| H0             |---| 
C| HAULIN         |-->| HAUTEUR DE REFERENCE.
C| HBOR           |-->| CONDITIONS AUX LIMITES SUR H.
C| HD             |---| 
C| HFROT          |---| 
C| HH             |---| 
C| HIT1           |---| 
C| HPROP          |-->| HAUTEUR DE PROPAGATION
C| HTILD          |---| 
C| ICONVF         |-->| FORME DE LA CONVECTION
C|                |   | TABLEAU DE 4 VALEURS ENTIERES POUR :
C|                |   | ICONVF(1) : U ET V
C|                |   | ICONVF(2) : H
C|                |   | ICONVF(3) : TRACEUR
C|                |   | ICONVF(4) : K ET EPSILON
C| INFOGR         |-->| INFORMATIONS SUR LE GRADIENT (LOGIQUE)
C| IORDRH         |-->| ORDRE DU TIR INITIAL POUR H
C| IORDRU         |-->| ORDRE DU TIR INITIAL POUR U
C| ISOUSI         |-->| NUMERO DE LA SOUS-ITERATION DANS LE PAS
C|                |   | DE TEMPS.
C| ITURB          |---| 
C| KARMAN         |-->| CONSTANTE DE KARMAN.
C| KDIR           |-->| CONDITION A LA LIMITE DE TYPE DIRICHLET
C| KFROT          |-->| LOI DE FROTTEMENT SUR LE FOND
C| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
C| LINDNER        |---| 
C| LISRUG         |---| 
C| LT,AT,DT       |-->| NUMERO D'ITERATION, TEMPS, PAS DE TEMPS
C| MASK           |-->| BLOC DE MASQUES POUR LES SEGMENTS :
C|                |   | MASK(MSK1): 1. SI KDIR SUR U 0. SINON
C|                |   | MASK(MSK2): 1. SI KDIR SUR V 0. SINON
C|                |   | MASK(MSK3): 1. SI KDDL SUR U 0. SINON
C|                |   | MASK(MSK4): 1. SI KDDL SUR V 0. SINON
C|                |   | MASK(MSK6): 1. SI KNEU SUR V 0. SINON
C|                |   | MASK(MSK7): 1. SI KOND 0. SINON
C|                |   | MASK(MSK9): 1. SI KDIR SUR H (POINT)
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKPT         |-->| MASQUES PAR POINTS.
C| MASSES         |-->| MASSE CREEE PAR TERME SOURCE PENDANT
C|                |   | LE PAS DE TEMPS.
C| MAT            |---| 
C| MATADJ         |---| 
C| MAXVAR         |---| 
C| MBOR           |---| 
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NDEF           |---| 
C| NFRLIQ         |---| 
C| NIT            |---| 
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| NREF           |---| 
C| NRES           |---| 
C| NVARRES        |---| 
C| OPDVIT         |---| 
C| OPTBAN         |-->| OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
C|                |   | NON UTILISE POUR L'INSTANT :
C| OPTCOST        |---| 
C| OPTHYB         |-->| OPTION DU SCHEMA HYBRIDE
C| OPTSOU         |---| 
C| OPTSUP         |---| 
C| OUTINI         |---| 
C| PATMOS         |-->| TABLEAU DE VALEURS DE LA PRESSION ATMOSPHER.
C| PP             |---| 
C| PRECCU         |---| 
C| PRIVE          |-->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
C| PROLIN         |-->| INDIQUE SI LA PROPAGATION EST LINEARISEE
C| QQ             |---| 
C| RHS            |---| 
C| RO             |-->| MASSE VOLUMIQUE SI ELLE VARIABLE
C| ROEAU          |-->| MASSE VOLUMIQUE DE L'EAU.
C| ROVAR          |-->| OUI SI LA MASSE VOLUMIQUE EST VARIABLE.
C| RR             |---| 
C| S             |-->| STRUCTURE BIDON
C| SB             |---| 
C| SLVPRO         |---| 
C| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
C| SOLSYS         |---| 
C| SP             |---| 
C| T1             |---| 
C| T10            |---| 
C| T11            |---| 
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| T7             |---| 
C| T8             |---| 
C| T9             |---| 
C| TAM1           |---| 
C| TAM2           |---| 
C| TAM3           |---| 
C| TB             |---| 
C| TBM1           |---| 
C| TBM2           |---| 
C| TCM1           |---| 
C| TCM2           |---| 
C| TE1            |---| 
C| TE2            |---| 
C| TE3            |---| 
C| TE4            |---| 
C| TE5            |---| 
C| TETAD          |-->| IMPLICITATION SUR LA DIFFUSION (=1.)
C| TETAH          |-->| IMPLICITATION SUR H DANS L'EQUATION SUR U
C| TETAHC         |-->| IMPLICITATION SUR H DANS LA CONTINUITE
C| TETAU          |-->| IMPLICITATION SUR U ET V
C| TEXREF         |---| 
C| TEXRES         |---| 
C| TEXTE          |---| 
C| TM1            |<->| MATRICE
C| TROUVE         |---| 
C| U ,V ,H        |<--| VALEURS A L' ETAPE N+1 (System direct) OR
C| UBOR           |-->| CONDITIONS AUX LIMITES SUR U.
C| UCONV,VCONV    |-->| CHAMP CONVECTEUR
C| UD             |---| 
C| UIT1           |---| 
C| UN,VN,HN       |-->| VALEURS A L' ETAPE N.
C| UNK            |---| 
C| UNKADJ         |---| 
C| UNSV2D         |---| 
C| UTILD          |---| 
C| UU             |---| 
C| VARCL          |---| 
C| VARCLA         |---| 
C| VARSOR         |---| 
C| VBOR           |-->| CONDITIONS AUX LIMITES SUR V.
C| VD             |---| 
C| VERTIC         |---| 
C| VISC           |-->| VISCOSITE TURBULENTE .
C| VISC_S         |---| 
C| VIT1           |---| 
C| VTILD          |---| 
C| VV             |---| 
C| W             |---| 
C| W1             |<->| TABLEAU DE TRAVAIL.
C| YASMH          |-->| INDIQUE SI ON PREND EN COMPTE SMH
C| ZF             |-->| COTE DU FONT AU NOEUD DE MAILLAGE .
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_PROPAG_ADJ => PROPAG_ADJ
      USE DECLARATIONS_TELEMAC2D, ONLY : KFROTL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LT,OPTSUP(4),KDIR,KFROT,ICONVF(4)
      INTEGER, INTENT(IN) :: IORDRH,IORDRU,ISOUSI,OPTBAN,OPTSOU,SOLSYS
      INTEGER, INTENT(IN) :: OPDVIT,NFRLIQ,LISRUG,ITURB,OPTCOST
      INTEGER, INTENT(IN)    :: NIT,NRES,NREF,MAXVAR,HFROT
      INTEGER, INTENT(INOUT) :: NVARRES,TROUVE(*),ALIRE(*)
      LOGICAL, INTENT(IN)    :: BILMAS,ATMOS,DIFVIT,INFOGR,CONVV(4),MSK
      LOGICAL, INTENT(IN)    :: YASMH,ROVAR,PRECCU,VERTIC,ADJO,CORCON
      LOGICAL, INTENT(IN)    :: OUTINI,LINDNER
      DOUBLE PRECISION, INTENT(IN)    :: TETAU,TETAD,TETAH,AGGLOC,AGGLOU
      DOUBLE PRECISION, INTENT(IN)    :: TETAHC,AT,DT,GRAV,ROEAU,CFLMAX
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,NDEF,DP,SP
      DOUBLE PRECISION, INTENT(INOUT) :: MASSES,SB
      TYPE(SLVCFG), INTENT(INOUT)     :: SLVPRO
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      TYPE(BIEF_OBJ), INTENT(IN)      :: UCONV,VCONV,SMH,UN,VN,HN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: RO
      TYPE(BIEF_OBJ), INTENT(IN)      :: UTILD,VTILD,PATMOS,CF,UNSV2D
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: U,V,H,CV1,CV2,CV3,PRIVE,DH,DHN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DU,DV,FU,FV,VISC,VISC_S,HTILD
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: UBOR,VBOR,HBOR,AUBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,MASKPT,ZF
      TYPE(BIEF_OBJ), INTENT(IN)      :: HPROP,H0,C0,COTOND,LIMPRO
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T11
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TE1,TE2,TE3,TE4,TE5
C     STRUCTURES OF MATRICES
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TAM1,TAM2,TAM3,TBM1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TBM2,TCM1,TCM2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: A23,A32,MBOR
C
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MASK,MAT,RHS,UNK,TB,BD,DIRBOR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: CHESTR
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: HD,UD,VD,ALPHA1,ALPHA2,ALPHA3
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: HH,UU,VV,UIT1,VIT1,HIT1
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: PP,QQ,RR,CHBORD,CFBOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: W1
      TYPE(BIEF_OBJ), INTENT(IN)      :: S
      REAL,  INTENT(INOUT)            :: W(*)
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: VARSOR
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: MATADJ,UNKADJ,ADJDIR,VARCL
      CHARACTER(LEN=72), INTENT(IN)   :: ESTIME
      CHARACTER(LEN=32), INTENT(INOUT):: VARCLA(10),TEXTE(*)
      CHARACTER(LEN=32), INTENT(INOUT):: TEXREF(*),TEXRES(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITER,I,IELMU,IELMH
      INTEGER UDIR,UDDL,UNEU,HOND,UNONNEU,VDIR,VDDL
C
      DOUBLE PRECISION Z(1),SL1,SL1U,C,AT1,HIST(1)
C
      LOGICAL MSKGRA
C
      CHARACTER*16 FORMULE
C
C-----------------------------------------------------------------------
C
C FH-FRDATA
      DOUBLE PRECISION, PARAMETER :: VK = 1.D-6
C FH-FRDATA
C-----------------------------------------------------------------------
C
      IELMH=HH%ELM
      IELMU=UU%ELM
C
C  ADDRESSES OF THE ARRAYS IN THE MASKING BLOCK: MASK
C
      UDIR = 1
      VDIR = 2
      UDDL = 3
      VDDL = 4
      UNEU = 5
C     VNEU = 6
      HOND = 7
      UNONNEU = 8
C
C-----------------------------------------------------------------------
C
      IF(SOLSYS.NE.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TRAITEMENT DU SYSTEME LINEAIRE : ',SOLSYS
          WRITE(LU,*) 'CAS NON PREVU EN MODE ESTIMATION'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TREATMENT OF THE LINEAR SYSTEM : ',SOLSYS
          WRITE(LU,*) 'UNEXPECTED CASE IN ESTIMATION MODE'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     COMPUTES MATRIX FOR ADJOINT SYSTEM
C
      CALL OM( 'M=TN    ' , TAM1, AM1, S, C, MESH )
      CALL OM( 'M=TN    ' , TAM2, AM2, S, C, MESH )
      CALL OM( 'M=TN    ' , TAM3, AM3, S, C, MESH )
      CALL OM( 'M=TN    ' , TBM1, BM1, S, C, MESH )
      CALL OM( 'M=TN    ' , TBM2, BM2, S, C, MESH )
      CALL OM( 'M=TN    ' , TCM1, CM1, S, C, MESH )
      CALL OM( 'M=TN    ' , TCM2, CM2, S, C, MESH )
C
C=======================================================================
C
C     COMPUTES RIGHT HAND SIDES FOR ADJOINT SYSTEM
C
C=======================================================================
C
C     NB: HIT1, UIT1, VIT1 ARE DIRECT VARIABLES AT TIME IT+1
C         HH  , UU  , VV   ARE DIRECT VARIABLES AT TIME IT
C         HN  , UN  , VN   ARE DIRECT VARIABLES AT TIME IT-1
C
C
C           IT    IT    IT
C  TERMS 2 W   ( X   - M   ) OR EQUIVALENT DEPENDING ON THE COST FUNCTION
C           IP    IP    IP
C
C     INITIALISES CV1, CV2 AND CV3
C     IN STEADY STATE MODE, WEIGHTS ALPHA1, ALPHA2 AND ALPHA3 ARE
C     INITIALISED BY A CALL TO "MESURES" IN HOMERE_T2D_ADJ, IN THE LOOP
C     FOR THE COMPUTATION OF THE COST FUNCTION. THEN THEY ARE CANCELLED
C     AT THE END OF THIS ROUTINE
C
      CALL COST_FUNCTION(C,OPTCOST,'RHS')
C
C-----------------------------------------------------------------------
C
C  PREPARES FRICTION TERMS AND VECTOR T1 EQUAL TO 0
C
C     T10 = MASS MATRIX LUMPED / COS(SLOPE)
      CALL SLOPES(TE3,ZF,MESH)
      CALL VECTOR(T10,'=','MASBAS          ',IELMU,1.D0,T2,
     &                T2,T2,T2,T2,T2,MESH,.TRUE.,TE3)
C     FU IN T11 (AND FV=FU)
C
C     T2 WILL HOLD CF AT ITERATION IT+1
C
      CALL CPSTVC(CF,T2)
C
CFH-FRDATA
C     CALL COEFRO(T2,HH,UU,VV,KARMAN,KFROT,CHESTR,GRAV,MESH,T1)
      CALL FRICTION_UNIF(MESH,HH,UU,VV,CHESTR,S,KFROT,KFROTL,0,LISRUG,
     &                   .FALSE.,SB,NDEF,DP,SP,VK,KARMAN,GRAV,
     &                   T1,T2,CHBORD,T2,CFBOR)
CFH-FRDATA
C
      CALL FRICTI(T11,T3,T4,T5,UU,VV,HH,T2,MESH,T6,T7,VERTIC,UNSV2D,
     &            MSK,MASKEL,HFROT)
C     CALL FRICTI(T11,T3,T4,T5,UU,VV,HH,CF,MESH,T6,VERTIC)
C
C     FINAL FU OF PHD IN T11
      CALL OS('X=XY    ', T11 , T10 , T10 , C )
C
C     T1 : 0 VECTOR
      CALL CPSTVC(HH,T1)
      CALL OS('X=C     ' , T1 , T1 , T1 , 0.D0)
C
C-----------------------------------------------------------------------
C
C  COMPUTES CV1 : CV1 = CV1 + T11+T12+T13+T14+T15+T16+T17
C
C-----------------------------------------------------------------------
C  TERM T11 (3 PARTS)
C-----------------------------------------------------------------------
C
C  T11_1 : M/DT  * H
C                   ADJ
C     TERM 1B OF AL
      CALL MATRIX(AM1,'M=N     ','MATMAS          ',IELMH,IELMH,
     &            1.D0/DT,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , AM1 , PP , C , MESH)
C
C  T11_2 : ADVECTION
C
C  T11_3 :
C
C     8B OF AL
C     CALL MATRIX(BM1,'M=N     ','MATGRF         X',IELMH,IELMH,
C    *           (TETAU-1.D0),UU,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
C     CALL MATVEC('X=X+AY  ', CV1 , BM1 , PN , C , MESH)
C     9B OF AL
C     CALL MATRIX(BM2,'M=N     ','MATGRF         Y',IELMH,IELMH,
C    *           (TETAU-1.D0),VV,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
C     CALL MATVEC('X=X+AY  ', CV1 , BM2 , PN , C , MESH)
C
C     CORRECTED VERSION JMH: NOW ICONVF(2) IS ALWAYS 5
C
C     IF(ICONVF(2).EQ.5.OR.ICONVF(2).EQ.8) THEN
        FORMULE='MATFGR          '
C     ELSE
C       FORMULE='MATGRF          '
C     ENDIF
C
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &           (TETAU-1.D0),PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM1 , UU , C , MESH)
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &           (TETAU-1.D0),PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM2 , VV , C , MESH)
C
C                           H
C  T11_4 : BOUNDARY TERM TB1
C                           ADJ
C
C     JMH : I HAVE -1.D0 INSTEAD OF TETAU-1.D0, BUT THEN HE SUBTRACTS TETAU ??
C           SAME THING EXCEPT FOR INCIDENT WAVE ???
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,UU,VV,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
C     DIRICHLET ON VELOCITY :
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,UU,VV,T1,MESH,
     &            .TRUE.,MASK%ADR(UDIR)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
C     FREE FLOW ON VELOCITY :
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,UU,VV,T1,MESH,
     &            .TRUE.,MASK%ADR(UDDL)%P)
      CALL OSDB('X=X+Y   ' , CV1 , T2 , T2 , C , MESH)
C
C-----------------------------------------------------------------------
C  TERM T12
C-----------------------------------------------------------------------
C
C     TERM 2B OF AL
      CALL MATVEC('X=X+CAY ',CV1,TCM1,QQ,(TETAH-1.D0)/TETAH,MESH)
C
C-----------------------------------------------------------------------
C  TERM T13
C-----------------------------------------------------------------------
C
C     TERM 3B OF AL
      CALL MATVEC('X=X+CAY ',CV1,TCM2,RR,(TETAH-1.D0)/TETAH,MESH)
C
C-----------------------------------------------------------------------
C  TERM T14
C-----------------------------------------------------------------------
C
C     TERM 1C OF AL
C     VERSION EB+AL, NOTE JMH : DON'T AGREE
C     CALL MATRIX(BM1,'M=N     ','MATGRF         X',IELMH,IELMH,
C    *            -TETAU,UN,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
C     CALL MATVEC('X=X+AY  ', CV1 , BM1 , PN , C , MESH)
C
C     VERSION JMH+CC
C                        START OF FORMULATION MADE FOR T11_3
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &            -TETAU,PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM1 , UIT1 , C , MESH)
C
C-----------------------------------------------------------------------
C  TERM T15 + T17
C-----------------------------------------------------------------------
C
C          IT+1   IT+1    IT+1    IT+1
C     T3= U    * Q     + V     * R
C
      CALL OS('X=YZ    ', T3 , UIT1 , QQ , C )
      CALL OS('X=X+YZ  ', T3 , VIT1 , RR , C )
C
C     T4=-(4/3)/H OR -1/H
      CALL OS('X=1/Y   ', T4 , HH , HH , C ,2,0.D0,1.D-6)
      IF(KFROT.EQ.3) THEN
        CALL OS('X=CX    ', T4 , T4 , T4 , -4.D0/3.D0)
      ELSEIF(KFROT.EQ.2) THEN
        CALL OS('X=CX    ', T4 , T4 , T4 , -1.D0     )
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'LOI NON TRAITEE POUR L''ESTIMATION'
        IF(LNG.EQ.2) WRITE(LU,*) 'WRONG FRICTION LAW FOR ESTIMATION'
        CALL PLANTE(1)
        STOP
      ENDIF
      CALL OS('X=XY    ', T4 , T11 , T11 , C )
C     AND IF T3 IS QUASI-BUBBLE?
      CALL OS('X=X+YZ  ', CV1 , T4 , T3  , C )
C
C-----------------------------------------------------------------------
C  TERM T16
C-----------------------------------------------------------------------
C
C     TERM 2C OF AL
C     VERSION EB+AL, NOTE JMH : DON'T AGREE
C     CALL MATRIX(BM2,'M=N     ','MATGRF         Y',IELMH,IELMH,
C    *            -TETAU,VN,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
C     CALL MATVEC('X=X+AY  ', CV1 , BM2 , PN , C , MESH)
C     VERSION JMH+CC
C                        START OF FORMULATION MADE FOR T11_3
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &            -TETAU,PP,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV1 , BM2 , VIT1 , C , MESH)
C
C-----------------------------------------------------------------------
C
C  COMPUTES CV2 AND CV3 :
C
C-----------------------------------------------------------------------
C  TERM  T21
C-----------------------------------------------------------------------
C
C  T21_1 : ADVECTION
C
C  T21_2 : (5B OF EB+AL)
C
      FORMULE(16:16)='X'
      CALL MATRIX(BM1,'M=N     ',FORMULE,IELMH,IELMU,
     &            1.D0,HH,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+CTAY',CV2,BM1,PP,TETAU-1.D0,MESH)
C OLD PROGRAMMING (TBM1 FROM HN AT AN INCORRECT TIMESTEP)
C     CALL MATVEC('X=X+CAY ',CV2,TBM1,PP,(TETAU-1.D0)/TETAU,MESH)
C
C                           U
C  T21_3 : BOUNDARY TERM TB1
C                           ADJ
C
C     JMH : I HAVE 1.D0 INSTEAD OF TETAU-1.D0
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,HH,T1,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
C     DIRICHLET ON VELOCITY U:
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,HH,T1,T1,MESH,
     &            .TRUE.,MASK%ADR(UDIR)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
C     FREE FLOW ON U:
      CALL VECTOR(T2,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,HH,T1,T1,MESH,
     &            .TRUE.,MASK%ADR(UDDL)%P)
      CALL OSDB('X=X+Y   ' , CV2 , T2 , T2 , C , MESH)
C
C-----------------------------------------------------------------------
C TERM  T31
C-----------------------------------------------------------------------
C
C  T31_1 : ADVECTION
C
C  T31_2 : (7B OF EB+AL)
C
      FORMULE(16:16)='Y'
      CALL MATRIX(BM2,'M=N     ',FORMULE,IELMH,IELMU,
     &            1.D0,HH,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+CTAY',CV3,BM2,PP,TETAU-1.D0,MESH)
C OLD PROGRAMMING (TBM2 FROM HN AT AN INCORRECT TIMESTEP)
C     CALL MATVEC('X=X+CAY ',CV3,TBM2,PP,(TETAU-1.D0)/TETAU,MESH)
C
C                           V
C  T31_3 : BOUNDARY TERM TB1
C                           ADJ
C
C     JMH : I HAVE 1.D0 INSTEAD OF TETAU-1.D0
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            TETAU-1.D0,PP,T1,T1,T1,HH,T1,MESH,.TRUE.,
     &            MASK%ADR(8)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
C     DIRICHLET ON VELOCITY V:
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,T1,HH,T1,MESH,
     &            .TRUE.,MASK%ADR(VDIR)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
C     FREE FLOW ON V:
      CALL VECTOR(T4,'=','FLUBDF          ',IELBOR(IELMH,1),
     &            -TETAU,PP,T1,T1,T1,HH,T1,MESH,
     &            .TRUE.,MASK%ADR(VDDL)%P)
      CALL OSDB('X=X+Y   ' , CV3 , T4 , T4 , C , MESH)
C
C-----------------------------------------------------------------------
C TERM  T22 (2 PARTS)
C-----------------------------------------------------------------------
C
C     TERM 4B OF AL       T
C     AM2 IS MASS/DT AT TIME IT+1
      CALL MATRIX(AM2,'M=N     ','MATMAS          ',IELMU,IELMU,
     &            1.D0/DT,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL)
      CALL MATVEC('X=X+AY  ', CV2 , AM2 , QQ , C , MESH)
C
C     MISSES AN ADVECTION TERM
C
C
C-----------------------------------------------------------------------
C TERM  T32 (2 PARTS)
C-----------------------------------------------------------------------
C
C     TERM 6B OF AL
C     AM2 IS MASS/DT AT TIME IT+1
      CALL MATVEC('X=X+AY  ', CV3 , AM2 , RR , C , MESH)
C
C     MISSES AN ADVECTION TERM
C
C-----------------------------------------------------------------------
C TERMS  T23 AND T33
C-----------------------------------------------------------------------
C
C   ADVECTION : NOT YET IMPLEMENTED
C
C-----------------------------------------------------------------------
C TERM  T24+T25 AND T34+T35
C-----------------------------------------------------------------------
C
C     T5=U/(U^2+V^2)  C     T6=V/(U^2+V^2)
      CALL OS('X=YZ    ', T7 , UU , UU , C    )
      CALL OS('X=X+YZ  ', T7 , VV , VV , C    )
      CALL OS('X=+(Y,C)', T7 , T7 , T7 , 1.D-6)
      CALL OS('X=Y/Z   ', T5 , UU , T7 , C    )
      CALL OS('X=Y/Z   ', T6 , VV , T7 , C    )
C
C     ADD TERMS TO CV2, CV3
C
C     T3=U*Q+V*R (ALREADY DONE)
      CALL OS('X=XY    ', T5 , T11 , T11 , C )
      CALL OS('X=XY    ', T6 , T11 , T11 , C )
      CALL OS('X=X+YZ  ', CV2 , T5 , T3  , C )
      CALL OS('X=X+YZ  ', CV3 , T6 , T3  , C )
C
C=======================================================================
C
C     END OF COMPUTATION OF RIGHT HAND SIDE FOR ADJOINT SYSTEM
C
C=======================================================================
C
C
C     DIRICHLET CONDITIONS FOR ADJOINT VARIABLES
      CALL OS ('X=C     ',ADJDIR,ADJDIR,ADJDIR,0.D0)
      CALL DIRICH(UNKADJ,MATADJ,RHS,ADJDIR,LIMPRO%I,
     &            TB,MESH,KDIR,MSK,MASKPT)
C
      CALL SOLVE(UNKADJ,MATADJ,RHS,TB,SLVPRO,INFOGR,MESH,TM1)
C
C     CONTRIBUTION TO COST FUNCTION
C
      CALL COST_FUNCTION(C,OPTCOST,'GRD')
C
C     PREPARES NEXT TIMESTEP
C
      CALL OS( 'X=Y     ' , HIT1 , HH  , HH  , C )
      CALL OS( 'X=Y     ' , UIT1 , UU  , UU  , C )
      CALL OS( 'X=Y     ' , VIT1 , VV  , VV  , C )
C
      IF(     INCLU2(ESTIME,'PERMANENT')
     &    .OR.INCLU2(ESTIME,'STEADY'   )  ) THEN
C
C      STEADY STATE : DOES NOT UPDATE DATA AND RESULTS,
C                     ONLY LAST TIMESTEP CONSIDERED
C
C      CALL OS( 'X=C     ' , ALPHA1 , ALPHA1 , ALPHA1 , 0.D0 )
C      CALL OS( 'X=C     ' , ALPHA2 , ALPHA2 , ALPHA2 , 0.D0 )
C      CALL OS( 'X=C     ' , ALPHA3 , ALPHA3 , ALPHA3 , 0.D0 )
C      U AND V MODIFIED BY BORD, RESET HERE (H USEFUL ??)
       CALL OS( 'X=Y     ' , H , HN  , HN  , C )
       CALL OS( 'X=Y     ' , U , UN  , UN  , C )
       CALL OS( 'X=Y     ' , V , VN  , VN  , C )
C
      ELSE
C
C      UNSTEADY STATE : UPDATES DATA AND RESULTS
C
       IF(LT.LT.NIT) THEN
C
C       HIT,.., HH,.. IN INITIAL CONDITIONS, SEE PROPIN_ADJ
        CALL OS( 'X=Y     ' , HH , HN  , HN  , C )
        CALL OS( 'X=Y     ' , UU , UN  , UN  , C )
        CALL OS( 'X=Y     ' , VV , VN  , VN  , C )
C
C       READS TELEMAC2D RESULTS (RESULTS FILE - UNIT NRES)
C       SEE ALSO CONDIN_ADJ
C
        DO I=1,2*(NVARRES+1)
          BACKSPACE NRES
        ENDDO
        CALL LITENR(VARSOR,VARCL,NRES,'STD',
     &       HIST,0,MESH%NPOIN,AT1,TEXTE,
     &       TEXRES,NVARRES,VARCLA,0,TROUVE,ALIRE,W,.FALSE.,MAXVAR)
C
C       READS THE MEASUREMENTS (REFERENCE FILE - UNIT NREF)
C
        ITER=NIT-LT
        IF(OUTINI) ITER=ITER+1
        CALL MESURES(ITER,AT-DT)
C
       ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
