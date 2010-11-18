C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIFFUSION, ADVECTION AND SOURCE TERMS FOR A TRACER.
!>  @code
!>  THE EQUATION SOLVED IS :<br><br>
!>          N+1                                            TILD
!>         F           1                                  F   + DT*SM
!>      ---------  -  ---  DIV ( H VISC * GRAD ( F   )) = ____________
!>         DT          H                                      DT<br>
!>                                                      N+1
!>                                  + SOURCES  + SMI * F
!>                                                     ___
!>                                                     H<br>
!>     WITH :    N+1  TILD   N
!>              F   ,F     ,F  =    DIFFUSED FUNCTION
!>              VISC           =    TURBULENT VISCOSITY
!>              SM             =    SECOND MEMBER (SOURCE TERMS)
!>              TETAT          =    IMPLICITATION COEFFICIENT
!>              DT             =    TIME STEP
!>                                         N+1              N
!>              F              =    TETAT F  + (1-TETAT) * F
!>              SMI            =    IMPLICIT SOURCE TERM<br><br>
!>                    TILD       N
!>     DISTINGUISHES F     FROM F   IN CASE A FRACTIONAL STEPS METHOD<br>
!>     HAD BEEN PREVIOUSLY PERFORMED (ADVECTION FOR EXAMPLE) GIVING<br>
!>      TILD       N
!>     F     FROM F<br>
!>-----------------------------------------------------------------------<br>
!>      BOUNDARY CONDITIONS :<br>
!>      ==>   NEUMANN CONDITION<br>
!>      VISC DF/DN = AFBOR . F  +  BFBOR<br><br>
!>      ==>   DIRICHLET CONDITION<br>
!>            TREATED BY MODIFICATION OF THE EQUATIONS IN THE
!>            SUBROUTINE DIRICH
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  JMH : W IS NOT USED.

!>  @warning  MATDIF DOES NOT GIVE THE DIFFUSION MATRIX. IT MISSES THE
!>            BOUNDARY TERMS AND THERE IS A MINUS SIGN WHICH IS TAKEN
!>            INTO ACCOUNT HERE

!>  @warning  AFBOR AND BFBOR MUST BE 0 FOR THE BOUNDARY ELEMENTS
!>            WITH NO FRICTION

!>  @warning  BEWARE DISCRETISATION OF VISC

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AFBOR, AGGLOT, AM1, AM2, BFBOR, BILAN, CONV, DIFT, DM1, DT, ENTET, F, FBOR, FLBOR, FLBORTRA, FN, FSCEXP, FTILD, H, HN, HPROP, ICONVF, INFOGT, ISOUSI, KDDL, KDIR, KENT, LIMTRA, LT, MASKEL, MASKPT, MASKTR, MASSOU, MBOR, MESH, MSK, NIT, OPDTRA, OPTBAN, OPTSOU, OPTSUP, OPTVF, S, SLVTRA, SM, SMH, SMI, SOLSYS, T1, T10, T2, T3, T4, T5, T6, T7, TB, TE1, TE2, TE3, TETAH, TETAT, UCONV, UNSV2D, V2DPAR, VCONV, VISC, VISC_S, W, YASMH, YASMI, ZCONV, ZF
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_NC ADV_NSC_NC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI_NC ADV_PSI_NC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI_TF ADV_PSI_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, CFLMAX, FORMUL, FV_SCHEME, I, IELMF, IELMH, IELMS, IELMU, IOPT, MSKNEU, MSQ, N
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CVDFTR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), CPSTVC(), CVTRVF(), CVTRVF_POS(), DECVRT(), DIRICH(), DOTS(), IELBOR(), KSUPG(), LUMP(), MATRIX(), MATVEC(), OM(), OS(), OSDB(), OV_2(), PARCOM(), PLANTE(), P_DOTS(), SOLVE(), VECTOR(), VGFPSI()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION(), TELEMAC2D()

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
!> </td><td> 29/12/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/02/2009
!> </td><td> JMH
!> </td><td> CALLS CVTFVF_POS, OPTION 14
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AFBOR,BFBOR
!></td><td>--></td><td>COEFFICIENTS DES CONDITIONS DE NEUMANN
!>                  VISC*DF/DN = AFBOR*F + BFBOR
!>                  SINON DONNE PAR POINT DE BORD
!>    </td></tr>
!>          <tr><td>AGGLOT
!></td><td>--></td><td>COEFFICIENT DE MASS-LUMPING DE T.
!>    </td></tr>
!>          <tr><td>AM1
!></td><td><-></td><td>MATRIX.
!>    </td></tr>
!>          <tr><td>AM2
!></td><td><-></td><td>MATRIX.
!>    </td></tr>
!>          <tr><td>BILAN
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON DOIT FAIRE UN BILAN
!>                  DE MASSE. DANS CE CAS IL FAUT RETOURNER LA
!>                  VALEUR DE L'APPORT DES TERMES SOURCES.
!>    </td></tr>
!>          <tr><td>CONV
!></td><td>--></td><td>IF YES ADVECTION OF F
!>    </td></tr>
!>          <tr><td>DIFT
!></td><td>--></td><td>IF YES, DIFFUSION IS DONE
!>    </td></tr>
!>          <tr><td>DM1,ZCONV
!></td><td>--></td><td>SEE BELOW
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
!>                  SUR LE BILAN DE MASSE DE TRACEUR
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>F AT TIME T(N+1)
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td>--></td><td>CONDITIONS DE DIRICHLET SUR F.
!>    </td></tr>
!>          <tr><td>FLBOR
!></td><td>--></td><td>FLUXES AT BOUNDARIES
!>    </td></tr>
!>          <tr><td>FLBORTRA
!></td><td><-></td><td>TRACER FLUXES AT BOUNDARIES
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>F AT TIME T(N)
!>    </td></tr>
!>          <tr><td>FSCEXP
!></td><td>--></td><td>EXPLICIT PART OF THE SOURCE TERM
!>                  EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
!>                  WHERE THERE IS FSCE - (1-TETAT) FN
!>                  SEE DIFSOU
!>    </td></tr>
!>          <tr><td>FTILD
!></td><td>--></td><td>F AFTER ADVECTION
!>    </td></tr>
!>          <tr><td>H , HN
!></td><td>--></td><td>DEPTH AT TIME T(N+1) AND T(N)
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>--></td><td>WORK ARRAY
!>    </td></tr>
!>          <tr><td>ICONVF
!></td><td>--></td><td>OPTION FOR ADVECTION TERMS
!>                  ICONVF = 1 : CHARACTERISTICS.
!>                  ICONVF = 2 : S.U.P.G.
!>                  ICONVF = 3 : CONSERVATIVE FINITE VOLUMES
!>                  ICONVF = 4 : IDEM
!>                  ICONVF = 6 : NON CONSERVATIVE PSI SCHEME.
!>                  ICONVF = 7 : NON CONSERVATIVE N SCHEME.
!>                  ICONVF =13 : EDGE BY EDGE FORM OF 3
!>                  ICONVF =14 : IDEM
!>    </td></tr>
!>          <tr><td>INFOGT
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
!>                  SUR LE SOLVEUR.
!>    </td></tr>
!>          <tr><td>ISOUSI
!></td><td>--></td><td>NUMERO DE LA SOUS-ITERATION
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>CONVENTION POUR LES DEGRES DE LIBERTE
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES POINTS DE DIRICHLET
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>LIMTRA
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES SUR LES
!>                  POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>LT,NIT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS,NOMBRE TOTAL DE PAS.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUES PAR POINTS.
!>    </td></tr>
!>          <tr><td>MASKTR(1,1)
!></td><td>--></td><td>MASQUE VALANT 1. POUR LES SEGMENTS DIRICHLET
!>    </td></tr>
!>          <tr><td>MASKTR(1,2)
!></td><td>--></td><td>MASQUE VALANT 1. POUR LES SEGMENTS DDL
!>    </td></tr>
!>          <tr><td>MASKTR(1,3)
!></td><td>--></td><td>MASQUE VALANT 1. POUR LES SEGMENTS NEUMANN
!>                  (ET ZERO SINON)
!>    </td></tr>
!>          <tr><td>MASSOU
!></td><td>--></td><td>MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
!>                  VOIR DIFSOU
!>    </td></tr>
!>          <tr><td>MBOR
!></td><td>--></td><td>MATRICE DE BORD
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>OPDTRA
!></td><td>--></td><td>MOT-CLE : OPTION POUR LA DIFFUSION DU TRACEUR
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>--></td><td>OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
!>                  1:CLASSIQUE   2:AVEC MASQUAGE.
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>--></td><td>OPTION DE TRAITEMENT DES TERMES SOURCES.
!>                  1 : NORMAL
!>                  2 : DIRAC
!>    </td></tr>
!>          <tr><td>OPTSUP
!></td><td>---</td><td>
!>                  1 : SUPG CLASSIQUE
!>                  2 : SUPG MODIFIE
!>    </td></tr>
!>          <tr><td>OPTVF
!></td><td>--></td><td>OPTIONS POUR LES VOLUMES FINIS (VOIR CVTRVF)
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>STRUCTURE BIDON
!>    </td></tr>
!>          <tr><td>SLVTRA
!></td><td>--></td><td>SLVCFG STRUCTURE CONTAINING DATA FOR CALLING SOLVE
!>    </td></tr>
!>          <tr><td>SM
!></td><td>--></td><td>TERMES SOURCES .
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERME SOURCE DE L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>SMI
!></td><td>--></td><td>IMPLICIT SOURCE TERM
!>    </td></tr>
!>          <tr><td>SOLSYS
!></td><td>--></td><td>1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T10
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
!>          <tr><td>TB
!></td><td>--></td><td>BLOC DE TABLEAUX DE TRAVAIL (CONTIENT T1,...)
!>    </td></tr>
!>          <tr><td>TE1,TE2,TE3
!></td><td><-></td><td>TABLEAUX DE TRAVAIL SUR LES ELEMENTS
!>    </td></tr>
!>          <tr><td>TETAH
!></td><td>--></td><td>IMPLICITATION BETWEEN H AND HN
!>    </td></tr>
!>          <tr><td>TETAT
!></td><td>--></td><td>COEFFICIENT D'IMPLICITATION DE LA CONVECTION
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>ADVECTION VELOCITY FIELD
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>--></td><td>=1/V2DPAR
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>--></td><td>INTEGRAL OF TEST FUNCTIONS (ASSEMBLED IN PARALLEL)
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>--></td><td>COEFFICIENTS DE VISCOSITE SUIVANT X,Y ET Z .
!>                  SI P0 : VISCOSITE DONNEE PAR ELEMENT
!>                  SINON : VISCOSITE DONNEE PAR POINT
!>    </td></tr>
!>          <tr><td>VISC_S
!></td><td><-></td><td>WORK ARRAY FOR SAVING VISC
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAU DE TRAVAIL DE DIMENSION :
!>                  NELMAX * (NOMBRE DE POINTS DANS L'ELEMENT)
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>IF YES SMH TAKEN INTO ACCOUNT
!>    </td></tr>
!>          <tr><td>YASMI
!></td><td>--></td><td>IF YES SMI TAKEN INTO ACCOUNT
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM ELEVATION.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CVDFTR
     &(F,FTILD,FN,FSCEXP,DIFT,ICONVF,CONV,
     & H,HN,HPROP,TETAH,UCONV,VCONV,DM1,ZCONV,SOLSYS,
     & VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,AM1,AM2,
     & ZF,FBOR,AFBOR,BFBOR,LIMTRA,MASKTR,MESH,W,TB,
     & T1,T2,T3,T4,T5,T6,T7,T10,TE1,TE2,TE3,KDIR,KDDL,KENT,DT,ENTET,
     & TETAT,AGGLOT,INFOGT,BILAN,OPTSUP,
     & ISOUSI,LT,NIT,OPDTRA,OPTBAN,MSK,MASKEL,MASKPT,MBOR,
     & S,MASSOU,OPTSOU,SLVTRA,FLBOR,V2DPAR,UNSV2D,OPTVF,FLBORTRA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AFBOR,BFBOR    |-->| COEFFICIENTS DES CONDITIONS DE NEUMANN
C|                |   | VISC*DF/DN = AFBOR*F + BFBOR
C|                |   | SINON DONNE PAR POINT DE BORD
C| AGGLOT         |-->| COEFFICIENT DE MASS-LUMPING DE T.
C| AM1            |<->| MATRIX.
C| AM2            |<->| MATRIX.
C| BILAN          |-->| LOGIQUE INDIQUANT SI ON DOIT FAIRE UN BILAN
C|                |   | DE MASSE. DANS CE CAS IL FAUT RETOURNER LA
C|                |   | VALEUR DE L'APPORT DES TERMES SOURCES.
C| CONV           |-->| IF YES ADVECTION OF F
C| DIFT           |-->| IF YES, DIFFUSION IS DONE
C| DM1,ZCONV      |-->| SEE BELOW
C| DT             |-->| PAS DE TEMPS
C| ENTET          |-->| LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
C|                |   | SUR LE BILAN DE MASSE DE TRACEUR
C| F             |<--| F AT TIME T(N+1)
C| FBOR           |-->| CONDITIONS DE DIRICHLET SUR F.
C| FLBOR          |-->| FLUXES AT BOUNDARIES
C| FLBORTRA       |<->| TRACER FLUXES AT BOUNDARIES
C| FN             |-->| F AT TIME T(N)
C| FSCEXP         |-->| EXPLICIT PART OF THE SOURCE TERM
C|                |   | EQUAL TO ZERO EVERYWHERE BUT ON SOURCES
C|                |   | WHERE THERE IS FSCE - (1-TETAT) FN
C|                |   | SEE DIFSOU
C| FTILD          |-->| F AFTER ADVECTION
C| H , HN         |-->| DEPTH AT TIME T(N+1) AND T(N)
C| HPROP          |-->| WORK ARRAY
C| ICONVF         |-->| OPTION FOR ADVECTION TERMS
C|                |   | ICONVF = 1 : CHARACTERISTICS.
C|                |   | ICONVF = 2 : S.U.P.G.
C|                |   | ICONVF = 3 : CONSERVATIVE FINITE VOLUMES
C|                |   | ICONVF = 4 : IDEM
C|                |   | ICONVF = 6 : NON CONSERVATIVE PSI SCHEME.
C|                |   | ICONVF = 7 : NON CONSERVATIVE N SCHEME.
C|                |   | ICONVF =13 : EDGE BY EDGE FORM OF 3
C|                |   | ICONVF =14 : IDEM
C| INFOGT         |-->| LOGIQUE INDIQUANT SI ON IMPRIME DES INFOS
C|                |   | SUR LE SOLVEUR.
C| ISOUSI         |-->| NUMERO DE LA SOUS-ITERATION
C| KDDL           |-->| CONVENTION POUR LES DEGRES DE LIBERTE
C| KDIR           |-->| CONVENTION POUR LES POINTS DE DIRICHLET
C| KENT           |-->| 
C| LIMTRA         |-->| TYPES DE CONDITIONS AUX LIMITES SUR LES
C|                |   | POINTS DE BORD.
C| LT,NIT         |-->| NUMERO DU PAS DE TEMPS,NOMBRE TOTAL DE PAS.
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKPT         |-->| TABLEAU DE MASQUES PAR POINTS.
C| MASKTR(1,1)    |-->| MASQUE VALANT 1. POUR LES SEGMENTS DIRICHLET
C| MASKTR(1,2)    |-->| MASQUE VALANT 1. POUR LES SEGMENTS DDL
C| MASKTR(1,3)    |-->| MASQUE VALANT 1. POUR LES SEGMENTS NEUMANN
C|                |   | (ET ZERO SINON)
C| MASSOU         |-->| MASSE DE TRACEUR AJOUTEE PAR TERME SOURCE
C|                |   | VOIR DIFSOU
C| MBOR           |-->| MATRICE DE BORD
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| OPDTRA         |-->| MOT-CLE : OPTION POUR LA DIFFUSION DU TRACEUR
C| OPTBAN         |-->| OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
C|                |   | 1:CLASSIQUE   2:AVEC MASQUAGE.
C| OPTSOU         |-->| OPTION DE TRAITEMENT DES TERMES SOURCES.
C|                |   | 1 : NORMAL
C|                |   | 2 : DIRAC
C| OPTSUP         |---| 
C|                |   | 1 : SUPG CLASSIQUE
C|                |   | 2 : SUPG MODIFIE
C| OPTVF          |-->| OPTIONS POUR LES VOLUMES FINIS (VOIR CVTRVF)
C| S             |-->| STRUCTURE BIDON
C| SLVTRA         |-->| SLVCFG STRUCTURE CONTAINING DATA FOR CALLING SOLVE
C| SM             |-->| TERMES SOURCES .
C| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE
C| SMI            |-->| IMPLICIT SOURCE TERM
C| SOLSYS         |-->| 1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
C| T1             |---| 
C| T10            |---| 
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| T7             |---| 
C| TB             |-->| BLOC DE TABLEAUX DE TRAVAIL (CONTIENT T1,...)
C| TE1,TE2,TE3    |<->| TABLEAUX DE TRAVAIL SUR LES ELEMENTS
C| TETAH          |-->| IMPLICITATION BETWEEN H AND HN
C| TETAT          |-->| COEFFICIENT D'IMPLICITATION DE LA CONVECTION
C| UCONV,VCONV    |-->| ADVECTION VELOCITY FIELD
C| UNSV2D         |-->| =1/V2DPAR
C| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS (ASSEMBLED IN PARALLEL)
C| VISC           |-->| COEFFICIENTS DE VISCOSITE SUIVANT X,Y ET Z .
C|                |   | SI P0 : VISCOSITE DONNEE PAR ELEMENT
C|                |   | SINON : VISCOSITE DONNEE PAR POINT
C| VISC_S         |<->| WORK ARRAY FOR SAVING VISC
C| W             |-->| TABLEAU DE TRAVAIL DE DIMENSION :
C|                |   | NELMAX * (NOMBRE DE POINTS DANS L'ELEMENT)
C| YASMH          |-->| IF YES SMH TAKEN INTO ACCOUNT
C| YASMI          |-->| IF YES SMI TAKEN INTO ACCOUNT
C| ZF             |-->| BOTTOM ELEVATION.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CVDFTR => CVDFTR
      USE DECLARATIONS_TELEMAC, ONLY : ADV_CAR,ADV_SUP,ADV_NSC,ADV_PSI,
     &   ADV_PSI_NC,ADV_NSC_NC,ADV_LPO,ADV_NSC_TF,ADV_PSI_TF,ADV_LPO_TF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: ICONVF,ISOUSI,OPTSUP,OPDTRA,KENT
      INTEGER, INTENT(IN)           :: LT,NIT,OPTBAN,OPTSOU,KDIR,SOLSYS
      INTEGER, INTENT(IN)           :: KDDL,OPTVF
      DOUBLE PRECISION, INTENT(IN)  :: TETAT,AGGLOT,TETAH,DT
      DOUBLE PRECISION, INTENT(INOUT)  :: MASSOU
      LOGICAL, INTENT(IN)           :: INFOGT,BILAN,CONV,YASMH
      LOGICAL, INTENT(IN)           :: DIFT,MSK,ENTET,YASMI
      TYPE(SLVCFG), INTENT(INOUT)   :: SLVTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,MASKPT,H,HN,AFBOR,BFBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F,SM,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: FBOR,UCONV,VCONV,ZF
      TYPE(BIEF_OBJ), INTENT(IN)    :: FTILD,FN,SMI
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SMH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TE1,TE2,TE3,W
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T10
      TYPE(BIEF_OBJ), INTENT(IN)    :: FSCEXP,DM1,ZCONV
      TYPE(BIEF_OBJ), INTENT(IN)    :: S,LIMTRA,FLBOR,V2DPAR,UNSV2D
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VISC_S,VISC
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AM1,AM2,MBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TB
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKTR
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C,CFLMAX
C
      INTEGER IELMF,IELMH,IELMS,IELMU,MSKNEU,I,N,IOPT
C
      LOGICAL MSQ,FV_SCHEME
C
      CHARACTER*16 FORMUL
C
C-----------------------------------------------------------------------
C
      IELMF = F%ELM
      IELMH = H%ELM
      IELMS = SM%ELM
      IELMU = UCONV%ELM
C
C-----------------------------------------------------------------------
C
C     IS IT A FINITE VOLUME SCHEME FOR ADVECTION ?
C
      FV_SCHEME=.FALSE.
      IF(  ICONVF.EQ.ADV_LPO.OR.ICONVF.EQ.ADV_LPO_TF.OR.
     &     ICONVF.EQ.ADV_NSC.OR.ICONVF.EQ.ADV_NSC_TF.OR.
     &     ICONVF.EQ.ADV_PSI.OR.ICONVF.EQ.ADV_PSI_TF     ) THEN
        FV_SCHEME=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C     CASE WHERE H AND T DON'T HAVE THE SAME DISCRETISATION
C
      IF(IELMF.NE.IELMS) THEN
        CALL CHGDIS(SM ,IELMS,IELMF,MESH)
      ENDIF
      IF(IELMF.NE.IELMH.AND.YASMH) THEN
        CALL CHGDIS(SMH ,IELMH,IELMF,MESH)
      ENDIF
C
C
C-----------------------------------------------------------------------
C
C     SEMI-IMPLICITATION OF THE DEPTH
C     WITH SCHEME 5 FOR H, TETAH=0
C
      CALL OS( 'X=CY    ' , X=HPROP , Y=H     , C=      TETAH )
      CALL OS( 'X=X+CY  ' , X=HPROP , Y=HN    , C= 1.D0-TETAH )
      CALL OS( 'X=Y     ' , X=T10   , Y=HPROP )
      IF(IELMF.NE.IELMH) THEN
        CALL CHGDIS(T10,IELMH,IELMF,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
C     INITIALISES THE VARIABLES
C
C     SOLUTION INITIALISED TO F AT TIME N
      IF(ISOUSI.EQ.1) CALL OS( 'X=Y     ' , X=F , Y=FN )
C
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C  IF SUPG, BUILDS THE SEMI-IMPLICIT MATRIX + SUPG IN AM2
C
      IF(ICONVF.EQ.ADV_SUP.AND.CONV) THEN
C
C       TERM IN U.GRAD(T) CENTERED:
C
        CALL MATRIX(AM2,'M=N     ','MATVGR          ',IELMF,IELMF,
     &              1.D0,S,S,S,UCONV,VCONV,S,
     &              MESH,MSK,MASKEL)
C
C       ADDS SUPG CONTRIBUTION TO AM2
C
        IF(OPTSUP.EQ.1) THEN
C         CLASSICAL SUPG
          CALL KSUPG(TE1,TE2,1.D0,UCONV,VCONV,MESH)
          CALL MATRIX(AM2,'M=M+N   ','MASUPG          ',IELMF,IELMF,
     &                1.D0,TE1,TE2,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
C
        ELSEIF(OPTSUP.EQ.2) THEN
C         MODIFIED SUPG
          CALL MATRIX(AM2,'M=M+N   ','MAUGUG          ',IELMF,IELMF,
     &                0.5D0*DT,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
        ENDIF
C
C     NON CONSERVATIVE, IMPLICIT N-SCHEME EQUATION
      ELSEIF(ICONVF.EQ.ADV_NSC_NC) THEN
C
C       TERM IN U.GRAD(T) (IMPLICIT N-SCHEME)
C
        CALL MATRIX(AM2,'M=N     ','MATVGR         N',IELMF,IELMF,
     &              1.D0,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   COMPUTES AM1: MASS MATRIX MULTIPLIED BY 1/DT
C
      IF(DIFT.OR..NOT.FV_SCHEME.OR..NOT.CONV.OR.BILAN) THEN
C
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
        CALL OS('X=Y+Z   ',T2,ZF,HN,C)
        CALL DECVRT(TE3,T2,ZF,MESH)
C       MASS MATRIX LOCALLY LUMPED ON THE TIDAL FLATS
        FORMUL='MSLUMP          '
C       WILL USE MSQ TO MASK THE DIFFUSION
        MSQ=.TRUE.
        IF(MSK) CALL OS('X=XY    ',TE3,MASKEL,MASKEL,C)
      ELSE
C       NORMAL MASS MATRIX
        FORMUL='MATMAS          '
C       MASK FOR THE DIFFUSION = MASKEL
        IF(MSK) CALL OS('X=Y     ',TE3,MASKEL,MASKEL,C)
        MSQ=MSK
      ENDIF
      CALL MATRIX(AM1,'M=N     ',FORMUL,IELMF,IELMF,
     &            1.D0/DT,TE3,S,S,S,S,S,MESH,MSK,MASKEL)
C
C   POSSIBLE MASS-LUMPING
C
      IF(AGGLOT.GT.0.001D0) THEN
        CALL LUMP(T1,AM1,MESH,AGGLOT)
        CALL OM( 'M=CN    ' , AM1 , AM1 , S  , 1.D0-AGGLOT , MESH )
        CALL OM( 'M=M+D   ' , AM1 , AM1 , T1 , C           , MESH )
      ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(BILAN) THEN
C
        CALL MATVEC( 'X=AY    ',T2,AM1,SM,C,MESH)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T2,2,MESH)
          MASSOU = MASSOU + P_DOTS(T2,T10,MESH)
        ELSE
          MASSOU = MASSOU + DOTS(T2,T10)
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   COMPUTES THE SECOND MEMBERS
C
C     COMPUTES DT * SM IN T2
C
C     CVTRVF AND CVTRVF_POS WILL TREAT SM IN A DIFFERENT WAY
      IF(.NOT.FV_SCHEME) THEN
        CALL OS( 'X=CY    ' , X=T2 , Y=SM , C=DT )
      ENDIF
C
C=======================================================================
C TREATS THE VARIOUS TYPES OF ADVECTION:
C-----------------------------------------------------------------------
C
      IF(ICONVF.EQ.ADV_CAR.OR..NOT.CONV) THEN
C
        CALL OS( 'X=X+Y   ' , X=T2 , Y=FTILD )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
C
C-----------------------------------------------------------------------
C
      ELSEIF(ICONVF.EQ.ADV_SUP.AND.CONV) THEN
C
C       AM1 MADE NONSYMMETRICAL IF IT WAS NOT ALREADY
C
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
C
C       SUPG CONTRIBUTION TO THE MASS MATRIX
C
        IF(OPTSUP.EQ.1) THEN
C         CLASSICAL SUPG
C         TE1 AND TE2 ALREADY COMPUTED
          CALL MATRIX(AM1,'M=M+TN    ','MATVGR          ',IELMF,IELMF,
     &                1.D0/DT,S,S,S,TE1,TE2,S,MESH,MSK,MASKEL)
C
        ELSEIF(OPTSUP.EQ.2) THEN
C         MODIFIED SUPG
          CALL MATRIX(AM1,'M=M+TN    ','MATVGR          ',IELMF,IELMF,
     &                0.5D0,S,S,S,UCONV,VCONV,S,MESH,MSK,MASKEL)
        ENDIF
C
C       END OF THE SUPG CONTRIBUTION TO THE MASS MATRIX
C
        CALL OS( 'X=X+Y   ' , T2 , FN , FN , C )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
C
C EXPLICIT ADVECTION TERM:
C
        CALL MATVEC( 'X=X+CAY ',SM,AM2,FN,TETAT-1.D0,MESH)
C
C ADDS THE IMPLICIT ADVECTION PART IN AM2 TO AM1
C
        CALL OM( 'M=M+CN  ' , AM1,AM2 , S , TETAT , MESH )
C
C-----------------------------------------------------------------------
C
      ELSEIF(ICONVF.EQ.ADV_NSC_NC.AND.CONV) THEN
C
C       AM1 MADE NONSYMMETRICAL IF IT WAS NOT ALREADY
C
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
C
        CALL OS( 'X=X+Y   ' , T2 , FN , FN , C )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
C
C EXPLICIT ADVECTION TERM:
C
        CALL MATVEC( 'X=X+CAY ',SM,AM2,FN,TETAT-1.D0,MESH)
C
C ADDS THE IMPLICIT ADVECTION PART IN AM2 TO AM1
C
        CALL OM( 'M=M+CN  ' , AM1,AM2 , S , TETAT , MESH )
C
C-----------------------------------------------------------------------
C
      ELSEIF(ICONVF.EQ.ADV_PSI_NC.AND.CONV) THEN
C
C PSI SCHEME
C
C       TRADITIONAL AM1 * FN TERM
C
        CALL OS( 'X=X+Y   ' , T2 , FN , FN , C )
        CALL MATVEC( 'X=AY    ',SM,AM1,T2,C,MESH)
C
C       EXPLICIT ADVECTION TERM (PSI SCHEME)
C
        CALL VGFPSI(T5,IELMF,UCONV,VCONV,FN,DT,-1.D0,CFLMAX,
     &              T6,T7,MESH,MSK,MASKEL)
        CALL OS( 'X=X+Y   ' , SM , T5 , T5 , C )
C
C-----------------------------------------------------------------------
C
      ELSEIF( (ICONVF.EQ.ADV_LPO.OR.
     &         ICONVF.EQ.ADV_NSC.OR.
     &         ICONVF.EQ.ADV_PSI    ).AND.CONV ) THEN
C
C CONSERVATIVE EQUATION, DISTRIBUTIVE SCHEMES (LEO POSTMA, N AND PSI)
C                        LEO POSTMA AND N-SCHEME ARE THE SAME IN 2D
C
C       TO BE REMOVED WHEN ALL CALLS TO CVDFTR ARE CHECKED
C       OPTVF SHOULD BE 0 (VELOCITY FIELD OBEYS THE CONTINUITY EQUATION)
C       OR 10 (VELOCITY FIELD DOES NOT OBEY THE CONTINUITY EQUATION)
        IOPT=10*(OPTVF/10)
C       OPTION TO DISTRIBUTE THE FLUXES (HERE 2 OR 3)
        IF(ICONVF.EQ.ADV_LPO) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_NSC) IOPT=IOPT+2
        IF(ICONVF.EQ.ADV_PSI) IOPT=IOPT+3
C
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL CVTRVF(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,
     &              TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &              TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &              TB%ADR(19)%P,TB%ADR(20)%P,TB%ADR(21)%P,
     &              TB%ADR(22)%P,
     &              AGGLOT,TE1,DT,ENTET,BILAN,
     &              OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,
C                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT)
C       IF EXITS AT THIS POINT, THE DIRICHLET ARE NOT DONE, ALSO WORKS
C       CAN THEN CHECK THE MASS CONSERVATION EXACTLY
        IF(.NOT.DIFT) RETURN
        CALL MATVEC( 'X=AY    ',SM,AM1,F,C,MESH)
C
C-----------------------------------------------------------------------
C
      ELSEIF( (ICONVF.EQ.ADV_LPO_TF.OR.
     &         ICONVF.EQ.ADV_NSC_TF.OR.
     &         ICONVF.EQ.ADV_PSI_TF    ).AND.CONV ) THEN
C
C EDGE-BASED VERSIONS, FOR TIDAL FLATS
C CONSERVATIVE EQUATION, DISTRIBUTIVE SCHEMES (LEO POSTMA, N AND PSI)
C                        LEO POSTMA AND N-SCHEME ARE THE SAME IN 2D
C
C       OPTION TO DISTRIBUTE THE FLUXES (HERE 2 OR 3)
        IF(ICONVF.EQ.ADV_LPO_TF) IOPT=2
        IF(ICONVF.EQ.ADV_NSC_TF) IOPT=2
        IF(ICONVF.EQ.ADV_PSI_TF) IOPT=3
        IF(TB%N.LT.22) THEN
          WRITE(LU,*) 'SIZE OF TB TOO SMALL IN CVDFTR'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL CVTRVF_POS(F,FN,FSCEXP,DIFT,CONV,H,HN,HPROP,UCONV,VCONV,
     &              DM1,ZCONV,SOLSYS,VISC,VISC_S,SM,SMH,YASMH,SMI,YASMI,
     &              FBOR,MASKTR,MESH,
     &              TB%ADR(13)%P,TB%ADR(14)%P,TB%ADR(15)%P,
     &              TB%ADR(16)%P,TB%ADR(17)%P,TB%ADR(18)%P,
     &              TB%ADR(19)%P,TB%ADR(20)%P,TB%ADR(21)%P,
     &              TB%ADR(22)%P,
     &              AGGLOT,TE1,DT,ENTET,BILAN,
     &              OPDTRA,MSK,MASKEL,S,MASSOU,OPTSOU,
C                                                       YAFLBOR
     &              LIMTRA%I,KDIR,KDDL,MESH%NPTFR,FLBOR,.TRUE.,
     &              V2DPAR,UNSV2D,IOPT,FLBORTRA,MASKPT,
     &            MESH%GLOSEG%I(                 1:  MESH%GLOSEG%DIM1),
     &            MESH%GLOSEG%I(MESH%GLOSEG%DIM1+1:2*MESH%GLOSEG%DIM1),
     &            MESH%NBOR%I)
C       IF EXITS AT THIS POINT, THE DIRICHLET ARE NOT DONE, ALSO WORKS
C       CAN THEN CHECK THE MASS CONSERVATION EXACTLY
        IF(.NOT.DIFT) RETURN
        CALL MATVEC( 'X=AY    ',SM,AM1,F,C,MESH)
      ELSE
C
C-----------------------------------------------------------------------
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CVDFTR : OPTION DE CONVECTION INCONNUE : ',ICONVF
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CVDFTR: UNKNOWN ADVECTION OPTION : ',ICONVF
        ENDIF
        CALL PLANTE(1)
        STOP
C
C-----------------------------------------------------------------------
C
      ENDIF
C
C END OF TREATMENT OF THE VARIOUS TYPES OF ADVECTION:
C=======================================================================
C
C                   COMPUTES THE MATRICES
C
C-----------------------------------------------------------------------
C   COMPUTES AM2 : - DIFFUSION MATRIX, AND BOUNDARY TERMS
C
      IF(DIFT) THEN
C
        IF(OPDTRA.EQ.2) THEN
C             SAVES THE DIFFUSION
              CALL OS('X=Y     ',VISC_S,VISC,VISC,C)
C             MULTIPLIES THE DIFFUSION BY HPROP
           CALL OV_2('X=XY    ',VISC%R,1,T10%R,1,T10%R,1,C,
     &                          VISC%MAXDIM1,VISC%DIM1)
           IF(VISC%DIM2.EQ.3) THEN
           CALL OV_2('X=XY    ',VISC%R,2,T10%R,1,T10%R,1,C,
     &                          VISC%MAXDIM1,VISC%DIM1)
           CALL OV_2('X=XY    ',VISC%R,3,T10%R,1,T10%R,1,C,
     &                          VISC%MAXDIM1,VISC%DIM1)
           ENDIF
        ENDIF
C
C       COMPUTES THE DIFFUSION MATRIX (OPTION WITH MONOTONICITY)
C
        CALL MATRIX(AM2,'M=N     ','MATDIF       MON',IELMF,IELMF,
     &              1.D0,S,S,S,VISC,S,S,MESH,MSQ,TE3)
C
        IF(OPDTRA.EQ.2) THEN
C         MULTIPLIES THE MATRIX BY 1/HPROP
          CALL OS( 'X=1/Y   ',T4,T10,T10,C,
     &             IOPT=2,INFINI=0.D0,ZERO=1.D-2)
          CALL OM( 'M=X(M)  ' , AM2 , AM2 , S  , C , MESH )
          CALL OM( 'M=DM    ' , AM2 , AM2 , T4 , C , MESH )
C         RETURNS THE DIFFUSION
          CALL OS('X=Y     ',VISC,VISC_S,VISC_S,C)
        ENDIF
C
C   TAKES THE BOUNDARY TERMS INTO ACCOUNT IN THE DIFFUSION MATRIX
C
        MSKNEU=3
        CALL MATRIX(MBOR,'M=N     ','FMATMA          ',
     &              IELBOR(IELMF,1),IELBOR(IELMF,1),
     &              -1.D0,AFBOR,S,S,S,S,S,
     &              MESH,.TRUE.,MASKTR%ADR(MSKNEU)%P)
        CALL OM( 'M=M+N   ' , AM2 , MBOR , S , C , MESH )
C
C       EXPLICIT DIFFUSION TERM
C
        CALL MATVEC( 'X=AY    ',T1,AM2,FN,C,MESH)
        CALL OS( 'X=X+CY  ' , SM , T1 , T1 , TETAT-1.D0 )
C
C       IMPLICIT DIFFUSION TERM ( AM1 + TETAT * AM2 )
C
        IF(AM1%TYPEXT.NE.'Q'.AND.AM2%TYPEXT.EQ.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
        CALL OM( 'M=M+CN  ' , AM1,AM2 , S , TETAT , MESH )
C
C       BOUNDARY STRESS TERMS
C
        CALL VECTOR(T2,'=','MASVEC          ',IELBOR(IELMF,1),
     &              1.D0,BFBOR,S,S,S,S,S,MESH,
     &              .TRUE.,MASKTR%ADR(MSKNEU)%P)
        CALL OSDB( 'X=X+Y   ' , SM , T2 , T2 , C , MESH )
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   TAKES INTO ACCOUNT THE IMPLICIT TERM RESULTING FROM THE POINT SOURCES:
C
      IF(YASMH.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
C
        IF(OPTSOU.EQ.1) THEN
C         JMH MODIFICATION 23/09/98
          CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &                1.D0,SMH,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS( 'X=Y/Z   ' ,T1,T2,T10,C,
     &              IOPT=2,INFINI=0.D0,ZERO=1.D-3)
C         IMPLICIT PART OF THE POINT SOURCE TERM
C         - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
          CALL OS( 'X=CX    ' , T1 , T1 , T1 , TETAT )
          CALL OM( 'M=M+D   ' , AM1 , AM1 , T1 , TETAT , MESH )
C         PREPARES THE EXPLICIT PART
          CALL OS( 'X=YZ    ' , T1 , SMH , FSCEXP , C )
          CALL VECTOR(T2,'=','MASVEC          ',IELMF,
     &                1.D0,T1,S,S,S,S,S,MESH,MSK,MASKEL)
          CALL OS( 'X=Y/Z   ' ,T1,T2,T10,C,
     &             IOPT=2,INFINI=0.D0,ZERO=1.D-3)
          CALL OS( 'X=X+Y   ' , SM , T1 , T1 , C )
        ELSEIF(OPTSOU.EQ.2) THEN
          CALL OS( 'X=Y/Z   ' ,T1,SMH,T10,C,
     &              IOPT=2,INFINI=0.D0,ZERO=1.D-3)
C         EXPLICIT PART OF THE POINT SOURCE TERM
C         1/HPROP (FSCE-(1-TETAT)FN) SMH
          CALL OS( 'X=X+YZ  ' , SM , T1 , FSCEXP , C )
C         IMPLICIT PART OF THE POINT SOURCE TERM
C         - TETAT T 1/HPROP SUM ( SCE PSI D(OMEGA)
          CALL OS( 'X=CX    ' , T1 , T1 , T1 , TETAT )
          CALL OM( 'M=M+D   ' , AM1 , AM1 , T1 , TETAT , MESH )
        ENDIF
C
      ENDIF
C
C   IMPLICIT TERM IF THERE IS ONE :
C
C   THE TREATMENT BELOW ENSURES THAT IF THE EXPLICIT SOURCE TERM
C   IS IN THE FORM  K*FN/H AND SMI EQUALS -K THEN THE TWO TERMS
C   WILL BE BALANCED (CASE OF EROSION AND DEPOSITION)
C
C                  FV_SCHEME : IMPLICIT SOURCE TERM HAS BEEN TREATED
C                  AND WILL NOT BE DONE TWICE
      IF(YASMI.AND..NOT.(FV_SCHEME.AND.CONV)) THEN
        CALL MATRIX(AM2,'M=N     ','MATMAS          ',IELMF,IELMF,
     &              -1.D0,S,S,S,S,S,S,MESH,MSK,MASKEL)
C       POSSIBLE MASS-LUMPING
        IF(AGGLOT.GT.0.001D0) THEN
          CALL LUMP(T1,AM2,MESH,AGGLOT)
          CALL OM( 'M=CN    ' , AM2 , AM2 , S  , 1.D0-AGGLOT , MESH )
          CALL OM( 'M=M+D   ' , AM2 , AM2 , T1 , C           , MESH )
        ENDIF
C       COMPUTES SMI/H (DOES NOT CHECK IF H SIZE IS SUFFICIENT!!)
        IF(OPTBAN.GT.0) THEN
C         DIVIDES BY H WITH HARD-CODED CLIPPING AT 0.01
          CALL CPSTVC(SMI,T4)
          DO I=1,SMI%DIM1
            IF(T10%R(I).LT.1.D-2) THEN
              T4%R(I)=0.D0
            ELSE
              T4%R(I)=SMI%R(I)/H%R(I)
            ENDIF
          ENDDO
        ELSE
C         DIVIDES WITHOUT CHECKING
          CALL OS( 'X=Y/Z   ',X=T4,Y=SMI,Z=H)
        ENDIF
        CALL OM( 'M=X(M)  ' , AM2 , AM2 , S  , C , MESH )
        CALL OM( 'M=MD    ' , AM2 , AM2 , T4 , C , MESH )
C       ADDS TO MATRIX AM1
        IF(AM1%TYPEXT.NE.'Q') THEN
          CALL OM( 'M=X(M)  ' , AM1 , AM1 , S , C , MESH )
        ENDIF
        CALL OM( 'M=M+N   ' , AM1 , AM2 , S , C , MESH )
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(ICONVF.EQ.ADV_CAR.AND..NOT.DIFT) THEN
        CALL OS( 'X=Y     ' , F , FTILD , FTILD , C )
      ENDIF
      IF(ICONVF.EQ.ADV_PSI_NC.AND.CONV) THEN
        CALL LUMP(T1,AM1,MESH,1.D0)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T1,2,MESH)
          CALL OS( 'X=Y     ' , T2 , SM , SM , C )
          CALL PARCOM(T2,2,MESH)
          CALL OS( 'X=Y/Z   ' , F , T2 , T1 , C ,2,0.D0,1.D-6)
        ELSE
          CALL OS( 'X=Y/Z   ' , F , SM , T1 , C ,2,0.D0,1.D-6)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C   BOUNDARY CONDITIONS (POINTS OF THE TYPE DIRICHLET)
C
      CALL DIRICH(F, AM1, SM,FBOR,LIMTRA%I,TB,MESH,KDIR,MSK,MASKPT)
C
C-----------------------------------------------------------------------
C
C   SOLVES THE LINEAR SYSTEM:
C
      CALL SOLVE(F,AM1,SM,TB,SLVTRA,INFOGT,MESH,AM2)
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE TRACER FLUX AT THE BOUNDARY
C
      IF(.NOT.FV_SCHEME) THEN
        DO I=1,MESH%NPTFR
          N=MESH%NBOR%I(I)
          FLBORTRA%R(I)=FLBOR%R(I)*(TETAT*F%R(N)+(1.D0-TETAT)*FN%R(N))
        ENDDO
C     ELSE
C       FLBORTRA ALREADY COMPUTED
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(IELMF.NE.IELMS) CALL CHGDIS(SM  ,IELMF,IELMS,MESH)
      IF(IELMF.NE.IELMH.AND.YASMH) CALL CHGDIS(SMH ,IELMF,IELMH,MESH)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C