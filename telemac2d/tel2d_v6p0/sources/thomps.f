C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TREATS LIQUID BOUNDARIES USING THOMPSON METHOD
!>                BASED ON CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CF, CORIOL, COUROU, DEBLIQ, DSCE, DT, FAIR, FCOR, FINLIQ, FRTYPE, FU, FV, GRAV, H, HBOR, HBTIL, HFROT, HWIND, IELM, IFABOR, IKLE, ISCE, ITRAV2, KP1BOR, KSORT, LIHBOR, LISPFR, LITBOR, LIUBOR, LIVBOR, LT, LV, MARDAT, MAREE, MARTIM, MASKEL, MASKPT, MESH, MSK, NBOR, NELBOR, NELEM, NELMAX, NFRLIQ, NIT, NORD, NPOIN, NPTFR, NPTH, NTRAC, NULONE, NUMLIQ, NVARCL, OPTPRO, OPTSOU, PHI0, SHP, SMH, SPHERI, SURDET, T, T5, T6, T8, TBOR, TBTIL, TEMPS, U, UBOR, UBTIL, UCONV, UNA, UNSV2D, USCE, V, VARCL, VARCLA, VBOR, VBTIL, VCONV, VENT, VSCE, W1, W1R, W2R, W3R, W4R, WINDX, WINDY, X, XNEBOR, Y, YNEBOR, ZBTIL, ZF
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPSIL, HHBOR, HMIN, IFRLIQ, ITRAC, J, K, KP, N, NDEB, NFIN, NPT, TSI
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_THOMPS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CARAFR(), CLIP(), FRICTI(), GTSH11(), OS(), PLANTE(), PROSOU()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 05/09/2008
!> </td><td> E DAVID (LHF) 04 76 33 42 36
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/09/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> POINTS GROUPED REGARDLESS OF THEIR BOUNDARY NUMBER.
!> <br>      THIS IS TO HAVE AN ALGORITHM THAT WORKS ALSO IN PARALLEL
!>          (ALTHOUGH THE GROUPS WILL BE DIFFERENT).
!> <br>      CALLING GTSH11 ONCE AT THE BEGINNING AND NOT IN CARAFR
!>          (NOW GTSH11 IS INDEPENDENT OF THE VELOCITY).
!> <br>      OTHER DIFFICULTIES FORBID PARALLELISM SO FAR
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>TABLEAU DE TRAVAIL : CELERITE DES ONDES
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CORIOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COUROU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBLIQ
!></td><td>--></td><td>TABLEAU D'INDICES DE DEBUT DE FRONTIERE LIQ.
!>    </td></tr>
!>          <tr><td>DSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>FAIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FCOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FINLIQ
!></td><td>--></td><td>TABLEAU D'INDICES DE FIN DE FRONTIERE LIQUI.
!>    </td></tr>
!>          <tr><td>FRTYPE
!></td><td>--></td><td>TYPE DE FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>FU,FV
!></td><td>--></td><td>TABLEAU DE TRAVAIL : TERMES SOURCES
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td><--</td><td>HAUTEUR IMPOSEE.
!>    </td></tr>
!>          <tr><td>HBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HWIND
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT,KENTU,
!></td><td>--></td><td>CONVENTION POUR LES TYPES DE CONDITIONS AUX
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>---</td><td>KENT:VALEURS IMPOSEES (SAUF U ET V)
!>                  KENTU:U ET V IMPOSES
!>                  KSORT:VALEURS LIBRES
!>                  KINC:ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMERO DU POINT FRONTIERE SUIVANT
!>    </td></tr>
!>          <tr><td>KSORT,
!></td><td>---</td><td>LIMITES PHYSIQUES
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>LISPFR
!></td><td>--></td><td>LISTE DES POINTS FRONTIERES CONTIGUS TRAITES
!>                  ENSEMBLES PAR LES CARACTERISTIQUES
!>                  CARACTERISTIQUES
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR LE TRACEUR
!>    </td></tr>
!>          <tr><td>LIUBOR,LIVBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR U ET V
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DE L'ITERATION EN COURS
!>    </td></tr>
!>          <tr><td>LV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARDAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAREE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARTIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>--></td><td>NOMBRE DE FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NORD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE.
!>    </td></tr>
!>          <tr><td>NPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVARCL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTPRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PHI0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPHERI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TRACEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td><--</td><td>TRACEUR IMPOSE AU BORD
!>    </td></tr>
!>          <tr><td>TBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEMPS
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td><--</td><td>VITESSE U IMPOSEE.
!>    </td></tr>
!>          <tr><td>UBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>TABLEAU DE TRAVAIL : CHAMPS DE VITESSE
!>                  CONVECTEUR DES INVARIANTS DE RIEMANN
!>    </td></tr>
!>          <tr><td>UNA
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>USCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARCL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARCLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td><--</td><td>VITESSE V IMPOSEE.
!>    </td></tr>
!>          <tr><td>VBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1R
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W2R
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3R
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W4R
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WINDX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WINDY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>NORMALES EXTERIEURES AUX POINTS.
!>    </td></tr>
!>          <tr><td>ZBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE THOMPS
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,T,ZF,X,Y,NBOR,FRTYPE,UNA,C,
     & UCONV,VCONV,T6,FU,FV,LIHBOR,LIUBOR,LIVBOR,LITBOR,LISPFR,T8,W1,
     & ITRAV2,
     & W1R,W2R,W3R,W4R,HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,SURDET,IKLE,
     & CF,SMH,IFABOR,NULONE,NELEM,MESH,
     & KP1BOR,XNEBOR,YNEBOR,NPOIN,NPTFR,LT,NIT,TEMPS,DT,GRAV,
     & DEBLIQ,FINLIQ,NTRAC,NFRLIQ,KSORT,LV,MSK,MASKEL,MASKPT,
     & NELBOR,NELMAX,IELM,NORD,FAIR,WINDX,WINDY,
     & VENT,HWIND,CORIOL,FCOR,SPHERI,
     & OPTPRO,MAREE,MARDAT,MARTIM,PHI0,OPTSOU,ISCE,DSCE,USCE,VSCE,T5,
     & COUROU,NPTH,VARCL,NVARCL,VARCLA,NUMLIQ,SHP,UNSV2D,HFROT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| TABLEAU DE TRAVAIL : CELERITE DES ONDES
C| CF             |---| 
C| CORIOL         |---| 
C| COUROU         |---| 
C| DEBLIQ         |-->| TABLEAU D'INDICES DE DEBUT DE FRONTIERE LIQ.
C| DSCE           |---| 
C| DT             |-->| PAS DE TEMPS
C| FAIR           |---| 
C| FCOR           |---| 
C| FINLIQ         |-->| TABLEAU D'INDICES DE FIN DE FRONTIERE LIQUI.
C| FRTYPE         |-->| TYPE DE FRONTIERES LIQUIDES
C| FU,FV          |-->| TABLEAU DE TRAVAIL : TERMES SOURCES
C| GRAV           |-->| GRAVITE
C| H             |-->| HAUTEUR AU TEMPS N
C| HBOR           |<--| HAUTEUR IMPOSEE.
C| HBTIL          |---| 
C| HFROT          |---| 
C| HWIND          |---| 
C| IELM           |---| 
C| IFABOR         |---| 
C| IKLE           |---| 
C| ISCE           |---| 
C| ITRAV2         |---| 
C| KENT,KENTU,    |-->| CONVENTION POUR LES TYPES DE CONDITIONS AUX
C| KINC           |---| KENT:VALEURS IMPOSEES (SAUF U ET V)
C|                |   | KENTU:U ET V IMPOSES
C|                |   | KSORT:VALEURS LIBRES
C|                |   | KINC:ONDE INCIDENTE
C| KP1BOR         |-->| NUMERO DU POINT FRONTIERE SUIVANT
C| KSORT,         |---| LIMITES PHYSIQUES
C| LIHBOR         |-->| CONDITIONS AUX LIMITES SUR H
C| LISPFR         |-->| LISTE DES POINTS FRONTIERES CONTIGUS TRAITES
C|                |   | ENSEMBLES PAR LES CARACTERISTIQUES
C|                |   | CARACTERISTIQUES
C| LITBOR         |-->| CONDITIONS AUX LIMITES SUR LE TRACEUR
C| LIUBOR,LIVBOR  |-->| CONDITIONS AUX LIMITES SUR U ET V
C| LT             |-->| NUMERO DE L'ITERATION EN COURS
C| LV             |---| 
C| MARDAT         |---| 
C| MAREE          |---| 
C| MARTIM         |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKPT         |---| 
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
C| NELEM          |---| 
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
C| NIT            |---| 
C| NORD           |---| 
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
C| NPTH           |---| 
C| NTRAC          |---| 
C| NULONE         |---| 
C| NUMLIQ         |---| 
C| NVARCL         |---| 
C| OPTPRO         |---| 
C| OPTSOU         |---| 
C| PHI0           |---| 
C| SHP            |---| 
C| SMH            |---| 
C| SPHERI         |---| 
C| SURDET         |---| 
C| T             |-->| TRACEUR AU TEMPS N
C| T5             |---| 
C| T6             |---| 
C| T8             |---| 
C| TBOR           |<--| TRACEUR IMPOSE AU BORD
C| TBTIL          |---| 
C| TEMPS          |-->| TEMPS
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| U,V            |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
C| UBOR           |<--| VITESSE U IMPOSEE.
C| UBTIL          |---| 
C| UCONV,VCONV    |-->| TABLEAU DE TRAVAIL : CHAMPS DE VITESSE
C|                |   | CONVECTEUR DES INVARIANTS DE RIEMANN
C| UNA            |-->| TABLEAU DE TRAVAIL
C| UNSV2D         |---| 
C| USCE           |---| 
C| VARCL          |---| 
C| VARCLA         |---| 
C| VBOR           |<--| VITESSE V IMPOSEE.
C| VBTIL          |---| 
C| VENT           |---| 
C| VSCE           |---| 
C| W1             |---| 
C| W1R            |---| 
C| W2R            |---| 
C| W3R            |---| 
C| W4R            |---| 
C| WINDX          |---| 
C| WINDY          |---| 
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XNEBOR,YNEBOR  |-->| NORMALES EXTERIEURES AUX POINTS.
C| ZBTIL          |---| 
C| ZF             |-->| FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_THOMPS => THOMPS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,LT,NIT,NPOIN,NELEM,NELMAX,NFRLIQ,LV
      INTEGER, INTENT(IN) :: NVARCL,NPTH,KSORT,IELM,NTRAC,HFROT
      INTEGER, INTENT(IN) :: OPTPRO,MARDAT(3),MARTIM(3),OPTSOU,ISCE(*)
      INTEGER, INTENT(IN) :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR,2),NELBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(*),IFABOR(*),NULONE(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(IN) :: FRTYPE(NFRLIQ),NUMLIQ(NFRLIQ)
      INTEGER, INTENT(INOUT) :: LISPFR(NPTFR)
C     ITRAV2 : OF DIMENSION NPOIN
      INTEGER, INTENT(INOUT) :: ITRAV2(*)
      LOGICAL, INTENT(IN) :: VENT,MAREE,CORIOL,SPHERI,MSK,COUROU
      DOUBLE PRECISION, INTENT(IN) :: HWIND
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(*),DSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: USCE(*),VSCE(*)
      DOUBLE PRECISION, INTENT(IN)  :: TEMPS,GRAV,DT,FAIR,FCOR,NORD,PHI0
      DOUBLE PRECISION, INTENT(INOUT) :: W1R(NPTFR),W2R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: W3R(NPTFR),W4R(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: HBTIL(NPTFR),UBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBTIL(NPTFR),ZBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: T5(NPOIN),SHP(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: WINDX,WINDY,MASKEL,MASKPT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: W1,VARCL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FU,FV,T8,UNA,UCONV,VCONV,C,U,V
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,T,SMH,TBOR,TBTIL,T6
      TYPE(BIEF_OBJ), INTENT(IN)      :: ZF,CF,LITBOR,UNSV2D
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      CHARACTER(LEN=32), INTENT(IN)   :: VARCLA(NVARCL)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,NDEB,NFIN,IFRLIQ,NPT,KP,J,ITRAC,N
C
      DOUBLE PRECISION EPSIL,HMIN,HHBOR
C
      LOGICAL TSI
C
      DATA EPSIL /1.D-5/
      DATA TSI   /.FALSE./
      DATA HMIN  /2.D-2/
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
      IF(NCSIZE.GT.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'THOMPSON NE MARCHE PAS EN PARALLELE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'THOMPSON NOT YET IMPLEMENTED IN PARALLEL'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     ONLY IF THERE ARE LIQUID BOUNDARIES
C
      IF(NFRLIQ.NE.0) THEN
C
C
C COMPUTES THE FRICTION TERM IN UCONV
C "C,C" ADDED BY JMH ON 08/08/2000 (VERTICAL STRUCTURES)
C BUT NOT TAKEN INTO ACCOUNT HERE (LAST ARGUMENT SET TO FALSE)
C
        CALL FRICTI(FU,FV,C,C,U,V,H,CF,MESH,T8,T6,.FALSE.,UNSV2D,
     &              MSK,MASKEL,HFROT)
C
C COMPUTES DT*UCONV*U
C
        CALL OS('X=CYZ   ', UCONV , FU , U , DT )
        CALL OS('X=CYZ   ', VCONV , FV , V , DT )
C
C COMPUTES THE SOURCE TERMS IN FU
C
        CALL PROSOU(FU,FV,SMH,U,V,H,GRAV,NORD,
     &              FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     &              SPHERI,TSI,MESH%COSLAT,MESH%SINLAT,
     &              TEMPS,LT,0,0,DSCE,ISCE,UNA,MESH,MSK,MASKEL,
     &              MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &              VARCL,NVARCL,VARCLA,UNSV2D)
C
C GOES IN FU
C
        CALL OS('X=Y+CZ  ', FU , UCONV , FU , DT )
        CALL OS('X=Y+CZ  ', FV , VCONV , FV , DT )
C
C COMPUTES THE CELERITY
C
        CALL OS('X=CY    ' , C , H , H , GRAV )
        CALL CLIP(C,0.D0,.TRUE.,1.D6,.FALSE.,0)
        CALL OS('X=SQR(Y)',X=C,Y=C )
C
C CORRECTS FOR TIDAL FLATS
C
        DO 9 K=1,NPOIN
          IF(H%R(K).LT.HMIN) THEN
            FU%R(K)=0.D0
            FV%R(K)=0.D0
          ENDIF
9       CONTINUE
C
C TEMPORAL MARCHING (SPLITTING DU/DT=FU)
C
        CALL OS('X=X+Y   ',X=U,Y=FU)
        CALL OS('X=X+Y   ',X=V,Y=FV)
C
C REGROUPS THE POINTS WITH THE SAME NORMAL
C TO THE ACCURACY 'EPSIL'
C NPT : NUMBER OF SEQUENTIAL POINTS
C LISPFR : LIST OF THOSE POINTS (BOUNDARY NODE NUMBERS)
C
      NDEB=0
C
19    CONTINUE
      K=NDEB
20    CONTINUE
C
      K=K+1
      IF(K.GT.NPTFR) GO TO 1000
      IF(NUMLIQ(K).EQ.0) GO TO 20
      IF(FRTYPE(NUMLIQ(K)).EQ.2) THEN
C       FIRST THOMPSON POINT IN THE LIST
        NPT=1
        LISPFR(NPT)=K
      ELSE
        GO TO 20
      ENDIF
      NDEB = K
      KP   = K
30    CONTINUE
      KP=KP+1
      IF(KP.GT.NPTFR) GO TO 999
      IF(NUMLIQ(KP).EQ.0) GO TO 999
      IF(FRTYPE(NUMLIQ(KP)).EQ.2.AND.
     &   ABS(XNEBOR(KP)-XNEBOR(K)).LT.EPSIL.AND.
     &   ABS(YNEBOR(KP)-YNEBOR(K)).LT.EPSIL     ) THEN
        NPT=NPT+1
        LISPFR(NPT)=KP
        GO TO 30
      ENDIF
999   CONTINUE
      NDEB=LISPFR(NPT)
C
C UPDATES THE BOUNDARY VALUES IF FREE EXIT BOUNDARY
C
      DO J=1,NPT
        K=LISPFR(J)
        N=NBOR(K)
        IF(LIHBOR(K).EQ.KSORT) THEN
          HBOR(K)=H%R(N)
        ENDIF
        IF(LIUBOR(K).EQ.KSORT) THEN
          UBOR(K)=U%R(N)
        ENDIF
        IF(LIVBOR(K).EQ.KSORT) THEN
          VBOR(K)=V%R(N)
        ENDIF
      ENDDO
      IF(NTRAC.GT.0) THEN
        DO J=1,NPT
          K=LISPFR(J)
          N=NBOR(K)
          DO ITRAC=1,NTRAC
            IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KSORT) THEN
              TBOR%ADR(ITRAC)%P%R(K)=T%ADR(ITRAC)%P%R(N)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C COMPUTES THE ADVECTION FIELD U ACCORDING TO THE NORMAL DIRECTION
C AT THE BOUNDARY
C
      CALL OS('X=CY    ',UNA  ,U  ,U  ,XNEBOR(LISPFR(1)))
      CALL OS('X=X+CY  ',UNA  ,V  ,V  ,YNEBOR(LISPFR(1)))
      CALL OS('X=CY    ',UCONV,UNA,UNA,XNEBOR(LISPFR(1)))
      CALL OS('X=CY    ',VCONV,UNA,UNA,YNEBOR(LISPFR(1)))
C
C CHARACTERISTICS FOR THE GROUP OF POINTS, ADVECTION FIELD U
C
      CALL GTSH11(UCONV%R,VCONV%R,X,Y,SHP,ITRAV2,
C                      INDIC  NLOC   (NOT USED ANYMORE)
     &            IKLE,ITRAV2,ITRAV2,NPOIN,NELEM,NELMAX,1,MSK,MASKEL%R)
      CALL CARAFR
     & ( U%R,V%R,H%R,T,UCONV%R,VCONV%R,X,Y,SHP,
     &   SURDET , DT , IKLE , IFABOR , ITRAV2 ,
     &   NBOR , NELBOR , NULONE , IELM , NELEM , NELMAX ,
     &   NPOIN , 3 , NPTFR ,
     &   MSK , MASKEL%R , MASKPT%R ,  NPT , LISPFR , NTRAC ,
     &   HBTIL , UBTIL , VBTIL , TBTIL , ZBTIL , ZF%R,T5)
C
C COMPUTES THE RIEMANN INVARIANTS W1 AND W4 (SECOND DIMENSION OF TBOR)
C CARRIED BY THIS FIELD
C
      DO J=1,NPT
       K=LISPFR(J)
       IF(UNA%R(NBOR(K)).GE.0.D0) THEN
        W1R(K)=-HBTIL(K)*(XNEBOR(LISPFR(1))*(VBTIL(K)-V%R(NBOR(K)))-
     &                    YNEBOR(LISPFR(1))*(UBTIL(K)-U%R(NBOR(K))))
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            TBOR%ADR(ITRAC)%P%R(K+NPTFR)=HBTIL(K)*
     &      (TBTIL%ADR(ITRAC)%P%R(K)-T%ADR(ITRAC)%P%R(NBOR(K)))
          ENDDO
        ENDIF
       ELSE
        W1R(K)=-HBOR(K)*(XNEBOR(LISPFR(1))*(VBOR(K)-V%R(NBOR(K)))-
     &                   YNEBOR(LISPFR(1))*(UBOR(K)-U%R(NBOR(K))) )
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            TBOR%ADR(ITRAC)%P%R(K+NPTFR)=
     &      HBOR(K)*(TBOR%ADR(ITRAC)%P%R(K)-T%ADR(ITRAC)%P%R(NBOR(K)))
          ENDDO
        ENDIF
       ENDIF
      ENDDO
C
C COMPUTES THE ADVECTION FIELD U + C ACCORDING TO THE NORMAL DIRECTION
C AT THE BOUNDARY
C
      CALL OS('X=X+CY  ',UNA  , C   , C   ,             1.D0 )
      CALL OS('X=CY    ',UCONV, UNA , UNA , XNEBOR(LISPFR(1)) )
      CALL OS('X=CY    ',VCONV, UNA , UNA , YNEBOR(LISPFR(1)) )
C
C CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
C
      CALL GTSH11(UCONV%R,VCONV%R,X,Y,SHP,ITRAV2,
C                      INDIC  NLOC   (NOT USED ANYMORE)
     &            IKLE,ITRAV2,ITRAV2,NPOIN,NELEM,NELMAX,1,MSK,MASKEL%R)
      CALL CARAFR
     & ( U%R,V%R,H%R,T,UCONV%R,VCONV%R,X,Y,SHP,
     &   SURDET,DT,IKLE,IFABOR,ITRAV2,
     &   NBOR,NELBOR,NULONE,IELM,NELEM,NELMAX,
     &   NPOIN,3,NPTFR,
     &   MSK,MASKEL%R,MASKPT%R,NPT,LISPFR,NTRAC,
     &   HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,ZF%R,T5)
C
C COMPUTES THE RIEMANN INVARIANTS W2 CARRIED BY THIS ADVECTION FIELD
C
      DO 50 J=1,NPT
       K=LISPFR(J)
       IF (UNA%R(NBOR(K)).GE.0.D0) THEN
        W2R(K)=(-ZF%R(NBOR(K))+HBTIL(K)+ZBTIL(K))*C%R(NBOR(K))+
     &         HBTIL(K)*(XNEBOR(LISPFR(1))*(UBTIL(K)-U%R(NBOR(K)))+
     &                   YNEBOR(LISPFR(1))*(VBTIL(K)-V%R(NBOR(K))) )
       ELSE
        W2R(K)=HBOR(K)*(C%R(NBOR(K))+
     &                 (XNEBOR(LISPFR(1))*(UBOR(K)-U%R(NBOR(K)))+
     &                  YNEBOR(LISPFR(1))*(VBOR(K)-V%R(NBOR(K)))) )
       ENDIF
50    CONTINUE
C
C COMPUTES THE ADVECTION FIELD U-C ACCORDING TO THE NORMAL DIRECTION
C AT THE BOUNDARY
C
      CALL OS('X=X+CY  ',X=UNA, Y=C , C=-2.D0 )
      CALL OS('X=CY    ',X=UCONV,Y=UNA , C=XNEBOR(LISPFR(1)) )
      CALL OS('X=CY    ',X=VCONV,Y=UNA , C=YNEBOR(LISPFR(1)) )
C
C CHARACTERISTICS FOR THE GROUP OF POINTS, FIELD U + C
C
      CALL GTSH11(UCONV%R,VCONV%R,X,Y,SHP,ITRAV2,
C                      INDIC  NLOC   (NOT USED ANYMORE)
     &            IKLE,ITRAV2,ITRAV2,NPOIN,NELEM,NELMAX,1,MSK,MASKEL%R)
      CALL CARAFR
     & ( U%R,V%R,H%R,T,UCONV%R,VCONV%R,X,Y,SHP,
     &   SURDET,DT,IKLE,IFABOR,ITRAV2,
     &   NBOR,NELBOR,NULONE,IELM,NELEM,NELMAX,NPOIN,3,NPTFR,
     &   MSK,MASKEL%R,MASKPT%R,NPT,LISPFR,NTRAC,
     &   HBTIL,UBTIL,VBTIL,TBTIL,ZBTIL,ZF%R,T5)
C
C COMPUTES THE RIEMANN INVARIANTS W3 CARRIED BY THIS ADVECTION FIELD
C
      DO 60 J=1,NPT
       K=LISPFR(J)
       IF(UNA%R(NBOR(K)).GE.0.D0) THEN
        W3R(K)=(-ZF%R(NBOR(K))+HBTIL(K)+ZBTIL(K))*C%R(NBOR(K))-
     &         HBTIL(K)*(XNEBOR(LISPFR(1))*(UBTIL(K)-U%R(NBOR(K)))+
     &                   YNEBOR(LISPFR(1))*(VBTIL(K)-V%R(NBOR(K))) )
       ELSE
        W3R(K)=HBOR(K)*(C%R(NBOR(K))-
     &                 (XNEBOR(LISPFR(1))*(UBOR(K)-U%R(NBOR(K)))+
     &                  YNEBOR(LISPFR(1))*(VBOR(K)-V%R(NBOR(K)))) )
       ENDIF
60    CONTINUE
C
C RE-BUILDS THE TELEMAC-2D VARIABLES
C
C FOR TIDAL FLATS (H HERE
C THE PREVIOUS RELEASE HAD
C
      DO 70 J=1,NPT
C
        K=LISPFR(J)
        IF(C%R(NBOR(K))**2.GT.GRAV*HMIN) THEN
          HBOR(K)=(W2R(K)+W3R(K))/(2*C%R(NBOR(K)))
          IF(HBOR(K).GT.HMIN) THEN
C           BEWARE TIDAL FLATS, AND HIDDEN PARAMETER 0.1
            HHBOR=MAX(0.1D0,HBOR(K))
            UBOR(K)=(YNEBOR(LISPFR(1))*W1R(K)+XNEBOR(LISPFR(1))*W2R(K)-
     &HBOR(K)*C%R(NBOR(K))*XNEBOR(LISPFR(1)))/HHBOR+U%R(NBOR(K))
            VBOR(K)=(YNEBOR(LISPFR(1))*W2R(K)-XNEBOR(LISPFR(1))*W1R(K)-
     &HBOR(K)*C%R(NBOR(K))*YNEBOR(LISPFR(1)))/HHBOR+V%R(NBOR(K))
            IF(NTRAC.GT.0) THEN
              DO ITRAC=1,NTRAC
                TBOR%ADR(ITRAC)%P%R(K)=
     &          TBOR%ADR(ITRAC)%P%R(K+NPTFR)/HHBOR+
     &          T%ADR(ITRAC)%P%R(NBOR(K))
              ENDDO
            ENDIF
          ELSE
C           BECOMES DRY
            HBOR(K)=MAX(0.D0,HBOR(K))
            UBOR(K)=0.D0
            VBOR(K)=0.D0
          ENDIF
        ELSE
C         WAS DRY, H IS GIVEN BY BORD
          UBOR(K)=0.D0
          VBOR(K)=0.D0
        ENDIF
C
70    CONTINUE
C
      IF(NDEB.LE.NPTFR) GO TO 19
C
C     TEST IF(NFRLIQ.GT.0)...
      ENDIF
C
C
1000  CONTINUE
C
C
C RECOVERS OF THE EXACT VALUES FOR U AND V
C REQUIRED FOR THE SUB-ITERATIONS
C
      CALL OS('X=X-Y   ' , X=U , Y=FU )
      CALL OS('X=X-Y   ' , X=V , Y=FV )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C