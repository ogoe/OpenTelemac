C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE ADVECTION EQUATIONS BY THE METHOD OF
!>                CHARACTERISTICS, FOR A NUMBER OF FUNCTIONS AND ON AN
!>                ENSEMBLE OF FIXED BOUNDARY POINTS: LISPFR(NPT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, ELT, H, HBTIL, IELM, IFABOR, IKLE, LISPFR, MASKEL, MASKPT, MSK, NBOR, NDP, NELBOR, NELEM, NELMAX, NPOIN, NPT, NPTFR, NTRAC, NULONE, SHP, SURDET, T, T5, TBTIL, U, UBTIL, UCONV, V, VBTIL, VCONV, X, Y, ZBTIL, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DX, DY, IFR, IPT, ITRAC, NRK, NSP, XCONV, YCONV
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHAR11(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>THOMPS()

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
!> </td><td> 05/09/2008
!> </td><td> E. DAVID (LHF) 04 76 33 42 36
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>ELT
!></td><td>---</td><td>NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HBTIL,UBTIL
!></td><td>---</td><td>..
!>                  DE H,U,V,T
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT : 11 : TRIANGLE P1
!>                  21 : QUADRANGLE P1
!>                  41 : PRISME DE TEL3D
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR
!>                  TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.
!>    </td></tr>
!>          <tr><td>LISPFR
!></td><td>--></td><td>LISTE DES POINTS FRONTIERES A TRAITER
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE.
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES POINTS
!>                  =1. : NORMAL   =0. : POINT MASQUE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT 2D.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS ADJACENTS AU BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPT
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES A TRAITER
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>NUMERO LOCAL D'UN POINT DE BORD DANS
!>                  L'ELEMENT ADJACENT DONNE PAR NELBOR.
!>    </td></tr>
!>          <tr><td>SHP
!></td><td>---</td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>1/DETERMINANT POUR LES ELEMENTS 2D.
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>VARIABLES A L'ETAPE N .
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>COMPOSANTES DES VITESSES DU CONVECTEUR.
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE .
!>    </td></tr>
!>          <tr><td>ZBTIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CARAFR
     & ( U,V,H,T,UCONV,VCONV,X , Y , SHP ,
     &   SURDET , DT , IKLE , IFABOR , ELT ,
     &   NBOR , NELBOR , NULONE , IELM , NELEM , NELMAX ,
     &   NPOIN , NDP , NPTFR ,
     &   MSK , MASKEL , MASKPT , NPT , LISPFR, NTRAC ,
     &   HBTIL , UBTIL , VBTIL , TBTIL , ZBTIL , ZF, T5  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS
C| ELT            |---| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| H             |---| 
C| HBTIL,UBTIL    |---| ..
C|                |   | DE H,U,V,T
C| IELM           |-->| TYPE D'ELEMENT : 11 : TRIANGLE P1
C|                |   | 21 : QUADRANGLE P1
C|                |   | 41 : PRISME DE TEL3D
C| IFABOR         |-->| NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR
C|                |   | TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.
C| LISPFR         |-->| LISTE DES POINTS FRONTIERES A TRAITER
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
C|                |   | =1. : NORMAL   =0. : POINT MASQUE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD.
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT 2D.
C| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AU BORD.
C| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU MAILLAGE.
C| NPT            |-->| NOMBRE DE POINTS FRONTIERES A TRAITER
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NTRAC          |---| 
C| NULONE         |-->| NUMERO LOCAL D'UN POINT DE BORD DANS
C|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR.
C| SHP            |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SURDET         |-->| 1/DETERMINANT POUR LES ELEMENTS 2D.
C| T             |---| 
C| T5             |---| 
C| TBTIL          |---| 
C| U             |-->| VARIABLES A L'ETAPE N .
C| UCONV,VCONV    |-->| COMPOSANTES DES VITESSES DU CONVECTEUR.
C| V             |---| 
C| VBTIL          |---| 
C| X,Y            |-->| COORDONNEES DU MAILLAGE .
C| ZBTIL          |---| 
C| ZF             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,NDP,NPTFR,IELM,NPT,NTRAC
      INTEGER, INTENT(IN) :: LISPFR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,NDP),IFABOR(NELMAX,*)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(INOUT)          :: ELT(NPOIN)
      LOGICAL, INTENT(IN)             :: MSK
      DOUBLE PRECISION, INTENT(INOUT) :: HBTIL(NPTFR),UBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBTIL(NPTFR),T5(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(NPOIN),VCONV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NDP,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX),MASKPT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBTIL
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NSP(1),IFR,IPT,NRK,ITRAC
C
      DOUBLE PRECISION XCONV(1),YCONV(1),DX(1),DY(1)
C
C-----------------------------------------------------------------------
C
C NUMBER OF SUB TIME STEPS OF RUNGE-KUTTA BY PROCESSED ELEMENT
C
      NRK = 3
C
      IF(IELM.EQ.11) THEN
C
C    P1 TRIANGLES
C    ============
C
C      CALLS THE SUBROUTINE UPWINDING THE CURVES OF CHARATERISTICS
C
        DO IFR=1,NPT
          IPT=NBOR(LISPFR(IFR))
          XCONV(1) = X(IPT)
          YCONV(1) = Y(IPT)
          CALL CHAR11(UCONV,VCONV,DT,NRK , X , Y , IKLE , IFABOR ,
     &                XCONV,YCONV,DX,DY , SHP(1,IPT) ,
     &                ELT(IPT) , NSP , 1 , NPOIN , NELEM , NELMAX ,
     &                SURDET , -1 ,T5)
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'CARAFR : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'CARAFR : UNKNOWN TYPE OF ELEMENT : ',I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INTERPOLATION AT THE FOOT OF THE CHARACTERISTICS
C
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        HBTIL(LISPFR(IFR)) =
     &         H(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + H(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + H(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        UBTIL(LISPFR(IFR)) =
     &         U(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + U(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + U(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        VBTIL(LISPFR(IFR)) =
     &         V(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + V(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + V(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        ZBTIL(LISPFR(IFR)) =
     &         ZF(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + ZF(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + ZF(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
              TBTIL%ADR(ITRAC)%P%R(LISPFR(IFR)) =
     &        T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &      + T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &      + T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),3)) * SHP(3,IPT)
          ENDDO
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C