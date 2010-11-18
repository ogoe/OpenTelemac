C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /             ->   ->
!>    VEC(I) = XMUL  /    PSI(I) *  U  . N  D(OMEGA)
!>                  /OMEGA<br>
!>    PSI(I) IS A BASE OF TYPE P1 SEGMENT
!>    ->
!>    U IS A VECTOR WITH COMPONENTS U AND V
!>    ->
!>    N IS THE OUTGOING NORMAL VECTOR TO THE ELEMENT
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, LGSEG, NBOR, NELEM, NELMAX, SU, SV, U, V, W1, W2, XMUL, XNOR, YNOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IELMU, IELMV, N1, N2, NG1, NG2, U1, U2, V1, V2, VX1, VX2, VY1, VY2, XSUR06
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC05OO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECTOS()

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
!> </td><td> 29/05/08
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>LGSEG
!></td><td>--></td><td>LONGUEUR DES SEGMENTS.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DES FONCTIONS U,V ET W
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR
!>                  INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>W1,2
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>          <tr><td>XNOR,
!></td><td>--></td><td>NORMALES AUX SEGMENTS.
!>    </td></tr>
!>          <tr><td>YNOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC05OO
     &(XMUL,SU,SV,U,V,XNOR,YNOR,LGSEG,IKLE,NBOR,NELEM,NELMAX,W1,W2 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |---| 
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| LGSEG          |-->| LONGUEUR DES SEGMENTS.
C| NBOR           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2           |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| XNOR,          |-->| NORMALES AUX SEGMENTS.
C| YNOR           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC05OO => VC05OO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
      INTEGER, INTENT(IN) :: NBOR(*)
C
      DOUBLE PRECISION, INTENT(IN)    :: XNOR(NELMAX),YNOR(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: LGSEG(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
C
C     STRUCTURES OF U, V AND REAL DATA
C
      TYPE(BIEF_OBJ)  , INTENT(IN) :: SU,SV
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N1,N2,NG1,NG2,IELEM,IELMU,IELMV
      DOUBLE PRECISION XSUR06,U1,U2,V1,V2,VX1,VY1,VX2,VY2
C
C-----------------------------------------------------------------------
C
      XSUR06 = XMUL/6.D0
C
C-----------------------------------------------------------------------
C
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C         ->
C   F AND U LINEAR FUNCTIONS ON TRIANGLES OR QUADRILATERALS
C
      IF( (IELMU.EQ.11.OR.IELMU.EQ.12.OR.IELMU.EQ.21) .AND.
     &    (IELMV.EQ.11.OR.IELMV.EQ.12.OR.IELMV.EQ.21)       ) THEN
C
      DO IELEM =1,NELEM
C
C     NUMBERING OF THE BOUNDARY NODES
C
C     GLOBAL NUMBERING
C
      NG1= NBOR(IKLE(IELEM,1))
      NG2= NBOR(IKLE(IELEM,2))
C
      U1 = U(NG1)
      U2 = U(NG2)
      V1 = V(NG1)
      V2 = V(NG2)
C
C   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
C
      VX1 = XSUR06 * ( U2 + U1 + U1 )
      VY1 = XSUR06 * ( V2 + V1 + V1 )
      VX2 = XSUR06 * ( U1 + U2 + U2 )
      VY2 = XSUR06 * ( V1 + V2 + V2 )
C
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
C
      ENDDO
C
C-----------------------------------------------------------------------
C   ->
C   U LINEAR FUNCTIONS ON SEGMENTS
C
      ELSEIF(IELMU.EQ.1.AND.IELMV.EQ.1) THEN
C
      DO IELEM =1,NELEM
C
C     NUMBERING OF THE BOUNDARY NODES
C
      N1 = IKLE(IELEM,1)
      N2 = IKLE(IELEM,2)
C
C     GLOBAL NUMBERING
C
      NG1= NBOR(N1)
      NG2= NBOR(N2)
C
      U1 = U(N1)
      U2 = U(N2)
      V1 = V(N1)
      V2 = V(N2)
C
C   DETERMINES THE BASE FUNCTIONS AT THE BOUNDARY:
C
      VX1 = XSUR06 * ( U2 + U1 + U1 )
      VY1 = XSUR06 * ( V2 + V1 + V1 )
      VX2 = XSUR06 * ( U1 + U2 + U2 )
      VY2 = XSUR06 * ( V1 + V2 + V2 )
C
      W1(IELEM) = LGSEG(IELEM) * ( VX1*XNOR(IELEM) + VY1*YNOR(IELEM) )
      W2(IELEM) = LGSEG(IELEM) * ( VX2*XNOR(IELEM) + VY2*YNOR(IELEM) )
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
        IF (LNG.EQ.1) WRITE(LU,100)
        IF (LNG.EQ.1) WRITE(LU,102) IELMU,SU%NAME
        IF (LNG.EQ.1) WRITE(LU,103) IELMV,SV%NAME
        IF (LNG.EQ.1) WRITE(LU,104)
        IF (LNG.EQ.2) WRITE(LU,110)
        IF (LNG.EQ.2) WRITE(LU,112) IELMU,SU%NAME
        IF (LNG.EQ.2) WRITE(LU,113) IELMV,SV%NAME
        IF (LNG.EQ.2) WRITE(LU,114)
100     FORMAT(1X,'VC05OO (BIEF) :')
102     FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &         1X,'NOM REEL : ',A6)
103     FORMAT(1X,'DISCRETISATION DE V : ',1I6,
     &         1X,'NOM REEL : ',A6)
104     FORMAT(1X,'CAS NON PREVU')
110     FORMAT(1X,'VC05OO (BIEF):')
112     FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &         1X,'REAL NAME: ',A6)
113     FORMAT(1X,'DISCRETIZATION OF V:',1I6,
     &         1X,'REAL NAME: ',A6)
114     FORMAT(1X,'CASE NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C