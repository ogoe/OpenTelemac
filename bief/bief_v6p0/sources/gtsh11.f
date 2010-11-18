C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FIXES THE BARYCENTRIC COORDINATES OF ALL THE MESH
!>                NODES IN THE ELEMENT TOWARDS WHICH POINTS THE
!>                CHARACTERISTIC CURVE, FOR THE TELEMAC-2D P1 TRIANGLES
!>                AND BEFORE TRACING BACK IN TIME THE CHARACTERISTIC
!>                CURVES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELT, IKLE, INDIC, LV, MASKEL, MSK, NELEM, NELMAX, NLOC, NPOIN, SHP, U, V, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, N1, N2, N3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CARACT(), DERLAG(), STREAMLINE(), STREAMLINE_TOMAWAC(), THOMPS()

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
!> </td><td> 19/08/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELT
!></td><td><--</td><td>NUMEROS DES ELEMENTS CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE.
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NLOC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>SHP
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
!>                  LEURS ELEMENTS "ELT" ASSOCIES.
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GTSH11
     &(U,V,X,Y,SHP,ELT,IKLE,INDIC,NLOC,NPOIN,NELEM,NELMAX,LV,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELT            |<--| NUMEROS DES ELEMENTS CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE.
C| INDIC          |---| 
C| LV             |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NLOC           |---| 
C| NPOIN          |-->| NOMBRE DE POINTS.
C| SHP            |<--| COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
C|                |   | LEURS ELEMENTS "ELT" ASSOCIES.
C| U,V            |-->| COMPOSANTES DE LA VITESSE
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPOIN,NELEM,NELMAX,LV
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,3)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN),INDIC(NPOIN),NLOC(NPOIN)
C
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX)
C
      LOGICAL, INTENT(IN) :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,N1,N2,N3
C
C***********************************************************************
C
C     FIRST LOOP: GETS AN ELEMENT FOR ALL POINTS
C
      DO IELEM = 1,NELEM
        N1=IKLE(IELEM,1)
        ELT(N1)=IELEM
        SHP(1,N1)=1.D0
        SHP(2,N1)=0.D0
        SHP(3,N1)=0.D0
        N2=IKLE(IELEM,2)
        ELT(N2)=IELEM
        SHP(1,N2)=0.D0
        SHP(2,N2)=1.D0
        SHP(3,N2)=0.D0
        N3=IKLE(IELEM,3)
        ELT(N3)=IELEM
        SHP(1,N3)=0.D0
        SHP(2,N3)=0.D0
        SHP(3,N3)=1.D0
      ENDDO
C
C     SECOND LOOP IF MASKING: GETS AN ELEMENT WHICH IS NOT MASKED,
C                             IF THERE IS ONE
C
      IF(MSK) THEN
        DO IELEM = 1,NELEM
          IF(MASKEL(IELEM).GT.0.5D0) THEN
            N1=IKLE(IELEM,1)
            ELT(N1)=IELEM
            SHP(1,N1)=1.D0
            SHP(2,N1)=0.D0
            SHP(3,N1)=0.D0
            N2=IKLE(IELEM,2)
            ELT(N2)=IELEM
            SHP(1,N2)=0.D0
            SHP(2,N2)=1.D0
            SHP(3,N2)=0.D0
            N3=IKLE(IELEM,3)
            ELT(N3)=IELEM
            SHP(1,N3)=0.D0
            SHP(2,N3)=0.D0
            SHP(3,N3)=1.D0
          ENDIF
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C