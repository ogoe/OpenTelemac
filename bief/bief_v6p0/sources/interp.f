C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INTERPOLATES THE VALUES OF A FUNCTION AT SOME OF THE
!>                MESH NODES ACCORDING TO THE BARYCENTRIC COORDINATES
!>                OF THE POINTS AND THE VALUES AT THE NODES OF THE
!>                FUNCTION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  DOES NOT WORK IF THE PROVIDED BARYCENTRIC COORDINATES
!>            DO NOT CORRESPOND TO THE ELEMENT OF THE FUNCTION

!>  @warning  ELEMENTS OTHER THAN 11, 21 AND 41 ARE NOT IMPLEMENTED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELT, ETA, IELM, IKLE, NDP, NELMAX, NP, NPLAN, NPOIN2, SHP, SHZ, U, UTILD
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_INTERP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CARACT(), STREAMLINE(), STREAMLINE_TOMAWAC()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 28/04/93
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELT
!></td><td>--></td><td>NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE.
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT POUR LA FONCTION
!>                  U
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NP
!></td><td>--></td><td>NOMBRE DE POINTS A INTERPOLER
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS EN 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS EN 2D (MEME POUR LE 3D)
!>    </td></tr>
!>          <tr><td>SHP
!></td><td>--></td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>--></td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
!>                  DES COURBES CARACTERISTIQUES (POUR TEL3D)
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>VALEURS AUX NOEUDS.
!>    </td></tr>
!>          <tr><td>UTILD
!></td><td><--</td><td>VALEURS INTERPOLEES.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INTERP
     & ( U , UTILD , SHP , NDP , SHZ , ETA , ELT , NP , NPOIN2 , NPLAN ,
     &   IELM , IKLE , NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELT            |-->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| ETA            |---| 
C| IELM           |-->| TYPE D'ELEMENT.
C| IKLE           |-->| TABLE DE CONNECTIVITE.
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT POUR LA FONCTION
C|                |   | U
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
C| NP             |-->| NOMBRE DE POINTS A INTERPOLER
C| NPLAN          |-->| NOMBRE DE PLANS EN 3D
C| NPOIN2         |-->| NOMBRE DE POINTS EN 2D (MEME POUR LE 3D)
C| SHP            |-->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHZ            |-->| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
C|                |   | DES COURBES CARACTERISTIQUES (POUR TEL3D)
C| U             |-->| VALEURS AUX NOEUDS.
C| UTILD          |<--| VALEURS INTERPOLEES.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_INTERP => INTERP
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NP,NELMAX,NPLAN,NPOIN2,NDP,IELM
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),ELT(NP),ETA(NP)
C
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: SHP(NDP,NP),SHZ(NP)
      DOUBLE PRECISION, INTENT(INOUT) :: UTILD(NP)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IP
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.11.OR.IELM.EQ.12) THEN
C
C    P1 TRIANGLES
C    ============
C
      DO IP = 1 , NP
         UTILD(IP) = U(IKLE(ELT(IP),1),1) * SHP(1,IP)
     &             + U(IKLE(ELT(IP),2),1) * SHP(2,IP)
     &             + U(IKLE(ELT(IP),3),1) * SHP(3,IP)
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELM.EQ.13) THEN
C
C    P2 TRIANGLES
C    ============
C
      DO IP = 1 , NP
         UTILD(IP) = U(IKLE(ELT(IP),1),1) *
     &               (2.D0*SHP(1,IP)-1.D0)* SHP(1,IP)
     &             + U(IKLE(ELT(IP),2),1) *
     &               (2.D0*SHP(2,IP)-1.D0)* SHP(2,IP)
     &             + U(IKLE(ELT(IP),3),1) *
     &               (2.D0*SHP(3,IP)-1.D0)* SHP(3,IP)
     &             + U(IKLE(ELT(IP),4),1) * 4.D0 * SHP(1,IP)*SHP(2,IP)
     &             + U(IKLE(ELT(IP),5),1) * 4.D0 * SHP(2,IP)*SHP(3,IP)
     &             + U(IKLE(ELT(IP),6),1) * 4.D0 * SHP(3,IP)*SHP(1,IP)
      ENDDO
C
C------------------------------------------------------------------------
C
      ELSEIF(IELM.EQ.41) THEN
C
C    TELEMAC-3D PRISMS
C    =====================
C
      DO IP = 1 , NP
         UTILD(IP) =
     &     U(IKLE(ELT(IP),1),ETA(IP))   * SHP(1,IP) * (1.D0-SHZ(IP))
     &   + U(IKLE(ELT(IP),2),ETA(IP))   * SHP(2,IP) * (1.D0-SHZ(IP))
     &   + U(IKLE(ELT(IP),3),ETA(IP))   * SHP(3,IP) * (1.D0-SHZ(IP))
     &   + U(IKLE(ELT(IP),1),ETA(IP)+1) * SHP(1,IP) * SHZ(IP)
     &   + U(IKLE(ELT(IP),2),ETA(IP)+1) * SHP(2,IP) * SHZ(IP)
     &   + U(IKLE(ELT(IP),3),ETA(IP)+1) * SHP(3,IP) * SHZ(IP)
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'INTERP : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'INTERP : UNKNOWN TYPE OF ELEMENT : ',I6)
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