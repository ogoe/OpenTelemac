C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FIXES THE BARYCENTRIC COORDINATES OF ALL THE MESH
!>                NODES IN THE ELEMENT TOWARDS WHICH POINTS THE
!>                CHARACTERISTIC CURVE, FOR THE TELEMAC-3D PRISMS AND
!>                BEFORE TRACING BACK IN TIME THE CHARACTERISTIC
!>                CURVES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELT, ETA, IKLE2, INDIC, LV, MASKEL, MSK, NELEM2, NLOC, NPLAN, NPOIN2, SHP, SHZ, U, V, WS, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, IELEM, IPLAN, IPOIN
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 21/08/2008
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
!></td><td><--</td><td>NUMEROS DES ELEMENTS CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
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
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES POINTS
!>                  =1. : NORMAL   =0. : POINT MASQUE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NLOC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>SHP
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
!>                  LEURS ELEMENTS "ELT" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>WS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GTSH41
     &(U,V,WS,X,Y,SHP,SHZ,ELT,ETA,IKLE2,INDIC,NLOC,NPOIN2,NELEM2,NPLAN,
     & LV,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELT            |<--| NUMEROS DES ELEMENTS CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |---| 
C| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE.
C| IKLE2          |---| 
C| INDIC          |---| 
C| LV             |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
C|                |   | =1. : NORMAL   =0. : POINT MASQUE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELEM2         |---| 
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NLOC           |---| 
C| NPLAN          |---| 
C| NPOIN          |-->| NOMBRE DE POINTS.
C| NPOIN2         |---| 
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| SHP            |<--| COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
C|                |   | LEURS ELEMENTS "ELT" ASSOCIES.
C| SHZ            |---| 
C| U,V            |-->| COMPOSANTES DE LA VITESSE
C| WS             |---| 
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPOIN2,NELEM2,NPLAN,LV
      INTEGER, INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN2,NPLAN),ETA(NPOIN2,NPLAN)
      INTEGER, INTENT(INOUT) :: INDIC(NPOIN2),NLOC(NPOIN2)
C
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: WS(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2),Y(NPOIN2),MASKEL(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHZ(NPOIN2,NPLAN)
C
      LOGICAL, INTENT(IN) :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLAN,IPOIN,IELEM,I1,I2,I3
C
C***********************************************************************
C
C     LOOP ON ALL POINTS
C
      DO IPLAN = 1,NPLAN
        DO IELEM=1,NELEM2
          I1=IKLE2(IELEM,1)
          I2=IKLE2(IELEM,2)
          I3=IKLE2(IELEM,3)
          ELT(I1,IPLAN)=IELEM
          ELT(I2,IPLAN)=IELEM
          ELT(I3,IPLAN)=IELEM
          SHP(1,I1,IPLAN)=1.D0
          SHP(2,I1,IPLAN)=0.D0
          SHP(3,I1,IPLAN)=0.D0
          SHP(1,I2,IPLAN)=0.D0
          SHP(2,I2,IPLAN)=1.D0
          SHP(3,I2,IPLAN)=0.D0
          SHP(1,I3,IPLAN)=0.D0
          SHP(2,I3,IPLAN)=0.D0
          SHP(3,I3,IPLAN)=1.D0
        ENDDO
      ENDDO
C
C     ON THE VERTICAL, IT IS DONE DEPENDING ON THE VERTICAL VELOCITY
C
      DO IPLAN = 1,NPLAN
        DO IPOIN=1,NPOIN2
          IF((WS(IPOIN,IPLAN).GT.0.D0.AND.IPLAN.NE.1).OR.
     &                                              IPLAN.EQ.NPLAN) THEN
            ETA(IPOIN,IPLAN) = IPLAN-1
            SHZ(IPOIN,IPLAN) = 1.D0
          ELSE
            ETA(IPOIN,IPLAN) = IPLAN
            SHZ(IPOIN,IPLAN) = 0.D0
          ENDIF
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C