C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, H, HMIN, IELM3, IFABOR3D, ITRA01, LT, MASKBR, MASKEL, MASKPT, MESH2D, NELBO3, NELEM2, NELMA2, NETAGE, NPLAN, NPOIN2, NPTFR, X, Y, ZF, ZFE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IADR, IELEM2, IETAGE, IPLAN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MASKBD(), MASKOB(), MASKTO(), OV(), OVBD()
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 21/10/08
!> </td><td> JMH (LNHE) 01 30 87 80 18; J-M JANIN (LNH) 30 87 72 84; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>BANDEC
!></td><td>--></td><td>SI OUI TRAITEMENT DES BANCS DECOUVRANTS
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>--></td><td>HAUTEUR D'EAU MINIMALE ACCEPTABLE
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>CORRESPONDANCE FACE DE BORD - ELEMENT 2D
!>    </td></tr>
!>          <tr><td>IFABOR3D
!></td><td><--</td><td>TABLEAU DES ELEMENTS ADJACENTS AUX FACES(3D)
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>CORRESPONDANCE LOCALE - GLOGALE EN 2D
!>    </td></tr>
!>          <tr><td>ITRA01
!></td><td>--></td><td>TABLEAUX DE TRAVAIL D'ENTIERS
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>MASKBR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td><--</td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>MASQUAGE DES POINTS
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELBO3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS EN 2D
!>    </td></tr>
!>          <tr><td>NELMA2
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS EN 2D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NPLAN - 1
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD 2D
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!>                  NUMEROTATION LOCALE 3D
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTES DU FOND
!>    </td></tr>
!>          <tr><td>ZFE
!></td><td>--></td><td>COTE DU FOND PAR ELEMENT.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MASK3D
     &(IFABOR3D,MASKEL,MASKPT,MASKBR,
     & X,Y,ZF,ZFE,H,HMIN,AT,LT,ITRA01,NELBO3,
     & NELMA2,NELEM2,NPOIN2,NPTFR,NPLAN,NETAGE,IELM3,MESH2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DU PAS DE TEMPS
C| BANDEC         |-->| SI OUI TRAITEMENT DES BANCS DECOUVRANTS
C| H             |-->| HAUTEUR D'EAU
C| HMIN           |-->| HAUTEUR D'EAU MINIMALE ACCEPTABLE
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IFABOR         |-->| CORRESPONDANCE FACE DE BORD - ELEMENT 2D
C| IFABOR3D       |<--| TABLEAU DES ELEMENTS ADJACENTS AUX FACES(3D)
C| IKLE2          |-->| CORRESPONDANCE LOCALE - GLOGALE EN 2D
C| ITRA01         |-->| TABLEAUX DE TRAVAIL D'ENTIERS
C| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| MASKBR         |---| 
C| MASKEL         |<--| MASQUAGE DES ELEMENTS
C| MASKPT         |-->| MASQUAGE DES POINTS
C| MESH2D         |---| 
C| NELBO3         |---| 
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
C| NELMA2         |-->| NOMBRE MAXIMAL D'ELEMENTS EN 2D
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
C| NULONE         |-->| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C| XNEBOR,YNEBOR  |-->| COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
C| ZF             |-->| COTES DU FOND
C| ZFE            |-->| COTE DU FOND PAR ELEMENT.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM2, NPOIN2, NETAGE, NPLAN
      INTEGER, INTENT(IN)             :: NELMA2, NPTFR
      INTEGER, INTENT(INOUT)          :: IFABOR3D(NELEM2,5,NETAGE)
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM2,NETAGE)
      DOUBLE PRECISION, INTENT(INOUT) :: MASKBR(NPTFR,NETAGE)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2), Y(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2), H(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZFE(NELEM2)
      INTEGER, INTENT(IN)             :: NELBO3(NPTFR,NETAGE)
      INTEGER, INTENT(INOUT)          :: ITRA01(NELEM2)
      INTEGER, INTENT(IN)             :: LT, IELM3
      DOUBLE PRECISION, INTENT(IN)    :: HMIN, AT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: MASKPT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM2,IETAGE,IADR,IPLAN
!
!***********************************************************************
!
C INITIALISES MASKEL IF MASK!
C  MASKS TIDAL FLATS (MASKBD)
C  MASKS A USER-DEFINED AREA (MASKOB)
C  FILLS IN MASKPT (MASKTO)
!=======================================================================
!
      CALL OV('X=C     ',MASKEL,MASKEL,MASKEL,1.D0,NELEM2)
!
      CALL MASKBD(MASKEL,ZFE,ZF,H,HMIN,MESH2D%IKLE%I,MESH2D%IFABOR%I,
     &            ITRA01,NELEM2,NPOIN2)
!
      CALL MASKOB(MASKEL,X,Y,MESH2D%IKLE%I,NELEM2,NELMA2,NPOIN2,AT,LT)
!
      CALL MASKTO(MASKEL,MASKPT,IFABOR3D,MESH2D%IKLE%I,
     &            MESH2D%IFABOR%I,MESH2D%ELTSEG%I,MESH2D%NSEG,
     &            NELEM2,NPOIN2,IELM3,MESH2D)
!
!=======================================================================
C     COPIES MASKEL ON HIGHER LEVELS
!=======================================================================
!
      IF(NETAGE.GE.2) THEN
        DO IETAGE = 2,NETAGE
          DO IELEM2 = 1,NELEM2
            MASKEL(IELEM2,IETAGE) = MASKEL(IELEM2,1)
          ENDDO
        ENDDO
      ENDIF
!
!=======================================================================
C     COPIES MASKPT ON HIGHER LEVELS
!=======================================================================
!
      DO IPLAN=2,NPLAN
        IADR=(IPLAN-1)*NPOIN2
        CALL OV('X=Y     ',MASKPT%R(IADR+1:IADR+NPOIN2),
     &                     MASKPT%R,MASKPT%R,0.D0,NPOIN2)
      ENDDO
!
!=======================================================================
C     INITIALISES MASKBR
!=======================================================================
!
      CALL OV ( 'X=C     ',MASKBR,MASKBR,MASKBR,1.D0,NPTFR*NETAGE)
!
C     AND SETS IT WITH THE NEIGHBOURING MASKEL
!
      CALL OVBD ('X=Y     ',MASKBR,MASKEL,MASKEL,
     &           0.D0,NELBO3,NPTFR*NETAGE)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C