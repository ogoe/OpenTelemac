C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODELS THE INFLUENCE OF TURBULENCE ON THE
!>                SETTLING VELOCITY:
!>  @code
!>                         (1+A*G)
!>                  WC= WC*________
!>                         (1+B*G*G)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HN, IELM3, LISRUF, MASKEL, MESH3, MSK, NPLAN, NPOIN2, RUGOF, S, TRAV1, TRAV2, TRAV3, TURBA, TURBB, U, UETCAR, V, W, WC, WCHU0
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_WCTURB
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CALCG(), OS(), PARCOM(), VECTOR()
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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> 01/08/97
!> </td><td> C LE NORMANT 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AMESH3
!></td><td>--></td><td>DONNEES SUR LE MAILLAGE
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>IMESH3
!></td><td>--></td><td>DONNEES SUR LE MAILLAGE
!>    </td></tr>
!>          <tr><td>LISRUF
!></td><td>--></td><td>REGIME DE TURBULENCE POUR LE FOND
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>LOGIQUE POUR LE MASQUAGE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>RUGOF
!></td><td>--></td><td>COEFFICIENT DE RUGOSITE DES FONDS
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>TRAV1,2,3
!></td><td>--></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TURBA,TURBBA
!></td><td>--></td><td>COEFFICIENTS UTILISES POUR LA PRISE EN
!>                  COMPTE DE L'INFLUENCE DE LA TURBULENCE
!>    </td></tr>
!>          <tr><td>TURBB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>UETCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WC
!></td><td><--</td><td>VITESSE DE CHUTE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>WCHU0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COODONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WCTURB
     &(WC, WCHU0, U, V, W, HN, RUGOF, LISRUF, TRAV1, TRAV2, TRAV3,
     & S, MESH3, IELM3, NPOIN2, NPLAN, TURBA, TURBB, MSK, MASKEL,
     & UETCAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AMESH3         |-->| DONNEES SUR LE MAILLAGE
C| HN             |-->| HAUTEUR D'EAU
C| IELM3          |-->| TYPE DE DISCRETISATION
C| IMESH3         |-->| DONNEES SUR LE MAILLAGE
C| LISRUF         |-->| REGIME DE TURBULENCE POUR LE FOND
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH3          |---| 
C| MSK            |-->| LOGIQUE POUR LE MASQUAGE
C| NPLAN          |-->| NOMBRE DE PLANS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| RUGOF          |-->| COEFFICIENT DE RUGOSITE DES FONDS
C| S             |-->| STRUCTURE VIDE
C| TRAV1,2,3      |-->| TABLEAUX DE TRAVAIL
C| TRAV2          |---| 
C| TRAV3          |---| 
C| TURBA,TURBBA   |-->| COEFFICIENTS UTILISES POUR LA PRISE EN
C|                |   | COMPTE DE L'INFLUENCE DE LA TURBULENCE
C| TURBB          |---| 
C| U,V,W          |-->| COMPOSANTES DE LA VITESSE
C| UETCAR         |---| 
C| WC             |<--| VITESSE DE CHUTE DU SEDIMENT
C| WCHU0          |---| 
C| X,Y,Z          |-->| COODONNEES DES POINTS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_WCTURB => WCTURB
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NPOIN2, NPLAN, IELM3, LISRUF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WC
      TYPE(BIEF_OBJ), INTENT(IN)    :: U, V, W
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKEL, S
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN, RUGOF ,UETCAR
      DOUBLE PRECISION, INTENT(IN)  :: WCHU0, TURBA, TURBB
      LOGICAL, INTENT(IN) :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C
!
!----------------------------------------------------------------------
!
C     COMPUTES THE VELOCITY GRADIENTS
!
      CALL VECTOR(TRAV1,'=','GRADF          Z',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV2,'=','GRADF          Z',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3,MSK,MASKEL)
      CALL VECTOR(TRAV3,'=','MASBAS          ',IELM3,1.D0,S,
     &            S,S,S,S,S,MESH3,MSK,MASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(TRAV3, 2, MESH3)
!
C  DU/DZ IS TRAV1; DV/DZ IS TRAV2
!
      CALL OS('X=Y/Z   ',TRAV1,TRAV1,TRAV3,C,2,0.D0,1.D-10)
      CALL OS('X=Y/Z   ',TRAV2,TRAV2,TRAV3,C,2,0.D0,1.D-10)
!
      CALL OS('X=XY    ',X=TRAV1,Y=TRAV1)
      CALL OS('X=XY    ',X=TRAV2,Y=TRAV2)
!
      CALL OS('X=X+Y   ',X=TRAV1,Y=TRAV2)
      CALL OS('X=SQR(Y)',X=TRAV2,Y=TRAV1)
!
!----------------------------------------------------------------------
!
C                           AUBORF * U_B * DU/DZ
!     COMPUTES  G  =  SQRT( --------------------- ) : TRAV3
C                             NU
!
      CALL CALCG(TRAV2%R,TRAV3%R,U%R,V%R,UETCAR%R,NPOIN2,NPLAN)
!
!----------------------------------------------------------------------
!
C     COMPUTES 1 + A G  : TRAV1
!
      CALL OS('X=CY    ',X=TRAV1,Y=TRAV3,C=TURBA)
      CALL OS('X=C     ',X=TRAV2,C=1.D0)
      CALL OS('X=X+Y   ',X=TRAV1,Y=TRAV2)
!
C     COMPUTES 1 + B G G  : TRAV2
!
      CALL OS('X=XY    ',X=TRAV3,Y=TRAV3)
      CALL OS('X=X+CY  ',X=TRAV2,Y=TRAV3,C=TURBB)
!
C     COMPUTES WC (...WITH WCHU0-BASE...)
!
      CALL OS('X=Y/Z   ',TRAV1,TRAV1,TRAV2,C,2,0.D0,1.D-10)
      CALL OS('X=CY    ',X=WC,Y=TRAV1,C=WCHU0)
!
!=======================================================================
!
      RETURN
      END SUBROUTINE WCTURB
C
C#######################################################################
C