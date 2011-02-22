C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE 2D GRADIENT OF FUNCTION F.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DFDX, DFDY, FU, FU2, IELM2, IPLAN, MASKEL, MESH2D, MSK, S, UNSV2D
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, NPOIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), VECTOR()
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
!>      <td><center> 5.7                                       </center>
!> </td><td> 25/11/97
!> </td><td> F LEPEINTRE (LNH) 30 87 78 54; J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DFDX
!></td><td><--</td><td>DF/DX
!>    </td></tr>
!>          <tr><td>DFDY
!></td><td><--</td><td>DF/DY
!>    </td></tr>
!>          <tr><td>F2
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 2D
!>    </td></tr>
!>          <tr><td>FU
!></td><td>--></td><td>FONCTION A DERIVER
!>    </td></tr>
!>          <tr><td>FU2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE DE DISCRETISATION 2D
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>IPLAN
!></td><td>--></td><td>PLAN SUR LEQUEL ON CALCULE LE GRADIENT
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>--></td><td>BLOC DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>--></td><td>INVERSE DU VOLUME DES BASES EN 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GRAD2D
     &(DFDX,DFDY,FU,NPLAN,S,UNSV2D,FU2,FU3,FU4,IELM2,MESH2D,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DFDX           |<--| DF/DX
C| DFDY           |<--| DF/DY
C| F2             |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 2D
C| FU             |-->| FONCTION A DERIVER
C| FU2            |---| 
C| FU3            |---| 
C| FU4            |---| 
C| IELM2          |-->| TYPE DE DISCRETISATION 2D
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IPLAN          |-->| PLAN SUR LEQUEL ON CALCULE LE GRADIENT
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH2D         |-->| BLOC DU MAILLAGE 2D
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| S              |---| 
C| SVIDE          |-->| STRUCTURE VIDE
C| UNSV2D         |-->| INVERSE DU VOLUME DES BASES EN 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DFDX, DFDY
      TYPE(BIEF_OBJ), INTENT(IN)    :: FU
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UNSV2D,S
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH2D
      INTEGER, INTENT(IN)           :: NPLAN, IELM2
      LOGICAL, INTENT(IN)           :: MSK
!     FU2,3,4 MUST BE 2D WORK FIELD - NO CHECKING
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FU2,FU3,FU4
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPOIN,I,IPLAN
      DOUBLE PRECISION, POINTER :: SAVEFU2(:),SAVEFU3(:),SAVEFU4(:)
C
C-----------------------------------------------------------------------
C
      NPOIN = MESH2D%NPOIN
!
      SAVEFU2=> FU2%R
      SAVEFU3=> FU3%R
      SAVEFU4=> FU4%R
!
      DO IPLAN=1,NPLAN
!
!     WORKING ON POINTERS RATHER THAN COPYING
      FU2%R=>  FU%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
      FU3%R=>DFDX%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
      FU4%R=>DFDY%R((IPLAN-1)*NPOIN+1:IPLAN*NPOIN)
!
!     CALCUL DE LA DERIVEE DF/DX
!
      CALL VECTOR(FU3,'=','GRADF          X',IELM2,1.D0,FU2,S,S,
     *            S,S,S,MESH2D,MSK,MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM (FU3, 2, MESH2D)
      CALL OS('X=XY    ' ,X=FU3,Y=UNSV2D)
!
!     CALCUL DE LA DERIVEE DF/DY
!
      CALL VECTOR(FU4,'=','GRADF          Y',IELM2,1.D0,FU2,S,S,
     *            S,S,S,MESH2D,MSK,MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM (FU4, 2, MESH2D)
      CALL OS('X=XY    ' ,X=FU4,Y=UNSV2D)
!
      ENDDO
!
      FU2%R=>SAVEFU2 
      FU3%R=>SAVEFU3
      FU4%R=>SAVEFU4
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
