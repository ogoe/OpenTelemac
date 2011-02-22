C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADDS THE DRAG FORCE OF VERTICAL STRUCTURES IN THE
!>                MOMENTUM EQUATION.
!>  @code
!>  FU IS THEN USED IN THE EQUATION AS FOLLOWS :
!>
!>  DU/DT + U GRAD(U) = - G * GRAD(FREE SURFACE) +..... + FU_IMP * U
!>
!>  AND THE TERM FU_IMP * U IS TREATED IMPLICITLY.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FUDRAG, FVDRAG
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::FU FU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IKLE IKLE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NELEM NELEM@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NELMAX NELMAX@endlink, 
!> @link DECLARATIONS_TELEMAC2D::S S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T1 T1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UN UN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VN VN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::X X@endlink, 
!> @link DECLARATIONS_TELEMAC2D::Y Y@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AIRE, CD, DIAM, I, I4, IELEM, N, NSOM, SOM, UNORM, X4, XSOM, Y4, YSOM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), CPSTVC(), INPOLY(), NBPTS(), OS(), PLANTE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FRICTI()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 01/03/1990
!> </td><td> J-M HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FU,FV
!></td><td><-></td><td>COEFFICIENTS WHERE TO ADD THE FRICTION TERM.
!>    </td></tr>
!>          <tr><td>FUDRAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FVDRAG
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DRAGFO
     &(FUDRAG,FVDRAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FU,FV          |<->| COEFFICIENTS WHERE TO ADD THE FRICTION TERM.
C| FUDRAG         |---| 
C| FVDRAG         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FUDRAG,FVDRAG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I,I4,NSOM
      DOUBLE PRECISION UNORM,AIRE,SOM,XSOM(4),YSOM(4),X4,Y4
C     DOUBLE PRECISION, PARAMETER :: CD=1.56D0,DIAM=2.D0
      DOUBLE PRECISION, PARAMETER :: CD=1.34D0,DIAM=2.D0
      INTEGER, PARAMETER :: N=1
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE MASSE INTEGRALS
C
      CALL VECTOR (T1,'=','MASBAS          ',UN%ELM,1.D0,
     &             S,S,S,S,S,S,MESH,.FALSE.,S)
C
      CALL CPSTVC(UN,FUDRAG)
      CALL CPSTVC(VN,FVDRAG)
      CALL OS('X=C     ',FUDRAG,FUDRAG,FUDRAG,0.D0)
      CALL OS('X=C     ',FVDRAG,FVDRAG,FVDRAG,0.D0)
C
C-----------------------------------------------------------------------
C
C     EXAMPLE : DRAGFORCE IS SET IN A QUADRILATERAL DEFINED BY
C               4 NODES
C     SURFACE OF 20 X 40 CENTERED ON (0,0)
C
      NSOM = 4
      XSOM(1) = -10.D0
      XSOM(2) =  10.D0
      XSOM(3) =  10.D0
      XSOM(4) = -10.D0
      YSOM(1) = -21.D0
      YSOM(2) = -21.D0
      YSOM(3) =  21.D0
      YSOM(4) =  21.D0
C
C--------------------------------------------------------------
C
C     P1 POINTS
C
      AIRE=0.D0
      DO I=1,BIEF_NBPTS(11,MESH)
C
        IF(INPOLY(X(I),Y(I),XSOM,YSOM,NSOM)) THEN
          UNORM = SQRT(UN%R(I)**2+VN%R(I)**2)
          FUDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM
          FVDRAG%R(I) =  - 0.5D0 * N * DIAM * CD * UNORM
          AIRE = AIRE + T1%R(I)
        ENDIF
C
      ENDDO
C
C     QUASI-BUBBLE POINTS
C
      IF(FU%ELM.EQ.12) THEN
C
        CALL CHGDIS(FUDRAG,11,12,MESH)
        CALL CHGDIS(FVDRAG,11,12,MESH)
C
        DO IELEM = 1 , NELEM
          I4=IKLE%I(IELEM+3*NELMAX)
          X4=(X(IKLE%I(IELEM         ))+
     &        X(IKLE%I(IELEM+  NELMAX))+
     &        X(IKLE%I(IELEM+2*NELMAX)))/3.D0
          Y4=(Y(IKLE%I(IELEM         ))+
     &        Y(IKLE%I(IELEM+  NELMAX))+
     &        Y(IKLE%I(IELEM+2*NELMAX)))/3.D0
          IF(INPOLY(X4,Y4,XSOM,YSOM,NSOM)) AIRE = AIRE + T1%R(I4)
        ENDDO
C
      ENDIF
C
      IF(AIRE.GT.1.D-6) THEN
        SOM = 1.D0 / AIRE
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'DRAGFO : AIRE DE LA ZONE NULLE'
        IF(LNG.EQ.2) WRITE(LU,*) 'DRAGFO: AREA OF ZONE EQUAL TO ZERO'
        CALL PLANTE(1)
        STOP
      ENDIF
C
      CALL OS('X=CX    ',X=FUDRAG,C=SOM)
      CALL OS('X=CX    ',X=FVDRAG,C=SOM)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
