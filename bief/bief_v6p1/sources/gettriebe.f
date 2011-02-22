C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!>                 PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!>  @code
!>            IF MTRI IS THIS TRIDIAGONAL PART, MAUX THE RESULT AND MDIF
!>            THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!>
!>            MAUX = TETA * MTRI
!>            MDIF CHANGED INTO (1-TETA) * MDIF
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AD, AX, IKLE, MESH, NELEM, NELMAX, NPOIN, TETA, XAUX
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
!>    </th><td> I1, I2, I3, I4, I5, I6, IAN, ICOM, IELEM, NPLAN, NPOIN2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GETTRIEBE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBPTS(), OV(), PARCOM2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GETTRI()

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
!> </td><td> 13/08/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAUX
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GETTRIEBE
     &(XAUX,AD,AX,TETA,IKLE,NPOIN,NELEM,NELMAX,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AD             |---| 
C| AX             |---| 
C| IKLE           |---| 
C| MESH           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C| NPOIN          |---| 
C| TETA           |---| 
C| XAUX           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_GETTRIEBE => GETTRIEBE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),AX(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: AD(NPOIN)
C
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,NPLAN,IAN,ICOM,NPOIN2
C
C-----------------------------------------------------------------------
C
C     CONSIDERS HERE THAT NPOIN < NELMAX TO USE XAUX AS XAUX(NPOIN,3)
C
C     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
C     XAUX(I,2) IS THE DIAGONAL
C     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
C
C-----------------------------------------------------------------------
C     INITIALISES THE DIAGONAL AND OFF-DIAGONAL TERMS
C-----------------------------------------------------------------------
C
      CALL OV('X=C     ',XAUX(1,1),AD,AD,0.D0,NPOIN)
      CALL OV('X=CY    ',XAUX(1,2),AD,AD,TETA,NPOIN)
      CALL OV('X=C     ',XAUX(1,3),AD,AD,0.D0,NPOIN)
C
      CALL OV('X=CX    ',AD,AD,AD,1.D0-TETA,NPOIN)
C
C-----------------------------------------------------------------------
C     ADDS TRIDIAGONAL TERMS
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
        XAUX(I1,3)=XAUX(I1,3)+TETA*AX(IELEM,03) ! TERM 1-4
        XAUX(I2,3)=XAUX(I2,3)+TETA*AX(IELEM,08) ! TERM 2-5
        XAUX(I3,3)=XAUX(I3,3)+TETA*AX(IELEM,12) ! TERM 3-6
        XAUX(I4,1)=XAUX(I4,1)+TETA*AX(IELEM,03) ! TERM 4-1
        XAUX(I5,1)=XAUX(I5,1)+TETA*AX(IELEM,08) ! TERM 5-2
        XAUX(I6,1)=XAUX(I6,1)+TETA*AX(IELEM,12) ! TERM 6-3
C
        AX(IELEM,03)=AX(IELEM,03)*(1.D0-TETA)
        AX(IELEM,08)=AX(IELEM,08)*(1.D0-TETA)
        AX(IELEM,12)=AX(IELEM,12)*(1.D0-TETA)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     PARALLEL MODE
C
      IF(NCSIZE.GT.1) THEN
        IAN    = 3
        ICOM   = 2
        NPOIN2 = BIEF_NBPTS(11,MESH)
        NPLAN=NPOIN/NPOIN2
        CALL PARCOM2(XAUX(1,1),XAUX(1,2),XAUX(1,3),
     &               NPOIN2,NPLAN,ICOM,IAN,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
