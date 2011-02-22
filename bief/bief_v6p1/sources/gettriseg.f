C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GETS THE TRIDIAGONAL PART OF A DIFFUSION MATRIX ON
!>                PRISMS AND REMOVES IT FROM THE INITIAL MATRIX.
!>  @code
!>           IF MTRI IS THIS TRIDIAGONAL PART, MAUX THE RESULT AND MDIF
!>           THE DIFFUSION MATRIX, THIS SUBROUTINE DOES:
!>
!>           MAUX = TETA * MTRI
!>           MDIF CHANGED INTO (1-TETA) * MDIF
!>
!>           SEGMENT STORAGE FOR MDIFF HERE !!!!!!!!!!!!!!!!!!!!!!!!
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
!>    </th><td> AD, AX, MESH, NPLAN, NPOIN, NPOIN2, NSEG2D, NSEG3D, TETA, XAUX
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
!>    </th><td> I2, I3, IAN, ICOM, IPLAN, NSEGH, NSEGV, SEGDOWN, SEGUP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GETTRISEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PARCOM2()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 11/08/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> CROSSED AND VERTICAL SEGMENTS SWAPPED (SEE STOSEG41)
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
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX PAR SEGMENTS
!>                  (ICI DIMENSION 1 CAR SYMETRIQUE)
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
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG3D
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
                        SUBROUTINE GETTRISEG
     &(XAUX,AD,AX,TETA,NPOIN,MESH,NSEG3D,NSEG2D,NPLAN,NPOIN2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AD             |---| 
C| AX             |-->| TERMES EXTRA-DIAGONAUX PAR SEGMENTS
C|                |   | (ICI DIMENSION 1 CAR SYMETRIQUE)
C| MESH           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C| NPLAN          |---| 
C| NPOIN          |---| 
C| NPOIN2         |---| 
C| NSEG2D         |---| 
C| NSEG3D         |---| 
C| TETA           |---| 
C| XAUX           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_GETTRISEG => GETTRISEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NSEG3D,NSEG2D,NPLAN,NPOIN2
C
      DOUBLE PRECISION, INTENT(IN)    :: TETA
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),AX(NSEG3D)
      DOUBLE PRECISION, INTENT(INOUT) :: AD(NPOIN)
C
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I2,I3,IPLAN,IAN,ICOM,SEGUP,SEGDOWN,NSEGH,NSEGV
C
C-----------------------------------------------------------------------
C
      NSEGH=NSEG2D*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
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
C     INITIALISES THE DIAGONAL TERMS
C-----------------------------------------------------------------------
C
      CALL OV('X=CY    ',XAUX(1,2),AD,AD,TETA,NPOIN)
      CALL OV('X=CX    ',AD,AD,AD,1.D0-TETA,NPOIN)
C
C-----------------------------------------------------------------------
C     TRIDIAGONAL TERMS
C-----------------------------------------------------------------------
C
C     PLANE ON THE BOTTOM
C
      DO I2=1,NPOIN2
        SEGUP=NSEGH+I2
        XAUX(I2,1)=0.D0
        XAUX(I2,3)=TETA*AX(SEGUP)
      ENDDO
C
C     PLANE AT THE FREE SURFACE
C
      DO I2=1,NPOIN2
        I3=I2+(NPLAN-1)*NPOIN2
        SEGDOWN=NSEGH+NPOIN2*(NPLAN-2)+I2
        XAUX(I3,1)=TETA*AX(SEGDOWN)
        XAUX(I3,3)=0.D0
      ENDDO
C
C     OTHER PLANES
C
      IF(NPLAN.GT.2) THEN
      DO IPLAN=2,NPLAN-1
        DO I2=1,NPOIN2
          I3=I2+(IPLAN-1)*NPOIN2
          SEGDOWN=NSEGH+NPOIN2*(IPLAN-2)+I2
          SEGUP  =SEGDOWN+NPOIN2
          XAUX(I3,1)=TETA*AX(SEGDOWN)
          XAUX(I3,3)=TETA*AX(SEGUP)
        ENDDO
      ENDDO
      ENDIF
C
      CALL OV('X=CX    ',AX(NSEGH+1:NSEGH+NSEGV),AX,AX,1.D0-TETA,NSEGV)
C
C-----------------------------------------------------------------------
C
C     PARALLEL MODE
C
      IF(NCSIZE.GT.1) THEN
        IAN    = 3
        ICOM   = 2
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