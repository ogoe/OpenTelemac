C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       UPWINDS THE ADVECTION TERM OF VERTICAL VELOCITY.
!>  @code
!>        A DIFFUSION TERM WITH DIFFUSION COEFFICIENT ABS(WCC)*DZ/2
!>        IS ADDED TO THE MATRIX. FORMULA IS OBTAINED BY SIMPLIFYING
!>        THE Z PART OF DIFFUSION MATRIX BUILT IN SUBROUTINE MT02PP
!>        DZ THEN VANISHES.<br>
!>        THIS IS USED IN DIFF3D FOR SEDIMENT SETTLING VELOCITY
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> D, DELTA, EXT, IKLE, NELEM2, NELEM3, NELMAX, NPLAN, NSEG2D, NSEG3D, SURFAC, WCC, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DS6, I, I1, I2, I3, I4, I5, I6, IELEM2, IELEM3, ISEG1, ISEG2, ISEG3, UP1, UP2, UP3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>UPWIND()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 12/12/05
!> </td><td> J.M. HERVOUET  (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>D
!></td><td><-></td><td>MATRIX DIAGONAL
!>    </td></tr>
!>          <tr><td>DELTA
!></td><td>--></td><td>UPWIND COEFFICIENT (BETWEEN 0 AND 1)
!>    </td></tr>
!>          <tr><td>EXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CONNECTIVITY TABLE
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 2D MESH
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 3D MESH
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF ELEMENTS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES ON THE VERTICAL
!>    </td></tr>
!>          <tr><td>NSEG2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>AREA OF TRIANGLES
!>    </td></tr>
!>          <tr><td>WCC
!></td><td>--></td><td>VELOCITY (NEGATIVE IF SETTLING VELOCITY)
!>                  CAN BE ALSO WSCONV IN THE TRANSFORMED MESH
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>MATRIX OFF-DIAGONAL TERMS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE UPWINDSEG
     &(D,X,IKLE,NELMAX,NELEM3,NELEM2,SURFAC,NPLAN,WCC,NSEG2D,NSEG3D,EXT,
     & DELTA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| D             |<->| MATRIX DIAGONAL
C| DELTA          |-->| UPWIND COEFFICIENT (BETWEEN 0 AND 1)
C| EXT            |---| 
C| IKLE           |-->| CONNECTIVITY TABLE
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
C| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D MESH
C| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
C| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
C| NSEG2D         |---| 
C| NSEG3D         |---| 
C| SURFAC         |-->| AREA OF TRIANGLES
C| WCC            |-->| VELOCITY (NEGATIVE IF SETTLING VELOCITY)
C|                |   | CAN BE ALSO WSCONV IN THE TRANSFORMED MESH
C| X             |<->| MATRIX OFF-DIAGONAL TERMS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELMAX,NELEM3,NELEM2,NPLAN
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6),NSEG2D,NSEG3D
      DOUBLE PRECISION, INTENT(IN   ) :: SURFAC(NELMAX),WCC(*),DELTA
      DOUBLE PRECISION, INTENT(INOUT) :: D(*),X(NSEG3D,2)
      CHARACTER(LEN=1), INTENT(IN)    :: EXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,I1,I2,I3,I4,I5,I6,IELEM2,IELEM3,ISEG1,ISEG2,ISEG3
      DOUBLE PRECISION UP1,UP2,UP3,DS6
C
      INTRINSIC ABS
C
C=======================================================================
C
C=======================================================================
C
      DS6=DELTA/6.D0
!
      IF(EXT.EQ.'S') THEN
!
      DO I=1,NPLAN-1
        DO IELEM2=1,NELEM2
          IELEM3=IELEM2+(I-1)*NELEM2
          I1=IKLE(IELEM3,1)
          I2=IKLE(IELEM3,2)
          I3=IKLE(IELEM3,3)
          I4=IKLE(IELEM3,4)
          I5=IKLE(IELEM3,5)
          I6=IKLE(IELEM3,6)
C         ADDRESS OF VERTICAL SEGMENTS IN LAYER I
C         THEY COME AFTER HORIZONTAL SEGMENTS WHICH ARE NSEG2D*NPLAN
          ISEG1=NPLAN*NSEG2D+I1
          ISEG2=NPLAN*NSEG2D+I2
          ISEG3=NPLAN*NSEG2D+I3
C         AVERAGE VELOCITY TAKEN AT THE BOTTOM OF THE PRISM
C         IF IT IS WSCONV (CALL BY PRECON, IT IS CONSTANT ON THE VERTICAL)
C         AND WCC(I4,I5 OR I6) NOT DEFINED FOR LAST LAYER OF ELEMENTS
C         IF IT IS THE SETTLING VELOCITY, IT IS CONSTANT EVERYWHERE
          UP1=SURFAC(IELEM2)*DS6*ABS(WCC(I1))
          UP2=SURFAC(IELEM2)*DS6*ABS(WCC(I2))
          UP3=SURFAC(IELEM2)*DS6*ABS(WCC(I3))
          X(ISEG1,1)=X(ISEG1,1)-UP1
          X(ISEG2,1)=X(ISEG2,1)-UP2
          X(ISEG3,1)=X(ISEG3,1)-UP3
          D(I1)=D(I1)+UP1
          D(I2)=D(I2)+UP2
          D(I3)=D(I3)+UP3
          D(I4)=D(I4)+UP1
          D(I5)=D(I5)+UP2
          D(I6)=D(I6)+UP3
        ENDDO
      ENDDO
!
      ELSEIF(EXT.EQ.'Q') THEN
!
      DO I=1,NPLAN-1
        DO IELEM2=1,NELEM2
          IELEM3=IELEM2+(I-1)*NELEM2
          I1=IKLE(IELEM3,1)
          I2=IKLE(IELEM3,2)
          I3=IKLE(IELEM3,3)
          I4=IKLE(IELEM3,4)
          I5=IKLE(IELEM3,5)
          I6=IKLE(IELEM3,6)
C         ADDRESS OF VERTICAL SEGMENTS IN LAYER I
C         THEY COME AFTER HORIZONTAL SEGMENTS WHICH ARE NSEG2D*NPLAN
          ISEG1=NPLAN*NSEG2D+I1
          ISEG2=NPLAN*NSEG2D+I2
          ISEG3=NPLAN*NSEG2D+I3
C         AVERAGE VELOCITY TAKEN AT THE BOTTOM OF THE PRISM
C         IF IT IS WSCONV (CALL BY PRECON, IT IS CONSTANT ON THE VERTICAL)
C         AND WCC(I4,I5 OR I6) NOT DEFINED FOR LAST LAYER OF ELEMENTS
C         IF IT IS THE SETTLING VELOCITY, IT IS CONSTANT EVERYWHERE
          UP1=SURFAC(IELEM2)*DS6*ABS(WCC(I1))
          UP2=SURFAC(IELEM2)*DS6*ABS(WCC(I2))
          UP3=SURFAC(IELEM2)*DS6*ABS(WCC(I3))
          X(ISEG1,1)=X(ISEG1,1)-UP1
          X(ISEG2,1)=X(ISEG2,1)-UP2
          X(ISEG3,1)=X(ISEG3,1)-UP3
          X(ISEG1,2)=X(ISEG1,2)-UP1
          X(ISEG2,2)=X(ISEG2,2)-UP2
          X(ISEG3,2)=X(ISEG3,2)-UP3
          D(I1)=D(I1)+UP1
          D(I2)=D(I2)+UP2
          D(I3)=D(I3)+UP3
          D(I4)=D(I4)+UP1
          D(I5)=D(I5)+UP2
          D(I6)=D(I6)+UP3
        ENDDO
      ENDDO
!
      ELSE
        IF (LNG.EQ.1) WRITE(LU,1000) EXT
        IF (LNG.EQ.2) WRITE(LU,1001) EXT
1000    FORMAT(1X,'UPWINDSEG : VALEUR DE EXT NON PREVUE : ',A1)
1001    FORMAT(1X,'UPWINDSEG: UNEXPECTED VALUE OF EXT: ',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C