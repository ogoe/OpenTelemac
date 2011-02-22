C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COLLAPSE OF SAND WITH A SLOPE GREATER THAN A
!>                STABILITY CRITERION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EVOL, IKLE, MESH, NELEM, NELMAX, NPOIN, SLOPE, UNSV2D, XEL, YEL, ZF, ZR
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
!>    </th><td> A, B, DEUXSURF, I, I1, I2, I3, IELEM, L, TANSL, X2, X3, Y2, Y3, Z2, Z3, ZC
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MAXSLOPE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), OS(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.8                                       </center>
!> </td><td> 16/11/2007
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>EVOL
!></td><td><-></td><td>WORK ARRAY, THEN EVOLUTION DUE TO SLIDE
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CONNECTIVITY TABLE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MESH STRUCTURE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NUMBER OF ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>SLOPE
!></td><td>--></td><td>MAXIMUM SLOPE IN DEGREES
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>--></td><td>INVERSE OF INTEGRAL OF BASES
!>    </td></tr>
!>          <tr><td>XEL,YEL
!></td><td>--></td><td>MESH COORDINATES PER ELEMENT
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>BOTTOM THAT WILL BE MODIFIED
!>    </td></tr>
!>          <tr><td>ZR
!></td><td>--></td><td>NON ERODABLE BED
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MAXSLOPE
     &(SLOPE,ZF,ZR,XEL,YEL,NELEM,NELMAX,NPOIN,IKLE,EVOL,UNSV2D,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| EVOL           |<->| WORK ARRAY, THEN EVOLUTION DUE TO SLIDE
C| IKLE           |-->| CONNECTIVITY TABLE
C| MESH           |-->| MESH STRUCTURE
C| NELEM          |-->| NUMBER OF ELEMENTS
C| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
C| SLOPE          |-->| MAXIMUM SLOPE IN DEGREES
C| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASES
C| XEL,YEL        |-->| MESH COORDINATES PER ELEMENT
C| ZF             |<->| BOTTOM THAT WILL BE MODIFIED
C| ZR             |-->| NON ERODABLE BED
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      USE INTERFACE_SISYPHE, EX_MAXSLOPE => MAXSLOPE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
C
      DOUBLE PRECISION, INTENT(IN   ) :: SLOPE
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(NELMAX,3),YEL(NELMAX,3)
C
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: EVOL
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I1,I2,I3,I
      DOUBLE PRECISION X2,X3,Y2,Y3,Z2,Z3,A,B,L,ZC,DEUXSURF,TANSL
C
      INTRINSIC SQRT,MIN,MAX,TAN
C
C-----------------------------------------------------------------------
C
      TANSL=TAN(3.141592653589D0*SLOPE/180.D0)
C
C     INITIALISES THE RIGHT-HAND SIDE EVOL TO ZERO
C
      CALL CPSTVC(UNSV2D,EVOL)
      CALL OS('X=0     ',X=EVOL)
C
C     LOOP ON ELEMENTS
C
      DO IELEM=1,NELEM
C
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
C
        X2=XEL(IELEM,2)
        X3=XEL(IELEM,3)
        Y2=YEL(IELEM,2)
        Y3=YEL(IELEM,3)
        Z2=ZF(I2)-ZF(I1)
        Z3=ZF(I3)-ZF(I1)
C
C       TWICE THE TRIANGLE AREA
C
        DEUXSURF=X2*Y3-X3*Y2
C
C       AVERAGE BOTTOM IN THE ELEMENT
C
        ZC=(ZF(I1)+ZF(I2)+ZF(I3))/3.D0
C
C       COMPONENTS OF BOTTOM GRADIENT
C
        A=(Z2*Y3-Z3*Y2)/DEUXSURF
        B=(Z3*X2-Z2*X3)/DEUXSURF
C
C       CORRECTING FACTOR ON SLOPE
C
        L=MIN(1.D0,TANSL/MAX(SQRT(A**2+B**2),1.D-8))
C
C       L LIMITED DUE TO NON-ERODABLE BEDS : ZF MUST NOT GO BELOW ZR
C
        IF(ZF(I1).GT.ZC) L=MAX(L,(ZR(I1)-ZC)/MAX(ZF(I1)-ZC,1.D-8))
        IF(ZF(I2).GT.ZC) L=MAX(L,(ZR(I2)-ZC)/MAX(ZF(I2)-ZC,1.D-8))
        IF(ZF(I3).GT.ZC) L=MAX(L,(ZR(I3)-ZC)/MAX(ZF(I3)-ZC,1.D-8))
C
C       BUILDS THE RIGHT-HAND SIDE
C
        EVOL%R(I1)=EVOL%R(I1)+(1.D0-L)*(ZC-ZF(I1))*DEUXSURF/6.D0
        EVOL%R(I2)=EVOL%R(I2)+(1.D0-L)*(ZC-ZF(I2))*DEUXSURF/6.D0
        EVOL%R(I3)=EVOL%R(I3)+(1.D0-L)*(ZC-ZF(I3))*DEUXSURF/6.D0
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     FINAL RESOLUTION
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(EVOL,2,MESH)
      ENDIF
C
      DO I=1,NPOIN
        EVOL%R(I)=EVOL%R(I)*UNSV2D%R(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C