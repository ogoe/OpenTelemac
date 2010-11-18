C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FOR WAVE EQUATION, COMPUTES THE PSEUDO-VISCOSITY
!>                AS A PIECE-WISE CONSTANT VARIABLE.
!><br>            THE COMPUTATION IS DONE SO THAT THE SUM ON THE
!>                VERTICAL OF FLUXES DUE TO GRADIENT OF FREE SURFACE
!>                COMPUTED IN VC04PP GIVES THE SAME RESULT
!>               (SEE VCGRADP WITH SPECAD=.TRUE.).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DM1, DZ, IKLE, NELEM2, NELMAX, NPLAN, NPOIN2, NUWAVE, XMUL, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> F1, F2, F3, G1, G2, G3, I1, I2, I3, IELEM, IETAGE, XSUR12
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAVE_EQUATION()

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
!> </td><td> 22/06/06
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DM1
!></td><td>--></td><td>D**-1 (SEE BOOK)
!>    </td></tr>
!>          <tr><td>DZ
!></td><td><--</td><td>DELTA(Z) : HEIGHT OF PRISMS
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CONNECTIVITY TABLE IN 2D MESH
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF 2D ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF 2D ELEMENTS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS PAR PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NUWAVE
!></td><td><--</td><td>PSEUDO-VISCOSITY IN WAVE EQUATION
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>MULTIPLICATIVE CONSTANT
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NUWAVE_P0
     &(NUWAVE,DM1,Z,DZ,IKLE,NPOIN2,NPLAN,NELMAX,NELEM2,XMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DM1            |-->| D**-1 (SEE BOOK)
C| DZ             |<--| DELTA(Z) : HEIGHT OF PRISMS
C| IKLE           |-->| CONNECTIVITY TABLE IN 2D MESH
C| NELEM2         |-->| NUMBER OF 2D ELEMENTS
C| NELMAX         |-->| MAXIMUM NUMBER OF 2D ELEMENTS
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS PAR PLANS HORIZONTAUX
C| NUWAVE         |<--| PSEUDO-VISCOSITY IN WAVE EQUATION
C| XMUL           |-->| MULTIPLICATIVE CONSTANT
C| Z             |-->| COTES DES NOEUDS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN)    :: NPOIN2,NPLAN,NELMAX,NELEM2
      INTEGER,          INTENT(IN)    :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: NUWAVE(NELEM2),DZ(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: DM1(NPOIN2,NPLAN),XMUL
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IETAGE,I1,I2,I3,IELEM
      DOUBLE PRECISION F1,F2,F3,G1,G2,G3,XSUR12
!
!***********************************************************************
!
      XSUR12=XMUL/12.D0
!
C     COMPUTES DELTA(Z) FOR ALL THE LEVELS : IN DZ
!
      CALL OV('X=Y-Z   ',DZ,Z(1,2),Z(1,1),0.D0,NPOIN2*(NPLAN-1))
!
C     SUMS AVERAGE DZ*DM1 FOR EACH LEVEL
C     AVERAGE DZ*DM1 = INTEGRAL OF DZ*DM1 ON THE TRIANGLE
C     DIVIDED BY THE TRIANGLE AREA
!
      CALL OV('X=C     ',NUWAVE,NUWAVE,NUWAVE,0.D0,NELEM2)
!
      DO IETAGE=1,NPLAN-1
        DO IELEM=1,NELEM2
!
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
C       FUNCTION F : DZ
C       FUNCTION G : AVERAGE DM1 FOR EACH VERTICAL
        F1=DZ(I1,IETAGE)
        F2=DZ(I2,IETAGE)
        F3=DZ(I3,IETAGE)
        G1=0.5D0*(DM1(I1,IETAGE)+DM1(I1,IETAGE+1))
        G2=0.5D0*(DM1(I2,IETAGE)+DM1(I2,IETAGE+1))
        G3=0.5D0*(DM1(I3,IETAGE)+DM1(I3,IETAGE+1))
        NUWAVE(IELEM)=NUWAVE(IELEM)+
     &   (F3*(G1+G2)+2*(F1*G1+F2*G2+F3*G3)+F2*(G1+G3)+F1*(G2+G3))*XSUR12
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C