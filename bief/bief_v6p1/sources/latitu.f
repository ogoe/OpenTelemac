C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ARRAYS THAT DEPEND ON THE LATITUDE
!>                OF THE GIVEN POINT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COSLAT, LAMBD0, NPOIN, SINLAT, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, LB2RAD, PISUR2, PISUR4, SURR, XLAMB
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 10/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COSLAT
!></td><td><--</td><td>COS(LAMBDA)
!>    </td></tr>
!>          <tr><td>LAMBD0
!></td><td>--></td><td>LATITUDE DU POINT ORIGINE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DANS LE MAILLAGE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU PRIVE DE DIMENSION (NPOIN,NPRIV)
!>                  NPRIV EST DONNE DANS LE PROGRAMME PRINCIPAL
!>    </td></tr>
!>          <tr><td>SINLAT
!></td><td><--</td><td>SIN(LAMBDA)
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LATITU
     &(COSLAT,SINLAT,LAMBD0,  Y,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COSLAT         |<--| COS(LAMBDA)
C| LAMBD0         |-->| LATITUDE DU POINT ORIGINE
C| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
C| PRIVE          |-->| TABLEAU PRIVE DE DIMENSION (NPOIN,NPRIV)
C|                |   | NPRIV EST DONNE DANS LE PROGRAMME PRINCIPAL
C| SINLAT         |<--| SIN(LAMBDA)
C| Y             |-->| COORDONNEES DES POINTS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: COSLAT(NPOIN),SINLAT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: LAMBD0
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      DOUBLE PRECISION LB2RAD,SURR,PISUR4,PISUR2,XLAMB
C
      INTRINSIC TAN,ATAN,SIN,COS,EXP
C
C-----------------------------------------------------------------------
C
C EARTH RADIUS
C
      SURR = 1.D0 / 6400000.D0
C
C-----------------------------------------------------------------------
C
      PISUR4 = ATAN(1.D0)
      PISUR2 = PISUR4 + PISUR4
C
C  LAMBD0/2 IN RADIANS
C
      LB2RAD = LAMBD0 * PISUR4 / 90.D0
C
C  1/COS(LAMBDA),COS(LAMBDA),SIN(LAMBDA)
C
      DO 10 I = 1 , NPOIN
C
        XLAMB = 2.D0* ATAN(EXP(Y(I)*SURR)*TAN(LB2RAD+PISUR4))-PISUR2
        COSLAT(I) = COS(XLAMB)
        SINLAT(I) = SIN(XLAMB)
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C