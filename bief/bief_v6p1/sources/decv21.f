C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IDENTIFIES TIDAL FLATS.
!><br>            DRYING ELEMENT : TETA = 0,
!><br>            NORMAL ELEMENT : TETA = 1.
!><br>            THE CRITERION FOR DRYING ELEMENTS IS THAT OF
!>                J.-M. JANIN : BOTTOM ELEVATION OF A POINT IN AN
!>                ELEMENT BEING HIGHER THAN THE FREE SURFACE
!>                ELEVATION OF ANOTHER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, NELEM, NELMAX, SL, TETA, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, SL1, SL2, SL3, SL4, ZF1, ZF2, ZF3, ZF4
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DECVRT()

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
!> </td><td> 09/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SL,ZF
!></td><td>--></td><td>SURFACE LIBRE ET FOND
!>    </td></tr>
!>          <tr><td>TETA
!></td><td><--</td><td>INDICATEUR (PAR ELEMENT)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DECV21
     &(TETA,SL,ZF,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |---| 
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| NELEM          |---| 
C| NELMAX         |---| 
C| SL,ZF          |-->| SURFACE LIBRE ET FOND
C| TETA           |<--| INDICATEUR (PAR ELEMENT)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)  :: NELEM,NELMAX
      INTEGER         , INTENT(IN)  :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(OUT) :: TETA(NELEM)
      DOUBLE PRECISION, INTENT(IN)  :: SL(*),ZF(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
C
      DOUBLE PRECISION SL1,SL2,SL3,SL4,ZF1,ZF2,ZF3,ZF4
C
      INTRINSIC MAX,MIN
C
C-----------------------------------------------------------------------
C
      CALL OV( 'X=C     ' , TETA , TETA , TETA , 1.D0 , NELEM )
C
C-----------------------------------------------------------------------
C
         DO 1 IELEM = 1 , NELEM
C
           SL1 = SL(IKLE(IELEM,1))
           SL2 = SL(IKLE(IELEM,2))
           SL3 = SL(IKLE(IELEM,3))
           SL4 = SL(IKLE(IELEM,4))
C
           ZF1 = ZF(IKLE(IELEM,1))
           ZF2 = ZF(IKLE(IELEM,2))
           ZF3 = ZF(IKLE(IELEM,3))
           ZF4 = ZF(IKLE(IELEM,4))
C
           IF(MAX(ZF1,ZF2,ZF3,ZF4).GT.MIN(SL1,SL2,SL3,SL4)) THEN
             TETA(IELEM) = 0.D0
           ENDIF
C
1        CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C