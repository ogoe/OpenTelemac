C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IDENTIFIES TIDAL FLATS.<br>
!><br>            IMPLEMENTS DELFINA ET AL WETTING/DRYING ALGORITHM.
!><br>            PARTIALLY WET ELEMENT : TETA = 0
!><br>            WET ELEMENT           : TETA = NU = 1.0
!><br>            DRY ELEMENT           : TETA = NU = 0.0<br>
!><br>            THE DRYING CRITERION IS THAT OF J.-M. JANIN :
!>                BOTTOM ELEVATION AT ONE NODE OF THE ELEMENT IS
!>                HIGHER THAN FREE SURFACE ELEVATION AT ANOTHER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HN, IKLE, NELEM, NELMAX, TETA, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPS1, EPS2, H1, H2, H3, IELEM, OPTNU, SL1, SL2, SL3, TEST1, TEST2, Y, Z, ZF1, ZF2, ZF3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>POROS()

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
!> </td><td> 01/08/1997
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; PAUL BATES (BRISTOL) 44 117 928 9108
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
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
!></td><td><--</td><td>NU (PAR ELEMENT)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PORO11
     &(TETA,ZF,HN,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HN             |---| 
C| IKLE           |---| 
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| NELEM          |---| 
C| NELMAX         |---| 
C| SL,ZF          |-->| SURFACE LIBRE ET FOND
C| TETA           |<--| NU (PAR ELEMENT)
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
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(*),HN(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TETA(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,OPTNU
C
      DOUBLE PRECISION Y(1),Z(1)
      DOUBLE PRECISION SL1,SL2,SL3,ZF1,ZF2,ZF3,H1,H2,H3,EPS1,EPS2
C
      LOGICAL TEST1, TEST2
C
      INTRINSIC MAX,MIN
C
C-----------------------------------------------------------------------
C
      CALL OV( 'X=C     ' , TETA , Y , Z , 1.D0 , NELEM )
C
C--------------------------------------------------------------
C
C  OPTION FOR COMPUTATION OF NU
C
C  1 = EXACT CALCULATION BASED ON PROJECTED INUNDATED AREA
C  2 = ROUGH APPROXIMATION TO INUNDATED AREA
C  3 = CONSTANT VALUE
C  4 = USER SPECIFIED
C
      EPS1=1.D-6
      EPS2=0.3D0
C
C-----------------------------------------------------------------------
C
      DO 4 IELEM = 1 , NELEM
C
        ZF1 = ZF(IKLE(IELEM,1))
        ZF2 = ZF(IKLE(IELEM,2))
        ZF3 = ZF(IKLE(IELEM,3))
C
        H1 = HN(IKLE(IELEM,1))
        H2 = HN(IKLE(IELEM,2))
        H3 = HN(IKLE(IELEM,3))
C
        SL1 = H1 + ZF1
        SL2 = H2 + ZF2
        SL3 = H3 + ZF3
C
        TEST1 = .FALSE.
        TEST2 = .FALSE.
C
C       IF TIDAL FLAT SUSPECTED AND SOME WATER IN ELEMENT
C       (POROSITY LEFT TO 1 IN TOTALLY DRY ELEMENTS)
C
        IF(     MAX(ZF1,ZF2,ZF3).GT.MIN(SL1,SL2,SL3)
     &     .AND.MAX(H1,H2,H3).GT.EPS1                ) TEST1 = .TRUE.
C
C
C
        IF(MAX(H1,H2,H3).EQ.H1.AND.SL1.GT.MIN(ZF1,ZF2,ZF3)
     &                        .AND.SL1.LT.MAX(ZF1,ZF2,ZF3).OR.
     &     MAX(H1,H2,H3).EQ.H2.AND.SL2.GT.MIN(ZF1,ZF2,ZF3)
     &                        .AND.SL2.LT.MAX(ZF1,ZF2,ZF3).OR.
     &     MAX(H1,H2,H3).EQ.H3.AND.SL3.GT.MIN(ZF1,ZF2,ZF3)
     &                    .AND.SL3.LT.MAX(ZF1,ZF2,ZF3)) TEST2=.TRUE.
C
C---------------------------------------------------------------------
C
        IF(TEST1.AND.TEST2) THEN
C         ROUGH CALCULATION OF POROSITY BY PERCENTAGE OF ELEMENT INUNDATED
          TETA(IELEM)=MAX(H1,H2,H3)/(MAX(ZF1,ZF2,ZF3)-MIN(ZF1,ZF2,ZF3))
          TETA(IELEM) = MAX(TETA(IELEM),EPS2)
        ENDIF
C
C---------------------------------------------------------------------
C
4     CONTINUE
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C