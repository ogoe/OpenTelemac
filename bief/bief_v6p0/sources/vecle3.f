C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DETERMINES THE LENGTH OF A VECTOR WITHOUT BACK
!>                DEPENDENCIES FOR LOOPS ON THE ELEMENTS.
!><br>            ONLY LOOKS FOR VALUES :
!>                1 , 64 , 128 , 256 , 512 , 1024.
!><br>            THE PRINCIPLE IS TO PERFORM, IN SCALAR AND VECTOR
!>                MODE, AN ALGORITHM WHICH COMPUTES THE NUMBER OF
!>                ADJACENT ELEMENTS AT EACH POINT.
!><br>            ELEMENT CONSIDERED: TRIANGLE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IN VECTOR MODE WITH DEPENDENCIES, THE RESULT IS WRONG

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, LV, NELEM, NELMAX, NPOIN, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IB, IELEM, Y, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DOT(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECLEN()

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
!> </td><td> 19/08/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td> ORIGINAL IDEA FROM J.-P. GREGOIRE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE
!>    </td></tr>
!>          <tr><td>LV
!></td><td><--</td><td>LONGUEUR DE VECTEUR ADMISSIBLE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>TABLEAU DE TRAVAIL REEL, DE DIMENSION NPOIN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VECLE3
     & (LV,IKLE,NELEM,NELMAX,NPOIN,V)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT
C| IKLE           |-->| TABLE DE CONNECTIVITE
C| LV             |<--| LONGUEUR DE VECTEUR ADMISSIBLE
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE TOTAL D'ELEMENTS
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| V             |-->| TABLEAU DE TRAVAIL REEL, DE DIMENSION NPOIN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VECLE3 => VECLE3
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(INOUT)          :: LV
      INTEGER, INTENT(IN)             :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
      DOUBLE PRECISION, INTENT(INOUT) :: V(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IB
      DOUBLE PRECISION Y(1),Z(1)
C
C-----------------------------------------------------------------------
C
      LV = 1024
C
5     CONTINUE
C
C  INITIALISES V AT 0
C
      CALL OV( 'X=C     ' , V , Y , Z , 0.D0 , NPOIN )
C
C  SCALAR MODE
C
      DO 10 IELEM = 1 , NELEM
        V(IKLE(IELEM,1)) = V(IKLE(IELEM,1)) + 1.D0
        V(IKLE(IELEM,2)) = V(IKLE(IELEM,2)) + 1.D0
        V(IKLE(IELEM,3)) = V(IKLE(IELEM,3)) + 1.D0
10    CONTINUE
C
C  VECTOR MODE WITH FORCED VECTORISATION
C (FUJITSU COMMANDS, THEN CRAY COMMANDS)
C
      DO 20 IB = 1,(NELEM+LV-1)/LV
CVOCL LOOP,NOVREC
CDIR$ IVDEP
      DO 30 IELEM = 1+(IB-1)*LV , MIN(NELEM,IB*LV)
        V(IKLE(IELEM,1)) = V(IKLE(IELEM,1)) - 1.D0
        V(IKLE(IELEM,2)) = V(IKLE(IELEM,2)) - 1.D0
        V(IKLE(IELEM,3)) = V(IKLE(IELEM,3)) - 1.D0
30    CONTINUE
20    CONTINUE
C
C-----------------------------------------------------------------------
C
      IF(DOT(NPOIN,V,V).GT.0.5D0.AND.LV.NE.1) THEN
        LV = LV/2
        IF(LV.LT.64) THEN
          LV = 1
          GO TO 1000
        ENDIF
        GO TO 5
      ENDIF
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C