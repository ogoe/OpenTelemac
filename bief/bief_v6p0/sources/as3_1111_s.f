C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!>                IN THE CASE OF EDGE-BASED STORAGE.
!><br>            CASE OF LINEAR-LINEAR ELEMENT
!>                AND SYMMETRICAL MATRIX.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG1, ELTSEG2, ELTSEG3, NELEM, NELMAX, NSEG1, XM, XMT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, ISEG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ASSEX3()

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
!> </td><td> 30/06/99
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELTSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELTSEG3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLE ET W.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TR
!></td><td>--></td><td>TABLEAU DE TRAVAIL DE TAILLE > NPTFR
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XM2
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX XA21,32,31
!>    </td></tr>
!>          <tr><td>XMAS
!></td><td><--</td><td>TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
!>    </td></tr>
!>          <tr><td>XMT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AS3_1111_S
     &(XM,NSEG1,XMT,NELMAX,NELEM,ELTSEG1,ELTSEG2,ELTSEG3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG1        |---| 
C| ELTSEG2        |---| 
C| ELTSEG3        |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NSEG1          |---| 
C| TR             |-->| TABLEAU DE TRAVAIL DE TAILLE > NPTFR
C| XM             |---| 
C| XM2            |-->| TERMES EXTRA-DIAGONAUX XA21,32,31
C| XMAS           |<--| TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
C| XMT            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG1
      INTEGER         , INTENT(IN)    :: ELTSEG1(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG2(NELMAX)
      INTEGER         , INTENT(IN)    :: ELTSEG3(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEG,IELEM
C
C-----------------------------------------------------------------------
C
C  INITIALISES
C
      DO ISEG = 1 , NSEG1
        XM(ISEG) = 0.D0
      ENDDO
C
C  ASSEMBLES
C
      DO IELEM = 1,NELEM
        XM(ELTSEG1(IELEM)) = XM(ELTSEG1(IELEM)) + XMT(IELEM,1)
        XM(ELTSEG2(IELEM)) = XM(ELTSEG2(IELEM)) + XMT(IELEM,3)
        XM(ELTSEG3(IELEM)) = XM(ELTSEG3(IELEM)) + XMT(IELEM,2)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C