C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FINDS THE NEAREST FROM -1 AND TO +1 POINTER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IFRM, IFRM1, ITO, ITOP1, NODENRS, NPOIN2, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DISFRM, DISTO, DX, DY, IPOIN, XFRM1, XTOP1, YFRM1, YTOP1
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GREDELMET(), TEL4DEL()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 03/04/2007
!> </td><td> LEO POSTMA (DELFT HYDRAULICS)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IFRM
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>IFRM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITO
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ITOP1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NODENRS
!></td><td>--></td><td>IF > 0 : NODE NUMBER
!>                  IF
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>NODE COORDINATES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FDNRST
     &(IFRM,ITO,X,Y,NODENRS,NPOIN2,IFRM1,ITOP1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IFRM           |-->| 
C| IFRM1          |---| 
C| ITO            |-->| 
C| ITOP1          |---| 
C| NODENRS        |-->| IF > 0 : NODE NUMBER
C|                |   | IF
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| X,Y            |-->| NODE COORDINATES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: IFRM,ITO,NPOIN2
      INTEGER, INTENT(IN)          :: NODENRS(NPOIN2)
      INTEGER, INTENT(INOUT)       :: IFRM1,ITOP1
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN2), Y(NPOIN2)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN
      DOUBLE PRECISION XFRM1,XTOP1,YFRM1,YTOP1,DISFRM,DISTO,DX,DY
C
C-----------------------------------------------------------------------
C
      XFRM1  = X(IFRM)
      YFRM1  = Y(IFRM)
      XTOP1  = X(ITO )
      YTOP1  = Y(ITO )
      DISFRM = XFRM1-XTOP1
      XFRM1  = XFRM1 + DISFRM
      XTOP1  = XTOP1 - DISFRM
      DISFRM = YFRM1-YTOP1
      YFRM1  = YFRM1 + DISFRM
      YTOP1  = YTOP1 - DISFRM
C
      DX     = XFRM1-X(1)
      DY     = YFRM1-Y(1)
      DISFRM = DX*DX + DY*DY
      DX     = XTOP1-X(1)
      DY     = YTOP1-Y(1)
      DISTO  = DX*DX + DY*DY
      IFRM1  = 1
      ITOP1  = 1
      DO IPOIN = 2, NPOIN2
         DX     = XFRM1-X(IPOIN)
         DY     = YFRM1-Y(IPOIN)
         DX     = DX*DX + DY*DY
         IF(DX.LT.DISFRM) THEN
           DISFRM = DX
           IFRM1  = IPOIN
         ENDIF
         DX     = XTOP1-X(IPOIN)
         DY     = YTOP1-Y(IPOIN)
         DX     = DX*DX + DY*DY
         IF(DX.LT.DISTO) THEN
           DISTO  = DX
           ITOP1  = IPOIN
         ENDIF
      ENDDO
      IFRM1 = NODENRS(IFRM1)
      ITOP1 = NODENRS(ITOP1)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C