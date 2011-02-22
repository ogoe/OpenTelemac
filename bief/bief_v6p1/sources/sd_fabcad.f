C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS A COMPACT STORAGE
!>               (INX,IPX) STRUCTURE WITH THE DIAGONAL
!>                VIA (IN,IP) = (XADJ, ADJNCY) OF EXTRADIAGONAL TERMS
!>                AND THE SEGMENT STORAGE (ISEGIP, XA, DA).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACTRI, DA, IN, INDTRI, INX, IP, IPX, ISEGIP, ISTRI, NPBLK, NSEGBLK, XA1, XA2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISEG, J, J1, J2, JN, ND
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_FABCAD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SD_STRTRI()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_SOLVE_1()

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
!> </td><td> 20/11/06
!> </td><td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACTRI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INDTRI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IPX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISEGIP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISTRI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPBLK
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NSEGBLK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SD_FABCAD
     &(NPBLK,NSEGBLK,IN,IP,ISEGIP,
     & INDTRI,ISTRI,INX,IPX,ACTRI,XA1,XA2,DA,AC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACTRI          |---| 
C| DA             |---| 
C| IN             |---| 
C| INDTRI         |---| 
C| INX            |---| 
C| IP             |---| 
C| IPX            |---| 
C| ISEGIP         |---| 
C| ISTRI          |---| 
C| NPBLK          |-->| 
C| NSEGBLK        |---| 
C| XA1            |---| 
C| XA2            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_FABCAD => SD_FABCAD
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPBLK,NSEGBLK
      INTEGER, INTENT(IN)             :: IN(NPBLK+1),IP(NSEGBLK*2+1)
      INTEGER, INTENT(IN)             :: ISEGIP(NSEGBLK*2+1)
      INTEGER, INTENT(INOUT)          :: INDTRI(NPBLK)
      INTEGER, INTENT(INOUT)          :: ISTRI(NPBLK)
      INTEGER, INTENT(INOUT)          :: INX(NPBLK+1)
      INTEGER, INTENT(INOUT)          :: IPX(NSEGBLK*2+NPBLK+1)
      DOUBLE PRECISION, INTENT(INOUT) :: ACTRI(NPBLK)
      DOUBLE PRECISION, INTENT(IN)    :: XA1(NSEGBLK),XA2(NSEGBLK)
      DOUBLE PRECISION, INTENT(IN)    :: DA(NPBLK)
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSEGBLK*2+NPBLK+1)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,J1,J2,JN,ISEG,ND
C
C-----------------------------------------------------------------------
C
C---> COMPACT STORAGE WITH THE DIAGONAL : (XADJ, ADJNCY) = (INX,IPX)
C
      DO I = 1, NPBLK+1
        INX(I) = IN(I)+I-1
      ENDDO
      J2=1
      DO I = 1, NPBLK
         IPX(INX(I)) = I
         AC(INX(I)) = DA(I)
         DO J1 = INX(I), INX(I+1)-1
            JN = J1-I+1
            J = IP(JN)
            J2=J2+1
            ISEG = ISEGIP(JN)
            IPX(J2) = J
            IF(ISEG.LT.0) AC(J2) = XA1(-ISEG)
            IF(ISEG.GT.0) AC(J2) = XA2(ISEG)
         ENDDO
      ENDDO
      DO I = 1, NPBLK
         ND = INX(I+1)-INX(I)
         DO J = 1,ND
            ISTRI(J) = IPX(INX(I)+J-1)
            ACTRI(J) = AC(INX(I)+J-1)
         ENDDO
         CALL SD_STRTRI(ISTRI,ND,INDTRI)
         DO J = 1,ND
            J1 = INDTRI(J)
            IPX(INX(I)+J-1) = ISTRI(J1)
            AC(INX(I)+J-1) = ACTRI(J1)
         ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C