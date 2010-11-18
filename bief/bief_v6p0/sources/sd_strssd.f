C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS COMPACT STORAGE
!>               (IN,IP) = (XADJ, ADJNCY)OF EXTRADIAGONAL TERMS
!>                VIA SEGMENT STORAGE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  NSEG AND IW ARE THE SAME ARRAY IN THE MEMORY

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GLOSEG1, GLOSEG2, IN, IP, ISEGIP, IW, NPBLK, NSEGBLK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISEG, J1, J2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_STRSSD
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 18/02/08
!> </td><td> E.RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>GLOSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISEGIP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPBLK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEGBLK
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SD_STRSSD
     &(NPBLK,NSEGBLK,GLOSEG1,GLOSEG2,IN,IP,ISEGIP,IW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GLOSEG1        |---| 
C| GLOSEG2        |---| 
C| IN             |---| 
C| IP             |---| 
C| ISEGIP         |---| 
C| IW             |---| 
C| NPBLK          |---| 
C| NSEGBLK        |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF,EX_SD_STRSSD => SD_STRSSD
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NSEGBLK,NPBLK
      INTEGER, INTENT(IN)    :: GLOSEG1(NSEGBLK),GLOSEG2(NSEGBLK)
      INTEGER, INTENT(INOUT) :: IN(NPBLK+1),IP(NSEGBLK*2+1)
      INTEGER, INTENT(INOUT) :: ISEGIP(NSEGBLK*2+1)
      INTEGER, INTENT(INOUT) :: IW(NPBLK)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J1,J2,ISEG
C
C  -----------------------
C
C---> DEGREE OF A POINT: NUMBER OF NEIGHBOURS
C
      DO I=1,NPBLK
        IW(I)=0
      ENDDO
C
C     IW : NUMBER OF NEIGHBOURS FOR EACH POINT
C
      DO ISEG=1,NSEGBLK
        J1 = GLOSEG1(ISEG)
        J2 = GLOSEG2(ISEG)
        IW(J1) = IW(J1) + 1
        IW(J2) = IW(J2) + 1
      ENDDO
C
C---> COMPACT STORAGE WITHOUT THE DIAGONAL: (XADJ,ADJNCY) = (IN,IP)
C
C     COEFFICIENTS FOR POINT I: FROM IN(I) TO IN(I+1)-1
C
      IN(1)=1
      DO I=1,NPBLK
        IN(I+1)=IN(I)+IW(I)
      ENDDO
C
C     IW IS NO LONGER THE NUMBER OF NEIGHBOURS
C
      DO I=1,NPBLK
        IW(I)=IN(I)
      ENDDO
C
      DO ISEG=1,NSEGBLK
        J1 = GLOSEG1(ISEG)
        J2 = GLOSEG2(ISEG)
C--> TABLE OF CONNECTIVITY: SEGMENT ---> POINT
        IP(IW(J1))=J2
        IP(IW(J2))=J1
C--> INVERSE TABLE OF CONNECTIVITY: POINT ---> SEGMENT
C    NOTATION FOR TRIANGULAR SUPERIOR COEFF.
        ISEGIP(IW(J1))=-ISEG
C    NOTATION FOR TRIANGULAR INFERIOR COEFF.
        ISEGIP(IW(J2))= ISEG
C
        IW(J1) = IW(J1) + 1
        IW(J2) = IW(J2) + 1
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C