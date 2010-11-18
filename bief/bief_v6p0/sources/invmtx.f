C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INVERTS A MATRIX OF NP BY NP.
!><br>            BM IS THE INVERSION OF AM.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THIS SUBROUTINE CALLS LUDCMP AND LUBKSB, WHICH WERE
!>         COPIED FROM "NUMERIC RECIPES" -- A WELL-KNOWN BOOK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AM, BM, NP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, INDX, J, N
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LUBKSB(), LUDCMP(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SPECTRE()

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
!> </td><td> 28/07/2006
!> </td><td> CHUN WANG
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                            SUBROUTINE INVMTX
     &(AM,BM,NP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AM             |---| 
C| BM             |---| 
C| NP             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NP
      DOUBLE PRECISION, INTENT(INOUT) :: AM(NP,NP),BM(NP,NP)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER INDX(500),N,I,J
C
C-----------------------------------------------------------------------
C
      IF(NP.GT.500) THEN
        WRITE(LU,*) 'NP MUST BE LESS THAN 500 IN INVMTX'
        CALL PLANTE(1)
        STOP
      ENDIF
C
      N = NP
      DO I=1,N !SET UP IDENTITY MATRIX
        DO J=1,N
          BM(I,J)=0.D0
        ENDDO
        BM(I,I)=1.D0
      ENDDO
C
C     DECOMPOSES THE MATRIX JUST ONCE
C
      CALL LUDCMP(AM,N,NP,INDX)
C
C     FINDS INVERSE BY COLUMNS
C
      DO J=1,N
        CALL LUBKSB(AM,N,NP,INDX,BM(1,J))
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C