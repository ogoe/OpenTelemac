C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    RETURNS THE POSITION OF THE LAST NON-WHITE AND NON-
!>             TABULATION CHARACTER OF THE LINE IN ARGUMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> LIGNE
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, LONG, TABUL
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CARLU(), CMD(), DAMOC(), INFLU()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 16/08/1994                                              </td>
!>    <td> J.M. HERVOUET (LNH); A. YESSAYAN; L. LEGUE              </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 15/12/1993                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH) 30.87.78.70                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>LIGNE
!></td><td>--></td><td>ARGUMENT A ANALYSER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        INTEGER FUNCTION LONGLU
     &( LIGNE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| LIGNE          |-->| ARGUMENT A ANALYSER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      CHARACTER*(*) LIGNE
C
C-----------------------------------------------------------------------
C
      INTEGER       I,LONG
      CHARACTER*1   TABUL
      INTRINSIC CHAR
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
      TABUL = CHAR(9)
      LONG = LEN(LIGNE)
      IF (LONG .EQ. 0) THEN
        I = 0
        GO TO 110
      ENDIF
      DO 100 I = LONG , 1 , -1
      IF (LIGNE(I:I).NE.' '.AND.LIGNE(I:I).NE.TABUL) GO TO 110
100   CONTINUE
110   CONTINUE
      LONGLU = I
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C