C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    RETURNS THE INDEX OF THE 1ST NON-WHITE, NON-TABULATION
!>             AND NON-COMMENT CHARACTER OF THE LINE, BEFORE COLUMN ICOL
!>             COLUMN ICOL IS EXCLUDED
!>             IF CANNOT FIND ANY, PREV = ICOL

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ICOL, LIGNE
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J, TABUL
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOC()

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
!>    <td> J.M. HERVOUET (LNH); A. YESSAYAN                        </td>
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
!>          <tr><td>ICOL
!></td><td>--></td><td>POSITION COURANTE DU POINTEUR DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>LIGNE
!></td><td>--></td><td>LIGNE EN COURS DE DECODAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        INTEGER FUNCTION PREV
     &( ICOL , LIGNE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ICOL           |-->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| LIGNE          |-->| LIGNE EN COURS DE DECODAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER       ICOL
      CHARACTER*(*) LIGNE
C
C-----------------------------------------------------------------------
C
      INTEGER       I,J
      CHARACTER*1   TABUL
      INTRINSIC CHAR
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
      TABUL = CHAR(9)
      PREV   = ICOL
      I      = ICOL
C
  100 CONTINUE
      I = I - 1
      IF ( I.LT.1 ) GO TO 1000
C
      IF (LIGNE(I:I).EQ.' '.OR.LIGNE(I:I).EQ.TABUL) GOTO 100
C
C-----------------------------------------------------------------------
C          DOES NOT CONSIDER THE COMMENTED LINES:
C
           IF ( LIGNE(I:I).NE.'/' ) THEN
                PREV = I
                GO TO 1000
           ELSE
                IF ( I.LE.1 ) GO TO 1000
                DO 110 J = I-1 , 1 , -1
                     IF ( LIGNE(J:J).EQ.'/' ) THEN
                          I = J
                          GO TO 100
                     ENDIF
  110           CONTINUE
           ENDIF
C-----------------------------------------------------------------------
C
 1000 CONTINUE
C
      RETURN
      END

C
C#######################################################################
C