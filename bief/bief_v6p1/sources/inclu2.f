C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHECKS IF A WORD IS COMPRISED IN A LIST OF WORDS.
!>                INCLU2=.TRUE. MEANS 'WORD C2 IS COMPRISED IN LIST C1'.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C1, C2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FLAG, I, IC1, IMAX, LC1, LC2
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>COST_FUNCTION(), HOMERE_ADJ_T2D(), PROPAG_ADJ(), TELEMAC2D()

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
!> </td><td> 17/08/94
!> </td><td> J.M. HERVOUET (LNH)   30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C1
!></td><td>--></td><td>LISTE DE MOTS SEPARES PAR AUTRE CHOSE QUE
!>                  A-Z ET 0-9
!>    </td></tr>
!>          <tr><td>C2
!></td><td>--></td><td>MOT RECHERCHE DANS C1
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        LOGICAL FUNCTION INCLU2
     &( C1 , C2 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C1             |-->| LISTE DE MOTS SEPARES PAR AUTRE CHOSE QUE
C|                |   | A-Z ET 0-9
C| C2             |-->| MOT RECHERCHE DANS C1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*(*) C1 , C2
C
      INTEGER I,IC1,LC1,LC2,IMAX
C
      LOGICAL FLAG
C
      INTRINSIC LEN
C
C-----------------------------------------------------------------------
C
      INCLU2 = .FALSE.
C
      LC1 = LEN(C1)
      LC2 = LEN(C2)
      IMAX = LC1-LC2
C
      IF(IMAX.GE.0) THEN
C
         DO 10 I = 0,IMAX
            IF(C1(I+1:I+LC2).EQ.C2(1:LC2)) THEN
               FLAG = .TRUE.
               IF (I.NE.0) THEN
                  IC1 = ICHAR(C1(I:I))
                  IF ((IC1.GE.48.AND.IC1.LE.57).OR.
     &                (IC1.GE.65.AND.IC1.LE.90)) FLAG = .FALSE.
               ENDIF
               IF (I.NE.IMAX) THEN
                  IC1 = ICHAR(C1(I+LC2+1:I+LC2+1))
                  IF ((IC1.GE.48.AND.IC1.LE.57).OR.
     &                (IC1.GE.65.AND.IC1.LE.90)) FLAG = .FALSE.
               ENDIF
               INCLU2 = INCLU2.OR.FLAG
            ENDIF
10       CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C