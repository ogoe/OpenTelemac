C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    RETURNS THE COLUMN INDEX OF THE 1ST VALID CHARACTER CAR
!>             IN THE LINE (I.E. NON-WHITE, NON-TABULATION AND NON-COMMENTED
!>             STRING)
!>             IF CANNOT FIND IT, RETURNS LONGLI + 1

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CAR1, CAR2, CAR3, ICOL, LIGNE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCMLIG : NLIGN, LONGLI
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CMD(), DAMOC(), INTLU(), READ_SUBMIT(), REALU()

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
!>          <tr><td>CAR1,CAR2,CAR3
!></td><td>--></td><td>CARACTERES RECHERCHES DANS LA LIGNE
!>    </td></tr>
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
                        INTEGER FUNCTION PREVAL
     &( ICOL , LIGNE , CAR1 , CAR2 , CAR3 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CAR1,CAR2,CAR3 |-->| CARACTERES RECHERCHES DANS LA LIGNE
C| ICOL           |-->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| LIGNE          |-->| LIGNE EN COURS DE DECODAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER       ICOL
      CHARACTER*1   CAR1,CAR2,CAR3
      CHARACTER*(*) LIGNE
C
      INTEGER       LONGLI,NLIGN
C
C-----------------------------------------------------------------------
C
      INTEGER       I,J
C
C-----------------------------------------------------------------------
C
      COMMON / DCMLIG / NLIGN,LONGLI
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
      PREVAL = LONGLI + 1
      I      = ICOL -1
C
100   CONTINUE
      I = I + 1
      IF (LIGNE(I:I).NE.CAR1.AND.LIGNE(I:I).NE.CAR2.AND.
     &    LIGNE(I:I).NE.CAR3) THEN
C-----------------------------------------------------------------------
C          DOES NOT CONSIDER THE COMMENTED LINES:
C
           IF ( I.GE.LONGLI ) GO TO 1000
           IF ( LIGNE(I:I).EQ.'/' ) THEN
                DO 110 J = I+1 , LONGLI
                     IF ( LIGNE(J:J).EQ.'/' ) THEN
                          I = J
                          GO TO 100
                     ENDIF
  110           CONTINUE
                GO TO 1000
C-----------------------------------------------------------------------
           ELSE
                GO TO 100
           ENDIF
      ELSE
           PREVAL = I
      ENDIF
C
1000  CONTINUE
C
      RETURN
      END

C
C#######################################################################
C