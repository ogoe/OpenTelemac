C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    RETURNS THE COLUMN INDEX OF THE 1ST CHARACTER CAR
!>             IN THE LINE (EVEN IF IT FOLLOWS '/')
!>             RETURNS THE MAXIMUM LENGTH OF THE LINE IF THIS CHARACTER
!>             IS NOT FOUND.<br>
!>             THIS FUNCTION IS USED TO FIND THE END OF A STRING OF
!>             CHARACTERS. THIS STRING CAN CONTAIN THE CHARACTER '/',
!>             WHICH IS WHY PREVAL IS NOT USED IN THIS CASE (PREVAL
!>             SKIPS COMMENTED LINES).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN
!>  @note      OPTIMISED: SENDS CAR1, CAR2, CAR3 IN THE MOST PROBABLE ORDER

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
!>    </th><td> K
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>AIDELU(), CARLU(), INFLU(), LOGLU()

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
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td>--></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       INTEGER FUNCTION PRECAR
     &( ICOL , LIGNE , CAR1 , CAR2 , CAR3 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CAR1,CAR2,CAR3 |-->| CARACTERES RECHERCHES DANS LA LIGNE
C| ICOL           |-->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| LIGNE          |-->| LIGNE EN COURS DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| NLIGN          |-->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
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
      INTEGER       K
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
      PRECAR = LONGLI
C
      DO 100 K = ICOL,LONGLI
      IF (LIGNE(K:K).EQ.CAR1.OR.LIGNE(K:K).EQ.CAR2.OR.
     &    LIGNE(K:K).EQ.CAR3) THEN
        PRECAR = K
        GO TO 1000
      ENDIF
100   CONTINUE
C
      PRECAR=LONGLI+1
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