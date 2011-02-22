C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON VECTORS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.<br>
!>   HERE X IS A VECTOR DEFINED ON THE BOUNDARY.
!>   Y AND Z ARE VECTORS DEFINED IN THE DOMAIN.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   OP = 'X=Y     '     :  COPIES Y IN X
!>   OP = 'X=+Y    '     :  IDEM
!>   OP = 'X=X+Y   '     :  ADDS Y TO X
!>   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!>   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!>   OP = 'X=X+CY  '     :  ADDS C.Y TO X
!>   OP = 'X=CXY   '     :  C.X.Y
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, NBOR, NPTFR, OP, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MASK3D(), OSBD()

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
!> </td><td> 06/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION GLOBALE DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>CHAINE DE CARACTERES INDIQUANT L'OPERATION
!>                  A EFFECTUER.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR RESULTAT
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OVBD
     & ( OP , X , Y , Z , C , NBOR , NPTFR )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| NBOR           |-->| NUMEROTATION GLOBALE DES POINTS DE BORD.
C| NPTFR          |---| 
C| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
C|                |   | A EFFECTUER.
C| X             |<--| VECTEUR RESULTAT
C| Y             |-->| VECTEUR OPERANDE
C| Z             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPTFR,NBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),Z(*),C
      CHARACTER(LEN=8), INTENT(IN)    :: OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'X=Y     '.OR.
     &   OP(1:8).EQ.'X=+Y    ') THEN
C
        DO K=1,NPTFR
          X(K) = Y(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+Y   ') THEN
C
        DO K=1,NPTFR
          X(K) = X(K) + Y(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=Y+Z   ') THEN
C
        DO K=1,NPTFR
          X(K) = Y(NBOR(K)) + Z(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-Y   ') THEN
C
        DO K=1,NPTFR
          X(K) = X(K) - Y(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=CY    ') THEN
C
        DO K=1,NPTFR
          X(K) = C * Y(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CY  ') THEN
C
        DO K=1,NPTFR
          X(K) = X(K) + C * Y(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=CXY   ') THEN
C
        DO K=1,NPTFR
          X(K) = C * X(K) * Y(NBOR(K))
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,1000) OP
         IF (LNG.EQ.2) WRITE(LU,1001) OP
1000     FORMAT(1X,'OVBD (BIEF) : OPERATION INCONNUE: ',A8)
1001     FORMAT(1X,'OVBD (BIEF) : UNKNOWN OPERATION: ',A8)
         CALL PLANTE(1)
         STOP
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