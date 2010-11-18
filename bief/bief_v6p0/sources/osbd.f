C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON VECTORS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.<br>
!>   HERE X IS A VECTOR DEFINED ON THE BOUNDARY.
!>   Y AND Z ARE VECTORS DEFINED IN THE DOMAIN.
!>   X, Y AND Z MUST BE STRUCTURES.<br>
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

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, MESH, OP, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMX, IELMY, NPTFR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OSBD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DIMENS(), OVBD(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BILANT1(), CELERITE(), PROPAG(), SUSPENSION_COMPUTATION(), WAVE_EQUATION()

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
!>      <td><center> 5.5                                       </center>
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
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>DIMENSION DES VECTEURS
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
                        SUBROUTINE OSBD
     & ( OP , X , Y , Z , C , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| MESH           |---| 
C| NPOIN          |-->| DIMENSION DES VECTEURS
C| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
C|                |   | A EFFECTUER.
C| X             |<--| VECTEUR RESULTAT
C| Y             |-->| VECTEUR OPERANDE
C| Z             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OSBD => OSBD
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DOUBLE PRECISION, INTENT(IN) :: C
C
      CHARACTER(LEN=8), INTENT(IN) :: OP
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
      TYPE(BIEF_OBJ), INTENT(IN)    :: Y,Z
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER NPTFR,IELMX,IELMY
C
C-----------------------------------------------------------------------
C
      IF(X%TYPE.NE.2.OR.Y%TYPE.NE.2) THEN
        IF (LNG.EQ.1) WRITE(LU,100)
        IF (LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'OSBD (BIEF) : X ET Y NE SONT PAS DES VECTEURS')
101     FORMAT(1X,'OSBD (BIEF) : X AND Y ARE NOT VECTORS')
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IELMX = X%ELM
      IELMY = Y%ELM
C
      IF((DIMENS(IELMX).NE.1.OR.DIMENS(IELMY).NE.2).AND.
     &   (DIMENS(IELMX).NE.2.OR.DIMENS(IELMY).NE.3)) THEN
        IF (LNG.EQ.1) WRITE(LU,102)
        IF (LNG.EQ.2) WRITE(LU,103)
102     FORMAT(1X,'OSBD (BIEF) : X ET Y ONT DE MAUVAISES DIMENSIONS')
103     FORMAT(1X,'OSBD (BIEF) : X AND Y HAVE WRONG DIMENSIONS')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      NPTFR = X%DIM1
C
      IF(  IELMY.EQ.11.OR.
     &     IELMY.EQ.61.OR.
     &     IELMY.EQ.21.OR.
     &     IELMY.EQ.71.OR.
     &     IELMY.EQ.12.OR.
     &     IELMY.EQ.31.OR.
     &     IELMY.EQ.41.OR.
     &     IELMY.EQ.51      ) THEN
C       ARRAY NBOR
        CALL OVBD( OP , X%R , Y%R , Z%R , C , MESH%NBOR%I , NPTFR )
!
C  JMH 23/06/2008: TEMPORARY: TO SEE WHO NEEDS IT
C                  CREATES AN ERROR IN PARALLEL MODE BECAUSE
C                  NELBOR CAN BE 0
!
C     ELSEIF(IELMY.EQ.10.OR.
C    *       IELMY.EQ.20.OR.
C    *       IELMY.EQ.30.OR.
C    *       IELMY.EQ.70.OR.
C    *       IELMY.EQ.40.OR.
C    *       IELMY.EQ.50     ) THEN
C       ARRAY NELBOR
C       CALL OVBD( OP , X%R , Y%R , Z%R , C , MESH%NELBOR%I , NPTFR )
      ELSE
        IF (LNG.EQ.1) WRITE(LU,104)
        IF (LNG.EQ.2) WRITE(LU,105)
104     FORMAT(1X,'OSBD (BIEF) : DISCRETISATIONS NON PREVUES')
105     FORMAT(1X,'OSBD (BIEF) : DISCRETIZATIONS NOT IMPLEMENTED')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C