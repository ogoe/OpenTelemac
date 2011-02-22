C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CONDITIONAL OPERATIONS ON VECTORS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.<br>
!>   HERE X IS A VECTOR DEFINED IN THE DOMAIN.
!>   Y IS A VECTOR DEFINED ON THE BOUNDARY.
!>   X, Y AND Z MUST BE STRUCTURES.<br>
!>   INDIC IS AN ARRAY: NOT A STRUCTURE ||||||||<br>
!>   |||||||| : THE OPERATION IS ONLY PERFORMED IF THE CONDITION
!>              INDIC(K)=CRITER IS MET FOR A BOUNDARY NODE K.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   OP = 'X=Y     '     :  COPIES Y IN X
!>   OP = 'X=+Y    '     :  IDEM
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CRITER, INDIC, MESH, OP, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELMX, IELMY, K, NPTFR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DIMENS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DIRI01(), DIRI04(), DIRI09()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 22/08/05
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CRITER
!></td><td>--></td><td>CRITERE POUR FAIRE L'OPERATION.
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TABLEAU D'INDICATEURS.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DU MAILLAGE
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OSDBIF
     & ( OP , X , Y , INDIC , CRITER , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CRITER         |-->| CRITERE POUR FAIRE L'OPERATION.
C| INDIC          |-->| TABLEAU D'INDICATEURS.
C| MESH           |-->| STRUCTURE DU MAILLAGE
C| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
C|                |   | A EFFECTUER.
C| X             |<--| VECTEUR RESULTAT
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      TYPE(BIEF_OBJ) :: X,Y
      TYPE(BIEF_MESH) :: MESH
C
      INTEGER K,NPTFR,IELMX,IELMY
      INTEGER INDIC(*),CRITER
C
      CHARACTER*8 OP
C
C-----------------------------------------------------------------------
C
      IF(X%TYPE.NE.2.OR.Y%TYPE.NE.2) THEN
        IF (LNG.EQ.1) WRITE(LU,100)
        IF (LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'OSDBIF (BIEF) : X ET Y NE SONT PAS DES VECTEURS')
101     FORMAT(1X,'OSDBIF (BIEF) : X AND Y ARE NOT VECTORS')
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IELMX = X%ELM
      IELMY = Y%ELM
!
C JP RENAUD 18/08/2005
C MODIFICATION FOR 3D MESHES: THE DOMAIN VECTOR DIMENSION IS 3
C AND THE BOUNDARY VECTOR DIMENSION IS 2. SO THE POSSIBLE
C COMBINATIONS ARE:
C     -2D: DIMESN(IELMX)==2 _AND_ DIMESN(IELMY)==1
C     -3D: DIMESN(IELMX)==3 _AND_ DIMESN(IELMY)==2
!
      IF( .NOT. (DIMENS(IELMX).EQ.3 .AND.DIMENS(IELMY).EQ.2 )
     &    .AND.
     &    .NOT. (DIMENS(IELMX).EQ.2 .AND. DIMENS(IELMY).EQ.1 ) ) THEN
!
        IF (LNG.EQ.1) WRITE(LU,102)
        IF (LNG.EQ.2) WRITE(LU,103)
102     FORMAT(1X,'OSDBIF (BIEF) : X ET Y MAUVAISES DIMENSIONS')
103     FORMAT(1X,'OSDBIF (BIEF) : X AND Y WRONG DIMENSIONS')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      NPTFR = Y%DIM1
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'X=Y     '.OR.
     &   OP(1:8).EQ.'X=+Y    ') THEN
C
        DO K=1,NPTFR
          IF(INDIC(K).EQ.CRITER) X%R(MESH%NBOR%I(K)) = Y%R(K)
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,1000) OP
         IF (LNG.EQ.2) WRITE(LU,1001) OP
1000     FORMAT(1X,'OSDBIF (BIEF) : OPERATION INCONNUE: ',A8)
1001     FORMAT(1X,'OSDBIF (BIEF) : UNKNOWN OPERATION: ',A8)
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