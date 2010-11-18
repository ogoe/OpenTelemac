C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       THE POINTS OF VECTOR F, WHICH ARE INSIDE OF THE
!>                POLYGON DEFINED BY VERTICES XSOM AND YSOM, ARE
!>                INITIALISED TO CONSTANT C.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE POLYGON IS READ ANTI-CLOCKWISE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, F, MESH, NSOM, XSOM, YSOM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM, NPOIN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FILPOL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FILP10(), FILP11(), FILP12(), PLANTE()
!>   </td></tr>
!>     </table>

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
!> </td><td> 08/12/94
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
!></td><td>--></td><td>CONSTANTE IMPOSEE A L'INTERIEUR DU POLYGONE.
!>    </td></tr>
!>          <tr><td>F
!></td><td><-></td><td>VECTEUR RESULTAT.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NSOM
!></td><td>--></td><td>NOMBRE DE SOMMETS.
!>    </td></tr>
!>          <tr><td>XMESH
!></td><td>--></td><td>BLOC DES TABLEAUX DE REELS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XSOM, YSOM
!></td><td>--></td><td>TABLEAUX DES COORDONNEES DES SOMMETS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FILPOL
     &( F , C , XSOM , YSOM , NSOM , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE IMPOSEE A L'INTERIEUR DU POLYGONE.
C| F             |<->| VECTEUR RESULTAT.
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C| NSOM           |-->| NOMBRE DE SOMMETS.
C| XMESH          |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE.
C| XSOM, YSOM     |-->| TABLEAUX DES COORDONNEES DES SOMMETS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FILPOL => FILPOL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NSOM
      DOUBLE PRECISION, INTENT(IN) :: C,XSOM(*),YSOM(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPOIN,IELM
C
C-----------------------------------------------------------------------
C
C  TYPE OF ELEMENT FOR VELOCITY
C
      IELM = F%ELM
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.10) THEN
C
        CALL FILP10( F%R , C , XSOM , YSOM , NSOM ,
     &       MESH%X%R,MESH%Y%R,MESH%NELEM,MESH%NELMAX,MESH%IKLE%I)
C
      ELSEIF(IELM.EQ.11) THEN
C
        NPOIN = F%DIM1
        CALL FILP11(F%R,C,XSOM,YSOM,NSOM,MESH%X%R,MESH%Y%R,NPOIN)
C
      ELSEIF(IELM.EQ.12) THEN
C
        NPOIN = F%DIM1 - MESH%NELEM
        CALL FILP12( F%R , C , XSOM , YSOM , NSOM , MESH%X%R , MESH%Y%R,
     &               NPOIN , MESH%NELEM , MESH%NELMAX , MESH%IKLE%I)
C
      ELSE
C
C       ELM NOT IMPLEMENTED: ERROR
C
        IF (LNG.EQ.1) WRITE(LU,100) IELM
        IF (LNG.EQ.2) WRITE(LU,101) IELM
100     FORMAT(1X,'FILPOL (BIEF) : IELM = ',1I6,' ELEMENT NON PREVU')
101     FORMAT(1X,'FILPOL (BIEF): IELM = ',1I6,' ELEMENT NOT AVAILABLE')
        CALL PLANTE(0)
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