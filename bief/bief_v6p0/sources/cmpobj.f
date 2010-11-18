C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPARES 2 OBJECTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> OBJ1, OBJ2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM1, IELM2, TYP1, TYP2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CMPOBJ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>OS()

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
!> </td><td> 01/03/90
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>OBJ1,2
!></td><td><-></td><td>LES DEUX STRUCTURES A COMPARER
!>    </td></tr>
!>          <tr><td>OBJ2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        LOGICAL FUNCTION CMPOBJ
     &( OBJ1 , OBJ2 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| OBJ1,2         |<->| LES DEUX STRUCTURES A COMPARER
C| OBJ2           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CMPOBJ => CMPOBJ
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN) ::  OBJ1,OBJ2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELM1,IELM2,TYP1,TYP2
C
C-----------------------------------------------------------------------
C
      CMPOBJ = .FALSE.
C
      TYP1 = OBJ1%TYPE
      TYP2 = OBJ2%TYPE
C
      IF(TYP1.EQ.TYP2) THEN
C
        IF(TYP1.EQ.2) THEN
C
C         VECTORS: CHECKS THE DISCRETISATION
C
          IELM1 = OBJ1%ELM
          IELM2 = OBJ2%ELM
          IF(IELM1.EQ.IELM2) CMPOBJ = .TRUE.
C
        ELSEIF(TYP1.EQ.4) THEN
C
C         BLOCKS: CHECKS THE NUMBER OF OBJECTS
C
          IF(OBJ1%N.EQ.OBJ2%N) CMPOBJ=.TRUE.
C
        ELSE
C
          IF(LNG.EQ.1) WRITE(LU,100)
          IF(LNG.EQ.2) WRITE(LU,101)
100       FORMAT(1X,'CMPOBJ (BIEF) : POUR BLOCS ET VECTEURS SEULEMENT')
101       FORMAT(1X,'CMPOBJ (BIEF) : FOR BLOCS AND VECTORS ONLY')
          CALL PLANTE(0)
          STOP
        ENDIF
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