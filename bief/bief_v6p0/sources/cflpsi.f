C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COURANT NUMBER AT EACH POINT OF THE MESH
!>                AND FOR EACH TIMESTEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE COORDINATES ARE HERE GIVEN BY ELEMENTS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, IELM, MASKEL, MESH, MSK, SYGMA, U, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CFLPSI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASSVEC(), CFLP11(), CFLP12(), CPSTVC(), NBPTS(), OS(), PARCOM(), PLANTE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRERES_TELEMAC2D(), SUSPENSION_LISTING(), TELEMAC2D(), VGFPSI()

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
!>      <td><center> 5.3                                       </center>
!> </td><td> 17/08/94
!> </td><td> JMH
!> </td><td> MODIFICATIONS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> C MOULIN   (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS DU CALCUL.
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SYGMA
!></td><td><--</td><td>NOMBRE DE COURANT.
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>VITESSE SUIVANT X.
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>VITESSE SUIVANT Y.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CFLPSI
     &(SYGMA,U,V,DT,IELM,MESH,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS DU CALCUL.
C| IELM           |-->| TYPE D'ELEMENT.
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| SYGMA          |<--| NOMBRE DE COURANT.
C| U             |-->| VITESSE SUIVANT X.
C| V             |-->| VITESSE SUIVANT Y.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CFLPSI => CFLPSI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: SYGMA
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U,V,MASKEL
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER         , INTENT(IN)    :: IELM
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      LOGICAL         , INTENT(IN)    :: MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C P1 TRIANGLES
C
      IF(IELM.EQ.11) THEN
C
        CALL CFLP11(U%R,V%R,MESH%XEL%R,MESH%YEL%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%W%R)
C
C-----------------------------------------------------------------------
C
C QUASI-BUBBLE TRIANGLES
C
      ELSEIF(IELM.EQ.12) THEN
C
        CALL CFLP12(U%R,V%R,MESH%XEL%R,MESH%YEL%R,
     &              MESH%IKLE%I,MESH%NELEM,MESH%NELMAX,MESH%W%R)
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,100) IELM
        IF (LNG.EQ.2) WRITE(LU,101) IELM
100     FORMAT(1X,'CFLPSI : IELM = ',1I6,'  CAS NON PREVU |')
101     FORMAT(1X,'CFLPSI: IELM = ',1I6,' COMBINATION NOT AVAILABLE |')
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C ASSEMBLES THE LIJ
C
      CALL ASSVEC(SYGMA%R,MESH%IKLE%I,NBPTS(IELM),
     &            MESH%NELEM,MESH%NELMAX,IELM,
     &            MESH%W%R,.TRUE.,MESH%LV,MSK,MASKEL%R)
C
C-----------------------------------------------------------------------
C
C FINAL RESULT
C
C     MASS OF THE BASES IN BIEF WORKING ARRAY
C
      CALL VECTOR(MESH%T,'=','MASBAS          ',
     &            IELM,1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
C     CORRECTION JMH 27/01/2003
      IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
C
C DIVIDES BY THE MASS OF THE BASES
C
      CALL CPSTVC(MESH%T,SYGMA)
      CALL OS( 'X=CY/Z  ' , SYGMA , SYGMA , MESH%T , DT ,2,0.D0,1.D-6)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C