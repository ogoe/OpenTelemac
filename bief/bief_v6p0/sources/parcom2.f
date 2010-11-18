C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLEMENTS A VECTOR AT THE INTERFACES BETWEEN
!>                SUB-DOMAINS.
!><br>            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!>                VECTORS IN THE BLOCK ARE TREATED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : FROM RELEASE 5.9 ON, IDENTICAL VALUES ARE
!>                     ENSURED AT INTERFACE POINTS SO THAT DIFFERENT
!>                     PROCESSORS WILL ALWAYS MAKE THE SAME DECISION
!>                     IN TESTS ON REAL NUMBERS.

!>  @warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!>            IGNORED FOR THE TIME BEING

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IAN, ICOM, MESH, NPLAN, NPOIN, X1, X2, X3
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARCOM2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARACO()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GETTRIEBE(), GETTRISEG(), INTER4D(), INTERP_TOMAWAC(), PARCOM(), PARCOM2_SEG(), PREPRO(), PREVEREBE(), PREVERSEG(), TOMAWAC_MPI(), TOMAWAC_MPI_TOOLS()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 13/08/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> AFTER REINHARD HINKELMANN (HANNOVER UNI.)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICOM
!></td><td>--></td><td>COMMUNICATION MODE
!>                  = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!>                  = 2 : CONTRIBUTIONS ADDED
!>                  = 3 : MAXIMUM CONTRIBUTION RETAINED
!>                  = 4 : MINIMUM CONTRIBUTION RETAINED
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR OU BLOC DE VECTEURS.
!>    </td></tr>
!>          <tr><td>X1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X3
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARCOM2
     &( X1 , X2 , X3 , NPOIN , NPLAN , ICOM , IAN , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IAN            |---| 
C| ICOM           |-->| COMMUNICATION MODE
C|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
C|                |   | = 2 : CONTRIBUTIONS ADDED
C|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
C|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
C| MESH           |-->| MAILLAGE.
C| NPLAN          |---| 
C| NPOIN          |---| 
C| X             |<->| VECTEUR OU BLOC DE VECTEURS.
C| X1             |---| 
C| X2             |---| 
C| X3             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARCOM2 => PARCOM2
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      INTEGER, INTENT(IN) :: ICOM,NPOIN,NPLAN,IAN
C
C     STRUCTURES: VECTORS OR BLOCKS
C
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: X1(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: X2(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: X3(NPOIN,NPLAN)
C
C-----------------------------------------------------------------------
C
      CALL PARACO(X1,X2,X3,NPOIN,ICOM,IAN,NPLAN,
     &            MESH%NB_NEIGHB,MESH%NB_NEIGHB_PT%I,MESH%LIST_SEND%I,
     &            MESH%NH_COM%I,MESH%NH_COM%DIM1,MESH%BUF_SEND%R,
     &            MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1)
C
C     RELEASE 5.9 : ENSURES SAME VALUES AT INTERFACE POINTS
C                   SHARED BY SEVERAL SUB-DOMAINS
C
      IF(ICOM.EQ.2.AND.NCSIZE.GT.2) THEN
C
      CALL PARACO(X1,X2,X3,NPOIN,1,IAN,NPLAN,
     &            MESH%NB_NEIGHB,MESH%NB_NEIGHB_PT%I,MESH%LIST_SEND%I,
     &            MESH%NH_COM%I,MESH%NH_COM%DIM1,MESH%BUF_SEND%R,
     &            MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1)
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