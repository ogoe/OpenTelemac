C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLEMENTS A VECTOR AT THE INTERFACES BETWEEN
!>                SUB-DOMAINS. HERE BOUNDARY VECTOR OF TYPE 1.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ICOM, MESH, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, NPTFR, TDIM1, TDIM2, TDIMDISC, TELM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARCOM_BORD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBPTS(), OS(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORNOR(), DIFFIN(), NORMAB(), TBORD()

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
!> </td><td> 24/10/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> REINHARD HINKELMANN (HANNOVER UNI.)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
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
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR DE BORD.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARCOM_BORD
     &( X , ICOM , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ICOM           |-->| COMMUNICATION MODE
C|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
C|                |   | = 2 : CONTRIBUTIONS ADDED
C|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
C|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
C| MESH           |-->| MAILLAGE.
C| X             |<->| VECTEUR DE BORD.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARCOM_BORD => PARCOM_BORD
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      INTEGER, INTENT(IN) :: ICOM
C
C     STRUCTURES: VECTORS OR BLOCKS
C
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
C
C-----------------------------------------------------------------------
C
      INTEGER NPTFR,I,TELM,TDIM1,TDIM2,TDIMDISC
C
C***********************************************************************
C
      NPTFR=BIEF_NBPTS(1,MESH)
C
      TELM     = MESH%T%ELM
      TDIM1    = MESH%T%DIM1
      TDIM2    = MESH%T%DIM2
      TDIMDISC = MESH%T%DIMDISC
C
      MESH%T%ELM     = 11
      MESH%T%DIM1    = BIEF_NBPTS(11,MESH)
      MESH%T%DIM2    = 1
      MESH%T%DIMDISC = 0
C
      CALL OS('X=0     ',X=MESH%T)
C
      DO I=1,NPTFR
        MESH%T%R(MESH%NBOR%I(I))=X(I)
      ENDDO
C
      CALL PARCOM(MESH%T,ICOM,MESH)
C
      DO I=1,NPTFR
        X(I)=MESH%T%R(MESH%NBOR%I(I))
      ENDDO
C
      MESH%T%ELM     = TELM
      MESH%T%DIM1    = TDIM1
      MESH%T%DIM2    = TDIM2
      MESH%T%DIMDISC = TDIMDISC
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
