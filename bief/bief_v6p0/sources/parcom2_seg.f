C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLEMENTS A VECTOR OF SEGMENT AT THE INTERFACES
!>                BETWEEN SUB-DOMAINS.
!><br>            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!>                VECTORS IN THE BLOCK ARE TREATED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IN 3D, THE FINITE VOLUME SEGMENTS IN PRISMS ARE
!>         CONSIDERED HERE, I.E. HORIZONTAL FIRST : NSEG*NPLAN
!>         THEN, VERTICAL : NPOIN2*(NPLAN-1).

!>  @warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!>            IGNORED FOR THE TIME BEING

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IAN, ICOM, MESH, NPLAN, NSEG, OPT, X1, X2, X3
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IDEB, IFIN, IPLAN, NPOIN2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARCOM2_SEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARACO(), PARCOM2()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVTRVF(), CVTRVF_POS(), MASKTO(), PARCOM(), POSITIVE_DEPTHS(), PRECON()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 19/10/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IAN
!></td><td>--></td><td>NUMBER OF VECTORS TO BE TREATED (X1, X2, X3)
!>    </td></tr>
!>          <tr><td>ICOM
!></td><td>--></td><td>COMMUNICATION MODE
!>                  = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!>                  = 2 : CONTRIBUTIONS ADDED
!>                  = 3 : MAXIMUM CONTRIBUTION RETAINED
!>                  = 4 : MINIMUM CONTRIBUTION RETAINED
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NUMBER OF 2D SEGMENTS
!>    </td></tr>
!>          <tr><td>OPT
!></td><td>--></td><td>1 : HORIZONTAL AND VERTICAL SEGMENTS ONLY
!>                  2 : ALL SEGMENTS
!>    </td></tr>
!>          <tr><td>X1,X2,X3
!></td><td><-></td><td>3 VECTORS CAN BE TREATED, SEE IAN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARCOM2_SEG
     &( X1 , X2 , X3 , NSEG , NPLAN , ICOM , IAN , MESH , OPT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IAN            |-->| NUMBER OF VECTORS TO BE TREATED (X1, X2, X3)
C| ICOM           |-->| COMMUNICATION MODE
C|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
C|                |   | = 2 : CONTRIBUTIONS ADDED
C|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
C|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
C| MESH           |-->| MAILLAGE 2D
C| NPLAN          |-->| NUMBER OF PLANES
C| NSEG           |-->| NUMBER OF 2D SEGMENTS
C| OPT            |-->| 1 : HORIZONTAL AND VERTICAL SEGMENTS ONLY
C|                |   | 2 : ALL SEGMENTS
C| X1,X2,X3       |<->| 3 VECTORS CAN BE TREATED, SEE IAN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARCOM2_SEG => PARCOM2_SEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: ICOM,NSEG,NPLAN,IAN,OPT
C
C     STRUCTURES: VECTORS OR BLOCKS
C
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
C
C     IN 2D X1(NSEG)
C     IN 3D X1(NSEG*NPLAN+NPOIN2*(NPLAN-1))
C
      DOUBLE PRECISION, INTENT(INOUT) :: X1(*),X2(*),X3(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPOIN2,IDEB,IFIN,IPLAN
C
C-----------------------------------------------------------------------
C
      IF(NPLAN.GT.1) THEN
C
C       1) HORIZONTAL FLUXES
C
        DO IPLAN=1,NPLAN
          IDEB=1+NSEG*(IPLAN-1)
          IFIN=  NSEG* IPLAN
          CALL PARACO(X1(IDEB:IFIN),X2(IDEB:IFIN),X3(IDEB:IFIN),
     &                NSEG,ICOM,IAN,1,
     &                MESH%NB_NEIGHB_SEG,MESH%NB_NEIGHB_PT_SEG%I,
     &                MESH%LIST_SEND_SEG%I,MESH%NH_COM_SEG%I,
     &                MESH%NH_COM_SEG%DIM1,MESH%BUF_SEND%R,
     &                MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1)
        ENDDO
C
C       2) VERTICAL FLUXES
C
        NPOIN2=MESH%NPOIN
        DO IPLAN=1,NPLAN-1
          IDEB=NSEG*NPLAN + NPOIN2*(IPLAN-1) + 1
          IFIN=NSEG*NPLAN + NPOIN2* IPLAN
          CALL PARCOM2(X1(IDEB:IFIN),X2(IDEB:IFIN),X3(IDEB:IFIN),
     &                 NPOIN2,1,ICOM,IAN,MESH)
        ENDDO
C
C       3) CROSSED FLUXES
C
        IF(OPT.EQ.2) THEN
          DO IPLAN=1,NPLAN-1
            IDEB=NSEG*NPLAN + NPOIN2*(NPLAN-1) + 2*NSEG*(IPLAN-1) + 1
            IFIN=NSEG*NPLAN + NPOIN2*(NPLAN-1) + 2*NSEG* IPLAN
            CALL PARACO(X1(IDEB:IFIN),X2(IDEB:IFIN),X3(IDEB:IFIN),
     &                  NSEG,ICOM,IAN,2,
     &                  MESH%NB_NEIGHB_SEG,MESH%NB_NEIGHB_PT_SEG%I,
     &                  MESH%LIST_SEND_SEG%I,MESH%NH_COM_SEG%I,
     &                  MESH%NH_COM_SEG%DIM1,MESH%BUF_SEND%R,
     &                  MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1)
          ENDDO
        ENDIF
C
      ELSE
C
        CALL PARACO(X1,X2,X3,
     &              NSEG,ICOM,IAN,1,
     &              MESH%NB_NEIGHB_SEG,MESH%NB_NEIGHB_PT_SEG%I,
     &              MESH%LIST_SEND_SEG%I,MESH%NH_COM_SEG%I,
     &              MESH%NH_COM_SEG%DIM1,MESH%BUF_SEND%R,
     &              MESH%BUF_RECV%R,MESH%BUF_SEND%DIM1)
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