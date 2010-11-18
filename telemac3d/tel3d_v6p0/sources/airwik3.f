C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ENSURES THE CONDITION U . N = 0  (U AND N ARE VECTORS).
!><br>           (FOR A LATERAL SOLID BOUNDARY, DUPLICATES THE NORMAL
!>                COMPONENT OF THE VELOCITY, COMPUTED BY TELEMAC, ON THE VERTICAL).
!><br>            ALSO ENSURES THE DIRICHLET CONDITIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KLOG, LIHBOR, NBOR, NPLAN, NPOIN2, NPTFR, U, V, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN, IPOIN, IPTFR, PSCAL
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAVE_EQUATION()

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
!> </td><td> 29/02/08
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>CONVENTION POUR LES PAROIS AVEC ADHERENCE
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>CONVENTION POUR LES VALEURS IMPOSEES (ENT.)
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR LES PAROIS AVEC GLISSEMENT
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>CONVENTION POUR LES VALEURS LIBRES (SORT.)
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOF,LIVBOF
!></td><td>--></td><td>TYPES DE C.L. SUR U ET V AU FOND
!>    </td></tr>
!>          <tr><td>LIUBOL,LIVBOL
!></td><td>--></td><td>TYPES DE C.L. SUR U ET V SUR LES PAROIS LAT.
!>    </td></tr>
!>          <tr><td>LIUBOS,LIVBOS
!></td><td>--></td><td>TYPES DE C.L. SUR U ET V EN SURFACE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATION FRONTIERE ET
!>                  NUMEROTATION GLOBALE EN 2D
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES 2D
!>    </td></tr>
!>          <tr><td>U , V
!></td><td><-></td><td>VITESSES U ET V 3D
!>    </td></tr>
!>          <tr><td>U2D , V2D
!></td><td><-></td><td>VITESSES U ET V 2D
!>    </td></tr>
!>          <tr><td>UBORF,VBORF
!></td><td>--></td><td>VITESSES U ET V IMPOSEES AU FOND
!>    </td></tr>
!>          <tr><td>UBORL,VBORL
!></td><td>--></td><td>VITESSES U ET V IMPOSEES SUR LES PAROIS LAT.
!>    </td></tr>
!>          <tr><td>UBORS,VBORS
!></td><td>--></td><td>VITESSES U ET V IMPOSEES EN SURFACE
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AIRWIK3
     & (LIHBOR,U,V,XNEBOR,YNEBOR,NBOR,NPTFR,NPLAN,NPOIN2,KLOG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KADH           |-->| CONVENTION POUR LES PAROIS AVEC ADHERENCE
C| KENT           |-->| CONVENTION POUR LES VALEURS IMPOSEES (ENT.)
C| KLOG           |-->| CONVENTION POUR LES PAROIS AVEC GLISSEMENT
C| KSORT          |-->| CONVENTION POUR LES VALEURS LIBRES (SORT.)
C| LIHBOR         |---| 
C| LIUBOF,LIVBOF  |-->| TYPES DE C.L. SUR U ET V AU FOND
C| LIUBOL,LIVBOL  |-->| TYPES DE C.L. SUR U ET V SUR LES PAROIS LAT.
C| LIUBOS,LIVBOS  |-->| TYPES DE C.L. SUR U ET V EN SURFACE
C| NBOR           |-->| CORRESPONDANCE NUMEROTATION FRONTIERE ET
C|                |   | NUMEROTATION GLOBALE EN 2D
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES 2D
C| U , V          |<->| VITESSES U ET V 3D
C| U2D , V2D      |<->| VITESSES U ET V 2D
C| UBORF,VBORF    |-->| VITESSES U ET V IMPOSEES AU FOND
C| UBORL,VBORL    |-->| VITESSES U ET V IMPOSEES SUR LES PAROIS LAT.
C| UBORS,VBORS    |-->| VITESSES U ET V IMPOSEES EN SURFACE
C| XNEBOR,YNEBOR  |-->| COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NPTFR,NPLAN,NPOIN2,KLOG
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN
      DOUBLE PRECISION PSCAL
!
!***********************************************************************
!
C BOUNDARY CONDITIONS
!
C ENFORCES BC'S ON THE 3D FIELDS
!
      DO IPLAN=1,NPLAN
        DO IPTFR=1,NPTFR
          IF(LIHBOR(IPTFR).EQ.KLOG) THEN
            IPOIN=NBOR(IPTFR)
            PSCAL = U(IPOIN,IPLAN)*XNEBOR(IPTFR)
     &            + V(IPOIN,IPLAN)*YNEBOR(IPTFR)
            U(IPOIN,IPLAN) = U(IPOIN,IPLAN) - PSCAL*XNEBOR(IPTFR)
            V(IPOIN,IPLAN) = V(IPOIN,IPLAN) - PSCAL*YNEBOR(IPTFR)
          ENDIF
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C