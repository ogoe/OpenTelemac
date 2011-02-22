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
!>    </th><td> KADH, KDEB, KENT, KLOG, KP1BOR, LIHBOR, LIUBOF, LIUBOL, LIUBOS, LIVBOF, LIVBOL, LIVBOS, LIWBOF, LIWBOL, LIWBOS, NBOR, NPLAN, NPOIN2, NPTFR, U, UBORF, UBORL, UBORS, V, VBORF, VBORL, VBORS, VELPROLAT, W, WBORF, WBORL, WBORS, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN, IPOIN, IPOIN2, IPTF2, IPTFR, PSCAL
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <tr>
!>      <td><center> 1.2                                       </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
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
!>          <tr><td>KDEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>CONVENTION POUR LES VALEURS IMPOSEES (ENT.)
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>CONVENTION POUR LES PAROIS AVEC GLISSEMENT
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
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
!>          <tr><td>LIWBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIWBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIWBOS
!></td><td>---</td><td>
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
!>          <tr><td>VELPROLAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WBORL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AIRWIK2
     &(LIHBOR,UBORF,VBORF,WBORF,LIUBOF,LIVBOF,LIWBOF,UBORL,VBORL,WBORL,
     & LIUBOL,LIVBOL,LIWBOL,
     & UBORS,VBORS,WBORS,LIUBOS,LIVBOS,LIWBOS,U,V,W,XNEBOR,YNEBOR,NBOR,
     & NPTFR,NPLAN,NPOIN2,KENT,KADH,KLOG,KDEB,KP1BOR,VELPROLAT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KADH           |-->| CONVENTION POUR LES PAROIS AVEC ADHERENCE
C| KDEB           |---| 
C| KENT           |-->| CONVENTION POUR LES VALEURS IMPOSEES (ENT.)
C| KLOG           |-->| CONVENTION POUR LES PAROIS AVEC GLISSEMENT
C| KP1BOR         |---| 
C| KSORT          |-->| CONVENTION POUR LES VALEURS LIBRES (SORT.)
C| LIHBOR         |---| 
C| LIUBOF,LIVBOF  |-->| TYPES DE C.L. SUR U ET V AU FOND
C| LIUBOL,LIVBOL  |-->| TYPES DE C.L. SUR U ET V SUR LES PAROIS LAT.
C| LIUBOS,LIVBOS  |-->| TYPES DE C.L. SUR U ET V EN SURFACE
C| LIWBOF         |---| 
C| LIWBOL         |---| 
C| LIWBOS         |---| 
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
C| VELPROLAT      |---| 
C| W             |---| 
C| WBORF          |---| 
C| WBORL          |---| 
C| WBORS          |---| 
C| XNEBOR,YNEBOR  |-->| COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN) :: KENT, KADH, KLOG, KDEB
!
      INTEGER, INTENT(IN) :: KP1BOR(NPTFR,2)
      INTEGER, INTENT(IN) :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(IN) :: LIUBOL(NPTFR,NPLAN),LIWBOF(NPOIN2)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIWBOS(NPOIN2)
      INTEGER, INTENT(IN) :: LIVBOF(NPOIN2), LIVBOS(NPOIN2)
      INTEGER, INTENT(IN) :: LIVBOL(NPTFR,NPLAN),LIWBOL(NPTFR,NPLAN)
      INTEGER, INTENT(IN) :: NBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN)    :: UBORF(NPOIN2),UBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: WBORF(NPOIN2),WBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: UBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: WBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: VBORF(NPOIN2), VBORS(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: VBORL(NPTFR,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: W(NPOIN2,NPLAN)
!
      LOGICAL, INTENT(IN)             :: VELPROLAT
!
!-----------------------------------------------------------------------
!
      INTEGER IPTFR,IPLAN,IPOIN,IPTF2,IPOIN2
      DOUBLE PRECISION PSCAL
!
!-----------------------------------------------------------------------
!
C ENFORCES U.N = 0
!
      IF(VELPROLAT) THEN
        DO IPLAN=1,NPLAN
          DO IPTFR=1,NPTFR
            IF(LIHBOR(IPTFR).EQ.KLOG) THEN
              IPOIN=NBOR(IPTFR)
              PSCAL = U(IPOIN,IPLAN)*XNEBOR(IPTFR)
     &              + V(IPOIN,IPLAN)*YNEBOR(IPTFR)
              U(IPOIN,IPLAN) = U(IPOIN,IPLAN) - PSCAL*XNEBOR(IPTFR)
              V(IPOIN,IPLAN) = V(IPOIN,IPLAN) - PSCAL*YNEBOR(IPTFR)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
C DIRICHLET AND NO SLIP CONDITION
!
      DO IPLAN=1,NPLAN
        DO IPTFR=1,NPTFR
           IF (LIUBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIUBOL(IPTFR,IPLAN).EQ.KDEB)
     &          U(NBOR(IPTFR),IPLAN) = UBORL(IPTFR,IPLAN)
           IF (LIVBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIVBOL(IPTFR,IPLAN).EQ.KDEB)
     &          V(NBOR(IPTFR),IPLAN) = VBORL(IPTFR,IPLAN)
           IF (LIWBOL(IPTFR,IPLAN).EQ.KENT.OR.
     &         LIWBOL(IPTFR,IPLAN).EQ.KDEB)
     &          W(NBOR(IPTFR),IPLAN) = WBORL(IPTFR,IPLAN)
           IF(LIUBOL(IPTFR,IPLAN).EQ.KADH) U(NBOR(IPTFR),IPLAN) = 0.D0
           IF(LIVBOL(IPTFR,IPLAN).EQ.KADH) V(NBOR(IPTFR),IPLAN) = 0.D0
           IF(LIWBOL(IPTFR,IPLAN).EQ.KADH) W(NBOR(IPTFR),IPLAN) = 0.D0
        ENDDO
      ENDDO
!
C BOTTOM AND FREE SURFACE
!
      DO IPOIN=1,NPOIN2
         IF (LIUBOF(IPOIN).EQ.KENT.OR.LIUBOF(IPOIN).EQ.KADH)
     &       U(IPOIN,1) = UBORF(IPOIN)
         IF (LIVBOF(IPOIN).EQ.KENT.OR.LIVBOF(IPOIN).EQ.KADH)
     &       V(IPOIN,1) = VBORF(IPOIN)
         IF (LIWBOF(IPOIN).EQ.KENT.OR.LIWBOF(IPOIN).EQ.KADH)
     &       W(IPOIN,1) = WBORF(IPOIN)
         IF (LIUBOS(IPOIN).EQ.KENT.OR.LIUBOS(IPOIN).EQ.KADH)
     &       U(IPOIN,NPLAN)=UBORS(IPOIN)
         IF (LIVBOS(IPOIN).EQ.KENT.OR.LIVBOS(IPOIN).EQ.KADH)
     &       V(IPOIN,NPLAN)=VBORS(IPOIN)
         IF (LIWBOS(IPOIN).EQ.KENT.OR.LIWBOS(IPOIN).EQ.KADH)
     &       W(IPOIN,NPLAN)=WBORS(IPOIN)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C