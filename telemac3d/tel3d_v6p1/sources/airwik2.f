!                    ******************
                     SUBROUTINE AIRWIK2
!                    ******************
!
     &(LIHBOR,UBORF,VBORF,WBORF,LIUBOF,LIVBOF,LIWBOF,UBORL,VBORL,WBORL,
     & LIUBOL,LIVBOL,LIWBOL,
     & UBORS,VBORS,WBORS,LIUBOS,LIVBOS,LIWBOS,U,V,W,XNEBOR,YNEBOR,NBOR,
     & NPTFR,NPLAN,NPOIN2,KENT,KADH,KLOG,KDEB,KP1BOR,VELPROLAT)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ENSURES THE CONDITION U . N = 0  (U AND N ARE VECTORS).
!+
!+           (FOR A LATERAL SOLID BOUNDARY, DUPLICATES THE NORMAL
!+                COMPONENT OF THE VELOCITY, COMPUTED BY TELEMAC, ON THE VERTICAL).
!+
!+            ALSO ENSURES THE DIRICHLET CONDITIONS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        V1P2
!+   FORTRAN95 VERSION 
!
!history  JM HERVOUET (LNHE)
!+        29/02/08
!+        V5P9
!+   
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into 
!+   English comments 
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and 
!+   cross-referencing of the FORTRAN sources 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| KADH           |-->| CONVENTION POUR LES PAROIS AVEC ADHERENCE
!| KDEB           |---| 
!| KENT           |-->| CONVENTION POUR LES VALEURS IMPOSEES (ENT.)
!| KLOG           |-->| CONVENTION POUR LES PAROIS AVEC GLISSEMENT
!| KP1BOR         |---| 
!| LIHBOR         |---| 
!| LIUBOF,LIVBOF  |-->| TYPES DE C.L. SUR U ET V AU FOND
!| LIUBOL,LIVBOL  |-->| TYPES DE C.L. SUR U ET V SUR LES PAROIS LAT.
!| LIUBOS,LIVBOS  |-->| TYPES DE C.L. SUR U ET V EN SURFACE
!| LIWBOF         |---| 
!| LIWBOL         |---| 
!| LIWBOS         |---| 
!| NBOR           |-->| CORRESPONDANCE NUMEROTATION FRONTIERE ET
!|                |   | NUMEROTATION GLOBALE EN 2D
!| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES 2D
!| UBORF,VBORF    |-->| VITESSES U ET V IMPOSEES AU FOND
!| UBORL,VBORL    |-->| VITESSES U ET V IMPOSEES SUR LES PAROIS LAT.
!| UBORS,VBORS    |-->| VITESSES U ET V IMPOSEES EN SURFACE
!| VELPROLAT      |---| 
!| W              |---| 
!| WBORF          |---| 
!| WBORL          |---| 
!| WBORS          |---| 
!| XNEBOR,YNEBOR  |-->| COMPOSANTES VECTEUR NORMAL POINTS FRONTIERES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
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
! ENFORCES U.N = 0
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
! DIRICHLET AND NO SLIP CONDITION
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
! BOTTOM AND FREE SURFACE
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