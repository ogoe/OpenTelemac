!                    *****************
                     SUBROUTINE KEPICL
!                    *****************
!
     & (LIKBOF,LIEBOF,LIUBOF,LIKBOL,LIEBOL,LIUBOL,LIKBOS,LIEBOS,LIUBOS,
     &  NPTFR,NPLAN,NPOIN2,KENT,KSORT,KADH,KLOG)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION
!+                SOURCE TERM STEP OF THE K-EPSILON MODEL.
!+
!+            CASE.
!
!warning  LIKBOR AND LIEBOR ARE BUILT FROM LIUBOR
!
!history  L. VAN HAREN (LNH)
!+        25/11/97
!+        V6P0
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!| KADH           |-->| CONVENTION POUR UNE PAROI AVEC ADHERENCE
!| KENT           |-->| CONVENTION POUR UN POINT A VALEUR IMPOSEE
!| KLOG           |-->| CONVENTION POUR UNE PAROI LOGARITHMIQUE
!| KSORT          |-->| CONVENTION POUR UN POINT A VALEUR LIBRE
!| LIEBOF         |---|
!| LIEBOL         |---|
!| LIEBOS         |---|
!| LIKBOF         |---|
!| LIKBOL         |---|
!| LIKBOS         |---|
!| LIUBOF         |---|
!| LIUBOL         |---|
!| LIUBOS         |---|
!| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES DU MAILLAGE 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR, NPLAN, NPOIN2
      INTEGER, INTENT(IN)    :: KENT, KSORT, KADH, KLOG
      INTEGER, INTENT(IN)    :: LIUBOF(NPOIN2), LIUBOS(NPOIN2)
      INTEGER, INTENT(IN)    :: LIUBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIKBOF(NPOIN2), LIKBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIKBOL(NPTFR*NPLAN*2)
      INTEGER, INTENT(INOUT) :: LIEBOF(NPOIN2), LIEBOS(NPOIN2)
      INTEGER, INTENT(INOUT) :: LIEBOL(NPTFR*NPLAN*2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR, IPLAN, IPOIN2,NPTFR3
!
!-----------------------------------------------------------------------
!
!     LATERAL BOUNDARIES :
!
!-----------------------------------------------------------------------
!
      NPTFR3=NPLAN*NPTFR
!
      DO IPTFR=1,NPTFR3
        IF(LIUBOL(IPTFR).EQ.KENT) THEN
          LIKBOL(IPTFR) = KENT
          LIEBOL(IPTFR) = KENT
        ELSE
          LIKBOL(IPTFR) = KSORT
          LIEBOL(IPTFR) = KSORT
        ENDIF
!       SAVING VALUES IN THE SECOND DIMENSION (SEE POINT_TELEMAC3D)
        LIKBOL(IPTFR+NPTFR3) = KSORT
        LIEBOL(IPTFR+NPTFR3) = KSORT
      ENDDO
!
!-----------------------------------------------------------------------
!
!     BOTTOM
!
!-----------------------------------------------------------------------
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF(IPOIN2).EQ.KSORT) THEN
          LIKBOF(IPOIN2) = KSORT
          LIEBOF(IPOIN2) = KSORT
        ELSE
          LIKBOF(IPOIN2) = KENT
          LIEBOF(IPOIN2) = KENT
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     FREE SURFACE
!
!-----------------------------------------------------------------------
!
      DO IPOIN2 = 1,NPOIN2
        LIKBOS(IPOIN2) = KSORT
        LIEBOS(IPOIN2) = KSORT
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END