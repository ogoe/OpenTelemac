!                    ****************
                     SUBROUTINE MAJUS
!                    ****************
!
     &(CHAINE)
!
!***********************************************************************
! DAMOCLES   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CONVERTS A CHARACTER STRING FROM LOWER TO UPPER CASE.
!
!history  J-M HERVOUET (LNH)
!+        30/01/1992
!+
!+
!
!history  A. DESITTER (BRISTOL)
!+        30/01/1992
!+        V5P1
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
!| CHAINE         |<->| CHAINE DE CARACTERES A MODIFIER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      CHARACTER*26 STMAJ,STMIN
      CHARACTER*(*) CHAINE
!
      INTEGER I,IPOS
!
      INTRINSIC LEN,INDEX
!
      DATA STMAJ /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA STMIN /'abcdefghijklmnopqrstuvwxyz'/
!
!----------------------------------------------------------------------
!
      DO 10 I=1,LEN(CHAINE)
!
      IPOS=INDEX(STMIN,CHAINE(I:I))
      IF(IPOS.NE.0) CHAINE(I:I)=STMAJ(IPOS:IPOS)
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END