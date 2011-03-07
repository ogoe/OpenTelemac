!                    *****************
                     SUBROUTINE KEPSIN
!                    *****************
!
     &(LIMKEP,LIUBOR,NPTFR,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,KNEU,KDIR)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR THE DIFFUSION STEP
!+                - SOURCE TERMS OF THE K-EPSILON MODEL.
!
!warning  LIMKEP IS BUILT FROM LIUBOR (LIKBOR AND LIEBOR DO NOT EXIST)
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V5P2
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
!| KADH           |-->| INDICATEUR DE POINT DIRICHLET.
!| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
!| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE .
!| KENTU          |-->| INDICATEUR DE VITESSES IMPOSEES.
!| KINC           |-->| INDICATEUR POUR L'ONDE INCIDENTE.
!| KLOG           |-->| INDICATEUR DE PAROI SOLIDE .
!| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
!| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE .
!| LIMKEP         |---| 
!| LIUBOR         |-->| TYPES DE CONDITIONS AUX LIMITES SUR U.
!| NPTFR          |-->| DIMENSION DES TABLEAUX .
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NPTFR,KENT,KSORT,KADH,KLOG
      INTEGER, INTENT(IN)    :: KINC,KNEU,KDIR,KENTU
      INTEGER, INTENT(INOUT) :: LIMKEP(NPTFR,2)
      INTEGER, INTENT(IN)    :: LIUBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
!-----------------------------------------------------------------------
!
!  BUILDS THE ARRAY LIMKEP
!
      DO 1 K=1,NPTFR
!
        IF(LIUBOR(K).EQ.KENT) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSEIF(LIUBOR(K).EQ.KENTU) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSEIF(LIUBOR(K).EQ.KADH) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSEIF(LIUBOR(K).EQ.KSORT) THEN
!
          LIMKEP(K,1) = KNEU
          LIMKEP(K,2) = KNEU
!
        ELSEIF(LIUBOR(K).EQ.KINC) THEN
!
          LIMKEP(K,1) = KNEU
          LIMKEP(K,2) = KNEU
!
        ELSEIF(LIUBOR(K).EQ.KLOG ) THEN
!
          LIMKEP(K,1) = KDIR
          LIMKEP(K,2) = KDIR
!
        ELSE
!
          IF(LNG.EQ.1) WRITE(LU,100) K,LIUBOR(K)
          IF(LNG.EQ.2) WRITE(LU,101) K,LIUBOR(K)
100       FORMAT(1X,'KEPSIN: K=',1I6,' LIUBOR=',1I6,' CAS NON PREVU')
101       FORMAT(1X,'KEPSIN: K=',1I6,' LIUBOR=',1I6,' UNKNOWN CASE')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
1     CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END