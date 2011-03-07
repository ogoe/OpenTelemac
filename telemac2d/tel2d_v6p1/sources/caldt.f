!                    ****************
                     SUBROUTINE CALDT
!                    ****************
!
     &(NS,G,H,U,V,DTHAUT,DT,CFL,ICIN,DTVARI,LISTIN)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TIMESTEP SATISFYING THE CFL CONDITION.
!
!history  ??????
!+        22/10/2001
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
!| CFL            |---|
!| DT             |---|
!| DTHAUT         |---|
!| G              |---|
!| H              |---|
!| NS             |---|
!| U              |---|
!| V              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,ICIN
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN) :: H(NS),U(NS),V(NS),DTHAUT(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,CFL
      LOGICAL, INTENT(IN) :: DTVARI,LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
!
      DOUBLE PRECISION RA3,EPSL
      DOUBLE PRECISION SIGMAX ,UA2, UA3, UNORM
!
!-----------------------------------------------------------------------
!
      IF(ICIN.EQ.1.OR.ICIN.EQ.2)THEN
!   KINETIC SCHEMES
        RA3 = SQRT(1.5D0*G)
!
        DT = 1.E+12
        EPSL = 0.01D0
!
        DO IS=1,NS
           SIGMAX = H(IS)
           UA2    = U(IS)
           UA3    = V(IS)
           UNORM=SQRT(UA2*UA2 + UA3*UA3)
           SIGMAX= MAX(EPSL, RA3*SQRT(SIGMAX) +UNORM )
           DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
        ENDDO
      ELSEIF((ICIN.EQ.0).OR.(ICIN.EQ.3).OR.(ICIN.EQ.4))THEN
! ROE, ZOKAGOA AND TCHAMEN SCHEME
           DT = 1.E+12
           EPSL = 0.01D0
!
           DO IS=1,NS
              SIGMAX = G*H(IS)
              UA2    = U(IS)
              UA3    = V(IS)
              UNORM=SQRT(UA2*UA2 + UA3*UA3)
              SIGMAX= MAX(EPSL, SQRT(SIGMAX) +UNORM )
!             DTHAUT=|Ci|/Sum(Lij) INRIA'S CHOICE
!             IT COULD BE CHANGED INTO MIN (CMI) FOR EXAMPLE
              DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
          ENDDO
      ENDIF
!
!*************************************************************************
!
! ERROR TREATMENT AND LISTING OUTPUTS
!
!*************************************************************************
!
      IF(DTVARI.AND.CFL.LT.1.0D0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,*) 'PAS DE TEMPS : ',DT
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,*) 'TIME-STEP: ',DT
      ELSEIF((DTVARI).AND.(CFL.GE.1.0D0)) THEN
        DT=0.5D0*DT/CFL
        IF(LISTIN.AND.LNG.EQ.1) THEN
            WRITE(LU,*) 'ATTENTION CFL DEMANDÉ > 1 !...!'
            WRITE(LU,*) 'PAS DE TEMPS (AVEC CFL = 0.5) : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN
            WRITE(LU,*) 'WARINING: WANTED CFL >1 !...! '
            WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.5): ',DT
        ENDIF
      ELSEIF(.NOT.DTVARI)THEN
!       INCLUDES THES CASES: NOT DTVARI, ALL CFL (> AND < 1)
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'ATTENTION CFL NON FOURNI !...!'
          WRITE(LU,*) 'PAS DE TEMPS (AVEC CFL = 0.5) : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'WARINING: PLEASE GIVE CFL  !...! '
          WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.5): ',DT
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END