!                    ****************
                     SUBROUTINE SOR3D
!                    ****************
!
     &(F,NPLAN,NF,TETA,FREQ,NELEM2,NPOIN2,AT,U,V,UV,VV,DEPTH,VENT,
     & COURAN,MAREE,TITRE,NR3D,BINR3D)
!
!***********************************************************************
! TOMAWAC   V6P1                                   28/06/2011
!***********************************************************************
!
!brief    WRITES DATA NECESSARY TO RESUME COMPUTATION
!+                AT A LATER DATE.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        28/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| BINR3D         |-->| GLOBAL RESULT FILE BINARY
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| MAREE          |-->| LOGICAL INDICATING CONSIDERATION OF TIDE
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NR3D           |-->| LOGICAL UNIT NUMBER OF GLOBAL RESULT FILE
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TITRE          |-->| TITLE
!| U              |-->| CURRENT SPEED ALONG X
!| UV             |-->| WIND SPEED ALONG X
!| V              |-->| CURRENT SPEED ALONG Y
!| VV             |-->| WIND SPEED ALONG Y
!| VENT           |-->| INDICATES IF WIND IS TAKEN INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NR3D,NF,NPLAN,NELEM2,NPOIN2
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),AT, ATT(1)
      DOUBLE PRECISION FREQ(NF),TETA(NPLAN)
      DOUBLE PRECISION U(NPOIN2),V(NPOIN2),UV(NPOIN2),VV(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2)
!
      INTEGER ISTAT,IB(2),NTOT
!
      LOGICAL COURAN,VENT,MAREE
!
      CHARACTER*3 BINR3D,CAR
      CHARACTER*80 TITRE
!
!***********************************************************************
!
!
! WRITES TITLE
!
      CALL ECRI2(F,IB,TITRE,80,'CH',NR3D,BINR3D,ISTAT)
!
! WRITES NPLAN, NF
!
      IB(1)=NPLAN
      IB(2)=NF
      CALL ECRI2(F,IB,CAR,2,'I ',NR3D,BINR3D,ISTAT)
!
! WRITES NELEM2, NPOIN2
!
      IB(1)=NELEM2
      IB(2)=NPOIN2
      CALL ECRI2(F,IB,CAR,2,'I ',NR3D,BINR3D,ISTAT)
!
! WRITES TIME
!
      ATT(1)=AT
      CALL ECRI2(ATT,IB,CAR,1,'R4',NR3D,BINR3D,ISTAT)
!
! WRITES TETA
!
      CALL ECRI2(TETA,IB,CAR,NPLAN,'R4',NR3D,BINR3D,ISTAT)
!
! WRITES FREQ
!
      CALL ECRI2(FREQ,IB,CAR,NF,'R4',NR3D,BINR3D,ISTAT)
!
! WRITES F
!
      NTOT=NPOIN2*NPLAN*NF
      CALL ECRI2(F,IB,CAR,NTOT,'R4',NR3D,BINR3D,ISTAT)
!
! WRITES U,V,UV,VV (IF HAS TO)
!
      IF (COURAN) THEN
      CALL ECRI2(U ,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      CALL ECRI2(V ,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      ENDIF
      IF (VENT) THEN
      CALL ECRI2(UV,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      CALL ECRI2(VV,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      ENDIF
      IF (MAREE) THEN
      CALL ECRI2(DEPTH,IB,CAR,NPOIN2,'R4',NR3D,BINR3D,ISTAT)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
