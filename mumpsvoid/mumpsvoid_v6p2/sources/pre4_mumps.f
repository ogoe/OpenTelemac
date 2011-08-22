!                    *********************
                     SUBROUTINE PRE4_MUMPS
!                    *********************
!
     &(NPOIN,NSEGB,GLOSEGB,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & XX1,XX2,CVB1,CVB2,INFOGR,TYPEXT)
!
!***********************************************************************
! MUMPSVOID   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CALLS THE DIRECT SOLVER MUMPS.
!+                IF MUMPS IS NOT INSTALLED : EMPTY SUBROUTINES ARE USED INSTEAD.
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/2006
!+
!+
!
!history  C. DENIS (SINETICS)
!+        14/10/2009
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
!| CVB1,CVB2      |-->| SECONDS MEMBRES
!| DAB1           |---|
!| DAB2           |---|
!| DAB3           |---|
!| DAB4           |---|
!| GLOSEGB        |---|
!| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
!| NPOIN          |-->| NOMBRE D'INCONNUES
!| NSEGB          |-->| NOMBRE DE SEGMENTS
!| TYPEXT         |---|
!| XAB1           |---|
!| XAB2           |---|
!| XAB3           |---|
!| XAB4           |---|
!| XX1,XX2        |<--| SOLUTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NSEGB
      INTEGER, INTENT(IN) :: GLOSEGB(NSEGB*2)
      LOGICAL, INTENT(IN) :: INFOGR
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(NSEGB),XAB2(NSEGB)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(NSEGB),XAB4(NSEGB)
      DOUBLE PRECISION, INTENT(INOUT) :: XX1(NPOIN),XX2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CVB1(NPOIN),CVB2(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
!
      IF(LNG.EQ.1) WRITE(LU,2018)
      IF(LNG.EQ.2) WRITE(LU,2019)
2018  FORMAT(1X,'MUMPS NON INSTALLE SUR CE SYSTEME,',/,1X,
     &     'CHOISIR UNE AUTRE METHODE',///)
2019  FORMAT(1X,'MUMPS NOT INSTALLED IN THIS SYSTEM',/,1X,
     &     'CHOOSE OTHER METHOD ',///)
      CALL PLANTE(1)
      STOP
!
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END