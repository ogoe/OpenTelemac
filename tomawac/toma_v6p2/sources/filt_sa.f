!                       ****************** 
                        SUBROUTINE FILT_SA 
!                       ****************** 
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!brief    NUMERICAL FILTER TO SMOOTH THE WAVE AMPLITUDES OF 
!+            DIRECTIONAL SPECTRA 
! 
!history  E. KRIEZI (LNH) 
!+        04/12/2006 
!+        V5P5 
!+ 
! 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/06/2012 
!+        V6P2 
!+   Modification for V6P2 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
! 
      USE BIEF 
      USE DECLARATIONS_TELEMAC 
      USE DECLARATIONS_TOMAWAC 
! 
      IMPLICIT NONE 
      DOUBLE PRECISION C 
      LISFON =1 
 
      CALL OV('X=C     ', SW1%R, ST1%R, ST2%R, 1.D0, NPOIN2) 
      CALL FILTER(SA,.TRUE.,ST1,ST2,AM1,'MATMAS          ', 
     *          1.D0,ST1,ST1,ST1,ST1,ST1,ST1,MESH,.FALSE.,SW1,LISFON) 
       
! 
!----------------------------------------------------------------------- 
      RETURN 
      END 
