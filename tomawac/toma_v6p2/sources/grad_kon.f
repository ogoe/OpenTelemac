!                    ********************
                     SUBROUTINE GRAD_KON
!                    ********************
!
!***********************************************************************
! TOMAWAC   V6P2                                   25/06/2012
!***********************************************************************
!
!brief    CALCULATES THE GRADIENT OF PARAMETER 1/K^2 FOR DIFFRACTION
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
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
      DOUBLE PRECISION C
!       
!.....CALCULATES THE GRADIENT OF 1/K^2 FOR DIFFRACTION
!     """""""""""""""""""""""""""""""""""""""""""""""
!     
!.....DERIVEES EN X
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SXKONPT,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
!
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDX%R,ST1%R,ST4%R,C,NPOIN2)
!
!.....DERIVEES EN Y
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SXKONPT,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
!
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     * ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL OV('X=Y/Z   ',SDDY%R,ST1%R,ST4%R,C,NPOIN2)
!
!-----------------------------------------------------------------------
      RETURN
      END
