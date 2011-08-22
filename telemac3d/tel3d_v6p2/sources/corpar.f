!                    *****************
                     SUBROUTINE CORPAR
!                    *****************
!
     &  (FVERT,FHORI,PHILATI)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES VERTICAL AND HORIZONTAL CORIOLIS PARAMETERS.
!
!note     PHILAT IN *GRAD* IS POSITIVE FOR THE NORTHERN HEMISPHERE
!+         AND NEGATIVE FOR THE SOUTHERN HEMISPHERE.
!
!history  JACEK A. JANKOWSKI, UNIVERSITAET HANNOVER
!+        **/03/99
!+        V5P1
!+   FORTRAN95
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
!| FHORI          |<->| HORIZONTAL CORIOLIS PARAMETER
!| FVERT          |<->| VERTICAL CORIOLIS PARAMETER
!| PHILATI        |-->| LATITUDE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: FVERT,FHORI
      DOUBLE PRECISION, INTENT(IN)    :: PHILATI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,DAYASTR
      PARAMETER (PI=3.141592654D0, DAYASTR = 86164.091D0)
!
      FVERT = (4.0D0 * PI / DAYASTR) * SIN (PHILATI*PI/180.0D0)
      FHORI = (4.0D0 * PI / DAYASTR) * COS (PHILATI*PI/180.0D0)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
