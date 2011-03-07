!                    *****************
                     SUBROUTINE HPROPA
!                    *****************
!
     &(HPROP ,HN,H,PROLIN,HAULIN,TETA,NSOUSI)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES PROPAGATION DEPTH.
!
!history  J-M HERVOUET (LNHE)
!+        16/07/2007
!+        V5P8
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
!| H              |<--| HAUTEUR
!| HAULIN         |-->| PROFONDEUR MOYENNE POUR LA LINEARISATION
!| HN             |<--| HAUTEUR AU PAS DE TEMPS PRECEDENT
!| HPROP          |<--| HAUTEUR DE PROPAGATION
!| NSOUSI         |-->| NOMBRE DE SOUS ITERATIONS
!| PROLIN         |-->| CORRESPOND AU MOT CLE:"PROPAGATON LINEARISEE"
!| TETA           |-->| SEMI-IMPLICITATION SUR H.
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
      INTEGER, INTENT(IN)           :: NSOUSI
      LOGICAL, INTENT(IN)           :: PROLIN
      DOUBLE PRECISION, INTENT(IN)  :: TETA,HAULIN
      TYPE(BIEF_OBJ), INTENT(IN)    :: HN,H
      TYPE(BIEF_OBJ), INTENT(INOUT) :: HPROP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(PROLIN) THEN
        CALL OS( 'X=C     ' , X=HPROP , C=HAULIN    )
      ELSEIF(NSOUSI.EQ.1) THEN
        CALL OS( 'X=Y     ' , X=HPROP , Y=HN )
      ELSE
        CALL OS( 'X=CY    ' , X=HPROP , Y=HN , C=1.D0-TETA )
        CALL OS( 'X=X+CY  ' , X=HPROP , Y=H  , C= TETA )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     CLIPS HPROP
!
      IF(.NOT.PROLIN) THEN
        CALL OS('X=+(Y,C)',X=HPROP,Y=HPROP,C=0.D0)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END