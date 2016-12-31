!                    *****************
                     SUBROUTINE CORPOR
!                    *****************
!
     &(POROS)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE POROSITY OF ELEMENTS.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
!+
!+     NSOM = 4
!+     XSOM(1) = 8020.88D0
!+     XSOM(2) = 7761.81D0
!+     XSOM(3) = 8679.17D0
!+     XSOM(4) = 8988.75D0
!+     YSOM(1) =-1547.11D0
!+     YSOM(2) =-2415.26D0
!+     YSOM(3) =-2604.16D0
!+     YSOM(4) =-1543.75D0
!+
!+-----------------------------------------------------------------------
!+
!+     CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
!+
!+--------------------------------------------------------------
!+
!+     DO IELEM = 1 , NELEM
!+
!+       XX1 = (  X(IKLE%I(IELEM)          )+
!+    *           X(IKLE%I(IELEM+NELMAX)   )+
!+    *           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!+       YY1 = (  Y(IKLE%I(IELEM)          )+
!+    *           Y(IKLE%I(IELEM+NELMAX)   )+
!+    *           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!+
!+       IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
!+         POROS%R(IELEM) = 0.5D0
!+       ENDIF
!+
!+      ENDDO ! IELEM
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
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
!| POROS          |<->| POROSITY TO BE MODIFIED.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1
!     INTEGER NSOM,IELEM
!
!-----------------------------------------------------------------------
!
!     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
!
!     NSOM = 4
!     XSOM(1) = 8020.88D0
!     XSOM(2) = 7761.81D0
!     XSOM(3) = 8679.17D0
!     XSOM(4) = 8988.75D0
!     YSOM(1) =-1547.11D0
!     YSOM(2) =-2415.26D0
!     YSOM(3) =-2604.16D0
!     YSOM(4) =-1543.75D0
!
!-----------------------------------------------------------------------
!
!     CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
!
!--------------------------------------------------------------
!
!     DO IELEM = 1 , NELEM
!
!       XX1 = (  X(IKLE%I(IELEM)          )+
!    *           X(IKLE%I(IELEM+NELMAX)   )+
!    *           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!       YY1 = (  Y(IKLE%I(IELEM)          )+
!    *           Y(IKLE%I(IELEM+NELMAX)   )+
!    *           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!
!       IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
!         POROS%R(IELEM) = 0.5D0
!       ENDIF
!
!      ENDDO ! IELEM
!
!-----------------------------------------------------------------------
!
      RETURN
      END
