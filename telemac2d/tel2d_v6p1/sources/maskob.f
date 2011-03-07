!                    *****************
                     SUBROUTINE MASKOB
!                    *****************
!
     &(MASKEL,X,Y,IKLE,NELEM,NELMAX,NPOIN,AT,LT)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    FORMALLY REMOVES ELEMENTS FROM THE MESH,
!+                USING THE MASKING ARRAY MASKEL:
!+
!+            MASKEL = 0.D0 FOR MASKED ELEMENTS
!+
!+            MASKEL = 1.D0 FOR'NORMAL' ELEMENTS
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  J-M JANIN (LNH)
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
!| AT             |---| 
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS
!| LT             |---| 
!| MASKEL         |<->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| NELEM          |-->| NOMBRE D'ELEMENTS.
!| NELMAX         |---| 
!| NPOIN          |-->| NOMBRE DE POINTS
!| X,Y            |-->| COORDONNEES DES POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NELMAX,LT
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM)
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END