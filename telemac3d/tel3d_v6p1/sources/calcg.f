!                    ****************
                     SUBROUTINE CALCG
!                    ****************
!
     & (TRAV2,TRAV3,U,V,UETCAR,NPOIN2,NPLAN)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    CALLED WHEN MODELLING THE INFLUENCE OF
!+                TURBULENCE ON THE SETTLING VELOCITY.
!
!history  C LE NORMANT
!+        01/08/97
!+        V5P4
!+   
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        
!+   FORTRAN95 VERSION 
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
!| NPLAN          |-->| NOMBRE DE PLANS
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| TRAV2,TRAV3    |<--| TABLEAUX DE TRAVAIL    NOTE JMH ?????
!| U,V            |-->| COMPOSANTES DE LA VITESSE
!| UETCAR         |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2),V(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TRAV2(NPOIN2*NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRAV3(NPOIN2*NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN
      DOUBLE PRECISION UQUA
!
!=======================================================================
!
! COMPUTES AUBORF IN TRAV1
!
      DO IPOIN2 = 1,NPOIN2
!
         UQUA = SQRT( (U(IPOIN2))**2 + (V(IPOIN2))**2 )
!
!                              AUBORF * U_B * DU/DZ
!        COMPUTES  G  =  SQRT( --------------------- ) : TRAV3
!                                     NU
!
        DO IPLAN = 1,NPLAN
         TRAV3(IPOIN2+(IPLAN-1)*NPOIN2) =
     &        SQRT(UETCAR(IPOIN2) * UQUA * 1.D06
     &        * TRAV2(IPOIN2+(IPLAN-1)*NPOIN2))
!
        ENDDO
!
      ENDDO
!
!=======================================================================
!
      RETURN
      END