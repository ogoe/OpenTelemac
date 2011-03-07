!                    *****************
                     SUBROUTINE ASMVEC
!                    *****************
!
     &(X, IKLE,NPOIN,NELEM,NELMAX,NDP,W,INIT,LV)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MULTIPLICATIVE ASSEMBLY FOR A VECTOR.
!
!warning  THIS VECTOR IS INITIALISED TO 1 IF INIT = .TRUE.
!
!history  J-M HERVOUET (LNH)    ; F  LEPEINTRE (LNH)
!+        17/08/94
!+        V5P1
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
!| IKLE           |-->| CORRESPONDANCES NUMEROTATION LOCALE-GLOBALE
!| INIT           |-->| LOGIQUE : SI VRAI : X EST INITIALISE A 0
!| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
!| NDP            |-->| DEUXIEME DIMENSION DE IKLE.
!| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
!|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
!| NPOIN          |-->| DIMENSION DU TABLEAU X
!| W              |-->| TABLEAUX DE TRAVAIL CONTENANT LE VECTEUR SOUS
!|                |   | FORME NON ASSEMBLEE
!|                |   | W EST DE DIMENSION NELMAX * NDP(IELM)
!|                |   | NDP EST LE NOMBRE DE POINTS DE L'ELEMENT
!| X              |<->| VECTEUR ASSEMBLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_ASMVEC => ASMVEC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NELMAX,NPOIN,NELEM,NDP,LV
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX,NDP)
      LOGICAL         , INTENT(IN)    :: INIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDP
!
!-----------------------------------------------------------------------
!   INITIALISES VECTOR X TO 1 IF(INIT)
!-----------------------------------------------------------------------
!
      IF(INIT) CALL OV( 'X=C     ' , X , X , X , 1.D0 , NPOIN )
!
!-----------------------------------------------------------------------
!   ASSEMBLES, CONTRIBUTION OF LOCAL POINTS 1,... TO NDP
!-----------------------------------------------------------------------
!
      DO IDP = 1 , NDP
!
        CALL ASMVE1(X, IKLE(1,IDP),W(1,IDP),NPOIN,NELEM,NELMAX,LV)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END