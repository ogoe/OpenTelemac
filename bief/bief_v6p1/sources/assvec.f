!                    *****************
                     SUBROUTINE ASSVEC
!                    *****************
!
     &(X, IKLE,NPOIN,NELEM,NELMAX,IELM,W,INIT,LV,MSK,MASKEL,NDP)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    VECTOR ASSEMBLY.
!
!warning  THIS VECTOR IS ONLY INITIALISED TO 0 IF INIT = .TRUE.
!
!history  J-M HERVOUET (LNH)
!+        29/02/08
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
!| IELM           |-->| TYPE D'ELEMENT (VOIR CI-DESSUS)
!| IKLE           |-->| CORRESPONDANCES NUMEROTATION LOCALE-GLOBALE
!| INIT           |-->| LOGIQUE : SI VRAI : X EST INITIALISE A 0
!| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
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
      USE BIEF, EX_ASSVEC => ASSVEC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      INTEGER         , INTENT(IN)    :: NELEM,NELMAX,NPOIN,IELM,LV,NDP
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: W(NELMAX,NDP),MASKEL(NELMAX)
      LOGICAL         , INTENT(IN)    :: INIT,MSK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IDP
!
!-----------------------------------------------------------------------
!   INITIALISES VECTOR X TO 0 IF(INIT)
!-----------------------------------------------------------------------
!
      IF(INIT) CALL OV( 'X=C     ' , X , X , X , 0.D0 , NPOIN )
!
!-----------------------------------------------------------------------
!   ASSEMBLES, CONTRIBUTION OF LOCAL POINTS 1,... TO NDP
!-----------------------------------------------------------------------
!
      DO IDP = 1 , NDP
!
        CALL ASSVE1(X,IKLE(1,IDP),W(1,IDP),NELEM,NELMAX,LV,MSK,MASKEL)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END