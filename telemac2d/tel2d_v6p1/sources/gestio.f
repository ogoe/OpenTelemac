!                    *****************
                     SUBROUTINE GESTIO
!                    *****************
!
     &(U,V,C,T,AK,EP,UTILD,VTILD,CTILD,TTILD,AKTILD,EPTILD,
     & TRAC,PROPA,CONVV,ITURB,IETAPE)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MANAGES THE ALLOCATION OF ARRAYS
!+                DEPENDING ON THE SELECTED EQUATIONS.
!
!warning  DOES NOT WORK IF USES SCHEMES OTHER THAN
!+            CHARACTERISTICS FOR ADVECTION
!
!history  J-M HERVOUET (LNHE)
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
!| AK             |<->| ENERGIE TURBULENTE A LA FIN DE L'ETAPE TRAITEE
!| AKTILD         |-->| ENERGIE TURBULENTE AVANT L'ETAPE TRAITEE
!| C              |<->| CELERITE A LA FIN DE L'ETAPE TRAITEE.
!| CONVV          |-->| LOGIQUES INDIQUANT LES VARIABLES QU'ON NE
!|                |   | VEUT PAS CONVECTER.
!| CTILD          |-->| CELERITE AVANT L'ETAPE TRAITEE.
!| EP             |<->| DISSIPASSION A FIN DE L'ETAPE TRAITEE.
!| EPTILD         |-->| DISSIPASSION AVANT L'ETAPE TRAITEE.
!| IETAPE         |-->| INDICATEUR D'AVANCEMENT DANS LE PROGRAMME .
!| ITURB          |-->| MODELE DE TURBULENCE  1 : LAMINAIRE
!|                |   | 2 : LONGUEUR DE MELANGE
!|                |   | 3 : K-EPSILON
!| PROPA          |-->| SI PROPA=.FALSE. : PAS DE PROPAGATION.
!| T              |<->| TRACEUR A LA FIN DE L'ETAPE TRAITEE.
!| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!| TTILD          |-->| TRACEUR AVANT L'ETAPE TRAITEE.
!| U,V            |<->| VITESSE A LA FIN DE L'ETAPE TRAITEE.
!| UTILD,VTILD    |-->| VITESSE AVANT L'ETAPE TRAITEE.
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
      INTEGER, INTENT(IN)           :: ITURB,IETAPE
      LOGICAL, INTENT(IN)           :: TRAC,CONVV(4),PROPA
      TYPE(BIEF_OBJ), INTENT(IN)    :: T,AK,EP
      TYPE(BIEF_OBJ), INTENT(INOUT) :: U,V,C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: UTILD,VTILD,CTILD,TTILD
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AKTILD,EPTILD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!----------------------------------------------------------------------
!    ARRAYS DEPENDING ON THE SELECTED EQUATIONS
!-----------------------------------------------------------------------
!
!    ADVECTION
!
      IF(IETAPE.EQ.3) THEN
!
        IF(.NOT.CONVV(1)) THEN
          CALL OS( 'X=Y     ' , X=UTILD , Y=U )
          CALL OS( 'X=Y     ' , X=VTILD , Y=V )
        ENDIF
        IF(.NOT.CONVV(2)) THEN
          CALL OS( 'X=Y     ' , X=CTILD , Y=C )
        ENDIF
        IF(TRAC.AND.(.NOT.CONVV(3))) THEN
          CALL OS( 'X=Y     ' , X=TTILD , Y=T )
        ENDIF
        IF(ITURB.EQ.3.AND.(.NOT.CONVV(4))) THEN
          CALL OS( 'X=Y     ' , X=AKTILD , Y=AK )
          CALL OS( 'X=Y     ' , X=EPTILD , Y=EP )
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!    PROPAGATION
!
      IF(IETAPE.EQ.6) THEN
!
            IF(.NOT.PROPA) THEN
!
                   CALL OS( 'X=Y     ' , X=U , Y=UTILD )
                   CALL OS( 'X=Y     ' , X=V , Y=VTILD )
                   CALL OS( 'X=Y     ' , X=C , Y=CTILD )
!
            ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END