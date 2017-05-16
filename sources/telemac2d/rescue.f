!                    *****************
                     SUBROUTINE RESCUE
!                    *****************
!
     &(U,V,H,S,ZF,T,TRAC0,NTRAC,ITURB,NPOIN,AKEP,TROUVE)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES MISSING DATA/VARIABLES (WHEN RESUMING SIMULATION).
!
!history  J-M HERVOUET (LNH)
!+        31/08/2007
!+        V5P8
!+   First version.
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
!history  D WANG & P TASSI (LNHE)
!+        10/07/2014
!+        V7P0
!+   Tracers ranks shifted due to secondary currents variables.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/07/2015
!+        V5P8
!+   English message corrected.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AKEP           |-->| IF YES, K AND EPSILON TO BE INITIALISED
!| H              |<--| WATER DEPTH
!| ITURB          |-->| TURBULENCE MODEL
!| NPOIN          |-->| NUMBER OF POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| S              |<--| FREE SURFACE
!| T              |<--| BLOCK OF TRACERS
!| TRAC0          |-->| INITIAL VALUES OF TRACERS
!| TROUVE         |-->| INTEGER ARRAY SAYING IF VARIABLES HAVE BEEN FOUND
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: TROUVE(*),ITURB,NPOIN,NTRAC
      LOGICAL, INTENT(INOUT)          :: AKEP
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TRAC0(NTRAC)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!  VELOCITY U-COMPONENT
!
      IF(TROUVE(1).NE.1 )  THEN
        IF(LNG.EQ.1) WRITE(LU,190)
        IF(LNG.EQ.2) WRITE(LU,191)
190     FORMAT(1X,'RESCUE : FICHIER DE RESULTATS DU CALCUL PRECEDENT',
     &         /,1X,'         SANS LA VITESSE U, ON LA PREND NULLE')
191     FORMAT(1X,'RESCUE : PREVIOUS COMPUTATION RESULTS FILE',
     &         /,1X,'         WITHOUT VELOCITY U, WE FIX IT TO ZERO')
        CALL OV( 'X=C     ' , U , U , U , 0.D0 , NPOIN )
      ENDIF
!
!-----------------------------------------------------------------------
!
!  VELOCITY V-COMPONENT
!
      IF(TROUVE(2).NE.1 )  THEN
        IF(LNG.EQ.1) WRITE(LU,200)
        IF(LNG.EQ.2) WRITE(LU,201)
200     FORMAT(1X,'RESCUE : FICHIER DE RESULTATS DU CALCUL PRECEDENT',
     &         /,1X,'         SANS LA VITESSE V, ON LA PREND NULLE')
201     FORMAT(1X,'RESCUE : PREVIOUS COMPUTATION RESULTS FILE',
     &         /,1X,'         WITHOUT VELOCITY V, WE FIX IT TO ZERO')
        CALL OV( 'X=C     ' , V , V , V , 0.D0 , NPOIN )
      ENDIF
!
!-----------------------------------------------------------------------
!
!  WATER DEPTH
!
      IF(TROUVE(4).NE.1) THEN
        IF(TROUVE(5).EQ.1) THEN
          IF(LNG.EQ.1) WRITE(LU,400)
          IF(LNG.EQ.2) WRITE(LU,401)
400       FORMAT(1X,'RESCUE : HAUTEUR D''EAU CALCULEE AVEC LE FOND',
     &         /,1X,'         ET LA SURFACE LIBRE')
401       FORMAT(1X,'RESCUE : WATER DEPTH COMPUTED WITH BATHYMETRY',
     &         /,1X,'         AND SURFACE ELEVATION')
          CALL OV( 'X=Y-Z   ' , H , S , ZF , 0.D0 , NPOIN )
        ELSE
          IF(LNG.EQ.1) WRITE(LU,420)
          IF(LNG.EQ.2) WRITE(LU,421)
420       FORMAT(1X,'RESCUE : IMPOSSIBLE DE CALCULER LA HAUTEUR D''EAU')
421       FORMAT(1X,'RESCUE : WATER DEPTH CANNOT BE COMPUTED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!  TRACER
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(TROUVE(34+1+ITRAC).EQ.0) THEN
            IF(LNG.EQ.1) WRITE(LU,900)
            IF(LNG.EQ.2) WRITE(LU,901)
900         FORMAT(1X,'RESCUE : CALCUL PRECEDENT SANS TRACEUR,',
     &           /,1X,'         ON PREND TRAC0')
901         FORMAT(1X,'RESCUE : PREVIOUS CALCULATION WITHOUT TRACER',
     &           /,1X,'         WE FIX IT TO TRAC0')
            CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!  K AND EPSILON
!
      IF(ITURB.EQ.3.AND.TROUVE(10).EQ.1.AND.TROUVE(11).EQ.1) THEN
        AKEP=.FALSE.
      ENDIF
      IF(ITURB.EQ.3.AND.(TROUVE(10).EQ.0.OR.TROUVE(11).EQ.0)) THEN
        IF(LNG.EQ.1) WRITE(LU,950)
        IF(LNG.EQ.2) WRITE(LU,951)
950     FORMAT(1X,'RESCUE : K ET EPSILON SERONT REINITIALISES')
951     FORMAT(1X,'RESCUE : K AND EPSILON WILL BE SET AGAIN')
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

