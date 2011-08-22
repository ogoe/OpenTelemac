!                    *****************
                     SUBROUTINE CONDIH
!                    *****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETER ARRAYS.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        02/06/1999
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I
!
      DOUBLE PRECISION COTE
      DOUBLE PRECISION PI,BID,DHTEST
      PARAMETER( PI = 3.1415926535897932384626433D0 )
!
      INTRINSIC SINH, SQRT
!
!-----------------------------------------------------------------------
!
      CALL MAJUS(CDTINI)
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WATER DEPTH H
!
      IF(INCLUS(CDTINI,'COTE NULLE').OR.
     &   INCLUS(CDTINI,'ZERO ELEVATION') ) THEN
        COTE = 0.D0
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'COTE CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT ELEVATION') ) THEN
        COTE = COTINI
        CALL OS( 'X=C     ' , H , SBID , SBID , COTE )
        CALL OS( 'X=X-Y   ' , H , ZF  , SBID , BID  )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR NULLE').OR.
     &       INCLUS(CDTINI,'ZERO DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , 0.D0 )
      ELSEIF(INCLUS(CDTINI,'HAUTEUR CONSTANTE').OR.
     &       INCLUS(CDTINI,'CONSTANT DEPTH') ) THEN
        CALL OS( 'X=C     ' , H , SBID , SBID , HAUTIN )
      ELSEIF(INCLUS(CDTINI,'PARTICULIERES').OR.
     &       INCLUS(CDTINI,'SPECIAL')        ) THEN
!  TO BE MODIFIED BY USER
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIH : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIH')
11      FORMAT(1X,'CONDIH : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIH')
        CALL PLANTE(0)
        STOP
!  END OF CODE TO BE MODIFIED BY USER
      ELSE
        IF(LNG.EQ.1) WRITE(LU,20) CDTINI
        IF(LNG.EQ.2) WRITE(LU,21) CDTINI
20      FORMAT(1X,'CONDIH : CONDITION INITIALE INCONNUE :',/,A72)
21      FORMAT(1X,'CONDIH : UNKNOWN INITIAL CONDITION :',/,A72)
        CALL PLANTE(0)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CLIPS H (VALUES LOWER THAN 1.D-2 NOT ALLOWED)
!
      CALL CLIP(H,1.D-2,.TRUE.,1.D6,.FALSE.,NPOIN)
!
!-----------------------------------------------------------------------
!
!   COMPUTES THE WAVE NUMBER: K
!   USING AN EXPLICIT FORMULATION (SEE EDF'S EXCELLENT REPORT BY
!   F. DHELLEMMES 'PRECIS SUR LES VAGUES' )
!
!
      OMEGA = 2.D0*PI/PER
      CALL OS('X=CY    ', T1 , H , SBID , OMEGA**2/GRAV )
!
!     INITIALISES DHTEST
!
      DHTEST = 1.D6
!
      DO 100 I=1,NPOIN
         T2%R(I) = 1.D0 + T1%R(I) *( 0.6522D0 +
     &                    T1%R(I) *( 0.4622D0 +
     &                    T1%R(I) *
     &                    T1%R(I) *( 0.0864D0 +
     &                    T1%R(I) *( 0.0675D0 ) )))
         T2%R(I) = SQRT( T1%R(I)*(T1%R(I) + 1.D0/T2%R(I)) )
         K%R(I)  = T2%R(I)/H%R(I)
         DHTEST  = MIN( DHTEST , H%R(I) )
100   CONTINUE
!
!   CHECKS WHETHER H HAS BEEN CLIPPED OR NOT
!
      IF (DHTEST.LE.1.01D-2) THEN
         IF(LNG.EQ.1) WRITE(LU,120)
         IF(LNG.EQ.2) WRITE(LU,121)
120      FORMAT(1X,'CONDIH : ATTENTION !! VOUS AVEZ ATTEINT LE SEUIL '
     &          ,/,'         MINI DE HAUTEUR D''EAU (1 CM).'
     &          ,/,'         VERIFIEZ BATHY OU CONDITIONS INITIALES')
121      FORMAT(1X,'CONDIH : WARNING !! YOU REACHED MINIMUM THRESHOLD'
     &          ,/,'         FOR WATER DEPTH (1 CM). CHECK THE'
     &          ,/,'         BATHYMETRY OR INITIAL CONDITIONS')
      ENDIF
!
!-----------------------------------------------------------------------
!
!   COMPUTES PHASE VELOCITY
!
      CALL OS('X=CY    ', T1    , K     , SBID , 1.D0/OMEGA )
      CALL OS('X=1/Y   ', C     , T1    , SBID , BID        )
!
!-----------------------------------------------------------------------
!
!   COMPUTES GROUP VELOCITY
!
      DO 200 I=1,NPOIN
         CG%R(I) = C%R(I)/2.D0 *
     &             (1.D0 + 2.D0*K%R(I)*H%R(I)/SINH(2.D0*K%R(I)*H%R(I)))
200   CONTINUE
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
