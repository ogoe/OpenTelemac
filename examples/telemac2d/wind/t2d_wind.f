!                    *****************
                     SUBROUTINE CONDIN
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS H, U, V ETC.
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
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
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        06/12/2011
!+        V6P2
!+   Addition of the Tsunami displacement (based on Okada's model)
!+   by calling CONDI_OKADA and of the TPXO tidal model by calling
!+   CONDI_TPXO (the TPXO model being coded in module TPXO)
!
!history  C.-T. PHAM (LNHE)
!+        03/09/2015
!+        V7P1
!+   Change in the number of arguments when calling CONDI_TPXO
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE TPXO
      USE OKADA
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION FAIR1, WIND, FVENT, HINI, LCANAL
      COMMON/FORFUN/FVENT,LCANAL,HINI
      INTEGER I,ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TIME
!
      AT = 0.D0
!
      FAIR1  = 1.2615D-3
      WIND   = 5.D0
      FVENT  = FAIR1*WIND*WIND
      HINI   = -ZF%R(1)
      LCANAL = 500.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE VELOCITIES: ZERO VELOCITIES
!
      CALL OS('X=0     ',X=U)
      CALL OS('X=0     ',X=V)
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WATER DEPTH H
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=0     ' , X=H )
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
        CALL OS('X=-Y    ',X=H,Y=ZF)
        CALL CONDI_TPXO(NPOIN,MESH%NPTFR,MESH%NBOR%I,
     &                  X,Y,H%R,U%R,V%R,
     &                  LIHBOR%I,LIUBOR%I,KENT,KENTU,
     &                  GEOSYST,NUMZONE,LAMBD0,PHI0,
     &                  T2D_FILES,T2DBB1,T2DBB2,
     &                  MARDAT,MARTIM,INTMICON,MSL,
     &                  TIDALTYPE,BOUNDARY_COLOUR,ICALHWG)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!
!  TO BE MODIFIED BY USER IF SPECIAL INITIAL CONDITIONS
!
      CALL EXACTE(H%R,X,Y,NPOIN,ZF%R)
      DO I = 1,NPOIN
        PRIVE%ADR(1)%P%R(I) = H%R(I)
      ENDDO
!
!  END OF CODE TO BE MODIFIED BY USER
!
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES TSUNAMI DISPLACEMENT
!
      IF(OPTTSUNAMI.EQ.1) THEN
        CALL CONDI_OKADA(NPOIN,X,Y,H%R,COETSUNAMI,LAMBD0,PHI0)
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS('X=C     ',X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE VISCOSITY
!
      CALL OS('X=C     ',X=VISC,C=PROPNU)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(FEXA,X,Y,NPOIN,ZF)
!
!***********************************************************************
! PROGICIEL : EX-PROGRAMME DE F. LEPEINTRE
!***********************************************************************
!
!     FONCTION:
!     =========
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |                |    |                                              |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NPOIN,I
!
      DOUBLE PRECISION FEXA(NPOIN),LCANAL,HINI,H0,GRAV,FVENT
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),ZF(NPOIN)
!
      EXTERNAL FUNC
      INTRINSIC SQRT
      DOUBLE PRECISION FUNC
!
      COMMON/FORFUN/FVENT,LCANAL,HINI
!
!-----------------------------------------------------------------------
!
!     HAUTEUR D'EAU EN X=0 A L'EQUILIBRE (POUR LA SOLUTION ANALYTIQUE)
!
      GRAV = 9.81D0
      H0 = HINI
      CALL ZBRENT(FUNC,1.D-6,0.D0,H0,100)
!
!     CALCUL DE LA SOLUTION EXACTE
!
      DO I = 1 , NPOIN
!
        FEXA(I) = SQRT( 2.D0*FVENT * X(I) / GRAV + H0*H0)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       ******************************
                        DOUBLE PRECISION FUNCTION FUNC
!                       ******************************
!
     &(X)
!
!***********************************************************************
! PROGICIEL : MITHRIDATE     01/06/90    PAINTER (LNH) 30 87 78 54
!***********************************************************************
!
!     FONCTION:
!     =========
!
!     FONCTION DONT LE ZERO CORRESPOND A LA HAUTEUR EN X=0
!     QUAND LA SURFACE LIBRE EQUILIBRE LA FORCE DUE AU VENT
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |                |    |                                              |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
      DOUBLE PRECISION A1,A2,LCANAL,X,FVENT,HINI,GRAV
!
      COMMON/FORFUN/FVENT,LCANAL,HINI
!
!----------------------------------------------------------------------
!
      GRAV = 9.81D0
      A1 = 2.D0 * FVENT * LCANAL / GRAV
      A2 = 3.D0 * FVENT * HINI * LCANAL / GRAV
      FUNC =  (A1+X*X)**1.5D0 - X**3 - A2
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CORFON
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        ZF%R(I) = -2.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE ZBRENT
!                       *****************
!
     &(FC1,EPS,X1,X2,ITMAX)
!
!***********************************************************************
! BIEF VERSION 3.0           18/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!  FONCTION  :  SOLUTION D'UNE EQUATION DONT UN ZERO UNIQUE EST ENTRE
!               LES POINTS X1 ET X2.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   FC1          | -->| FONCTION DONT ON CHERCHE LE ZERO
! |                |    | DOIT ETRE DEFINIE EN DOUBLE PRECISION
! |                |    | PAR AILLEURS.
! |   EPS          | -->| PRECISION CHERCHEE.
! |   X1,X2        | -->| ENCADREMENT DE LA SOLUTION ENTREE
! |                |<-->| X2 = SOLUTION EN SORTIE.
! |   ITMAX        | -->| NOMBRE MAXIMUM D'ITERATIONS.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!  FONCTION APPELEE : FC1
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION AA,B,C,D,E,X1,X2,FA,FB,FC,EPS,EPS2,XM,S,P,Q,R
!
      INTEGER ITMAX,ITER
!
      DOUBLE PRECISION FC1
      EXTERNAL FC1
!
      INTRINSIC ABS,SIGN,MIN
!
!-----------------------------------------------------------------------
!
!  ON VERIFIE QU'ON ENCADRE BIEN LA SOLUTION :
!
      AA=X1
      B=X2
      FA=FC1(AA)
      FB=FC1(B)
      IF(FB*FA.GT.0.D0) THEN
        IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : FC1(X1)*FC1(X2) EST POSITIF'
        IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : ROOT MUST BE BRACKETED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!  ITERATIONS :
!
      FC=FB
      DO ITER=1,ITMAX
        IF(FB*FC.GT.0.D0) THEN
          C=AA
          FC=FA
          D=B-AA
          E=D
        ENDIF
        IF(ABS(FC).LT.ABS(FB)) THEN
          AA=B
          B=C
          C=AA
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        EPS2=0.5D0*EPS
        XM=0.5D0*(C-B)
        IF(ABS(XM).LE.EPS2.OR.FB.EQ.0.D0)THEN
          X2=B
          RETURN
        ENDIF
        IF(ABS(E).GE.EPS2.AND.ABS(FA).GT.ABS(FB)) THEN
          S=FB/FA
          IF(AA.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-AA)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)
          ENDIF
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2*P.LT.MIN(3.D0*XM*Q-ABS(EPS2*Q),ABS(E*Q))) THEN
            E=D
            D=P/Q
          ELSE
            D=XM
            E=D
          ENDIF
        ELSE
          D=XM
          E=D
        ENDIF
        AA=B
        FA=FB
        IF(ABS(D).GT.EPS2) THEN
          B=B+D
        ELSE
          B=B+SIGN(EPS2,XM)
        ENDIF
        FB=FC1(B)
      ENDDO
!
      IF (LNG.EQ.1) WRITE(LU,*) 'ZBRENT : MAXIMUM D''ITERATIONS ATTEINT'
      IF (LNG.EQ.2) WRITE(LU,*) 'ZBRENT : EXCEEDING MAXIMUM ITERATIONS'
      X2=B
!
!-----------------------------------------------------------------------
!
      RETURN
      END

