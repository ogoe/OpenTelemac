!
!  CHANGES VS SOURCE FILES:
!  IN CONDIM: SPECIAL INITIAL CONDITIONS FOR WATER DEPTH: CALLS CORSUI
!             WHICH USES DISTAN
!  IN UTIMP: TO EXTRACT DATA TO BE COMPARED WITH MEASUREMENTS
!
!                    *****************
                     SUBROUTINE CONDIM
!                    *****************
!
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES VELOCITY, DEPTH AND TRACERS.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET(LNH)
!+        11/12/2000
!+        V5P1
!+   TELEMAC 3D VERSION 5.1
!
!history
!+        20/04/2007
!+
!+   ADDED INITIALISATION OF DPWAVE
!
!history
!+        23/01/2009
!+
!+   ADDED CHECK OF ZSTAR
!
!history
!+        16/03/2010
!+
!+   NEW OPTIONS FOR BUILDING THE MESH IN CONDIM, SEE BELOW
!
!history  J-M HERVOUET(LNHE)
!+        05/05/2010
!+        V6P0
!+   SUPPRESSED INITIALISATION OF DPWAVE
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
!+        C.-T. PHAM (LNHE)
!+        19/07/2012
!+        V6P2
!+   Addition of the TPXO tidal model by calling CONDI_TPXO
!+   (the TPXO model being coded in module TPXO)
!
!history  C.-T. PHAM (LNHE), M.S.TURNBULL (HRW)
!+        02/11/2012
!+        V6P3
!+   Correction of bugs when initialising velocity with TPXO
!+   or when sea levels are referenced with respect to Chart Datum (CD)
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
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE TPXO
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!***********************************************************************
!
!     ORIGIN OF TIME
!
      IF(.NOT.SUIT2) AT  = 0.D0
!
!     INITIALISES H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
        CALL OS('X=-Y    ',X=H,Y=ZF)
        CALL CONDI_TPXO(NPOIN2,MESH2D%NPTFR,MESH2D%NBOR%I,
     &                  X2%R,Y2%R,H%R,U2D%R,V2D%R,
     &                  LIHBOR%I,LIUBOL%I,KENT,KENTU,
     &                  GEOSYST,NUMZONE,LATIT,LONGIT,
     &                  T3D_FILES,T3DBB1,T3DBB2,
     &                  MARDAT,MARTIM,INTMICON,MSL,
     &                  TIDALTYPE,BOUNDARY_COLOUR,ICALHWG)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!     USER INPUT :
!     PROGRAM HERE SPECIAL INITIAL CONDITIONS ON DEPTH
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'
     &      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')
        CALL PLANTE(1)
        STOP
!     END OF SPECIAL INITIAL CONDITIONS
!     END OF USER INPUT
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
! BEGIN OF PART SPECIFIC TO THIS CASE
      CALL CORSUI(H%R,Z,X,Y,NPOIN2)
! END OF PART SPECIFIC TO THIS CASE
!
!     CLIPS H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
!     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
!     TRANSF IS KEYWORD "MESH TRANSFORMATION"
!     IF TRANSF = 0, SUBROUTINE CALCOT MUST BE IMPLEMENTED BY THE USER
!
!     AN EQUIVALENT OF TRANSF MUST BE GIVEN FOR EVERY PLANE:
!
!     POSSIBLE VALUES OF TRANSF_PLANE :
!
!     1 : SIGMA TRANSFORMATION WITH EVENLY SPACED PLANES
!     2 : SIGMA TRANSFORMATION WITH PROPORTIONS GIVEN IN ZSTAR
!     3 : PRESCRIBED ELEVATION GIVEN IN ZPLANE
!
!     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
!
!     OTHER EXAMPLES:
!
!     EXAMPLE 1: ALL PLANES WITH PRESCRIBED ELEVATION
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=3
!     ENDDO
!     ZPLANE%R(2)=-7.D0
!     ZPLANE%R(3)=-4.D0
!     ...
!     ZPLANE%R(NPLAN-1)=-0.05D0
!
!
!     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     ZSTAR%R(1)=0.D0
!     ZSTAR%R(2)=0.02D0
!     ZSTAR%R(3)=0.1D0
!     ...
!     ZSTAR%R(NPLAN-1)=0.95D0
!     ZSTAR%R(NPLAN)=1.D0
!
!
!     EXAMPLE 3: ONE PLANE (NUMBER 4) WITH PRESCRIBED ELEVATION
!                AND SIGMA ELSEWHERE
!
!     DO IPLAN = 1,NPLAN
!       TRANSF_PLANE%I(IPLAN)=1
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=-3.D0
!
!
!     EXAMPLE 4: ONE PLANE WITH PRESCRIBED ELEVATION
!                AND 2 SIGMA TRANSFORMATIONS, WITH NPLAN=7
!                SIGMA TRANSFORMATIONS ARE MEANT BETWEEN
!                BOTTOM, FIXED ELEVATION PLANES AND FREE SURFACE
!                THE VALUES OF ZSTAR ARE LOCAL FOR EVERY
!                SIGMA TRANSFORMATION: 0. FOR LOWER FIXED PLANE
!                                      1. FOR UPPER FIXED PLANE
!
!     DO IPLAN = 1,7
!       TRANSF_PLANE%I(IPLAN)=2
!     ENDDO
!     TRANSF_PLANE%I(4)=3
!     ZPLANE%R(4)=3.D0
!     ZSTAR%R(2)=0.2D0
!     ZSTAR%R(3)=0.8D0
!     ZSTAR%R(5)=0.1D0
!     ZSTAR%R(6)=0.9D0
!
!
!***********************************************************************
!
!     COMPUTES ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISES VELOCITIES
!
      IF(SUIT2) THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR.
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
            U%R((I-1)*NPOIN2+J)=U2D%R(J)
            V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISES THE K-EPSILON MODEL (OPTIONAL)
!   WHEN DONE: AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISES K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISES THE PRESSURE FIELDS TO 0.0
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!               ***************************************************
                DOUBLE PRECISION FUNCTION DISTAN(X1,Y1,X2,Y2,X3,Y3)
!               ***************************************************
!
!***********************************************************************
! PROGICIEL : TELEMAC           23/07/91
!
!***********************************************************************
!
!   FONCTION : CETE FONCTION CALCULE LA DISTANCE ENTRE UNE DROITE
! ET UN POINT SUR LE MAILLAGE
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    X1          | -->  ABSCISSE DU PREMIER POINT SUR LA DROITE
! |    Y1          | -->| COORDONNEE DU PREMIER POINT SUR LA DROITE
! |    X2          | -->  ABSCISSE DU DEUXIEME POINT SUR LA DROITE
! |    Y2          | -->| COORDONNEE DU DEUXIEME POINT SUR LA DROITE
! |    X           | -->| ABSCISSE DU POINT POUR LEQUEL ON CHERCHE DIST1
! |    Y           | -->| COORDONNEE DU POINT POUR LEQUEL ON CHERCHE DIS
! |    DISTAN      |<-- |  DISTANCE ENTRE LA DROITE ET LE POINT
! |________________|____|_______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3
      DOUBLE PRECISION A1,B1,C1,DET
      INTRINSIC SQRT
      A1=Y1-Y2
      B1=-X1+X2
      C1=X1*Y2-X2*Y1
      DET=SQRT((A1**2)+(B1**2))
      DISTAN=((A1*X3)+(B1*Y3)+C1)/DET
      RETURN
      END
!                       *****************
                        SUBROUTINE CORSUI
!                       *****************
!
!
     &(H,ZF,X,Y,NPOIN)
!
!***********************************************************************
! PROGICIEL : TELEMAC           01/03/90    J-M HERVOUET
!***********************************************************************
!
!  FONCTION  : FONCTION DE CORRECTION DES FONDS RELEVES
!
!              CE SOUS-PROGRAMME UTILITAIRE NE FAIT RIEN DANS LA
!              VERSION STANDARD. IL EST A LA DISPOSITION DES
!              UTILISATEURS, POUR LISSER OU CORRIGER DES FONDS SAISIS
!              PAR EXEMPLE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT : TELMAC
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NPOIN,I
!
      DOUBLE PRECISION H(*),X(*),Y(*),ZF(*)
!
      DOUBLE PRECISION DISTAN,X1,X2,Y1,Y2,HD
      EXTERNAL DISTAN
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VARIABLES POUR LE CALCUL DE LA SITUATION DU POINT
!   X1,Y1,X2,Y2 POINT DEFINISANT LA DROITE DE LIMITE DE BARRAGE
!   X3,Y3 POINT DEFINISANT LES COORDONNEES D POINT A DROITE DE LIMITE DE
!
      X1= 4701.183D0
      Y1= 4143.407D0
      X2= 4655.553D0
      Y2= 4392.104D0
!
      DO I=1,NPOIN
        HD=DISTAN(X1,Y1,X2,Y2,X(I),Y(I))
        IF(HD.GT.0.001D0) THEN
          H(I) = 100.D0 - ZF(I)
        ENDIF
!
!  ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS
!  LA RETENUE.
!
        IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN
          H(I)=0.D0
        ENDIF
!
        H(I)=MAX(0.D0,H(I))
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ****************
                     SUBROUTINE UTIMP
!                    ****************
!
     & (LT,TIME,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PRINTS OUT SPECIFIC RESULTS
!+               (SEE THE LIST OF VARIABLES IN DECLARATIONS_TELEMAC3D).
!+
!+            FOR BIEF_OBJ STRUCTURES, THE DOUBLE PRECISION ARRAY
!+                IS IN COMPONENT R, E.G. U%R FOR THE VELOCITY.
!
!history  C LE NORMANT(LNH)    ; F LEPEINTRE (LNH)
!+        25/11/97
!+        V5P2
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
!| GRADEBL        |-->| NUMBER OF FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!| GRAPRDL        |-->| GRAPHIC PRINTOUT PERIOD
!| LISDEBL        |-->| NUMBER OF FIRST TIME STEP FOR LISTING PRINTOUTS
!| LISPRDL        |-->| LISTING PRINTOUT PERIOD
!| LT             |-->| ITERATION NUMBER
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: LT,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,J,NRFO,INUM(14),IH
!
      DOUBLE PRECISION XMES(14),YMES(14),HAUT(14),ARR_TIME(3)
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL P_DMAX,P_DMIN
      LOGICAL FIND_ARR_TIME(3)
      LOGICAL DEJA_UTIMP
!
      DATA    DEJA_UTIMP /.FALSE./
!
      INTRINSIC MOD
!
      SAVE INUM,IH,HAUT,ARR_TIME,FIND_ARR_TIME,DEJA_UTIMP
!
!***********************************************************************
! USER OUTPUT ACCORDING TO THE VALUES: TO BE IMPLEMENTED
!
      IF(.NOT.DEJA_UTIMP) THEN
!
        XMES( 1) = 5550.D0
        XMES( 2) = 11900.D0
        XMES( 3) = 13000.D0
        XMES( 4) = 11900.D0
        XMES( 5) = 13000.D0
        XMES( 6) = 4947.D0
        XMES( 7) = 5717.D0
        XMES( 8) = 6775.D0
        XMES( 9) = 7128.D0
        XMES(10) = 8585.D0
        XMES(11) = 9674.D0
        XMES(12) = 10939.D0
        XMES(13) = 11724.D0
        XMES(14) = 12723.D0
!
        YMES( 1) = 4400.D0
        YMES( 2) = 3250.D0
        YMES( 3) = 2700.D0
        YMES( 4) = 3250.D0
        YMES( 5) = 2700.D0
        YMES( 6) = 4289.D0
        YMES( 7) = 4407.D0
        YMES( 8) = 3869.D0
        YMES( 9) = 3162.D0
        YMES(10) = 3443.D0
        YMES(11) = 3085.D0
        YMES(12) = 3044.D0
        YMES(13) = 2810.D0
        YMES(14) = 2485.D0
!
        DO J=1,14
          INUM(J) = 0
          HAUT(J) = 0.D0
        ENDDO
!
        CALL PROXIM(INUM,XMES,YMES,X2%R,Y2%R,14,NPOIN2,IKLE2%I,NELEM2,
     &              NELEM2)
!
        DO IVAR=1,MAXVAR
          IF (TEXTE(IVAR) == 'WATER DEPTH') IH = IVAR
        ENDDO
!
        DO J=1,3
          FIND_ARR_TIME(J) = .FALSE.
          ARR_TIME(J)      = 0.D0
        ENDDO
!
        DEJA_UTIMP = .TRUE.
!
      ENDIF
!
      DO J=1,14
        IF(INUM(J).NE.0) THEN
          HAUT(J) = MAX(H%R(INUM(J)),HAUT(J))
        ELSE
          HAUT(J) = 0.D0
        ENDIF
      ENDDO
!
      DO J=1,3
        IF(INUM(J).NE.0) THEN
          IF(H%R(INUM(J)).GT.1.D-4.AND..NOT.FIND_ARR_TIME(J)) THEN
            ARR_TIME(J) = AT
            FIND_ARR_TIME(J) = .TRUE.
          ENDIF
        ELSE
          ARR_TIME(J) = 0.D0
          FIND_ARR_TIME(J) = .FALSE.
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO J=1,14
          HAUT(J) = P_DMAX(HAUT(J))+P_DMIN(HAUT(J))
        ENDDO
        DO J=1,3
          ARR_TIME(J) = P_DMAX(ARR_TIME(J))+P_DMIN(ARR_TIME(J))
        ENDDO
      ENDIF
!
      IF(LT.EQ.NIT) THEN
        NRFO = T3D_FILES(T3DRFO)%LU
        WRITE(NRFO,*) 'MAXIMUM WATER DEPTHS'
        DO J=6,14
          WRITE(NRFO,1001) 'MEASUREMENT POINT',J,' = ',HAUT(J),' M'
        ENDDO
!
        WRITE(NRFO,*)
        WRITE(NRFO,1002)'ARRIVAL TIME AT A :',ARR_TIME(1),' S'
        WRITE(NRFO,1002)'TIME FROM A TO B  :',ARR_TIME(2)-ARR_TIME(1),
     &                  ' S'
        WRITE(NRFO,1002)'TIME FROM A TO C  :',ARR_TIME(3)-ARR_TIME(1),
     &                  ' S'
      ENDIF
!
 1001 FORMAT((A,I3,A,F6.2,A))
 1002 FORMAT((A,F7.1,A))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP
