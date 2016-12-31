!
!  CHANGES VS SOURCE FILES:
!  IN CONDIN: SPECIAL INITIAL CONDITIONS FOR WATER DEPTH: CALLS CORSUI
!             WHICH USES DISTAN
!
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
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TIME
!
      AT = 0.D0
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
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIN : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &         ,/,'         VOUS DEVEZ MODIFIER CONDIN')
11      FORMAT(1X,'CONDIN : WITH SPECIAL INITIAL CONDITIONS'
     &         ,/,'         YOU HAVE TO MODIFY CONDIN')
        CALL PLANTE(1)
        STOP
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
! BEGIN OF PART SPECIFIC TO THIS CASE
      CALL CORSUI(H%R,U%R,V%R,ZF%R,X,Y,NPOIN)
! END OF PART SPECIFIC TO THIS CASE
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
! INITIALISES THE VISCOSITY
!
      CALL OS('X=C     ',X=VISC,C=PROPNU)
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
     &(H,U,V,ZF,X,Y,NPOIN)
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
      DOUBLE PRECISION H(*),X(*),Y(*),ZF(*),U(*),V(*)
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
          U(I) = 0.D0
          V(I) = 0.D0
        ENDIF
!
!  ZONE DERRIERE LE BARRAGE MAIS QUI N'EST PAS DANS
!  LA RETENUE.
!
        IF((X(I)-4500.D0)**2+(Y(I)-5350.D0)**2.LT.200.D0**2) THEN
          H(I)=0.D0
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    **************************
                     SUBROUTINE UTIMP_TELEMAC2D
!                    **************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
!+        V5P4
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
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
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
      SAVE INUM,IH,HAUT,ARR_TIME,FIND_ARR_TIME,DEJA_UTIMP
!
!***********************************************************************
! USER OUTPUT
!
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
        CALL PROXIM(INUM,XMES,YMES,X,Y,14,NPOIN,IKLE%I,NELEM,NELEM)
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
      IF(IPID.EQ.0) THEN
        IF(LT.EQ.NIT) THEN
          NRFO = T2D_FILES(T2DRFO)%LU
          WRITE(NRFO,*) 'MAXIMUM WATER DEPTHS'
          DO J=6,14
            WRITE(NRFO,1001) 'MEASUREMENT POINT',J,' = ',HAUT(J),' M'
          ENDDO
!
          WRITE(NRFO,*)
          WRITE(NRFO,1002)'ARRIVAL TIME AT A :',ARR_TIME(1),' S'
          WRITE(NRFO,1002)'TIME FROM A TO B  :',ARR_TIME(2)-ARR_TIME(1),
     &                    ' S'
          WRITE(NRFO,1002)'TIME FROM A TO C  :',ARR_TIME(3)-ARR_TIME(1),
     &                    ' S'
        ENDIF
      ENDIF
!
 1001 FORMAT((A,I3,A,F6.2,A))
 1002 FORMAT((A,F7.1,A))
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D
