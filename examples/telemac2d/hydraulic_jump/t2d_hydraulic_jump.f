!                    ***************************
                     SUBROUTINE PRERES_TELEMAC2D
!                    ***************************
     &    (IMP,LEO)
!
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  J-M HERVOUET (LNHE)
!+        24/11/2009
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
!history  C. GOEURY (EDF R&D LNHE)
!+        25/07/2013
!+        V6P3
!+   Sum of HAP in oilspills has been added.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        02/01/2014
!+        V7P0
!+   Securing bound checking in parallelism.
!
!history  J-M HERVOUET EDF R&D, LNHE)
!+        28/10/2014
!+        V7P0
!+   Initialising Lagrangian drifts for iteration 0 in case they are
!+   in outputs.
!
!history  R. ATA & J-M HERVOUET (EDF LAB, LNHE)
!+        10/06/2015
!+        V7P1
!+   Now all the variables asked for graphic printouts are written for
!+   remarkable points.
!
!history  R. ATA (EDF LAB, LNHE)
!+        11/01/2016
!+        V7P2
!+   Now preres gives instruction to bief_desimp to write graphical
!+   results (through leo and imp)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL ,INTENT(INOUT)::IMP,LEO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL DEJA1,DEJA2
!
      INTEGER LTT,N,IMAX,I,II,JJ
!
      DOUBLE PRECISION HHH,XMAX
      DOUBLE PRECISION, PARAMETER:: EPSS=1.E-10
      DOUBLE PRECISION GPRDTIME,LPRDTIME,RESTE
!
      INTRINSIC MAX,SQRT,CEILING
!
!-----------------------------------------------------------------------
!
! LOGIQUES POUR DECIDER DES SORTIES
!
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      IF(LT.EQ.0) THEN
        IMP=.TRUE.
        LEO=.TRUE.
        COMPLEO=0
      ELSE
         IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         FEM
          LTT=(LT/LISPRD)*LISPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
          LTT=(LT/LEOPRD)*LEOPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
!         FOR GRAPHICAL OUTPUTS          
          IF(LEO)COMPLEO=COMPLEO+1
        ELSE
!         FVM
          GPRDTIME=LEOPRD*DTINI
          LPRDTIME=LISPRD*DTINI
          IF(GPRDTIME.LT.EPSS.OR.LPRDTIME.LT.EPSS)THEN
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(LT.GE.PTINIG)THEN
!           GRAPHIC OUTPUT
            LTT=CEILING(AT/GPRDTIME)
            RESTE=(LTT*GPRDTIME-AT)/GPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              LEO=.TRUE.
              COMPLEO=COMPLEO+1
            ENDIF
            
          ENDIF
          IF(LT.GT.PTINIL)THEN
!           LISTING OUTPUT
            LTT=CEILING(AT/LPRDTIME)
            RESTE=(LTT*LPRDTIME-AT)/LPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              IMP=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! CALCUL DE LA CELERITE (MISE DANS FU, VOIR LE BLOC VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE (= H + ZF, MISE DANS FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS( 'X=Y+Z   ' , FV , H , ZF , 0.D0 )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE FROUDE
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL OS( 'X=YZ    ' , T4 , H , U , 0.D0 )
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL OS( 'X=YZ    ' , T5 , H , V , HHH )
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , T6 , U , V , HHH )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE COURANT
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF (LNG.EQ.1) WRITE(LU,78) XMAX
        IF (LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE ET DE LA HAUTEUR EXACTE : FORME CONSERVATIVE
!=======================================================================
!
      IF(((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))).AND.
     &   ((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)))) THEN
!                   HAUTEUR          VITESSE
        CALL EXACTE(PRIVE%ADR(1)%P%R,PRIVE%ADR(2)%P%R,ZF%R,X,NPOIN,1)
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE EXACTE
!=======================================================================
!
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
!                            Z
        CALL OV( 'X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     &                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE "NON-CONSERVATIVE"
!=======================================================================
!
      IF((LEO.AND.SORLEO(26)).OR.(IMP.AND.SORIMP(26))) THEN
        DO N=1,NPOIN
          HHH = MAX(PRIVE%ADR(1)%P%R(N),1.D-8)
          PRIVE%ADR(4)%P%R(N)=SQRT(PRIVE%ADR(2)%P%R(N)**2/(HHH*GRAV))
        ENDDO
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
      RETURN
      END
!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(HN,U,ZF,X,NPOIN,ICONS)
!
!***********************************************************************
! PROGICIEL : 'TELEMAC'       12/12/88    F. LEPEINTRE
!                             10/02/92    J-M HERVOUET (REMPLACEMENT
!                             DE ZRPOLY PAR ZBRENT, IMPLICIT NONE ET
!                             DOUBLE PRECISION)
!                             02/03/92    F. LEPEINTRE
!***********************************************************************
!
!      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE
!                   SUR UN BUMP. AVEC UN RESSAUT
!
!                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE
!
!      ATTENTION : ON UTILISE ICI LE FAIT QUE LE MAILLAGE
!                  EST RECTANGULAIRE.
!
!
!      ICONS = 1 : HAUTEURS CONJUGUEES CLASSIQUES
!      ICONS = 2 : HAUTEURS CONJUGUEES "NON CONSERVATIVES"
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     HN         |<-- |  HAUTEUR D'EAU.                              |
! |     U          |<-- |  VITESSE U.
! |     ZF         | -->|  COTE DU FOND.
! |     X          | -->|  ABCISSES DES POINTS DU MAILLAGE
! |     NPOIN      | -->|  NOMBRE DE POINTS DU MAILLAGE
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION U(132,11),ZF(132,11),X(132,11),HN(132,11)
      DOUBLE PRECISION YVF(300),YV(300),PRAD(300),YAVAL
      DOUBLE PRECISION QFIXG,G,ST,PRK,XRK,YRK,XRKPR,YRKPR,YCRIT
      DOUBLE PRECISION XRKCO,YRKCO,YFLU,CSTE,RES,Y(1),Z(1)
!
      INTEGER IM,JM,I,J,ICRIT,INOEUD,NOEUD,NOEUDF,ND,NG,NRK,N,IC,NPOIN
      INTEGER ICONS
!
      DOUBLE PRECISION A(4)
      COMMON/FORFC1/A
      COMMON/HCRIT/YCRIT
!
      EXTERNAL F
      DOUBLE PRECISION F
!
      INTRINSIC REAL
!
!-----------------------------------------------------------------------
!
!     LARGEUR DU CANAL EGALE A 1.
!
!     MAILLAGE DE CARRES DECOUPES EN TRIANGLES, L'ORDRE DES POINTS EST TEL
!     QU'ON PEUT FAIRE COMME SUR UN MAILLAGE REGULIER.
      IM = 132
      JM = 11
!     POINT DE PASSAGE EN CRITIQUE (DIFFERENT DU SEUIL)
!     LE FOND A ETE CALCULE POUR AVOIR LE POINT CRITIQUE A
!     LA PREMIERE MAILLE APRES LE SEUIL.
!
      ICRIT = 63
!     COTE AVAL
      YAVAL  = 0.6D0
!     DEBIT LINEIQUE (PAR M2 DE LARGEUR)
      QFIXG  = 1.D0
      G      = 9.81D0
!     COEFFICIENT DE STRICKLER
      ST     = 40.D0
!
      DO NOEUD=1,IM-1
        ND = NOEUD + 1
        NG = NOEUD
!       PENTE RADIER ? (AVEC UN SIGNE -)
        PRAD(NOEUD) = -(ZF(ND,5)-ZF(NG,5)) / (X(ND,5)-X(NG,5))
      ENDDO
!
!     PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)
!     NOMBRE DE SOUS PAS
      NRK = 10000
!     PAS DE RUNGE-KUTTA
      PRK = (X(IM,5)-X(1,5))/FLOAT(NRK-1)
!
!     ON COMMENCE PAR CALCULER LA LIGNE D'EAU FLUVIALE
!     DEPUIS L'AVAL TANT QUE Y SUPERIEUR A YCRITIQUE
!
      YCRIT=(QFIXG**2/G)**(1.D0/3.D0)
      YVF(IM) = YAVAL
      XRK = X(IM,5)
      YRK = YAVAL
      IC  = IM-1
!
      DO N=1,NRK
!
!       PREDICTION
        XRKPR = XRK - PRK
        YRKPR = YRK - PRK*F(YRK,IC,QFIXG,PRAD,ST)
        IF(YRKPR.LT.YCRIT) THEN
!         CHARGE INSUFFISANTE POUR PASSER LE SEUIL EN FLUVIAL
          NOEUDF = IC+1
          GOTO 30
        ENDIF
!       CORRECTION
        XRKCO = XRKPR
        YRKCO = YRK - PRK*(F(YRK  ,IC,QFIXG,PRAD,ST) +
     &                     F(YRKPR,IC,QFIXG,PRAD,ST))*0.5D0
        IF(YRKCO.LT.YCRIT) THEN
!         CHARGE INSUFFISANTE POUR PASSER LE SEUIL EN FLUVIAL
          NOEUDF = IC+1
          GOTO 30
        ENDIF
!
!       EST-ON SORTI DE LA MAILLE COURANTE ?
        IF(XRKCO.LE.X(IC,5)) THEN
!         CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE
          YVF(IC) = (YRK-YRKCO)/PRK*(X(IC,5)-XRK)+ YRK
!         CHANGEMENT DE MAILLE COURANTE
          IC = IC-1
          IF (IC.EQ.0) GOTO 40
        ENDIF
!       ACTUALISATION
        XRK = XRKCO
        YRK = YRKCO
!
        ENDDO
40      DO NOEUD=1,IM
          YV(NOEUD) = YVF(NOEUD)
        ENDDO
        GOTO 60
!
30      CONTINUE
!
!
!       CALCUL DE LA LIGNE D'EAU A PARTIR DU POINT CRITIQUE
!
!       PARTIE FLUVIALE
!
!       PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)
!       NOMBRE DE SOUS PAS
        NRK = 10000
!       PAS DE RUNGE-KUTTA
        PRK = (X(ICRIT,5)-X(1,5))/REAL(NRK-1)
!
        YV(ICRIT) = YCRIT
        XRK = X(ICRIT,5)
        YRK = YV(ICRIT)
        IC  = ICRIT-1
!
        DO N=1,NRK
!
!       PREDICTION
          XRKPR = XRK - PRK
          YRKPR = YRK - PRK*F(YRK,IC,QFIXG,PRAD,ST)
!       CORRECTION
          XRKCO = XRKPR
          YRKCO = YRK - PRK*(F(YRK  ,IC,QFIXG,PRAD,ST)+
     &                       F(YRKPR,IC,QFIXG,PRAD,ST))/2.D0
!
!       EST-ON SORTI DE LA MAILLE COURANTE ?
          IF (XRKCO.LE.X(IC,5)) THEN
!           CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE
            YV(IC) = (YRK-YRKCO)/PRK*(X(IC,5)-XRK)+ YRK
!           CHANGEMENT DE MAILLE COURANTE
            IC = IC-1
            IF (IC.EQ.0) GOTO 80
          ENDIF
!         ACTUALISATION
          XRK = XRKCO
          YRK = YRKCO
!
          ENDDO
!
!         PARTIE TORRENTIELLE
!
80        CONTINUE
!
!         PAR RUNGE-KUTTA (METHODE DE LA TANGENTE AMELIOREE)
!         NOMBRE DE SOUS PAS
          NRK = 10000
!         PAS DE RUNGE-KUTTA
          PRK = (X(IM,5)-X(ICRIT,5))/REAL(NRK-1)
!
          XRK = X(ICRIT,5)
!         0.9999 POUR PARTIR SUR LA BONNE BRANCHE DE SOLUTION
          YRK = 0.9999D0*YCRIT
          IC  = ICRIT + 1
!
      DO N=1,NRK
!
!       PREDICTION
        XRKPR = XRK + PRK
        YRKPR = YRK + PRK*F(YRK,IC-1,QFIXG,PRAD,ST)
!       CORRECTION
        XRKCO = XRKPR
        YRKCO = YRK + PRK*(F(YRK,IC-1,QFIXG,PRAD,ST)+
     &                     F(YRKPR,IC-1,QFIXG,PRAD,ST))/2.D0
!
!       EST-ON SORTI DE LA MAILLE COURANTE
        IF (XRKCO.GE.X(IC,5)) THEN
!         CALCUL DE Y AU NOEUD PAR INTERPOLATION LINEAIRE
          YV(IC) = (YRKCO-YRK)/PRK*(X(IC,5)-XRK)+ YRK
!         CHANGEMENT DE MAILLE COURANTE
          IC = IC+1
        ENDIF
!
!       ACTUALISATION
!
        XRK = XRKCO
        YRK = YRKCO
!
      ENDDO
!
!     RECHERCHE D'UN RESSAUT
!
      IF (NOEUDF.EQ.IM) GOTO 120
      DO NOEUD=NOEUDF,IM
!       HAUTEUR CONJUGUEE DU FLUVIAL H1*H2(H1+H2)=2HC**3
        YFLU = YVF(NOEUD)
        IF(ICONS.EQ.1) THEN
          RES = (-YFLU**2+SQRT(YFLU**4+8*YFLU*YCRIT**3))/(2*YFLU)
        ELSEIF(ICONS.EQ.2) THEN
          RES = (YCRIT**3/2.D0+SQRT(YCRIT**6/4.D0
     &                   +2.D0*YFLU**3*YCRIT**3))/(2*YFLU**2)
        ELSE
          WRITE(LU,*) 'ICONS = 1 OU 2, PAS ',ICONS
          STOP
        ENDIF
        IF (RES.LE.YV(NOEUD)) THEN
          DO INOEUD=NOEUD+1,IM
            YV(INOEUD)=YVF(INOEUD)
          ENDDO
          GOTO 60
        ENDIF
      ENDDO
!     PAS DE RESSAUT
120   CONTINUE
!
60    CONTINUE
!
!-----------------------------------------------------------------------
!
      DO I=1,IM
        DO J=1,JM
          HN(I,J) = YV(I)
          U(I,J) = QFIXG/HN(I,J)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       ***************************
                        DOUBLE PRECISION FUNCTION F
!                       ***************************
!
     &(Z,MAILLE,QFIXG,PRAD,ST)
!
!***********************************************************************
! PROGICIEL : TELEMAC        07/12/88    J-M HERVOUET (LNH) 30 71 80 18
!***********************************************************************
!
!  FONCTION  :
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   Z            | -->|
! |   MAILLE       | -->|
! |   QFIXG        | -->|
! |   PRAD         | -->|
! |   ST           | -->|
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION PRAD(*),G,ANUM,DENO,QFIXG,ST,Z,YCRIT,Y
!
      INTEGER MAILLE
!
      INTRINSIC ABS,SIGN
!
      COMMON/HCRIT/YCRIT
!
!-----------------------------------------------------------------------
!
!     SI ON EST A LA PROFONDEUR CRITIQUE (DENO=0), ON PREND LA VALEUR
!     DE LA PENTE SUR UN POINT PROCHE
!
      IF(ABS(Z-YCRIT).LT.1.D-5) THEN
        Y=Z+0.001D0
      ELSE
        Y=Z
      ENDIF
!
      G = 9.81D0
      ANUM = PRAD(MAILLE) - (QFIXG**2/(ST**2*Y**(10.D0/3.D0)))
      DENO = 1.D0 - (QFIXG**2/(G*Y**3))
!     IF (ABS(DENO).LT.1.D-6) DENO = SIGN(1.D-6,DENO)
!
      F = ANUM/DENO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ***************************
                     SUBROUTINE NOMVAR_TELEMAC2D
!                    ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC,N_NAMES_PRIV,
     & NAMES_PRIVE,SECCURRENTS)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (IN TEXTPR).
!+
!+                TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  J-M HERVOUET (LNHE)
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
!+   Secondary flow correction: add variables
!+   tau_s, Omega/h and r^{-1} for visualization
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        27/07/2015
!+        V7P1
!+   Now taking into account names of private arrays given by user.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MNEMO          |<--| MNEMONIC FOR 'VARIABLES FOR GRAPHIC OUTPUTS'
!| N_NAMES_PRIV   |-->| NUMBER OF NAMES OF PRIVATE VARIABLES GIVEN
!| NAMES_PRIVE    |-->| NAME OF PRIVATE VARIABLES GIVEN BY USER
!| NAMETRAC       |-->| NAME OF TRACERS (GIVEN BY KEYWORDS)
!| NPERIAF        |-->| NUMBER OF PERIODS FOR FOURRIER ANALYSIS
!| NTRAC          |-->| NUMBER OF TRACERS
!| TEXTE          |<--| SEE ABOVE
!| TEXTPR         |<--| SEE ABOVE
!| SECCURRENTS    |-->| IF YES SECONDARY CURRENTS COMPUTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC,N_NAMES_PRIV
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(32),NAMES_PRIVE(4)
      LOGICAL, INTENT(IN)              :: SECCURRENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=2) I_IN_2_LETTERS(34)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32','33','34'/
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF(LNG.EQ.2) THEN
!
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'TRACER                          '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITY       M2/S            '
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'EXACT DEPTH     M               '
      TEXTE (24) = 'EXACT VELOCITY  M/S             '
      TEXTE (25) = 'EXACT ELEVATION M               '
      TEXTE (26) = 'NONCONS   ELEV  M               '
      TEXTE (27) = 'HIGH WATER MARK M               '
      TEXTE (28) = 'HIGH WATER TIME S               '
      TEXTE (29) = 'HIGHEST VELOCITYM/S             '
      TEXTE (30) = 'TIME OF HIGH VELS               '
      TEXTE (31) = 'FRICTION VEL.   M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
!
! TEXTPR IS USED FOR READING PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
! FROM ANOTHER CODE WITH DIFFERENT NAMES THAT YOU HAVE TO
! WRITE HERE.
!
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'TRACER                          '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITY       M2/S            '
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'VARIABLE 23     UNIT   ??       '
      TEXTPR (24) = 'VARIABLE 24     UNIT   ??       '
      TEXTPR (25) = 'VARIABLE 25     UNIT   ??       '
      TEXTPR (26) = 'VARIABLE 26     UNIT   ??       '
      TEXTPR (27) = 'HIGH WATER MARK M               '
      TEXTPR (28) = 'HIGH WATER TIME S               '
      TEXTPR (29) = 'HIGHEST VELOCITYM/S             '
      TEXTPR (30) = 'TIME OF HIGH VELS               '
      TEXTPR (31) = 'FRICTION VEL.   M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
!
!-----------------------------------------------------------------------
!
!  FRANCAIS OU AUTRE
!
      ELSE
!
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'TRACEUR                         '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'EXACT DEPTH     M               '
      TEXTE (24) = 'EXACT VELOCITY  M/S             '
      TEXTE (25) = 'EXACT ELEVATION M               '
      TEXTE (26) = 'NONCONS   ELEV  M               '
      TEXTE (27) = 'COTE MAXIMUM    M               '
      TEXTE (28) = 'TEMPS COTE MAXI S               '
      TEXTE (29) = 'VITESSE MAXIMUM M/S             '
      TEXTE (30) = 'T VITESSE MAXI  S               '
      TEXTE (31) = 'VITESSE DE FROT.M/S             '
      TEXTE (32) = 'TAU_S           NA              '
      TEXTE (33) = '1/R             1/M             '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'TRACEUR                         '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITE TURB. M2/S            '
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'VARIABLE 23     UNITES ??       '
      TEXTPR (24) = 'VARIABLE 24     UNITES ??       '
      TEXTPR (25) = 'VARIABLE 25     UNITES ??       '
      TEXTPR (26) = 'VARIABLE 26     UNITES ??       '
      TEXTPR (27) = 'COTE MAXIMUM    M               '
      TEXTPR (28) = 'TEMPS COTE MAXI S               '
      TEXTPR (29) = 'VITESSE MAXIMUM M/S             '
      TEXTPR (30) = 'T VITESSE MAXI  S               '
      TEXTPR (31) = 'VITESSE DE FROT.M/S             '
      TEXTPR (32) = 'TAU_S           NA              '
      TEXTPR (33) = '1/R             1/M             '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
!     UVCHSBFQTKEDIJMXYPWAGLNORZ
!     VELOCITY COMPONENT U
      MNEMO(1)   = 'U       '
!     VELOCITY COMPONENT V
      MNEMO(2)   = 'V       '
!     CELERITY
      MNEMO(3)   = 'C       '
!     WATER DEPTH
      MNEMO(4)   = 'H       '
!     FREE SURFACE ELEVATION
      MNEMO(5)   = 'S       '
!     BOTTOM ELEVATION
      MNEMO(6)   = 'B       '
!     FROUDE
      MNEMO(7)   = 'F       '
!     FLOW RATE
      MNEMO(8)   = 'Q       '
!     EX TRACER
      MNEMO(9)   = '?       '
!     TURBULENT ENERGY
      MNEMO(10)   = 'K       '
!     DISSIPATION
      MNEMO(11)   = 'E       '
!     TURBULENT VISCOSITY
      MNEMO(12)   = 'D       '
!     FLOWRATE ALONG X
      MNEMO(13)   = 'I       '
!     FLOWRATE ALONG Y
      MNEMO(14)   = 'J       '
!     SPEED
      MNEMO(15)   = 'M       '
!     WIND COMPONENT X
      MNEMO(16)   = 'X       '
!     WIND COMPONENT Y
      MNEMO(17)   = 'Y       '
!     ATMOSPHERIC PRESSURE
      MNEMO(18)   = 'P       '
!     FRICTION
      MNEMO(19)   = 'W       '
!     DRIFT IN X
      MNEMO(20)   = 'A       '
!     DRIFT IN Y
      MNEMO(21)   = 'G       '
!     COURANT NUMBER
      MNEMO(22)   = 'L       '
!     VARIABLE 23
      MNEMO(23)   = 'N       '
!     VARIABLE 24
      MNEMO(24)   = 'O       '
!     VARIABLE 25
      MNEMO(25)   = 'R       '
!     VARIABLE 26
      MNEMO(26)   = 'Z       '
!     VARIABLE 27
      MNEMO(27)   = 'MAXZ    '
!     VARIABLE 28
      MNEMO(28)   = 'TMXZ    '
!     VARIABLE 29
      MNEMO(29)   = 'MAXV    '
!     VARIABLE 30
      MNEMO(30)   = 'TMXV    '
!     VARIABLE 31
      MNEMO(31)   = 'US      '
!
      MNEMO(32)   = 'TAU_S   '
!
      MNEMO(33)   = '1/R     '
!
!-----------------------------------------------------------------------
!
!     FOURIER ANALYSES
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
          IF(LNG.EQ.1) THEN
            TEXTE(34+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(35+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(34+NTRAC+2*(I-1)) =  'AMPLI PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(35+NTRAC+2*(I-1)) =  'PHASE PERIODE '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ELSE
            TEXTE(34+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTE(35+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
            TEXTPR(34+NTRAC+2*(I-1)) =  'AMPLI PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'M               '
            TEXTPR(35+NTRAC+2*(I-1)) =  'PHASE PERIOD  '
     &                         //I_IN_2_LETTERS(I)
     &                         //'DEGRES          '
          ENDIF
          MNEMO(34+NTRAC+2*(I-1)) = 'AMPL'//I_IN_2_LETTERS(I)//'  '
          MNEMO(35+NTRAC+2*(I-1)) = 'PHAS'//I_IN_2_LETTERS(I)//'  '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(33+I)  = NAMETRAC(I)
          TEXTPR(33+I) = NAMETRAC(I)
          MNEMO(33+I)  = 'T'//I_IN_2_LETTERS(I)//'   '
        ENDDO
!       OMEGA FOR SECONDARY CURRENTS
        IF(SECCURRENTS) THEN
          TEXTE(33+NTRAC) = NAMETRAC(NTRAC)
          TEXTPR(33+NTRAC)= NAMETRAC(NTRAC)
          MNEMO(33+NTRAC) = 'OMEGA   '
        ENDIF
      ENDIF
      IF(N_NAMES_PRIV.GT.0) THEN
        DO I=1,N_NAMES_PRIV
          TEXTE(22+I)  = NAMES_PRIVE(I)
          TEXTPR(22+I) = NAMES_PRIVE(I)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER I
      DOUBLE PRECISION N
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
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
        ZF%R(I) = MAX(-0.2D0,-0.0246875D0*(X(I)-10.D0)**2)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
