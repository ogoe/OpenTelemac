!                       ***************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!  ARTEMIS    VERSION 5.1 22/08/00   D. AELBRECHT (LNH) 01 30 87 74 12
!
!  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
!
!***********************************************************************
!
!      FUNCTION :  SPECIFIES THE CONDITIONS AT EACH NODE OF THE BOUNDARY
!
!      IN VERSION 5.1, COUPLING WITH COWADIS IS POSSIBLE.
!      INFORMATION FROM A COWADIS RESULT FILE CAN BE INPUT TO DEFINE
!      INCIDENT WAVE BOUNDARY CONDITIONS WITH NON UNIFORM DIRECTION
!
!      THIS SUBROUTINE CAN BE EXTENDED BY THE USER
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   RP           |<-- |  COEFFICIENTS DE REFLEXION DES PAROIS        |
! |   TETAP        |<-- |  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES |
! |                |    |  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES    |
! |                |    |  LES FRONTIERES LIQUIDES                     |
! |                |    |  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE |
! |                |    |   DANS LE SENS DIRECT)                       |
! |   ALFAP        |<-- |  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE  |
! |                |    |  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST |
! |                |    |  POSITIF, L'ONDE REFLECHIE EST EN RETARD)    |
! |   HB           |<-- |  HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES |
! |   TETAB        |<-- |  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)   |
! |                |    |  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE   |
! |                |    |   SENS DIRECT)                               |
! |    H           | -->|  HAUTEUR D'EAU                               |
! |    K           | -->|  NOMBRE D'ONDE                               |
! |    C,CG        | -->|  VITESSES DE PHASE ET DE GROUPE              |
! |    C           | -->|  CELERITE AU TEMPS N                         |
! |    ZF          | -->|  FOND                                        |
! |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE          |
! |  TRA01,...,3   |<-->|  TABLEAUX DE TRAVAIL                         |
! | XSGBOR,YSGBOR  | -->|  NORMALES EXTERIEURES AUX SEGMENTS DE BORD   |
! |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
! |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
! |   KP1BOR       | -->|  NUMERO DU POINT FRONTIERE SUIVANT           |
! |   OMEGA        | -->|  PULSATION DE LA HOULE                       |
! |   PER          | -->|  PERIODE DE LA HOULE                         |
! |   TETAH        | -->|  ANGLE DE PROPAGATION DE LA HOULE            |
! |   GRAV         | -->|  GRAVITE                                     |
! |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
! |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
! |   KENT,KLOG    | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
! |   KSORT,KINC   |    |  LIMITES                                     |
! |                |    |  KENT  : ENTREE (VALEUR IMPOSEE)             |
! |                |    |  KLOG  : PAROI                               |
! |                |    |  KSORT : SORTIE                              |
! |                |    |  KINC  : ONDE INCIDENTE                      |
! |   PRIVE        | -->|  TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)  |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : ARTEMIS
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
!CP
      INTEGER IG0  , IG
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
!CP

      DOUBLE PRECISION PI
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
      INTRINSIC COS,SIN
!
! ---------------------------------------------------------------------
!
! DEBUT : DECLARATIONS SUPPLEMENTAIRES POUR LECTURE FICHIER COWADIS
!
! ATTENTION, POUR L'INSTANT BEAUCOUP DE CHOSES EN DUR !!
!
      LOGICAL COUPLA
!
      INTEGER NCOW,NPTH,ISTAT,IB(4),IPARAM(100),IDATE(6)
      INTEGER NVAR,NP,ID(2),JB,IERR
!
      DOUBLE PRECISION Z(1),ATT(1),RADDEG
!
!     IL FAUT DIMENSIONNER LES TABLEAUX EN DUR
!
      DOUBLE PRECISION XCOWA(15000) ,YCOWA(15000)
      DOUBLE PRECISION HSCOWA(15000),DMCOWA(15000)
      DOUBLE PRECISION HSARTE(12000),DMARTE(12000)
!
      REAL TAB1(15000)
!
      CHARACTER*80 TTITRE    , BID
      CHARACTER*32 TTEXTE(40)
      CHARACTER*3  BINCOW
      CHARACTER(LEN=32) VAR1, VAR2
      CHARACTER(LEN=8) :: FFORMAT
      INTEGER TYPE_ELEM
!
!
! FIN : DECLARATIONS SUPPLEMENTAIRES POUR LECTURE FICHIER COWADIS
!
!-----------------------------------------------------------------------
!
! DEBUT : LECTURE FICHIER COWADIS
!
!     RECUPERATION DES RESULTATS ISSUS DE COWADIS
!     POUR DEFINIR LES CONDITIONS AUX LIMITES ARTEMIS
!
!     ATTENTION ENCORE BEAUCOUP DE CHOSES EN DUR
!
!     **********************************************************
!
!     SPECIFIER LE NUMERO DU PAS DE TEMPS DE CALCUL DANS COWADIS
!     (NPTH POURRA DEVENIR 1 MOT-CLE DANS UNE PROCHAINE VERSION)
!
      NPTH = 1
!
!     **********************************************************
!
      COUPLA = .TRUE.
!
!   (COUPLA POURRA DEVENIR 1 MOT-CLE DANS UNE PROCHAINE VERSION)
!
      NCOW   = ART_FILES(ARTBI1)%LU
      BINCOW = 'STD'
      RADDEG = 180.D0/PI
      FFORMAT = 'SERAFIN '
      TYPE_ELEM = 0
!
      IF (COUPLA) THEN
!
!       LECTURE DU TITRE DU FICHIER COWADIS
!
        CALL GET_MESH_TITLE(FFORMAT,NCOW,TTITRE,IERR)
        CALL CHECK_CALL(IERR, 'BORH:GET_MESH_TITLE')
!
!       NPOIN
!
        CALL GET_MESH_NPOIN(FFORMAT,NCOW,TYPE_ELEM,NP,IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_MESH_NPOIN')
!
        WRITE(LU,*) '------------------------------------------'
        IF (LNG.EQ.1) WRITE(LU,230)
        IF (LNG.EQ.2) WRITE(LU,231)
 230    FORMAT(/,1X,'BORH : LECTURE DU FICHIER COWADIS')
 231    FORMAT(/,1X,'BORH : READING COWADIS FILE')
        IF (LNG.EQ.1) WRITE(LU,240) NP
        IF (LNG.EQ.2) WRITE(LU,241) NP
 240    FORMAT(/,1X,'NOMBRE DE POINTS DU MAILLAGE COWADIS :',1X,I7)
 241    FORMAT(/,1X,'NUMBER OF NODES OF COWADIS MESH :',1X,I7)
        IF (LNG.EQ.1) WRITE(LU,250)
        IF (LNG.EQ.2) WRITE(LU,251)
 250    FORMAT(/,1X,'MAILLAGES ARTEMIS ET COWADIS DIFFERENTS :',1X,
     &         'ON INTERPOLE')
 251    FORMAT(/,1X,'COWADIS AND ARTEMIS MESHES ARE DIFFERENT :',1X,
     &         'INTERPOLATION')
!
!       XCOWA ET YCOWA
!
        CALL GET_MESH_COORD(FFORMAT, NCOW, 1, 2, NP, XCOWA, IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_MESH_COORD:X')
        CALL GET_MESH_COORD(FFORMAT, NCOW, 2, 2, NP, YCOWA, IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_MESH_COORD:Y')
!
!       PAS DE TEMPS ET VARIABLES
!
        VAR1 = 'HAUTEUR_HM0     M               '
        VAR2 = 'TETA_MOYEN      DEG             '
        CALL GET_DATA_VALUE(FFORMAT, NCOW, NPTH, VAR1, HSCOWA, NP,IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_DATA_VALUE:HS')
        CALL GET_DATA_VALUE(FFORMAT, NCOW, NPTH, VAR2, DMCOWA, NP,IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_DATA_VALUE:DM')
        CALL GET_DATA_TIME(FFORMAT, NCOW, NPTH, ATT(1), IERR)
        CALL CHECK_CALL(IERR,'BORH:GET_DATA_TIME')
!
!       IMPRESSIONS SUR LE LISTING
!
        IF (LNG.EQ.1) WRITE(LU,260) ATT
        IF (LNG.EQ.2) WRITE(LU,261) ATT
 260    FORMAT(/,1X,'TEMPS DU CALCUL COWADIS RETENU :',1X,F10.2,' s')
 261    FORMAT(/,1X,'TIME READ IN COWADIS FILE :',1X,F10.2,' s')
        IF (LNG.EQ.1) WRITE(LU,270)
        IF (LNG.EQ.2) WRITE(LU,271)
 270    FORMAT(/,1X,'VARIABLES DE COWADIS RETENUES :')
 271    FORMAT(/,1X,'VARIABLES READ IN COWADIS FILE :')
        WRITE(LU,280) VAR1, VAR2
 280    FORMAT(/,5X,'=> ',A32,/,5X,'=> ',A32)
!
!       MODIFICATION DE LA VARIABLE DMARTE POUR ARTEMIS :
!       CHANGEMENT DE REPERE ET D'UNITE
!
        DO I=1,NP
           DMCOWA(I) = 90.D0 - DMCOWA(I)
        ENDDO
!
        IF (LNG.EQ.1) WRITE(LU,290)
        IF (LNG.EQ.2) WRITE(LU,291)
 290    FORMAT(/,1X,'BORH : FIN DE LECTURE DU FICHIER COWADIS')
 291    FORMAT(/,1X,'BORH : END OF READING COWADIS FILE')
        WRITE(LU,*) ' '
        WRITE(LU,*) '------------------------------------------'

        REWIND(NCOW)
!
!       INTERPOLATION
!        IF (NCSIZE .LE. 1) THEN

!           CALL FASPDA (X,Y,HSARTE,NPOIN,NPTFR,MESH%NBOR%I,
!     *          XCOWA,YCOWA,HSCOWA,NP)
!           CALL FASPDA (X,Y,DMARTE,NPOIN,NPTFR,MESH%NBOR%I,
!     *               XCOWA,YCOWA,DMCOWA,NP)
!        ELSE

           CALL FASPDA (X,Y,HSARTE,NPOIN,NPTFR,MESH%NBOR%I,
     &          XCOWA,YCOWA,HSCOWA,NP)
           CALL FASPDA (X,Y,DMARTE,NPOIN,NPTFR,MESH%NBOR%I,
     &          XCOWA,YCOWA,DMCOWA,NP)
!        END IF

      ENDIF
!
!
! FIN : FIN LECTURE FICHIER COWADIS
!
!--------------------------------------------------------------------
!
! CONDITIONS AUX LIMITES
! UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
! UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
! UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
! UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
!
! TOUS LES ANGLES SONT EN DEGRES
!                         ------
! ---------------------------------------
! INITIALISATION DES VARIABLES PAR DEFAUT
! ---------------------------------------
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 1.D0
!
! ------------
! PAROI SOLIDE
! ------------
!

      DO I=1,NPTFR
        JB=BOUNDARY_COLOUR%I(I)
! ------------
! PAROI SOLIDE
! ------------
!
        IF(JB.GE.1.AND.JB.LE.84)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 0.5D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF

        IF(JB.GE.85.AND.JB.LE.142)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 1.D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF

        IF(JB.GE.143.AND.JB.LE.260)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 0.5D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF
!
! ------------
! FRONTIERE ONDE INCIDENTE
! ------------
!
        IF(JB.GE.261.AND.JB.LE.281)THEN
          LIHBOR%I(I) = KINC
          HB%R(I)     = HSARTE(MESH%NBOR%I(I))
          TETAB%R(I)  = DMARTE(MESH%NBOR%I(I))
          TETAP%R(I)  = 0.D0
          ALFAP%R(I)  = 0.D0
        ENDIF
!
! ------------
! PAROI SOLIDE
! ------------
!
        IF(JB.GE.282.AND.JB.LE.302)THEN
          LIHBOR%I(I) = KLOG
          RP%R(I) = 0.5D0
          TETAP%R(I) = 0.D0
          ALFAP%R(I) = 0.D0
        ENDIF

      ENDDO
!
!-----------------------------------------------------------------------
!
! -----------------------------
!
      RETURN
      END

