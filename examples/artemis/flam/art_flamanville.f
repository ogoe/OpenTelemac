
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
      NCOW   = 24
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
!                       *****************
                        SUBROUTINE FASPDA
!                       *****************
!
     &(X,Y,VARINT,NPOIN,NPTFR,NBOR,XRELV,YRELV,VRELV,NP)
!
!***********************************************************************
!
! BIEF VERSION 4.0              17/08/94  J-C GALLAND   01 30 87 78 13
!                                         J-M HERVOUET  01 30 87 80 18
! MODIFIE POUR ARTEMIS 5.0      22/08/00  D AELBRECHT   01 30 87 74 12
!
!***********************************************************************
!
!   FONCTION : INTERPOLATION D'UNE VARIABLE SUR LES POINTS DU MAILLAGE A
!              PARTIR DE POINTS RELEVES D'UN AUTRE MAILLAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    X,Y         | -->|  COORDONNEES DU MAILLAGE
! |    VARINT      | <--|  VARIABLE INTERPOLEE EN X,Y
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE.
! |    XRELV       | -->|  ABCISSES DES POINTS RELEVES
! |    YRELV       | -->|  ORDONNEES DES POINTS RELEVES
! |    VRELV       | -->|  VALEURS DE LA VARIABLE AUX POINTS RELEVES
! |    NP          | -->|  NOMBRE DE POINTS RELEVES
! |    NBOR        | -->|  NUMEROTATION GLOBALE DES POINTS DE BORD
! |    NPTFR       | -->|  NOMBRE DE POINTS DE BORD.
! |    DM          | -->|  DISTANCE MINIMUM A LA COTE TOLEREE POUR
! |                |    |  ACCEPTER UN POINT RELEVE.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE: CROSFR
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NP,N,NPOIN,NPTFR,INUM,I,IFR
      INTEGER, INTENT(IN) ::  NBOR(NPTFR)
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),XRELV(NP),YRELV(NP),VRELV(NP)
      DOUBLE PRECISION DIST1,DIST2,DIST3,DIST4
      DOUBLE PRECISION ZCADR1,ZCADR2,ZCADR3,ZCADR4
      DOUBLE PRECISION DIFX,DIFY,DIST,X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DOUBLE PRECISION ZNUM,ZDEN,VARINT(NPOIN)
!
      LOGICAL OK1,OK2,OK3,OK4
!
!-----------------------------------------------------------------------
!
!  BOUCLE SUR LES POINTS DU MAILLAGE :
!
      DO IFR = 1 , NPTFR
!
      I = NBOR(IFR)
!
!     FOND INTERPOLE A PARTIR DE 4 QUADRANTS
!
! ---->  INITIALISATIONS:
!
      DIST1=1.D12
      DIST2=1.D12
      DIST3=1.D12
      DIST4=1.D12
!
      OK1 = .FALSE.
      OK2 = .FALSE.
      OK3 = .FALSE.
      OK4 = .FALSE.
!
      ZCADR1=0.D0
      ZCADR2=0.D0
      ZCADR3=0.D0
      ZCADR4=0.D0
!
! --------->  BOUCLE SUR LES POINTS RELEVES (IL Y EN A NP):
      DO N=1,NP
        DIFX = XRELV(N)-X(I)
        DIFY = YRELV(N)-Y(I)
        DIST = DIFX*DIFX + DIFY*DIFY
!
        IF ( DIST.LT.1.D-6 ) DIST=1.D-6
!         ->QUADRANT 1 :
          IF( DIFX.LE.0.D0.AND.DIFY.LE.0.D0) THEN
            IF(DIST.LE.DIST1)THEN
              X1=XRELV(N)
              Y1=YRELV(N)
              DIST1=DIST
              ZCADR1=VRELV(N)
              OK1 = .TRUE.
            ENDIF
!         ->QUADRANT 2 :
          ELSE IF( DIFX.GE.0.D0.AND.DIFY.LE.0.D0) THEN
            IF(DIST.LE.DIST2)THEN
              X2=XRELV(N)
              Y2=YRELV(N)
              DIST2=DIST
              ZCADR2=VRELV(N)
              OK2 = .TRUE.
            ENDIF
!         ->QUADRANT 3 :
          ELSE IF( DIFX.GE.0.D0.AND.DIFY.GE.0.D0) THEN
            IF(DIST.LE.DIST3)THEN
              X3=XRELV(N)
              Y3=YRELV(N)
              DIST3=DIST
              ZCADR3=VRELV(N)
              OK3 = .TRUE.
            ENDIF
!         ->QUADRANT 4 :
          ELSE IF( DIFX.LE.0.D0.AND.DIFY.GE.0.D0) THEN
            IF(DIST.LE.DIST4)THEN
              X4=XRELV(N)
              Y4=YRELV(N)
              DIST4=DIST
              ZCADR4=VRELV(N)
              OK4 = .TRUE.
            ENDIF
          ENDIF
      ENDDO
!
! --------->  FIN DE LA BOUCLE SUR LES POINTS RELEVES.
!
      ZNUM = 0.D0
      ZDEN = 0.D0
      INUM = 0
      IF(OK1) THEN
        ZNUM = ZNUM + ZCADR1/DIST1
        ZDEN = ZDEN + 1.D0/DIST1
        INUM = INUM + 1
      ENDIF
      IF(OK2) THEN
        ZNUM = ZNUM + ZCADR2/DIST2
        ZDEN = ZDEN + 1.D0/DIST2
        INUM = INUM + 1
      ENDIF
      IF(OK3) THEN
        ZNUM = ZNUM + ZCADR3/DIST3
        ZDEN = ZDEN + 1.D0/DIST3
        INUM = INUM + 1
      ENDIF
      IF(OK4) THEN
        ZNUM = ZNUM + ZCADR4/DIST4
        ZDEN = ZDEN + 1.D0/DIST4
        INUM = INUM + 1
      ENDIF
!
      IF(INUM.NE.0) THEN
!       VARINT : VARIABLE AU POINT
        VARINT(I)=ZNUM/ZDEN
      ELSE
        WRITE(*,*) 'INUM = ', INUM
        WRITE(*,*) 'PAS DE POINT TROUVE POUR INTERPOLER '
        WRITE(*,*) 'IGLB = ', I
        VARINT(I) = 0.D0
      ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE ART_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE ART_CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
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
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
      ENDIF
!
!-----------------------------------------------------------------------
      DO I = 1,NPOIN
        ZF%R(I) = MIN(ZF%R(I),12.D0)
      ENDDO
!
      RETURN
      END
!                    **********************
                     PROGRAM HOMERE_ARTEMIS
!                    **********************
!
!
!***********************************************************************
! ARTEMIS   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    1)  ACQUIRES DATA REQUIRED TO ALLOCATE MEMORY
!+                   (STEERING FILE + GEOMETRY)
!+
!+
!+            2)  CALLS THE REAL MAIN PROGRAM ARTEMIS.
!
!history  J-M HERVOUET (LNH)
!+        24/04/1997
!+
!+   LINKED TO BIEF 5.5
!
!history  D. AELBRECHT (LNH)
!+        19/04/1999
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER TDEB,TFIN,NCAR,IFLOT
!
      CHARACTER(LEN=24), PARAMETER :: CODE='ARTEMIS                 '
!
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
!======================================================================
!
      CHARACTER(LEN=250) PATH
      CHARACTER(LEN=144) FILE_DESC(4,300)
!
!-----------------------------------------------------------------------
!
      CALL BIEF_INIT(CODE,PATH,NCAR,.TRUE.)
!
      TDEB = TIME_IN_SECONDS()
!
!     HEADING
!
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE ARTEMIS ',78('-'))
101   FORMAT(/////,1X,'LISTING OF ARTEMIS ',78('-'))
102   FORMAT(/////,
     &14X,'    AAA  RRRR  TTTTT EEEEE M   M IIIII  SSSS',/,
     &14X,'   A   A R   R   T   E     MM MM   I   S    ',/,
     &14X,'   AAAAA RRRR    T   EEEEE M M M   I    SSS ',/,
     &14X,'   A   A R   R   T   E     M   M   I       S',/,
     &14X,'   A   A R   R   T   EEEEE M   M IIIII SSSS ',/,
     &14X,'                                            ',/,
     &14X,'          VERSION 7.0      FORTRAN 90 ',/,
     &14X,/////)
!
!-----------------------------------------------------------------------
!
!     READS THE STEERING FILE
!
      CALL LECDON_ARTEMIS(FILE_DESC,PATH,NCAR,CODE)
!-----------------------------------------------------------------------
!
!     OPENS THE FILES
!
      IFLOT = 0
      CALL BIEF_OPEN_FILES(CODE,ART_FILES,MAXLU_ART,PATH,NCAR,
     &                     .FALSE.,IFLOT,1,.FALSE.)
!-----------------------------------------------------------------------
!
!     ALLOCATES MEMORY FOR BIEF_OBJ STRUCTURES (VECTORS, MATRICES)
!
!
      CALL POINT_ARTEMIS
!
!-----------------------------------------------------------------------
!
!     CALLS REAL MAIN PROGRAM
!
      CALL ARTEMIS
!
!-----------------------------------------------------------------------
!
      CALL BIEF_CLOSE_FILES(CODE,ART_FILES,MAXLU_ART,.TRUE.)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
      TFIN = TIME_IN_SECONDS()
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DUREE DU CALCUL : ',TFIN-TDEB,' SECONDES'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
      ENDIF
!
!-----------------------------------------------------------------------
!
      STOP
      END
