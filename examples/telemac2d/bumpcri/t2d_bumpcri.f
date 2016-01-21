!                       *****************
                        SUBROUTINE CONDIN
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1         19/08/98  J-M HERVOUET TEL: 30 87 80 18
!
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                | -- |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
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
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DU TEMPS
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DES VITESSES : VITESSES NULLES
!
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
!
!-----------------------------------------------------------------------
!
!   INITIALISATION DE H , LA HAUTEUR D'EAU
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
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
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!  ZONE A MODIFIER
      CALL EXACTE(H%R,U%R,ZF%R,X,NPOIN)
!  FIN DE LA ZONE A MODIFIER
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
!   INITIALISATION DU TRACEUR
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P , C=TRAC0(ITRAC) )
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISATION DE LA VISCOSITE
!
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
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
!
      INTEGER LTT,N,IMAX
!
      DOUBLE PRECISION HHH,XMAX
      DOUBLE PRECISION, PARAMETER:: EPSS=1.E-10
      DOUBLE PRECISION GPRDTIME,LPRDTIME,RESTE
!
      INTRINSIC MAX,SQRT
!
!-----------------------------------------------------------------------
!
! LOGIQUES POUR DECIDER DES SORTIES
!
      IMP=.FALSE.
      LEO=.FALSE.
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
!      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
!        IMP=.FALSE.
!        LEO=.FALSE.
!      ENDIF
!     Always write the intial conditions
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
        CALL OS( 'X=YZ    ' , T4 , H , U , HHH )
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
! CALCUL DE LA VITESSE ET DE LA HAUTEUR EXACTE
!=======================================================================
!
      IF(((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))).AND.
     &   ((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24)))) THEN
        CALL EXACTE(PRIVE%ADR(1)%P%R,PRIVE%ADR(2)%P%R,ZF%R,X,NPOIN)
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE EXACTE
!=======================================================================
!
      IF((LEO.AND.SORLEO(25)).OR.(IMP.AND.SORIMP(25))) THEN
        CALL OV( 'X=Y+Z   ' ,PRIVE%ADR(3)%P%R,
     &                       PRIVE%ADR(1)%P%R,ZF%R,0.D0,NPOIN)
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE FROUDE EXACT
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
!                       ***************************
                        SUBROUTINE NOMVAR_TELEMAC2D
!                       ***************************
!
     &(TEXTE,TEXTPR,MNEMO,NPERIAF,NTRAC,NAMETRAC)
!
!***********************************************************************
!  TELEMAC 2D 7.0
!
!***********************************************************************
!
! FONCTION  :  FIXE LES NOMS DES VARIABLES DU CODE POUR LES FICHIERS
!              DE RESULTAT ET DE GEOMETRIE (TEXTE) ET POUR LE FICHIER
!              DE RESULTATS DU CALCUL PRECEDENT (TEXTPR)
!
!              EN GENERAL TEXTE ET TEXTPR SONT EGAUX SAUF SI ON FAIT
!              UNE SUITE A PARTIR D'UN AUTRE LOGICIEL.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE |
! |________________|____|______________________________________________|
! |   TEXTE        |<-- | NOM DES VARIABLES
! |   TEXTPR       |<-- | NOM DES VARIABLES DU CALCUL PRECEDENT
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : PREDON
!
! SOUS-PROGAMME APPELE : NEANT
!
!**********************************************************************
!
      USE DECLARATIONS_TELEMAC2D, ONLY : SECCURRENTS
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8),  INTENT(INOUT) :: MNEMO(*)
      INTEGER, INTENT(IN)              :: NPERIAF,NTRAC
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(32)
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
      TEXTE (26) = 'EXACT FROUDE                    '
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
      TEXTE (23) = 'HAUTEUR EXACTE  M               '
      TEXTE (24) = 'VITESSE EXACTE  M/S             '
      TEXTE (25) = 'SURFACE EXACTE  M               '
      TEXTE (26) = 'FROUDE EXACT                    '
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
!                       *****************
                        SUBROUTINE EXACTE
!                       *****************
!
     &(H,U,ZF,X,NPOIN)
!
!***********************************************************************
! PROGICIEL : 'TELEMAC'       12/12/88    J-M HERVOUET
!
!***********************************************************************
!
!      FONCTION:    SOLUTION EXACTE DE L'ECOULEMENT TRANSCRITIQUE
!                   SUR UN BUMP.
!
!                   PAR CONVENTION, ZF=0. AU POINT CRITIQUE
!
!                   ATTENTION, IL NE S'AGIT ICI QUE DE LA SOLUTION
!                   PERMANENTE, QUI EST TOUTEFOIS MISE DANS LE
!                   FICHIER DE RESULTATS A TOUS LES PAS DE TEMPS.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |     HN         |<-- |  HAUTEUR D'EAU.                              |
! |     U          |<-- |  VITESSE U.
! |     ZF         | -->|  COTE DU FOND.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!**********************************************************************
!
      IMPLICIT NONE
!
      INTEGER I,NPOIN
!
      DOUBLE PRECISION H(NPOIN),U(NPOIN),ZF(NPOIN),X(NPOIN)
      DOUBLE PRECISION Q,H0,A(4)
!
      EXTERNAL FC1
      DOUBLE PRECISION FC1
!
      COMMON/FORFC1/A
!
!-----------------------------------------------------------------------
!
! DEBIT ET HAUTEUR AU POINT CRITIQUE
!
      Q = 0.3D0
      H0 = ( Q**2/9.81D0 )**(1.D0/3.D0)
!
! EQUATION A RESOUDRE : A(1)*H**3 + A(2)*H**2 + A(3)*H + A(4)
!
      A(1) = 1.D0
      A(3) = 0.D0
      A(4) = H0**3.D0/2.D0
      DO I=1,NPOIN
!
        A(2) = ZF(I)-3.D0*H0/2.D0
!
        IF(X(I).GT.10.D0) THEN
!         ON PREND LA PLUS PETITE SOLUTION REELLE
          H(I) = H0
          CALL ZBRENT(FC1,1.D-3,0.1D0,H(I),100)
        ELSEIF(X(I).LT.10.D0) THEN
!         ON PREND LA PLUS GRANDE SOLUTION REELLE
          H(I) = 0.5D0
          CALL ZBRENT(FC1,1.D-3,H0,H(I),100)
        ELSE
!         POINT CRITIQUE
          H(I) = H0
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN
        U(I) = Q / MAX(H(I),1.D-8)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************************
                        DOUBLE PRECISION FUNCTION FC1
!                       *****************************
!
     &(X)
!
!***********************************************************************
! PROGICIEL : TELEMAC        07/12/88    J-M HERVOUET (LNH) 30 71 80 18
!
!***********************************************************************
!
!  FONCTION  : CALCULE UN POLYNOME DU TROISIEME DEGRE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X            | -->| ARGUMENT DE LA FONCTION.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!***********************************************************************
!
      IMPLICIT NONE
!
      DOUBLE PRECISION A(4),X
!
      COMMON/FORFC1/A
!
!-----------------------------------------------------------------------
!
      FC1 = A(1)*X**3 + A(2)*X**2 + A(3)*X + A(4)
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION A,B,C,D,E,X1,X2,FA,FB,FC,EPS,EPS2,XM,S,P,Q,R
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
      A=X1
      B=X2
      FA=FC1(A)
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
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
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
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
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
        A=B
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
