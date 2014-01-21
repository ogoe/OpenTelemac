!
!   ATTENTION : UN NUMERO EN DUR DANS PRERES_TELEMAC3D
!
!
!
!                       *****************
                        SUBROUTINE CONDIM
!                       *****************
!
!
!***********************************************************************
! TELEMAC 3D VERSION 5.1    11/12/00      J-M HERVOUET(LNH) 30 87 80 18
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : TELEMAC-3D
! SOUS-PROGRAMMES APPELES : OV , (CALCOT)
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!***********************************************************************
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU.
!
      IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR MISE A 0.5'
      IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS SET TO 0.5'
      CALL OS('X=C     ',H,H,H,0.5D0)
!
!  CLIPPING OF H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),HMIN)
      ENDDO
!
      CALL OS ('X=Y     ', HN, H, H, 0.D0)
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
!     ZSTAR%R(2)=0.02D0
!     ZSTAR%R(3)=0.1D0
!     ...
!     ZSTAR%R(NPLAN-1)=0.95D0
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
!     COMPUTATION OF ELEVATIONS
!     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION OF VELOCITIES
!
      CALL OS( 'X=C     ' , U , U , U , 1.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
      CALL OS( 'X=C     ' , W , W , W , 0.D0 )
!
!-----------------------------------------------------------------------
!
!     TRACERS INITIALIZATION
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
!   INITIALISATION DU MODELE K-EPSILON (FACULTATIF)
!   SI VOUS LE FAITES, INDIQUEZ AKEP = .FALSE.
!
      AKEP=.TRUE.
!
!     IF(ITURBV.EQ.3) THEN
!
!       HERE INITIALISE K AND EPSILON
!
!       AKEP = .FALSE.
!     ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
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
!                       *********************
                        SUBROUTINE T3D_CORFON
!                       *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC 3D VERSION 5.1    25/11/97      J.M. JANIN  (LNH) 30 87 72 84
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!  FONCTION  : CORRECTION DES FONDS RELEVES POUR TELEMAC-3D
!              (EQUIVALENT A CORFON DANS BIEF MAIS AVEC
!               DISTINCTION ENTRE DONNEES ET STRUCTURES)
!
!              EN STANDARD, CE SOUS-PROGRAMME UTILITAIRE NE FAIT
!              QUE DE LISSER LES FONDS AU PRORATA DU NOMBRE DE
!              LISSAGES FIXE DANS LE FICHIER DES PARAMETRES.
!
!              IL EST A LA DISPOSITION DES UTILISATEURS, POUR
!              LISSER SELECTIVEMENT OU CORRIGER DES FONDS SAISIS
!              PAR EXEMPLE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !  NOM           !MODE!                  ROLE                        !
! !________________!____!______________________________________________!
! !  (S)ZF         !<-->! FOND A MODIFIER.(SI S DEVANT : STRUCTURE)    !
! !  (S)T1,2       !<-->! TABLEAUX DE TRAVAIL (SI S DEVANT : STRUCTURE)!
! !  X,Y           ! -->! COORDONNEES DU MAILLAGE                      !
! !  PRIVE         ! -->! TABLEAU PRIVE POUR L'UTILISATEUR.            !
! !  NPOIN2        ! -->! NOMBRE DE POINTS DU MAILLAGE 2D.             !
! !  LISFON        ! -->! NOMBRE DE LISSAGES DU FOND.                  !
! !  MSK           ! -->! SI OUI, PRESENCE D'ELEMENTS MASQUES          !
! !  MASKEL        ! -->! MASQUAGE DES ELEMENTS                        !
! !  MATR          !<-->! MATRICE DE TRAVAIL                           !
! !  IMESH2        ! -->! BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D   !
! !  AMESH2        ! -->! BLOC DES TABLEAUX DE REELS DU MAILLAGE 2D    !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : MITRID
! SOUS-PROGRAMMES APPELES : FILTER
!
!***********************************************************************
!
      USE BIEF

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2 
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y 
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE 
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!------------------------------------------------------------------
!
      INTEGER K,I
      LOGICAL MAS
!
!***********************************************************************
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
         MAS = .TRUE.
!
         CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &               1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)

      ENDIF
!
!-----------------------------------------------------------------------
!
      DO I=1,NPOIN2
        ZF(I)=-1.01D-3*MESH2D%X%R(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END 
C                       ***************************
                        SUBROUTINE PRERES_TELEMAC3D
C                       ***************************
C
     *(LT)
C
C***********************************************************************
! TELEMAC 3D VERSION 5.1    07/09/00    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
C                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |      LT        | -->| NUMERO D'ITERATION
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : TELMAC
C
C  SOUS-PROGRAMME APPELE : OV
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     
      LOGICAL LEO
C
      INTEGER LTT,I,NODE,N
      DOUBLE PRECISION C
C
C-----------------------------------------------------------------------
C
C LOGIQUES POUR DECIDER DES SORTIES
C
      LEO=.FALSE.
      LTT=(LT/GRAPRD)*GRAPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.GRADEB) LEO=.TRUE.
C
C     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.LEO) GO TO 1000
C
C-----------------------------------------------------------------------
C
C VARIABLES POUR LES RESULTATS 2D
C
C-----------------------------------------------------------------------
C
C SPECIFIQUE PROFIL DE ROUSE
C
C ESSAI JMH
C
      NODE=GLOBAL_TO_LOCAL_POINT(46,MESH2D)
      IF(LT.EQ.NIT.AND.NODE.GT.0) THEN
      WRITE(LU,*)
     &  ' Z          U         C            NUT TRAC      NUT VIT '
      C=0.D0
      DO I=1,NPLAN
       WRITE(LU,*) MESH3D%Z%R(NODE+(I-1)*NPOIN2),U%R(NODE+(I-1)*NPOIN2), 
     &         TA%ADR(NTRAC)%P%R(NODE+(I-1)*NPOIN2),
     &         VISCTA%ADR(NTRAC)%P%ADR(3)%P%R(NODE+(I-1)*NPOIN2), 
     &         VISCVI%ADR(3)%P%R(NODE+(I-1)*NPOIN2) 
        C=C+TA%ADR(NTRAC)%P%R(NODE+(I-1)*NPOIN2)  
      ENDDO
       C=(C-0.5D0*(TA%ADR(NTRAC)%P%R(NODE)+
     &             TA%ADR(NTRAC)%P%R(NODE+(NPLAN-1)*NPOIN2)))/
     &             FLOAT(NPLAN-1)
       WRITE(LU,*) 'C VOLUMIQUE MOYEN EN SORTIE : ',C/2650.D0
      ENDIF
C
C FIN ESSAI JMH
C
C=======================================================================
C CALCUL DE LA CELERITE (= SQRT(GH) T2_10
C=======================================================================
C
      IF (LEO.AND.SORG2D(3)) THEN
         CALL OS( 'X=CY    ' , T2_10 , H     , H  , GRAV )
         CALL OS( 'X=SQR(Y)' , T2_10 , T2_10 , H  , GRAV )
      ENDIF
C
C=======================================================================
C CALCUL DE LA SURFACE LIBRE (= H + ZF) : T2_01
C=======================================================================
C
      IF (LEO.AND.SORG2D(5)) THEN
         CALL OS( 'X=Y+Z   ' , T2_01 , H  , ZF , 0.0D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU NOMBRE DE FROUDE T2_02
C=======================================================================
C
      IF (LEO.AND.SORG2D(7)) THEN
         CALL OS( 'X=YZ    ' , T2_02 , U2D , U2D , 0.0D0 )
         CALL OS( 'X=X+YZ  ' , T2_02 , V2D , V2D , 0.0D0 )
         CALL OS( 'X=CY/Z  ' , T2_02 , T2_02 , H  , 1.D0/GRAV )
         CALL OS( 'X=SQR(Y)' , T2_02 , T2_02 , ZF , 0.0D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT SCALAIRE T2_03
C=======================================================================
C
      IF (LEO.AND.SORG2D(8)) THEN
         CALL OS( 'X=N(Y,Z)' , T2_03 , U2D, V2D, 0.0D0 )
         CALL OS( 'X=XY    ' , T2_03 , H  , ZF , 0.0D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X T2_04
C=======================================================================
C
      IF (LEO.AND.SORG2D(13)) THEN
         CALL OS( 'X=YZ    ' , T2_04 , H , U2D , 0.0D0 )
      ENDIF
C
C=======================================================================
C CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y T2_05
C=======================================================================
C
      IF (LEO.AND.SORG2D(14)) THEN
         CALL OS( 'X=YZ    ' , T2_05 , H , V2D , 0.0D0 )
      ENDIF
C
C=======================================================================
C CALCUL DE LA VITESSE SCALAIRE T2_06
C=======================================================================
C
      IF (LEO.AND.SORG2D(15)) THEN
         CALL OS( 'X=N(Y,Z)' , T2_06 , U2D , V2D , 0.0D0 )
      ENDIF
C
C=======================================================================
C SEDIMENT RELATED VARIABLES THAT ARE ONLY KNOWN AT THE END OF THE
C FIRST TIME-STEP : SET HERE TO 0 IF LT=0
C=======================================================================
C
      IF(LEO.AND.SORG2D(24).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=EPAI)
      IF(LEO.AND.SORG2D(25).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=FLUER)
      IF(LEO.AND.SORG2D(26).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=PDEPO)
C
C=======================================================================
C CALCUL DE LA VITESSE DE FROTTEMENT
C=======================================================================
C
      IF(LEO.AND.SORG2D(31)) THEN
         CALL OS( 'X=SQR(Y)' , X=T2_07 , Y=UETCAR )
      ENDIF
C
C-----------------------------------------------------------------------
C
C VARIABLES POUR LES RESULTATS 3D
C
C-----------------------------------------------------------------------
C
      IF (NONHYD.AND.LEO.AND.SORG3D(12)) THEN 
         CALL PHSTAT(PH%R,DELTAR%R,Z,T3_01%R,T3_02%R,RHO0,GRAV,
     *               NPOIN3,NPOIN2,NPLAN,PRIVE)
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
      RETURN
      END
!                       *****************
                        SUBROUTINE CONDIS
!                       *****************
!
     &(IVIDE, EPAI  , TREST , CONC , TEMP   , HDEP   ,
     & ZR   , ZF    , X     , Y    , NPOIN2 , NPOIN3 ,
     & NPF  , NPFMAX, NCOUCH, TASSE, GIBSON , PRIVE  , CONSOL)
!
!***********************************************************************
! TELEMAC 3D VERSION 5.4      12/06/92      C LE NORMANT (LNH) 30 87 78 54
! FORTRAN95 VERSION         MARCH 1999        JACEK A. JANKOWSKI PINXIT
!***********************************************************************
!
!     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES (TABLEAUX)
!                 SEDIMENTOLOGIQUES
!
!-----------------------------------------------------------------------
!
!     FUNCTION  : INITIALIZATION OF SEDIMENT VARIABLES
!
!-----------------------------------------------------------------------
!                       ARGUMENTS
! .________________.____._______________________________________________
! |      NOM       |MODE|                 ROLE
! |________________|____|_______________________________________________
! |    IVIDE       |<-- | VOID RATIO
! |                |    |  (GIBSON MODEL ONLY)                         
! |    EPAI        |<-- | THICKNESS OF SOLID FRACTION oF THE BED LAYER
! |                |    | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
! |    TREST       |<-- | CONSOLIDATION TIME SCALE 
! |                |    |        (ONLY FOR MULTILAYER MODEL) 
! |    CONC        |<-- | CONCENTRATION OF MUD BED LAYER
! |                |    |         (MULTILAYER MODEL) 
! |    TEMP        |<-- |  TIME COUNTER FOR CONSOLIDATION MODEL
! |                |    |         (MULTILAYER MODEL) 
! |    HDEP        |<-- | THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
! |    ZR          |<-- | ELEVATION OF RIDIG BED
! |    ZF          | -->| BOTTOM ELEVATION
! |    X,Y         | -->| COORDINATES OF 2D MESH
! |    NPOIN2      | -->| NUMBER OF POINTS IN 2D
! |    NPOIN3      | -->| NUMBER OF POINTS IN 3D
! |    NPF         |<-- | NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL  
! |    NPFMAX      | -->| MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED       
! |                |    | 	 (GIBSON MODEL)
! |    NCOUCH      | -->| NUMBER OF LAYERS WITHIN THE BED
! |                |    |   	 (GIBSON MODEL)
! |    TASSE       | -->| MULTILAYER SETTLING MODEL
! |    GIBSON      | -->| GIBSON SETTLING MODEL
! |    PRIVE       | -->| BLOCK OF PRIVATE ARRAYS FOR USER
! |    NPRIV       | -->| NUMBER OR ARRAYS IN BLOCK PRIVE
! |________________|____|_______________________________________________
! MODE : -->(INPUT PARAMETER OR VARIABLE), <--(RESULT), 
!       <-->(MODIFIED PARAMETER OR VARIABLE)
!-----------------------------------------------------------------------
!**********************************************************************
!
      USE BIEF
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NPFMAX,NCOUCH
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      INTEGER, INTENT(INOUT)          :: NPF(NPOIN2)
      TYPE(BIEF_OBJ)                  :: PRIVE
      LOGICAL, INTENT(IN)             :: TASSE, GIBSON,CONSOL
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ECOUCH , TCAR
      INTEGER IPOIN, IC, IPF
!
      INTRINSIC LOG10
!
!=======================================================================
!
!     -----        INITIALIZATION OF HDEP               -----
!     -----      NOT USED BY THE MULTILAYER MODEL       -----
!
!     ICI PAS DE DEPOTS INITIAUX
!
      CALL OV( 'X=C     ' , HDEP , HDEP , HDEP , 0.D0 , NPOIN2)
!
!     -----    INITIALIZATION OF ZR    ---
!
      CALL OV( 'X=Y-Z   ' , ZR   , ZF   , HDEP , 0.D0 , NPOIN2)
!
!     -------------------------------------------------------
!           INITIAL CONDITIONS FOR THE MULTILAYER MODEL
!     -------------------------------------------------------
!
      IF (TASSE) THEN
!
!       -----  INITIALIZATION OF EPAI   ---
!
        CALL OV( 'X=C     ' , EPAI ,EPAI ,EPAI  , 0.D0 , NPOIN2*NCOUCH)
!
!       ----- INITIALIZATION OF CONC    -----
!
        CALL OV( 'X=C     ' , CONC , CONC  , CONC  , 0.D0 , NCOUCH)
!
!       DEFAULT OPTION:  
!             CONCENTRATIONS ARE CALCULATED AS A FUNCTION 
!             OF CONSOLIDATION TIME SCALE
!             (EMPIRICAL RELATION, LOIRE ESTUARY , Fristch et al. 1989)
! 
        TCAR = TREST(NCOUCH)/2.D0
        DO IC = NCOUCH , 1 , -1
          IF (IC.LT.NCOUCH) TCAR = TCAR + (TREST(IC+1)+TREST(IC))/2.D0
          IF (TCAR.LT.24.D0) THEN
            CONC(IC) = 136.2D0*LOG10(TCAR+5.424D0)
          ELSE
            CONC(IC) = 200.D0+70.D0*LOG10(TCAR/24.D0)
          ENDIF
        END DO
!
!       -----   HOURS CHANGED INTO SECONDS  ------
!
        CALL OV( 'X=CX    ' , TREST, TREST, TREST, 3600.D0, NCOUCH)
!
!       -----    INITIALIZATION OF TEMP    ------
!
        CALL OV( 'X=C     ' , TEMP, TEMP, TEMP, 0.D0 , NPOIN2*NCOUCH)
!
!       --------  MODIFYING ZR   --------
!
        DO IPOIN=1,NPOIN2
          DO IC=1,NCOUCH
            ZR(IPOIN)=ZR(IPOIN)-EPAI(IC,IPOIN)
          ENDDO
        ENDDO
!
!     ---------------------------------------
!     INITIAL CONDITIONS FOR THE GIBSON MODEL
!     ---------------------------------------
!
      ELSEIF (GIBSON) THEN
!
!       -----  INITIALIZATION OF NPF -----
!
        DO IPOIN=1,NPOIN2
          NPF(IPOIN)=0
        ENDDO
!
!       -----  INITIALIZATION OF IVIDE  ----
!
        CALL OV( 'X=C     ', IVIDE ,IVIDE , IVIDE, 0.D0, NPOIN2*NPFMAX)
!
!       -----  INITIALIZATION OF EPAI  -----
!
        CALL OV( 'X=C     ', EPAI, EPAI, EPAI, 0.D0, NPOIN2*(NPFMAX-1))
!
!       -----  MODIFICATION OF ZR -----
!
        DO IPOIN=1,NPOIN2
          DO IPF=1,NPF(IPOIN)-1
            ZR(IPOIN)=ZR(IPOIN)-EPAI(IPF,IPOIN)
            ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))*0.5D0
            EPAI(IPF,IPOIN)=EPAI(IPF,IPOIN)/(1.D0+ECOUCH)
          ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CONDIS


