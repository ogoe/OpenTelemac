C                       *****************
                        SUBROUTINE CORRXY
C                       *****************
     * (X,Y,NPOIN)
C
C***********************************************************************
C BIEF VERSION 5.6  17/10/05  EMILE RAZAFINDRAKOTO (LNHE) 01 30 87 74 03
C***********************************************************************
C
C  USER SUBROUTINE CORRXY
C
C  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
C
C
C              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
C              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
C              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
C              ALL THE DATA STRUCTURE OF THIS CODE IS
C              AVAILABLE
C
C              BEWARE : DO NOT DO ROTATIONS THAT WILL CHANGE THE
C                       NUMBERING OF LIQUID BOUNDARIES
C                       
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |    X,Y         | -->|  COORDONNEES DU MAILLAGE .                   |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
C APPELE PAR : INBIEF
C
C SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      USE BIEF, EX_CORRXY => CORRXY
C
C
C     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EVERY PROGRAM
C
C     USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER I
C
C-----------------------------------------------------------------------
C
C  EXAMPLE : MULTIPLICATION BY A CONSTANT (SCALING THE MESH)
C            CHANGING OF THE ORIGIN
C
       DO I = 1 , NPOIN
          X(I) = 4.D0 * X(I) 
       ENDDO
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'CORRXY (BIEF) : MODIFICATION DES COORDONNEES'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)'CORRXY (BIEF): MODIFICATION OF COORDINATES'
        WRITE(LU,*)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
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
      INTEGER IPLAN, I,J,ITRAC
      DOUBLE PRECISION DELTAZ,AL
!
!***********************************************************************
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU.
!
      IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR MISE A 10.'
      IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS SET TO 10.'
      CALL OS('X=C     ',X=H,C=10.D0)
!
!  CLIPPING OF H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),HMIN)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)

!-----------------------------------------------------------------------
!
!     INITIALISATION DE LA COTE DU PLAN INTERMEDIAIRE DE REFERENCE.
!     PAR DEFAUT, CE PLAN EST PLACE ENTRE FOND ET SURFACE AU PRORATA
!     DU PARAMETRE NPLINT.
!
      IF (NPLINT.GE.2) THEN
        CALL OV( 'X=C     ' , Z((NPLINT-1)*NPOIN2+1 : NPLINT*NPOIN2),
     *                Z, Z, COTINT , NPOIN2)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DE ZSTAR, LE RAPPORT ENTRE LA HAUTEUR D'EAU SOUS
!     UN PLAN QUASI HORIZONTAL ET LA HAUTEUR D'EAU TOTALE
!
! CAS SANS PLAN INTERMEDIAIRE DE REFERENCE
! ----------------------------------------
!
!         ON DOIT AVOIR :
!            * ZSTAR%R(1)     = 0.D0 ( PLAN DU FOND )
!            * ZSTAR%R(NPLAN) = 1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
! CAS AVEC PLAN INTERMEDIAIRE DE REFERENCE
! ----------------------------------------
!
!         ON DOIT AVOIR :
!            * ZSTAR%R(1)      = -1.D0 ( PLAN DU FOND )
!            * ZSTAR%R(NPLINT) =  0.D0 ( PLAN INTERMEDIAIRE DE REFERENCE
!            * ZSTAR%R(NPLAN)  =  1.D0 ( PLAN DE LA SURFACE LIBRE )
!         ET POUR TOUT I COMPRIS ENTRE 1 ET NPLAN-1
!            * ZSTAR%R(I) < ZSTAR%R(I+1)
!
!     PAR DEFAUT, LES PLANS QUASI HORIZONTAUX SONT REGULIEREMENT ESPACES
!
!***********************************************************************
!     POUR DONNER VOTRE PROPRE REPARTITION DES PLANS, MODIFIEZ LES
!     DEUX BOUCLES SUIVANTES
!     REMARQUE : NPLINT=1 QUAND IL N'Y A PAS DE PLAN INTERMEDIAIRE
!     ATTENTION : EN CAS DE TRANSFORMATION SIGMA GENERALISEE,
!     ---------   ZSTAR(2) A ZSTAR(NPLAN-1) DOIVENT ETRE MODIFIEES
!                 ET CONTENIR LA COTE DE POSITIONNEMENT DES DIFFERENTS
!                 PLANS DU MAILLAGE (IL VA DE SOIT QUE CELLES-CI DOIVENT
!                 ETRE DONNEES DANS UN ORDRE STRICTEMENT CROISSANT).
!***********************************************************************
!
      IF(NPLINT.GE.2) THEN
        DO IPLAN = 1,NPLINT-1
          ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLINT-1)
        ENDDO
      ENDIF
!
      DO IPLAN = NPLINT,NPLAN
        ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLAN-NPLINT)
      ENDDO
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
      CALL OS( 'X=C     ' , X=U,C=1.D0 )
      CALL OS( 'X=0     ' , X=V )
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
!     TRACERS INITIALIZATION
!
      IF(NTRAC.NE.0) THEN
        DO ITRAC=1,NTRAC
!         CALL OS('X=C     ',X=TA%ADR(ITRAC)%P,C=TRAC0(ITRAC))
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              J=NPOIN2*(IPLAN-1)+I
              IF(IPLAN.GT.18) THEN
                TA%ADR(ITRAC)%P%R(J)=28.D0
              ELSE
                TA%ADR(ITRAC)%P%R(J)=38.D0
              ENDIF     
            ENDDO
          ENDDO
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
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
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
        ZF(I)=-9.473D-6*MESH2D%X%R(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CORFON
!                       *****************
                        SUBROUTINE BORD3D
!                       ***************** 
!
!   ICI STRATIFICATION DE TRACEUR MISE EN ENTREE
!
!
!
     & (TIME,LT,ENTET,NPTFR2_DIM,NFRLIQ)
! 
!*********************************************************************** 
! TELEMAC 3D VERSION 5.7 07/08/2006 J.-M. HERVOUET (LNHE) 01 30 87 80 18
!         
!*********************************************************************** 
! 
!      FONCTION: 
!      ========= 
! 
!      CONDITIONS AUX LIMITES SPECIFIQUES. PEUT ETRE MODIFIE PAR
!      L'UTILISATEUR 
!
!-----------------------------------------------------------------------
! 
!      FUNCTION: 
!      ========= 
! 
!      SPECIFIC BOUNDARY CONDITIONS, MAY BE MODIFIED BY THE USER. 
!
!----------------------------------------------------------------------- 
!                          SOME USEFUL PARAMETERS
! .________________.____.______________________________________________. 
! !  NOM           !MODE!                  ROLE                        ! 
! !________________!____!______________________________________________! 
! !  UBORF         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
! !  UBORL         !<-- ! PRESCRIBED VELOCITY ALONG X ON THE LATERAL
! !                !    ! BOUNDARY
! !  UBORS         !<-- ! PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
! !  VBORF         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
! !  VBORL         !<-- ! PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
! !                !    ! BOUNDARY
! !  VBORS         !<-- ! PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
! !  WBORF         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
! !  WBORL         !<-- ! PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
! !                !    ! BOUNDARY
! !  WBORS         !<-- ! PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
! !  TABORF        !<-- ! PRESCRIBED TRACERS ON THE BOTTOM
! !  TABORL        !<-- ! PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
! !  TABORS        !<-- ! PRESCRIBED TRACERS AT FREE SURFACE
! !                !    !
! !                !    ! LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
! !                !    !                    FOR VELOCITIES
! !                !    !                    ATABO,L,S AND BTABO,L,S
! !                !    !                    FOR TRACERS
! !                !    ! 
! ! AUBOR,BUBOR    !<-- ! LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
! !                !    ! IF BUBOR(F,L,S) ADDED HERE, SPECIFY BUBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! ! AUBOR,BVBOR    !<-- ! LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
! !                !    ! IF BVBOR(F,L,S) ADDED HERE, SPECIFY BVBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! ! AWBOR,BWBOR    !<-- ! LOG LAW: NU*DW/DN = AWBOR*W + BWBOR
! !                !    ! IF BWBOR(F,L,S) ADDED HERE, SPECIFY BWBOR%TYPR='Q'
! !                !    ! SEE END OF THE ROUTINE
! !                !    ! 
! ! ATABO,BTABO    !<-- ! LOG LAW: NU*DTA/DN = ATABO*TA + BTABO 
! !                !    !
! !                !    ! TYPES OF BOUNDARY CONDITIONS
! !                !    !                    
! !  LIU,V,WBOF    !<-->! ON BOTTOM FOR U,V,W
! !  LIU,V,WBOL    !<-->! ON LATERAL BOUNDARIES FOR U,V,W
! !  LIU,V,WBOS    !<-->! AT FREE SURFACE FOR U,V,W
! !  LITA,BF       !<-->! ON BOTTOM FOR TRACERS
! !  LITA,BL       !<-->! ON LATERAL BOUNDARIES FOR TRACERS
! !  LITA,BS       !<-->! AT FREE SURFACE FOR TRACERS
! !                !    !
! !  U,V,W         ! -->! COMPONENTS OF VELOCITY
! !  UMOY,VMOY     ! -->! DEPTH AVERAGED VELOCITY
! !  TA            ! -->! CONCENTRATION OF TRACERS
! !  ITURBV        ! -->! TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
! !  VENT          ! -->! WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
! !  FAIR          ! -->! DRAG COEFFICIENT OF WIND
! !  VENTX         ! -->! WIND VELOCITY ALONG X
! !  VENTY         ! -->! WIND VELOCITY ALONG Y
! !  AT            ! -->! TIME
! !  LT            ! -->! CURRENT NUMBER OF TIME-STEP
! !  DT            ! -->! TIME-STEP
! !  LIHBOR        ! -->! TYPE OF BOUNDARY CONDITIONS ON DEPTH
! !  HBOR          ! -->! PRESCRIBED DEPTH ON LATERAL BOUNDARIES
! !  HN            ! -->! DEPTH AT TIME TN
! !  X,Y,Z         ! -->! MESH COORDINATES
! !  ZF            ! -->! BOTTOM ELEVATION
! !  NBOR          ! -->! GLOBAL NUMBER OF 2D BOUNDARY POINTS
! !  NELEM3        ! -->! NUMBER OF 3D ELEMENTS
! !  IKLE3         ! -->! CONNECTIVITY TABLE IN 3D
! !  KP1BOR        ! -->! IN 2D : NEXT POINT ON THE BOUNDARY
! !                !    ! (BOUNDARY NUMBERING)
! ! XSGBOR,YSGBOR  ! -->! 2D NORMAL VECTORS TO THE SEGMENTS
! !  NPOIN3        ! -->! NUMBER OF POINTS IN 3D
! !  NPOIN2        ! -->! NUMBER OF POINTS IN 2D
! !  NETAGE        ! -->! NUMBER OF LAYERS OF 3D ELEMENTS
! !  NPTFR         ! -->! NUMBER OF BOUNDARY POINTS IN 2D
! !  NPTFR3        ! -->! NUMBER OF BOUNDARY POINTS IN 3D
! !  NPLAN         ! -->! NUMBER OF PLANES ON THE VERTICAL
! !  NELEM2        ! -->! NUMBER OF ELEMENTS IN 2D
! !                !    ! 
! !                !    ! POSSIBLE TYPES OF BOUNDARY CONDITIONS
! !                !    ! 
! !  KENT          ! -->! PRESCRIBED VALUE
! !  KENTU         ! -->! PRESCRIBED VELOCITY
! !  KSORT         ! -->! FREE (E.G. AT AN OUTPUT)
! !  KADH          ! -->! NO SLIP CONDITION
! !  KLOG          ! -->! SOLID BOUNDARY
! !  NTRAC         ! -->! NUMBER OF TRACERS
! !  SEDI          ! -->! IF YES, THERE IS SEDIMENT
! !  PRIVE         ! -->! BLOCK OF ARRAYS FOR THE USER 
! !  NPRIV         ! -->! NUMBER OF ARRAYS IN BLOCK PRIVE
! !  NDEBIT        ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
! !  NVIT          ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
! !  NCOTE         ! -->! NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
! !  DEBIMP        ! -->! ARRAY OF PRESCRIBED DISCHARGES
! !  COTIMP        ! -->! ARRAY OF PRESCRIBED ELEVATIONS
! !  VITIMP        ! -->! ARRAY OF PRESCRIBED VELOCITIES
! !________________!____!______________________________________________! 
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
! 
!*********************************************************************** 
!                                                                       
! INFORMATION : FOR PRESCRIBED BOUNDARIES OF POINT BEING BOTH LATERAL
!               AND BOTTOM, USE LATERAL ARRAYS. 
!
!               FOR TYPES OF BOUNDARY CONDITIONS : USE SUBROUTINE LIMI3D
!
!               SEDIMENT IS THE LAST TRACER              
!                                                                      
!*********************************************************************** 
! 
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC3D, EX_BORD3D => BORD3D
!
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)    :: TIME
      INTEGER         , INTENT(IN)    :: LT
      LOGICAL         , INTENT(IN)    :: ENTET
      INTEGER         , INTENT(IN)    :: NPTFR2_DIM
      INTEGER         , INTENT(INOUT) :: NFRLIQ
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IPOIN2,NP,K1,IBORD,IVIT,ICOT,IDEB,IFRLIQ,IPROF
      INTEGER IPTFR,ITRAC,IPLAN
      DOUBLE PRECISION ROEAU,ROAIR,VITV,PROFZ      
!

      INTEGER K
      INTEGER N,ERR
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
!
!--------------------------------------------------------------
!
      DOUBLE PRECISION XB,YB,ZB,NORM,CP
      DOUBLE PRECISION RO0,A,B,SAL,WW,TREEL,RO,LAMB
!
      INTEGER YADEB(100),MSK1,IPTFR2,I2,IJK,ITEMP
!
!     SIMPLE CASES FOR LATERAL BOUNDARIES ARE TREATED AUTOMATICALLY:
!
!     - PRESCRIBED DEPTH     (5 4 4)
!     - PRESCRIBED VELOCITY  (  6 6)
!     - PRESCRIBED DISCHARGE (  5 5)
!
!     CORRESPONDING KEY-WORDS ARE:
!
!     'PRESCRIBED ELEVATIONS' OR 'COTES IMPOSEES'
!     'PRESCRIBED VELOCITIES' OR 'VITESSES IMPOSEES'
!     'PRESCRIBED FLOWRATES' OR 'DEBITS IMPOSES'
!
!     THE IMPLEMENTATION OF AUTOMATIC CASES MAY BE CANCELLED
!     PROVIDED THAT THE  RELEVANT ARRAYS ARE FILLED
!
!
!***********************************************************************
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!              AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!=======================================================================
! 
!     SECURING NO SLIP BOUNDARY CONDITIONS
!
      IF(LT.EQ.1) THEN
!
!     VELOCITIES
!
      DO IPTFR = 1,NPTFR2
        IPOIN2 = NBOR2%I(IPTFR)
        DO IPLAN = 1,NPLAN
          IBORD = (IPLAN-1)*NPTFR2 + IPTFR
          IF(LIUBOL%I(IBORD).EQ.KADH) UBORL%R(IBORD) = 0.D0
          IF(LIVBOL%I(IBORD).EQ.KADH) VBORL%R(IBORD) = 0.D0
          IF(LIWBOL%I(IBORD).EQ.KADH) WBORL%R(IBORD) = 0.D0
        ENDDO
      ENDDO
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF%I(IPOIN2).EQ.KADH) UBORF%R(IPOIN2) = 0.D0
        IF(LIVBOF%I(IPOIN2).EQ.KADH) VBORF%R(IPOIN2) = 0.D0
        IF(LIWBOF%I(IPOIN2).EQ.KADH) WBORF%R(IPOIN2) = 0.D0
        IF(LIUBOS%I(IPOIN2).EQ.KADH) UBORS%R(IPOIN2) = 0.D0
        IF(LIVBOS%I(IPOIN2).EQ.KADH) VBORS%R(IPOIN2) = 0.D0
        IF(LIWBOS%I(IPOIN2).EQ.KADH) WBORS%R(IPOIN2) = 0.D0
      ENDDO
!
!     IMPORTANT OPTION:
!     VERTICAL VELOCITIES SET AS HORIZONTAL VELOCITIES 
!     THIS IS AN OPTION, OTHERWISE LIWBOL=KSORT (SEE LIMI3D)
!
C     DO IPTFR = 1,NPTFR2
C       IPOIN2 = NBOR2%I(IPTFR)
C       DO IPLAN = 1,NPLAN
C         IBORD = (IPLAN-1)*NPTFR2 + IPTFR  
C         LIWBOL%I(IBORD)= LIUBOL%I(IBORD)
C         IF(LIWBOL%I(IBORD).EQ.KENT) WBORL%R(IBORD) = 0.D0
C       ENDDO
C     ENDDO
!
!     TRACERS 
!
!     IF(NTRAC.NE.0) THEN
!
!       DO ITRAC = 1,NTRAC
!
!         DO IPTFR = 1,NPTFR2
!           IPOIN2 = NBOR2%I(IPTFR)
!           LITABF%ADR(ITRAC)%P%I(IPOIN2) = KSORT (DOES NOT WORK WITH SEDIMENT)
!           LITABS%ADR(ITRAC)%P%I(IPOIN2) = KSORT
!           DO IPLAN = 1,NPLAN
!             IBORD = (IPLAN-1)*NPTFR2 + IPTFR
!             IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KADH)
!    &           TABORL%ADR(ITRAC)%P%R(IBORD) = 0.D0
!           ENDDO
!         ENDDO
!
!         DO IPOIN2 = 1,NPOIN2
!           IF(LITABF%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!           IF(LITABS%ADR(ITRAC)%P%I(IPOIN2).EQ.KADH)
!    &                       TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
!         ENDDO
!
!       ENDDO
!
!     ENDIF
!
      ENDIF
!
!=======================================================================
!
!
!  FOR ALL TIME STEPS
!
!     INITIALISING YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
!
      IDEB=0
      ICOT=0
      IVIT=0
!
!     LOOP ON ALL 2D BOUNDARY POINTS
!
      DO 5 K=1,NPTFR2
!
!     PRESCRIBED ELEVATION GIVEN IN PARAMETER FILE (NCOTE<>0)
!     -------------------------------------------------------
!
      IF(LIHBOR%I(K).EQ.KENT.AND.NCOTE.NE.0) THEN
!
        IF(NCOTE.GE.NUMLIQ%I(K)) THEN
!
        ICOT=NUMLIQ%I(K)
        HBOR%R(K) = SL3(ICOT,AT,NBOR2%I(K),INFOGR)-ZF%R(NBOR2%I(K))
        HBOR%R(K) = MAX(0.D0,HBOR%R(K))
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) NUMLIQ%I(K)
100       FORMAT(1X,'BORD3D : COTES IMPOSEES EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,101) NUMLIQ%I(K)
101       FORMAT(1X,'BORD3D : MORE PRESCRIBED ELEVATIONS ARE REQUIRED',/,
     *           1X,'       IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!     PRESCRIBED DISCHARGE GIVEN IN PARAMETER FILE (NDEBIT<>0)
!     --------------------------------------------------------
!
!     A VELOCITY PROFILE IS SET HERE AND WILL BE CORRECTED AFTER
!     TO GET THE CORRECT DISCHARGE (CALL TO DEBIMP3D)
!
      IF(LIUBOL%I(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
!
        DO NP=1,NPLAN
          IJK=(NP-1)*NPTFR2+K
          IFRLIQ=NUMLIQ%I(K)
          IF(PROFVEL(IFRLIQ).EQ.2) THEN
C           GIVEN BY USER IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = VBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.3) THEN
C           NORMAL AND NORM GIVEN BY UBOR IN BOUNDARY CONDITIONS FILE
            UBORL%R(IJK) = -XNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
            VBORL%R(IJK) = -YNEBOR2%R(K)*UBOR2D%R(K+NPTFR2)
          ELSEIF(PROFVEL(IFRLIQ).EQ.4) THEN
C           NORMAL AND PROPORTIONAL TO SQRT(H)
            UBORL%R(IJK)=-XNEBOR2%R(K) * SQRT(MAX(H%R(NBOR2%I(K)),0.D0))
            VBORL%R(IJK)=-YNEBOR2%R(K) * SQRT(MAX(H%R(NBOR2%I(K)),0.D0))
          ELSE
C           NORMAL AND NORM 1
            UBORL%R(IJK)=-XNEBOR2%R(K)
            VBORL%R(IJK)=-YNEBOR2%R(K)
          ENDIF
!         NO VELOCITY IF NO WATER
          IF(H%R(NBOR2%I(K)).LT.1.D-4) THEN
            UBORL%R(IJK) = 0.D0
            VBORL%R(IJK) = 0.D0
          ENDIF
!         CASE OF A VERTICAL PROFILE
          IF(VERPROVEL(IFRLIQ).NE.1) THEN
            PROFZ=VEL_PROF_Z(IFRLIQ,NBOR2%I(K),
     *                       AT,LT,NP,INFOGR,VERPROVEL(IFRLIQ))        
            UBORL%R(IJK) = UBORL%R(IJK)*PROFZ
            VBORL%R(IJK) = VBORL%R(IJK)*PROFZ
          ENDIF
!         U AND V INITIALISED WITH PRESCRIBED VALUES (FOR DEBIMP3D)
!         WILL BE CHANGED AGAIN AFTER DEBIMP3D
          U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
          V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)        
        ENDDO
!    
        YADEB(NUMLIQ%I(K))=1
!
      ENDIF
!        
!     PRESCRIBED VELOCITY GIVEN IN PARAMETER FILE (NVIT<>0)
!     -----------------------------------------------------
!
!     THIS VELOCITY IS CONSIDERED NORMAL TO THE BOUNDARY
!
      IF(LIUBOL%I(K).EQ.KENTU.AND.NVIT.NE.0) THEN
        IVIT=NUMLIQ%I(K)
        IF(NVIT.GE.IVIT) THEN
!
             DO NP=1,NPLAN
               IBORD = (NP-1)*NPTFR2+K
               UBORL%R(IBORD) =
     *         -MESH2D%XNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
               VBORL%R(IBORD) =
     *         -MESH2D%YNEBOR%R(K)*VIT3(IVIT,AT,NBOR2%I(K),INFOGR)
               WBORL%R(IBORD)=0.D0
             END DO
!
        ELSE
          IF(LNG.EQ.1) WRITE(LU,200) NUMLIQ%I(K)
200       FORMAT(1X,'BORD3D : VITESSES IMPOSEES EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,201) NUMLIQ%I(K)
201       FORMAT(1X,'BORD3D : MORE PRESCRIBED VELOCITIES ARE REQUIRED',/,
     *           1X,'       IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!     PRESCRIBED TRACER GIVEN IN PARAMETER FILE, BUT POSSIBLE
!     OVERWRITING IF LIQUID BOUNDARY FILE IS GIVEN
!     SEE FUNCTION TR3
!     -------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
        DO NP=1,NPLAN
          IBORD = (NP-1)*NPTFR2+K 
          IFRLIQ=NUMLIQ%I(K)
          IF(LITABL%ADR(ITRAC)%P%I(IBORD).EQ.KENT.AND.NTRACER.NE.0) THEN
            IFRLIQ=NUMLIQ%I(K)            
            IF(NTRACER.GE.IFRLIQ*NTRAC) THEN                             
              TABORL%ADR(ITRAC)%P%R(IBORD) = 
     *        TR3(IFRLIQ,ITRAC,NBOR3%I(IBORD),AT,INFOGR)
            ELSE
              IF(LNG.EQ.1) WRITE(LU,300) NUMLIQ%I(K)*NTRAC
300           FORMAT(1X,'BORD3D : VALEURS IMPOSEES DU TRACEUR',/,
     *               1X,'         EN NOMBRE INSUFFISANT',/,
     *               1X,'         DANS LE FICHIER DES PARAMETRES',/,
     *               1X,'         IL EN FAUT AU MOINS : ',1I6)
              IF(LNG.EQ.2) WRITE(LU,301) NUMLIQ%I(K)
301           FORMAT(1X,'BORD3D: MORE PRESCRIBED TRACER VALUES',/,
     *               1X,'        ARE REQUIRED IN THE PARAMETER FILE',/,
     *               1X,'        AT LEAST ',1I6,' MUST BE GIVEN')
              CALL PLANTE(1)
              STOP
            ENDIF
C           CASE OF A PROFILE ON THE VERTICAL
            IPROF=VERPROTRA(ITRAC+(IFRLIQ-1)*NTRAC)
            IF(IPROF.NE.1) THEN
              PROFZ=TRA_PROF_Z(IFRLIQ,NBOR2%I(K),
     *                         AT,LT,NP,INFOGR,IPROF,ITRAC)
              IF(IPROF.EQ.2.OR.IPROF.EQ.3) THEN
                TABORL%ADR(ITRAC)%P%R(IBORD)=PROFZ
              ELSE     
                TABORL%ADR(ITRAC)%P%R(IBORD)=
     *          TABORL%ADR(ITRAC)%P%R(IBORD)*PROFZ
              ENDIF
            ENDIF
!
!   STRATIFICATION MISE EN ENTREE
!
            IF(NP.GT.18.AND.ITRAC.EQ.1) THEN
              TABORL%ADR(ITRAC)%P%R(IBORD)=28.D0
            ENDIF
!
!
          ENDIF
C          
        ENDDO
        ENDDO
      ENDIF
!
5      CONTINUE
!        
!     PRESCRIBED DISCHARGES: FINAL TREATMENT OF VELOCITIES
!     ----------------------------------------------------
!
!     LOOP ON LIQUID BOUNDARIES
!
      IF(NFRLIQ.NE.0) THEN
      DO 10 IFRLIQ = 1 , NFRLIQ
!
      IF(NDEBIT.NE.0) THEN
!
        MSK1=1
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
           IF(YADEB(IFRLIQ).EQ.1) THEN 
           CALL DEBIMP3D(Q3(IFRLIQ,AT,INFOGR),
     &                   UBORL%R,VBORL%R,WBORL%R,
     &                   U,V,H,NUMLIQ%I,IFRLIQ,T3_01,T3_02,T3_03,
     &                   NPTFR2,NETAGE,MASK%ADR(MSK1)%P%R,
     &                   MESH3D,EQUA,NPOIN2,
     &                   IELM2V,SIGMAG,SVIDE,MASKBR,ZPROP)     
           ENDIF
          ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD3D : DEBITS IMPOSES',/,
     *           1X,'       EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD3D : MORE PRESCRIBED FLOWRATES',/,
     *           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF

      ENDIF
!
10    CONTINUE
      ENDIF
!
!     RESETTING BOUNDARY CONDITIONS ON U AND V (WILL BE USED BY TFOND
!     AND OTHER SUBROUTINES BEFORE THE NEXT BOUNDARY CONDITIONS TREATMENT)
!
      DO K=1,NPTFR2
        IF(LIUBOL%I(K).EQ.KENT) THEN
          DO NP=1,NPLAN
            IJK=(NP-1)*NPTFR2+K
            U%R((NP-1)*NPOIN2+NBOR2%I(K))=UBORL%R(IJK)
            V%R((NP-1)*NPOIN2+NBOR2%I(K))=VBORL%R(IJK)        
          ENDDO
        ENDIF
      ENDDO
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!           END OF AUTOMATIC TREATMENT OF LIQUID BOUNDARIES
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                               WIND
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
      IF(VENT) THEN 
         ROEAU = 1000.D0 
         ROAIR = 1.3D0 
         DO IPOIN2 = 1,NPOIN2 
            VITV  = SQRT(WIND%ADR(1)%P%R(IPOIN2)**2
     &                 + WIND%ADR(2)%P%R(IPOIN2)**2) 
! 
! A MORE ACCURATE TREATMENT
! 
!CX         IF(VITV.LE.5.D0) THEN 
!CX           FAIR = ROAIR/ROEAU*0.565D-3 
!CX         ELSEIF (VITV.LE.19.22D0) THEN 
!CX           FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3 
!CX         ELSE 
!CX           FAIR = ROAIR/ROEAU*2.513D-3 
!CX         ENDIF 
! 
! BEWARE : BUBORS IS VISCVI*DU/DN, NOT DU/DN
! 
            BUBORS%R(IPOIN2) = FAIR*VITV*WIND%ADR(1)%P%R(IPOIN2) 
            BVBORS%R(IPOIN2) = FAIR*VITV*WIND%ADR(2)%P%R(IPOIN2) 
         ENDDO
      ENDIF
!
! 
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                         END OF WIND TREATMENT
!           +++++++++++++++++++++++++++++++++++++++++++++++ 
!
!
!
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                     HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!                 LINES BELOW WITH '!C' ARE AN EXAMPLE
! 
!    TO BE GIVEN :
! 
!    ITEMP = NUMBER OF TRACER WHICH IS THE HEAT 
!    TAIR  = CONSTANT AIR TEMPERATURE
!    SAL   = CONSTANT WATER SALINITY 
! 
!C    ITEMP=1 
!C    CP=4.18D3 
!C    RO0=999.972D0 
!C    B=0.0025D0 
!C    TAIR=15.D0 
!C    SAL=35.D-3 
!C    WW=0.D0 
!C    IF (VENT) WW=VITV 
!C    DO IPOIN2=1,NPOIN2 
!C       TREEL=TA%ADR(ITEMP)%P%R(NPOIN3-NPOIN2+IPOIN2) 
!C       RO=RO0*(1.D0-(7.D0*(TREEL-4.D0)*(TREEL-4.D0)-750.D0*SAL)*1D-6) 
!C       LAMB=RO*CP 
!C       A=(4.48D0+0.049D0*TREEL)+2021.5D0*B*(1.D0+WW)* 
!C   &     (1.12D0+0.018D0*TREEL+0.00158D0*TREEL*TREEL) 
!C       ATABOS%ADR(ITEMP)%P%R(IPOIN2)=-A/LAMB 
!C       BTABOS%ADR(ITEMP)%P%R(IPOIN2)= A*TAIR/LAMB
!C    ENDDO
!     IMPORTANT:
!     STATING THAT ATABOS AND BTABOS ARE NOT ZERO (SEE LIMI3D AND DIFF3D)
!     OTHERWISE THEY WILL NOT BE CONSIDERED
!C    ATABOS%ADR(ITEMP)%P%TYPR='Q'
!C    BTABOS%ADR(ITEMP)%P%TYPR='Q'
! 
!
!           +++++++++++++++++++++++++++++++++++++++++++++++
!                 END OF HEAT EXCHANGE WITH ATMOSPHERE
!           +++++++++++++++++++++++++++++++++++++++++++++++
!
!
!
!----------------------------------------------------------------------- 
!
!     OPTIMIZATION:
!
!     EXPLICIT STRESSES WILL NOT BE TREATED IF SAID TO BE ZERO
!
!     EXPLICIT STRESSES SET TO 0 ON VELOCITIES (UNLESS PROGRAMMED
!                                               IN THIS SUBROUTINE):
!
      BUBORF%TYPR='0'
      BUBORL%TYPR='0'      
      BVBORF%TYPR='0'
      BVBORL%TYPR='0'      
      BWBORF%TYPR='0'
      BWBORL%TYPR='0'
      BWBORS%TYPR='0'
!
!     CASE OF WIND (SEE ABOVE)
!
      IF(VENT) THEN
        BUBORS%TYPR='Q'
        BVBORS%TYPR='Q'
      ELSE
        BUBORS%TYPR='0'
        BVBORS%TYPR='0'
      ENDIF
!
!----------------------------------------------------------------------- 
! 
      RETURN
      END
