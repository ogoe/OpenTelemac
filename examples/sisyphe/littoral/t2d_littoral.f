!                    **************************
                     SUBROUTINE BEDLOAD_SOULSBY 
!                    **************************
!
     &  (UCMOY,HN, UW, NPOIN, DENS, GRAV, DM, DSTAR, HMIN, D90, QSC,
     &   QSS)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    SOULSBY & VAN RIJN BEDLOAD TRANSPORT FORMULATION.
!
!history  SOGREAH
!+        22/05/2001
!+        V5P2
!+
!
!history  C.VILLARET
!+        **/11/2003
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| D90            |-->| D90
!| DENS           |-->| RELATIVE DENSITY
!| DM             |-->| SEDIMENT GRAIN DIAMETER
!| DSTAR          |-->| NON-DIMENSIONAL DIAMETER
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| NPOIN          |-->| NUMBER OF POINTS
!| QSC            |<->| BED LOAD TRANSPORT 
!| QSS            |<->| SUSPENDED LOAD TRANSPORT RATE
!| UCMOY          |-->| CURRENT INTENSITY (M/S)
!| UW             |-->| ORBITAL WAVE VELOCITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_SOULSBY => BEDLOAD_SOULSBY
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)  :: HN, UCMOY, UW
      INTEGER,          INTENT(IN)  :: NPOIN
      DOUBLE PRECISION, INTENT(IN)  :: DENS, GRAV, DM, DSTAR, HMIN, D90
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: COEF, ASS, ASB, CD
      DOUBLE PRECISION            :: UCR, VTOT, TRA
      DOUBLE PRECISION, PARAMETER :: Z0=0.006D0
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ************************* !
      ! I - SUSPENSION COEFFCIENT !
      ! ************************* !
      COEF = (DENS *GRAV*DM)**1.2D0
      ASS  = 0.012D0*DM*(DSTAR**(-0.6D0))/COEF
!
      DO I = 1, NPOIN
!
         ! *************************** !
         ! III - BEDLOAD COEFFICIENT   !
         ! *************************** !
         ASB = 0.005D0*HN%R(I)*(DM/MAX(HN%R(I),DM))**1.2D0 / COEF
!
         ! ********************************** !
         ! IV - ROUGHNESS COEFFICIENT CD      !
         !      SOULSBY: Z0=0.006 --> KS=18CM !
         ! ********************************** !
         CD = (0.4D0 / (LOG(MAX(HN%R(I),Z0)/Z0)-1.D0))**2
!
         ! ************************************************ !
         ! V - CRTITICAL CURRENT SPEED UCR                  !
         ! ************************************************ !
         IF (DM < 0.0005D0) THEN
            UCR = 0.19D0*(DM**0.1D0)*LOG10(4.D0*MAX(HN%R(I),D90)/D90)
         ELSE
            UCR = 8.50D0*(DM**0.6D0)*LOG10(4.D0*MAX(HN%R(I),D90)/D90)
         ENDIF
!
         ! ************************************************* !
         ! VI - SPEED INDUCED BY THE CURRENT AND WAVES       !
         ! ************************************************* !
         VTOT = SQRT(UCMOY%R(I)**2+(0.018D0/CD)*UW%R(I)**2)
!
         ! *********************************************** !
         ! VII - SUSPENDED AND BEDLOAD TRANSPORT           !
         ! *********************************************** !
         IF (VTOT > UCR) THEN
            TRA     = UCMOY%R(I)  * (VTOT - UCR )**2.4D0
            QSS%R(I)= ASS * TRA
            QSC%R(I)= ASB * TRA
         ELSE
            QSS%R(I) = 0.D0
            QSC%R(I) = 0.D0
         ENDIF
      ENDDO
!======================================================================!
!======================================================================!
      RETURN
      END SUBROUTINE BEDLOAD_SOULSBY


C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER I
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C-----------------------------------------------------------------------
      DO I=1,NPOIN
          ZF%R(I)= MIN(0.05D0*Y(I)-10.D0,-0.10D0)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  
C                       ***************
                        SUBROUTINE BORD
C                       ***************
C
     *(HBOR,UBOR,VBOR,TBOR,U,V,H,
     * ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     * XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2, TEMPS,
     * NDEBIT,NCOTE,NVITES,
     * NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     * NOMIMP)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.8    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    MODIFIE LES TABLEAUX DE CONDITIONS AUX LIMITES
C                   DANS LE CAS OU ELLES SONT VARIABLES EN TEMPS.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
C      SOIT DIRECTEMENT, SOIT PAR L'INTERMEDIAIRE DES FONCTIONS :
C
C             Q , SL , TR , VIT
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   HBOR         |<-- |  HAUTEUR IMPOSEE.                            |
C |   UBOR         |<-- |  VITESSE U IMPOSEE.                          |
C |   VBOR         |<-- |  VITESSE V IMPOSEE.                          |
C |   TBOR         |<-- |  TRACEUR IMPOSE AU BORD                      |
C |    U,V         | -->|  COMPOSANTES DE LA VITESSE AU TEMPS N        |
C |    H           | -->|  HAUTEUR AU TEMPS N                          |
C |    ZF          | -->|  FOND                                        |
C |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
C |  TRA05,TRA06   | -->|  TABLEAUX DE TRAVAIL                         |
C |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
C | LIUBOR         | -->|  CONDITIONS AUX LIMITES SUR U 
C |   LITBOR       | -->|  CONDITIONS AUX LIMITES SUR LE TRACEUR       |
C |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
C |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
C |   TEMPS        | -->|  TEMPS                                       |
C |   DEBIT        |<-->|  TABLEAU DE DEBITS IMPOSES                   |
C |   NDEBIT       | -->|  NOMBRE DE FRONTIERES A DEBIT IMPOSE         |
C |   COTE         |<-->|  TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
C |   COTINI       | -->|  COTE INITIALE
C |   NCOTE        | -->|  NOMBRE DE FRONTIERES A COTE IMPOSEE         |
C |   VITES        |<-->|  TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
C |                |    |  IMPOSEES                                    |
C |   NVITES       | -->|  NOMBRE DE FRONTIERES A VITESSE IMPOSEE      |
C |   TRAC         | -->|  LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR  |
C |   TRACER       |<-->|  TABLEAU DE VALEURS DU TRACEUR IMPOSEES      |
C |   NTRACE       | -->|  NOMBRE DE FRONTIERES A TRACEUR IMPOSE       |
C |   NFRLIQ       | -->|  NOMBRE DE FRONTIERES LIQUIDES
C |   KENT,KENTU,  | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
C |                |    |  KENTU:U ET V IMPOSES                        |
C |   PROVEL       | -->|  OPTION POUR LES PROFILS DE VITESSE          |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : TELMAC
C
C SOUS-PROGRAMME APPELE : DEBIMP
C
C FONCTIONS APPELEES : Q , SL , TR , VIT
C
C
C-----------------------------------------------------------------------
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY : STA_DIS_CURVES,PTS_CURVES,QZ,
     *                                   FLUX_BOUNDARIES
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR2) 
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN) :: EQUA
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)  :: MASK,LITBOR 
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C local
!GM
!      INTEGER IP,IGAU,IDRO,IMIL
      INTEGER IP,IGAU(40),IDRO(40),IMIL(40), N, K
!GM Fin
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
        IF(TEMPS.GE.200.D0) THEN
C            
C       VITESSE IMPOSEE SUR LES FRONTIERES DROITE ET GAUCHE 
C
         IF(NCSIZE.LE.1) THEN
          DO IP=1,40
           IDRO(IP)= 52+IP-1
           IGAU(IP)=180-IP+1
           IMIL(IP)=1117+IP-1
           IF(IMIL(IP).EQ.1156) IMIL(IP)=116
           UBOR(IGAU(IP),1)    = MAX(U%R(IMIL(IP)),0.D0)
           U%R(NBOR(IGAU(IP))) =  UBOR(IGAU(IP),1) 
           UBOR(IDRO(IP),1)    = MAX(U%R(IMIL(IP)),0.D0)
           U%R(NBOR(IDRO(IP))) =  UBOR(IDRO(IP),1) 
          ENDDO
C
C         COTE IMPOSEE SUR LES FRONTIERES DROITE ET GAUCHE
C
          DO IP=1,40
           IDRO(IP)= 52+IP-1
           IGAU(IP)=180-IP+1
           IMIL(IP)=1117+IP-1
           IF(IMIL(IP).EQ.1156) IMIL(IP)=116
C           PERIODICITE
           HBOR(IDRO(IP))=H%R(IMIL(IP))
           H%R(NBOR(IDRO(IP))) = HBOR(IDRO(IP))
           HBOR(IGAU(IP))=H%R(IMIL(IP))
           H%R(NBOR(IGAU(IP))) = HBOR(IGAU(IP))
          ENDDO
         ENDIF
         
         IF(NCSIZE.GT.1) THEN
          DO IP=1,40
           IDRO(IP)= 52+IP-1
           IGAU(IP)=180-IP+1
           IMIL(IP)=1117+IP-1
           IF(IMIL(IP).EQ.1156) IMIL(IP)=116

           N=NBOR(IGAU(IP))
           
           DO K=1,NPTFR
              N=NBOR(K)
              IF(MESH%KNOLG%I(N).EQ.IGAU(IP)) THEN
                UBOR(K,1) = MAX(U%R(MESH%KNOGL%I(IMIL(IP))),0.D0)
                U%R(N) = UBOR(K,1)
                HBOR(K) = H%R(MESH%KNOGL%I(IMIL(IP)))
                H%R(N) = HBOR(K)
              ENDIF
           ENDDO
           
           DO K=1,NPTFR
              N=NBOR(K)
              IF(MESH%KNOLG%I(N).EQ.IDRO(IP)) THEN
                UBOR(K,1) = MAX(U%R(MESH%KNOGL%I(IMIL(IP))),0.D0)
                U%R(N) = UBOR(K,1)
                HBOR(K) = H%R(MESH%KNOGL%I(IMIL(IP)))
                H%R(N) = HBOR(K)
              ENDIF
           ENDDO
              
           CALL BORD_TEL(HBOR,UBOR,U,H,NPOIN,NPTFR2,IP)
          ENDDO
         ENDIF
C
        ENDIF
C
      RETURN
      END
C
C#######################################################################
C
C                  *******************
                   SUBROUTINE BORD_TEL
C                  *******************
C
     *(HBOR , UBOR , U , H , NPOIN , NPTFR2 , IP)
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C |   HBOR         |<-- |  HAUTEUR IMPOSEE.                            |
C |   UBOR         |<-- |  VITESSE U IMPOSEE.                          |
C !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : LIMWAC
C
C***********************************************************************
      USE BIEF
      USE DECLARATIONS_TELEMAC2D ,ONLY : MESH, NCSIZE
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPOIN,NPTFR,NPTFR2,LT,NPRIV,NBOR(NPTFR2)
C
      DOUBLE PRECISION HBOR(NPOIN),UBOR(NPOIN)
      DOUBLE PRECISION TOTO_H(NPOIN),TOTO_U(NPOIN)
C
      INTEGER IP, IMIL, IDRO, IGAU, BORDRO, BORGAU
      DOUBLE PRECISION DUMMY_H,DUMMY_U
      DOUBLE PRECISION P_DMAX, P_DMIN
      EXTERNAL P_DMAX, P_DMIN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: H,U
C
C***********************************************************************
      IMIL=1117+IP-1
      IF (IMIL.EQ.1156) IMIL=116
      IGAU=180-IP+1
      IDRO= 52+IP-1
C
      IMIL=MESH%KNOGL%I(IMIL)
      IF(IMIL.EQ.0) THEN
        DUMMY_U=0.
        DUMMY_H=0.
      ELSE
        DUMMY_U=MAX(U%R(IMIL),0.D0)
        DUMMY_H=H%R(IMIL)
      ENDIF
C
      BORGAU=MESH%KNOGL%I(NBOR(IGAU))
      BORDRO=MESH%KNOGL%I(NBOR(IDRO))
      IGAU=MESH%KNOGL%I(IGAU)
      IDRO=MESH%KNOGL%I(IDRO)

      IF(IGAU.EQ.0) THEN
        TOTO_H(IGAU) = P_DMAX(DUMMY_H)+P_DMIN(DUMMY_H)
        TOTO_U(IGAU) = P_DMAX(DUMMY_U)+P_DMIN(DUMMY_U)
      ELSE
        H%R(BORGAU) = P_DMAX(DUMMY_H)+P_DMIN(DUMMY_H)
        U%R(BORGAU) = P_DMAX(DUMMY_U)+P_DMIN(DUMMY_U)
      ENDIF
C
      IF(IDRO.EQ.0) THEN
        TOTO_H(IDRO) = P_DMAX(DUMMY_H)+P_DMIN(DUMMY_H)
        TOTO_U(IDRO) = P_DMAX(DUMMY_U)+P_DMIN(DUMMY_U)
      ELSE
        H%R(BORDRO) = P_DMAX(DUMMY_H)+P_DMIN(DUMMY_H)
        U%R(BORDRO) = P_DMAX(DUMMY_U)+P_DMIN(DUMMY_U)
      ENDIF
C
!      IF(IP.GT.38) WRITE(*,*) 'OK !', IP
      RETURN
      END
    

