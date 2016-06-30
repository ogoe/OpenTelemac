!                       ***************
                        SUBROUTINE BORH
!                       ***************
!
!***********************************************************************
!
!  ARTEMIS    VERSION 6.2    07/12   D. AELBRECHT (LNH) 01 30 87 74 12
!
!  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
!
!***********************************************************************
!
!      FONCTION:    PREND EN COMPTE LES CONDITIONS AUX LIMITES
!                   DE L'UTILISATEUR
!                   ELLES SONT DONNEES PAR SEGMENT.
!
!      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
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
! APPELE PAR : ARTEMI
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION PI,BID
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
!
! JCB :
      INTEGER IG

!
!CP
      INTEGER IG0 ,JB
      DOUBLE PRECISION PHASOI,AUXIC,AUXIS,DEGRAD,X0,Y0,KK
!CP
!
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
      INTRINSIC COS,SIN
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
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
! PAROIS SOLIDES
!
      DO I=1,NPTFR
          JB=BOUNDARY_COLOUR%I(I)

          IF(JB.GE.101.AND.JB.LE.304)THEN
            LIHBOR%I(I)=KLOG
            RP%R(I)=1.D0
          ENDIF
!         PAROIS LIQUIDES -FRONTIERE LIBRE
!
          IF(JB.GE.42.AND.JB.LE.100)THEN
            LIHBOR%I(I)=KSORT
          ENDIF
          IF(JB.GE.305.AND.JB.LE.363)THEN
            LIHBOR%I(I)=KSORT
          ENDIF
!
!         PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
!
          DEGRAD=PI/180.D0
          PHASOI=0.D0
          X0=2000.
          Y0=4500.
          AUXIC =COS(TETAH*DEGRAD)
          AUXIS =SIN(TETAH*DEGRAD)
!
!
          IF(JB.GE.364.AND.JB.LE.484)THEN
            LIHBOR%I(I)=KINC
            TETAB%R(I)=0.
            HB%R(I)=1.D0
!   ------- PHASE
            IG   = MESH%NBOR%I(I)
            KK=K%R(IG)
            PHASOI=KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
            ALFAP%R(I) = PHASOI/DEGRAD
          ENDIF
          IF(JB.GE.1.AND.JB.LE.41)THEN
            LIHBOR%I(I)=KINC
            TETAB%R(I)=0.
            HB%R(I)=1.D0
!   ------- PHASE
            IG   = MESH%NBOR%I(I)
            KK=K%R(IG)
            PHASOI=KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
            ALFAP%R(I) = PHASOI/DEGRAD
          ENDIF
      ENDDO
!
! ------------
!
! JCB :
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I
!
      DOUBLE PRECISION PI,BID
!
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
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
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!

!-----------------------------------------------------------------------
!
! EXEMPLE :
!
!
!      DO I = 1,NPOIN
!        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!        IF (Y(I).GE.700.D0) THEN
!           ZF%R(I) = -15.D0
!        ENDIF
!      ENDDO
!-----------------------------------------------------------------------
! VOTRE MODIFICATION DES FONDS :
!-----------------------------------------------------------------------
!
! JCB :
!
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
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
