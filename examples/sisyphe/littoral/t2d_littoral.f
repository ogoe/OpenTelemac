!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.2          01/03/90    J-M HERVOUET
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
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
      DO I=1,NPOIN
          ZF%R(I)= MIN(0.05D0*Y(I)-10.D0,-0.10D0)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END                  
!                       ***************
                        SUBROUTINE BORD
!                       ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2, TEMPS,
     & NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.8    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!      FONCTION:    MODIFIE LES TABLEAUX DE CONDITIONS AUX LIMITES
!                   DANS LE CAS OU ELLES SONT VARIABLES EN TEMPS.
!
!      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
!      SOIT DIRECTEMENT, SOIT PAR L'INTERMEDIAIRE DES FONCTIONS :
!
!             Q , SL , TR , VIT
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   HBOR         |<-- |  HAUTEUR IMPOSEE.                            |
! |   UBOR         |<-- |  VITESSE U IMPOSEE.                          |
! |   VBOR         |<-- |  VITESSE V IMPOSEE.                          |
! |   TBOR         |<-- |  TRACEUR IMPOSE AU BORD                      |
! |    U,V         | -->|  COMPOSANTES DE LA VITESSE AU TEMPS N        |
! |    H           | -->|  HAUTEUR AU TEMPS N                          |
! |    ZF          | -->|  FOND                                        |
! |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
! |  TRA05,TRA06   | -->|  TABLEAUX DE TRAVAIL                         |
! |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
! | LIUBOR         | -->|  CONDITIONS AUX LIMITES SUR U 
! |   LITBOR       | -->|  CONDITIONS AUX LIMITES SUR LE TRACEUR       |
! |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
! |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
! |   TEMPS        | -->|  TEMPS                                       |
! |   DEBIT        |<-->|  TABLEAU DE DEBITS IMPOSES                   |
! |   NDEBIT       | -->|  NOMBRE DE FRONTIERES A DEBIT IMPOSE         |
! |   COTE         |<-->|  TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
! |   COTINI       | -->|  COTE INITIALE
! |   NCOTE        | -->|  NOMBRE DE FRONTIERES A COTE IMPOSEE         |
! |   VITES        |<-->|  TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
! |                |    |  IMPOSEES                                    |
! |   NVITES       | -->|  NOMBRE DE FRONTIERES A VITESSE IMPOSEE      |
! |   TRAC         | -->|  LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR  |
! |   TRACER       |<-->|  TABLEAU DE VALEURS DU TRACEUR IMPOSEES      |
! |   NTRACE       | -->|  NOMBRE DE FRONTIERES A TRACEUR IMPOSE       |
! |   NFRLIQ       | -->|  NOMBRE DE FRONTIERES LIQUIDES
! |   KENT,KENTU,  | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
! |                |    |  KENTU:U ET V IMPOSES                        |
! |   PROVEL       | -->|  OPTION POUR LES PROFILS DE VITESSE          |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : TELMAC
!
! SOUS-PROGRAMME APPELE : DEBIMP
!
! FONCTIONS APPELEES : Q , SL , TR , VIT
!
!
!-----------------------------------------------------------------------
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY : STA_DIS_CURVES,PTS_CURVES,QZ,
     &                                   FLUX_BOUNDARIES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION P_DMAX, P_DMIN
      EXTERNAL P_DMAX, P_DMIN
!GM
      INTEGER IP,IGAU(40),IDRO(40),IMIL(40), N, K
      DOUBLE PRECISION DUMMY_U,DUMMY_H
!GM Fin
!
!-----------------------------------------------------------------------
!
        IF(TEMPS.GE.200.D0) THEN
!            
!       VITESSE IMPOSEE SUR LES FRONTIERES DROITE ET GAUCHE 
!
         IF(NCSIZE.LE.1) THEN
!
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
!
!         COTE IMPOSEE SUR LES FRONTIERES DROITE ET GAUCHE
!
          DO IP=1,40
           IDRO(IP)= 52+IP-1
           IGAU(IP)=180-IP+1
           IMIL(IP)=1117+IP-1
           IF(IMIL(IP).EQ.1156) IMIL(IP)=116
!           PERIODICITE
           HBOR(IDRO(IP))=H%R(IMIL(IP))
           H%R(NBOR(IDRO(IP))) = HBOR(IDRO(IP))
           HBOR(IGAU(IP))=H%R(IMIL(IP))
           H%R(NBOR(IGAU(IP))) = HBOR(IGAU(IP))
          ENDDO
!         
         ELSEIF(NCSIZE.GT.1) THEN
!
          DO IP=1,40
!
           IDRO(IP)=52+IP-1
           IGAU(IP)=180-IP+1
           IMIL(IP)=1117+IP-1
           IF(IMIL(IP).EQ.1156) IMIL(IP)=116
!
           IMIL(IP)=GLOBAL_TO_LOCAL_POINT(IMIL(IP),MESH)
           IF(IMIL(IP).EQ.0) THEN
             DUMMY_U=0.D0
             DUMMY_H=0.D0
           ELSE
             DUMMY_U=MAX(U%R(IMIL(IP)),0.D0)
             DUMMY_H=H%R(IMIL(IP))
           ENDIF
!
           DUMMY_H=P_DMAX(DUMMY_H)+P_DMIN(DUMMY_H)
           DUMMY_U=P_DMAX(DUMMY_U)+P_DMIN(DUMMY_U)
!     
           DO K=1,NPTFR
              N=NBOR(K)
              IF(MESH%KNOLG%I(N).EQ.IGAU(IP)) THEN
                UBOR(K,1) = DUMMY_U
                U%R(N) = UBOR(K,1)
                HBOR(K) = DUMMY_H
                H%R(N) = HBOR(K)
              ENDIF
           ENDDO
!           
           DO K=1,NPTFR
              N=NBOR(K)
              IF(MESH%KNOLG%I(N).EQ.IDRO(IP)) THEN
                UBOR(K,1) = DUMMY_U
                U%R(N) = UBOR(K,1)
                HBOR(K) = DUMMY_H
                H%R(N) = HBOR(K)
              ENDIF
           ENDDO
!
          ENDDO
         ENDIF
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
