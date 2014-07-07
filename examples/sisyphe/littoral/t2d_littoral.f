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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DMAX, P_DMIN
      EXTERNAL P_DMAX, P_DMIN
!GM
      INTEGER IP,IGAU(40),IDRO(40),IMIL(40), N, K
      DOUBLE PRECISION DUMMY_U,DUMMY_H
!GM Fin
C
C-----------------------------------------------------------------------
C
        IF(TEMPS.GE.200.D0) THEN
C            
C       VITESSE IMPOSEE SUR LES FRONTIERES DROITE ET GAUCHE 
C
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
