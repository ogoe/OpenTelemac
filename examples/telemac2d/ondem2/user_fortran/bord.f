!                       ***************
                        SUBROUTINE BORD
!                       ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,
     & NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,
     & MASK,MESH,EQUA,NOMIMP)
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.0    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
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
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : BOUNDARY_COLOUR, T2D_FILES,
     &                                   T2DFO1
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER K,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE,MSK1,NTRAC,NPTFR2
      INTEGER NPOIN,NFRLIQ
      INTEGER IFRLIQ
      INTEGER KENT,KENTU
      INTEGER NBOR(NPTFR)
      INTEGER LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER PROVEL(*)
      INTEGER NUMLIQ(NPTFR)
!
      DOUBLE PRECISION HBOR(NPTFR),UBOR(NPTFR,2),VBOR(NPTFR,2)
      DOUBLE PRECISION ZF(NPOIN)
      DOUBLE PRECISION XNEBOR(NPTFR),YNEBOR(NPTFR)
!
      DOUBLE PRECISION TEMPS,Z
!
      CHARACTER(LEN=20) EQUA
      CHARACTER(LEN=144) NOMIMP
!
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ)  :: MASK,H,U,V,TRA05,TRA06,LITBOR,TBOR
!
      INTEGER P_IMAX
      DOUBLE PRECISION Q,SL,VIT,TR
      EXTERNAL         Q,SL,VIT,TR ,P_IMAX
      INTRINSIC MAX
!
!  TABLEAUX ISSUS DU FICHIER 26 , DONNANT LES CONDITIONS AUX LIMITES
!
      DOUBLE PRECISION C,HB(77),PHASEB(77),HC(77),PHASEC(77)
!
      DOUBLE PRECISION TE,T2,TI,PI,AN,ARG,T,W
!
      INTEGER N,NBORL(77),NPTFRL,I,KK
      INTEGER :: ID
!
!  TABLEAUX DE DONNEES TEMPORELLES
!
      SAVE NBORL,HB,PHASEB,PHASEC,HC
!
!-----------------------------------------------------------------------
!
      MSK1 = 1
!
!  LECTURE SUR LE FICHIER 26 DE HB ET PHASEB, CALCULES
!  PAR INTERPOLATION SUR CHAQUE POINT FRONTIERE
!
      PI = 3.141592653589793D0
      NPTFRL=77
!     DO K = 1, NPTFR
!       HBOR(K) = 0.D0
!     ENDDO
      ID = T2D_FILES(T2DFO1)%LU
      IF(TEMPS.EQ.150.) THEN
        REWIND ID
        DO K= 1 , NPTFRL
          READ(ID,*) I,HB(K),PHASEB(K),HC(K),PHASEC(K)
          PHASEB(K)=PI/180.D0*PHASEB(K)
          PHASEC(K)=PI/180.D0*PHASEC(K)
          NBORL(K)=I
        ENDDO
      ENDIF
!
      T=44714.D0
      W=2*PI/T
!
      DO K= 1 , NPTFRL
        ARG = MOD (W*TEMPS - PHASEB(K),2*PI)
        AN  = HB(K) * COS(ARG)
        ARG = MOD (2*W*TEMPS - PHASEC(K),2*PI)
        AN  = AN + HC(K) * COS(ARG)
        IF (TEMPS.LT.2500.D0) AN=AN*0.0004D0*TEMPS
        IF(NCSIZE.GT.0) THEN
          DO KK=1,NPTFR
            IF(BOUNDARY_COLOUR%I(KK).EQ.NBORL(K)) THEN
              HBOR(KK)=AN-ZF(NBOR(KK))
            ENDIF
          ENDDO
        ELSE
          HBOR(NBORL(K))=AN-ZF(NBOR(NBORL(K)))
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

