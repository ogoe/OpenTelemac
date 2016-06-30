!                       *****************
                        SUBROUTINE STRCHE
!                       *****************
!
!    COEFFICIENTS DE ELISABETH BARROS
!
!***********************************************************************
!  BIEF VERSION 5.0           01/10/96    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!
!      FONCTION: CALCUL DU COEFFICIENT DE FROTTEMENT SUR LE FOND
!                SI IL EST VARIABLE EN ESPACE.
!
!      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
!      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
! |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
! |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
! |    ZF          | -->|  COTE DU FOND                                |
! |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
! |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
! |    MESH        | -->|  BLOC DES ENTIERS DU MAILLAGE.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : PREDAT
!
!  SOUS-PROGRAMME APPELE : OV
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION X0(10),Y0(10),A1(10),A2(10),C(10),A3,R,PI
!
      INTEGER I, J
!
!-----------------------------------------------------------------------
!
!  ICI ON MET UN COEFFICIENT DE STRICKLER CONSTANT (EXEMPLE)
!
!EX   DO I=1,NPOIN
!EX     CHESTR%R(I) = 60.D0
!EX   ENDDO
!
!-----------------------------------------------------------------------
!
      X0(1)=-5.30D0
      Y0(1)=50.18D0
      A1(1)=  30.D0
      A2(1)= -63.D0
      C(1) = 54.3D0
!
      X0(2)=-2.71D0
      Y0(2)=49.36D0
      A1(2)=  47.D0
      A2(2)= -30.D0
      C(2) = 47.2D0
!
      X0(3)=-2.71D0
      Y0(3)=49.36D0
      A1(3)= -30.D0
      A2(3)=-110.D0
      C(3) = 47.2D0
!
      X0(4)=-2.12D0
      Y0(4)=49.14D0
      A1(4)=  45.D0
      A2(4)= -83.D0
      C(4) = 47.2D0
!
      X0(5)=-2.00D0
      Y0(5)=49.75D0
      A1(5)= 110.D0
      A2(5)= -40.D0
      C(5) = 62.2D0
!
      X0(6)=-1.35D0
      Y0(6)=49.68D0
      A1(6)=   0.D0
      A2(6)= -90.D0
      C(6) = 62.2D0
!
      X0(7)= 0.57D0
      Y0(7)=50.85D0
      A1(7)=  90.D0
      A2(7)= -48.D0
      C(7) = 62.2D0
!
      X0(8)= 1.47D0
      Y0(8)=51.00D0
      A1(8)= 132.D0
      A2(8)= -48.D0
      C(8) = 76.5D0
!
      PI=3.1415926535D0
      R=6400000.D0
      DO I=1,8
        X0(I)=R*X0(I)*PI/180.D0
        Y0(I)=R*LOG(TAN((Y0(I)+90.D0)*PI/360.D0))
     &       -R*LOG(TAN(69.D0*PI/180.D0))
        A1(I)=A1(I)*PI/180.D0
        A2(I)=A2(I)*PI/180.D0
      ENDDO
!
      DO I=1,NPOIN
        CHESTR%R(I) = 95.3D0
      ENDDO
      DO I=1,NPOIN
        IF (Y(I).GT.100000.D0) THEN
          DO J=1,8
            A3=ATAN2(Y(I)-Y0(J),X(I)-X0(J))
            IF (A3.LT.A1(J).AND.A3.GT.A2(J)) CHESTR%R(I)=C(J)
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE STRCHEJMJ
!                       *****************
!
!***********************************************************************
!  BIEF VERSION 5.0           01/10/96    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!
!      FONCTION: CALCUL DU COEFFICIENT DE FROTTEMENT SUR LE FOND
!                SI IL  EST VARIABLE EN ESPACE.
!
!      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
!      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
! |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
! |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
! |    ZF          | -->|  COTE DU FOND                                |
! |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
! |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
! |    MESH        | -->|  BLOC DES ENTIERS DU MAILLAGE.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : PREDAT
!
!  SOUS-PROGRAMME APPELE : OV
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION X0(10),Y0(10),A1(10),A2(10),C(10),A3,R,PI

!
      INTEGER I, J
!
!-----------------------------------------------------------------------
!
!  ICI ON MET UN COEFFICIENT DE STRICKLER CONSTANT (EXEMPLE)
!
!EX   DO I=1,NPOIN
!EX     CHESTR%R(I) = 60.D0
!EX   ENDDO
!
!-----------------------------------------------------------------------
!
      X0(1)=-5.30D0
      Y0(1)=50.18D0
      A1(1)=  30.D0
      A2(1)= -63.D0
      C(1) = 60.D0
!
      X0(2)=-2.71D0
      Y0(2)=49.36D0
      A1(2)=  47.D0
      A2(2)= -30.D0
      C(2) = 45.D0
!
      X0(3)=-2.71D0
      Y0(3)=49.36D0
      A1(3)= -30.D0
      A2(3)=-110.D0
      C(3) = 45.D0
!
      X0(4)=-2.12D0
      Y0(4)=49.14D0
      A1(4)=  45.D0
      A2(4)= -83.D0
      C(4) = 45.D0
!
      X0(5)=-2.00D0
      Y0(5)=49.75D0
      A1(5)= 110.D0
      A2(5)= -40.D0
      C(5) = 73.D0
!
      X0(6)=-1.35D0
      Y0(6)=49.68D0
      A1(6)=   0.D0
      A2(6)= -90.D0
      C(6) = 73.D0
!
      X0(7)= 0.57D0
      Y0(7)=50.85D0
      A1(7)=  90.D0
      A2(7)= -48.D0
      C(7) = 73.D0
!
      X0(8)= 1.47D0
      Y0(8)=51.00D0
      A1(8)= 132.D0
      A2(8)= -48.D0
      C(8) = 60.D0
!
!
      PI=3.141592653589793D0
      R=6400000.D0
      DO I=1,8
        X0(I)=R*X0(I)*PI/180.D0
        Y0(I)=R*LOG(TAN((Y0(I)+90.D0)*PI/360.D0))
     &       -R*LOG(TAN(69.D0*PI/180.D0))
        A1(I)=A1(I)*PI/180.D0
        A2(I)=A2(I)*PI/180.D0
      ENDDO
!
      DO I=1,NPOIN
        CHESTR%R(I) = 90.D0
      ENDDO
      DO I=1,NPOIN
        IF (Y(I).GT.100000.D0) THEN
          DO J=1,8
            A3=DATAN2(Y(I)-Y0(J),X(I)-X0(J))
            IF (A3.LT.A1(J).AND.A3.GT.A2(J)) CHESTR%R(I)=C(J)
          ENDDO
        ENDIF
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
      USE DECLARATIONS_TELEMAC2D, ONLY : BOUNDARY_COLOUR, LT
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
!
!  TABLEAUX DE DONNEES TEMPORELLES
!
      SAVE HB,PHASEB,PHASEC,NBORL,HC
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
      IF(LT.EQ.0) THEN
        DO K = 1, NPTFR
          HBOR(K) = 1.D0
        ENDDO
      ENDIF
      IF(TEMPS.EQ.150.) THEN
        REWIND 26
        DO K= 1, NPTFRL
          READ(26,*) I,HB(K),PHASEB(K),HC(K),PHASEC(K)
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
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC-2D 5.0          01/03/90    J-M HERVOUET
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER I,J
      DOUBLE PRECISION ZM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     REMARQUE JMH : 5023 POINTS DANS LE FICHIER MAIS NPOIN = 5007 ?????
      DOUBLE PRECISION ZZM(5023)
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
!  CALCUL DU DEMI-MARNAGE EN CHAQUE POINT DU MAILLAGE
!
      REWIND 27
!
!  LECTURE DES DEMI-MARNAGES DONNES PAR LE FICHIER
!  INITIALISATIONS DES TABLEAUX ET DES VARIABLES
!  CALCUL DE LA COTE DU FOND AU POINT I
!
      IF(NCSIZE.GT.1) THEN
!       NOMBRE DE POINTS DU MAILLAGE NON DECOUPE (5023 DANS FICHIER)
        DO I=1,5023
          READ(27,*) J,ZZM(I)
          IF(I.NE.J) STOP 'PROBLEME DANS FICHIER 27'
        ENDDO
        DO I=1,NPOIN
          ZF%R(I)=ZF%R(I)-ZZM(MESH%KNOLG%I(I))*12.D0/7.D0
        ENDDO
      ELSE
        DO I=1,NPOIN
          READ (27,*) J,ZM
          ZF%R(I)=ZF%R(I)-ZM*12.D0/7.D0
        ENDDO
      ENDIF
!
! ON LISSE 5 FOIS LE FOND (PB DE PENTE DU TALUS)
!
      MAS = .TRUE.
      LISFON = 5
      CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
