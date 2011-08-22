!                       ************************
                        SUBROUTINE VOISIN_STBTEL
!                       ************************
!
!
!    SEULE DIFFERENCE AVEC BIEF : LA TAILLE DE IDIMAT
!                                 QUI PERMET ICI DE TRAITER
!                                 DES QUADRILATERES.
!
!
     &(IFABOR,NELEM,NELMAX,IELM,IKLE,NPOIN,NACHB,NBOR,NPTFR,IADR,NVOIS)
!
!***********************************************************************
! PROGICIEL STBTEL V5.2         24/04/97  J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!    FONCTION : CONSTRUCTION DU TABLEAU IFABOR, OU IFABOR(IELEM,IFACE)
!               EST LE NUMERO GLOBAL DU VOISIN DE LA FACE IFACE DE
!               L'ELEMENT IELEM SI CE VOISIN EXISTE ET 0 SI LA FACE EST
!               SUR LA FRONTIERE DU DOMAINE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    IFABOR      |<-- | TABLEAU DES VOISINS DES FACES.
! |    NELEM       | -->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
! |    NELMAX      | -->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
! |                |    | (CAS DES MAILLAGES ADAPTATIFS)
! |    IELM        | -->| 11: TRIANGLES
! |                |    | 21: QUADRILATERES
! |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
! |    NPOIN       | -->| NOMBRE TOTAL DE POINTS DU DOMAINE
! |________________|____|_______________________________________________
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT DANS TELEMAC 2D : PREDAT
!
!***********************************************************************
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPTFR,IR1,IR2,IR3,IR4
      INTEGER NBOR(NPTFR),NACHB(5,NPTIR)
!
      INTEGER NELEM,NELMAX,IELM,IDIMAT,NPOIN,I,J,ERR
      INTEGER NFACE,KEL,I1,I2,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER IFABOR(NELMAX,*),IKLE(NELMAX,*),NVOIS(NPOIN),IADR(NPOIN)
!
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
!
!  TABLEAUX DE TRAVAIL ALLOUES DYNAMIQUEMENT
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
!
!-----------------------------------------------------------------------
!
!     IDIMAT EST UNE MAJORATION DE LA SOMME DES NOMBRES DE VOISINS DE
!     TOUS LES POINTS.
!     IDIMAT = NPOIN + NBFEL(IELM) * NELEM
      IDIMAT = NPOIN + 4 * NELEM
!
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
!
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'VOISIN : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'VOISIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INITIALISATION DES IFABOR A 0 :
!
      IF(IELM.EQ.21) THEN
!       QUADRILATERES
        NFACE = 4
        KEL = 2
      ELSEIF(IELM.EQ.11) THEN
!       TRIANGLES
        NFACE = 3
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  CALCUL DU TABLEAU NVOIS POUR CHAQUE POINT
!  ATTENTION : NVOIS N'EST PAS LE NOMBRE DE VOISINS MAIS PERMET DE
!              RESERVER ASSEZ DE PLACE DANS LES TABLEAUX MAT1,2,3.
!
      DO 10 I=1,NPOIN
!                  4 (EN PARALLELISME SELON R. HINKELMANN ?)
        NVOIS(I) = 2
10    CONTINUE
!
      DO 20 IFACE = 1,NFACE
        DO 30 IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
30      CONTINUE
20    CONTINUE
!
      DO 11 I=1,NPOIN
        NVOIS(I) = NVOIS(I) / 2
11    CONTINUE
!
!  LE TOTAL DE TOUS LES NVOIS DONNE : NPOIN + NFACE*NELEM
!  CE NOMBRE SERT A DIMENSIONNER W1 DANS POINT
!
!-----------------------------------------------------------------------
!
!  CALCUL DES ADRESSES DE CHAQUE POINT DANS UNE STRUCTURE DE TYPE
!  MATRICE COMPACTE
!
      IADR(1) = 1
      DO 50 I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
50    CONTINUE
!
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        IF(LNG.EQ.1) WRITE(LU,51) IDIMAT,IMAX
        IF(LNG.EQ.2) WRITE(LU,52) IDIMAT,IMAX
51      FORMAT(1X,'VOISIN: TAILLE DE MAT1,2,3 (',1I6,') INSUFFISANTE',/,
     &         1X,'IL FAUT AU MOINS : ',1I6)
52      FORMAT(1X,'VOISIN: SIZE OF MAT1,2,3 (',1I6,') TOO SHORT',/,
     &         1X,'MINIMUM SIZE: ',1I6)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INITIALISATION A ZERO DE LA MATRICE COMPACTE
!
      DO 53 I=1,IMAX
        MAT1(I) = 0
53    CONTINUE
!
!-----------------------------------------------------------------------
!
!  BOUCLE SUR LES FACES DE CHAQUE ELEMENT :
!
      DO 60 IFACE = 1 , NFACE
      DO 70 IELEM = 1 , NELEM
!
      IFABOR(IELEM,IFACE) = -1
!
!        NUMEROS GLOBAUX DES POINTS DE LA FACE :
!
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
!        NUMEROS GLOBAUX ORDONNES :
!
         M1 = MIN0(I1,I2)
         M2 = MAX0(I1,I2)
!
         DO 80 IV = 1,NVOIS(M1)
!
           IF(MAT1(IADR(M1)+IV-1).EQ.0) THEN
              MAT1(IADR(M1)+IV-1)=M2
              MAT2(IADR(M1)+IV-1)=IELEM
              MAT3(IADR(M1)+IV-1)=IFACE
              GO TO 81
           ELSEIF(MAT1(IADR(M1)+IV-1).EQ.M2) THEN
              IELEM2 = MAT2(IADR(M1)+IV-1)
              IFACE2 = MAT3(IADR(M1)+IV-1)
              IFABOR(IELEM,IFACE) = IELEM2
              IFABOR(IELEM2,IFACE2) = IELEM
              GO TO 81
           ENDIF
!
80       CONTINUE
!
         IF(LNG.EQ.1) WRITE(LU,82)
         IF(LNG.EQ.2) WRITE(LU,83)
82       FORMAT(1X,'VOISIN : ERREUR DANS LE MAILLAGE       ',/,1X,
     &             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     &             '         MAYBE SUPERIMPOSED POINTS     ')
         STOP
!
81       CONTINUE
!
70    CONTINUE
60    CONTINUE
!
!  ON POURRAIT ESSAYER AVEC UN ALGORITHME PLUS LEGER.
!  PAR EXEMPLE EN UTILISANT INDPU
!
      IF(NCSIZE.GT.1) THEN
!
      DO 61 IFACE=1,NFACE
      DO 71 IELEM=1,NELEM
!
!  CERTAINES FACE DE BORD SONT EN FAIT DES INTERFACES ENTRE
!  SOUS-DOMAINES : ON LEUR MET UNE VALEUR -2 AU LIEU DE -1
!
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
!
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
!
         IR1=0
         IR2=0
!
         IF(NPTIR.GT.0) THEN
           DO 44 J=1,NPTIR
             IF(I1.EQ.NACHB(1,J)) IR1=1
             IF(I2.EQ.NACHB(1,J)) IR2=1
44         CONTINUE
         ENDIF
!
         IF(IR1.EQ.1.AND.IR2.EQ.1) THEN
!          SEGMENT INTERFACE DETECTE, ON REGARDE SI CE N'EST PAS
!          AUSSI UNE VRAIE FACE DE BORD
           IR3=0
           IR4=0
           DO 55 J=1,NPTFR
             IF(I1.EQ.NBOR(J)) IR3=1
             IF(I2.EQ.NBOR(J)) IR4=1
55         CONTINUE
!          PRIORITE AUX VRAIES FACES DE BORD
           IF(IR3.EQ.0.OR.IR4.EQ.0) THEN
             IFABOR(IELEM,IFACE)=-2
           ENDIF
         ENDIF
!
      ENDIF
!
71    CONTINUE
61    CONTINUE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
!
!-----------------------------------------------------------------------
!
      RETURN
      END