C                       ************************
                        SUBROUTINE VOISIN_STBTEL
C                       ************************
C
C
C    SEULE DIFFERENCE AVEC BIEF : LA TAILLE DE IDIMAT
C                                 QUI PERMET ICI DE TRAITER
C                                 DES QUADRILATERES.
C
C
     *(IFABOR,NELEM,NELMAX,IELM,IKLE,NPOIN,NACHB,NBOR,NPTFR,IADR,NVOIS)
C
C***********************************************************************
C PROGICIEL STBTEL V5.2         24/04/97  J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C    FONCTION : CONSTRUCTION DU TABLEAU IFABOR, OU IFABOR(IELEM,IFACE)
C               EST LE NUMERO GLOBAL DU VOISIN DE LA FACE IFACE DE
C               L'ELEMENT IELEM SI CE VOISIN EXISTE ET 0 SI LA FACE EST
C               SUR LA FRONTIERE DU DOMAINE.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    IFABOR      |<-- | TABLEAU DES VOISINS DES FACES.
C |    NELEM       | -->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C |    NELMAX      | -->| NOMBRE MAXIMUM D'ELEMENTS DANS LE MAILLAGE.
C |                |    | (CAS DES MAILLAGES ADAPTATIFS)
C |    IELM        | -->| 11: TRIANGLES
C |                |    | 21: QUADRILATERES
C |    IKLE        | -->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT
C |    NPOIN       | -->| NOMBRE TOTAL DE POINTS DU DOMAINE
C |________________|____|_______________________________________________
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT DANS TELEMAC 2D : PREDAT
C
C***********************************************************************
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPTFR,IR1,IR2,IR3,IR4
      INTEGER NBOR(NPTFR),NACHB(5,NPTIR)
C
      INTEGER NELEM,NELMAX,IELM,IDIMAT,NPOIN,I,J,ERR
      INTEGER NFACE,KEL,I1,I2,IMAX,IFACE,IELEM,M1,M2,IV,IELEM2,IFACE2
      INTEGER IFABOR(NELMAX,*),IKLE(NELMAX,*),NVOIS(NPOIN),IADR(NPOIN)
C
      INTEGER SOMFAC(2,4,2)
      DATA SOMFAC / 1,2 , 2,3 , 3,1 , 0,0   ,  1,2 , 2,3 , 3,4 , 4,1 /
C
C  TABLEAUX DE TRAVAIL ALLOUES DYNAMIQUEMENT
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: MAT1,MAT2,MAT3
C
C-----------------------------------------------------------------------
C
C     IDIMAT EST UNE MAJORATION DE LA SOMME DES NOMBRES DE VOISINS DE
C     TOUS LES POINTS.
C     IDIMAT = NPOIN + NBFEL(IELM) * NELEM
      IDIMAT = NPOIN + 4 * NELEM
C
      ALLOCATE(MAT1(IDIMAT),STAT=ERR)
      ALLOCATE(MAT2(IDIMAT),STAT=ERR)
      ALLOCATE(MAT3(IDIMAT),STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'VOISIN : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     *            'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'VOISIN: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     *            'ERROR CODE: ',1I6)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INITIALISATION DES IFABOR A 0 :
C
      IF(IELM.EQ.21) THEN
C       QUADRILATERES
        NFACE = 4
        KEL = 2
      ELSEIF(IELM.EQ.11) THEN
C       TRIANGLES
        NFACE = 3
        KEL = 1
      ELSE
        IF(LNG.EQ.1) WRITE(LU,98) IELM
        IF(LNG.EQ.2) WRITE(LU,99) IELM
98      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99      FORMAT(1X,'VOISIN: IELM=',1I6,' TYPE OF ELEMENT NOT AVAILABLE')
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  CALCUL DU TABLEAU NVOIS POUR CHAQUE POINT
C  ATTENTION : NVOIS N'EST PAS LE NOMBRE DE VOISINS MAIS PERMET DE
C              RESERVER ASSEZ DE PLACE DANS LES TABLEAUX MAT1,2,3.
C
      DO 10 I=1,NPOIN
C                  4 (EN PARALLELISME SELON R. HINKELMANN ?)
        NVOIS(I) = 2
10    CONTINUE
C
      DO 20 IFACE = 1,NFACE
        DO 30 IELEM=1,NELEM
          I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
          I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
          NVOIS(I1) = NVOIS(I1) + 1
          NVOIS(I2) = NVOIS(I2) + 1
30      CONTINUE
20    CONTINUE
C
      DO 11 I=1,NPOIN
        NVOIS(I) = NVOIS(I) / 2
11    CONTINUE
C
C  LE TOTAL DE TOUS LES NVOIS DONNE : NPOIN + NFACE*NELEM
C  CE NOMBRE SERT A DIMENSIONNER W1 DANS POINT
C
C-----------------------------------------------------------------------
C
C  CALCUL DES ADRESSES DE CHAQUE POINT DANS UNE STRUCTURE DE TYPE
C  MATRICE COMPACTE
C
      IADR(1) = 1
      DO 50 I= 2,NPOIN
        IADR(I) = IADR(I-1) + NVOIS(I-1)
50    CONTINUE
C
      IMAX = IADR(NPOIN) + NVOIS(NPOIN) - 1
      IF(IMAX.GT.IDIMAT) THEN
        IF(LNG.EQ.1) WRITE(LU,51) IDIMAT,IMAX
        IF(LNG.EQ.2) WRITE(LU,52) IDIMAT,IMAX
51      FORMAT(1X,'VOISIN: TAILLE DE MAT1,2,3 (',1I6,') INSUFFISANTE',/,
     *         1X,'IL FAUT AU MOINS : ',1I6)
52      FORMAT(1X,'VOISIN: SIZE OF MAT1,2,3 (',1I6,') TOO SHORT',/,
     *         1X,'MINIMUM SIZE: ',1I6)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INITIALISATION A ZERO DE LA MATRICE COMPACTE
C
      DO 53 I=1,IMAX
        MAT1(I) = 0
53    CONTINUE
C
C-----------------------------------------------------------------------
C
C  BOUCLE SUR LES FACES DE CHAQUE ELEMENT :
C
      DO 60 IFACE = 1 , NFACE
      DO 70 IELEM = 1 , NELEM
C
      IFABOR(IELEM,IFACE) = -1
C
C        NUMEROS GLOBAUX DES POINTS DE LA FACE :
C
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
C        NUMEROS GLOBAUX ORDONNES :
C
         M1 = MIN0(I1,I2)
         M2 = MAX0(I1,I2)
C
         DO 80 IV = 1,NVOIS(M1)
C
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
C
80       CONTINUE
C
         IF(LNG.EQ.1) WRITE(LU,82)
         IF(LNG.EQ.2) WRITE(LU,83)
82       FORMAT(1X,'VOISIN : ERREUR DANS LE MAILLAGE       ',/,1X,
     *             '         PEUT-ETRE DES POINTS CONFONDUS')
83       FORMAT(1X,'VOISIN : ERROR IN THE MESH             ',/,1X,
     *             '         MAYBE SUPERIMPOSED POINTS     ')
         STOP
C
81       CONTINUE
C
70    CONTINUE
60    CONTINUE
C
C  ON POURRAIT ESSAYER AVEC UN ALGORITHME PLUS LEGER.
C  PAR EXEMPLE EN UTILISANT INDPU
C
      IF(NCSIZE.GT.1) THEN
C
      DO 61 IFACE=1,NFACE
      DO 71 IELEM=1,NELEM
C
C  CERTAINES FACE DE BORD SONT EN FAIT DES INTERFACES ENTRE
C  SOUS-DOMAINES : ON LEUR MET UNE VALEUR -2 AU LIEU DE -1
C
      IF(IFABOR(IELEM,IFACE).EQ.-1) THEN
C
         I1 = IKLE( IELEM , SOMFAC(1,IFACE,KEL) )
         I2 = IKLE( IELEM , SOMFAC(2,IFACE,KEL) )
C
         IR1=0
         IR2=0
C
         IF(NPTIR.GT.0) THEN
           DO 44 J=1,NPTIR
             IF(I1.EQ.NACHB(1,J)) IR1=1
             IF(I2.EQ.NACHB(1,J)) IR2=1
44         CONTINUE
         ENDIF
C
         IF(IR1.EQ.1.AND.IR2.EQ.1) THEN
C          SEGMENT INTERFACE DETECTE, ON REGARDE SI CE N'EST PAS
C          AUSSI UNE VRAIE FACE DE BORD
           IR3=0
           IR4=0
           DO 55 J=1,NPTFR
             IF(I1.EQ.NBOR(J)) IR3=1
             IF(I2.EQ.NBOR(J)) IR4=1
55         CONTINUE
C          PRIORITE AUX VRAIES FACES DE BORD
           IF(IR3.EQ.0.OR.IR4.EQ.0) THEN
             IFABOR(IELEM,IFACE)=-2
           ENDIF
         ENDIF
C
      ENDIF
C
71    CONTINUE
61    CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(MAT1)
      DEALLOCATE(MAT2)
      DEALLOCATE(MAT3)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
