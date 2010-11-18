C                       ****************
                        SUBROUTINE ELMPB
C                       ****************
C
     *(NBPB,NUMPB,X,Y,IKLE,NCOLOR,ISDRY,NEW)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2                    A. CABAL / P. LANG SOGREAH
C***********************************************************************
C
C     FONCTION  :  ELIMINATION DES ELEMENTS APPARTENANT A PLUSIEURS
C                  SEGMENTS FRONTIERES
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   NBPB         |--> | NB DE POINTS A SUPPRIMER
C |   NUMPB        |--> | NUMERO DES POINTS A SUPPRIMER
C |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
C |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
C |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
C | ELPSEC         | -->| INDICATEUR ELIMIN. DES ELEMENTS PARTIELLEMENT SECS              
C | ISDRY(NELMAX)  |<-- | TAB INDICATEUR ELEMENTS SECS
C |                |    | = 1 POINT TOUJOURS SEC,
C |                |    | = 0 SOUS SEUSEC M D'EAU AU MOINS POUR 1 PAS DE TEMPS
C | IHAUT          | -->| NUM D'ORDRE DE LA VARIABLE HAUT D'EAU DANS FICH TEL2D
C | NVAR           | -->| NB DE VAR STOCKEES DANS LE FICHIER TEL2D
C | H              | -->| TABLEAU DES HAUTEURS D'EAU 
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |   
C |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C APPELE PAR : STBTEL
C***********************************************************************
C
      IMPLICIT NONE

      INTEGER      MESH, NDP , NPOIN , NELEM , NPMAX , NELMAX
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
C
      INTEGER IKLE(NELMAX,4), ISDRY(NPMAX), NEW(NPMAX)
      INTEGER NCOLOR(NPMAX)
      INTEGER NBPB, NUMPB(100)
      INTEGER I, IEL, J, NELI
    
      DOUBLE PRECISION X(NPMAX) , Y(NPMAX)
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C     -------------------------------------------------------------
C     ELIMINATION DES ELEMENTS COMPORTANT DES POINTS A PROBLEME
C     -------------------------------------------------------------
C
      DO 10 I=1,NBPB
        DO 11 IEL = 1, NELEM
          IF (IKLE(IEL,1).EQ.NUMPB(I).OR.IKLE(IEL,2).EQ.NUMPB(I)
     +        .OR.IKLE(IEL,3).EQ.NUMPB(I)) THEN
            IKLE(IEL, 1) = 0
            IKLE(IEL, 2) = 0
            IKLE(IEL, 3) = 0
          ENDIF
 11     CONTINUE
 10   CONTINUE
C
C     ELIMINATION DES ELEMENTS      
C     ------------------------
C
      NELI = 0
      IEL = 1
C     POUR CHAQUE ELEMENT FAIRE       
 20   CONTINUE     
        IF ((IKLE(IEL, 1).EQ.0).AND.(IKLE(IEL, 2).EQ.0).AND.
     +     (IKLE(IEL, 3).EQ.0)) THEN
         NELI = NELI + 1              
         DO 48 I = IEL, NELEM - NELI
           IKLE(I,1) = IKLE(I+1, 1)
           IKLE(I,2) = IKLE(I+1, 2)
           IKLE(I,3) = IKLE(I+1, 3)
 48      CONTINUE
        ELSE
         IEL = IEL + 1
        ENDIF
      IF (IEL .LE. NELEM-NELI) GOTO 20
C     FIN POUR CHAQUE ELEMENT

      IF (LNG.EQ.1) WRITE(LU,1009) NELI 
      IF (LNG.EQ.2) WRITE(LU,2009) NELI

      NELEM = NELEM - NELI

C      ELIMINATION DES POINTS NE FAISANT PLUS PARTIE DU MAILLAGE
C      REUTILISATION DE ISDRY POUR MARQUER LES POINTS NON UTILISEES
C      ---------------------------------------------
       DO 65 I = 1, NPOIN
         ISDRY(I) = 0
         NEW(I) = 0
 65    CONTINUE 
       
       DO 75 IEL = 1, NELEM
        ISDRY(IKLE(IEL,1)) = IKLE(IEL,1)
        ISDRY(IKLE(IEL,2)) = IKLE(IEL,2)
        ISDRY(IKLE(IEL,3)) = IKLE(IEL,3)
 75    CONTINUE
       
       NELI = 0
       I = 1
C      POUR CHAQUE POINT FAIRE 
       DO 85 I = 1, NPOIN
         IF (ISDRY(I) .EQ.0) THEN
           NELI = NELI + 1
           NEW(I) = 0
         ELSE
           NEW(I) = I - NELI
         ENDIF
 85    CONTINUE
C      FIN POUR CHAQUE POINT         

       NELI = 0
       I = 1
C      POUR CHAQUE POINT FAIRE 
 30    CONTINUE       
         IF (ISDRY(I) .EQ.0) THEN
C          POINT I  A ELIMINER
           NELI = NELI + 1
C          DECALAGE DANS LE TABLEAU DES POINTS
           DO 95 J = I, NPOIN - NELI 
             X(J) = X(J+1)
             Y(J) = Y(J+1)
             NCOLOR(J) = NCOLOR(J+1)
             IF (ISDRY(J+1).GT.0) THEN
               ISDRY(J) = ISDRY(J+1) - 1
             ELSE
               ISDRY(J) = 0
             ENDIF
 95        CONTINUE
         ELSE
           I = I + 1
         ENDIF
       IF (I .LE. NPOIN - NELI) GOTO 30  
C      FIN POUR CHAQUE POINT         
       IF (LNG.EQ.1) WRITE(LU,1011) NELI 
       IF (LNG.EQ.2) WRITE(LU,2011) NELI
       NPOIN = NPOIN - NELI
       
C      ON REPERCUTE LA RENUMEROTATION DANS IKLE
C      ----------------------------------------   
       DO 115 IEL = 1, NELEM
         J = IKLE(IEL,1)
         IKLE(IEL,1) = NEW(J)
         J = IKLE(IEL,2)
         IKLE(IEL,2) = NEW(J)
         J = IKLE(IEL,3)
         IKLE(IEL,3) = NEW(J)
 115   CONTINUE
       RETURN
C***********************************************************************
 1009 FORMAT(1X,'ELEMENTS SUPPRIMES DU MAILLAGE :',I8)
 2009 FORMAT(1X,'ELEMENTS CANCELLED IN THE MESH:',I8)
 1011 FORMAT(1X,'POINTS SUPPRIMES DU MAILLAGE :  ',I8)
 2011 FORMAT(1X,'POINTS CANCELLED IN THE MESH:  ',I8)
      END
