C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : TELEMAC-2D 5.0          01/03/90    J-M HERVOUET
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
      INTEGER LNG,LU,I
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
      DO 20 I = 1,NPOIN
         IF (X(I).LT.-5.D0) THEN
            ZF%R(I) = -0.616D0
         ENDIF
         IF (X(I).GE.-5.D0.AND.X(I).LT.5.D0) THEN
            ZF%R(I) = -0.616D0 + 0.05*(X(I) + 5.D0)
         ENDIF
         IF (X(I).GE.5.D0.AND.X(I).LT.9.4) THEN
            ZF%R(I) = -0.116D0 -0.025*(X(I) - 5.D0)
         ENDIF
         IF (X(I).GE.9.4) THEN
            ZF%R(I) = -0.226D0 + 0.05*(X(I) - 9.4)
         ENDIF
         IF (ZF%R(I).GT.-0.04D0) THEN
            ZF%R(I) = -0.04D0
         ENDIF
 20   CONTINUE
C
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  
C                       *****************
                        SUBROUTINE INCIDE
C                       *****************
C
     *(COTOND,H,C0,PATMOS,ATMOS,ZF,MESH,LT,AT,GRAV,ROEAU,PRIVE)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.0    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  :  CALCUL DE L'ONDE INCIDENTE IMPOSEE AUX BORDS
C
C     EN CHAQUE POINT D'ONDE INCIDENTE, IL FAUT DONNER :
C
C
C     ( 1 - NINC . NBOR ) A COS ( PHI - OMEGA T)
C
C     AVEC :
C
C     NINC  : DIRECTION DE L'ONDE INCIDENTE.
C     NBOR  : NORMALE A LA PAROI (XSGBOR ET YSGBOR).
C     A     : AMPLITUDE DE L'ONDE.
C     OMEGA : PULSATION DE L'ONDE.
C     PHI   : PHASE DE L'ONDE.
C     T     : TEMPS.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |  COTOND        |<-- | ONDE RESULTAT.
C |  H             | -->| HAUTEUR D'EAU.
C |  C0            | -->| CELERITE DE REFERENCE
C |  PATMOS        | -->| PRESSION ATMOSPHERIQUE
C |  ATMOS         | -->| LOGIQUE INDIQUANT SI PATMOS EST REMPLI.
C |  ZF            | -->| FOND
C |  X , Y         | -->| COORDONNEES DES POINTS
C |  XSGBOR,YSGBOR | -->| COMPOSANTES DES NORMALES EXTERIEURES.
C |  NBOR          | -->| CORRESPONDANCE NUMEROTATION DE BORD - GLOBALE
C |  LT,AT         | -->| NUMERO DE L'ITERATION,TEMPS
C |  NPTFR         | -->| NOMBRE DE POINTS FRONTIERES
C |  NPOIN         | -->| NOMBRE DE POINTS DU MAILLAGE
C |  GRAV          | -->| PESANTEUR
C |  ROEAU         | -->| MASSE VOLUMIQUE DE L'EAU
C |  PRIVE         | -->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : PROPAG
C
C SOUS-PROGRAMME APPELE : OV
C
C***********************************************************************
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION AT,GRAV,ROEAU,PI
C
      LOGICAL ATMOS
C
      INTEGER LT
C
C-----------------------------------------------------------------------
C
C  STRUCTURE DE MAILLAGE
C
      TYPE(BIEF_MESH) MESH
C
C-----------------------------------------------------------------------
C
C  STRUCTURES DE VECTEURS
C
      TYPE(BIEF_OBJ) COTOND,PATMOS,H,C0,ZF,PRIVE
C
C-----------------------------------------------------------------------
C
      CALL OS( 'X=C     ' , COTOND , COTOND , COTOND , 0.D0 )
C
C     PI = 3.141592653589D0
C     T=200.D0
C     W=2.*PI/T
C     A=0.25
C
C      DO 10 K=261,271
C       COTOND(K) = 2.*A*SIN(W*AT)
C10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

