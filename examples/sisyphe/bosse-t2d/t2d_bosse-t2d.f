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
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION PI
      INTEGER I
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  Bosse à t=0
C
      PI=3.141592653D0 
      DO I=1,NPOIN   
         ZF%R(I) = 0.D0
         IF (X(I) .GE. 2.D0 .AND. X(I) .LE. 10.D0) THEN
         ZF%R(I)=0.1D0*SIN(PI*(X(I)-2.D0)/8.D0)**2
         ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  
C

C                       **************************
                        SUBROUTINE CARACTERISTIQUE
C                       **************************
C
     *(X,Y,NPOIN,HFINAL,TEMPS)
C
C----------------------------------------------------------------
C
      INTEGER, PARAMETER :: NN = 1600
      DOUBLE PRECISION   X(NPOIN),Y(NPOIN),HFINAL(NPOIN)
      DOUBLE PRECISION   ZF0(NN), H0(NN)
      DOUBLE PRECISION   DIST,DIST1,DIST2
      DOUBLE PRECISION   XFICTIF(NN)
      DOUBLE PRECISION   XNEW(NN)
      INTEGER            NPOIN,I,II,compteur,QUOT,RESTE,KK
      DOUBLE PRECISION   GRAV,D,S,STRICKLER,CFROT,DEBIT,pi,PAS
      DOUBLE PRECISION   K1,K2,K,MOYENNE_H,TEMPS,DX
C
      DX=0.01D0
      PI= 4.D0*ATAN(1.D0)
C       
      DO II=1,NN 
        XFICTIF(II) = (II-1)*DX
      ENDDO
      DO II=1,NN 
        H0(II)=0.D0 
        ZF0(II)=0.D0
        IF(XFICTIF(II).GE. 2.D0 .AND.
     *     XFICTIF(II).LE.10.D0) THEN                        
          ZF0(II)=0.1D0*SIN(PI*(XFICTIF(II)-2.D0)/8.D0)**2
        ENDIF
        H0(II)=0.6D0-ZF0(II)  
      ENDDO
      DO II=1,NN
        XNEW(II)=0.D0 
        IF(H0(II).GE.1.D0) H0(II)=0
      ENDDO
C
C INITIALISATION DES VARIABLES
C----------------------------------------------------------------
C
      DO I=1,NPOIN
         HFINAL(I)=0.D0
      ENDDO
C      
C  CALCUL DE LA HAUTEUR D'EAU MOYENNE
C----------------------------------------------------------------
C
      MOYENNE_H = 0.D0
      DO I=1,NN
         MOYENNE_H = MOYENNE_H + H0(I)
      ENDDO
      MOYENNE_H = MOYENNE_H / NN
C
C  PARAMETRES ET CONSTANTES
C----------------------------------------------------------------
C
      GRAV = 9.81D0
      D = 0.000150D0
      S = 2.65D0
      STRICKLER = 50.D0
      CFROT = 2.D0*GRAV/(STRICKLER**2*MOYENNE_H**(1.D0/3.D0))
      DEBIT = 0.25D0
      K1 = SQRT(GRAV*(S-1)*D**3)
      K2 = CFROT/(2*GRAV*(S-1)*D)
C XKV= 1.6;  N=1-1/XKV=0.375
      K=1.6D0*0.5D0*K1*K2**(5.D0/2.D0)*Debit**5/CFROT
C
C  CREATION DE LA SOLUTION PAR METHODE DES CARACTERISTIQUES
C----------------------------------------------------------------
C     
      DO I=1,NN
        XNEW(I) = XFICTIF(I) + K*TEMPS/H0(I)**6
      ENDDO
C
C  INTERPOLATION AVEC L'ANCIEN AXE DES ABSCISSES
C----------------------------------------------------------------
C
      COMPTEUR=0
      DO I=1,NPOIN
        COMPTEUR=0
        DO J=1,NN-1
          DIST =XNEW(J+1)-XNEW(J)
          DIST1=XNEW(J+1)-X(I)
          DIST2=X(I)-XNEW(J)
          IF(DIST1.GE.0 .AND. DIST2.GE.0 .AND.compteur.EQ.0) THEN
            HFINAL(I)=0.6D0-(DIST1*H0(J+1)+DIST2*H0(J))/DIST
            COMPTEUR=COMPTEUR+1
          ENDIF
          IF(COMPTEUR.EQ.0) HFINAL(I)=0.D0
        ENDDO
      ENDDO
C
C----------------------------------------------------------------
C     
      RETURN     
      END
C                       *****************
                        SUBROUTINE PREDES
C                       *****************
C
     *(LLT,AAT)
C
C***********************************************************************
C SISYPHE VERSION 6.0                             E. PELTIER    11/09/95
C                                                 C. LENORMANT
C                                                 J.-M. HERVOUET
C 
C
C JMH 07/12/2009: KS SET TO 0 IF LLT=0
C                                               
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
C***********************************************************************
C
C     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
C                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |      LLT       |--> | LOCAL LT (MAY BE LT-1+PERCOU) 
C |      AAT       |--> | CURRENT TIME (FOR BUILDING SOLUTIONS)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C     - PROGRAMME APPELANT : SISYPH  
C     - SOUS-PROGRAMMES APPELES : OVD,OV
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     
      DOUBLE PRECISION BID 
      INTEGER LTT,IN      
      LOGICAL IMP,LEO
C
C-----------------------------------------------------------------------
C
C     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
C     FOLLOWING TESTS, WHICH MUST BE THE SAME THAN IN DESIMP (BIEF LIBRARY)
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
C
C     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
C
C=======================================================================
C ECRITURE DES VARIABLES CALCULEES
C=======================================================================
C
C     VITESSE U:    U=QU/H
C
      IF ((LEO.AND.SORLEO(1)).OR.(IMP.AND.SORIMP(1))) THEN
       CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
C
C     VITESSE V:    V=QV/H
C
      IF ((LEO.AND.SORLEO(2)).OR.(IMP.AND.SORIMP(2))) THEN
       CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
C
C     CELERITE C:   (GRAV*H)**0.5
C
      IF ((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
         CALL CPSTVC(HN,T3)
         DO IN=1, NPOIN
            T3%R(IN)= SQRT (GRAV*HN%R(IN))
         ENDDO
      ENDIF
C
C     SURFACE LIBRE Z: H+ZF
C
      IF ((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
         CALL OS('X=Y+Z   ',Z,HN,ZF, BID )
      ENDIF
C
C     FROUDE F: ((QU**2+QV**2)/(GRAV*H**3))**0.5
C
      IF ((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN ) 
        CALL CPSTVC(QU,T4) 
        DO IN=1,NPOIN
           T4%R(IN)= T1%R(IN)**2+T2%R(IN)**2
        ENDDO       
        CALL OS( 'X=Y/Z   ' , T4 , T4 , HN , 0.D0 , 2 , 0.D0 , HMIN )  
        DO IN=1,NPOIN
          T4%R(IN)=SQRT(T4%R(IN)/GRAV)        
        ENDDO
      ENDIF
C
C=======================================================================
C
C     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
C
      IF(LLT.EQ.0) THEN
C       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
C
C=======================================================================
C
C  SOLUTION ANALYTIQUE POUR LE FOND (PREMIER TABLEAU PRIVE)
C
C  CV    IF((LEO.AND.SORLEO(27+(NOMBLAY+4)*NSICLA+NOMBLAY)).OR.
C  CV  *   (IMP.AND.SORIMP(27+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
      IF(LEO.OR.IMP) THEN
            CALL CARACTERISTIQUE(MESH%X%R,MESH%Y%R,NPOIN,
     &                                        PRIVE%ADR(1)%P%R,AAT)
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
C
C=======================================================================
C
      RETURN
      END 

