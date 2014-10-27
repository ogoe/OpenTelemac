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
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION PI
      INTEGER I
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  Bosse à t=0
!
      PI=3.141592653D0 
      DO I=1,NPOIN   
        ZF%R(I) = 0.D0
        IF (X(I) .GE. 2.D0 .AND. X(I) .LE. 10.D0) THEN
          ZF%R(I)=0.1D0*SIN(PI*(X(I)-2.D0)/8.D0)**2
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END                  
!

!                       **************************
                        SUBROUTINE CARACTERISTIQUE
!                       **************************
!
     &(X,Y,NPOIN,HFINAL,TEMPS)
!
!----------------------------------------------------------------
!
      INTEGER, PARAMETER :: NN = 1600
      DOUBLE PRECISION   X(NPOIN),Y(NPOIN),HFINAL(NPOIN)
      DOUBLE PRECISION   ZF0(NN), H0(NN)
      DOUBLE PRECISION   DIST,DIST1,DIST2
      DOUBLE PRECISION   XFICTIF(NN)
      DOUBLE PRECISION   XNEW(NN)
      INTEGER            NPOIN,I,II,COMPTEUR,QUOT,RESTE,KK
      DOUBLE PRECISION   GRAV,D,S,STRICKLER,CFROT,DEBIT,PI,PAS
      DOUBLE PRECISION   K1,K2,K,MOYENNE_H,TEMPS,DX
!
      DX=0.01D0
      PI= 4.D0*ATAN(1.D0)
!       
      DO II=1,NN 
        XFICTIF(II) = (II-1)*DX
      ENDDO
      DO II=1,NN 
        H0(II)=0.D0 
        ZF0(II)=0.D0
        IF(XFICTIF(II).GE. 2.D0 .AND.
     &     XFICTIF(II).LE.10.D0) THEN                        
          ZF0(II)=0.1D0*SIN(PI*(XFICTIF(II)-2.D0)/8.D0)**2
        ENDIF
        H0(II)=0.6D0-ZF0(II)  
      ENDDO
      DO II=1,NN
        XNEW(II)=0.D0 
        IF(H0(II).GE.1.D0) H0(II)=0
      ENDDO
!
! INITIALISATION DES VARIABLES
!----------------------------------------------------------------
!
      DO I=1,NPOIN
        HFINAL(I)=0.D0
      ENDDO
!      
!  CALCUL DE LA HAUTEUR D'EAU MOYENNE
!----------------------------------------------------------------
!
      MOYENNE_H = 0.D0
      DO I=1,NN
        MOYENNE_H = MOYENNE_H + H0(I)
      ENDDO
      MOYENNE_H = MOYENNE_H / NN
!
!  PARAMETRES ET CONSTANTES
!----------------------------------------------------------------
!
      GRAV = 9.81D0
      D = 0.000150D0
      S = 2.65D0
      STRICKLER = 50.D0
      CFROT = 2.D0*GRAV/(STRICKLER**2*MOYENNE_H**(1.D0/3.D0))
      DEBIT = 0.25D0
      K1 = SQRT(GRAV*(S-1)*D**3)
      K2 = CFROT/(2*GRAV*(S-1)*D)
! XKV= 1.6;  N=1-1/XKV=0.375
      K=1.6D0*0.5D0*K1*K2**(5.D0/2.D0)*DEBIT**5/CFROT
!
!  CREATION DE LA SOLUTION PAR METHODE DES CARACTERISTIQUES
!----------------------------------------------------------------
!     
      DO I=1,NN
        XNEW(I) = XFICTIF(I) + K*TEMPS/H0(I)**6
      ENDDO
!
!  INTERPOLATION AVEC L'ANCIEN AXE DES ABSCISSES
!----------------------------------------------------------------
!
      COMPTEUR=0
      DO I=1,NPOIN
        COMPTEUR=0
        DO J=1,NN-1
          DIST =XNEW(J+1)-XNEW(J)
          DIST1=XNEW(J+1)-X(I)
          DIST2=X(I)-XNEW(J)
          IF(DIST1.GE.0 .AND. DIST2.GE.0 .AND.COMPTEUR.EQ.0) THEN
            HFINAL(I)=0.6D0-(DIST1*H0(J+1)+DIST2*H0(J))/DIST
            COMPTEUR=COMPTEUR+1
          ENDIF
          IF(COMPTEUR.EQ.0) HFINAL(I)=0.D0
        ENDDO
      ENDDO
!
!----------------------------------------------------------------
!     
      RETURN     
      END
!                       *****************
                        SUBROUTINE PREDES
!                       *****************
!
     &(LLT,AAT)
!
!***********************************************************************
! SISYPHE VERSION 6.0                             E. PELTIER    11/09/95
!                                                 C. LENORMANT
!                                                 J.-M. HERVOUET
! 
!
! JMH 07/12/2009: KS SET TO 0 IF LLT=0
!                                               
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
!***********************************************************************
!
!     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
!                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |      LLT       |--> | LOCAL LT (MAY BE LT-1+PERCOU) 
! |      AAT       |--> | CURRENT TIME (FOR BUILDING SOLUTIONS)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!     - PROGRAMME APPELANT : SISYPH  
!     - SOUS-PROGRAMMES APPELES : OVD,OV
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      DOUBLE PRECISION BID 
      INTEGER LTT,IN      
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME THAN IN DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
!
!=======================================================================
! ECRITURE DES VARIABLES CALCULEES
!=======================================================================
!
!     VITESSE U:    U=QU/H
!
      IF ((LEO.AND.SORLEO(1)).OR.(IMP.AND.SORIMP(1))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
!
!     VITESSE V:    V=QV/H
!
      IF ((LEO.AND.SORLEO(2)).OR.(IMP.AND.SORIMP(2))) THEN
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
!
!     CELERITE C:   (GRAV*H)**0.5
!
      IF ((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(HN,T3)
        DO IN=1, NPOIN
          T3%R(IN)= SQRT (GRAV*HN%R(IN))
        ENDDO
      ENDIF
!
!     SURFACE LIBRE Z: H+ZF
!
      IF ((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS('X=Y+Z   ',Z,HN,ZF, BID )
      ENDIF
!
!     FROUDE F: ((QU**2+QV**2)/(GRAV*H**3))**0.5
!
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
!
!=======================================================================
!
!     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
!
      IF(LLT.EQ.0) THEN
!       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
!
!=======================================================================
!
!  SOLUTION ANALYTIQUE POUR LE FOND (PREMIER TABLEAU PRIVE)
!
!  CV    IF((LEO.AND.SORLEO(27+(NOMBLAY+4)*NSICLA+NOMBLAY)).OR.
!  CV  *   (IMP.AND.SORIMP(27+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
      IF(LEO.OR.IMP) THEN
            CALL CARACTERISTIQUE(MESH%X%R,MESH%Y%R,NPOIN,
     &                                        PRIVE%ADR(1)%P%R,AAT)
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
!
      RETURN
      END 

