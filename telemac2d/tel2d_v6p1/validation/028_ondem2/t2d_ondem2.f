C                       *****************
                        SUBROUTINE STRCHE
C                       *****************
C
C    COEFFICIENTS DE ELISABETH BARROS
C
C***********************************************************************
C  BIEF VERSION 5.0           01/10/96    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C
C      FONCTION: CALCUL DU COEFFICIENT DE FROTTEMENT SUR LE FOND
C                SI IL EST VARIABLE EN ESPACE.
C
C      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
C      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
C |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
C |    ZF          | -->|  COTE DU FOND                                |
C |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
C |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
C |    MESH        | -->|  BLOC DES ENTIERS DU MAILLAGE.
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : PREDAT
C
C  SOUS-PROGRAMME APPELE : OV
C
C**********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION X0(10),Y0(10),A1(10),A2(10),C(10),A3,R,PI                
C
      INTEGER I, J
C
C-----------------------------------------------------------------------
C
C  ICI ON MET UN COEFFICIENT DE STRICKLER CONSTANT (EXEMPLE)
C
CEX   DO I=1,NPOIN
CEX     CHESTR%R(I) = 60.D0
CEX   ENDDO
C
C-----------------------------------------------------------------------
C
      X0(1)=-5.30D0                                                             
      Y0(1)=50.18D0                                                             
      A1(1)=  30.D0                                                             
      A2(1)= -63.D0                                                             
      C(1) = 54.3D0                                                              
C                                                                               
      X0(2)=-2.71D0                                                             
      Y0(2)=49.36D0                                                             
      A1(2)=  47.D0                                                             
      A2(2)= -30.D0                                                             
      C(2) = 47.2D0                                                              
C                                                                               
      X0(3)=-2.71D0                                                             
      Y0(3)=49.36D0                                                             
      A1(3)= -30.D0                                                             
      A2(3)=-110.D0                                                             
      C(3) = 47.2D0                                                              
C                                                                               
      X0(4)=-2.12D0                                                             
      Y0(4)=49.14D0                                                             
      A1(4)=  45.D0                                                             
      A2(4)= -83.D0                                                             
      C(4) = 47.2D0                                                              
C                                                                                                                                          
      X0(5)=-2.00D0                                                             
      Y0(5)=49.75D0                                                             
      A1(5)= 110.D0                                                             
      A2(5)= -40.D0                                                             
      C(5) = 62.2D0                                                              
C                                                                               
      X0(6)=-1.35D0                                                             
      Y0(6)=49.68D0                                                             
      A1(6)=   0.D0                                                             
      A2(6)= -90.D0                                                             
      C(6) = 62.2D0                                                              
C                                                                               
      X0(7)= 0.57D0                                                             
      Y0(7)=50.85D0                                                             
      A1(7)=  90.D0                                                             
      A2(7)= -48.D0                                                             
      C(7) = 62.2D0                                                              
C                                                                               
      X0(8)= 1.47D0                                                             
      Y0(8)=51.00D0                                                             
      A1(8)= 132.D0                                                             
      A2(8)= -48.D0                                                             
      C(8) = 76.5D0                                                              
C                                                                                                                                                              
      PI=3.1415926535D0                                                         
      R=6400000.D0                                                              
      DO 5 I=1,8                                                                
         X0(I)=R*X0(I)*PI/180.D0                                                
         Y0(I)=R*LOG(TAN((Y0(I)+90.D0)*PI/360.D0))                            
     *        -R*LOG(TAN(69.D0*PI/180.D0))                                    
         A1(I)=A1(I)*PI/180.D0                                                  
         A2(I)=A2(I)*PI/180.D0                                                  
5     CONTINUE                                                                  
C                                                                               
      DO I=1,NPOIN
        CHESTR%R(I) = 95.3D0
      ENDDO
      DO 10 I=1,NPOIN                                                           
         IF (Y(I).GT.100000.D0) THEN                                            
            DO 20 J=1,8                                                         
               A3=ATAN2(Y(I)-Y0(J),X(I)-X0(J))                                 
               IF (A3.LT.A1(J).AND.A3.GT.A2(J)) CHESTR%R(I)=C(J)                  
 20         CONTINUE                                                            
         ENDIF                                                                  
 10   CONTINUE                                                                  
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE STRCHEJMJ
C                       *****************
C
C***********************************************************************
C  BIEF VERSION 5.0           01/10/96    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C
C      FONCTION: CALCUL DU COEFFICIENT DE FROTTEMENT SUR LE FOND
C                SI IL  EST VARIABLE EN ESPACE.
C
C      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
C      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
C |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
C |    ZF          | -->|  COTE DU FOND                                |
C |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
C |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
C |    MESH        | -->|  BLOC DES ENTIERS DU MAILLAGE.
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : PREDAT
C
C  SOUS-PROGRAMME APPELE : OV
C
C**********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION X0(10),Y0(10),A1(10),A2(10),C(10),A3,R,PI                

C
      INTEGER I, J
C
C-----------------------------------------------------------------------
C
C  ICI ON MET UN COEFFICIENT DE STRICKLER CONSTANT (EXEMPLE)
C
CEX   DO I=1,NPOIN
CEX     CHESTR%R(I) = 60.D0
CEX   ENDDO
C
C-----------------------------------------------------------------------
C
      X0(1)=-5.30D0                                                             
      Y0(1)=50.18D0                                                             
      A1(1)=  30.D0                                                             
      A2(1)= -63.D0                                                             
      C(1) = 60.D0                                                              
C                                                                               
      X0(2)=-2.71D0                                                             
      Y0(2)=49.36D0                                                             
      A1(2)=  47.D0                                                             
      A2(2)= -30.D0                                                             
      C(2) = 45.D0                                                              
C                                                                               
      X0(3)=-2.71D0                                                             
      Y0(3)=49.36D0                                                             
      A1(3)= -30.D0                                                             
      A2(3)=-110.D0                                                             
      C(3) = 45.D0                                                              
C                                                                               
      X0(4)=-2.12D0                                                             
      Y0(4)=49.14D0                                                             
      A1(4)=  45.D0                                                             
      A2(4)= -83.D0                                                             
      C(4) = 45.D0                                                              
C                                                                                                                                          
      X0(5)=-2.00D0                                                             
      Y0(5)=49.75D0                                                             
      A1(5)= 110.D0                                                             
      A2(5)= -40.D0                                                             
      C(5) = 73.D0                                                              
C                                                                               
      X0(6)=-1.35D0                                                             
      Y0(6)=49.68D0                                                             
      A1(6)=   0.D0                                                             
      A2(6)= -90.D0                                                             
      C(6) = 73.D0                                                              
C                                                                               
      X0(7)= 0.57D0                                                             
      Y0(7)=50.85D0                                                             
      A1(7)=  90.D0                                                             
      A2(7)= -48.D0                                                             
      C(7) = 73.D0                                                              
C                                                                               
      X0(8)= 1.47D0                                                             
      Y0(8)=51.00D0                                                             
      A1(8)= 132.D0                                                             
      A2(8)= -48.D0                                                             
      C(8) = 60.D0                                                              
C                                                                               
C                                                                               
      PI=3.141592653589793D0                                                         
      R=6400000.D0                                                              
      DO 5 I=1,8                                                                
         X0(I)=R*X0(I)*PI/180.D0                                                
         Y0(I)=R*LOG(TAN((Y0(I)+90.D0)*PI/360.D0))                            
     *        -R*LOG(TAN(69.D0*PI/180.D0))                                    
         A1(I)=A1(I)*PI/180.D0                                                  
         A2(I)=A2(I)*PI/180.D0                                                  
5     CONTINUE                                                                  
C                                                                               
      DO I=1,NPOIN
        CHESTR%R(I) = 90.D0
      ENDDO
      DO 10 I=1,NPOIN                                                           
         IF (Y(I).GT.100000.D0) THEN                                            
            DO 20 J=1,8                                                         
               A3=DATAN2(Y(I)-Y0(J),X(I)-X0(J))                                 
               IF (A3.LT.A1(J).AND.A3.GT.A2(J)) CHESTR%R(I)=C(J)                  
 20         CONTINUE                                                            
         ENDIF                                                                  
 10   CONTINUE                                                                  
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
     * XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,
     * NDEBIT,NCOTE,NVITES,
     * NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,
     * MASK,MESH,EQUA,NOMIMP)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.0    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
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
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : BOUNDARY_COLOUR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER K,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE,MSK1,NTRAC,NPTFR2
      INTEGER NPOIN,NFRLIQ
      INTEGER IFRLIQ
      INTEGER KENT,KENTU
      INTEGER NBOR(NPTFR)
      INTEGER LIHBOR(NPTFR),LIUBOR(NPTFR)
      INTEGER PROVEL(100)
      INTEGER NUMLIQ(NPTFR)
C
      DOUBLE PRECISION HBOR(NPTFR),UBOR(NPTFR,2),VBOR(NPTFR,2)
      DOUBLE PRECISION ZF(NPOIN) 
      DOUBLE PRECISION XNEBOR(NPTFR),YNEBOR(NPTFR)
C
      DOUBLE PRECISION TEMPS,Z
C
      CHARACTER(LEN=20) EQUA
      CHARACTER(LEN=144) NOMIMP
C
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ)  :: MASK,H,U,V,TRA05,TRA06,LITBOR,TBOR
C
      INTEGER P_IMAX
      DOUBLE PRECISION Q,SL,VIT,TR
      EXTERNAL         Q,SL,VIT,TR ,P_IMAX
      INTRINSIC MAX
C                                                                               
C  TABLEAUX ISSUS DU FICHIER 26 , DONNANT LES CONDITIONS AUX LIMITES
C            
      DOUBLE PRECISION C,HB(77),PHASEB(77),HC(77),PHASEC(77)                    
C                                                                               
      DOUBLE PRECISION TE,T2,TI,PI,AN,ARG,T,W          
C                                                                               
      INTEGER N,NBORL(77),NPTFRL,I,KK                                   
C                                                                               
C  TABLEAUX DE DONNEES TEMPORELLES                                             
C                                                                               
      SAVE                                                                
C                                                                               
C-----------------------------------------------------------------------
C
      MSK1 = 1
C
C  LECTURE SUR LE FICHIER 26 DE HB ET PHASEB, CALCULES                          
C  PAR INTERPOLATION SUR CHAQUE POINT FRONTIERE                                 
C                                                                               
      PI = 3.141592653589793D0                                                  
      NPTFRL=77 
!     DO K = 1, NPTFR
!       HBOR(K) = 0.D0
!     ENDDO
      IF(TEMPS.EQ.150.) THEN                                                          
         REWIND 26                                                              
         DO K= 1 , NPTFRL                                                   
          READ(26,*) I,HB(K),PHASEB(K),HC(K),PHASEC(K)                          
            PHASEB(K)=PI/180.D0*PHASEB(K)                                       
            PHASEC(K)=PI/180.D0*PHASEC(K)                                       
            NBORL(K)=I                                                   
         ENDDO                   
      ENDIF                                                                     
C                                                                               
      T=44714.D0                                                                
      W=2*PI/T                                                                  
C                                                                               
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
C
C-----------------------------------------------------------------------
C
      RETURN
      END 
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
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER I,J
      DOUBLE PRECISION ZM
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     REMARQUE JMH : 5023 POINTS DANS LE FICHIER MAIS NPOIN = 5007 ?????
      DOUBLE PRECISION ZZM(5023)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
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
C  CALCUL DU DEMI-MARNAGE EN CHAQUE POINT DU MAILLAGE                           
C                                                                               
      REWIND 27                                                                 
C                                                                               
C  LECTURE DES DEMI-MARNAGES DONNES PAR LE FICHIER                              
C  INITIALISATIONS DES TABLEAUX ET DES VARIABLES                                
C  CALCUL DE LA COTE DU FOND AU POINT I                                         
C
      IF(NCSIZE.GT.1) THEN
C       NOMBRE DE POINTS DU MAILLAGE NON DECOUPE (5023 DANS FICHIER)
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
C                                                                               
C ON LISSE 5 FOIS LE FOND (PB DE PENTE DU TALUS)                                
C                                                                               
      MAS = .TRUE.  
      LISFON = 5 
      CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

