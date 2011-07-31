C                       ***************** 
                        SUBROUTINE CORSTR 
C                       ***************** 
C 
C*********************************************************************** 
C  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18 
C 
C*********************************************************************** 
C 
C      FONCTION: CORRECTION DU COEFFICIENT DE FROTTEMENT SUR LE FOND 
C                QUAND IL EST VARIABLE EN TEMPS. 
C 
C      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE 
C      IL DOIT ETRE REMPLI PAR L'UTILISATEUR 
C 
C 
C  
C 
C----------------------------------------------------------------------- 
C  EXAMPLE OF POSSIBLE ARGUMENTS 
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
C |    H           | -->|  HAUTEUR D'EAU. 
C |    AT          | -->|  TIME. 
C |________________|____|______________________________________________| 
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE) 
C 
C----------------------------------------------------------------------- 
C 
C  APPELE PAR : TELMAC 
C 
C  SOUS-PROGRAMME APPELE : 
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
      INTEGER I 
C 
      INTEGER NPMAX2 
      INTEGER ND 
      INTEGER NG 
C 
! deja defini dans bief 
c      PARAMETER (NPMAX=2000)                                                
      PARAMETER (NPMAX2=200)        
      DOUBLE PRECISION XD(NPMAX2),YD(NPMAX2) 
      DOUBLE PRECISION XG(NPMAX2),YG(NPMAX2) 
C 
      DOUBLE PRECISION ZTAMPON,DISTOT,DIST 
C 
      DOUBLE PRECISION Q,DGRA,DMAX 
      EXTERNAL         Q 
C
C-----------------------------------------------------------------------  
C 
C 
       OPEN (61,file='../berge_rd.SX',status='old') 
       read (61,*) ND 
       DO I=1,ND 
         read (61,*) xD(I),yD(I) 
 	ENDDO 
       CLOSE(61) 
C 
      OPEN (61,file='../berge_rg.SX',status='old') 
       read (61,*) Ng 
       DO I=1,Ng 
         read (61,*) xg(I),yg(I) 
 	ENDDO 
       CLOSE(61) 
C 
C 
      DO I = 1 , NPOIN 
 
 
        IF (inpoly(mesh%x%R(I),mesh%y%R(I),xD,yD,nD)) THEN 
          CHESTR%R(I) = 1.D0 
!          CHESTR%R(I) = 0.3D0 
	  ENDIF 
 
        IF (inpoly(mesh%x%R(I),mesh%y%R(I),xg,yg,ng)) THEN 
          CHESTR%R(I) = 1.D0 
!          CHESTR%R(I) = 0.3D0 
	  ENDIF 
	       
      ENDDO   
	 
!	write (*,*) 'dans corstr :', at   
C 
C----------------------------------------------------------------------- 
C 
      RETURN 
      END 
C                       ***************** 
                        SUBROUTINE NOEROD 
C                       ***************** 
C 
     * (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS ) 
C 
C*********************************************************************** 
C SISYPHE VERSION 5.1                             C. LENORMANT 
C                                                 
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT    
C*********************************************************************** 
C 
C     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR 
C 
C 
C     RQ: LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE 
C     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR  
C     CHOISIR DE LISSER LA SOLUTION OBTENUE i.e NLISS > 0.   
C 
C     FUNCTION  : IMPOSE THE RIGID BED LEVEL  ZR 
C 
C----------------------------------------------------------------------- 
C                             ARGUMENTS 
C .________________.____.______________________________________________ 
C |      NOM       |MODE|                   ROLE 
C |________________|____|______________________________________________ 
C |   H            | -->| WATER DEPTH 
C |   ZF           | -->| BED LEVEL 
C |   ZR           |<-- | RIGID BED LEVEL 
C |   Z            | -->| FREE SURFACE  
C |   X,Y          | -->| 2D COORDINATES 
C |   NPOIN        | -->| NUMBER OF 2D POINTS 
C |   CHOIX        | -->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS 
C |   NLISS        |<-->| NUMBER OF SMOOTHINGS 
C |________________|____|______________________________________________ 
C MODE : -->(INPUT), <--(RESULT), <-->(MODIFIED DATA) 
C----------------------------------------------------------------------- 
C 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C 
      INTEGER, INTENT(IN):: NPOIN , CHOIX 
      INTEGER, INTENT(INOUT):: NLISS  
C 
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)     
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN) 
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN) 
C 
C----------------------------------------------------------------------- 
      INTEGER I 
C 
      INTEGER NPMAX2 
      INTEGER ND 
      INTEGER NG 
C 
! deja defini dans bief 
c      PARAMETER (NPMAX=2000)                                                
      PARAMETER (NPMAX2=200)        
      DOUBLE PRECISION XD(NPMAX2),YD(NPMAX2) 
      DOUBLE PRECISION XG(NPMAX2),YG(NPMAX2) 
C-------------------- 
C RIGID BEDS POSITION 
C--------------------- 
C 
C       DEFAULT VALUE:       ZR=ZF-100  
C                                                               
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-1000.D0,NPOIN)                                                     
C 
C------------------ 
C SMOOTHING OPTION 
C------------------ 
C       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE 
C                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING) 
C 
      NLISS = 0    
      OPEN (61,file='../berge_rd.SX',status='old') 
      read (61,*) ND 
      DO I=1,ND 
        read (61,*) xD(I),yD(I) 
	ENDDO 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_rg.SX',status='old') 
      read (61,*) Ng 
      DO I=1,Ng 
        read (61,*) xg(I),yg(I) 
	ENDDO 
      CLOSE(61) 
C 
C 
      DO I = 1 , NPOIN 
 
        IF (inpoly(x(I),y(I),xD,yD,nD)) THEN 
          ZR(I) = ZF(I) 
	  ENDIF 
 
        IF (inpoly(x(I),y(I),xg,yg,ng)) THEN 
          ZR(I) = ZF(I) 
	  ENDIF 
	       
      ENDDO  	        
C  
      RETURN 
      END SUBROUTINE NOEROD 
C                         ********************* 
                          SUBROUTINE INIT_COMPO
C                         ********************* 
C
     *(NCOUCHES) 
C
C*********************************************************************** 
C SISYPHE VERSION 5.3 
C                             Matthieu GONZALES DE LINARES 2002 
C 
C                                                 
C COPYRIGHT EDF-BAW-IFH    
C*********************************************************************** 
C 
C     FONCTION  : DISTRIBUTION DES CLASSES 
C                 % PAR COUCHE, STRATIFICATION  
C     SUBROUTINE A REMPLIR PAR l'UTILISATEUR 
C 
C  
C     FUNCTION  : INITIAL FRACTION DISTRIBUTION, STRATIFICATION,  
C                 VARIATION IN SPACE 
C 
C----------------------------------------------------------------------- 
C                             ARGUMENTS 
C .________________.____.______________________________________________ 
C |      NOM       |MODE|                   ROLE 
C |________________|____|______________________________________________ 
C |                |    |   
C |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE 
C |    AVAIL(10,NSICLA,NPOIN) 
C |    ES          |<-- |  THICKNESS FOR EACH LAYER AND NODE 
C |    ES(10,NPOIN) 
C |    NCOUCHES     |--> |  NUMBER OF LAYER FOR EACH POINT 
C |    NSICLA      |--> |  NUMBER OF SIZE-CLASSES OF BED MATERIAL 
C                           (LESS THAN 10) 
C |    NPOIN       |--> |  NUMBER OF NODES 
C |________________|____|______________________________________________ 
C MODE : -->(INPUT), <--(RESULT), <--> (MODIFIED INPUT) 
C----------------------------------------------------------------------- 
C PROGRAMME APPELANT : INIT_AVAI  
C PROGRAMMES APPELES : NONE 
C*********************************************************************** 
C 
      USE BIEF 
      USE DECLARATIONS_TELEMAC 
      USE DECLARATIONS_SISYPHE 
C 
      IMPLICIT NONE   
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
C 
C                                       NPOIN 
      INTEGER, INTENT (INOUT)::NCOUCHES(*) 
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
C 
      INTEGER I , J   
C  
C----------------------------------------------------------------------- 
C 
      DO J=1,NPOIN 
C 
C       BY DEFAULT : UNIFORM BED COMPOSITION 
C 
          NCOUCHES(J) = 1 
C 
      IF (zf%R(J)>454.5D0) THEN 
C  
      AVAIL(J,1,1) = 0.065 
      AVAIL(J,1,2) = 0.045 
      AVAIL(J,1,3) = 0.145 
      AVAIL(J,1,4) = 0.285 
      AVAIL(J,1,5) = 0.345 
      AVAIL(J,1,6) = 0.115 
C  
      elseif(zf%R(J)<453.8D0) then 
C 
      AVAIL(J,1,1) = 0.065D0 
      AVAIL(J,1,2) = 0. 
      AVAIL(J,1,3) = 0. 
      AVAIL(J,1,4) = 0. 
      AVAIL(J,1,5) = 0. 
      AVAIL(J,1,6) = 0.935D0 
C 
	elseif(zf%R(J)<454.5D0.AND.zf%R(J)>453.8D0) then 
!      DGRA = DMAX + (zf%R(J)-453.8D0)*(0.022D0-DMAX)/(454.5D0-453.8D0) 
C 
      AVAIL(J,1,1) = 0.065 
      AVAIL(J,1,2) = 0.045*(zf%R(J)-453.8D0)/(454.5D0-453.8D0) 
      AVAIL(J,1,3) = 0.145*(zf%R(J)-453.8D0)/(454.5D0-453.8D0) 
      AVAIL(J,1,4) = 0.285*(zf%R(J)-453.8D0)/(454.5D0-453.8D0) 
      AVAIL(J,1,5) = 0.345*(zf%R(J)-453.8D0)/(454.5D0-453.8D0) 
      AVAIL(J,1,6) = 1.-(AVAIL(J,1,1)+AVAIL(J,1,2)+AVAIL(J,1,3)
     &                    +AVAIL(J,1,4)+AVAIL(J,1,5)) 
C 
      ENDIF       
      ENDDO         
C  TO BE FILLED BY THE USER 
!      NCOUCHES(J) = 10 
!        ES(1,J) = 1 
!      ES(2,J) = 1 
!      ES(3,J) = 1 
!        ES(4,J) = 1 
!      ES(5,J) = 1 
!      ES(6,J) = 1 
!        ES(7,J) = 1 
!      ES(8,J) = 1 
!      ES(9,J) = 1       
!         DO I = 1, NSICLA 
!          DO K = 1, NCOUCHES(J) 
!          AVAIL(J,K,I) = AVA0(I) 
!       ENDDO 
!        ENDDO 
C           
C 
      RETURN 
      END 
