C                       ***************** 
                        SUBROUTINE CORSTR 
C                       ***************** 
C 
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
      INTEGER I,n,nchen 
      DOUBLE PRECISION xchen(10),ychen(10) 
C 
      DOUBLE PRECISION Q 
      EXTERNAL         Q
C 
C----------------------------------------------------------------------- 
C 
      DO I = 1 , NPOIN 
        IF(Q(1)<1500.D0) THEN 
        CHESTR%R(I)=-2.097D-8*Q(1)**3+8.8764D-5*Q(1)**2
     *           -1.26853047D-1 *  Q(1)+91.128939535D0 
        ELSE 
        CHESTR%R(I)=1.98823D-6*Q(1)**2-1.25161242D-2*Q(1)+43.1D0 
        ENDIF 
        IF (Q(1)<308.D0) THEN 
          CHESTR%R(I)= 60.D0 
        ENDIF 
      ENDDO   
C 
      OPEN (61,file='../epis_sx/chen.sx',status='old') 
      read (61,*) Nchen 
      DO n=1,nchen 
        read (61,*) xchen(n),ychen(n) 
      ENDDO 
!       
      CLOSE(61) 
      DO I=1,NPOIN 
        IF (inpoly(mesh%x%R(I),mesh%y%R(I),xchen,ychen,nchen)) THEN 
          CHESTR%R(I)=-9.84D-10*Q(1)**3+1.1182D-5*Q(1)**2-4.1862D-2 
     *              *  Q(1)+82.012D0 
          IF(Q(1)<643.D0) CHESTR%R(I)= 60.D0  
        ENDIF 
      ENDDO
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
      USE DECLARATIONS_SISYPHE, ONLY : MESH,NPTFR
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  k , n  
      DOUBLE PRECISION c 
C 
      INTEGER NPMAX2 
      INTEGER NPM6 
      INTEGER NPM7 
      INTEGER NPM8 
      INTEGER NPM9 
      INTEGER NPM10 
      INTEGER NPM11 
      INTEGER NPM12 
      INTEGER NPM13 
      INTEGER NPM14 
      INTEGER NPM15 
      INTEGER NPM16 
      INTEGER NPM17 
      INTEGER NPM18 
      INTEGER NPM19 
      INTEGER NPM20 
      INTEGER NPM21 
      INTEGER NPM22 
      INTEGER NPM23 
      INTEGER NPMCAI 
      INTEGER NPM1 
      INTEGER NPM2 
      INTEGER NPM3 
      INTEGER NPM4 
      INTEGER NPM31 
      INTEGER NPM32 
      INTEGER NPM33 
      INTEGER NPM34 
      INTEGER NPM36 
      INTEGER NPM37 
      INTEGER NPM38 
      INTEGER NPM39 
      INTEGER NPM40 
      INTEGER NPM41 
      INTEGER NPM351 
      INTEGER NPM352 
      INTEGER NPM50 
      INTEGER NPM60 
      INTEGER NPMSE1 
      INTEGER NPMSE2 
C 
      INTEGER NPMS1 
      INTEGER NPMS2 
      INTEGER NPMS3 
      INTEGER NPMS4 
      INTEGER NPMS5 
      INTEGER NPMS6 
      INTEGER NPMS7 
      INTEGER NPMS8 
      INTEGER NPMS9 
      INTEGER NPMS10 
      INTEGER NPMS11 
      INTEGER NPMS12 
      INTEGER NPMS13 
      INTEGER NPMS14 
      INTEGER NPMS15 
      INTEGER NPMS16 
      INTEGER NPMS17 
      INTEGER NPMS18 
      INTEGER NPMS19 
      INTEGER NPMS20 
      INTEGER NPMS21 
      INTEGER NPMS22 
      INTEGER NNE1 
C                                           
      PARAMETER (NPMAX2=200)        
      DOUBLE PRECISION X6(NPMAX2),Y6(NPMAX2) 
      DOUBLE PRECISION X7(NPMAX2),Y7(NPMAX2) 
      DOUBLE PRECISION X8(NPMAX2),Y8(NPMAX2) 
      DOUBLE PRECISION X9(NPMAX2),Y9(NPMAX2) 
      DOUBLE PRECISION X10(NPMAX2),Y10(NPMAX2) 
      DOUBLE PRECISION X11(NPMAX2),Y11(NPMAX2) 
      DOUBLE PRECISION X12(NPMAX2),Y12(NPMAX2) 
      DOUBLE PRECISION X13(NPMAX2),Y13(NPMAX2) 
      DOUBLE PRECISION X14(NPMAX2),Y14(NPMAX2) 
      DOUBLE PRECISION X15(NPMAX2),Y15(NPMAX2) 
      DOUBLE PRECISION X16(NPMAX2),Y16(NPMAX2) 
      DOUBLE PRECISION X17(NPMAX2),Y17(NPMAX2) 
      DOUBLE PRECISION X18(NPMAX2),Y18(NPMAX2) 
      DOUBLE PRECISION X19(NPMAX2),Y19(NPMAX2) 
      DOUBLE PRECISION X20(NPMAX2),Y20(NPMAX2) 
      DOUBLE PRECISION X21(NPMAX2),Y21(NPMAX2) 
      DOUBLE PRECISION X22(NPMAX2),Y22(NPMAX2) 
      DOUBLE PRECISION X23(NPMAX2),Y23(NPMAX2)       
      DOUBLE PRECISION XCAI(NPMAX2),YCAI(NPMAX2) 
      DOUBLE PRECISION X1(NPMAX2),Y1(NPMAX2) 
      DOUBLE PRECISION X2(NPMAX2),Y2(NPMAX2) 
      DOUBLE PRECISION X3(NPMAX2),Y3(NPMAX2) 
      DOUBLE PRECISION X4(NPMAX2),Y4(NPMAX2) 
      DOUBLE PRECISION X31(NPMAX2),Y31(NPMAX2) 
      DOUBLE PRECISION X32(NPMAX2),Y32(NPMAX2) 
      DOUBLE PRECISION X33(NPMAX2),Y33(NPMAX2) 
      DOUBLE PRECISION X34(NPMAX2),Y34(NPMAX2) 
      DOUBLE PRECISION X36(NPMAX2),Y36(NPMAX2) 
      DOUBLE PRECISION X37(NPMAX2),Y37(NPMAX2) 
      DOUBLE PRECISION X38(NPMAX2),Y38(NPMAX2) 
      DOUBLE PRECISION X39(NPMAX2),Y39(NPMAX2) 
      DOUBLE PRECISION X40(NPMAX2),Y40(NPMAX2) 
      DOUBLE PRECISION X41(NPMAX2),Y41(NPMAX2) 
      DOUBLE PRECISION X351(NPMAX2),Y351(NPMAX2) 
      DOUBLE PRECISION X352(NPMAX2),Y352(NPMAX2) 
      DOUBLE PRECISION X50(NPMAX2),Y50(NPMAX2) 
      DOUBLE PRECISION X60(NPMAX2),Y60(NPMAX2)       
      DOUBLE PRECISION XSE1(NPMAX2),YSE1(NPMAX2) 
      DOUBLE PRECISION XSE2(NPMAX2),YSE2(NPMAX2) 
C 
      DOUBLE PRECISION XS1(NPMAX2),YS1(NPMAX2) 
      DOUBLE PRECISION XS2(NPMAX2),YS2(NPMAX2) 
      DOUBLE PRECISION XS3(NPMAX2),YS3(NPMAX2) 
      DOUBLE PRECISION XS4(NPMAX2),YS4(NPMAX2) 
      DOUBLE PRECISION XS5(NPMAX2),YS5(NPMAX2) 
      DOUBLE PRECISION XS6(NPMAX2),YS6(NPMAX2) 
      DOUBLE PRECISION XS7(NPMAX2),YS7(NPMAX2) 
      DOUBLE PRECISION XS8(NPMAX2),YS8(NPMAX2) 
      DOUBLE PRECISION XS9(NPMAX2),YS9(NPMAX2) 
      DOUBLE PRECISION XS10(NPMAX2),YS10(NPMAX2) 
      DOUBLE PRECISION XS11(NPMAX2),YS11(NPMAX2) 
      DOUBLE PRECISION XS12(NPMAX2),YS12(NPMAX2) 
      DOUBLE PRECISION XS13(NPMAX2),YS13(NPMAX2) 
      DOUBLE PRECISION XS14(NPMAX2),YS14(NPMAX2) 
      DOUBLE PRECISION XS15(NPMAX2),YS15(NPMAX2) 
      DOUBLE PRECISION XS16(NPMAX2),YS16(NPMAX2) 
      DOUBLE PRECISION XS17(NPMAX2),YS17(NPMAX2) 
      DOUBLE PRECISION XS18(NPMAX2),YS18(NPMAX2) 
      DOUBLE PRECISION XS19(NPMAX2),YS19(NPMAX2) 
      DOUBLE PRECISION XS20(NPMAX2),YS20(NPMAX2) 
      DOUBLE PRECISION XS21(NPMAX2),YS21(NPMAX2) 
      DOUBLE PRECISION XS22(NPMAX2),YS22(NPMAX2) 
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
C 
C 
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
C 
      LOGICAL MAS 
C 
C----------------------------------------------------------------------- 
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-100.D0,NPOIN)   
C 
C  LISSAGES EVENTUELS DU FOND 
C 
      print *,'DANS NOEROD' 
C 
      OPEN (61,file='../epis_sx/epi6.sx',status='old') 
      read (61,*) NPM6 
      DO n=1,npm6 
        read (61,*) x6(n),y6(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npm6 
      CLOSE(61) 
C 
      OPEN (62,file='../epis_sx/epi7.sx',status='old') 
      read (62,*) NPM7 
      DO n=1,npm7 
        read (62,*) x7(n),y7(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 7',npm7 
      CLOSE(62) 
C 
      OPEN (63,file='../epis_sx/epi8.sx',status='old') 
      read (63,*) NPM8 
      DO n=1,npm8 
        read (63,*) x8(n),y8(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 8',npm8 
      CLOSE(63) 
C 
      OPEN (64,file='../epis_sx/epi9.sx',status='old') 
      read (64,*) NPM9 
      DO n=1,npm9 
        read (64,*) x9(n),y9(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 9',npm9 
      CLOSE(64) 
C 
      OPEN (65,file='../epis_sx/epi10.sx',status='old') 
      read (65,*) NPM10 
      DO n=1,npm10 
        read (65,*) x10(n),y10(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 10',npm10 
      CLOSE(65) 
C 
      OPEN (66,file='../epis_sx/epi11.sx',status='old') 
      read (66,*) NPM11 
      DO n=1,npm11 
        read (66,*) x11(n),y11(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 11',npm11 
      CLOSE(66) 
C 
      OPEN (67,file='../epis_sx/epi12.sx',status='old') 
      read (67,*) NPM12 
      DO n=1,npm12 
        read (67,*) x12(n),y12(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 12',npm12 
      CLOSE(67) 
C 
      OPEN (68,file='../epis_sx/epi13.sx',status='old') 
      read (68,*) NPM13 
      DO n=1,npm13 
        read (68,*) x13(n),y13(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 13',npm13 
      CLOSE(68) 
C 
      OPEN (69,file='../epis_sx/epi14.sx',status='old') 
      read (69,*) NPM14 
      DO n=1,npm14 
        read (69,*) x14(n),y14(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 14',npm14 
      CLOSE(69) 
C 
      OPEN (70,file='../epis_sx/epi15.sx',status='old') 
      read (70,*) NPM15 
      DO n=1,npm15 
        read (70,*) x15(n),y15(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 15',npm15 
      CLOSE(70) 
! 
!     seuil 2 ! 
      OPEN (71,file='../epis_sx/epi16.sx',status='old') 
      read (71,*) NPM16 
      DO n=1,npm16 
        read (71,*) x16(n),y16(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 16',npm16 
      CLOSE(71) 
C 
      OPEN (72,file='../epis_sx/epi17.sx',status='old') 
      read (72,*) NPM17 
      DO n=1,npm17 
        read (72,*) x17(n),y17(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 17',npm17 
      CLOSE(72) 
! 
!     seuil 1 
      OPEN (73,file='../epis_sx/epi18.sx',status='old') 
      read (73,*) NPM18 
      DO n=1,npm18 
        read (73,*) x18(n),y18(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 18',npm18 
      CLOSE(73) 
C 
      OPEN (74,file='../epis_sx/epi19.sx',status='old') 
      read (74,*) NPM19 
      DO n=1,npm19 
        read (74,*) x19(n),y19(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 19',npm19 
      CLOSE(74) 
C 
      OPEN (75,file='../epis_sx/epi20.sx',status='old') 
      read (75,*) NPM20 
      DO n=1,npm20 
        read (75,*) x20(n),y20(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 20',npm20 
      CLOSE(75) 
C 
      OPEN (76,file='../epis_sx/epi21.sx',status='old') 
      read (76,*) NPM21 
      DO n=1,npm21 
        read (76,*) x21(n),y21(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 21',npm13 
      CLOSE(76) 
C 
      OPEN (77,file='../epis_sx/epi22.sx',status='old') 
      read (77,*) NPM22 
      DO n=1,npm22 
        read (77,*) x22(n),y22(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 22',npm22 
      CLOSE(77) 
C 
      OPEN (78,file='../epis_sx/epi23.sx',status='old') 
      read (78,*) NPM23 
      DO n=1,npm23 
        read (78,*) x23(n),y23(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 23',npm23 
      CLOSE(78) 
C 
      OPEN (79,file='../epis_sx/epi1.sx',status='old') 
      read (79,*) NPM1 
      DO n=1,npm1 
        read (79,*) X1(n),Y1(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm1 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi2.sx',status='old') 
      read (79,*) NPM2 
      DO n=1,npm2 
        read (79,*) X2(n),Y2(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm2 
      CLOSE(79) 
       OPEN (79,file='../epis_sx/epi3.sx',status='old') 
      read (79,*) NPM3 
      DO n=1,npm3 
        read (79,*) X3(n),Y3(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm3 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi4.sx',status='old') 
      read (79,*) NPM4 
      DO n=1,npm4 
        read (79,*) X4(n),Y4(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm4 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi31.sx',status='old') 
      read (79,*) NPM31 
      DO n=1,npm31 
        read (79,*) X31(n),Y31(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm31 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi32.sx',status='old') 
      read (79,*) NPM32 
      DO n=1,npm32 
        read (79,*) X32(n),Y32(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm32 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi33.sx',status='old') 
      read (79,*) NPM33 
      DO n=1,npm33 
        read (79,*) X33(n),Y33(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm33 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi34.sx',status='old') 
      read (79,*) NPM34 
      DO n=1,npm34 
        read (79,*) X34(n),Y34(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm34 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi36.sx',status='old') 
      read (79,*) NPM36 
      DO n=1,npm36 
        read (79,*) X36(n),Y36(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm36 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi37.sx',status='old') 
      read (79,*) NPM37 
      DO n=1,npm37 
        read (79,*) X37(n),Y37(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm37 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi38.sx',status='old') 
      read (79,*) NPM38 
      DO n=1,npm38 
        read (79,*) X38(n),Y38(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm38 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi39.sx',status='old') 
      read (79,*) NPM39 
      DO n=1,npm39 
        read (79,*) X39(n),Y39(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm39 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi40.sx',status='old') 
      read (79,*) NPM40 
      DO n=1,npm40 
        read (79,*) X40(n),Y40(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm40 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi41.sx',status='old') 
      read (79,*) NPM41 
      DO n=1,npm41 
        read (79,*) X41(n),Y41(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm40 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi351.sx',status='old') 
      read (79,*) NPM351 
      DO n=1,npm351 
        read (79,*) X351(n),Y351(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm351 
      CLOSE(79) 
C     
      OPEN (79,file='../epis_sx/epi352.sx',status='old') 
      read (79,*) NPM352 
      DO n=1,npm352 
        read (79,*) X352(n),Y352(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm352 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/epi50.sx',status='old') 
      read (79,*) NPM50 
      DO n=1,npm50 
        read (79,*) X50(n),Y50(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npm50 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/seuil1.sx',status='old') 
      read (79,*) NPMSe1 
      DO n=1,npmSe1 
        read (79,*) XSE1(n),YSE1(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npmSe1 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/seuil2.sx',status='old') 
      read (79,*) NPMSE2 
      DO n=1,npmSe2 
        read (79,*) XSE2(n),YSE2(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npmSe2 
      CLOSE(79) 
C 
      OPEN (79,file='../epis_sx/caisson.sx',status='old') 
      read (79,*) NPMcai 
      DO n=1,npmcai 
        read (79,*) Xcai(n),Ycai(n) 
      ENDDO 
      print *,'FIN LECTURE CAISSON',npmcai 
      CLOSE(79) 
C 
      OPEN (61,file='../berge_noerod/s1.sx',status='old') 
      read (61,*) NPmS1 
      DO n=1,npmS1 
        read (61,*) xs1(n),ys1(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s2.sx',status='old') 
      read (61,*) NPmS2 
      DO n=1,npmS2 
        read (61,*) xs2(n),ys2(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s3.sx',status='old') 
      read (61,*) NPmS3 
      DO n=1,npms3 
        read (61,*) xs3(n),ys3(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms3 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s4.sx',status='old') 
      read (61,*) NPmS4 
      DO n=1,npmS4 
        read (61,*) xs4(n),ys4(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms4 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s5.sx',status='old') 
      read (61,*) NPmS5 
      DO n=1,npmS5 
        read (61,*) xs5(n),ys5(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms5 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s6.sx',status='old') 
      read (61,*) NPmS6 
      DO n=1,npmS6 
        read (61,*) xs6(n),ys6(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s7.sx',status='old') 
      read (61,*) NPmS7 
      DO n=1,npms7 
        read (61,*) xs7(n),ys7(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms7 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s8.sx',status='old') 
      read (61,*) NPms8 
      DO n=1,npms8 
        read (61,*) xs8(n),ys8(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s9.sx',status='old') 
      read (61,*) NPms9 
      DO n=1,npms9 
        read (61,*) xs9(n),ys9(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms9 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s10.sx',status='old') 
      read (61,*) NPms10 
      DO n=1,npms10 
        read (61,*) xs10(n),ys10(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms10 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s11.sx',status='old') 
      read (61,*) NPmS11 
      DO n=1,npmS11 
        read (61,*) xs11(n),ys11(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms11 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s12.sx',status='old') 
      read (61,*) NPmS12 
      DO n=1,npmS12 
        read (61,*) xs12(n),ys12(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms12 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s1.sx',status='old') 
      read (61,*) NPmS13 
      DO n=1,npmS13 
        read (61,*) xs13(n),ys13(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms13 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s14.sx',status='old') 
      read (61,*) NPmS14 
      DO n=1,npmS14 
        read (61,*) xs14(n),ys14(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms14 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s15.sx',status='old') 
      read (61,*) NPmS15 
      DO n=1,npmS15 
        read (61,*) xs15(n),ys15(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms15 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s16.sx',status='old') 
      read (61,*) NPmS16 
      DO n=1,npmS16 
        read (61,*) xs16(n),ys16(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s17.sx',status='old') 
      read (61,*) NPmS17 
      DO n=1,npmS17 
        read (61,*) xs17(n),ys17(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s18.sx',status='old') 
      read (61,*) NPmS18 
      DO n=1,npmS18 
        read (61,*) xs18(n),ys18(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s19.sx',status='old') 
      read (61,*) NPmS19 
      DO n=1,npmS19 
        read (61,*) xs19(n),ys19(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s20.sx',status='old') 
      read (61,*) NPmS20 
      DO n=1,npmS20 
        read (61,*) xs20(n),ys20(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s21.sx',status='old') 
      read (61,*) NPmS21 
      DO n=1,npmS21 
        read (61,*) xs21(n),ys21(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61) 
C 
      OPEN (61,file='../berge_noerod/s22.sx',status='old') 
      read (61,*) NPmS22 
      DO n=1,npmS22 
        read (61,*) xs22(n),ys22(n) 
      ENDDO 
      print *,'FIN LECTURE seuils 6',npms1 
      CLOSE(61)  
C 
      DO N=1,NPOIN 
      IF (inpoly(x(N),y(N),x6,y6,npm6)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x7,y7,npm7)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x8,y8,npm8)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x9,y9,npm9)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x10,y10,npm10)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x11,y11,npm11)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x12,y12,npm12)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x13,y13,npm13)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x14,y14,npm14)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x15,y15,npm15)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x16,y16,npm16)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x17,y17,npm17)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x18,y18,npm18)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x19,y19,npm19)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x20,y20,npm20)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x21,y21,npm21)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x22,y22,npm22)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x23,y23,npm23)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x1,y1,npm1)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x2,y2,npm2)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x3,y3,npm3)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x4,y4,npm4)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x31,y31,npm31)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x32,y32,npm32)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x33,y33,npm33)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x34,y34,npm34)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x36,y36,npm36)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x37,y37,npm37)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x38,y38,npm38)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x39,y39,npm39)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x40,y40,npm40)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x41,y41,npm41)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x351,y351,npm351)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x352,y352,npm352)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),x50,y50,npm50)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),xSE1,ysE1,npmse1)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),xsE2,ysE2,npmse2)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),xcai,ycai,npmcai)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),xs1,ys1,npms1)) THEN 
          ZR(N)= ZF(N) 
      ELSEIF (inpoly(x(N),y(N),xs2,ys2,npms2)) THEN 
          ZR(N)= ZF(N) 
      ENDIF 
      ENDDO 
C  
      DO K = 1, NPTFR 
         N = MESH%NBOR%I(K) 
            ZR(N)= ZF(N) 
         IF (N==17 
     *   .OR.N==80 
     *   .OR.N==492 
     *   .OR.N==495 
     *   .OR.N==356 
     *   .OR.N==56 
     *   .OR.N==478 
     *   .OR.N==171 
     *   .OR.N==500 
     *   .OR.N==201 
     *   .OR.N==505 
     *   .OR.N==39 
     *   .OR.N==165 
     *   .OR.N==323 
     *   .OR.N==9 ) THEN 
            ZR(N)= ZF(N)-100.D0 
         ENDIF 
      ENDDO  
C 
      RETURN 
      END 
      
!
!  FACTEUR 100 AJOUTE
!
           
      ! ************************ !      
        SUBROUTINE BEDLOAD_MEYER 
      ! ************************ ! 
 
     &  (TETAP, HIDING, HIDFAC, DENS, GRAV, DM, AC, 
     &   ACP, QSC, SLOPEFF, COEFPN) 
 
 
C**********************************************************************C 
C SISYPHE VERSION 5.4  --/10/2003   C.VILLARET                         C 
C SISYPHE VERSION 5.1  11/09/1995  E. PELTIER                          C 
C SISYPHE VERSION 5.1  11/09/1995  C. LENORMANT                        C 
C SISYPHE VERSION 5.1  11/09/1995  J.-M. HERVOUET                      C 
C**********************************************************************C 
 
 
           ! =========================================== ! 
           !   Bed-load transport formula of Meyer-Peter ! 
           ! =========================================== ! 
 
 
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT 
C**********************************************************************C 
C                                                                      C 
C                 SSSS I   SSSS Y   Y PPPP  H   H EEEEE                C 
C                S     I  S      Y Y  P   P H   H E                    C 
C                 SSS  I   SSS    Y   PPPP  HHHHH EEEE                 C 
C                    S I      S   Y   P     H   H E                    C 
C                SSSS  I  SSSS    Y   P     H   H EEEEE                C 
C                                                                      C 
C----------------------------------------------------------------------C 
C                             ARGUMENTS                                C 
C .________________.____.______________________________________________C 
C |      NOM       |MODE|                   ROLE                       C 
C |________________|____|______________________________________________C 
C |________________|____|______________________________________________C 
C                    <=  Can't be change by the user                   C 
C                    =>  Can be changed by the user                    C  
C ---------------------------------------------------------------------C 
!                                                                      ! 
! CALLED BY BEDLOAD_FORMULA                                            ! 
!                                                                      ! 
! CALL      ------                                                     ! 
!                                                                      ! 
!======================================================================! 
!======================================================================! 
!                    DECLARATION DES TYPES ET DIMENSIONS               ! 
!======================================================================! 
!======================================================================! 
 
      ! 1/ MODULES 
      ! ---------- 
      USE INTERFACE_SISYPHE, 
     %    EX_BEDLOAD_MEYER => BEDLOAD_MEYER 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
 
 
      ! 2/ GLOBAL VARIABLES 
      ! ------------------- 
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TETAP, HIDING 
      INTEGER,          INTENT(IN)    :: HIDFAC, SLOPEFF 
      DOUBLE PRECISION, INTENT(IN)    :: DENS, GRAV, DM, AC 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ACP ! work array T1
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: QSC, COEFPN 
 
 
      ! 3/ LOCAL VARIABLES 
      ! ------------------ 
      DOUBLE PRECISION :: C2 
 
 
!======================================================================! 
!======================================================================! 
!                               PROGRAMME                              ! 
!======================================================================! 
!======================================================================! 
 
      CALL OS('X=C     ', X=ACP, C=AC) 
 
      ! **************************************** ! 
      ! 0 - EFFET DE PENTE : FORMULE DE SOULBY   ! 
      ! **************************************** ! 
      IF(SLOPEFF == 2) THEN 
        CALL OS('X=XY    ', X=ACP, Y=COEFPN ) 
      ENDIF 

 
      ! **************************************** ! 
      ! III - TRANSPORT PAR CHARRIAGE AVEC       ! 
      !       CORRECTION POUR LA GRANULO ETENDUE !  
      ! **************************************** ! 

!   FACTEUR 100 AJOUTE POUR CE CAS    !!!!!!!!!!!!!     
      
      C2 = 100.D0 *   8.D0 * SQRT(GRAV*DENS*DM**3) 
      
      
      IF ((HIDFAC == 1) .OR. (HIDFAC == 2) ) THEN 
         CALL OS('X=XY    ', X=ACP, Y=HIDING) 
         CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP) 
         CALL OS('X=+(Y,C)', X=QSC, Y=QSC , C=0.D0) 
         CALL OS('X=Y**C  ', X=QSC, Y=QSC , C=1.5D0) 
         CALL OS('X=CX    ', X=QSC, C=C2) 
      ELSE 
          CALL OS('X=Y-Z   ', X=QSC, Y=TETAP, Z=ACP) 
          CALL OS('X=+(Y,C)', X=QSC, Y=QSC, C=0.D0) 
         CALL OS('X=Y**C  ', X=QSC, Y=QSC, C=1.5D0) 
         CALL OS('X=CX    ', X=QSC, C=C2) 
         CALL OS('X=XY    ', X=QSC, Y=HIDING) 
      ENDIF 
 
!======================================================================! 
!======================================================================! 
 
      RETURN 
      END  
