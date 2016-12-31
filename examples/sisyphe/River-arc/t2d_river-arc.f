!                       *****************
                        SUBROUTINE CORSTR
!                       *****************
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!      FONCTION: CORRECTION DU COEFFICIENT DE FROTTEMENT SUR LE FOND
!                QUAND IL EST VARIABLE EN TEMPS.
!
!      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
!      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
!
!
!
!
!-----------------------------------------------------------------------
!  EXAMPLE OF POSSIBLE ARGUMENTS
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
! |    H           | -->|  HAUTEUR D'EAU.
! |    AT          | -->|  TIME.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : TELMAC
!
!  SOUS-PROGRAMME APPELE :
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER I
!
      INTEGER NPMAX2,ND,NG
!
      PARAMETER (NPMAX2=200)
      DOUBLE PRECISION XD(NPMAX2),YD(NPMAX2)
      DOUBLE PRECISION XG(NPMAX2),YG(NPMAX2)
!
      DOUBLE PRECISION ZTAMPON,DISTOT,DIST
!
      DOUBLE PRECISION Q,DGRA,DMAX
      EXTERNAL         Q
!
!-----------------------------------------------------------------------
!
      ND=17
!
      XD(01)=462.665
      YD(01)=1186.187
      XD(02)=449.843
      YD(02)=1184.821
      XD(03)=446.900
      YD(03)=1181.878
      XD(04)=463.611
      YD(04)=1148.455
      XD(05)=480.638
      YD(05)=1118.921
      XD(06)=494.617
      YD(06)=1098.005
      XD(07)=504.181
      YD(07)=1082.765
      XD(08)=511.854
      YD(08)=1070.783
      XD(09)=527.199
      YD(09)=1046.504
      XD(10)=544.331
      YD(10)=1017.811
      XD(11)=548.325
      YD(11)=1011.505
      XD(12)=552.529
      YD(12)=1004.358
      XD(13)=558.625
      YD(13)= 993.847
      XD(14)= 568.715
      YD(14)= 972.091
      XD(15)= 620.952
      YD(15)= 861.942
      XD(16)=627.468
      YD(16)= 869.615
      XD(17)= 652.378
      YD(17)= 934.779
!
      NG=13
!
      XG(01)=400.759
      YG(01)=1162.329
      XG(02)=413.372
      YG(02)=1166.112
      XG(03)=431.134
      YG(03)=1129.116
      XG(04)=481.584
      YG(04)=1031.474
      XG(05)=491.043
      YG(05)=1014.868
      XG(06)=494.407
      YG(06)=1008.667
      XG(07)=518.896
      YG(07)= 966.941
      XG(08)=530.142
      YG(08)= 948.232
      XG(09)=557.574
      YG(09)= 899.884
      XG(10)=567.559
      YG(10)= 880.230
      XG(11)=573.129
      YG(11)= 866.146
      XG(12)=581.538
      YG(12)= 844.915
      XG(13)=568.400
      YG(13)= 840.606
!
!
      DO I = 1 , NPOIN
        IF (INPOLY(MESH%X%R(I),MESH%Y%R(I),XD,YD,ND)) THEN
          CHESTR%R(I) = 1.D0
!         CHESTR%R(I) = 0.3D0
        ENDIF

        IF (INPOLY(MESH%X%R(I),MESH%Y%R(I),XG,YG,NG)) THEN
          CHESTR%R(I) = 1.D0
!          CHESTR%R(I) = 0.3D0
        ENDIF

      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE NOEROD
!                       *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE VERSION 5.1                             C. LENORMANT
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!***********************************************************************
!
!     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
!
!
!     RQ: LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
!     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR
!     CHOISIR DE LISSER LA SOLUTION OBTENUE i.e NLISS > 0.
!
!     FUNCTION  : IMPOSE THE RIGID BED LEVEL  ZR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   H            | -->| WATER DEPTH
! |   ZF           | -->| BED LEVEL
! |   ZR           |<-- | RIGID BED LEVEL
! |   Z            | -->| FREE SURFACE
! |   X,Y          | -->| 2D COORDINATES
! |   NPOIN        | -->| NUMBER OF 2D POINTS
! |   CHOIX        | -->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
! |   NLISS        |<-->| NUMBER OF SMOOTHINGS
! |________________|____|______________________________________________
! MODE : -->(INPUT), <--(RESULT), <-->(MODIFIED DATA)
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!-----------------------------------------------------------------------
      INTEGER I
!
      INTEGER NPMAX2
      INTEGER ND
      INTEGER NG
!
! deja defini dans bief
!      PARAMETER (NPMAX=2000)
      PARAMETER (NPMAX2=200)
      DOUBLE PRECISION XD(NPMAX2),YD(NPMAX2)
      DOUBLE PRECISION XG(NPMAX2),YG(NPMAX2)
!--------------------
! RIGID BEDS POSITION
!---------------------
!
!       DEFAULT VALUE:       ZR=ZF-100
!
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-1000.D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
      ND=17
!
      XD(01)=462.665
      YD(01)=1186.187
      XD(02)=449.843
      YD(02)=1184.821
      XD(03)=446.900
      YD(03)=1181.878
      XD(04)=463.611
      YD(04)=1148.455
      XD(05)=480.638
      YD(05)=1118.921
      XD(06)=494.617
      YD(06)=1098.005
      XD(07)=504.181
      YD(07)=1082.765
      XD(08)=511.854
      YD(08)=1070.783
      XD(09)=527.199
      YD(09)=1046.504
      XD(10)=544.331
      YD(10)=1017.811
      XD(11)=548.325
      YD(11)=1011.505
      XD(12)=552.529
      YD(12)=1004.358
      XD(13)=558.625
      YD(13)= 993.847
      XD(14)= 568.715
      YD(14)= 972.091
      XD(15)= 620.952
      YD(15)= 861.942
      XD(16)=627.468
      YD(16)= 869.615
      XD(17)= 652.378
      YD(17)= 934.779
!
      NG=13
!
      XG(01)=400.759
      YG(01)=1162.329
      XG(02)=413.372
      YG(02)=1166.112
      XG(03)=431.134
      YG(03)=1129.116
      XG(04)=481.584
      YG(04)=1031.474
      XG(05)=491.043
      YG(05)=1014.868
      XG(06)=494.407
      YG(06)=1008.667
      XG(07)=518.896
      YG(07)= 966.941
      XG(08)=530.142
      YG(08)= 948.232
      XG(09)=557.574
      YG(09)= 899.884
      XG(10)=567.559
      YG(10)= 880.230
      XG(11)=573.129
      YG(11)= 866.146
      XG(12)=581.538
      YG(12)= 844.915
      XG(13)=568.400
      YG(13)= 840.606
!
      DO I = 1 , NPOIN

        IF (INPOLY(X(I),Y(I),XD,YD,ND)) THEN
          ZR(I) = ZF(I)
          ENDIF

        IF (INPOLY(X(I),Y(I),XG,YG,NG)) THEN
          ZR(I) = ZF(I)
          ENDIF

      ENDDO
!
      RETURN
      END SUBROUTINE NOEROD
!                         *********************
                          SUBROUTINE INIT_COMPO
!                         *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE VERSION 5.3
!                             Matthieu GONZALES DE LINARES 2002
!
!
! COPYRIGHT EDF-BAW-IFH
!***********************************************************************
!
!     FONCTION  : DISTRIBUTION DES CLASSES
!                 % PAR COUCHE, STRATIFICATION
!     SUBROUTINE A REMPLIR PAR l'UTILISATEUR
!
!
!     FUNCTION  : INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!                 VARIATION IN SPACE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                |    |
! |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE
! |    AVAIL(10,NSICLA,NPOIN)
! |    ES          |<-- |  THICKNESS FOR EACH LAYER AND NODE
! |    ES(10,NPOIN)
! |    NCOUCHES     |--> |  NUMBER OF LAYER FOR EACH POINT
! |    NSICLA      |--> |  NUMBER OF SIZE-CLASSES OF BED MATERIAL
!                           (LESS THAN 10)
! |    NPOIN       |--> |  NUMBER OF NODES
! |________________|____|______________________________________________
! MODE : -->(INPUT), <--(RESULT), <--> (MODIFIED INPUT)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : INIT_AVAI
! PROGRAMMES APPELES : NONE
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I , J
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       BY DEFAULT : UNIFORM BED COMPOSITION
!
          NCOUCHES(J) = 1
!
      IF (ZF%R(J)>454.5D0) THEN
!
      AVAIL(J,1,1) = 0.065
      AVAIL(J,1,2) = 0.045
      AVAIL(J,1,3) = 0.145
      AVAIL(J,1,4) = 0.285
      AVAIL(J,1,5) = 0.345
      AVAIL(J,1,6) = 0.115
!
      ELSEIF(ZF%R(J)<453.8D0) THEN
!
      AVAIL(J,1,1) = 0.065D0
      AVAIL(J,1,2) = 0.
      AVAIL(J,1,3) = 0.
      AVAIL(J,1,4) = 0.
      AVAIL(J,1,5) = 0.
      AVAIL(J,1,6) = 0.935D0
!
      ELSEIF(ZF%R(J)<454.5D0.AND.ZF%R(J)>453.8D0) THEN
!      DGRA = DMAX + (zf%R(J)-453.8D0)*(0.022D0-DMAX)/(454.5D0-453.8D0)
!
      AVAIL(J,1,1) = 0.065
      AVAIL(J,1,2) = 0.045*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,3) = 0.145*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,4) = 0.285*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,5) = 0.345*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,6) = 1.-(AVAIL(J,1,1)+AVAIL(J,1,2)+AVAIL(J,1,3)
     &                    +AVAIL(J,1,4)+AVAIL(J,1,5))
!
      ENDIF
      ENDDO
!  TO BE FILLED BY THE USER
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
!
!
      RETURN
      END
