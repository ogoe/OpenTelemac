C                       ***************
                        SUBROUTINE BORH
C                       ***************
C
C***********************************************************************
C
C  ARTEMIS    VERSION 6.1 28/06/11   D. AELBRECHT (LNH) 01 30 87 74 12 
C
C  LINKED TO BIEF VERS. 5.0          J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C      FONCTION:    PREND EN COMPTE LES CONDITIONS AUX LIMITES
C                   DE L'UTILISATEUR
C                   ELLES SONT DONNEES PAR SEGMENT.
C
C      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   RP           |<-- |  COEFFICIENTS DE REFLEXION DES PAROIS        |
C |   TETAP        |<-- |  ANGLE D'ATTAQUE DE LA HOULE SUR LES LIMITES |
C |                |    |  PAS SEULEMENT LES PAROIS, MAIS AUSSI LES    |
C |                |    |  LES FRONTIERES LIQUIDES                     |
C |                |    |  (COMPTE PAR RAPPORT A LA NORMALE EXTERIEURE |
C |                |    |   DANS LE SENS DIRECT)                       |
C |   ALFAP        |<-- |  DEPHASAGE INDUIT PAR LA PAROI ENTRE L'ONDE  |
C |                |    |  REFLECHIE ET L'ONDE INCIDENTE (SI ALFAP EST |
C |                |    |  POSITIF, L'ONDE REFLECHIE EST EN RETARD)    |
C |   HB           |<-- |  HAUTEUR DE LA HOULE AUX FRONTIERES OUVERTES |
C |   TETAB        |<-- |  ANGLE D'ATTAQUE DE LA HOULE (FRONT. OUV.)   |
C |                |    |  (COMPTE PAR RAPPORT A L'AXE DES X DANS LE   |
C |                |    |   SENS DIRECT)                               |
C |    H           | -->|  HAUTEUR D'EAU                               |
C |    K           | -->|  NOMBRE D'ONDE                               |
C |    C,CG        | -->|  VITESSES DE PHASE ET DE GROUPE              |
C |    C           | -->|  CELERITE AU TEMPS N                         |
C |    ZF          | -->|  FOND                                        |
C |    X,Y         | -->|  COORDONNEES DES POINTS DU MAILLAGE          |
C |  TRA01,...,3   |<-->|  TABLEAUX DE TRAVAIL                         |
C | XSGBOR,YSGBOR  | -->|  NORMALES EXTERIEURES AUX SEGMENTS DE BORD   |
C |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
C |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
C |   KP1BOR       | -->|  NUMERO DU POINT FRONTIERE SUIVANT           |
C |   OMEGA        | -->|  PULSATION DE LA HOULE                       |
C |   PER          | -->|  PERIODE DE LA HOULE                         |
C |   TETAH        | -->|  ANGLE DE PROPAGATION DE LA HOULE            |
C |   GRAV         | -->|  GRAVITE                                     |
C |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
C |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
C |   KENT,KLOG    | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
C |   KSORT,KINC   |    |  LIMITES                                     |
C |                |    |  KENT  : ENTREE (VALEUR IMPOSEE)             |
C |                |    |  KLOG  : PAROI                               |
C |                |    |  KSORT : SORTIE                              |
C |                |    |  KINC  : ONDE INCIDENTE                      |
C |   PRIVE        | -->|  TABLEAU DE TRAVAIL (DIMENSION DANS PRINCI)  |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : ARTEMI
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
C
      DOUBLE PRECISION PI,BID         
C
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C           
C                                                                   
C JCB :    
      INTEGER IG  ,JB            
      DOUBLE PRECISION R0,R1  ,PHASOI,DEGRAD,X0,Y0                                                                 
      DOUBLE PRECISION AUXIC,AUXIS,KK                                                                
C
C
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
      INTRINSIC COS,SIN
C
C-----------------------------------------------------------------------
C
C CONDITIONS AUX LIMITES
C UN SEGMENT EST SOLIDE SI IL EST DE TYPE KLOG.
C UN SEGMENT EST ONDE INCIDENTE SI IL EST DE TYPE KINC.
C UN SEGMENT EST UNE ENTREE SI IL EST DE TYPE KENT.
C UN SEGMENT EST UNE SORTIE SI IL EST DE TYPE KSORT.
C
C TOUS LES ANGLES SONT EN DEGRES
C                         ------
C ---------------------------------------
C INITIALISATION DES VARIABLES PAR DEFAUT
C ---------------------------------------
      TETAB%R(:) = TETAH
      TETAP%R(:) = 0.D0
      ALFAP%R(:) = 0.D0
      RP%R(:)    = 0.D0
      HB%R(:)    = 1.D0 

C PHASE INITIALISATION      
      DEGRAD=PI/180.D0
      PHASOI=0.D0
      X0=-240000.D0
      Y0=-240000.D0


      DO I=1,NPTFR
       JB=BOUNDARY_COLOUR%I(I)

C PAROIS LIQUIDES - FRONTIERE ONDE INCIDENTE
C
      IF(JB.GE.1.AND.JB.LE.192)THEN
	 LIHBOR%I(I)=KINC
	 HB%R(I)=1.D0
	 TETAB%R(I)=90.D0
CCP ---- PHASE : K and THETAB are the same everywhere on the boundary	 
	 IG   = MESH%NBOR%I(I)
	 AUXIC =COS(TETAB%R(I)*DEGRAD)
         AUXIS =SIN(TETAB%R(I)*DEGRAD)
	 KK=K%R(IG)
	 PHASOI=KK*AUXIC*(X(IG)-X0)+KK*AUXIS*(Y(IG)-Y0)
         ALFAP%R(I) = PHASOI/DEGRAD
      ENDIF
C
C PAROIS SOLIDES
      IF(JB.GE.193.AND.JB.LE.352)THEN
	 LIHBOR%I(I)=KLOG
	 RP%R(I)=1.D0
	 TETAP%R(I)=0.D0
	 ALFAP%R(I)=0.D0
      ENDIF

      ENDDO
      
      RETURN                                                            
      END 

C                       *****************
                        SUBROUTINE ART_CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE ART_CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
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
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I,II
C
      DOUBLE PRECISION PI,BID
C
      PARAMETER( PI = 3.1415926535897932384626433D0)
C
C
C     ---------------------------------------- 
C     VOS NOUVELLES DECLARATIONS DE VARIABLES :
C     ---------------------------------------- 
C                                                                       
C JCB :                                                                       
      DOUBLE PRECISION R0,R1,R2
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  LISSAGES EVENTUELS DU FOND
      DO  I = 1,NPOIN
        ZF%R(I) = 0.D0
      ENDDO
C
c      IF(LISFON.GT.0) THEN
C
c        MAS=.TRUE.
c        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
c     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
c      ENDIF
C                                                            
C RAYON DU CYLINDRE
C      R0 = 1.D50
C      DO 120 I=1,NPOIN
C         IF ( R0.GT.SQRT( X(I)**2 + Y(I)**2 ) ) THEN
C            R0 = SQRT( X(I)**2 + Y(I)**2 )
C         ENDIF
C 120   CONTINUE
C
C     RAYON DE L'ETENDUE DU FOND PARABOLIQUE
      R0 = 39850.D0
C      WRITE(LU,*) 'R0=',R0
      R1 = 3.D0*R0
C
C-----------------------------------------------------------------------
C
      DO 11 II = 1 , NPOIN
C
         R2 = X(II)**2 + Y(II)**2
	 IF (R2.LE.R1**2) ZF%R(II) = 4000.D0*(1.D0-R2/R1**2)
C
11    CONTINUE
C
C
      RETURN
      END                                                      


 

!                    *****************
                     SUBROUTINE CORRXY
!                    *****************
!
     & (X,Y,NPOIN)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE COORDINATES OF THE POINTS IN THE MESH.
!
!warning  USER SUBROUTINE; COMMENTED LINES ARE AN EXAMPLE
!code
!+  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!+            CHANGES THE ORIGIN
!+
!+      DO I = 1 , NPOIN
!+         X(I) = 3.D0 * X(I) + 100.D0
!+         Y(I) = 5.D0 * Y(I) - 50.D0
!+      ENDDO
!warning  DO NOT PERFORM ROTATIONS AS IT WILL CHANGE
!+            THE NUMBERING OF THE LIQUID BOUNDARIES
!
!history  EMILE RAZAFINDRAKOTO (LNHE)
!+        17/10/05
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| X              |<->| ABSCISSAE OF POINTS IN THE MESH
!| X,Y            |<->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CORRXY => CORRXY
!
!
!     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EACH PROGRAM
!
!     USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!     THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!     BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!     ALL THE DATA STRUCTURE OF THIS CODE IS AVAILABLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
       INTEGER I
!
!-----------------------------------------------------------------------
!
!  EXAMPLE : MULTIPLIES BY A CONSTANT (SCALES THE MESH)
!            CHANGES THE ORIGIN
!
      	DO I=1,NPOIN
	X(I)=X(I)*400.D0
	Y(I)=Y(I)*400.D0
	ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'CORRXY (BIEF) : PAS DE MODIFICATION DES COORDONNEES'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)'CORRXY (BIEF):NO MODIFICATION OF COORDINATES'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
 
