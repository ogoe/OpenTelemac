!                    *******************
                     SUBROUTINE OIL_FLOT
!                    *******************
!
     &(PARTICULES,NFLOT,NFLOT_MAX,MESH,LT,VOLDEV,RHO_OIL,NB_COMPO,
     &NB_HAP,FMCOMPO,TBCOMPO,FMHAP,TBHAP,SOLU,ETAL,AREA,NPLAN,GRAV)
!
!***********************************************************************
! TELEMAC2D & TELEMAC3D  V6P3                               21/08/2010
!***********************************************************************
!
!brief    THE USER MUST GIVE :
!+
!+
!+   1) THE TIMESTEP WHEN THE FLOATING BODY IS RELEASED.
!+
!+
!+   2) THE TIME WHEN THE COMPUTATION IS STOPPED FOR THIS FLOATING BODY.
!+
!+
!+   3) THE INITIAL POSITION OF THE FLOATING BODY AT THE TIME OF RELEASE.
!
!history  J-M JANIN (LNH)
!+        17/08/1994
!+        V5P2
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
!history  CEDRIC GOEURY (LHSV)
!+        28/06/2013
!+        V6P3
!+   First version
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELTFLO         |<->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| ETAFLO         |<->| LEVELS WHERE ARE THE FLOATS
!| LT             |-->| CURRENT TIME STEP
!| MESH           |<->| MESH STRUCTURE
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPLAN          |-->| NUMBER OF PLANES
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| SHPFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| SHZFLO         |<->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR LEVEL
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| Z              |-->| ELEVATIONS OF POINTS IN THE MESH
!| XFLOT          |<->| ABSCISSAE OF FLOATING BODIES
!| YFLOT          |<->| ORDINATES OF FLOATING BODIES
!| ZFLOT          |<->| ELEVATIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE, ONLY : ADD_PARTICLE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(INOUT)          :: NFLOT
      INTEGER, INTENT(IN)             :: NFLOT_MAX,LT,NPLAN
      INTEGER, INTENT(IN)             :: NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)             :: ETAL
      DOUBLE PRECISION, INTENT(IN)    :: VOLDEV,RHO_OIL,AREA
      DOUBLE PRECISION, INTENT(IN)    :: GRAV
      DOUBLE PRECISION, INTENT(IN)    :: FMCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: TBCOMPO(NB_COMPO)
      DOUBLE PRECISION, INTENT(IN)    :: FMHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: TBHAP(NB_HAP)
      DOUBLE PRECISION, INTENT(IN)    :: SOLU(NB_HAP)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(OIL_PART), INTENT(INOUT)   :: PARTICULES(NFLOT_MAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER                         :: K,J,NUM_GLO,NUM_LOC,NUM_MAX,I
      INTEGER                         :: NFLOT_OIL
      DOUBLE PRECISION                :: RHO_EAU,PI,COEF1
      DOUBLE PRECISION                :: COEF2,DELTA,NU,NU2
      DOUBLE PRECISION                :: COORD_X, COORD_Y
      DOUBLE PRECISION                :: XFLOT(1), YFLOT(1),ZFLOT(1)
      DOUBLE PRECISION                :: SHPFLO(3,1)
      DOUBLE PRECISION                :: SHZFLO(1)
      INTEGER                         :: TAGFLO(1)
      INTEGER                         :: ELTFLO(1)
      INTEGER                         :: ETAFLO(1)     
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS IS AN EXAMPLE !!!!!!!!!!!!!!!!!!!!
!
      RHO_EAU=1000.D0
      PI=ACOS(-1.D0)
!     HARDCODED WATER MOLECULAR VISCOSITY
      NU=1.D-6
      NU2=NU**2
!
      COEF1=1.21D0**4
      COEF2=COEF1/1.53**2
      DELTA=(RHO_EAU-RHO_OIL)/(RHO_EAU)
!
      IF(LT.EQ.10000) THEN 
        NUM_GLO=0
        NUM_MAX=0
        NUM_LOC=0
        COORD_X=0.D0
        COORD_Y=0.D0 
        NUM_MAX=INT(SQRT(REAL(NFLOT_MAX)))
        DO K=0,NUM_MAX-1
          DO J=0,NUM_MAX-1
            COORD_X=336000.D0+REAL(J)
            COORD_Y=371000.D0+REAL(K)
            NUM_GLO=NUM_GLO+1
            NFLOT_OIL = 0
            IF(MESH%DIM.EQ.3)THEN
              CALL ADD_PARTICLE(COORD_X,COORD_Y,COORD_Y,NUM_GLO,
     &           NFLOT_OIL,1,XFLOT,YFLOT,ZFLOT,TAGFLO,SHPFLO,SHZFLO,
     &           ELTFLO,ETAFLO,MESH,NPLAN,0.D0,0.D0,0.D0,0.D0,0,0)
            ELSEIF(MESH%DIM.EQ.2)THEN
              CALL ADD_PARTICLE(COORD_X,COORD_Y,0.D0,NUM_GLO,
     &           NFLOT_OIL,1,XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,
     &           ELTFLO,ELTFLO,MESH,NPLAN,0.D0,0.D0,0.D0,0.D0,0,0)
            END IF
            IF(NFLOT_OIL.EQ.1)THEN
              NUM_LOC = NUM_LOC+1
!=========================================================================
!----INITIALIZATION PARAMETERS FOR THE CALCULATION OF PARTICULE MOTION----
!=========================================================================
              PARTICULES(NUM_LOC)%XOIL = XFLOT(1)
              PARTICULES(NUM_LOC)%YOIL = YFLOT(1)
              PARTICULES(NUM_LOC)%ID = TAGFLO(1)
              PARTICULES(NUM_LOC)%SHPOIL(1) = SHPFLO(1,1)
              PARTICULES(NUM_LOC)%SHPOIL(2) = SHPFLO(2,1)
              PARTICULES(NUM_LOC)%SHPOIL(3) = SHPFLO(3,1)
              PARTICULES(NUM_LOC)%ELTOIL = ELTFLO(1)
              IF(MESH%DIM.EQ.3)THEN
                PARTICULES(NUM_LOC)%ZOIL = ZFLOT(1)
                PARTICULES(NUM_LOC)%ETAOIL = ETAFLO(1)
                PARTICULES(NUM_LOC)%SHZOIL = SHZFLO(1)
              END IF
!=========================================================================
!-----------INITIALIZATION PARAMETERS FOR THE CALCULATION OF OIL----------
!---------------------------WEATHERING PROCESSES--------------------------
!=========================================================================
              PARTICULES(NUM_LOC)%STATE=1
              PARTICULES(NUM_LOC)%TPSECH=0
              IF(ETAL.EQ.1)THEN
                PARTICULES(NUM_LOC)%SURFACE=PI*COEF2*
     &               (DELTA*GRAV/(VOLDEV*NU2))**(1.D0/6.D0)
     &               *VOLDEV/NFLOT_MAX 
              ELSEIF(ETAL.EQ.3)THEN
                PARTICULES(NUM_LOC)%SURFACE = AREA
              ELSEIF(ETAL.EQ.2) THEN
                PARTICULES(NUM_LOC)%SURFACE = 0.D0
              ELSE
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'ETAL=',ETAL,' INCONNU DANS OIL_FLOT'
                ENDIF
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'ETAL=',ETAL,' UNKNOWN IN OIL_FLOT'
                ENDIF
                CALL PLANTE(1)
                STOP
              END IF
              PARTICULES(NUM_LOC)%MASS0 = (VOLDEV*RHO_OIL)/NFLOT_MAX
              PARTICULES(NUM_LOC)%MASS_EVAP=0.D0
              PARTICULES(NUM_LOC)%MASS_DISS=0.D0
              DO I=1,NB_COMPO
                PARTICULES(NUM_LOC)%COMPO(I)%MASS=
     &               PARTICULES(NUM_LOC)%MASS0*FMCOMPO(I)
                PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &               PARTICULES(NUM_LOC)%COMPO(I)%MASS
              END DO
              DO I=1,NB_HAP
                PARTICULES(NUM_LOC)%HAP(I)%MASS=
     &               PARTICULES(NUM_LOC)%MASS0*FMHAP(I)
                PARTICULES(NUM_LOC)%HAP(I)%TB=TBHAP(I)
                 PARTICULES(NUM_LOC)%HAP(I)%SOL=SOLU(I)
                PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &               PARTICULES(NUM_LOC)%HAP(I)%MASS
              END DO
              NFLOT = NUM_LOC
            END IF
          END DO
        END DO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE CORFON
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE
!
!history  J-M HERVOUET (LNHE)
!+        01/03/1990
!+        V5P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
      DO I=1,NPOIN
        IF(X(I).GT.417939.D0.AND.X(I).LT.421750.D0.AND.
     &       Y(I).GT.283000.D0.AND.Y(I).LT.284451.D0      ) THEN
          ZF%R(I)=0.D0
        ENDIF
        IF(X(I).GT.410500.D0.AND.X(I).LT.411750.D0.AND.
     &       Y(I).GT.255750.D0.AND.Y(I).LT.256250.D0      ) THEN
          ZF%R(I)=0.D0
        ENDIF
      ENDDO 
!-----------------------------------------------------------------------
!
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE STRCHE
!                    *****************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BOTTOM FRICTION COEFFICIENT
!+                IF VARIABLE IN SPACE.
!
!note     IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!+         THESE VALUES ARE DISCARDED.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+  COMMENTS CEX MUST BE REMOVED TO IMPLEMENT THE EXAMPLE.
!+  HERE A CONSTANT FRICTION VALUE IS GIVEN:
!+
!+CEX   DO I=1,NPOIN
!+CEX     CHESTR%R(I) = 60.D0
!+CEX   ENDDO
!
!history  J-M HERVOUET (LNH)
!+        01/10/96
!+        V5P2
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
!     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
!     THIS EXAMPLE APPLIES TO TELEMAC2D
!
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!     HERE A CONSTANT FRICTION VALUE IS GIVEN
!
!EX   DO I=1,NPOIN
!EX     CHESTR%R(I) = 60.D0
!EX   ENDDO

      DO I=1,NPOIN
        IF (Y(I)-X(I).GE.10000.D0) THEN
          CHESTR%R(I) = 70.D0
        ELSEIF (X(I).LE.370000.D0) THEN
          CHESTR%R(I) = 75.D0 
        ELSEIF (X(I).LE.374000.D0.AND.Y(I).GE.282000.D0) THEN
          CHESTR%R(I) = 70.D0
        ELSE
          CHESTR%R(I) = 60.D0
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMMENTS HERE MAY BE CHANGED
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'STRCHE (BIEF) : PAS DE MODIFICATION DU FROTTEMENT'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'STRCHE (BIEF): NO MODIFICATION OF FRICTION'
        WRITE(LU,*)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       ***************
                        SUBROUTINE BORD
!                       ***************
!
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,
     & NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.0    24/04/97    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!      FONCTION:    MODIFIE LES TABLEAUX DE CONDITIONS AUX LIMITES
!                   DANS LE CAS OU ELLES SONT VARIABLES EN TEMPS.
!
!      CE SOUS-PROGRAMME PEUT ETRE COMPLETE PAR L'UTILISATEUR
!      SOIT DIRECTEMENT, SOIT PAR L'INTERMEDIAIRE DES FONCTIONS :
!
!             Q , SL , TR , VIT
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   HBOR         |<-- |  HAUTEUR IMPOSEE.                            |
! |   UBOR         |<-- |  VITESSE U IMPOSEE.                          |
! |   VBOR         |<-- |  VITESSE V IMPOSEE.                          |
! |   TBOR         |<-- |  TRACEUR IMPOSE AU BORD                      |
! |    U,V         | -->|  COMPOSANTES DE LA VITESSE AU TEMPS N        |
! |    H           | -->|  HAUTEUR AU TEMPS N                          |
! |    ZF          | -->|  FOND                                        |
! |    NBOR        | -->|  ADRESSES DES POINTS DE BORD                 |
! |  TRA05,TRA06   | -->|  TABLEAUX DE TRAVAIL                         |
! |   LIHBOR       | -->|  CONDITIONS AUX LIMITES SUR H                |
! |   LIUBOR       | -->|  CONDITIONS AUX LIMITES SUR U                |
! |   LITBOR       | -->|  CONDITIONS AUX LIMITES SUR LE TRACEUR       |
! |   NPOIN        | -->|  NOMBRE DE POINTS DU MAILLAGE.               |
! |   NPTFR        | -->|  NOMBRE DE POINTS FRONTIERE.                 |
! |   TEMPS        | -->|  TEMPS                                       |
! |   DEBIT        |<-->|  TABLEAU DE DEBITS IMPOSES                   |
! |   NDEBIT       | -->|  NOMBRE DE FRONTIERES A DEBIT IMPOSE         |
! |   COTE         |<-->|  TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
! |   COTINI       | -->|  COTE INITIALE
! |   NCOTE        | -->|  NOMBRE DE FRONTIERES A COTE IMPOSEE         |
! |   VITES        |<-->|  TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
! |                |    |  IMPOSEES                                    |
! |   NVITES       | -->|  NOMBRE DE FRONTIERES A VITESSE IMPOSEE      |
! |   TRAC         | -->|  LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR  |
! |   TRACER       |<-->|  TABLEAU DE VALEURS DU TRACEUR IMPOSEES      |
! |   NTRACE       | -->|  NOMBRE DE FRONTIERES A TRACEUR IMPOSE       |
! |   NFRLIQ       | -->|  NOMBRE DE FRONTIERES LIQUIDES
! |   KENT,KENTU,  | -->|  CONVENTION POUR LES TYPES DE CONDITIONS AUX |
! |                |    |  KENTU:U ET V IMPOSES                        |
! |   PROVEL       | -->|  OPTION POUR LES PROFILS DE VITESSE          |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
! 
!----------------------------------------------------------------------- 
! 
! APPELE PAR : TELMAC 
! 
! SOUS-PROGRAMME APPELE : DEBIMP 
! 
! FONCTIONS APPELEES : Q , SL , TR , VIT 
! 
!*********************************************************************** 
! 
      USE BIEF 
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFO1
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP 
      INTEGER K,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE,MSK1,NTRAC
      INTEGER NPOIN,NFRLIQ,NPTFR2 
      INTEGER IFRLIQ 
      INTEGER KENT,KENTU
      INTEGER NBOR(NPTFR) 
      INTEGER LIHBOR(NPTFR) , LIUBOR(NPTFR) 
      INTEGER LITBOR(NPTFR) , PROVEL(100)
      INTEGER NUMLIQ(NPTFR) 
! 
      INTEGER YADEB(100) 
! 
      DOUBLE PRECISION HBOR(NPTFR)     , UBOR(NPTFR,2)  , VBOR(NPTFR,2) 
      DOUBLE PRECISION TBOR(NPTFR)   
      DOUBLE PRECISION ZF(NPOIN)  
      DOUBLE PRECISION XNEBOR(NPTFR)   , YNEBOR(NPTFR) 
!
      DOUBLE PRECISION TEMPS,Z(1),COEF 
!
      CHARACTER(LEN=20) EQUA 
!
      TYPE(BIEF_MESH) :: MESH 
      TYPE(BIEF_OBJ)  :: MASK,H,U,V,TRA05,TRA06 
!
      INTEGER P_IMAX 
      DOUBLE PRECISION Q,SL,VIT,TR 
      EXTERNAL          Q,SL,VIT,TR ,P_IMAX
!      INTRINSIC MAX
!
!
!GB*****************************************************************
!GB       DECLARATION SUPPLEMENTAIRES : Variables pour la marée
!GB********************************************************
!GB
  
   
!     
!     
!     variable pour calculer le temps universel et variables fonda:      
      DOUBLE PRECISION ALF ,PI,Y100,AJUL4,NLUN, HT 
      DOUBLE PRECISION  DD,JD,TUNIV,HSUN,SLUN,PLUN,TLUN,PSUN
!     periode des différentes ondes; nbre d'ondes max=120:
!     NSPECTR: nombre d'ondes du spectre. Si NSPECTR>120 changer taille des vecteurs
!     NONDES: nombre d'ondes selectionnées  
      DOUBLE PRECISION TONDES (120) 
!     facteurs nodaux u f + phase Vn:
      DOUBLE PRECISION UONDES(120),FONDES(120),VONDES(120)
!     amplitude et phase des ondes hn gn pour les Nondes en chaque noeud frontière:
      DOUBLE PRECISION AHN(50,120) , PHN(50,120)
!     Indices noeud frontière maritime et nombre d'ondes + nbr de noeuds frontires maritime et 
!     d'ondes:
      INTEGER NPTFRL,IPTFRL,NPTFRLM,IONDES,NONDES,NSPECTR,NFO1
      INTEGER YY,MM,DAY,HOUR,MINU,SEC,AJUL,BJUL

!
      DOUBLE PRECISION NIVM(50)        
      DOUBLE PRECISION PROF(NPOIN)
!

      SAVE AHN, PHN    
!
!GB****************************************************
!GB     FIN DES DECLARATIONS SUPPLEMENTAIRES
!GB****************************************************
!
!
      DOUBLE PRECISION, POINTER :: X(:),Y(:)
      X=>MESH%X%R
      Y=>MESH%Y%R
!
      
      NFO1=T2D_FILES(T2DFO1)%LU
      MSK1 = 1
!
!  INITIALISATION DE YADEB
!
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF                                                            
!--------------------------------------------------------------------
! L'amplitude de la maree est representee par une somme d'harmonique
! Nondes= nombre d'ondes, donnees lue ds fichier 26
! Pour chaque harmonique, l 'amplidude de l'onde n est calculée
!  An=Hn fn cos(sn*temps-gn+Vn+un)
! la détermination des différents termes se fait en plusieurs etapes
! Etape 1: lectures des donnees d'entree: Hn, gn et sn
!          Hn et gn sont fournies par le Shom
!          sn=2PI/Tn Tn est la periode
! Etape 2 calcul de un Vn et fn pour une date donnée, début du calcul
!-----------------------------------------------------------------------

!======================================================
! LECTURE DES DONNEES DE MAREES AU PREMIER PAS DE TEMPS
! Etape 1: Tn, Hn, gn
!======================================================
!
      PI=ACOS(-1.D0)
!BD   NB DE POINTS DU MODELE DE BD

!C CB a modifier pour 41 si seulement estuaire ou plus si aussi debit
! garonne et gironde 41+2*5=51
!.......NPTFRLM est le nombre de points de mer du modele.
      NPTFRLM = 28
!     print *, ' hello'


!

!   ordre des ondes en fct de leur amplitude decroissante
!    M2,S2,N2,M4, K2,O1,K1,MN4,M3,2N2,MS4,P1,Q1,MU2
! 26 ou NF01
      REWIND NFO1
! 27 o NF02
       


      READ(NFO1,*) NSPECTR
      
      READ(NFO1,*) NONDES
      
      DO IONDES =1,NONDES
        READ (NFO1,*) TONDES(IONDES)
      ENDDO
      
      DO IONDES =NONDES+1,NSPECTR
        READ (NFO1,*) TONDES(IONDES)
      ENDDO
       
!

!     plus utilise pour le moment
      DO IPTFRL = 1,NPTFRLM
!
!         READ(27,*)
!         READ(27,*)   NIVM(IPTFRL), ALF
         
         
        IONDES=1
         

!      Lecture des Hn et gn des Nondes pour les differents noeuds frontières
        READ(NFO1,*)
        
        DO IONDES =1,NONDES
          READ(NFO1,777) AHN(IPTFRL,IONDES),PHN(IPTFRL,IONDES)
        ENDDO
 
        DO IONDES =NONDES+1,NSPECTR
          READ(NFO1,777) AHN(IPTFRL,IONDES),PHN(IPTFRL,IONDES)
        ENDDO

        IONDES=1

!
!
! les phases sont calcul�es de mani�re � se recaler en temps par rapport
! au 06 avril 1999 22h (t=0 simu, TU)
    


!  Etape 2 calcul de un, fn, Vn
! ----------------------------------------------------------------
! ----------------------------------------------------------------      
!        Definition de la date
!-----------------------------------------------------------------
! -------------------------------------------------------------------
!       year
        YY=1999
!       MONTH 
        MM=4
! DAY 
        DAY=8
        HOUR=22
        MINU=0 
        SEC=0
!---------------------------------------------------------------
!         passage en calendrier Julien JD et temps universel Tuniv
!         jour 6 à 22h donne DDdd=6.917
!------------------------------------------------------------------

        DD=DAY+(HOUR*3600.D0+MINU*60.D0+SEC)/(24.D0*3600.D0)
        HT=HOUR+(MINU*60.D0+SEC)/3600.D0


        IF ((MM.EQ.2) .OR.(MM.EQ.1)) THEN
        YY=YY-1
        MM=MM+12
        ENDIF
        Y100=YY/100.D0
        AJUL=DINT(Y100)
        AJUL4=AJUL/4.D0
        BJUL=2.D0-AJUL+DINT(AJUL4)
!       reutilsation des var y100, ajul4

        Y100=365.25D0*YY 
        AJUL4=30.6001D0*(MM+1)

        JD= DINT(Y100)+DINT(AJUL4)+DD +1720994.5D0+BJUL
        
        TUNIV=(JD-2415020.5D0)/36525.D0
!-------------------------------------------------------
!           calcul des variables fondamentales des astres
!            tlun, slun, hsun, plun, Nlun, psun
!----------------------------------------------------------
        SLUN=DMOD(277.0248D0+481267.8906D0*TUNIV
     &  +0.002D0*TUNIV**2.D0,360.D0)
        HSUN=DMOD(280.1895D0+36000.7689D0*TUNIV
     &           +0.0003D0*TUNIV**2D0,360.D0)
        TLUN=DMOD(15.D0*HT+HSUN-SLUN,360.D0)
        PLUN=DMOD(334.3853D0+4069.034D0*TUNIV
     &           -0.0103D0*TUNIV**2.D0,360.D0)
  
        NLUN=DMOD(100.8432D0+1934.142D0*TUNIV
     &           -0.0021D0*TUNIV**2.D0,360.D0) 
        PSUN=DMOD(281.2209D0+1.7192D0*TUNIV
     &           +0.0005D0*TUNIV**2.D0,360.D0)    
!-----------------------------------------------
! Calcul des un , facteurs nodaux de phases
!    M2,S2,N2,M4,K2,O1,K1,MN4,M3,2N2,MS4,P1,Q1,MU2
!       M2             
        UONDES(1)=2.1D0*DSIN(PI*NLUN/180.D0)
!       S2
        UONDES(2)=0.D0
!       N2
        UONDES(3)=UONDES(1)
!       M4
!       uondes(4)=0.075d0*180.d0*sin(PI*Nlun/180.d0)/PI
        UONDES(4)=2.D0*UONDES(1)
!       K2 attention a changer
        UONDES(5)=24.97D0*DSIN(PI*NLUN/180.D0)
        UONDES(6)=-10.8D0*DSIN(PI*NLUN/180.D0)
!       k1 à changer aussi
        UONDES(7)=11.36D0*DSIN(PI*NLUN/180.D0)

        UONDES(8)=2.D0*UONDES(1)
        UONDES(9)=0.056D0*180.D0*DSIN(PI*NLUN/180.D0)/PI
        UONDES(10)=2.1D0*DSIN(PI*NLUN/180.D0)
        UONDES(11)=2.1D0*DSIN(PI*NLUN/180.D0)
        UONDES(12)=0.D0
        UONDES(13)=-10.8D0*DSIN(PI*NLUN/180.D0)
        UONDES(14)=2.1D0*DSIN(PI*NLUN/180.D0)
!       do IONDES =1,NONDES 
!       do IONDES =NONDES+1,NSPECTR
!         print *,IONDES, Tondes(IONDES)
!       enddo
                             

! Calcul de VN phase de l 'astre perturbateur
             
            

        IONDES=1 

        VONDES (1)=MOD(2.D0*TLUN,360.D0) 
        VONDES (2)=MOD(2.D0*TLUN+2.D0*SLUN-2.D0*HSUN,360.D0) 
        VONDES (3)=MOD(2.D0*TLUN-SLUN+PLUN,360.D0) 
        VONDES (4)=MOD(4.D0*TLUN,360.D0) 
        VONDES (5)=MOD(2.D0*TLUN+2.D0*SLUN,360.D0) 
        VONDES (6)=MOD(TLUN-SLUN,360.D0) 
        VONDES (7)=MOD(TLUN+SLUN,360.D0) 
        VONDES (8)=MOD(4.D0*TLUN-SLUN+PLUN,360.D0) 
        VONDES (9)=MOD(3.D0*TLUN,360.D0) 
        VONDES (10)=MOD(2.D0*TLUN-2.D0*SLUN+2.D0*PLUN,360.D0) 
        VONDES (11)=MOD(4.D0*TLUN+2.D0*SLUN-2.D0*HSUN,360.D0) 
        VONDES (12)=MOD(TLUN+SLUN-2.D0*HSUN,360.D0) 
        VONDES (13)=MOD(TLUN-2.D0*SLUN+PLUN,360.D0) 
        VONDES (14)=MOD(2.D0*TLUN-4.D0*SLUN+4.D0*HSUN,360.D0)
          
               

! Calcul des fn, facteurs nodaux en amplitudes
        DO IONDES =1,NONDES
          FONDES (IONDES)=1.D0
        ENDDO

        IONDES=1


        FONDES(1)=1.D0-0.037D0*DCOS(PI*NLUN/180.D0)
        FONDES(3)=FONDES(1)
        FONDES(4)=FONDES(1)**2.D0
        FONDES(5)=1.024D0+0.436D0*DCOS(PI*NLUN/180.D0)
        FONDES(6)=1.009D0+0.187D0*DCOS(PI*NLUN/180.D0)
!       fondes (6)=0.D0
!       fondes(7)=0.D0
        FONDES(7)=1.006+0.198*DCOS(PI*NLUN/180.D0)
        FONDES (8)=FONDES(1)**2.D0
        FONDES (9)=1.D0-0.056D0*DCOS(PI*NLUN/180.D0)
        FONDES(10)=1.0D0-0.037D0*DCOS(PI*NLUN/180.D0)
        FONDES(11)=1.0D0-0.037D0*DCOS(PI*NLUN/180.D0)
        
        FONDES(13)=1.009D0+0.187D0*DCOS(PI*NLUN/180.D0)
        FONDES(14)=1.0D0-0.037D0*DCOS(PI*NLUN/180.D0)

!       do IONDES =1,NONDES 
!         do IONDES =NONDES+1,NSPECTR
!        print *,IONDES, Vondes(IONDES),fondes(Iondes),
!     * uondes(Iondes) 
!       enddo
! calcul des dephasages gn-Vn-un

            
        DO IONDES=1,NONDES
          PHN(IPTFRL,IONDES) = (PHN(IPTFRL,IONDES)-UONDES(IONDES)
     &            -VONDES(IONDES)) / 360.D0
        ENDDO
! ----------------------------------------------------------------------           


!
!            PREMIERE VALEUR SUSPECTE FIDE FL + PAS DE FACTEUR NODAL
!
      ENDDO
! 777      FORMAT(16X,F4.2,5X,F5.1)
  777 FORMAT(F9.3,F9.3)
!XC       ENDIF
!
!======================================================
! CALCUL DE LA MAREE: amplitude
!======================================================
!
      IPTFRL = 1
      DO K=1,NPTFR
!
!
!
        IF(LIHBOR(K).EQ.KENT) THEN
!


          IONDES=1
          PROF(K)=0.D0

            DO IONDES=1,NONDES

!             PROF (K)= amplitude de la marée A au noeud K
!             A=AM2+AS2+AN2+AM4
!             An=Hn*fn*cos(sn*temps-gn+un+Vn)

              PROF(K)=PROF(K)+AHN(IPTFRL,IONDES)*FONDES(IONDES)
     &        *COS(2.D0*PI*(TEMPS/TONDES(IONDES)-PHN(IPTFRL,IONDES)))
            ENDDO
!           ajout du niveau moyen et de la bathy     
!           HBOR(K) = -ZF(NBOR(K)) + NIVM(IPTFRL) + PROF(K)+0.312d0
            HBOR(K) = -ZF(NBOR(K)) + 0.2D0+ PROF(K)    
!
!GB
            IPTFRL=IPTFRL+1
        ENDIF
     
!
!NC...On n'impose pas les vitesses
!                                                                    
!      IF(LIUBOR(K).EQ.KENTU) THEN                        
!                                        
!      UM2 = AUM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PUM2(IPTFRL)))
!      US2 = AUS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PUS2(IPTFRL)))
!      UN2 = AUN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PUN2(IPTFRL)))
!      UM4 = AUM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PUM4(IPTFRL)))
!      VM2 = AVM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PVM2(IPTFRL)))
!      VS2 = AVS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PVS2(IPTFRL)))
!      VN2 = AVN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PVN2(IPTFRL)))
!      VM4 = AVM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PVM4(IPTFRL)))
!      MODU = UM2 + UN2 + US2 + UM4
!     MODV = VM2 + VN2 + VS2 + VM4
!
!XC      IF (TEMPS.LT.1800.D0) THEN
!XC        MODU=MODU*(TEMPS/1800.D0)
!XC        MODV=MODV*(TEMPS/1800.D0)
!XC     ENDIF
!
!      IPTFRL=IPTFRL+1
!      ENDIF   
!
!=====================================================
!
!  DEBIT IMPOSE : DIFFERENTES OPTIONS SUIVANT PROVEL
!                 ON UTILISE LES VALEURS DONNEES PAR L'UTILISATEUR
!                 COMME PROFIL DE VITESSE.
!                 UBOR(K,2) ET VBOR(K,2) SONT LES VALEURS DU
!                 FICHIER CONLIM CONSERVEES.
!
!                 ON NE MET PAS DE VITESSE SI IL N'Y A PAS D'EAU.
!
!
        IF(LIUBOR(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
          IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
!           PROFIL NORMAL CONSTANT
            UBOR(K,1) = -XNEBOR(K)
            VBOR(K,1) = -YNEBOR(K)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
!           PROFIL DONNE PAR L'UTILISATEUR
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
!           VITESSE NORMALE DONNEE DANS UBOR
            UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
            VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.4) THEN
!           PROFIL NORMAL EN RACINE DE H
            UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
            VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          ENDIF
          IF(H%R(NBOR(K)).LT.1.D-3) THEN
            UBOR(K,1) = 0.D0
            VBOR(K,1) = 0.D0
          ENDIF
!         U ET V INITIALISES AVEC LES BONNES VALEURS
          U%R(NBOR(K)) = UBOR(K,1)
          V%R(NBOR(K)) = VBOR(K,1)
          YADEB(NUMLIQ(K))=1
!         PRINT*,'K=',K,' UBOR=',UBOR(K,1),' VBOR=',VBOR(K,1)
!         PRINT*,'H%R(NBOR(K))=',H%R(NBOR(K))
        ENDIF
!
      ENDDO   
!                                                                         
!!
!-----------------------------------------------------------------------
!
!     AUTOMATIC TIDAL BOUNDARY CONDITIONS
!
!-----------------------------------------------------------------------
!
!  QUADRATIC VELOCITIES
!
      IF(U%ELM .EQ.13)THEN
        DO K=1,NPTFR
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))/2.D0
        V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))/2.D0
          ENDIF
        ENDDO
      ENDIF
!  CAS DES DEBITS IMPOSES :
!
!  BOUCLE SUR LES FRONTIERES LIQUIDES
!
      IF(NFRLIQ.NE.0) THEN
!
        DO IFRLIQ = 1 , NFRLIQ
!       
          IF(NDEBIT.NE.0) THEN
            IF(NDEBIT.GE.IFRLIQ) THEN
              IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
              IF(YADEB(IFRLIQ).EQ.1) THEN
                CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     &                      IFRLIQ,TRA05,TRA06,
     &                      NPTFR,MASK%ADR(MSK1)%P%R,MESH,MESH%KP1BOR%I,
     &                      EQUA)
              ENDIF
            ELSE
              IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400           FORMAT(1X,'BORD : DEBITS IMPOSES',/,
     &               1X,'       EN NOMBRE INSUFFISANT',/,
     &               1X,'       DANS LE FICHIER DES PARAMETRES',/,
     &               1X,'       IL EN FAUT AU MOINS : ',1I6)
              IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401           FORMAT(1X,'BORD : MORE PRESCRIBED FLOWRATES',/,
     &               1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     &               1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
              CALL PLANTE(1)
              STOP
            ENDIF
!         
          ENDIF
!
!
!-----------------------------------------------------------------------
!
        ENDDO
!
      ENDIF

      IF(NCSIZE.GT.1) CALL PARCOM_BORD(HBOR,1,MESH)
!
!MB===============================================================
!                                                               
      RETURN                                                   
      END

