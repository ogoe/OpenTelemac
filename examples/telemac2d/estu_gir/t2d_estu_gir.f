!                    *******************
                     SUBROUTINE OIL_FLOT
!                    *******************
!
     &(PARTICULES,NFLOT,NFLOT_MAX,MESH,LT,VOLDEV,RHO_OIL,
     &NB_COMPO,NB_HAP,FMCOMPO,TBCOMPO,FMHAP,TBHAP,SOLU,ETAL,AREA)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
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
!| AT             |-->| TIME
!| ELTFLO         |-->| NUMBERS OF ELEMENTS WHERE ARE THE FLOATS
!| LT             |-->| CURRENT TIME STEP
!| MESH           |<->| MESH STRUCTURE
!| NFLOT          |-->| NUMBER OF FLOATS
!| NFLOT_MAX      |-->| MAXIMUM NUMBER OF FLOATS
!| NIT            |-->| NUMBER OF TIME STEPS
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| PARTICULES     |<->| OIL STRUCTURE DEFINED IN BIEF DEF
!| SHPFLO         |-->| BARYCENTRIC COORDINATES OF FLOATS IN THEIR 
!|                |   | ELEMENTS.
!| X,Y            |-->| COORDINATES OF POINTS IN THE MESH
!| XFLOT,YFLOT    |-->| POSITIONS OF FLOATING BODIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : GRAV
      USE STREAMLINE, ONLY : ADD_PARTICLE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NFLOT_MAX,LT
      INTEGER, INTENT(IN)             :: NB_COMPO,NB_HAP
      INTEGER, INTENT(IN)             :: ETAL
      INTEGER, INTENT(INOUT)          :: NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: VOLDEV,RHO_OIL,AREA
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
      DOUBLE PRECISION                :: XFLOT(1), YFLOT(1)
      DOUBLE PRECISION                :: SHPFLO(3,1)
      INTEGER                         :: TAGFLO(1)
      INTEGER                         :: ELTFLO(1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS IS AN EXAMPLE !!!!!!!!!!!!!!!!!!!!
!
         RHO_EAU=1000.D0
         PI=ACOS(-1.D0)
!        HARDCODED WATER MOLECULAR VISCOSITY
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
               COORD_X=336000.D0+REAL(j)
               COORD_Y=371000.D0+REAL(k)
               NUM_GLO=NUM_GLO+1
               NFLOT_OIL = 0
               CALL ADD_PARTICLE(COORD_X,COORD_Y,0.D0,NUM_GLO,NFLOT_OIL,
     &              1,XFLOT,YFLOT,YFLOT,TAGFLO,SHPFLO,SHPFLO,ELTFLO,
     &              ELTFLO,MESH,1,0.D0,0.D0,0.D0,0.D0,0,0)
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
!=========================================================================
!-----------INITIALIZATION PARAMETERS FOR THE CALCULATION OF OIL----------
!---------------------------WEATHERING PROCESSES--------------------------
!=========================================================================
                  PARTICULES(NUM_LOC)%STATE=1
                  PARTICULES(NUM_LOC)%TPSECH=0
                  IF(ETAL.EQ.1)THEN
                     PARTICULES(NUM_LOC)%SURFACE=PI*COEF2*
     &                    (DELTA*GRAV/(VOLDEV*NU2))**(1.D0/6.D0)
     &                    *VOLDEV/NFLOT_MAX 
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
     &                    PARTICULES(NUM_LOC)%MASS0*FMCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%TB=TBCOMPO(I)
                     PARTICULES(NUM_LOC)%COMPO(I)%SOL=0.D0
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &                    PARTICULES(NUM_LOC)%COMPO(I)%MASS
                  END DO
                  DO I=1,NB_HAP
                     PARTICULES(NUM_LOC)%HAP(I)%MASS=
     &                    PARTICULES(NUM_LOC)%MASS0*FMHAP(I)
                     PARTICULES(NUM_LOC)%HAP(I)%TB=TBHAP(I)
                      PARTICULES(NUM_LOC)%HAP(I)%SOL=SOLU(I)
                     PARTICULES(NUM_LOC)%MASS=PARTICULES(NUM_LOC)%MASS+
     &                    PARTICULES(NUM_LOC)%HAP(I)%MASS
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
     *        Y(I).GT.283000.D0.AND.Y(I).LT.284451.D0      ) THEN
            ZF%R(I)=0.D0
         ENDIF
         IF(X(I).GT.410500.D0.AND.X(I).LT.411750.D0.AND.
     *        Y(I).GT.255750.D0.AND.Y(I).LT.256250.D0      ) THEN
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
        if (y(i)-x(i).ge.10000.d0) then
          CHESTR%R(I) = 70.D0
        elseif (x(i).le.370000.d0) then
          CHESTR%R(I) = 75.D0 
        elseif (x(i).le.374000.d0.and.y(i).ge.282000.d0) then
          CHESTR%R(I) = 70.D0
        else
          CHESTR%R(I) = 60.D0
        endif
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
C                       ***************
                        SUBROUTINE BORD
C                       ***************
C
     *(HBOR,UBOR,VBOR,TBOR,U,V,H,
     * ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     * XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,
     * NDEBIT,NCOTE,NVITES,
     * NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     * NOMIMP)
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
C |   LIUBOR       | -->|  CONDITIONS AUX LIMITES SUR U                |
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
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFO1
C 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
C 
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP 
      INTEGER K,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE,MSK1,NTRAC
      INTEGER NPOIN,NFRLIQ,NPTFR2 
      INTEGER IFRLIQ 
      INTEGER KENT,KENTU
      INTEGER NBOR(NPTFR) 
      INTEGER LIHBOR(NPTFR) , LIUBOR(NPTFR) 
      INTEGER LITBOR(NPTFR) , PROVEL(100)
      INTEGER NUMLIQ(NPTFR) 
C 
      INTEGER YADEB(100) 
C 
      DOUBLE PRECISION HBOR(NPTFR)     , UBOR(NPTFR,2)  , VBOR(NPTFR,2) 
      DOUBLE PRECISION TBOR(NPTFR)   
      DOUBLE PRECISION ZF(NPOIN)  
      DOUBLE PRECISION XNEBOR(NPTFR)   , YNEBOR(NPTFR) 
C
      DOUBLE PRECISION TEMPS,Z(1),COEF 
C
      CHARACTER(LEN=20) EQUA 
C
      TYPE(BIEF_MESH) :: MESH 
      TYPE(BIEF_OBJ)  :: MASK,H,U,V,TRA05,TRA06 
C
      INTEGER P_IMAX 
      DOUBLE PRECISION Q,SL,VIT,TR 
      EXTERNAL          Q,SL,VIT,TR ,P_IMAX
c      INTRINSIC MAX
C
C
CGB*****************************************************************
CGB       DECLARATION SUPPLEMENTAIRES : Variables pour la marée
CGB********************************************************
CGB
  
   
C     
c     
c     variable pour calculer le temps universel et variables fonda:      
      DOUBLE PRECISION ALF ,PI,y100,Ajul4,Nlun, ht 
      DOUBLE PRECISION  Dd,JD,Tuniv,hsun,slun,plun,tlun,psun
c     periode des différentes ondes; nbre d'ondes max=120:
c     NSPECTR: nombre d'ondes du spectre. Si NSPECTR>120 changer taille des vecteurs
c     NONDES: nombre d'ondes selectionnées  
      DOUBLE PRECISION Tondes (120) 
c     facteurs nodaux u f + phase Vn:
      DOUBLE PRECISION uondes(120),fondes(120),Vondes(120)
c     amplitude et phase des ondes hn gn pour les Nondes en chaque noeud frontière:
      DOUBLE PRECISION AHN(50,120) , PHN(50,120)
c     Indices noeud frontière maritime et nombre d'ondes + nbr de noeuds frontires maritime et 
c     d'ondes:
      INTEGER NPTFRL,IPTFRL,NPTFRLM,IONDES,NONDES,NSPECTR,NFO1
      INTEGER YY,MM,day,hour,minu,sec,Ajul,Bjul

c
      DOUBLE PRECISION NIVM(50)        
      DOUBLE PRECISION PROF(NPOIN)
C

      SAVE AHN, PHN    
C
CGB****************************************************
CGB     FIN DES DECLARATIONS SUPPLEMENTAIRES
*******************************************************
C
C
      DOUBLE PRECISION, POINTER :: X(:),Y(:)
      X=>MESH%X%R
      Y=>MESH%Y%R
C
      
      NFO1=T2D_FILES(T2DFO1)%LU
      MSK1 = 1
C
C  INITIALISATION DE YADEB
C
      IF(NFRLIQ.GE.1) THEN
        DO 1 K=1,NFRLIQ
          YADEB(K)=0
1       CONTINUE
      ENDIF                                                            
C--------------------------------------------------------------------
C L'amplitude de la maree est representee par une somme d'harmonique
c Nondes= nombre d'ondes, donnees lue ds fichier 26
C Pour chaque harmonique, l 'amplidude de l'onde n est calculée
C  An=Hn fn cos(sn*temps-gn+Vn+un)
C la détermination des différents termes se fait en plusieurs etapes
C Etape 1: lectures des donnees d'entree: Hn, gn et sn
C          Hn et gn sont fournies par le Shom
c          sn=2PI/Tn Tn est la periode
C Etape 2 calcul de un Vn et fn pour une date donnée, début du calcul
c-----------------------------------------------------------------------

C======================================================
C LECTURE DES DONNEES DE MAREES AU PREMIER PAS DE TEMPS
C Etape 1: Tn, Hn, gn
C======================================================
C
       PI=ACOS(-1.D0)
CBD    NB DE POINTS DU MODELE DE BD

CC CB a modifier pour 41 si seulement estuaire ou plus si aussi debit
C garonne et gironde 41+2*5=51
C.......NPTFRLM est le nombre de points de mer du modele.
       NPTFRLM = 28
c      print *, ' hello'


C

c   ordre des ondes en fct de leur amplitude decroissante
c    M2,S2,N2,M4, K2,O1,K1,MN4,M3,2N2,MS4,P1,Q1,MU2
C 26 ou NF01
       REWIND NFO1
C 27 ou NF02
       


        READ(NFO1,*) NSPECTR
        
        READ(NFO1,*) NONDES
      
        do IONDES =1,NONDES
             read (NFO1,*) Tondes(IONDES)
        enddo
      
        do IONDES =NONDES+1,NSPECTR
             read (NFO1,*) Tondes(IONDES)
        enddo
         
C

c          plus utilise pour le moment
          DO 2 IPTFRL = 1,NPTFRLM
C
c             READ(27,*)
c             READ(27,*)   NIVM(IPTFRL), ALF
             
             
             IONDES=1
             

C          Lecture des Hn et gn des Nondes pour les differents noeuds frontières
             READ(NFO1,*)
            
            do IONDES =1,NONDES
       
             READ(NFO1,777) AHN(IPTFRL,IONDES),PHN(IPTFRL,IONDES)
             enddo
 
             do IONDES =NONDES+1,NSPECTR
       
             READ(NFO1,777) AHN(IPTFRL,IONDES),PHN(IPTFRL,IONDES)
             enddo

             IONDES=1

c
C
C les phases sont calcul�es de mani�re � se recaler en temps par rapport
C au 06 avril 1999 22h (t=0 simu, TU)
    


C  Etape 2 calcul de un, fn, Vn
c ----------------------------------------------------------------
c ----------------------------------------------------------------      
C        Definition de la date
c-----------------------------------------------------------------
c -------------------------------------------------------------------
c           year
             YY=1999
c           month 
             MM=4
c day 
             day=8
             hour=22
             minu=0 
             sec=0
c---------------------------------------------------------------
c         passage en calendrier Julien JD et temps universel Tuniv
c         jour 6 à 22h donne DDdd=6.917
c------------------------------------------------------------------

              Dd=day+(hour*3600.D0+minu*60.D0+sec)/(24.D0*3600.D0)
              ht=hour+(minu*60.D0+sec)/3600.D0


             if ((MM.eq.2) .OR.(MM.eq.1)) then
             YY=YY-1
             MM=MM+12
             endif
             y100=YY/100.D0
             Ajul=DINT(y100)
             Ajul4=Ajul/4.D0
             Bjul=2.D0-Ajul+DINT(Ajul4)
c           reutilsation des var y100, ajul4

             y100=365.25D0*YY 
             Ajul4=30.6001D0*(MM+1)

             JD= DINT(Y100)+DINT(Ajul4)+Dd +1720994.5D0+Bjul
        
             Tuniv=(JD-2415020.5D0)/36525.D0
c-------------------------------------------------------
c           calcul des variables fondamentales des astres
c            tlun, slun, hsun, plun, Nlun, psun
c----------------------------------------------------------
             slun=DMOD(277.0248d0+481267.8906d0*Tuniv
     *       +0.002d0*Tuniv**2.d0,360.d0)
             hsun=DMOD(280.1895d0+36000.7689d0*Tuniv
     *                +0.0003d0*Tuniv**2d0,360.d0)
             tlun=DMOD(15.d0*ht+hsun-slun,360.d0)
             plun=DMOD(334.3853d0+4069.034d0*Tuniv
     *                -0.0103d0*Tuniv**2.d0,360.d0)
  
             Nlun=DMOD(100.8432d0+1934.142d0*Tuniv
     *                -0.0021d0*Tuniv**2.d0,360.d0) 
             psun=DMOD(281.2209d0+1.7192d0*Tuniv
     *                +0.0005d0*Tuniv**2.d0,360.d0)    
c-----------------------------------------------
c Calcul des un , facteurs nodaux de phases
c    M2,S2,N2,M4,K2,O1,K1,MN4,M3,2N2,MS4,P1,Q1,MU2
c            M2             
             uondes(1)=2.1d0*dsin(PI*Nlun/180.d0)
c            S2
             uondes(2)=0.d0
c            N2
             uondes(3)=uondes(1)
c            M4
c             uondes(4)=0.075d0*180.d0*sin(PI*Nlun/180.d0)/PI
              uondes(4)=2.D0*uondes(1)
c            K2 attention a changer
             uondes(5)=24.97d0*dsin(PI*Nlun/180.d0)
             uondes(6)=-10.8d0*dsin(PI*Nlun/180.d0)
c            k1 à changer aussi
             uondes(7)=11.36d0*dsin(PI*Nlun/180.d0)

             uondes(8)=2.D0*uondes(1)
             uondes(9)=0.056d0*180.d0*dsin(PI*Nlun/180.d0)/PI
             uondes(10)=2.1d0*dsin(PI*Nlun/180.d0)
              uondes(11)=2.1d0*dsin(PI*Nlun/180.d0)
               uondes(12)=0.d0
               uondes(13)=-10.8d0*dsin(PI*Nlun/180.d0)
              uondes(14)=2.1d0*dsin(PI*Nlun/180.d0)
c          do IONDES =1,NONDES 
c          do IONDES =NONDES+1,NSPECTR
c             print *,IONDES, Tondes(IONDES)
c         enddo
                             

c Calcul de VN phase de l 'astre perturbateur
             
            

             IONDES=1 

             Vondes (1)=MOD(2.d0*tlun,360.d0) 
             Vondes (2)=MOD(2.d0*tlun+2.d0*slun-2.d0*hsun,360.d0) 
             Vondes (3)=MOD(2.d0*tlun-slun+plun,360.d0) 
             Vondes (4)=MOD(4.d0*tlun,360.d0) 
             Vondes (5)=MOD(2.d0*tlun+2.d0*slun,360.d0) 
             Vondes (6)=MOD(tlun-slun,360.d0) 
             Vondes (7)=MOD(tlun+slun,360.d0) 
             Vondes (8)=MOD(4.d0*tlun-slun+plun,360.d0) 
             Vondes (9)=MOD(3.d0*tlun,360.d0) 
             Vondes (10)=MOD(2.d0*tlun-2.d0*slun+2.d0*plun,360.d0) 
             Vondes (11)=MOD(4.d0*tlun+2.d0*slun-2.d0*hsun,360.d0) 
             Vondes (12)=MOD(tlun+slun-2.d0*hsun,360.d0) 
             Vondes (13)=MOD(tlun-2.d0*slun+plun,360.d0) 
             Vondes (14)=MOD(2.d0*tlun-4.d0*slun+4.d0*hsun,360.d0)
          
               

c Calcul des fn, facteurs nodaux en amplitudes
             do IONDES =1,NONDES
       
             fondes (IONDES)=1.D0
             enddo

             IONDES=1


             fondes(1)=1.D0-0.037D0*dcos(PI*Nlun/180.D0)
             fondes(3)=fondes(1)
             fondes(4)=fondes(1)**2.D0
             fondes(5)=1.024D0+0.436D0*dcos(PI*Nlun/180.D0)

             fondes(6)=1.009D0+0.187D0*dcos(PI*Nlun/180.D0)
c             fondes (6)=0.D0
c             fondes(7)=0.D0
             fondes(7)=1.006+0.198*dcos(PI*Nlun/180.D0)
             fondes (8)=fondes(1)**2.D0
             fondes (9)=1.D0-0.056D0*dcos(PI*Nlun/180.D0)
             fondes(10)=1.0D0-0.037d0*dcos(PI*Nlun/180.D0)
             fondes(11)=1.0D0-0.037d0*dcos(PI*Nlun/180.D0)
            
             fondes(13)=1.009D0+0.187D0*dcos(PI*Nlun/180.D0)
              fondes(14)=1.0D0-0.037d0*dcos(PI*Nlun/180.D0)

c            do IONDES =1,NONDES 
c              do IONDES =NONDES+1,NSPECTR
c             print *,IONDES, Vondes(IONDES),fondes(Iondes),
c     *      uondes(Iondes) 
c            enddo
C calcul des dephasages gn-Vn-un

            
            do IONDES=1,NONDES
             PHN(IPTFRL,IONDES) = (PHN(IPTFRL,IONDES)-uondes(IONDES)
     *                -Vondes(IONDES)) / 360.D0
              enddo
c ----------------------------------------------------------------------           


C
C            PREMIERE VALEUR SUSPECTE FIDE FL + PAS DE FACTEUR NODAL
C
2        CONTINUE
c 777      FORMAT(16X,F4.2,5X,F5.1)
  777   FORMAT(F9.3,F9.3)
CXC       ENDIF
C
C======================================================
C CALCUL DE LA MAREE: amplitude
C======================================================
C
      IPTFRL = 1
      DO 51 K=1,NPTFR
C
C
C
      IF(LIHBOR(K).EQ.KENT) THEN
C


       IONDES=1
       PROF(K)=0.d0

       DO IONDES=1,NONDES

c    PROF (K)= amplitude de la marée A au noeud K
c    A=AM2+AS2+AN2+AM4
c    An=Hn*fn*cos(sn*temps-gn+un+Vn)

        PROF(K)=PROF(K)+AHN(IPTFRL,IONDES)*fondes(IONDES)
     *  *COS(2.D0*PI*(TEMPS/Tondes(IONDES)-PHN(IPTFRL,IONDES)))
        enddo
c     ajout du niveau moyen et de la bathy     
C       HBOR(K) = -ZF(NBOR(K)) + NIVM(IPTFRL) + PROF(K)+0.312d0
       HBOR(K) = -ZF(NBOR(K)) + 0.2d0+ PROF(K)    
C
CGB
      IPTFRL=IPTFRL+1
      ENDIF
     
C
CNC...On n'impose pas les vitesses
C                                                                    
C      IF(LIUBOR(K).EQ.KENTU) THEN                        
C                                        
C      UM2 = AUM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PUM2(IPTFRL)))
C      US2 = AUS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PUS2(IPTFRL)))
C      UN2 = AUN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PUN2(IPTFRL)))
C      UM4 = AUM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PUM4(IPTFRL)))
C      VM2 = AVM2(IPTFRL) * COS(2.D0*PI*(TEMPS/TM2-PVM2(IPTFRL)))
C      VS2 = AVS2(IPTFRL) * COS(2.D0*PI*(TEMPS/TS2-PVS2(IPTFRL)))
C      VN2 = AVN2(IPTFRL) * COS(2.D0*PI*(TEMPS/TN2-PVN2(IPTFRL)))
C      VM4 = AVM4(IPTFRL) * COS(2.D0*PI*(TEMPS/TM4-PVM4(IPTFRL)))
C      MODU = UM2 + UN2 + US2 + UM4
C     MODV = VM2 + VN2 + VS2 + VM4
C
CXC      IF (TEMPS.LT.1800.D0) THEN
CXC        MODU=MODU*(TEMPS/1800.D0)
CXC        MODV=MODV*(TEMPS/1800.D0)
CXC     ENDIF
C
C      IPTFRL=IPTFRL+1
C      ENDIF   
C
C=====================================================
C
C  DEBIT IMPOSE : DIFFERENTES OPTIONS SUIVANT PROVEL
C                 ON UTILISE LES VALEURS DONNEES PAR L'UTILISATEUR
C                 COMME PROFIL DE VITESSE.
C                 UBOR(K,2) ET VBOR(K,2) SONT LES VALEURS DU
C                 FICHIER CONLIM CONSERVEES.
C
C                 ON NE MET PAS DE VITESSE SI IL N'Y A PAS D'EAU.
C
C
      IF(LIUBOR(K).EQ.KENT.AND.NDEBIT.NE.0) THEN
        IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
C         PROFIL NORMAL CONSTANT
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
C         PROFIL DONNE PAR L'UTILISATEUR
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
C         VITESSE NORMALE DONNEE DANS UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.4) THEN
C         PROFIL NORMAL EN RACINE DE H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
        ENDIF
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
C       U ET V INITIALISES AVEC LES BONNES VALEURS
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
!       PRINT*,'K=',K,' UBOR=',UBOR(K,1),' VBOR=',VBOR(K,1)
!       PRINT*,'H%R(NBOR(K))=',H%R(NBOR(K))
      ENDIF
C
51    CONTINUE   
C                                                                         
C!
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
C  CAS DES DEBITS IMPOSES :
C
C  BOUCLE SUR LES FRONTIERES LIQUIDES
C
      IF(NFRLIQ.NE.0) THEN
C
      DO 10 IFRLIQ = 1 , NFRLIQ
C
      IF(NDEBIT.NE.0) THEN
        IF(NDEBIT.GE.IFRLIQ) THEN
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     *                  IFRLIQ,TRA05,TRA06,
     *                  NPTFR,MASK%ADR(MSK1)%P%R,MESH,MESH%KP1BOR%I,
     *                  EQUA)
          ENDIF
        ELSE
          IF(LNG.EQ.1) WRITE(LU,400) IFRLIQ
400       FORMAT(1X,'BORD : DEBITS IMPOSES',/,
     *           1X,'       EN NOMBRE INSUFFISANT',/,
     *           1X,'       DANS LE FICHIER DES PARAMETRES',/,
     *           1X,'       IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) IFRLIQ
401       FORMAT(1X,'BORD : MORE PRESCRIBED FLOWRATES',/,
     *           1X,'       ARE REQUIRED IN THE PARAMETER FILE',/,
     *           1X,'       AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
C
      ENDIF
C
C
C-----------------------------------------------------------------------
C
10    CONTINUE
C
      ENDIF
C
CMB===============================================================
C                                                               
      RETURN                                                   
      END
