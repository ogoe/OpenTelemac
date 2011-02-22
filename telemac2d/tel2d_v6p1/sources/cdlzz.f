C                       ******************                              
                        SUBROUTINE CDLZZ
C                       ******************                               
C                                                                       
     *(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,KDDL,G,
     * HBOR,UBOR,VBOR,W,CE,FLUENT,FLUSORT,
     * FLBOR,DTHAUT,DT,CFL,EPS,ZF,WINF)
C                                                                       
C***********************************************************************
C  TELEMAC 2D VERSION 5.9                                         R. ATA
C-----------------------------------------------------------------------
C                 ZOKAGOA SCHEME (OPTVF =3)
C   
C     COMPUTATION OF THE CONVECTIVE FLUXES AT BOUNDARIES
C     
C
C     W(1,IS) = H,  W(2,IS)=QU  ,W(3,IS)=QV
C
C-----------------------------------------------------------------------
C                             ARGUMENTS                                 
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C |  NS            | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |  NPTFR         | -->|  NOMBRE DE POINTS FRONTIERE                  |
C |  NBOR          | -->|  NUMEROS GLOBAUX DES POINTS DE BORD          | 
C |  LIMPRO        | -->|  TYPES DE CONDITIONS AUX LIMITES             |
C |  XNEBOR,YNEBOR | -->|  NORMALE AUX POINTS FRONTIERE                |
C |  KDIR          | -->|  CONVENTION POUR LES POINTS DIRICHLET        |
C |  KNEU          | -->|  CONVENTION POUR LES POINTS NEUMANN          |
C |  G             | -->|  CONSTANTE DE GRAVITE                        |
C |  HBOR          | -->|  VALEURS IMPOSEES DE H                       | 
C |  UBOR          | -->|  VALEURS IMPOSEES DE U                       | 
C |  VBOR          | -->|  VALEURS IMPOSEES DE V                       | 
C |  W             | -->|  W(1,IS) = H,  W(2,IS)=QU  ,W(3,IS)=QV      |
C |  CE            |<-->|  FLUX                                        |
C !  FLUENT,FLUSORT|<-- |  FLUX MASSE ENTREE ET SORTIE                 |
C |  DT            |<-->|  PAS DE TEMPS                                |
C !  CFL           ! -->!  NOMBRE DE CFL                               !
C !________________!____!______________________________________________!
C  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)   
C        -- (TABLEAU DE TRAVAIL)                                        
C-----------------------------------------------------------------------
C     - SOUS PROGRAMME(S) APPELANT : FLUHYD                              
C 
C***********************************************************************
C 
      USE BIEF
C  
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),W(3,NS),DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL,EPS,ZF(NS)
      DOUBLE PRECISION, INTENT(IN)    :: DT,WINF(3,NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,K,NIT,IDRY    
C    
      DOUBLE PRECISION VNX,VNY,XNN,YNN,VNL
      DOUBLE PRECISION :: FLX(3),H1,U10,U1,V1,ETA1,FLUIJ_20
      DOUBLE PRECISION :: H2,ETA2,U2,V2
      DOUBLE PRECISION :: INFLOW,OUTFLOW
C
C LOOP OVER BOUNDARY NODES
      DO K=1,NPTFR
       IS=NBOR(K)
C
C INITIALIZATION
       FLUENT  = 0.D0
       FLUSORT = 0.0D0
       INFLOW  = 0.0D0
       OUTFLOW = 0.0D0
       FLX(1)  = 0.0D0
       FLX(2)  = 0.0D0
       FLX(3)  = 0.0D0
C INDICATOR FOR DRY CELLS
       IDRY=0
C   NORMALIZED NORMAL    
       XNN=XNEBOR(K)
       YNN=YNEBOR(K)
C   NON NORMALIZED NORMAL
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
       
C
       H1   = W(1,IS)
       ETA1=H1+ZF(IS)
       IF(H1.GT.EPS)THEN
          U1   = W(2,IS)/H1
          V1   = W(3,IS)/H1
       ELSE
          U1   = 0.0D0
          V1   = 0.0D0
          IDRY=IDRY+1
       ENDIF
C**************************************************
C         PAROIS SOLIDES
C**************************************************
C===============================
C    CONDITION DE GLISSEMENT
C===============================
C
       IF(LIMPRO(K,1).EQ.KNEU) THEN 
C
C DEFINITION DE L'ETAT Ue
         H2=H1
         ETA2=ETA1
C        ROTATION SUIVANT LA NORMALE
         U10 = U1
         U1  = XNN*U10+YNN*V1
         V1  =-YNN*U10+XNN*V1
C         
         U1 =  0.0D0
         U2 =  U1
         V2 =  V1

         CALL ZOKA_SMALL(H1,H2,ETA1,ETA2,U1,U2,V1,V2,G,FLX)

C        ROTATION INVERSE
         FLUIJ_20 = FLX(2)
         FLX(2)  = XNN*FLUIJ_20-YNN*FLX(3)
         FLX(3)  = YNN*FLUIJ_20+XNN*FLX(3)
C**************************************************
C         PAROIS LIQUIDES
C**************************************************
       ELSEIF((LIMPRO(K,1).EQ.KDIR).OR.(LIMPRO(K,1).EQ.KDDL))THEN 
C===============================
C    SI H EST IMPOSEE
C===============================
C
        IF(LIMPRO(K,1).EQ.KDIR) THEN
C
          H2 = WINF(1,K)
          ETA2 = H2 + ZF(IS)
          IF(H2 .GT.EPS)THEN
            U2 = WINF(2,K) / H2
            V2 = WINF(3,K) / H2
          ELSE
            U2 = 0.0D0
            V2 = 0.0D0
            IDRY = IDRY + 1
          ENDIF
C
          IF(IDRY.LT.2)THEN
C         AT LEAST ONE WET CELL
            CALL FLU_ZOKAGOA(H1,H2,ETA1,ETA2,U1,U2,
     *                       V1,V2,XNN,YNN,FLX,G)
          ELSE
            FLX(1)=0.0D0
            FLX(2)=0.0D0
            FLX(3)=0.0D0
          ENDIF 
          OUTFLOW    = FLX(1)*VNL
          FLUSORT    = FLUSORT + OUTFLOW
          FLBOR%R(K) = OUTFLOW
         
C       LIMPRO(K,1).NE.KDIR    
        ELSE 

          H2 = H1
          U2 = U1
          V2 = V1
          ETA2=ETA1
          
          H1 = WINF(1,K)
          ETA1=H1+ZF(IS)
          IF(H1.GT.EPS)THEN
            U1 = WINF(2,K) / H1
            V1 = WINF(3,K) / H1
          ELSE
            U1 = 0.0D0
            V1 = 0.0D0
            IDRY = IDRY + 1
          ENDIF
C
          IF(IDRY.LT.2)THEN
C         AT LEAST ONE WET CELL
            CALL FLU_ZOKAGOA(H2,H1,ETA2,ETA1,U2,U1,
     *                       V2,V1,XNN,YNN,FLX,G)
          ELSE
            FLX(1)=0.0D0
            FLX(2)=0.0D0
            FLX(3)=0.0D0
          ENDIF 
          INFLOW     = FLX(1)*VNL
          FLUENT     = FLUENT + INFLOW
          FLBOR%R(K) = INFLOW  
         
      ENDIF
      ENDIF
C
C
100    CONTINUE

       CE(IS,1)  = CE(IS,1) - VNL*FLX(1)
       CE(IS,2)  = CE(IS,2) - VNL*FLX(2)
       CE(IS,3)  = CE(IS,3) - VNL*FLX(3)
C
       ENDDO
C
C-----------------------------------------------------------------------
C
       RETURN
       END
