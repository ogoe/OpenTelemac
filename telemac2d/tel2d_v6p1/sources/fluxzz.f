C                       *******************
                        SUBROUTINE FLUXZZ
C                       *******************

     *(NS,NSEG,NUBO,G,X,Y,W,ZF,VNOCL,CE,AIRS)
C
C***********************************************************************
C TELEMAC 2D VERSION 5.9                                          R. ATA
C
C***********************************************************************
C
C     FONCTION  : CALCUL DES FLUX POUR LES INTERFACES INTERNES 
C     AVEC LES FORMULES DE FLUX SUGGÉRÉES PAR ZOKAGOA ET AL.
C     "MODELING OF WETTING-DRYING TRANSITIONS IN FREE SURFACE FLOWS 
C      OVER COMPLEX TOPOGRAPHIES" CMAME 199(2010) PP 2281-2304 
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |  NS            | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |  NSEG          | -->|  NOMBRE D'ARETES DU MAILLAGE                 |
C !  NUBO          ! -->!  NUMEROS GLOBAUX DES EXTREMITES DES ARETES   !
C |  G             | -->|  CONSTANTE DE GRAVITE                        |
C |  X,Y           | -->|  COORDONNEES DES NOEUDS DU MAILLAGE          |
C |  W             | -->|  W(1,IS) = H,  W(2,IS)=U  ,W(3,IS)=V         |
C |  ZF            | -->|  COTES DU FOND                               |
C !  VNOCL         ! -->!  NORMALE A l'INTERFACE                       |
C !                !    !   (2 PREMIERES COMPOSANTES) ET               |
C !                !    !   LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)  |
C |  CE            |<-->|  FLUX  INCREMENTES DES FLUX                  |
C |                |    |      AUX INTERFACES INTERNES                 |
C |  AIRS          | -->|  AIRES DES CELLULES                          |
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
      INTEGER, INTENT(IN) :: NS,NSEG
      INTEGER, INTENT(IN) :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NS),VNOCL(3,NSEG),AIRS(*)
      DOUBLE PRECISION, INTENT(IN)    :: G,W(3,NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3)
C***********************************************************************
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C-----------------------------------------------------------------------
C
      INTEGER NSG,NUBO1,NUBO2,J,IVAR,IS,K,IDRY     
C
      DOUBLE PRECISION VNX,VNY,VNL,ZF1,ZF2,XNN,YNN,RNN
      DOUBLE PRECISION V21,V22,V31,V32
      DOUBLE PRECISION HI,HI0,HIJ,CIJ,V210,V220
      DOUBLE PRECISION HJ,HJ0,HJI,CJI,DZIJ,DZJI
      DOUBLE PRECISION FLU11,FLUIJ_2,FLUIJ_3,FLU12,FLU22
      DOUBLE PRECISION FLUIJ_20
      DOUBLE PRECISION SIGMAX,UNORM
c
      DOUBLE PRECISION FLUIJ_1,H1,H2,EPS,FLX(3)
      DOUBLE PRECISION ETA1,ETA2,U_IJ,D_IJ,C_IJ,C_I,C_J

C-----------------------------------------------------------------------
C**************************************************************
C VOIR SON EFFET
         EPS=1.E-6
C**************************************************************
C INITIALISATION DE CE      
       DO IS=1,3
         DO IVAR=1,NS
           CE(IVAR,IS) = 0.D0
         ENDDO
       ENDDO
C
C
C-----------------------------------------------------------------------
C     LOOP ON GLOBAL LIST OF EDGES
C    ******************************
C
      DO 5500 NSG=1,NSEG 
C INDICATOR FOR DRY CELLS
         IDRY=0
C INITIALIZATION
         FLX(1)=0.0D0
         FLX(2)=0.0D0
         FLX(3)=0.0D0
C RECUPERATE NODES OF THE EDGE 
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
C THEIR BATHYMETRIES
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)

C NORMAL COORDINATES NX, NY AND SEGMENT LENGTH
         XNN       = VNOCL(1,NSG)
         YNN       = VNOCL(2,NSG)
         RNN       = VNOCL(3,NSG) 
C
C WATER DEPTH
C
         H1=W(1,NUBO1)
         H2=W(1,NUBO2)
C
C UNKNOWN SET (V1,V2,V3)=(eta,U,V) FOR EACH NODE
C
         ETA1     = W(1,NUBO1)+ZF1
         ETA2     = W(1,NUBO2)+ZF2

         IF(H1.GT.EPS)THEN
            V21 = W(2,NUBO1)/H1
            V31 = W(3,NUBO1)/H1
         ELSE
            V21=0.0D0
            V31=0.0D0
            IDRY=IDRY+1
         ENDIF

         IF(H2.GT.EPS)THEN
            V22 = W(2,NUBO2)/H2 
            V32 = W(3,NUBO2)/H2
         ELSE
            V22=0.0D0
            V32=0.0D0
            IDRY=IDRY+1
         ENDIF

C
C LOCAL FLUX COMPUTATION
C
C        AT LEAST ONE WET CELL
        IF(IDRY.LT.2)THEN
           CALL FLU_ZOKAGOA(H1,H2,ETA1,ETA2,V21,V22,
     *                      V31,V32,XNN,YNN,FLX,G)
        ELSE
          FLX(1)=0.0D0
          FLX(2)=0.0D0
          FLX(3)=0.0D0
        ENDIF 
C
C FLUX INCREMENT
C
         CE(NUBO1,1) = CE(NUBO1,1) - RNN*FLX(1)
         CE(NUBO1,2) = CE(NUBO1,2) - RNN*FLX(2) 
         CE(NUBO1,3) = CE(NUBO1,3) - RNN*FLX(3) 
C
         CE(NUBO2,1) = CE(NUBO2,1) + RNN*FLX(1)
         CE(NUBO2,2) = CE(NUBO2,2) + RNN*FLX(2) 
         CE(NUBO2,3) = CE(NUBO2,3) + RNN*FLX(3) 
C
5500   CONTINUE
C
C-----------------------------------------------------------------------
      RETURN
      END
