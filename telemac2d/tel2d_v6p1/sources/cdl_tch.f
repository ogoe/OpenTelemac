!                    ******************
                     SUBROUTINE CDL_TCH
!                    ******************
!
     &(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,KDDL,G,
     & HBOR,UBOR,VBOR,W,CE,FLUENT,FLUSORT,
     & FLBOR,DTHAUT,DT,CFL,EPS,
     & ZF,WINF)
!
!***********************************************************************
! TELEMAC2D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CE             |<->| FLUX
!| CFL            |-->| NOMBRE DE CFL
!| DT             |<->| PAS DE TEMPS
!| DTHAUT         |-->| UTILISE POUR CONDITION CFL
!| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE
!| G              |-->| CONSTANTE DE GRAVITE
!| HBOR           |-->| VALEURS IMPOSEES DE H
!| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
!| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
!| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
!| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
!| UBOR           |-->| VALEURS IMPOSEES DE U
!| VBOR           |-->| VALEURS IMPOSEES DE V
!| W              |-->| W(1,IS) = H,  W(2,IS)=QU  ,W(3,IS)=QV
!| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMPRO(NPTFR,6)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: HBOR(NPTFR),W(3,NS),DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: G,CFL,EPS,ZF(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DT,WINF(3,NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3),FLUENT,FLUSORT
      TYPE(BIEF_OBJ) , INTENT(INOUT)  :: FLBOR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NIT,IDRY
!
      DOUBLE PRECISION VNX,VNY,XNN,YNN,VNL
      DOUBLE PRECISION :: FLX(3),H1,U10,U1,V1,ETA1,FLUIJ_20
      DOUBLE PRECISION :: H2,ETA2,U2,V2
      DOUBLE PRECISION :: INFLOW,OUTFLOW
!
! LOOP OVER BOUNDARY NODES
      DO K=1,NPTFR
       IS=NBOR(K)
!
! INITIALIZATION
       FLUENT  = 0.D0
       FLUSORT = 0.0D0
       INFLOW  = 0.0D0
       OUTFLOW = 0.0D0
       FLX(1)  = 0.0D0
       FLX(2)  = 0.0D0
       FLX(3)  = 0.0D0
! INDICATOR FOR DRY CELLS
       IDRY=0
!   NORMALIZED NORMAL
       XNN=XNEBOR(K)
       YNN=YNEBOR(K)
!   NON NORMALIZED NORMAL
       VNX=XNEBOR(K+NPTFR)
       VNY=YNEBOR(K+NPTFR)
       VNL=SQRT(VNX**2+VNY**2)
!
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
!**************************************************
!         PAROIS SOLIDES
!**************************************************
!===============================
!    CONDITION DE GLISSEMENT
!===============================
!
       IF(LIMPRO(K,1).EQ.KNEU) THEN
!
! DEFINITION DE L'ETAT Ue
         H2=H1
         ETA2=ETA1
!        ROTATION SUIVANT LA NORMALE
         U10 = U1
         U1  = XNN*U10+YNN*V1
         V1  =-YNN*U10+XNN*V1
!
         U1 = 0.0D0
         U2 =  U1
         V2 =  V1
         CALL TCHAM_SMALL(H1,H2,ETA1,ETA2,U1,U2,V1,V2,G,FLX)
!        ROTATION INVERSE
         FLUIJ_20 = FLX(2)
         FLX(2)  = XNN*FLUIJ_20-YNN*FLX(3)
         FLX(3)  = YNN*FLUIJ_20+XNN*FLX(3)
!**************************************************
!         PAROIS LIQUIDES
!**************************************************
       ELSEIF((LIMPRO(K,1).EQ.KDIR).OR.(LIMPRO(K,1).EQ.KDDL))THEN
!===============================
!    SI H EST IMPOSEE
!===============================
!
        IF(LIMPRO(K,1).EQ.KDIR) THEN
!
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
!
          IF(IDRY.LT.2)THEN
!         AT LEAST ONE WET CELL
            CALL FLU_TCHAMEN(H1,H2,ETA1,ETA2,U1,U2,
     &                       V1,V2,XNN,YNN,FLX,G)
          ELSE
            FLX(1)=0.0D0
            FLX(2)=0.0D0
            FLX(3)=0.0D0
          ENDIF
          OUTFLOW    = FLX(1)*VNL
          FLUSORT    = FLUSORT + OUTFLOW
          FLBOR%R(K) = OUTFLOW
!       LIMPRO(K,1).NE.KDIR
        ELSE
          H2 = H1
          U2 = U1
          V2 = V1
          ETA2=ETA1
!
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
!
          IF(IDRY.LT.2)THEN
!         AT LEAST ONE WET CELL
            CALL FLU_TCHAMEN(H2,H1,ETA2,ETA1,U2,U1,
     &                      V2,V1,XNN,YNN,FLX,G)
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
!
!
100    CONTINUE
       CE(IS,1)  = CE(IS,1) - VNL*FLX(1)
       CE(IS,2)  = CE(IS,2) - VNL*FLX(2)
       CE(IS,3)  = CE(IS,3) - VNL*FLX(3)
!
       ENDDO
!
!-----------------------------------------------------------------------
!
       RETURN
       END