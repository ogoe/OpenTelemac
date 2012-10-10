!                    *****************
                     SUBROUTINE PREPRO
!                    *****************
!
     &( CX    , CY    , CT    , CF    , DT    , NRK   , X     , Y     ,
     &  TETA  , COSTET, SINTET, FREQ  , IKLE2 , IFABOR, ETAP1 , TRA01 ,
     &  SHP   , SHZ   , SHF   , ELT   , ETA   , FRE   ,
     &  DEPTH , DZHDT , DZX   , DZY   , U     , V     , DUX   , DUY   ,
     &  DVX   , DVY   , XK    , CG    , COSF  , TGF   , ITR01 , NPOIN3,
     &  NPOIN2, NELEM2, NPLAN , NF    , SURDET, COURAN, SPHE  ,
     &  PROINF, PROMIN, MESH  , MESH3D,SIKLE2,TB,IELM3, DIFFRA, ISUB)
!
!***********************************************************************
! TOMAWAC   V6P3                                   25/06/2012
!***********************************************************************
!
!brief    PREPARES ADVECTION.
!+
!+            COMPUTES THE ADVECTION FIELD; TRACES BACK THE
!+                CHARACTERISTICS.
!
!history  F. MARCOS (LNH)
!+        04/12/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2012
!+        V6P2
!+   Modifications : possibility of taking into account diffraction
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |<->| ADVECTION FIELD ALONG FREQUENCY
!| CG             |-->| DISCRETIZED GROUP VELOCITY
!| COSF           |-->| COSINE OF THE LATITUDES OF THE POINTS 2D
!| COSTET         |-->| COSINE OF TETA ANGLE
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| CX             |<->| ADVECTION FIELD ALONG X(OR PHI)
!| CY             |<->| ADVECTION FIELD ALONG Y(OR LAMBDA)
!| CT             |<->| ADVECTION FIELD ALONG TETA
!| DEPTH          |-->| WATER DEPTH
!| DIFFRA         |-->| 0: NO DIFFRACTION  1: DIFFRACTION
!| DT             |-->| TIME STEP
!| DUX            |-->| DERIVATIVE OF CURRENT SPEED DU/DX
!| DUY            |-->| DERIVATIVE OF CURRENT SPEED DU/DY
!| DVX            |-->| DERIVATIVE OF CURRENT SPEED DV/DX
!| DVY            |-->| DERIVATIVE OF CURRENT SPEED DV/DY
!| DZHDT          |-->| WATER DEPTH DERIVATIVE WITH RESPECT TO T
!| DZX            |-->| BOTTOM SLOPE ALONG X
!| DZY            |-->| BOTTOM SLOPE ALONG Y
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAP1          |<->| HIGHER LAYERS TABLE
!| FRE            |<->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| IELM3          |-->| TYPE OF 3D ELEMENT
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ISUB           |<->| ARRIVAL SUB-DOMAIN OF CHARACTERISTICS
!| ITR01          |<->| WORK TABLE
!| MESH           |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NPLAN
!| NRK            |-->| NUMBER OF RUNGE-KUTTA SUB-ITERATIONS
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| PROMIN         |-->| MINIMUM VALUE OF WATER DEPTH
!| SHF            |<->| BARYCENTRIC COORDINATES ALONG F OF THE 
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP            |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| SIKLE2         |-->| IKLE2 IN A BIEF_OBJ STRUCTURE
!| SINTET         |-->| SINE OF TETA ANGLE
!| SPHE           |-->| LOGICAL INDICATING SPHERICAL COORD ASSUMPTION
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF.
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| TGF            |-->| TANGENT OF THE LATITUDES OF THE POINTS 2D
!| TRA01          |<->| WORK TABLE
!| U              |-->| CURRENT SPEED ALONG X
!| V              |-->| CURRENT SPEED ALONG Y
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE TOMAWAC_MPI_TOOLS
      USE TOMAWAC_MPI
      USE INTERFACE_TOMAWAC, EX_PREPRO => PREPRO
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NRK,NPOIN3,NPOIN2,NELEM2,NPLAN,NF,DIFFRA
      INTEGER, INTENT(INOUT) :: IELM3
      DOUBLE PRECISION, INTENT(IN) :: DT,PROMIN
      DOUBLE PRECISION DZHDT(NPOIN2)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION XK(NPOIN2,NF),CG(NPOIN2,NF)
      DOUBLE PRECISION FREQ(NF)
      DOUBLE PRECISION SINTET(NPLAN),COSTET(NPLAN)
      DOUBLE PRECISION COSF(NPOIN2),TGF(NPOIN2)
      DOUBLE PRECISION DEPTH(NPOIN2),DZX(NPOIN2),DZY(NPOIN2)
      DOUBLE PRECISION U(NPOIN2),DUX(NPOIN2),DUY(NPOIN2)
      DOUBLE PRECISION V(NPOIN2),DVX(NPOIN2),DVY(NPOIN2)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3,8)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: ISUB(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: FRE(NPOIN3,NF)
      INTEGER, INTENT(IN) :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN) :: ETAP1(NPLAN)
      INTEGER, INTENT(INOUT) :: ITR01(NPOIN3,3),IFABOR(NELEM2,7)
      LOGICAL, INTENT(IN) :: COURAN,SPHE,PROINF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SHP,SHZ,SHF,CX,CY,CT,CF,TB
      TYPE(BIEF_OBJ), INTENT(IN)    :: SIKLE2,TETA
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH,MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER JF,I,ISTAT,IEL,I1,I2,I3,I4
      CHARACTER*3 CAR
      INTEGER          LAST_NOMB,NLOSTAGAIN,NUMBER,IER,NRECV,NUMBERLOST
      INTEGER          ITE,IP,IPLAN,NBB,IPOIN
      INTEGER          NARRSUM,IFF
      TYPE(BIEF_OBJ) :: BID
!
      INTEGER,DIMENSION(:,:,:), ALLOCATABLE :: GOODELT
      DOUBLE PRECISION,DIMENSION(:,:), ALLOCATABLE :: TES
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      SAVE
!
!----------------------------------------------------------------------
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(GOODELT(NPOIN2,NPLAN,NF))
        ALLOCATE(TES(NPOIN2,NPLAN))
        DEJA=.TRUE.
      ENDIF
!
!----------------------------------------------------------------------
!
      NFREQ=NF
      IF (.NOT.COURAN) THEN
!
!   -------------------------------------------------------------------
!
!   RELATIVE = ABSOLUTE => ADVECTION IN 3D
!   SEPARATES OUT THE FREQUENCIES
!
        DO 200 JF=1,NF
!
!      ---------------------------------------------------------------
!
!      COMPUTES THE ADVECTION FIELD
!
         CALL CONWAC
     &( CY%R  , CX%R  , CT%R , XK    , CG    , COSF  , TGF   , DEPTH ,
     &  DZY   , DZX   , FREQ , COSTET, SINTET, NPOIN2, NPLAN , JF    ,
     &  NF    , PROINF, SPHE , PROMIN, TRA01 , TRA01(1,2)    )
!
!      ----------------------------------------------------------------         
!
      DO IEL=1,NELEM2
        I1=IKLE2(IEL,1)
        I2=IKLE2(IEL,2)
        I3=IKLE2(IEL,3)
        IF((DEPTH(I1).LT.PROMIN).AND.(DEPTH(I2).LT.PROMIN).AND.
     &     (IFABOR(IEL,1).GT.0)) IFABOR(IEL,1)=-1
        IF((DEPTH(I2).LT.PROMIN).AND.(DEPTH(I3).LT.PROMIN).AND.
     &     (IFABOR(IEL,2).GT.0)) IFABOR(IEL,2)=-1
        IF((DEPTH(I3).LT.PROMIN).AND.(DEPTH(I1).LT.PROMIN).AND.
     &     (IFABOR(IEL,3).GT.0)) IFABOR(IEL,3)=-1
      ENDDO
!
      IF(DIFFRA.EQ.0) THEN      
!
!         CALLING CHARAC WITH NOMB=0, FN AND FTILD, THOUGH NOT USED
!         WILL GIVE THE NUMBER OF POINTS, HENCE SHZ
!
          CALL CHARAC(SHZ%ADR(JF)%P,SHZ%ADR(JF)%P,0,
     &                CX,CY,CT,TETA,DT,MESH3D%IFABOR,IELM3,
     &                NPOIN2,NPLAN,1,.FALSE.,BID,SHP%ADR(JF)%P,
     &                SHZ%ADR(JF)%P,TB,ELT(1,JF),ETA(1,JF),ITR01,
     &                ISUB(1,JF),MESH3D,NELEM2,NELEM2,SIKLE2,
     &                MESH%SURDET,
!                     A POSTERIORI INTERPOLATION
     &                .TRUE.,
!                     AND PERIODICITY 
     &                .TRUE.)
!    
      ELSE
!
          CALL MPOINT
     &  (CX%R,CY%R,CT%R,
     &   DT,X,Y,TETA%R,IKLE2,IFABOR,ETAP1,TRA01,TRA01(1,2),
     &   TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),
     &   SHP%ADR(JF)%P%R(         1:  NPOIN3),
     &   SHP%ADR(JF)%P%R(  NPOIN3+1:2*NPOIN3),
     &   SHP%ADR(JF)%P%R(2*NPOIN3+1:3*NPOIN3),
     &   SHZ%ADR(JF)%P%R,ELT(1,JF),ETA(1,JF),
     &   ITR01(1,1),NPOIN3,
     &   NPOIN2,NELEM2,NPLAN,JF,SURDET,-1,ITR01(1,2))
!
      ENDIF   
!
200   CONTINUE
!
      ELSE
!
!   ---------------------------------------------------------------
!
!   IN A RELATIVE REFERENCE SYSTEM => ADVECTION IN 4D
!   IT IS NO LONGER POSSIBLE TO SEPARATE THE FREQUENCIES OUT
!
      DO JF=1,NF
!
         CALL CONW4D
     &(CY%R,CX%R,CT%R,CF%R,
     & V,U,XK,CG,COSF,TGF,DEPTH,DZHDT,DZY,DZX,DVY,DVX,
     & DUY,DUX,FREQ,COSTET,SINTET,NPOIN2,NPLAN,JF,NF,PROINF,SPHE,
     & COURAN,TRA01,TRA01(1,2))
      ENDDO
!
       IF (NCSIZE.LE.1) THEN
        DO JF=1,NF       
!        
        CALL GTSH41(SHP%ADR(JF)%P%R,SHZ%ADR(JF)%P%R,
     &              CT%R,ELT(1,JF),ETA(1,JF),IKLE2,
     &              MESH%ELTCAR%I,NPOIN2,NELEM2,NPLAN,
     &              .FALSE.,.FALSE.)                 
!
!         THIS IS NORMALLY DONE OR SHOULD BE DONE IN CHARAC
!          
          DO IP=1,NPLAN
          DO IPOIN=1,NPOIN2
            I3=IPOIN+(IP-1)*NPOIN2
            TRA01(I3,1)=X(IPOIN)
            TRA01(I3,2)=Y(IPOIN)
            TRA01(I3,3)=TETA%R(IP)
            TRA01(I3,4)=FREQ(JF)
!
           IF(((CF%R(NPOIN3*(JF-1)+I3).GT.0.D0).AND.(JF.NE.1)).OR.
     &             (JF.EQ.NF)) THEN
!          IF(JF.EQ.NF) THEN
             FRE(I3,JF) = JF-1
             SHF%ADR(JF)%P%R(I3)=1.D0
           ELSE
             FRE(I3,JF) = JF
             SHF%ADR(JF)%P%R(I3)=0.D0
           ENDIF
!
          ENDDO
          ENDDO                
!
         WRITE(LU,*) 'FREQUENCE :',JF
         CALL PIED4D
     &(CX%R,CY%R,CT%R,CF%R,
     & DT,NRK,X,Y,TETA%R,FREQ,IKLE2,IFABOR,ETAP1,
     & TRA01,TRA01(1,2),TRA01(1,3),TRA01(1,4),TRA01(1,5),
     & TRA01(1,6),TRA01(1,7),TRA01(1,8),
     & SHP%ADR(JF)%P%R,SHZ%ADR(JF)%P%R,SHF%ADR(JF)%P%R,
     & ELT(1,JF),ETA(1,JF),FRE(1,JF),ITR01(1,1),NPOIN3,NPOIN2,
     & NELEM2,NPLAN,NF,SURDET,-1,ITR01(1,2))
        ENDDO
!
       ELSE
         DO JF=1,NF
       I1=NPOIN3*(JF-1)+1
       I2=NPOIN3*JF
       CALL INIP4D
     &(CX%R(I1:I2),CY%R(I1:I2),CT%R(I1:I2),CF%R(I1:I2),X,Y,
     & SHP%ADR(JF)%P%R,SHZ%ADR(JF)%P%R,SHF%ADR(JF)%P%R,
     & ELT(1,JF),ETA(1,JF),FRE(1,JF),
     & TRA01,TRA01(1,2),TRA01(1,3),TRA01(1,4),TETA%R,FREQ,IKLE2,
     & NPOIN2,NELEM2,NPLAN,JF,NF,IFABOR,GOODELT(1,1,JF))
         WRITE(LU,*) 'FREQUENCE :',JF
         IF (JF==1) THEN
           IF (ALLOCATED(SH_LOC_4D)) THEN
               DO IFF=1,NF
             DEALLOCATE(SH_LOC_4D(IFF)%SHP1,SH_LOC_4D(IFF)%SHP2,
     &                  SH_LOC_4D(IFF)%SHP3,SH_LOC_4D(IFF)%SHZ,
     &                  SH_LOC_4D(IFF)%SHF,SH_LOC_4D(IFF)%ELT,
     &                  SH_LOC_4D(IFF)%ETA,SH_LOC_4D(IFF)%FRE)
               ENDDO
             DEALLOCATE(SH_LOC_4D)
           ENDIF
           LAST_NOMB = 1
         ENDIF
!
         CALL CORRECT_GOODELT(GOODELT(1,1,JF),NPOIN2,NPLAN,MESH)
!
         IF (.NOT.ALLOCATED(NCHARA)) ALLOCATE(NCHARA(NF),NLOSTCHAR(NF),
     &                                        NSEND(NF))
         CALL INIT_TOMAWAC_4D(NCHARA(JF),NCHDIM,1,
     &                                       NPOIN3,LAST_NOMB)
!
         IF(.NOT.ALLOCATED(TEST)) ALLOCATE(TEST(NPOIN3,NF))
         IFREQ=JF
!
           CALL PIED4D_TOMAWAC
     &(CX%R(I1:I2),CY%R(I1:I2),CT%R(I1:I2),CF%R(I1:I2),
     & DT,NRK,X,Y,TETA%R,FREQ,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),
     &  TRA01(1,7),TRA01(1,8),
     &  SHP%ADR(JF)%P%R,SHZ%ADR(JF)%P%R,SHF%ADR(JF)%P%R,
     &  ELT(1,JF),ETA(1,JF),FRE(1,JF),
     &  ITR01(1,1),NPOIN3,NPOIN2,
     &  NELEM2,NPLAN,NF,SURDET,-1,ITR01(1,2),MESH%IFAPAR%I,TEST(1,JF),
     &  NCHDIM,NCHARA(JF),MESH,GOODELT(1,1,JF),JF)
! CHECKS WHETHER A CHARACTERISTICS CLOSE TO THE BOUNDARY EXITS AND NOT
! THE OTHER ONE. IN THIS CASE ONLY THE MAXIMUM CONTRIBUTION (FROM BOTH)
! IS CONSIDERED AND THE EXIT CHARACTERISTICS IS NOT TREATED
          DO IP = 1,NPOIN2
             DO IPLAN = 1,NPLAN
                TES(IP,IPLAN)  =TEST(IP+NPOIN2*(IPLAN-1),JF)
             ENDDO
          ENDDO
          DO IP=1,NPOIN3
            IF(TEST(IP,JF).LT.0.5D0) THEN
              SHP%ADR(JF)%P%R(IP)=0.D0
              SHP%ADR(JF)%P%R(IP+NPOIN3)=0.D0
              SHP%ADR(JF)%P%R(IP+2*NPOIN3)=0.D0
              SHZ%ADR(JF)%P%R(IP)=0.D0
              SHF%ADR(JF)%P%R(IP)=0.D0
            ENDIF
          ENDDO
          DO IPLAN = 1,NPLAN
          CALL PARCOM2
     & ( TES(1,IPLAN) ,
     &   TES(1,IPLAN) ,
     &   TES(1,IPLAN) ,
     &   NPOIN2 , 1 , 2 , 1 , MESH )
          ENDDO
          DO IP = 1,NPOIN2
             DO IPLAN = 1,NPLAN
                TEST(IP+NPOIN2*(IPLAN-1),JF)=TES(IP,IPLAN)
             ENDDO
          ENDDO
!
! HEAPCHAR(NCHARA,NFREQ) AND HEAPCOUNT(NCSIZE,NFREQ)
! HEAPCOUNT=> NUMBER OF CHARACTERISTICS ON EACH PROCESSOR
         CALL WIPE_HEAPED_CHAR_4D(TEST(1,JF),NPOIN3,.TRUE.,NSEND(JF),
     &                        NLOSTCHAR(JF),NCHDIM,
     &                        NCHARA(JF))
! IS NOT NECESSARILY USEFUL, CHECKS IF TEST==1, IN WHICH CASE IT IS DELETED
! FROM THE LIST OF CHARACTERISTICS BY ASSIGNING HEAPCAHR%NEPID==-1
!        DO WHILE(P_IMAX(NLOSTCHAR(JF))>0)! THERE ARE -REALLY- LOST TRACEBACKS SOMEWHERE
          CALL PREP_INITIAL_SEND_4D(NSEND,NLOSTCHAR,NCHARA)
! CREATES THE ARRAY 'SDISP' AND ORDERS THE DATA (ASCENDING)
          CALL GLOB_CHAR_COMM_4D ()
! SENDS SENDCHAR AND WRITES TO RECVCHAR
!
         IF(.NOT.ALLOCATED(ISPDONE)) ALLOCATE(ISPDONE(NPOIN3,NF))
         IF(.NOT.ALLOCATED(NARRV)) ALLOCATE(NARRV(NF))
         CALL ALLOC_LOCAL_4D(NARRV(IFREQ),IFREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)
!
         IF (NUMBERLOST>0) THEN
       CALL PIEDS4D_TOMAWAC_MPI
     &(CX%R(I1:I2),CY%R(I1:I2),CT%R(I1:I2),CF%R(I1:I2),
     &  DT,NRK,X,Y,TETA%R,FREQ,IKLE2,IFABOR,ETAP1,
     &  TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),TRA01(1,7),
     &  TRA01(1,8),SH_LOC_4D(JF)%SHP1,SH_LOC_4D(JF)%SHP2,
     &  SH_LOC_4D(JF)%SHP3,SH_LOC_4D(JF)%SHZ,SH_LOC_4D(JF)%SHF,
     &  SH_LOC_4D(JF)%ELT,SH_LOC_4D(JF)%ETA,SH_LOC_4D(JF)%FRE,
     &  NARRV(JF),NPOIN2,
     &  NELEM2,NPLAN,NF,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVCHAR_4D(1,JF))
        CALL ALLOC_AGAIN_4D(NARRV(IFREQ),IFREQ,NLOSTAGAIN,NUMBERLOST,
     &                   NUMBER)
        CALL ORGANIZE_SENDAGAIN_4D()
        CALL SUPP_ENVOI_AGAIN_4D(IFREQ,NUMBER)
!
        ITE = 0
        DO WHILE((NUMBERLOST>0).AND.(ITE.LE.20))
          ITE= ITE + 1
          CALL ORGANIZE_SENDAGAIN_4D()
          CALL ENVOI_AGAIN_4D(NRECV)
          CALL PIEDS4D_TOMAWAC_MPI
     &(CX%R(I1:I2),CY%R(I1:I2),CT%R(I1:I2),CF%R(I1:I2),
     & DT,NRK,X,Y,TETA%R,FREQ,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),TRA01(1,7),
     &  TRA01(1,8),SH_AGAIN_4D%SHP1,SH_AGAIN_4D%SHP2,SH_AGAIN_4D%SHP3,
     &  SH_AGAIN_4D%SHZ,SH_AGAIN_4D%SHF,
     &  SH_AGAIN_4D%ELT,SH_AGAIN_4D%ETA,SH_AGAIN_4D%FRE,
     &  NRECV,NPOIN2,
     &  NELEM2,NPLAN,NF,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVAGAIN_4D)
         CALL INCREM_ENVOI_RECV_4D(IFREQ,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                         NRECV)
        ENDDO ! END OF THE DOWHILE LOOP
        CALL FINAL_ORGA_RECV_4D(NARRV(IFREQ),IFREQ)
!
          ELSE
           CALL RESET_COUNT(IFREQ)
          ENDIF
!
         ENDDO
       ENDIF
      ENDIF
!
!     DEALLOCATE(GOODELT)
!
!----------------------------------------------------------------------
!
      RETURN
      END
