C                       ***********************
                        SUBROUTINE FLU_TCHAMEN
C                       ***********************

     *(H1,H2,ETA1,ETA2,U1,U2,V1,V2,
     * XNN,YNN,FLX,G,EPS)
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
C A TERMINER !!!
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
      DOUBLE PRECISION, INTENT(IN)    :: G,H1,H2,ETA1,ETA2,U1,U2
      DOUBLE PRECISION, INTENT(IN)    :: V1,V2,XNN,YNN,EPS
      DOUBLE PRECISION, INTENT(INOUT) :: FLX(3)
C***********************************************************************
C
      INTEGER NSG,J,IVAR,IS,K,ILIM,ERR     
C
      DOUBLE PRECISION VNX,VNY,VNL,ZF1,ZF2
      DOUBLE PRECISION FLUIJ_20
      DOUBLE PRECISION SIGMAX,UNORM
c
      INTEGER CHOICE_D
      DOUBLE PRECISION ALPHA,FLUIJ_1
      DOUBLE PRECISION UI,UJ,VI,VJ
      DOUBLE PRECISION U_IJ,D_IJ,C_IJ,C_I,C_J,UI0,UJ0
      DOUBLE PRECISION  FLUIJ_2,FLUIJ_3

C-----------------------------------------------------------------------
C**************************************************************
C VOIR SON EFFET
         ALPHA=1.D0
         CHOICE_D=2
C**************************************************************
C INITIALISATION DE FLX      
         DO IVAR=1,3
           FLX(IVAR) = 0.D0
         ENDDO
C
C-----------------------------------------------------------------------
C BATHYMETRIES
         ZF1   =    ETA1-H1
         ZF2   =    ETA2-H2
C VELOCITIES
         UI=U1
         VI=V1
         UJ=U2
         VJ=V2
C
C ROTATION
C
         UI0 = UI
         UI  = XNN*UI0+YNN*VI
         VI  =-YNN*UI0+XNN*VI
C
         UJ0 = UJ
         UJ  = XNN*UJ0+YNN*VJ
         VJ  =-YNN*UJ0+XNN*VJ

C
C WET/DRY TREATMENT
C
        CALL WETDRY(ETA1,ZF1,H1,UI,VI,ETA2,ZF2,H2,UJ,VJ,EPS)
C
1234   CONTINUE
C
C LET'S COMPUTE D_IJ
C
        IF(CHOICE_D.EQ.1)THEN
C ZOKAGOA'S CHOICE

        U_IJ=0.5D0*(UI+UJ)
        C_IJ=SQRT(0.5*G*(H1+H2))
        D_IJ=ALPHA*MAX(ABS(U_IJ-C_IJ),MAX(ABS(U_IJ),ABS(U_IJ+C_IJ)))

        ELSEIF(CHOICE_D.EQ.2)THEN
C TORO'S CHOICE
        C_I=SQRT(G*H1)
        C_J=SQRT(G*H2)
        D_IJ=MAX(ABS(UI)+C_I,ABS(UJ)+C_J)
        ELSE
C
C ERROR MESSAGE        
C
        IF(LNG.EQ.1) WRITE(LU,4010) ERR
        IF(LNG.EQ.2) WRITE(LU,4020) ERR
4010    FORMAT(1X,'FLU_AZZ : ERREUR DANS LE CHOIX DE L UPWIND : ',/,1X,
     *         'CODE D''ERREUR : ',1I6)
4020    FORMAT(1X,'FLU_AZZ: ERROR IN THE UPWIND CHOICE: ',/,1X,
     *        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
       ENDIF

5000   CONTINUE
C
C CENTERED FLUX COMPUTATION
C
C TCHAMEN FLUX
           FLUIJ_1=0.5D0*(H1*UI+H2*UJ)
           FLUIJ_2=0.5D0*(H1*(UI*UI) + H2*(UJ*UJ) +
     *              G*H1*(ETA1+ETA2))
           FLUIJ_3=0.5D0*(H1*UI*VI+H2*UJ*VJ)
C
C UPWIND ADDING
C 
          FLUIJ_1=FLUIJ_1-0.5D0*D_IJ*(ETA2-ETA1)
          FLUIJ_2=FLUIJ_2-0.5D0*D_IJ*(H2*UJ-H1*UI)
          FLUIJ_3=FLUIJ_3-0.5D0*D_IJ*(H2*VJ-H1*VI)

C
C INVERSE ROTATION
C
         FLUIJ_20 = FLUIJ_2
         FLUIJ_2  = XNN*FLUIJ_20-YNN*FLUIJ_3
         FLUIJ_3  = YNN*FLUIJ_20+XNN*FLUIJ_3
C
C FINAL FLUX 
C
         FLX(1) =  FLUIJ_1
         FLX(2) =  FLUIJ_2 
         FLX(3) =  FLUIJ_3 
C
5500   CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
