
       SUBROUTINE BEDLOAD_MEYER ! (_IMP_) 
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
      ! 0 - EFFET DE PENTE : FORMULE DE SOULBY   ! (_IMP_) 
      ! **************************************** ! 
      IF(SLOPEFF == 2) THEN 
        CALL OS('X=XY    ', X=ACP, Y=COEFPN ) 
      ENDIF 

 
      ! **************************************** ! 
      ! III - TRANSPORT PAR CHARRIAGE AVEC       ! (_IMP_) 
      !       CORRECTION POUR LA GRANULO ETENDUE ! (_IMP_) 
      ! **************************************** ! 
!RK      C2 = 8.D0 * SQRT(GRAV*DENS*DM**3) 
      C2 = 5.D0 * SQRT(GRAV*DENS*DM**3) 
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
      END SUBROUTINE BEDLOAD_MEYER 

**********************************************************************************************************
! Subroutine to calculate the new tau from secondary currents
!************************************************
       SUBROUTINE BEDLOAD_SECCURRENT(IELMU)
!************************************************
!
!
       USE DECLARATIONS_SISYPHE
       USE BIEF
       IMPLICIT NONE
!
       INTEGER LNG,LU
       COMMON/INFO/LNG,LU
!
       INTEGER I, IELMU
       DOUBLE PRECISION C, ALPHAP
!
! remember: QU = U_tel*H_tel, QV=V_tel*H_tel
!
!
!
! calculation of Pi
!       PI = ACOS(-1.D0)
!
!RK change for secondary currents
! calculating the gradient of the free surface in x-direction
       CALL VECTOR(T5,'=','GRADF          X',IELMU,
     &      1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
! for parallel computing
       IF (NCSIZE.GT.1) CALL PARCOM (T5, 2, MESH)
! calculating the gradient of the free surface in y-direction
       CALL VECTOR(T6,'=','GRADF          Y',IELMU,
     &      1.D0,Z,S,S,S,S,S,MESH,MSK,MASKEL)
! for parallel computing
       IF (NCSIZE.GT.1) CALL PARCOM (T6, 2, MESH)
! calculating the mass-matrix
      CALL VECTOR(T4,'=','MASBAS          ',IELMU,
     &      1.D0,S,S,S,S,S,S,MESH,MSK,MASKEL)
! for parallel computing
       IF (NCSIZE.GT.1) CALL PARCOM (T4, 2, MESH)
! for the weak formulation in FEM, there must be divided by the mass-matrix
       CALL OS ('X=Y/Z   ', T5,T5,T4,C,2,0.D0,1.D-12)
       CALL OS ('X=Y/Z   ', T6,T6,T4,C,2,0.D0,1.D-12)
!
!
! calculation of the x- and y-parts of the secondary current according to Engelung
! tau_x_sec = C*QV, tau_y_sec = C*QU
!
! at the moment alpha must be set here  (0,75 for very rough bottoms, 1 for smooth ones)
! attention: the variable alpha is more than the alpha from the theory
cgl    04.09.2008  scheinbar 
      ALPHAP = 1.0D0
cgl   ALPHAP = 7.0D0 / ALPHAP * XMVE *GRAV
c      ALPHAP = 3.5D0  / ALPHAP * XMVE *GRAV  ! entspricht alpha = 2.0     => ergab zu wenig Sekundärströmungseffekt
c      ALPHAP = 5.25D0 / ALPHAP * XMVE *GRAV  ! entspricht alpha = 1.3333
       ALPHAP = 6.125D0 / ALPHAP * XMVE *GRAV  ! entspricht alpha = 1.1429
!     WRITE(LU,*)'ALPHAP',1.D0/ALPHAP*7.D0*GRAV*XMVE
!
!
      CALL OS( 'X=YZ    ' , T1 , T6      , QU   , C   ) ! dzsdy*QU
      CALL OS( 'X=Y/Z   ' , T1 , T1      , HN   , C   ) ! dzsdy*QU/HN
      CALL OS( 'X=YZ    ' , T2 , T5      , QV   , C   ) ! dzsdx*QV
      CALL OS( 'X=Y/Z   ' , T2 , T2      , HN   , C   ) ! dzsdx*QV/HN
      CALL OS( 'X=-Y    ' , T2 , T2      , T3   , C   )
      CALL OS( 'X=X+Y   ' , T1 , T2      , T3   , C   ) ! QU*dzsdy - QV*dzsdx
!
      CALL OS( 'X=YZ    ' , T2 , QU      , QU   , C   ) ! QU**2
      CALL OS( 'X=Y/Z   ' , T2 , T2      , HN   , C   ) ! QU**2/HN
      CALL OS( 'X=Y/Z   ' , T2 , T2      , HN   , C   ) ! QU**2/HN**2
      CALL OS( 'X=YZ    ' , T3 , QV      , QV   , C   ) ! QV**2
      CALL OS( 'X=Y/Z   ' , T3 , T3      , HN   , C   ) ! QV**2/HN
      CALL OS( 'X=Y/Z   ' , T3 , T3      , HN   , C   ) ! QV**2/HN**2
      CALL OS( 'X=X+Y   ' , T2 , T3      , T3   , C   ) ! QU**2+QV**2
!
      CALL OS('X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) !(QU*dzsdy - QV*dzsdx)/(QU**2+QV**2)
!
      CALL OS( 'X=CX    ' , T1 , T2      , T3   , ALPHAP   ) ! T1 * 7/Alpha*XMVE*GRAV
      CALL OS( 'X=XY    ' , T1 , HN      , T3   , C   ) ! T1*HN
!
! only for Strickler roughness
! T4: chestr as kstr
!      CALL OS( 'X=C     ' , T4 , T2      , T3   ,71.2D0   ) ! set of kstr

!      CALL OS( 'X=XC    ' , T1 , HN      , T3   , GRAV   ) ! T1*HN*GRAV
!      CALL OS( 'X=YZ    ' , T2 , T4  , T4  , C   ) ! Chestr**2
!      CALL OS( 'X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) ! T1 / chestr**2
!      C = 1.D0/3.D0
!      CALL OS( 'X=Y**C   ' , T2 , HN   , HN   , C   ) ! HN**1/3
!      CALL OS( 'X=Y/Z   ' , T1 , T1 , T2, C ,2 , 0.D0,1.D-12) ! T1 / HN**1/3
!
!
! for all roughness laws 
      CALL OS( 'X=CXY   ' , T1 , CF      , T3   , 0.5D0   )!T1*CF/2
!
! tau_x_sek = -c*qv : T5
! tau_y_sek = c*qu : T6
      CALL OS('X=YZ    ' , T5 , T1    , QV,  C ) ! c*qv
      CALL OS('X=Y/Z   ' , T5 , T5    , HN,  C ) ! c*qv/HN
      CALL OS('X=YZ    ' , T6 , T1    , QU,  C ) ! c*qu
      CALL OS('X=Y/Z   ' , T6 , T6    , HN,  C ) ! c*qu/HN
      CALL OS('X=-Y    ' , T6 , T6    , QV,   C ) ! -c*qu
! sqrt(tau_x_sek**2+tau_y_sek**2) : T3
      CALL OS('X=YZ    ' , T2 , T5    , T5,  C ) ! T2 = (c*qv)**2
      CALL OS('X=YZ    ' , T3 , T6    , T6,  C ) ! T3 = (c*qu)**2
      CALL OS('X=X+Y   ' , T2 , T3    , T3,  C ) ! T2 = (c*qv)**2+(c*qu)**2
      CALL OS('X=SQR(Y)' , T3 , T2    , T3,  C ) ! T3 = sqrt((c*qu)**2+(c*qv)**2
!      print*,'taux',T5%r(1061),T6%r(1061)
!
! tau_x_ges = tob*effpnt*calfa + tau_x_sek : T1
! tau_y_ges = tob*effpnt*salfa + tau_y_sek : T2
      CALL OS( 'X=YZ    ' , T1 , TOB      , COEFPN   , C   ) ! tob*effpnt
      CALL OS( 'X=YZ    ' , T2 , T1      ,  SALFA   , C   ) ! tob*effpnt*salfa
      CALL OS( 'X=YZ    ' , T1 , T1      , CALFA   , C   ) ! tob*effpnt*calfa
      CALL OS('X=X+Y   ' , T1 , T5    , T3,  C ) ! tau_x_ges = tob*calfa+tau_x_sek
      CALL OS('X=X+Y   ' , T2 , T6    , T3,  C ) ! tau_y_ges = tob*salfa+tau_y_sek
!tau_ges=sqrt(tau_x_ges**2+tau_y_ges**2)
      CALL OS( 'X=YZ    ' , T3 , T1      , T1   , C   ) ! tau_x_ges**2
      CALL OS( 'X=YZ    ' , T4 , T2      , T2   , C   ) ! tau_y_ges**2
      CALL OS('X=X+Y   ' , T4 , T3    , T3,  C ) !tau_x_ges**2+tau_y_ges**2
      CALL OS('X=SQR(Y)' , T4 , T4    , T3,  C ) ! sqrt(tau_x_ges**2+tau_y_ges**2)
!
!
! new angle
! calfa_new = cos(tau_x_ges/tau_ges)
! salfa_new = sin(tau_y_ges/tau_ges)
      CALL OS('X=Y/Z   ' , T1 , T1 , T4, C ,2 , 0.D0,1.D-12) !tau_x_ges/tau_ges
      CALL OS('X=Y/Z   ' , T2 , T2 , T4, C ,2 , 0.D0,1.D-12) !tau_y_ges/tau_ges
!
! taken from effpnt ueber
! to be sure, that tau_x_ges/tau_ges are between (-1,1)
       DO i=1,NPOIN
         if(T1%R(i).lt.-1.D0.or.T1%R(i).gt.1.D0.or.
     &      T2%R(i).lt.-1.D0.or.T2%R(i).gt.1.D0) THEN
            print*,'not acceptable border crossing',i
         ENDIF
         T1%R(i) = MIN(T1%R(I),1.D0)
         T1%R(i) = MAX(T1%R(I),-1.D0)  
         T2%R(i) = MIN(T2%R(I),1.D0)
         T2%R(i) = MAX(T2%R(i),-1.D0)
       ENDDO
!
      CALL OS( 'X=Y     ' ,X=CALFA ,Y=T1 ) ! (tau_x_ges/tau_ges)
      CALL OS( 'X=Y     ' ,X=SALFA ,Y=T2 ) ! (tau_y_ges/tau_ges)
!
! coefpn_new = tau_ges / TOB
      CALL OS('X=Y/Z   ' , COEFPN , T4 , TOB, C ,2 , 0.D0,1.D-12) !coefpn=tau_ges/tob
!
!from effpnt
C   TRAITEMENT DES POINTS FRONTIERES A DEBIT IMPOSE :
C  PAS DE MODIFICATION DU QS QUAND IL EST DETERMINIE PAR UTILISATEUR
      DO 10 I = 1 , NPTFR
        IF (LIQBOR%I(I).EQ.5) THEN
          COEFPN%R(MESH%NBOR%I(I)) = 1.D0
        ENDIF
10    CONTINUE
!
      RETURN
      END    ! SUBROUTINE BEDLOAD_SECCURRENT(IELMU)
Cgl                     *****************aus v5p6
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
!RK
      USE declarations_sisyphe, ONLY: chestr
      
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS 
C
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)    
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
cgl  ----- 21.01.2008  für Donau  ------  
      DOUBLE PRECISION   temp
      INTEGER            tempi,  rest
      
       DOUBLE PRECISION Flaeche
       EXTERNAL Flaeche
      

C
C-----------------------------------------------------------------------
      INTEGER I

cgl  ----- 02.12.2008  für Donau  ------      
       TYPE Punkt2d                                                           !
          DOUBLE PRECISION  :: x                                              !
          DOUBLE PRECISION  :: y                                              !
       END TYPE Punkt2d                                                       !

       TYPE Polygon                                                         !
          TYPE (Punkt2d)    :: P(4)                                          ! Punkte des Polygons
       END TYPE Polygon 
                                                           !
       TYPE (Punkt2d) :: Knoten                                          ! Punkte des Polygons

       TYPE (Polygon) :: Poly

c      ---- Rigidbed im Outlet Do-km 2290,800 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       Poly%P(1)%x =  64919.535D0
       Poly%P(2)%x =  64979.568D0
       Poly%P(3)%x =  65060.302D0
       Poly%P(4)%x =  65001.511D0
            
       Poly%P(1)%y =  12071.058D0     
       Poly%P(2)%y =  11879.367D0  
       Poly%P(3)%y =  11907.521D0  
       Poly%P(4)%y =  12098.384D0  
       
       

C--------------------
C RIGID BEDS POSITION
C---------------------
C
C       DEFAULT VALUE:       ZR=ZF-100 
C                                                              
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-100.D0,NPOIN)                                                    
!RK
c        DO i=1,npoin
c           IF(CHESTR%R(I).GT.0.1) ZR(I) = ZF(I)
c        END DO
cgl  ----- 21.01.2008  für Donau  ------ 
c    ----- Im bereich erlaubter Erosion wird die Rauhheit  z.B. mit Janet von beispielsweise 0.05
c    ----- von beispielsweise 0.0500 auf 0.0501 gesetzt

        DO i=1,npoin

           temp   = CHESTR%R(I) * 100000.D0
           tempi  =  nint(temp)
           rest   = mod(tempi,100)
!            write(lu,*)'i ', i,' rest =', rest,'von ',CHESTR%R(I) 
!           IF(rest.GT.10)then
           IF(rest.GT.9)then
             ZR(I) = ZF(I)
           endif
              

        END DO 
C
C------------------
cgl  ----- 26.08.2008  Sohlschwellen für Donau ------ 
c    ----- Im bereich der Sohlschwellen wird die Rauhheit  z.B. mit Janet 
c    ----- von beispielsweise 0.0500 auf 0.05021 gesetzt. der Wert 21 wird
c    ----- abgefragt und Rigidbed ( ZR ) wird entsprechend gesetzt. 

        DO i=1,npoin

           temp   = CHESTR%R(I) * 100000.D0
           tempi  =  nint(temp)
           rest   = mod(tempi,100)
c           write(lu,*)'i ', i,' rest =', rest,'von ',tempi 
!        
!        c          ---- Sohlschwelle: Mühlham_1 z=300.20m
!                   IF(rest.EQ.21)then
!                     ZR(I) = 300.20D0
!                      write(lu,*)'ZR= ', ZR(I), 'Mühlham_1 z=300.20m' 
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_2 z=300.30m
!                   IF(rest.EQ.22)then
!                     ZR(I) = 300.30D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_2 z=300.30m' 
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_3 z=301.40m
!                   IF(rest.EQ.23)then
!                     ZR(I) = 301.40D0 
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_3 z=301.40m'  
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_4 z=300.50m
!                   IF(rest.EQ.24)then
!                     ZR(I) = 300.50D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_4 z=300.50m'  
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_5 z=300.60m
!                   IF(rest.EQ.25)then
!                     ZR(I) = 300.60D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_5 z=300.60m'  
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_6 z=300.40m
!                   IF(rest.EQ.26)then
!                     ZR(I) = 300.40D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_6 z=300.40m'  
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_7 z=299.60m
!                   IF(rest.EQ.27)then
!                     ZR(I) = 299.60D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_7 z=299.60m'  
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_8 z=299.90m
!                   IF(rest.EQ.28)then
!                     ZR(I) = 299.90D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_8 z=299.90m'  
!                   endif
!                   
!        c          ---- Sohlschwelle: Mühlham_9 z=299.60m
!                   IF(rest.EQ.29)then
!                     ZR(I) = 299.60D0
!                      write(lu,*)'ZR= ', ZR(I) ,'Mühlham_9 z=299.60m'  
!                   endif
!                   
c          ---- Rigidbed im Outlet Do-km 2256,600 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           IF(Y(I).LE.-6380)then
             ZR(I) = ZF(I)
!              write(lu,*)'ZR= ', ZR(I) ,'Mühlham_9 z=299.60m'  
           endif
           
c          ---- Rigidbed im Outlet Do-km 2290,800 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!              IF(X(I).GT. 64800)then
!                ZR(I) = ZF(I)
!               endif
            
c          ---- Rigidbed im Outlet Do-km 2290,800 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Knoten-Belegung in lokale Variable
               Knoten%x = X(I)     
               Knoten%y = Y(I)     
!   Test, ob Knoten im Polygon (z.b.Klappfeld) liegen       
            IF ((Flaeche(Poly%P(1)%x,Poly%P(1)%y,
     & Poly%P(2)%x,Poly%P(2)%y,Knoten%x,Knoten%y) .GT. 0.0  
     &    .AND.  Flaeche(Poly%P(2)%x,Poly%P(2)%y,
     & Poly%P(3)%x,Poly%P(3)%y,Knoten%x,Knoten%y) .GT. 0.0  
     &    .AND.  Flaeche(Poly%P(3)%x,Poly%P(3)%y,
     & Poly%P(4)%x,Poly%P(4)%y,Knoten%x,Knoten%y) .GT. 0.0  
     &    .AND.  Flaeche(Poly%P(4)%x,Poly%P(4)%y,
     & Poly%P(1)%x,Poly%P(1)%y,Knoten%x,Knoten%y) .GT. 0.0 )
     &    .OR. ( Flaeche(Poly%P(1)%x,Poly%P(1)%y,
     & Poly%P(2)%x,Poly%P(2)%y,Knoten%x,Knoten%y) .LT. 0.0  
     &    .AND.  Flaeche(Poly%P(2)%x,Poly%P(2)%y,
     & Poly%P(3)%x,Poly%P(3)%y,Knoten%x,Knoten%y) .LT. 0.0  
     &    .AND.  Flaeche(Poly%P(3)%x,Poly%P(3)%y,
     & Poly%P(4)%x,Poly%P(4)%y,Knoten%x,Knoten%y) .LT. 0.0  
     &    .AND.  Flaeche(Poly%P(4)%x,Poly%P(4)%y,
     & Poly%P(1)%x,Poly%P(1)%y,Knoten%x,Knoten%y) .LT. 0.0 ))THEN     
              ZR(I) = ZF(I) - 0.11D0
            ENDIF                                       
           
           

        END DO
C
C------------------
C SMOOTHING OPTION
C------------------
C       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
C                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
C
        NLISS = 0        
C
C
      RETURN
      END SUBROUTINE NOEROD
C      
!      ***********************************************
       DOUBLE PRECISION FUNCTION Flaeche (AX,AY,BX,BY,CX,CY)  
!      ***********************************************                                                         !
       DOUBLE PRECISION  :: Ax,AY,BX,BY,CX,CY                                                               !
       Flaeche = Ax*By-Ay*Bx+Ay*Cx-Ax*Cy+Bx*Cy-By*Cx 
       RETURN                               
       END ! FUNCTION Flaeche 

      ! ************************* ! 
        SUBROUTINE BEDLOAD_EFFPNT ! (_IMP_) 
      ! ************************* ! 
 
     & (MASKEL,LIQBOR,S,ZF,U2D,V2D,UCMOY,NPOIN,NPTFR,IELMT,KENT, 
     &  BETA,PI,MSK,MESH,DZFDX,DZFDY,CTETA,STETA, 
     &  COEF,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2, 
     &  TOB,XMVS,XMVE,DM,GRAV,UNSV2D) 
 
C**********************************************************************C 
C SISYPHE VERSION 5.1  11/09/1995  E. PELTIER                          C 
C SISYPHE VERSION 5.1  11/09/1995  C. LENORMANT                        C 
C SISYPHE VERSION 5.1  11/09/1995  J.-M. HERVOUET                      C 
C**********************************************************************C 
 
             ! ========================================= ! 
             ! Calcul des parametres de l'effet de pente ! 
             ! ========================================= ! 
 
 
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
C .____________.____.__________________________________________________C 
C |    NOM     |MODE|                    ROLE                          C 
C |____________|____|__________________________________________________C 
C |____________|____|__________________________________________________C 
C                                                                      C 
C                =>  Can't be change                                   C 
C                <=> Can be change                                     C 
C                <=  Must be set                                       C  
C ---------------------------------------------------------------------C 
!                                                                      ! 
! CALLED BY BEDLOAD_INIT                                               ! 
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
      USE INTERFACE_SISYPHE,EX_BEDLOAD_EFFPNT => BEDLOAD_EFFPNT 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
 
 
      ! 2/ GLOBAL VARIABLES 
      ! ------------------- 
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D 
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF, U2D,V2D, UCMOY, TOB 
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, KENT 
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA 
      DOUBLE PRECISION, INTENT(IN)    :: BETA, PI, PHISED, BETA2 
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, GRAV, DM 
      LOGICAL,          INTENT(IN)    :: MSK 
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX, DZFDY 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CTETA,STETA 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF, CALFA, SALFA 
 
 
      ! 3/ LOCAL VARIABLES 
      ! ------------------ 
      INTEGER          :: I, K 
      DOUBLE PRECISION :: C,ZETA,C1,CALPHA,SALPHA,AA,BB
      DOUBLE PRECISION :: CPSI,SPSI,DZF,TANPHI,CZETA,SZETA,SURBETA2 
      DOUBLE PRECISION :: NORM ,TT1 
! 
!======================================================================! 
!======================================================================! 
!                               PROGRAMME                              ! 
!======================================================================! 
!======================================================================! 
! 
!     DETERMINATION DE COS ET SIN TETA 
!     TETA = ANGLE DE L'ECOULEMENT PAR RAPPORT A AXE X 
!
      DO I=1,NPOIN
        IF(UCMOY%R(I).GE.1.D-12) THEN
          CTETA%R(I)=U2D%R(I)/UCMOY%R(I)
          STETA%R(I)=V2D%R(I)/UCMOY%R(I)
        ELSE
          CTETA%R(I)=1.D0
          STETA%R(I)=0.D0
        ENDIF
      ENDDO
! 
!----------------------------------------------------------------------  
! 
!     CALCUL DE LA PENTE  : D(ZF)/DX ET D(ZF)/DY (VALEURS NODALES) 
! 
      CALL VECTOR(DZFDX, '=', 'GRADF          X',IELMT,1.D0,ZF,S,S, 
     *            S,S,S,MESH,MSK,MASKEL) 
      CALL VECTOR(DZFDY, '=', 'GRADF          Y',IELMT,1.D0,ZF,S,S, 
     *            S,S,S,MESH,MSK,MASKEL) 
C 
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DZFDX,2,MESH)
        CALL PARCOM(DZFDY,2,MESH)
      ENDIF 
C 
      CALL OS('X=XY    ',X=DZFDX,Y=UNSV2D)  
      CALL OS('X=XY    ',X=DZFDY,Y=UNSV2D)    
! 
!====================================================================== 
!
!     CALCUL DE L'ANGLE DU TRANSPORT SOLIDE ALFA = TETA + DEVIATION 
!
!     1 : KOCH ET FLOKSTRA
!
      IF(DEVIA==1) THEN 
! 
      C = 2.D0*(XMVS-XMVE)*GRAV*DM/3.D0
      DO I=1,NPOIN
!RK        TT1=C/MAX(TOB%R(I),1.D-10)
        TT1 = BETA
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM 
      ENDDO 
! 
!     2 : TALMON ET AL. JHR 1995 33(4) 
! 
      ELSEIF(DEVIA==2) THEN 
! 
      SURBETA2=1.D0/BETA2
      C = (XMVS-XMVE)*GRAV*DM*SURBETA2**2
      DO I=1,NPOIN
        TT1=SQRT(C/MAX(TOB%R(I),1.D-10))
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM 
      ENDDO 
!  
      ENDIF 
! 
!====================================================================== 
! 
!     CALCUL DE COEF POUR LA PRISE EN COMPTE DE L'EFFET DE PENTE 
!     SUR L'AMPLITUDE DU TRANSPORT SOLIDE                         
! 
!     METHODE 1 (EMPIRICAL METHOD) 
! 
      IF(SLOPEFF==1) THEN 
! 
        DO I=1,NPOIN
          COEF%R(I)=MAX(0.D0,
     *    1.D0-BETA*(DZFDX%R(I)*CTETA%R(I)+DZFDY%R(I)*STETA%R(I)) )
        ENDDO
! 
!     METHODE 2 : SOULSBY 1997 DYNAMICS OF MARINE SANDS p107-108 
!
      ELSEIF(SLOPEFF.EQ.2) THEN 
C 
        TANPHI = TAN(PHISED*PI/180.D0) 
C 
        DO I=1,NPOIN 
C
C         COSINUS ET SINUS DE LA DIRECTION DE LA PENTE
          DZF=SQRT(DZFDX%R(I)**2+DZFDY%R(I)**2)
          IF(DZF.GT.1.D-12) THEN
            CALPHA=DZFDX%R(I)/DZF
            SALPHA=DZFDY%R(I)/DZF
          ELSE
            CALPHA=1.D0
            SALPHA=0.D0
          ENDIF
C 
C         ZETA ANGLE QUE FAIT LA PENTE AVEC HORIZONTALE (BETA DE SOULSBY) 
          ZETA=ATAN(DZF)
          CZETA=COS(ZETA)
          SZETA=SIN(ZETA)    
C   
C         PSI ANGLE DU COURANT PAR RAPPORT A LA DIRECTION DE LA PENTE
C         PSI=TETA%R(I)-ALPHA
          CPSI=CTETA%R(I)*CALPHA+STETA%R(I)*SALPHA
          SPSI=STETA%R(I)*CALPHA-CTETA%R(I)*SALPHA  
          C1=(CZETA*TANPHI)**2-(SPSI*SZETA)**2 
          COEF%R(I)=MAX((CPSI*SZETA+SQRT(MAX(C1,0.D0)))/TANPHI,0.D0) 
          COEF%R(I)=MAX(COEF%R(I),0.D0) 
C 
        ENDDO 
! 
      ENDIF 
!
! ********************************************************************* ! 
!     V - TRAITEMENT DES POINTS FRONTIERES A DEBIT IMPOSE               ! 
!      PAS DE MODIFICATION DU QS QUAND IL EST DETERMINE PAR UTILISATEUR !  
! ********************************************************************* !
! 
      DO K = 1 , NPTFR 
         IF (LIQBOR%I(K) == KENT) COEF%R(MESH%NBOR%I(K)) = 1.D0 
!                           R.K. mai 2007 
!                           KSORT = 4
         IF (LIQBOR%I(K) == 4) COEF%R(MESH%NBOR%I(K)) = 1.D0 
      ENDDO 
! 
!====================================================================== 
!====================================================================== 
! 
      RETURN 
      END SUBROUTINE BEDLOAD_EFFPNT
C                         *********************
                          SUBROUTINE INIT_COMPO
C                         *********************
C
     *(NCOUCHES)
C
C***********************************************************************
C SISYPHE VERSION 6.0
C                             Matthieu GONZALES DE LINARES 2002
C
C                                                
C COPYRIGHT EDF-BAW-IFH   
C***********************************************************************
C
C     FONCTION  : DISTRIBUTION DES CLASSES
C                 % PAR COUCHE, STRATIFICATION 
C     SUBROUTINE A REMPLIR PAR l'UTILISATEUR
C
C 
C     FUNCTION  : INITIAL FRACTION DISTRIBUTION, STRATIFICATION, 
C                 VARIATION IN SPACE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                |    |  
C |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE
C |                |    | AVAIL(NPOIN,10,NSICLA)
C |    ES          |<-- | THICKNESS FOR EACH LAYER AND NODE ES(NPOIN,10)
C |    NCOUCHES    |--> | NUMBER OF LAYER FOR EACH POINT
C |    NSICLA      |--> | NUMBER OF SIZE-CLASSES OF BED MATERIAL
C |                |    | (LESS THAN 10)
C |    NPOIN       |--> | NUMBER OF NODES
C |________________|____|______________________________________________
C MODE : -->(INPUT), <--(RESULT), <--> (MODIFIED INPUT)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : INIT_AVAI 
C PROGRAMMES APPELES : NONE
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
C
      IMPLICIT NONE  
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I , J  
C 
C-----------------------------------------------------------------------
C
      WRITE(LU,*) 'in init_compo'
      DO J=1,NPOIN
C
C       BY DEFAULT : UNIFORM BED COMPOSITION
C
!          NCOUCHES(J) = 1
!          DO I = 1, NSICLA
!            AVAIL(J,1,I) = AVA0(I)
!            AVAIL(J,2,I) = AVA0(I)
!          ENDDO
          NCOUCHES(J) =  3
          ES(j,1)     =  0.1D0    !  Schichtdicke Activlayer sollte mit dem We$
          ES(j,2)     =  1.0D0    !  Schichtdicke Stratum (erste Unterschicht)
          ES(j,3)     =  0.0D0    !  wird keine Angabe gemacht  geht sie bis $

!        Oberwasser Isar    0.13; 0.26; 0.39; 0.14; 0.08 ------------

        ! active Layer ------------
            AVAIL(J,1,1) = 0.13D0           ! Fraktion 1   Oberwasser Isar
            AVAIL(J,1,2) = 0.26D0           ! Fraktion 2   Oberwasser Isar
            AVAIL(J,1,3) = 0.39D0           ! Fraktion 3   Oberwasser Isar
            AVAIL(J,1,4) = 0.14D0           ! Fraktion 4   Oberwasser Isar
            AVAIL(J,1,5) = 0.08D0           ! Fraktion 5   Oberwasser Isar

        ! 1te Unterschicht---------
            AVAIL(j,2,1) = 0.13D0           ! Fraktion 1   Oberwasser Isar
            AVAIL(j,2,2) = 0.26D0           ! Fraktion 2   Oberwasser Isar
            AVAIL(j,2,3) = 0.39D0           ! Fraktion 3   Oberwasser Isar
            AVAIL(j,2,4) = 0.14D0           ! Fraktion 4   Oberwasser Isar
            AVAIL(j,2,5) = 0.08D0           ! Fraktion 5   Oberwasser Isar
        ! 2te Unterschicht---------
            AVAIL(j,3,1) = 0.13D0           ! Fraktion 1   Oberwasser Isar
            AVAIL(j,3,2) = 0.26D0           ! Fraktion 2   Oberwasser Isar
            AVAIL(j,3,3) = 0.39D0           ! Fraktion 3   Oberwasser Isar
            AVAIL(j,3,4) = 0.14D0           ! Fraktion 4   Oberwasser Isar
            AVAIL(j,3,5) = 0.08D0           ! Fraktion 5   Oberwasser Isar


C      
C  TO BE FILLED BY THE USER
!      NCOUCHES(J) = 10
!      ES(J,1) = 1.D0
!      ES(J,2) = 1.D0
!      ES(J,3) = 1.D0
!      ES(J,4) = 1.D0
!      ES(J,5) = 1.D0
!      ES(J,6) = 1.D0
!      ES(J,7) = 1.D0
!      ES(J,8) = 1.D0
!      ES(J,9) = 1.D0      
!        DO I = 1, NSICLA
!          DO K = 1, NCOUCHES(J)
!          AVAIL(J,K,I) = AVA0(I)
!          ENDDO
!        ENDDO
C          
      ENDDO 
      WRITE(LU,*) 'nach init_compo'     
C 
C-----------------------------------------------------------------------
C
      RETURN
      END
