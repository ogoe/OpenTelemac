C                        *******************
                          SUBROUTINE SORPAR
C                        *******************
C
     .   (NPARF, NPART, NPMAX  , IT  , KENT, KFLUID,  
     .    KLIST, KPAR , KPARMOB, KPER, EPS , LM    ,
     .    MASS , NUT  , P      , RHO , S   , TKE   ,
     .    VX   , VZ   , X      , XMIN, XMAX, Z     , 
     .    ZMIN , ZMAX                              )
C
C----------------------------------------------------------------------
C                         MAIN VARIABLES
C .________________.___._______________________________________________
C !           !      !                                                !
C !   NAME    ! MODE !                MEANING                         !
C !___________!______!________________________________________________!
C !           !      !                                                !
C ! EPS       ! <--> ! DISSIPATION RATE                               !
C ! IT        ! -->  ! INDEX OF THE CURRENT TIME STEP                 !
C ! KENT      ! <--> ! CONDITION TYPE AT FLUID BOUNDARIES             !
C ! KFLUID    ! <--> ! FLUID TYPE                                     !
C ! KLIST     ! -->  ! LOGICAL FOR LISTING PRINTOUT                   !
C ! KPAR      ! <--> ! PARTICLE  TYPE                                 !
C ! KPARMOB   ! <--> ! MOVING WALL OR EDGE PARTICLE TYPE              !
C ! KPER      ! -->  ! LOGICAL INDEX FOR PERIODICITY                  !
C ! LM        ! <--> ! MIXING LENGTH                                  !
C ! MASS      ! <--> ! PARTICLE MASS                                  !
C ! NPARF     ! <--> ! NUMBER OF FLUID PARTICLES                      !
C ! NPART     ! <--> ! TOTAL PARTICLE NUMBER                          !
C ! NPMAX     ! -->  ! MAXIMUM PARTICLE NUMBER                        !
C ! NUT       ! <--> ! EDDY VISCOSITY                                 !
C ! P         ! <--> ! PRESSURE                                       !
C ! RHO       ! <--> ! DENSITY                                        !
C ! S         ! <--> ! RATE OF STRAIN                                 !
C ! TKE       ! <--> ! TURBULENT KINETIC ENERGY                       !
C ! VX, VZ    ! <--> ! VELOCITY COMPONENTS                            ! 
C ! X, Z      ! <--> ! PARTICLE POSITION                              !
C ! XMIN, XMAX! -->  ! MINIMUM AND MAXIMUM X OF THE DOMAIN            !
C ! ZMIN, ZMAX! -->  ! MINIMUM AND MAXIMUM Z OF THE DOMAIN            !
C !___________!______!________________________________________________!
C
C MODE : -->(NON MODIFIED DATA), <--(RESULT), <-->(MODIFIED DATA)
C----------------------------------------------------------------------
C
C SPARTACUS2D V5P9
C D. Violeau           & R. Issa
C +33(0)1-30-87-78-31 // +33(0)1-30-87-84-28 
C LNHE - 2008
C
C FONCTION : gere les sorties de particules
C FUNCTION : deals with outgoing particles
C
C PROGRAMMES APPELANT : SPARTACUS2D
C CALLED BY
C
C PROGRAMMES APPELES  : -
C CALLED PROGRAMS
C
C----------------------------------------------------------------------
C
C Variables
C==========
C
      IMPLICIT NONE
C
      INTEGER NPMAX , NPARF, NPART, NPARS
      INTEGER NPARST, IT   , I    , J
      INTEGER LNG   , LU
      COMMON/INFO/LNG,LU      
C
      LOGICAL KLIST, KPER
C
      DOUBLE PRECISION XMIN, ZMIN, XMAX, ZMAX
C 
      INTEGER KPAR   (NPMAX), KENT(NPMAX), KFLUID(NPMAX)
      INTEGER KPARMOB(NPMAX)
      
C
      DOUBLE PRECISION X   (NPMAX), Z  (NPMAX), RHO(NPMAX)
      DOUBLE PRECISION VX  (NPMAX), VZ (NPMAX), P  (NPMAX)
      DOUBLE PRECISION MASS(NPMAX), TKE(NPMAX), EPS(NPMAX)
      DOUBLE PRECISION NUT (NPMAX), S  (NPMAX), LM (NPMAX)
C     
      SAVE NPARST
C     
C Particules sortantes
C=====================
C Outgoing particles
C===================
C     
      IF (IT.EQ.1) THEN
        NPARST=0
      ENDIF
C     
      NPARS=0
      DO 666 I=1,NPARF
C
        IF ((.NOT.KPER
     .      .AND.(X(I).GT.XMAX.OR.X(I).LT.XMIN))
     .      .OR.  Z(I).GT.ZMAX.OR.Z(I).LT.ZMIN) THEN
C
          NPARS=NPARS+1
          DO 667 J=I,NPART-1
            KENT   (J)=KENT   (J+1)
            KPAR   (J)=KPAR   (J+1)
            KFLUID (J)=KFLUID (J+1)
            KPARMOB(J)=KPARMOB(J+1)	    	    
            X      (J)=X      (J+1)
            Z      (J)=Z      (J+1)
            VX     (J)=VX     (J+1)
            VZ     (J)=VZ     (J+1)
            RHO    (J)=RHO    (J+1)
            P      (J)=P      (J+1)
            MASS   (J)=MASS   (J+1)
            TKE    (J)=TKE    (J+1)
            EPS    (J)=EPS    (J+1)
            NUT    (J)=NUT    (J+1)
            S      (J)=S      (J+1)
            LM     (J)=LM     (J+1)
 667      CONTINUE
          NPARF=NPARF-1
          NPART=NPART-1
C
        ENDIF
C
 666  CONTINUE
C     
      NPARST=NPARST+NPARS
C     
C Impressions
C============
C Writing
C========
C     
      IF (KLIST) THEN
        IF (NPARS.NE.0) THEN
          IF (LNG.EQ.1) THEN
            PRINT*,NPARS,' particules sorties'
            PRINT*,'Nombre total de particules sorties : ',NPARST
          ELSEIF (LNG.EQ.2) THEN
            PRINT*,NPARS,' outgoing particles'
            PRINT*,'Total number of outgoing particles : ',NPARST	  
          ENDIF      
        ENDIF
        IF (LNG.EQ.1) THEN
          PRINT*,'Nombre de particules fluides       : ',NPARF
        ELSEIF (LNG.EQ.2) THEN
          PRINT*,'Number of fluid particles          : ',NPARF	
        ENDIF
      ENDIF
C     
      RETURN
      END
