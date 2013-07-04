!                    *****************
                     SUBROUTINE SOUKEP
!                    *****************
!
     &(CV1,CV2,S1K,S1E,U,V,W,DELTAR,RI,
     & DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ,DTADZ,
     & AK,EP,C1,C2,CMU,GRAV,TR,NPOIN3,MSK,MASKEL,MESH3D,IELM3,S,DT,
     & VENT,WIND,H,EBORS,NPOIN2,KMIN,EMIN,PRANDTL)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE SOURCE TERMS IN THE DIFFUSION EQUATION OF
!+                K AND EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  A. GARAPON  & V. BOYER LNHE
!+        **/02/2001
!+
!+   REVISITED
!
!history  J-M HERVOUET (LNHE)
!+        24/12/2009
!+        V6P0
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        12/04/2013
!+        V6P3
!+   Richardson's number formula changed. It is built only if asked for
!+   post-processing.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |<->| TURBULENT ENERGY K
!| C1             |-->| CONSTANT FOR K-EPSILON MODEL
!| C2             |-->| CONSTANT FOR K-EPSILON MODEL
!| CMU            |-->| CONSTANT FOR K-EPSILON MODEL
!| CV1            |<->| SOURCE TERM FOR K AND EPSILON
!| CV2            |<->| SOURCE TERM FOR K AND EPSILON
!| DELTAR         |-->| DELTA(RHO)/RHO
!| DT             |-->| TIME STEP
!| DTADZ          |<->| DERIVATIVE OF TRACEUR N0 1 WITH RESPECT TO Z
!| DUDX           |<->| DU/DX
!| DUDY           |<->| DU/DY
!| DUDZ           |<->| DU/DZ
!| DVDX           |<->| DV/DX
!| DVDY           |<->| DV/DY
!| DVDZ           |<->| DV/DZ
!| DWDX           |<->| DW/DX
!| DWDY           |<->| DW/DY
!| DWDZ           |<->| DW/DZ
!| EBORS          |<->| EPSILON AT SURFACE
!| EMIN           |-->| MINIMUM VALUE FOR EPSILON WHEN CLIPPING
!| EP             |<->| TURBULENT DISSIPATION EPSILON
!| GRAV           |-->| GRAVITY ACCELERATION
!| H              |-->| WATER DEPTH
!| IELM3          |---| TYPE OF ELEMENT
!| KMIN           |-->| MINIMUM VALUE FOR K WHEN CLIPPING
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| PRANDTL        |-->| PRANDTL NUMBER
!| RI             |<->| RICHARDSON NUMBER
!| S              |-->| BIEF_OBJ
!| S1E            |<->| C2*EPSILON/K
!| S1K            |<->| EPSILON/K
!| TR             |<->| TABLEAU DE TRAVAIL PAR POINTS
!| U              |-->| VELOCITY COMPONENT
!| V              |-->| VELOCITY COMPONENT
!| VENT           |-->| LOGICAL FOR WIND
!| W              |-->| VELOCITY COMPONENT
!| WIND           |-->| STRUCTURE FOR WIND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT,NONHYD,CLIPK,CLIPE,WSIK,
     &                                   YAP,RHO0,PERNORM2,PERPROD,
     &                                   RIMIN,RIMAX,OPTPROD,SORG3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,IELM3,NPOIN2
!
      DOUBLE PRECISION, INTENT(INOUT) :: CV1(NPOIN3),CV2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: S1K(NPOIN3),S1E(NPOIN3)
!     AK AND EP COULD BE CHANGED IF SOURCE TERMS WERE TREATED HERE
      DOUBLE PRECISION, INTENT(INOUT) :: AK(NPOIN3),EP(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: RI(NPOIN3)
!
      TYPE (BIEF_OBJ), INTENT(INOUT), TARGET :: DUDX,DUDY,DUDZ
      TYPE (BIEF_OBJ), INTENT(INOUT), TARGET :: DVDX,DVDY,DVDZ
      TYPE (BIEF_OBJ), INTENT(INOUT), TARGET :: DWDX,DWDY,DWDZ,DTADZ,TR
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL,U,V,W,DELTAR,S,WIND,H
      TYPE (BIEF_OBJ), INTENT(INOUT) :: EBORS
!
      DOUBLE PRECISION, INTENT(IN)    :: C1,C2,CMU,GRAV,DT,KMIN,EMIN
      DOUBLE PRECISION, INTENT(IN)    :: PRANDTL
!
      LOGICAL, INTENT(IN)             :: MSK,VENT
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION VISC,G,S1,SDELTAZ,DTADZU,DTADZD,VITV,FAIR
      DOUBLE PRECISION TAU,A12LOG,ETA,LL,YY,YP,PRODQ,ROAIR
      DOUBLE PRECISION UNORM2,SURPRAN,UNSURTAU,SQRCMU
      INTEGER N,OPTKEPS,N2D,I
!
!     !!!!!!!!!!!!!!!!!  MEMORY BOOK-KEEPING, IMPORTANT !!!!!!!!!!!!!!!!
!
      TYPE(BIEF_OBJ), POINTER :: S2,PROD
!     DUDX AND DUDY WILL NOT BE USED AFTER BUILDING S2
!     THEIR MEMORY IS REUSED
      S2  =>DUDX
      PROD=>DUDY
!
!     !!!!!!!!!!!!!!  END OF MEMORY BOOK-KEEPING, IMPORTANT !!!!!!!!!!!!
!
!-----------------------------------------------------------------------
!
      ROAIR = 1.3D0
      SURPRAN=1.D0/PRANDTL
      SQRCMU=SQRT(CMU)
!
!-----------------------------------------------------------------------
!
!     COMPUTING VELOCITY GRADIENTS
!
      CALL VECTOR(DUDX,'=','GRADF          X',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DUDY,'=','GRADF          Y',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DUDZ,'=','GRADF          Z',IELM3,1.D0,U,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DVDX,'=','GRADF          X',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DVDY,'=','GRADF          Y',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DVDZ,'=','GRADF          Z',IELM3,1.D0,V,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DWDX,'=','GRADF          X',IELM3,1.D0,W,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DWDY,'=','GRADF          Y',IELM3,1.D0,W,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DWDZ,'=','GRADF          Z',IELM3,1.D0,W,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DTADZ,'=','GRADF          Z',IELM3,1.D0,DELTAR,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(TR,'=','MASBAS          ',IELM3,1.D0,
     &            S,S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DUDX ,2,MESH3D)
        CALL PARCOM(DUDY ,2,MESH3D)
        CALL PARCOM(DUDZ ,2,MESH3D)
        CALL PARCOM(DVDX ,2,MESH3D)
        CALL PARCOM(DVDY ,2,MESH3D)
        CALL PARCOM(DVDZ ,2,MESH3D)
        CALL PARCOM(DWDX ,2,MESH3D)
        CALL PARCOM(DWDY ,2,MESH3D)
        CALL PARCOM(DWDZ ,2,MESH3D)
        CALL PARCOM(DTADZ,2,MESH3D)
        CALL PARCOM(TR   ,2,MESH3D)
      ENDIF
!
      DO N=1,NPOIN3
        IF(TR%R(N).GT.1.D-6) THEN
          TR%R(N)=1.D0/TR%R(N)
          DUDX%R(N) =DUDX%R(N) *TR%R(N)
          DUDY%R(N) =DUDY%R(N) *TR%R(N)
          DUDZ%R(N) =DUDZ%R(N) *TR%R(N)
          DVDX%R(N) =DVDX%R(N) *TR%R(N)
          DVDY%R(N) =DVDY%R(N) *TR%R(N)
          DVDZ%R(N) =DVDZ%R(N) *TR%R(N)
          DWDX%R(N) =DWDX%R(N) *TR%R(N)
          DWDY%R(N) =DWDY%R(N) *TR%R(N)
          DWDZ%R(N) =DWDZ%R(N) *TR%R(N)
          DTADZ%R(N)=DTADZ%R(N)*TR%R(N)
        ELSE
          DUDX%R(N) =0.D0
          DUDY%R(N) =0.D0
          DUDZ%R(N) =0.D0
          DVDX%R(N) =0.D0
          DVDY%R(N) =0.D0
          DVDZ%R(N) =0.D0
          DWDX%R(N) =0.D0
          DWDY%R(N) =0.D0
          DWDZ%R(N) =0.D0
          DTADZ%R(N)=0.D0
        ENDIF
      ENDDO
!
!
!***********************************************************************
!                                                                      *
!     SECOND MEMBERS                                                   *
!     IMPLICIT SOURCE TERMS FOR K AND EPSILON                          *
!                                                                      *
!     IMPLICIT TERM ON K :       + EP(N)/K(N) * K (N+1) +G/K(N)*K(N+1) *
!                                          IF G IS POSITIVE            *
!     IMPLICIT TERM ON K :       + EP(N)/K(N) * K (N+1)   ELSE         *
!     IMPLICIT TERM ON EPSILON:         + C2 * EP(N)/AK(N) * EP(N+1)   *
!                                                                      *
!***********************************************************************
!
!
!-----------------------------------------------------------------------
!                  LIMITING K WITH PHYSICAL CRITERIA
!-----------------------------------------------------------------------
!
      IF(CLIPK) THEN
!       LIMITS K BY THE NORM OF THE HORIZONTAL VELOCITY 
        DO N=1,NPOIN3
          AK(N)=MIN(PERNORM2*(U%R(N)**2+V%R(N)**2+W%R(N)**2),AK(N))
          AK(N)=MAX(AK(N),KMIN)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!                             STRESS TENSOR 
!-----------------------------------------------------------------------
!
      DO N=1,NPOIN3
        S2%R(N) = 2.D0 * (DUDX%R(N)**2+DVDY%R(N)**2+DWDZ%R(N)**2)
     &            + ( DUDY%R(N)+DVDX%R(N) )**2
     &            + ( DUDZ%R(N)+DWDX%R(N) )**2
     &            + ( DVDZ%R(N)+DWDY%R(N) )**2  
      ENDDO
!
!-----------------------------------------------------------------------
!          RICHARDSON NUMBER (RESTRICTED TO VERTICAL GRADIENTS)
!-----------------------------------------------------------------------
!
!     SO FAR ONLY FOR POST-PROCESSING...
!
      IF(SORG3D(10)) THEN
        DO N=1,NPOIN3
          RI(N)=-GRAV*DTADZ%R(N)/MAX(DUDZ%R(N)**2+DVDZ%R(N)**2,1.D-10)
!         CLIPPING BETWEEN RIMIN AND RIMAX
          RI(N)=MAX(MIN(RI(N),RIMAX),RIMIN)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!                     TURBULENCE PRODUCTION
!-----------------------------------------------------------------------
!
      IF(OPTPROD.EQ.1) THEN
        DO N=1,NPOIN3
          S1=SQRT(S2%R(N))
          TAU=AK(N)/EP(N)
          IF(S1*TAU.GT.3.33D0) THEN
            A12LOG=SQRCMU
          ELSE
            A12LOG=SQRCMU*S1*TAU/3.33D0
          ENDIF
          PROD%R(N)=A12LOG*S1*AK(N)
        ENDDO
      ELSEIF(OPTPROD.EQ.2) THEN
        DO N=1,NPOIN3
          VISC = CMU * AK(N)**2 / EP(N)
!         QUADRATIC PRODUCTION
          PROD%R(N) = S2%R(N) * VISC
        ENDDO
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SOUKEP : VALEUR DE OPTPROD : ',OPTPROD
          WRITE(LU,*) '         NON PROGRAMMEE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SOUKEP : VALEUR DE OPTPROD : ',OPTPROD
          WRITE(LU,*) '         NON PROGRAMMEE'
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!              LIMITING EPSILON WITH PHYSICAL CRITERIA
!-----------------------------------------------------------------------
!
!     JMH 24/12/2009 (XMAS!): SOMETIMES AK IS "NORMAL" AND EP IS
!     EMIN, HENCE VERY HIGH VALUES OF VISC. EP SHOULD NEVER BE FAR
!     FROM PROD, HENCE THE LIMITATION (EP AT LEAST
!     THE TENTH OF PRODUCTION ACCORDING TO DOMINIQUE LAURENCE)
!     TO HAVE REALISTIC VALUES OF VISC FOR COMPUTING G
!
      IF(CLIPE) THEN
!       LIMITS EPSILON BY A PERCENTAGE OF THE PRODUCTION
        DO N=1,NPOIN3
          EP(N) = MAX(EP(N),PERPROD*PROD%R(N))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!                          WIND STRESS IN K
!-----------------------------------------------------------------------
!
      IF(VENT.AND.WSIK) THEN
        DO N2D=1,NPOIN2
          N=NPOIN3-NPOIN2+N2D
          VITV=SQRT(WIND%ADR(1)%P%R(N2D)**2+WIND%ADR(2)%P%R(N2D)**2)
!         SEE BORD3D, USER MAY HAVE CHANGED THE FORMULATION...
!         A SOLUTION WOULD BE TO STORE FAIR IN AN ARRAY
          IF(VITV.LE.5.D0) THEN
            FAIR = ROAIR/RHO0*0.565D-3
          ELSEIF(VITV.LE.19.22D0) THEN
            FAIR = ROAIR/RHO0*(-0.12D0+0.137D0*VITV)*1.D-3
          ELSE
            FAIR = ROAIR/RHO0*2.513D-3
          ENDIF
!         USTAR**2 DUE TO WIND STRESS (=FAIR*VITV**2) PUT IN FORMULA
!         AND FORMULA K=USTAR**2/SQRT(CMU) USED (EQUILIBRIUM NEAR A WALL)
!         HERE QUADRATIC PRODUCTION HARDCODED
          PROD%R(N)=S2%R(N)*CMU*MAX(FAIR*VITV**2/SQRCMU,KMIN)**2/EP(N)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!                     RIGHT-HAND SIDES OF EQUATIONS
!-----------------------------------------------------------------------
!
      IF(YAP) THEN
        DO N=1,NPOIN3
          CV1(N) = PROD%R(N)
!         THEORY NEEDED HERE TO LIMIT EP/AK
          UNSURTAU = MIN(EP(N)/AK(N),100.D0)
          S1K(N) = UNSURTAU
          LL=CMU**0.75D0*SQRT(AK(N)**3)/EP(N)
          YY=MESH3D%Z%R(N)-MESH3D%Z%R(MOD(N-1,NPOIN2)+1)
          IF(LL.GT.0.41D0*YY) THEN
            YY=MAX(YY,1.D-4)
            YP=0.83D0*(LL/0.41D0/YY-1.D0)*(LL/0.41D0/YY)**2
            YP=MIN(YP,C2)
          ELSE
            YP=0.D0
          ENDIF
          S1E(N) = (C2-YP)*S1K(N)
          VISC = CMU * AK(N)**2 / EP(N)
          G=VISC*GRAV*DTADZ%R(N)*SURPRAN
          IF(G.GT.0.D0) THEN
!           TREATS G EXPLICITLY
            CV1(N) = CV1(N) + G
            CV2(N) = C1*(PROD%R(N)+G)*UNSURTAU
          ELSE
!           TREATS G IMPLICITLY
            S1K(N) = S1K(N) - G/AK(N)
            CV2(N) = C1*PROD%R(N)*UNSURTAU
          ENDIF
        ENDDO
      ELSE
        DO N=1,NPOIN3
          CV1(N) = PROD%R(N)
!         THEORY NEEDED HERE TO LIMIT EP/AK
          UNSURTAU=MIN(EP(N)/AK(N),100.D0)
          S1K(N) = UNSURTAU
!         -EP TERM OF THE SECOND MEMBER OF THE EQUATION ON K
!         IN IMPLICIT FORM
          S1E(N) = C2*S1K(N)
          VISC = CMU * AK(N)**2 / EP(N)
          G=VISC*GRAV*DTADZ%R(N)*SURPRAN
          IF(G.GT.0.D0) THEN
!           TREATS G EXPLICITLY
            CV1(N) = CV1(N) + G
            CV2(N) = C1*(PROD%R(N)+G)*UNSURTAU
          ELSE
!           TREATS G IMPLICITLY
            S1K(N) = S1K(N) - G/AK(N)
            CV2(N) = C1*PROD%R(N)*UNSURTAU
          ENDIF
        ENDDO
      ENDIF
!
!     FOR FURTHER INVESTIGATIONS...
!
!     CALL MAXI(G,N,AK,NPOIN3)
!     PRINT*,'K MAXI=',G,' EN ',N
!     CALL MINI(G,N,AK,NPOIN3)
!     PRINT*,'K MINI=',G,' EN ',N
!     CALL MAXI(G,N,EP,NPOIN3)
!     PRINT*,'E MAXI=',G,' EN ',N
!     CALL MINI(G,N,EP,NPOIN3)
!     PRINT*,'E MINI=',G,' EN ',N
!     CALL MAXI(G,N,CV1,NPOIN3)
!     PRINT*,'CV1 MAXI=',G,' EN ',N
!     CALL MAXI(G,N,CV2,NPOIN3)
!     PRINT*,'CV2 MAXI=',G,' EN ',N
!     G=0.D0
!     I=1
!     DO N=1,NPOIN3
!       IF(G.LT.CMU*AK(N)**2/EP(N)) THEN
!        G=CMU*AK(N)**2/EP(N)
!        I=N
!       ENDIF
!     ENDDO
!     PRINT*,'MAX VISCOSITY=',G,' AT POINT ',I,' AK=',AK(I),' EP=',EP(I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
