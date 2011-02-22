C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE SOURCE TERMS IN THE DIFFUSION EQUATION OF
!>                K AND EPSILON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 24/12/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/02/2001
!> </td><td> A. GARAPON  & V. BOYER LNHE
!> </td><td> REVISITED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE SOUKEP
     &(CV1,CV2,S1K,S1E,U,V,W,DELTAR,RI,
     & DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ,DTADZ,
     & AK,EP,C1,C2,CMU,GRAV,TR,NPOIN3,MSK,MASKEL,MESH3D,IELM3,S,DT,
     & VENT,WIND,H,EBORS,NPOIN2,KMIN,EMIN,PRANDTL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK,EP          |-->| K ET EPSILON
C| C1             |-->| CONSTANTE DU MODELE K-EPSILON
C| C2             |-->| CONSTANTE DU MODELE K-EPSILON
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| CV1,CV2        |<--| TERMES SOURCE POUR K ET EPSILON
C| DELTAR         |-->| DELTA(RO)/RO
C| DT             |---| 
C| DTADZ          |-->| DERIVEE EN Z DU TRACEUR N0 1
C| DUDX,          |-->| DERIVEES DES COMPOSANTES DE LA VITESSE
C| DUDY           |---| 
C| DUDZ           |---| 
C| DVDX           |---| 
C| DVDY           |---| 
C| DVDZ           |---| 
C| DWDX           |---| 
C| DWDY           |---| 
C| DWDZ           |---| 
C| EBORS          |---| 
C| EMIN           |---| 
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| H             |---| 
C| IELM3          |---| 
C| KMIN           |---| 
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH3D         |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NPOIN2         |---| 
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| PRANDTL        |---| 
C| RI             |---| 
C| S             |---| 
C| S1E            |---| 
C| S1K            |---| 
C| TR             |-->| TABLEAU DE TRAVAIL PAR POINTS
C| U,V,W          |-->| COMPOSANTES DE LA VITESSE
C| VENT           |---| 
C| W1             |-->| TABLEAU DE TRAVAIL PAR ELEMENTS 3D
C| WIND           |---| 
C| WTAZ           |-->| DERIVEE PAR ELEMENT DU TRACEUR ACTIF N0 1
C| WUX,           |-->| DERIVEES PAR ELEMENT DE LA VITESSE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC3D, ONLY : IPBOT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN3,IELM3,NPOIN2
!
      DOUBLE PRECISION, INTENT(INOUT) :: CV1(NPOIN3),CV2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: S1K(NPOIN3),S1E(NPOIN3)
C     AK AND EP COULD BE CHANGED IF SOURCE TERMS WERE TREATED HERE
      DOUBLE PRECISION, INTENT(INOUT) :: AK(NPOIN3),EP(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: RI(NPOIN3)
!
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: DUDX,DUDY,DUDZ,DVDX,DVDY,EBORS
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: DVDZ,DWDX,DWDY,DWDZ,DTADZ,TR
      TYPE (BIEF_OBJ), INTENT(IN)     :: MASKEL,U,V,W,DELTAR,S,WIND,H
!
      DOUBLE PRECISION, INTENT(IN)    :: C1,C2,CMU,GRAV,DT,KMIN,EMIN
      DOUBLE PRECISION, INTENT(IN)    :: PRANDTL
!
      LOGICAL, INTENT(IN)             :: MSK,VENT
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION VISC,PROD,G,S2,S1,SDELTAZ,DTADZU,DTADZD,VITV,FAIR
      DOUBLE PRECISION TAU,C1RNG,A12LOG,ETA,LL,YY,YP,PRODQ,ROEAU,ROAIR
      DOUBLE PRECISION UNORM2,SURPRAN,UNSURTAU
      INTEGER N,OPTKEPS,N2D,I
!
!-----------------------------------------------------------------------
!
      SURPRAN=1.D0/PRANDTL
!
!-----------------------------------------------------------------------
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
!***********************************************************************
!                                                                      *
C     SECOND MEMBERS                                                   *
C     IMPLICIT SOURCE TERMS FOR K AND EPSILON                          *
!                                                                      *
C     IMPLICIT TERM ON K :       + EP(N)/K(N) * K (N+1) +G/K(N)*K(N+1) *
C                                          IF G IS POSITIVE            *
C     IMPLICIT TERM ON K :       + EP(N)/K(N) * K (N+1)   ELSE         *
C     IMPLICIT TERM ON EPSILON:         + C2 * EP(N)/AK(N) * EP(N+1)   *
!                                                                      *
!***********************************************************************
!
C     OPTKEPS=1 : STANDARD MODEL (BEWARE: NO WIND)
C     OPTKEPS=2 : LINEAR PRODUCTION + YAP + RNG
!
      OPTKEPS=2
!
      IF(OPTKEPS.EQ.1) THEN
!
      DO N=1,NPOIN3
!
        VISC = CMU * AK(N)**2 / EP(N)
!
        S2 = (  2.D0 * (DUDX%R(N)**2+DVDY%R(N)**2+DWDZ%R(N)**2)
     &                  + ( DUDY%R(N)+DVDX%R(N) )**2
     &                  + ( DUDZ%R(N)+DWDX%R(N) )**2
     &                  + ( DVDZ%R(N)+DWDY%R(N) )**2  )
!
C       RICHARDSON NUMBER (BETWEEN 0. AND 100.)
        RI(N)=-GRAV*DTADZ%R(N)/MAX(S2,1.D-10)
        RI(N)=MAX(MIN(RI(N),100.D0),0.D0)
C       QUADRATIC PRODUCTION
        PROD = S2 * VISC
!
        CV1(N) = PROD
C       -EP TERM OF THE SECOND MEMBER OF THE EQUATION ON K
C       IN IMPLICIT FORM
!
C       THEORY NEEDED HERE TO LIMIT EP/AK
        UNSURTAU=MIN(EP(N)/AK(N),100.D0)
        S1K(N) = UNSURTAU
        S1E(N) = C2*S1K(N)
!
C       QUADRATIC PRODUCTION
        G=VISC*GRAV*DTADZ%R(N)*SURPRAN
!
        IF(G.GT.0.D0) THEN
C         TREATS G EXPLICITLY
          CV1(N) = CV1(N) + G
          CV2(N) = C1*(PROD+G)*UNSURTAU
        ELSE
C         TREATS G IMPLICITLY
          S1K(N) = S1K(N) - G/AK(N)
          CV2(N) = C1*PROD*UNSURTAU
        ENDIF
!
      ENDDO
!
      ELSEIF(OPTKEPS.EQ.2) THEN
!
      DO N=1,NPOIN3
!
C       LIMITS K BY THE NORM OF THE HORIZONTAL VELOCITY (NOT SURE W EXISTS)
        UNORM2=U%R(N)**2+V%R(N)**2+W%R(N)**2
        IF(AK(N).GT.UNORM2) AK(N)=MAX(0.5D0*UNORM2,KMIN)
!
        S2 = (  2.D0 * (DUDX%R(N)**2+DVDY%R(N)**2+DWDZ%R(N)**2)
     &                  + ( DUDY%R(N)+DVDX%R(N) )**2
     &                  + ( DUDZ%R(N)+DWDX%R(N) )**2
     &                  + ( DVDZ%R(N)+DWDY%R(N) )**2  )
!
C       RICHARDSON NUMBER (BETWEEN 0. AND 100.)
C       NOT ALWAYS USEFUL ?????
!
        RI(N)=-GRAV*DTADZ%R(N)/MAX(S2,1.D-10)
        RI(N)=MAX(MIN(RI(N),100.D0),0.D0)
!
        S1=SQRT(S2)
        TAU=AK(N)/EP(N)
        IF(S1*TAU.GT.3.33D0) THEN
          A12LOG=0.3D0
        ELSE
          A12LOG=0.3D0*S1*TAU/3.33D0
        ENDIF
!
        IF(VENT.AND.N.GT.NPOIN3-NPOIN2) THEN
C         FREE SURFACE WITH MINIMUM OF K DUE TO THE WIND
          ROEAU = 1000.D0
          ROAIR = 1.3D0
          N2D=N-NPOIN3+NPOIN2
          VITV=SQRT(WIND%ADR(1)%P%R(N2D)**2+
     &              WIND%ADR(2)%P%R(N2D)**2)
C         SEE BORD3D, USER MAY HAVE CHANGED THE FORMULATION...
C         A SOLUTION WOULD BE TO STORE FAIR IN AN ARRAY
          IF(VITV.LE.5.D0) THEN
            FAIR = ROAIR/ROEAU*0.565D-3
          ELSEIF(VITV.LE.19.22D0) THEN
            FAIR = ROAIR/ROEAU*(-0.12D0+0.137D0*VITV)*1.D-3
          ELSE
            FAIR = ROAIR/ROEAU*2.513D-3
          ENDIF
C         USTAR**2 DUE TO WIND PUT IN VITV
          VITV=VITV**2*FAIR
C         LINEAR PRODUCTION
C         PROD=A12LOG*S1*MAX(AK(N),VITV/0.3D0)
          PROD=A12LOG*S1*MAX(VITV/0.3D0,KMIN)
C         QUADRATIC PRODUCTION
          PRODQ=S2*CMU*MAX(VITV/0.3D0,KMIN)**2/EP(N)
        ELSE
C         LINEAR PRODUCTION
          PROD=A12LOG*S1*AK(N)
C         QUADRATIC PRODUCTION
C         PRODQ=S2*CMU*AK(N)**2/EP(N)
        ENDIF
!
C       LINEAR PRODUCTION FOR K
        CV1(N) = PROD
C       QUADRATIC PRODUCTION FOR K
C       CV1(N) = PRODQ
!
C       -EP TERM OF THE SECOND MEMBER OF THE EQUATION ON K
C       IN IMPLICIT FORM
!
C       THEORY NEEDED HERE TO LIMIT EP/AK ??
        UNSURTAU=EP(N)/AK(N)
        S1K(N) = UNSURTAU
!
C       WITH CORRECTION OF YAP
        LL=CMU**0.75D0*SQRT(AK(N)**3)/EP(N)
        YY=MESH3D%Z%R(N)-MESH3D%Z%R(MOD(N-1,NPOIN2)+1)
        IF(LL.GT.0.41D0*YY) THEN
          YY=MAX(YY,1.D-4)
          YP=0.83D0*(LL/0.41D0/YY-1.D0)*(LL/0.41D0/YY)**2
          YP=MIN(YP,C2)
        ELSE
          YP=0.D0
        ENDIF
        S1E(N) = (C2-YP)*UNSURTAU
!
C       WITHOUT CORRECTION OF YAP
C       S1E(N) = C2*UNSURTAU
!
C       PRODUCTION DUE TO BUOYANCY EFFECTS
!
C       JMH 24/12/2009 (XMAS!): SOMETIMES AK IS "NORMAL" AND EP IS
C       EMIN, HENCE VERY HIGH VALUES OF VISC. EP SHOULD NEVER BE FAR
C       FROM PROD, HENCE THE LIMITATION OF 1/EP BELOW (EP AT LEAST
C       THE TENTH OF PRODUCTION ACCORDING TO DOMINIQUE LAURENCE)
C       TO HAVE REALISTIC VALUES OF VISC FOR COMPUTING G
!
C       VISC = CMU * AK(N)**2 /     EP(N)
        VISC = CMU * AK(N)**2 / MAX(EP(N),0.1D0*PROD)
        G=VISC*GRAV*DTADZ%R(N)*SURPRAN
!
C       RNG MODEL : HEM... GIVES BIG DIFFERENCES (??!!??)
C       JMH MODIF : ETA LIMITED TO 8.51, MAXIMUM VALUE OF CC1
C       ETA=MIN(S1*TAU,8.51D0)
C       IF(ETA.GT.3.33D0) THEN
C         C1RNG=C1+(ETA/3.33D0-1.D0)*ETA/(1.D0+0.012D0*ETA**3)
C       ELSE
C         C1RNG=C1
C       ENDIF
!
C       HERE NO RNG
        C1RNG=C1
!
        IF(G.GT.0.D0) THEN
C         TREATS G EXPLICITLY
          CV1(N) = CV1(N) + G
C         LINEAR PRODUCTION FOR EPSILON
          CV2(N) = C1RNG*(PROD+G)*UNSURTAU
C         QUADRATIC PRODUCTION FOR EPSILON
C         CV2(N) = C1RNG*(PRODQ+G)*UNSURTAU
        ELSE
C         TREATS G IMPLICITLY
          S1K(N) = S1K(N) - G/AK(N)
C         LINEAR PRODUCTION FOR EPSILON
          CV2(N) = C1RNG*PROD*UNSURTAU
C         QUADRATIC PRODUCTION FOR EPSILON
C         CV2(N) = C1RNG*PRODQ*UNSURTAU
        ENDIF
!
      ENDDO
!
      ENDIF
!
C     FOR FURTHER INVESTIGATIONS...
!
C     CALL MAXI(G,N,AK,NPOIN3)
C     PRINT*,'K MAXI=',G,' EN ',N
C     CALL MINI(G,N,AK,NPOIN3)
C     PRINT*,'K MINI=',G,' EN ',N
C     CALL MAXI(G,N,EP,NPOIN3)
C     PRINT*,'E MAXI=',G,' EN ',N
C     CALL MINI(G,N,EP,NPOIN3)
C     PRINT*,'E MINI=',G,' EN ',N
C     CALL MAXI(G,N,CV1,NPOIN3)
C     PRINT*,'CV1 MAXI=',G,' EN ',N
C     CALL MAXI(G,N,CV2,NPOIN3)
C     PRINT*,'CV2 MAXI=',G,' EN ',N
C     G=0.D0
C     I=1
C     DO N=1,NPOIN3
C       IF(G.LT.CMU*AK(N)**2/EP(N)) THEN
C        G=CMU*AK(N)**2/EP(N)
C        I=N
C       ENDIF
C     ENDDO
C     PRINT*,'MAX VISCOSITY=',G,' AT POINT ',I,' AK=',AK(I),' EP=',EP(I)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
