C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE SOURCES TERMS IN THE DIFFUSION EQUATION OF
!>                K AND OMEGA.

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
!> </td><td> 24/08/07
!> </td><td> J-M HERVOUET (LNHE)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/02/01
!> </td><td> HOLGER WEILBEER   ISEB/UHA
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE SOUKOM
     &(CV1,CV2,S1K,S1E,U,V,W,DELTAR,
     & DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ,DTADZ,
     & DKDX,DKDY,DKDZ,DODX,DODY,DODZ,ROTAT,
     & AK,EP,ALPHA,BETA,BETAS,GRAV,TR,NPOIN3,MSK,MASKEL,MESH3D,IELM3,S,
     & PRANDTL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK,EP          |-->| K ET OMEGA
C| ALPHA          |---| 
C| ALPHA ETC      |-->| CONSTANTE DU MODELE K-OMEGA
C| BETA           |---| 
C| BETAC          |-->| CSTES DE DILATATION VOL DES TRACEURS ACTIFS
C| BETAS          |---| 
C| CV1,CV2        |<--| TERMES SOURCE POUR K ET EPSILON
C| DELTAR         |-->| DELTA(RO)/RO
C| DKDX           |---| 
C| DKDY           |---| 
C| DKDZ           |---| 
C| DODX           |---| 
C| DODY           |---| 
C| DODZ           |---| 
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
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| IELM3          |---| 
C| IKLE3          |-->| CORRESPONDANCE ENTRE LA NUMEROTATION LOCALE
C|                |   | ET GLOBALE 3D
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH3D         |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 3D
C| NETAGE         |-->| NOMBRE D'ETAGES SUR LA VERTICALE
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| PRANDTL        |---| 
C| ROTAT          |---| 
C| S             |---| 
C| S1E            |---| 
C| S1K            |---| 
C| SURFAC         |-->| SURFACE DES ELEMENTS 2D
C| TR             |-->| TABLEAU DE TRAVAIL PAR POINTS
C| U,V,W          |-->| COMPOSANTES DE LA VITESSE
C| W1             |-->| TABLEAU DE TRAVAIL PAR ELEMENTS 3D
C| WTAZ           |-->| DERIVEE PAR ELEMENT DU TRACEUR ACTIF N0 1
C| WUX,           |-->| DERIVEES PAR ELEMENT DE LA VITESSE
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE 3D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN3,IELM3
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CV1,CV2,S1K,S1E
      TYPE(BIEF_OBJ), INTENT(IN)    :: AK,EP
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: ROTAT
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DUDX,DUDY,DUDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DVDX,DVDY,DVDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DWDX,DWDY,DWDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DKDX,DKDY,DKDZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DODX,DODY,DODZ
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: DTADZ,TR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,U,V,W,DELTAR,S
!
      DOUBLE PRECISION, INTENT(IN)    :: GRAV,ALPHA,BETA,BETAS,PRANDTL
!
      LOGICAL, INTENT(IN)             :: MSK
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
!
      INTRINSIC ABS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION VISC,PROD,C3,G,GRDKOM,SIGMAD
      INTEGER N
C
C-----------------------------------------------------------------------
C
      CALL VECTOR(DKDX,'=','GRADF          X',IELM3,1.D0,AK,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DKDY,'=','GRADF          Y',IELM3,1.D0,AK,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DKDZ,'=','GRADF          Z',IELM3,1.D0,AK,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DODX,'=','GRADF          X',IELM3,1.D0,EP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DODY,'=','GRADF          Y',IELM3,1.D0,EP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
      CALL VECTOR(DODZ,'=','GRADF          Z',IELM3,1.D0,EP,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
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
!
      CALL VECTOR(DTADZ,'=','GRADF          Z',IELM3,1.D0,DELTAR,
     &            S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      CALL VECTOR(TR,'=','MASBAS          ',IELM3,1.D0,
     &            S,S,S,S,S,S,MESH3D,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DKDX ,2,MESH3D)
        CALL PARCOM(DKDY ,2,MESH3D)
        CALL PARCOM(DKDZ ,2,MESH3D)
        CALL PARCOM(DODX ,2,MESH3D)
        CALL PARCOM(DODY ,2,MESH3D)
        CALL PARCOM(DODZ ,2,MESH3D)
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
        CALL PARCOM(TR,2,MESH3D)
      ENDIF
!
      CALL OS('X=1/Y   ',TR,TR,TR,0.D0,2,1.D0,1.D-10)
      CALL OS('X=XY    ',X=DKDX,Y=TR)
      CALL OS('X=XY    ',X=DKDY,Y=TR)
      CALL OS('X=XY    ',X=DKDZ,Y=TR)
      CALL OS('X=XY    ',X=DODX,Y=TR)
      CALL OS('X=XY    ',X=DODY,Y=TR)
      CALL OS('X=XY    ',X=DODZ,Y=TR)
      CALL OS('X=XY    ',X=DUDX,Y=TR)
      CALL OS('X=XY    ',X=DUDY,Y=TR)
      CALL OS('X=XY    ',X=DUDZ,Y=TR)
      CALL OS('X=XY    ',X=DVDX,Y=TR)
      CALL OS('X=XY    ',X=DVDY,Y=TR)
      CALL OS('X=XY    ',X=DVDZ,Y=TR)
      CALL OS('X=XY    ',X=DWDX,Y=TR)
      CALL OS('X=XY    ',X=DWDY,Y=TR)
      CALL OS('X=XY    ',X=DWDZ,Y=TR)
      CALL OS('X=XY    ',X=DTADZ,Y=TR)
C
C-----------------------------------------------------------------------
C
      DO N=1,NPOIN3
C
         ROTAT%R(N) = ABS(DWDY%R(N)-DVDZ%R(N))
     &              + ABS(DUDZ%R(N)-DWDX%R(N))
     &              + ABS(DVDX%R(N)-DUDY%R(N))
         GRDKOM   = DKDX%R(N)*DODX%R(N)
     &             +DKDY%R(N)*DODY%R(N)+DKDZ%R(N)*DODZ%R(N)
C
         IF(GRDKOM.GE.0.D0) THEN
           SIGMAD = 0.5D0
         ELSE
           SIGMAD   = 0.D0
         ENDIF
C
         VISC = 0.3D0*AK%R(N)/MAX(0.3D0*EP%R(N),ROTAT%R(N))
C
         PROD = (  2.D0 * (DUDX%R(N)**2+DVDY%R(N)**2+DWDZ%R(N)**2)
     &                  + ( DUDY%R(N)+DVDX%R(N) )**2
     &                  + ( DUDZ%R(N)+DWDX%R(N) )**2
     &                  + ( DVDZ%R(N)+DWDY%R(N) )**2  ) * VISC
C
C        COMPUTES G AND C3
C
         G = VISC*GRAV*DTADZ%R(N)
C
         IF(G.GT.0.D0) THEN
           C3=0.D0
         ELSE
           C3=1.D0
         ENDIF
C
C        EXPLICIT K
C
         CV1%R(N) = PROD + G
C
C        EXPLICIT OMEGA
C
         CV2%R(N)=ALPHA*(PROD+(1-C3)*G)*EP%R(N)/AK%R(N)
     &            +SIGMAD/EP%R(N)*GRDKOM
C
      ENDDO
C
C***********************************************************************
C                                                                      *
C     IMPLICIT SOURCE TERMS FOR K AND OMEGA                            *
C                                                                      *
C     IMPLICIT TERM ON K :              + BETAS* EP(N) * K (N+1)       *
C     IMPLICIT TERM ON OMEGA:           + BETA * EP(N) * EP(N+1)       *
C                                                                      *
C***********************************************************************
C
      CALL OS( 'X=CY    ' , X=S1K , Y=EP  , C=BETAS )
      CALL OS( 'X=CY    ' , X=S1E , Y=EP  , C=BETA  )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
