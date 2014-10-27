!                    ***************** 
                     SUBROUTINE CONDIN 
!                    ***************** 
! 
! 
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010 
!***********************************************************************
! 
!brief    INITIALISES THE PHYSICAL PARAMETERS H, U, V ETC. 
! 
!history  J-M HERVOUET (LNHE) 
!+        30/08/2007 
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
!history  M.S.TURNBULL (HRW), N.DURAND (HRW), S.E.BOURBAN (HRW) 
!+        06/12/2011 
!+        V6P2 
!+   Addition of the Tsunami displacement (based on Okada's model) 
!+   by calling CONDI_OKADA and of the TPXO tidal model by calling 
!+   CONDI_TPXO (the TPXO model being coded in module TPXO) 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE BIEF 
      USE DECLARATIONS_TELEMAC 
      USE DECLARATIONS_TELEMAC2D 
      USE TPXO 
      USE OKADA 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER ITRAC 
      INTEGER I 
! 
!-----------------------------------------------------------------------
! 
!   INITIALISES THE TIME 
! 
      AT = 0.D0 
! 
!-----------------------------------------------------------------------
! 
!   INITIALISES THE VELOCITIES: ZERO VELOCITIES 
! 
      CALL OS('X=0     ',X=U) 
      CALL OS('X=0     ',X=V) 
! 
!-----------------------------------------------------------------------
! 
!   INITIALISES THE WATER DEPTH H 
! 
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR. 
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN 
        CALL OS( 'X=0     ' , X=H ) 
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF ) 
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR. 
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN 
        CALL OS( 'X=C     ' , H , H  , H , COTINI ) 
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   ) 
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR. 
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN 
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  ) 
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR. 
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN 
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN ) 
      ELSEIF(CDTINI(1:25).EQ.'ALTIMETRIE SATELLITE TPXO'.OR. 
     &       CDTINI(1:24).EQ.'TPXO SATELLITE ALTIMETRY') THEN 
        CALL OS('X=-Y    ',X=H,Y=ZF) 
        CALL CONDI_TPXO(NPOIN,MESH%NPTFR,MESH%NBOR%I, 
     &                  X,Y,H%R,U%R,V%R, 
     &                  LIHBOR%I,LIUBOR%I,KENT,KENTU, 
     &                  GEOSYST,NUMZONE,LAMBD0,PHI0, 
     &                  T2D_FILES,T2DBB1,T2DBB2, 
     &                  MARDAT,MARTIM,INTMICON,MSL) 
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR. 
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR. 
     &       CDTINI(1:07).EQ.'SPECIAL') THEN 
! 
!  TO BE MODIFIED BY USER IF SPECIAL INITIAL CONDITIONS 
!                    
! KD09 dam-break IC:                                              
      DO I=1,NPOIN
        IF(X(I)>5.0D0) THEN                                            
          H%R(I) = 0.D0
          U%R(I) = 0.D0
        ELSE
          H%R(I) = 1.D0
          U%R(I) = 0.D0
        ENDIF
      END DO     
! 
!  END OF CODE TO BE MODIFIED BY USER 
! 
      ELSE 
        IF(LNG.EQ.1) THEN 
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI 
        ENDIF 
        IF(LNG.EQ.2) THEN 
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI 
        ENDIF 
        CALL PLANTE(1) 
        STOP 
      ENDIF 
! 
!-----------------------------------------------------------------------
! 
!   INITIALISES TSUNAMI DISPLACEMENT 
! 
      IF(OPTTSUNAMI.EQ.1) THEN 
        CALL CONDI_OKADA(NPOIN,X,Y,H%R,COETSUNAMI,LAMBD0,PHI0) 
      ENDIF 
! 
!-----------------------------------------------------------------------
! 
!   INITIALISES THE TRACERS 
! 
      IF(NTRAC.GT.0) THEN 
        DO ITRAC=1,NTRAC 
          CALL OS('X=C     ',X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC)) 
        ENDDO 
      ENDIF 
! 
!-----------------------------------------------------------------------
! 
! INITIALISES THE VISCOSITY 
! 
      CALL OS('X=C     ',X=VISC,C=PROPNU) 
! 
!-----------------------------------------------------------------------
! 
      RETURN 
      END 
 
! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 
!                    ***************** 
                     SUBROUTINE QSFORM 
!                    ***************** 
! 
     &(U2D, V2D, TOB, HN, XMVE, TETAP, MU, NPOIN, DM,  
     & DENS, GRAV, DSTAR, AC, QSC, QSS) 
! 
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011 
!***********************************************************************
! 
!brief    ALLOWS THE USER TO CODE THEIR OWN BEDLOAD TRANSPORT 
!+                FORMULATION, BEST SUITED TO THEIR APPLICATION. 
! 
!warning  USER SUBROUTINE; SAND TRANSPORT FORMULA MUST BE CODED BY THE USER
! 
!history  F. HUVELIN 
!+        **/11/2003 
!+        V5P4 
!+   MODIFIED 
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
!history  P. Tassi 
!+        22/05/2012 
!+        V6P2 
!+   Arguments added 
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
      USE INTERFACE_SISYPHE, EX_QSFORM => QSFORM 
!     USE DECLARATIONS_SISYPHE 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D,V2D,TOB,HN,TETAP,MU 
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS 
      INTEGER,          INTENT(IN)    :: NPOIN 
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, DM, DENS, GRAV, DSTAR, AC
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER          :: I 
      DOUBLE PRECISION :: C1, C2, T 
      DOUBLE PRECISION, PARAMETER :: ACOEFF = 0.004D0 ! Sediment transport param (m^2s^-1)
! 
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
! 
!     GRASS (1981) TYPE 
!      
      DO I = 1, NPOIN 
 
        QSC%R(I) = ACOEFF * U2D%R(I) * (U2D%R(I)**2+V2D%R(I)**2)   ! Grass (1981) type bedload (total load) 
        QSS%R(I) = 0.D0                                            ! Zero suspended load
 
      END DO 
! 
! 
!-----------------------------------------------------------------------
! 
      RETURN 
      END 
