!
!
!
!
!
!
!
!      include '../../_nestor88.f'
!      include '../../_nestor89_syncMod.f'
!
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!                    *********************
                     SUBROUTINE INIT_COMPO
!                    *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE:
!+      NCOUCHES(J) = 10
!+      ES(J,1) = 1.D0
!+      ES(J,2) = 1.D0
!+      ES(J,3) = 1.D0
!+      ES(J,4) = 1.D0
!+      ES(J,5) = 1.D0
!+      ES(J,6) = 1.D0
!+      ES(J,7) = 1.D0
!+      ES(J,8) = 1.D0
!+      ES(J,9) = 1.D0
!+        DO I = 1, NSICLA
!+          DO K = 1, NCOUCHES(J)
!+          AVAIL(J,K,I) = AVA0(I)
!+          ENDDO
!+        ENDDO
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
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
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        2016
!+        V7P2
!+   Checking coherence of data: ZR+sediment height=ZF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
      DOUBLE PRECISION EPAI
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
      	
       NCOUCHES(J) = 3

       ES(J,1) = 0.1D0
       ES(J,2) = 0.1D0
       ES(J,3) = 9.8D0 
      
!       IF(  MESH%X%R(J) > 600.0_8 )  THEN   ! debug
!         AVA0(1) = 1.0_8                   ! debug
!         AVA0(2) = 0.0_8                   ! debug
!         AVA0(3) = 0.0_8                   ! debug
!       ELSE                                ! debug
!         AVA0(1) = 0.0_8                   ! debug
!         AVA0(2) = 0.0_8                   ! debug
!         AVA0(3) = 1.0_8                   ! debug
!       ENDIF                               ! debug

       DO I = 1, NSICLA
         DO K = 1, NCOUCHES(J)
           AVAIL(J,K,I) = AVA0(I)
         ENDDO
       ENDDO 
         
      ENDDO
      
!-----------------------------------------------------------------------
!
!     CHECKING THE CONSISTENCY OF DATA
!     THE FORMULA USED HERE ZR+SED. HEIGHT = ZF CAN BE USED TO GIVE THE
!     HEIGHT OF THE LAST LAYER.
!
      DO J=1,MESH%NPOIN
        EPAI=0.D0
        DO I=1,NCOUCHES(J)
          EPAI=EPAI+ES(J,I)
        ENDDO
        IF(ABS(ZR%R(J)+EPAI-ZF%R(J)).GT.1.D-6) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'INIT_COMPO, ERREUR :'
            WRITE(LU,*) 'ZR+EPAISSEUR=',ZR%R(J)+EPAI
            WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),' EPAISSEUR=',EPAI
            WRITE(LU,*) 'AU POINT ',J
          ELSE
            WRITE(LU,*) 'INIT_COMPO, ERROR:'
            WRITE(LU,*) 'ZR+SEDIMENT HEIGHT=',ZR%R(J)+EPAI
            WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),
     &                  ' SEDIMENT HEIGHT=',EPAI
            WRITE(LU,*) 'AT POINT ',J
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
      
      
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************

!                    *****************
                     SUBROUTINE NOEROD
!                    *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE   V6P3                                  21/07/2011
!***********************************************************************
!
!brief    FIXES THE NON-ERODABLE BED ELEVATION ZR.
!
!note     METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.
!note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.
!
!history  C. LENORMANT
!+
!+        V5P1
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
!+        21/06/2013
!+        V6P3
!+   Now ZR=ZF-100.D0 by default
!+   previous versions was erronneously ZR=-100.D0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!| H              |-->| WATER DEPTH
!| NLISS          |<->| NUMBER OF SMOOTHINGS
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| X,Y            |-->| 2D COORDINATES
!| Z              |-->| FREE SURFACE
!| ZF             |-->| BED LEVEL
!| ZR             |<--| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER I
!
!---------------------
! RIGID BEDS POSITION
!---------------------
!
!     DEFAULT VALUE: ZR=ZF-100.D0
!
cgl   CALL OV('X=Y+C   ',ZR,ZF,ZF,-100.D0,NPOIN)
      CALL OV('X=Y+C   ',ZR,ZF,ZF, -10.D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!--------------------------------------------------
! CONTROL (CAN BE ACTIVATED IF ZR USER DEFINED...)
!--------------------------------------------------
!
!     DO I=1,NPOIN
!       IF(ZR(I).GT.ZF(I)) THEN
!         WRITE(LU,*) 'POINT ',I,' NON ERODABLE BED HIGHER THAN BED'
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
      




!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!                    ************************
                     SUBROUTINE xINIT_SEDIMENT
!                    ************************
!
     &(NSICLA,ELAY,ZF,ZR,NPOIN,AVAIL,FRACSED_GF,AVA0,
     & LGRAFED,CALWC,XMVS,XMVE,GRAV,VCE,XWC,FDM,
     & CALAC,AC,SEDCO,ES,ES_SABLE, ES_VASE ,NOMBLAY,CONC_VASE,
     & MS_SABLE,MS_VASE,ACLADM,UNLADM,TOCE_SABLE,
     & CONC,NLAYER,DEBU,MIXTE)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief
!
!history  C. VILLARET (LNHE)
!+        30/12/2008
!+
!+
!
!history  JMH
!+        16/09/2009
!+        V6P0
!+   AVAIL(NPOIN,10,NSICLA)
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!history  P.TASSI (EDF-LNHE)
!+        30/05/2012
!+        V6P2
!+  Case DSTAR > 150 AC(I) = 0.045D0
!+
!
!history  P.TASSI (EDF-LNHE)
!+        06/07/2012
!+        V6P2
!+  Line MIXTE=.FALSE. added.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AT0            |<->| TIME IN S
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CALAC          |-->| IF YES, SHIELDS PARAMETER FOUND IN PARAMETER FILE
!| CALWC          |-->| IF YES, SETTLING VELOCITIES FOUND IN PARAMETER FILE
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| ES_SABLE       |<->| LAYER THICKNESSES OF SAND AS DOUBLE PRECISION
!| ES_VASE        |<->| LAYER THICKNESSES OF MUD AS DOUBLE PRECISION
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| FRACSED_GF     |-->|(A SUPPRIMER)
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| LGRAFED        |-->|(A SUPPRIMER)
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| ES_SABLE       |<->| THICKNESS OF SAND LAYER (M)
!| ES_VASE        |<->| THICKNESS OF MUD LAYER  (M)
!| MIXTE          |<->| SEDIMENT MIXTE  (SABLE /VASE)
!| NOMBLAY        |-->| NUMBER OF BED LAYERS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| WATER DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!| CONC           |<->| CONCENTRATION OF BED LAYER
!| NLAYER         |<->| NUMBER OF BED LAYER
!| DEBU           |-->| FLAG, RESTART ON SEDIMENTOLOGICAL FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_SEDIMENT => INIT_SEDIMENT
!     --> nestor      
      USE DECLARATIONS_SISYPHE, ONLY : T13,T14,IELMH_SIS,MESH,MSK
     &                                ,MASKEL,MARDAT,MARTIM,MOFAC
     &                                ,SIS_FILES,SISMAF
      USE DECLARATIONS_TELEMAC2D, ONLY : LEOPRD
!     --> nestor      

      IMPLICIT NONE
      
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)     :: NSICLA,NPOIN,NOMBLAY
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ELAY,ZF,ZR
      TYPE(BIEF_OBJ), INTENT(INOUT)     :: MS_SABLE, MS_VASE
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ACLADM, UNLADM
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: NLAYER
      LOGICAL,           INTENT(IN)     :: LGRAFED,CALWC
      LOGICAL,           INTENT(IN)     :: CALAC
      DOUBLE PRECISION,  INTENT(IN)     :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVA0(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FDM(NSICLA),XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AC(NSICLA),TOCE_SABLE
      LOGICAL,           INTENT(IN)     :: SEDCO(NSICLA), DEBU
      LOGICAL,           INTENT(IN)     :: MIXTE
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_SABLE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: ES_VASE(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: I,J
      DOUBLE PRECISION   :: DENS,DSTAR
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!  ------ BED COMPOSITION
!
      CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
!
!     ONLY ONE CLASS
!
      IF(NSICLA.EQ.1) THEN
        DO I=1,NPOIN
          AVAIL(I,1,1) = 1.D0
          ACLADM%R(I) = FDM(1)
        ENDDO
!       PURE MUD ONLY
        IF(SEDCO(1)) CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &                               ES_SABLE, ES_VASE,
     &                               ELAY%R,NOMBLAY,CONC_VASE,
     &                                MS_SABLE%R,MS_VASE%R,ZF%R,
     &                               ZR%R,AVA0,CONC,DEBU,.FALSE.)
!
      ELSE
!
!     NON-COHESIVE, MULTI-CLASSES
!
        IF(.NOT.MIXTE) THEN
!
!
          CALL INIT_AVAI
!         CALL MEAN_GRAIN_SIZE
!         THIS PART CAN BE INTEGRATED INTO INIT_AVAI
          DO J=1,NPOIN
            ACLADM%R(J) = 0.D0
            UNLADM%R(J) = 0.D0
            DO I=1,NSICLA
              IF(AVAIL(J,1,I).GT.0.D0) THEN
                ACLADM%R(J) = ACLADM%R(J) + FDM(I)*AVAIL(J,1,I)
                UNLADM%R(J) = UNLADM%R(J) + FDM(I)*AVAIL(J,2,I)
              ENDIF
            ENDDO
            ACLADM%R(J)=MAX(ACLADM%R(J),0.D0)
            UNLADM%R(J)=MAX(UNLADM%R(J),0.D0)
          ENDDO
        ELSE
!
          CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &               ES_SABLE, ES_VASE, ELAY%R,
     &               NOMBLAY,CONC_VASE,MS_SABLE%R,
     &               MS_VASE%R,ZF%R,ZR%R,AVA0,CONC,DEBU,MIXTE)
          DO I=1,NPOIN
            ACLADM%R(I) = FDM(1)
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(LGRAFED) THEN
        DO I=1, NSICLA
          FRACSED_GF(I)=AVA0(I)
        ENDDO
      ENDIF
!
!     SETTLING VELOCITY
!
      IF(.NOT.CALWC) THEN
        DENS = (XMVS - XMVE) / XMVE
        DO I = 1, NSICLA
          CALL VITCHU_SISYPHE(XWC(I),DENS,FDM(I),GRAV,VCE)
        ENDDO
      ENDIF
!
!     SHIELDS PARAMETER
!
      IF(.NOT.CALAC) THEN
        DENS  = (XMVS - XMVE )/ XMVE
        DO I = 1, NSICLA
          DSTAR = FDM(I)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
          IF (DSTAR <= 4.D0) THEN
            AC(I) = 0.24D0/DSTAR
          ELSEIF (DSTAR <= 10.D0) THEN
            AC(I) = 0.14D0*DSTAR**(-0.64D0)
          ELSEIF (DSTAR <= 20.D0) THEN
            AC(I) = 0.04D0*DSTAR**(-0.1D0)
          ELSEIF (DSTAR <= 150.D0) THEN
            AC(I) = 0.013D0*DSTAR**0.29D0
          ELSE
!           CORRECTION 30/05/2012
!           AC(I) = 0.055D0
            AC(I) = 0.045D0
          ENDIF
        ENDDO
      ENDIF
!
!     FOR MIXED SEDIMENTS
!
      IF(MIXTE) TOCE_SABLE=AC(1)*FDM(1)*GRAV*(XMVS - XMVE)

!-----------------------------------------------------------------------
!     
!      _____________________________________________________________________
!     /________ calculation of Node-areas for all nodes of grid ___________/
      ! calculation of Node-areas in T13      !  for parallel: here the
      ! interface-nodes
      !> node-area is allready reduced
      CALL VECTOR(T13,'=','MASBAS          '  
     &      ,IELMH_SIS,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)   
!         IF(NCSIZE.GT.1) CALL PARCOM(T13,2,MESH) 
!      
      CLOSE( SIS_FILES(SISMAF)%LU )  
      !> Close the SimuDig steering file (e.g. _DigActions.dat )
      !  It was opened by default.

!!!   SUBROUTINE   CALL InterFaceInitSimuDig
      CALL InterFaceInitNestor(
     &                             NCSIZE, IPID, NPOIN
     &                           , NSICLA
     &                           , MARDAT, MARTIM ! Sis start: date , time
     &                           , MOFAC    ! morphological factor
     &                           , DEBU                  !
     &                           , LEOPRD  ! period of graphical outputs
     &                        , MESH%X%R
     &                        , MESH%Y%R 
     &                        , T13%R
     &                        , MAXVAL( MESH%KNOLG%I(:) )
     &                                       )
     
! LEOPRD aus T2D, GRAFCOUNT in sisyphe.f
!
!
      WRITE(6,*)' --------  end nestor ----------'

      RETURN
      END















!
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!                    ***********************
                     SUBROUTINE xBEDLOAD_MAIN
!                    ***********************
!
     &(ACLADM,KSP,KSR, V2DPAR,UNSV2D,CF,EBOR,FW,HN,LIQBOR,
     & MASK, MASKEL, MASKPT, Q, QBOR, U2D,
     & V2D, S,UNLADM,UW,THETAW,MU,TOB,TOBW,TW,ZF,
     & DEBUG, HIDFAC, ICF, IELMT, ISOUS, KDDL, KDIR,
     & KENT, KINC, KLOG, KNEU, KSORT, LOADMETH, LT,
     & NPOIN, NPTFR, NSICLA, OPTBAN, LS0, BETA, FD90, FDM,
     & GRAV, HIDI, HMIN, VCE, CSF_SABLE, XMVE, XMVS, XWC,
     & PI, KARMAN, ZERO, KARIM_HOLLY_YANG,MSK, SUSP, VF,
     & ENTET, CONST_ALAYER, LCONDIS, LGRAFED, MESH,
     & ELAY, LIEBOR, LIMTEC, MASKTR,
     & IT1, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11,
     & T12,T13,UNORM,AC, AT0, DTS, ELAY0, FRACSED_GF,
     &   AVAIL, BREACH, CALFA_CL, COEFPN,
     & DZF_GF, HIDING, QSCL_C, QSCL_S, QS_C,
     & QSCLXC, QSXC, QSCLYC, QSYC, SALFA_CL, ZF_C, ZFCL_C, NSOUS,
     & ENTETS, SECCURRENT, SLOPEFF,
     & PHISED, DEVIA, BETA2, BIJK,SEDCO,HOULE,
     & U3D,V3D,CODE,FLBCLA,MAXADV)
!
!***********************************************************************
! SISYPHE   V7P2                                   21/07/2011
!***********************************************************************
!
!brief    MAIN SUBROUTINE FOR THE BEDLOAD TRANSPORT.
!
!history  F. HUVELIN
!+        14/09/2004
!+
!+
!
!history  JMH
!+        21/12/2006
!+        V6P0
!+   BEDLOAD_TIMESTEP NO LONGER EXISTS
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        28/03/2014
!+        V7P0
!+  Call to bedload_diffin changed.
!history  R KOPMANN (BAW)
!+        10/05/2016
!+        V7P2
!+ CALFA,SALFA dependent of grain classes
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AT0            |<->| TIME IN S
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| BETA           |-->| COEFFICIENT FOR SLOPING BED EFFECT ( KOCH AND FLOKSTRA)
!| BETA2          |-->| COEFFICIENT FOR THE DEVIATION  (TALMON ET AL.)
!| BIJK           |-->| COEFFICIENT OF THE BIJKER FORMULA
!| BREACH         |<->| INDICATOR FOR NON ERODIBLE BED (FINITE VOLUMES SHEMES)
!| CALFA          |<->| COSINUS OF THE ANGLE BETWEEN MEAN FLOW AND TRANSPORT
!| CF             |-->| QUADRATIC FRICTION COEFFICIENT
!| CODE           |-->| HYDRODYNAMIC CODE IN CASE OF COUPLING
!| COEFPN         |<->| CORRECTION OF TRANSORT FOR SLOPING BED EFFECT
!| CONST_ALAYER   |-->| CONSTANT ACTIVE LAYER THICKNESS OR NOT
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| DEVIA          |-->| SLOPE EFFECT FORMULA FOR DEVIATION
!| DTS            |<->| TIME STEP FOR SUSPENSION
!| DZF_GF         |<->| (A SUPPRIMER)
!| EBOR           |<->| IMPOSED BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ELAY0          |<->| ACTIVE LAYER THICKNESS
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION
!| ENTETS         |<->| LOGICAL, IF YES INFORMATION IS GIVEN ON MASS CONSERVATION FOR SUSPENSION
!| FD90           |-->| DIAMETER D90
!| FDM            |-->| DIAMETER DM FOR EACH CLASS
!| FLBCLA         |-->| BLOCK OF FLUXES AT BOUNDARY FOR EACH CLASS
!| FRACSED_GF     |<->| (A SUPPRIMER)
!| FW             |-->| WAVE FRICTION FACTOR
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| HIDFAC         |-->| HIDING FACTOR FORMULAS
!| HIDI           |-->| HIDING FACTOR FOR PARTICULAR SIZE CLASS (HIDFAC =0)
!| HIDING         |-->| HIDING FACTOR CORRECTION
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| HOULE          |-->| LOGICAL, FOR WAVE EFFECTS
!| ICF            |-->| BED-LOAD OR TOTAL LOAD TRANSPORT FORMULAS
!| IELMT          |-->| NUMBER OF ELEMENTS
!| ISOUS          |-->| SUB-ITERATIONS
!| IT1            |<->| INTEGER WORK ARRAY IN A BIEF_OBJ STRUCTURE
!| KARMAN         |-->| VON KARMAN CONSTANT
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| KSP            |-->| BED SKIN ROUGHNESS
!| KSR            |-->| RIPPLE BED ROUGHNESS
!| LCONDIS        |-->| LOGICAL, CONSTANT FLOW DISCHARGE
!| LGRAFED        |-->| (A SUPPRIMER)
!| LIEBOR         |<->| TYPE OF BOUNDARY CONDITIONS FOR BED EVOLUTION
!| LIMTEC         |<->| TECHNICAL BOUNDARY CONDITION (NEUMAN...)
!| LIQBOR         |-->| TYPE OF BOUNDARY CONDITION FOR QS
!| LOADMETH       |-->| (A SUPPRIMER)
!| LS0            |-->| (A SUPPRIMER)
!| LT             |-->| ITERATION
!| MASK           |-->| BLOCK OF MASKS, EVERY ONE FOR A TYPE OF BOUNDARY
!|                |   | SEE DIFFIN.F IN LIBRARY BIEF.
!| MASKEL         |-->| MASKING OF ELEMENTS
!| MASKPT         |-->| MASKING PER POINT
!| MASKTR         |<->| MASKING FOR TRACERS, PER POINT
!| MESH           |<->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS
!| MU             |<->| CORRECTION FACTOR FOR BED ROUGHNESS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NSOUS          |<->| NUMBER OF SUB-ITERATIONS
!| OPTBAN         |-->| OPTION FOR TIDAL FLATS
!| PHISED         |-->| ANGLE OF REPOSE OF THE SEDIMENT
!| PI             |-->| PI
!| Q              |-->| FLOW DISCHARGE
!| QBOR           |-->| BOUNDARY CONDITION FOR TRANSPORT RATE
!| QSCLXC         |<->| TRANSPORT RATE FOR EACH CLASS X-DIRECTION
!| QSCLYC         |<->| TRANSPORT RATE FOR EACH CLASS Y-DIRECTION
!| QSCL_C         |<->| BEDLOAD TRANSPORT RATE
!| QSCL_S         |<->| SUSPENDED LOAD TRANSPORT RATE
!| QSXC           |<->| BEDLOAD TRANSPORT RATE X-DIRECTION
!| QSYC           |<->| BEDLOAD TRANSPORT RATE Y-DIRECTION
!| QS_C           |<->| BEDLOAD TRANSPORT RATE
!| S              |-->| VOID STRUCTURE
!| SALFA          |<->| SINUS OF THE ANGLE BETWEEN TRANSPORT RATE AND CURRENT
!| SECCURRENT     |-->| LOGICAL, PARAMETRISATION FOR SECONDARY CURRENTS
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| SLOPEFF        |-->| LOGICAL, SLOPING BED EFFECT OR NOT
!| SUSP           |-->| LOGICAL, SUSPENSION
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| T11            |<->| WORK BIEF_OBJ STRUCTURE
!| T12            |<->| WORK BIEF_OBJ STRUCTURE
!| T13            |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| T3             |<->| WORK BIEF_OBJ STRUCTURE
!| T4             |<->| WORK BIEF_OBJ STRUCTURE
!| T5             |<->| WORK BIEF_OBJ STRUCTURE
!| T6             |<->| WORK BIEF_OBJ STRUCTURE
!| T7             |<->| WORK BIEF_OBJ STRUCTURE
!| T8             |<->| WORK BIEF_OBJ STRUCTURE
!| T9             |<->| WORK BIEF_OBJ STRUCTURE
!| THETAW         |-->| ANGLE BETWEEN WAVE AND CURRENT
!| TOB            |<->| BED SHEAR STRESS (TOTAL FRICTION)
!| TOBW           |-->| WAVE INDUCED SHEAR STRESS
!| TW             |-->| WAVE PERIOD
!| U2D            |<->| MEAN FLOW VELOCITY X-DIRECTION
!| U3D            |-->| THREE-DIMENSIONAL VELOCITY X-DIRECTION
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| UNORM          |<->| NORM OF THE MEAN FLOW VELOCITY
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| UW             |-->| ORBITAL WAVE VELOCITY
!| V2D            |<->| MEAN FLOW VELOCITY Y-DIRECTION
!| V2DPAR         |-->| INTEGRAL OF TEST FUNCTIONS, ASSEMBLED IN PARALLEL
!| V3D            |-->| THREE-DIMENSIONAL VELOCITY Y-DIRECTION
!| VCE            |-->| WATER VISCOSITY
!| VF             |-->| LOGICAL, FINITE VOLUMES OR NOT
!| CSF_SABLE      |-->| BED VOLUME CONCENTRATION CSF = (1-POROSITY)
!| XMVE           |-->| FLUID DENSITY
!| XMVS           |-->| SEDIMENT DENSITY
!| XWC            |-->| SETTLING VELOCITY
!| ZERO           |-->| ZERO
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZFCL_C         |<->| BEDLOAD EVOLUTION FOR EACH SEDIMENT CLASS
!| ZF_C           |<->| BEDLOAD EVOLUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_BEDLOAD_MAIN => BEDLOAD_MAIN
cgl   USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM,NOMBLAY
      USE DECLARATIONS_SISYPHE, ONLY :           NOMBLAY
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, KSR,V2DPAR,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CF,FW,KSP,HN,LIQBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASK, MASKEL, MASKPT
      TYPE(BIEF_OBJ),   INTENT(IN)    :: Q, QBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: U2D,V2D,TOB,MU,UNORM,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, THETAW,  TOBW, TW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF
      INTEGER,          INTENT(IN)    :: DEBUG, HIDFAC, ICF,MAXADV
      INTEGER,          INTENT(IN)    :: IELMT, ISOUS, KDDL, KDIR, KENT
      INTEGER,          INTENT(IN)    :: KINC, KLOG, KNEU, KSORT
      INTEGER,          INTENT(IN)    :: LOADMETH, LT,NPOIN, NPTFR
      INTEGER,          INTENT(IN)    :: NSICLA, OPTBAN
      DOUBLE PRECISION, INTENT(IN)    :: LS0, BETA, FD90(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: FDM(NSICLA),GRAV
      DOUBLE PRECISION, INTENT(IN)    :: HIDI(NSICLA),HMIN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: CSF_SABLE,XMVE,XMVS,XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: PI,KARMAN,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: MSK, SUSP, VF
      LOGICAL,          INTENT(IN)    :: ENTET, CONST_ALAYER
      LOGICAL,          INTENT(IN)    :: LCONDIS, LGRAFED,SECCURRENT
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),HOULE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ELAY,FLBCLA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: LIEBOR, LIMTEC, MASKTR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: IT1,T1,T2,T3,T4,T5,T6,T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T9,T10,T11,T12,T13
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSICLA), AT0, DTS, ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, CALFA_CL, COEFPN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZF_GF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS_C, QSCLXC, QSXC, QSCLYC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSYC, SALFA_CL, ZF_C, ZFCL_C
      INTEGER,          INTENT(INOUT) :: NSOUS
      LOGICAL,          INTENT(INOUT) :: ENTETS
      DOUBLE PRECISION,   INTENT(IN)  :: BETA2, PHISED
      INTEGER, INTENT (IN)            :: SLOPEFF, DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BIJK
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!     --> nestor       
      LOGICAL :: NESTOR = .TRUE.
!
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
!     INITIALISES TECHNICAL BOUNDARY CONDITIONS
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_DIFFIN'
      CALL BEDLOAD_DIFFIN
     &        (U2D, V2D, MESH%NBOR, MESH%XNEBOR, MESH%YNEBOR,
     &         MASKEL, MESH%NELBOR, NPTFR, KENT, KSORT, KLOG,
     &         KDIR, KDDL, KNEU, MSK, IT1, LIEBOR, MASKTR,LIMTEC,
     &         MESH%IKLBOR%I,MESH%NELEB,MESH%NELEBX)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_DIFFIN'
!
      DO I = 1, NSICLA
!
!       FOR SAND
        IF(.NOT.SEDCO(I)) THEN
          IF (DEBUG > 0) WRITE(LU,*)
     &      'BEDLOAD_SOLIDISCHARGE : ',I,'/',NSICLA
          CALL BEDLOAD_SOLIDISCHARGE
     &       (MESH, U2D, V2D, UNORM,HN, TW, UW, MU,TOB,CF,
     &         TOBW,FW,THETAW,AVAIL(1:NPOIN,1,I),
     &         MASKPT, MASKEL, ACLADM,
     &         UNLADM,KSP,KSR, LIQBOR, QBOR%ADR(I)%P, DEBUG, NPOIN,
     &         NPTFR, IELMT, ICF, KENT, OPTBAN, HIDFAC, GRAV,
     &         FDM(I), FD90(I), XWC(I), XMVE, XMVS, VCE, HMIN,
     &         HIDI(I),KARMAN,ZERO,PI,
     &         KARIM_HOLLY_YANG,SUSP,MSK,T1,T2,
     &         T3, T4, T5, T6, T7, T8, T9, T10, T11,T12, AC(I),
     &         HIDING,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &         SLOPEFF,COEFPN,PHISED,
     &         CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
     &         BETA,ZF,S,
     &         DEVIA, BETA2 , SECCURRENT, BIJK,HOULE,UNSV2D,
     &         U3D,V3D,CODE)
          IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLIDISCHARGE'
        ELSE
!         FOR COHESIVE SEDIMENT: ZERO BEDLOAD TRANSPORT RATE
!         JMH: IS THIS USEFUL ???
          CALL OS('X=0     ',X=QSCL_C%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLXC%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLYC%ADR(I)%P)
        ENDIF
!
      ENDDO
!
!     COMPUTES THE EVOLUTION FOR EACH CLASS
!
      DO I = 1, NSICLA
!
        IF(.NOT.SEDCO(I)) THEN
!
          IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EVOL : ',I,'/',NSICLA
          CALL BEDLOAD_EVOL(HN,Q,S,ELAY,ACLADM,AVAIL(1:NPOIN,1,I),
     &                      COEFPN,CALFA_CL%ADR(I)%P,SALFA_CL%ADR(I)%P,
     &                      LIMTEC,
     &                      EBOR%ADR(I)%P,MASKEL,MASK,
     &                      V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,IELMT,
     &                      KENT,KDIR,KDDL,LOADMETH,
     &                      DTS,FDM(I),FD90(I),HMIN,LS0,GRAV,XMVS,XMVE,
     &                      VCE,VF,ENTETS,MSK,CONST_ALAYER,
     &                      LCONDIS,MESH,QSCL_C%ADR(I)%P,
     &                      T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     &                      T13,CSF_SABLE,BREACH,QSCLXC%ADR(I)%P,
     &                      QSCLYC%ADR(I)%P,ZFCL_C%ADR(I)%P,SLOPEFF,
     &                      I,FLBCLA,LIQBOR,QBOR%ADR(I)%P,MAXADV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_EVOL'
!
!         NOW DIVIDING BY CSF_SABLE TO GET THE EVOLUTION OF BED
!         INCLUDING VOIDS
!         NOTE JMH: IN BEDLOAD_EVOL THERE IS A PRELIMINARY MULTIPLICATION BY
!                   CSF_SABLE, SO THIS COULD BE SIMPLIFIED, BUT FOR THE
!                   FINITE ELEMENT OPTION ONLY, THE FINITE VOLUME IMPLEMENTATION
!                   SEEMS MORE COMPLICATED TO SORT OUT.
!                  
          CALL OS('X=CX    ',X= ZFCL_C%ADR(I)%P,C=1.D0/CSF_SABLE)
!
        ELSE
!
!         NO EVOLUTION FOR COHESIVE SEDIMENT
          CALL OS('X=0     ',X=ZFCL_C%ADR(I)%P)
!
        ENDIF
!
      ENDDO
!
!     nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!      nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!       nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!        nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!         nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!          nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!           nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!            nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor \ nestor
!            nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!           nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!          nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!         nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!        nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!       nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!      nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!     nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor / nestor
!
       IF (NESTOR.EQV..TRUE.) THEN
       CALL InterFaceRunNestor(   NPOIN      !  number of points (Nodes)
     &                          , NSICLA     !  number of SIze CLAsses
     &                          , LT         !  Telemac time step
     &                          , DTS        !  duration of Sisyphe time step
     &                          , AT0        !  time
     &                          , ELAY0      !  active layer thickness [m]
     &                          , ZF%R       !  bottom [m+NN]
     &                          , ZFCL_C     !  evolution per class per time step [m]
     &                          , AVAIL(1:NPOIN,1,1:NSICLA)    !
     &                          , MESH%KNOLG%I    ! index list: Local to Global node index
!    &                          , MESH%KNOGL%I    ! index list: Global to Local node index
!    &                          , SIZE( MESH%KNOGL%I(:))    ! index list: Global to Local node index
!    &                          , MESH%X%R
!    &                          , MESH%Y%R
     &                        )
       ENDIF
!
!     CALLS DREDGESIM
!
!     IF(DREDGESIM) CALL DREDGESIM_INTERFACE(2)
      ! *********************************************** !
      ! II - EVOLUTIONS AND QS FOR EACH CLASS ARE ADDED !
      ! *********************************************** !
      ! II.1 - INITIALISES
      ! ---------------------
      CALL OS('X=0     ', X=QS_C)
      CALL OS('X=0     ', X=ZF_C)
      CALL OS('X=0     ',X=QSXC)
      CALL OS('X=0     ',X=QSYC)
      ! II.2 - ADDS THE CLASSES
      ! ----------------------
      !
      DO I=1,NSICLA
        IF(.NOT.SEDCO(I)) THEN
          CALL OS('X=X+Y   ', X=QS_C, Y=QSCL_C%ADR(I)%P)
          CALL OS('X=X+Y   ', X=ZF_C, Y=ZFCL_C%ADR(I)%P)
           CALL OS('X=X+YZ  ', X=QSXC, Y=QSCL_C%ADR(I)%P, 
     &                               Z=CALFA_CL%ADR(I)%P)
           CALL OS('X=X+YZ  ', X=QSYC, Y=QSCL_C%ADR(I)%P, 
     &                               Z=SALFA_CL%ADR(I)%P)

        ENDIF
      ENDDO
!
!     TIDAL FLATS WITH MASKING     JMH ON 27/07/2006
!
      IF(OPTBAN.EQ.2) CALL OS('X=XY    ',X=ZF_C,Y=MASKPT)
!
      ! II.3 - SLOPE EFFECT FOR THE SUM OF THE QS
      ! -----------------------------------------
      ! QS : COEFPN AND CALFA, SALFA ALREADY ADDED IN QSCL_C
!      CALL OS('X=YZ    ', X=QSXC, Y=QS_C, Z=CALFA)
!      CALL OS('X=YZ    ', X=QSYC, Y=QS_C, Z=SALFA)
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
      
      

