!                       *****************
                        SUBROUTINE QDSCUR
!                       *****************
!
     &( TSTOT , TSDER , F     , CF    , XK    , FREQ  , USOLD , USNEW , 
     &  DEPTH , PROINF, CDSCUR, CMOUT4, NF    , NPLAN , NPOIN2, CIMPLI,
     &  F_INT  , BETOTO, BETOTN)
!
!**********************************************************************
! TOMAWAC   V7P0                                 30/07/2014
!**********************************************************************
!
!brief   COMPUTES THE CONTRIBUTION OF THE WAVE BLOCKING SINK TERM USING
!+          THE  PARAMETRISATION OF VAN DER WESTHUYSEN (2012).
!
!reference    VAN DER WESTHUYSEN (2012):  SPECTRAL 
!+              MODELLING OF WAVES DISSIPATION ON NEGATIVE
!+                   CURRENT GRADIENTS
!
!history  E. GAGNAIRE-RENOU (EDF/LNHE)
!+        09/2014
!+        V7P0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BETOTN         |<->| WORK TABLE
!| BETOTO         |<->| WORK TABLE
!| F_INT          |<->| WORK TABLE
!| CIMPLI         |-->| IMPLICITATION COEFFICIENT FOR SOURCE TERM INTEG.
!| CF             |-->| ADVECTION FIELD ALONG FREQUENCY
!| CDSCUR         |-->| WESTHUYSEN WAVE BLOCKING COEFFICIENT
!| CMOUT4         |-->| WESTHUYSEN SATURATION THRES. FOR THE DISSIPATION
!| DEPTH          |-->| WATER DEPTH
!| F              |-->| DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| XK             |-->| DISCRETIZED WAVE NUMBER
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| PROINF         |-->| LOGICAL INDICATING INFINITE DEPTH ASSUMPTION
!| TSDER          |<->| DERIVED PART OF THE SOURCE TERM CONTRIBUTION
!| TSTOT          |-->| TOTAL PART OF THE SOURCE TERM CONTRIBUTION
!| USNEW          |-->| FRICTION VELOCITY AT TIME N+1
!| USOLD          |<->| FRICTION VELOCITY AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  REMARKS:
!  ********
!
!  - THE CONSTANT CDSCUR (C"dis,break) UTILISED IN WESTHUYSEN (2012)
!                                    IS EQUAL TO 0.65 BY DEFAULT
!  - THE CONSTANT CMOUT4 (Br) UTILISED IN WESTHUYSEN (2007)
!                                    IS EQUAL TO 1.75*10^(-3)
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI,GRAVIT
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NF,NPLAN,NPOIN2
      DOUBLE PRECISION, INTENT(IN)    :: CMOUT4, CDSCUR
      DOUBLE PRECISION, INTENT(IN)    :: CIMPLI
      DOUBLE PRECISION, INTENT(IN)    :: USNEW(NPOIN2),USOLD(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: FREQ(NF),DEPTH(NPOIN2) 
      DOUBLE PRECISION, INTENT(IN)    :: XK(NPOIN2,NF) 
      DOUBLE PRECISION, INTENT(INOUT) :: F_INT(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTO(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: BETOTN(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TSTOT(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN2,NPLAN,NF)
      LOGICAL, INTENT(IN)             :: PROINF
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER JP,IFF,IP
      DOUBLE PRECISION P0O,P0N,W,SURDEUPIFREQ,B,DTETAR
      DOUBLE PRECISION CPHAS,CG1,SQBSCMOUT4,BETA,DEUKD,KD,SURCMOUT4
!
      INTRINSIC SQRT,TANH,DBLE,SINH
!
!-----------------------------------------------------------------------
!
      DTETAR=1.D0/DBLE(NPLAN)
      W=25.D0
      SURCMOUT4=1.D0/CMOUT4
!
!     LOOP ON THE DISCRETISED FREQUENCIES
! 
      DO IFF=1,NF
!       
        SURDEUPIFREQ=1.D0/(DEUPI*FREQ(IFF))
!
        DO IP=1,NPOIN2
          F_INT(IP)=F(IP,1,IFF)
        ENDDO
        DO JP=2,NPLAN
          DO IP=1,NPOIN2
            F_INT(IP)=F_INT(IP)+F(IP,JP,IFF)
          ENDDO
        ENDDO
        DO IP=1,NPOIN2
          F_INT(IP)=F_INT(IP)*DTETAR
        ENDDO
!
        IF(PROINF) THEN
!
          DO IP=1,NPOIN2
!
            CPHAS=XK(IP,IFF)*SURDEUPIFREQ
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            P0N=3.D0+TANH(W*(USNEW(IP)*CPHAS-0.1D0))                    
            CG1=0.5D0*GRAVIT*SURDEUPIFREQ
            B=CG1*F_INT(IP)*XK(IP,IFF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)           
            DO JP=1,NPLAN
              BETOTO(IP,JP)=-CDSCUR*SQBSCMOUT4**(P0O/2)*
     &      MAX(CF(IP,JP,IFF)/FREQ(IFF),0.D0) 
              BETOTN(IP,JP)=-CDSCUR*SQBSCMOUT4**(P0N/2)*
     &      MAX(CF(IP,JP,IFF)/FREQ(IFF),0.D0) 
            ENDDO
!
          ENDDO
!
        ELSE
!
          DO IP=1,NPOIN2
! 
            CPHAS=XK(IP,IFF)*SURDEUPIFREQ      
            P0O=3.D0+TANH(W*(USOLD(IP)*CPHAS-0.1D0))
            P0N=3.D0+TANH(W*(USNEW(IP)*CPHAS-0.1D0))         
            KD=MIN(XK(IP,IFF)*DEPTH(IP),350.D0)
            DEUKD=KD+KD                                            
            CG1=( 0.5D0+XK(IP,IFF)*DEPTH(IP)/SINH(DEUKD) )/CPHAS     
            B=CG1*F_INT(IP)*XK(IP,IFF)**3
            SQBSCMOUT4=SQRT(B*SURCMOUT4)                            
            DO JP=1,NPLAN
              BETOTO(IP,JP)=-CDSCUR*SQBSCMOUT4**(P0O/2)*
     &      MAX(CF(IP,JP,IFF)/FREQ(IFF),0.D0) 
              BETOTN(IP,JP)=-CDSCUR*SQBSCMOUT4**(P0N/2)*
     &      MAX(CF(IP,JP,IFF)/FREQ(IFF),0.D0) 
            ENDDO
!
          ENDDO 
!
        ENDIF        
!
!       TAKES THE SOURCE TERM INTO ACCOUNT
!      
        DO JP=1,NPLAN
          DO IP=1,NPOIN2
            TSTOT(IP,JP,IFF)=TSTOT(IP,JP,IFF)
     &      +(BETOTO(IP,JP)+CIMPLI*(BETOTN(IP,JP)-BETOTO(IP,JP)))
     &      *F(IP,JP,IFF)
            TSDER(IP,JP,IFF)=TSDER(IP,JP,IFF)+BETOTN(IP,JP)
          ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *****************
                     SUBROUTINE ANACOS
!                    *****************
!
     &( UC    , VC    , X     , Y     , NPOIN2 )
!
!***********************************************************************
! TOMAWAC   V6P1                                   09/06/2011
!***********************************************************************
!
!brief    SPECIFIES A ! STATIONARY ! ANALYTICAL CURRENT.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+      UCONST=0.D0
!+      VCONST=0.D0
!+
!+      DO 100 IP=1,NPOIN2
!+        UC(IP)=UCONST
!+        VC(IP)=VCONST
!+  100 CONTINUE
!
!history
!+        07/06/2001
!+        V5P2
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
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UC             |<--| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |<--| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP
!
      DO IP=1,NPOIN2
        IF (X(IP).GE.(1.56D0).AND.X(IP).LE.(2.78D0)) THEN
          UC(IP)=-0.13*1/(1-(0.3D0/1.22D0)*(X(IP)-1.56D0)/0.75D0)
        ELSEIF (X(IP).GT.(2.78D0).AND.X(IP).LE.(5.22D0)) THEN
	  UC(IP)=-0.22D0
        ELSEIF (X(IP).GT.(5.22D0).AND.X(IP).LE.(6.44D0)) THEN
	  UC(IP)=-0.22*1/(1+(0.3D0/1.22D0)*(X(IP)-5.22D0)/0.45D0)
        ELSE
          UC(IP)=-0.13D0
        ENDIF	  
        VC(IP)=0.D0
	WRITE(LU,*) IP,UC(IP),VC(IP),'IP,UC,VC'
      ENDDO
!
!
      RETURN
      END
!                    *********************
                     SUBROUTINE TOM_CORFON
!                    *********************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!
!history  F. MARCOS
!+
!+
!+
!
!history  OPTIMER (    )
!+        12/01/2001
!+
!+   TOMAWAC/COWADIS MERGE
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!
CEGR---------------------------------MODIF debut
      INTEGER IP
!
      DO IP=1,NPOIN2
        IF (X(IP).GE.(1.56D0).AND.X(IP).LE.(2.78D0)) THEN
          ZF(IP)=-0.75+(30.D0/122.D0)*(X(IP)-1.56D0)
        ELSEIF (X(IP).GT.(2.78D0).AND.X(IP).LE.(5.22D0)) THEN
	  ZF(IP)=-0.45D0
        ELSEIF (X(IP).GT.(5.22D0).AND.X(IP).LE.(6.44D0)) THEN
	  ZF(IP)=-0.45-(30.D0/122.D0)*(X(IP)-5.22D0)
        ELSE
          ZF(IP)=-0.75D0
        ENDIF	  
      ENDDO
CEGR---------------------------------MODIF fin
!
      RETURN
      END
