
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES ADVECTION FOR ADVECTED VARIABLES

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
!> </td><td> 18/12/2009
!> </td><td> J.M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE PREPARE_ADVECTION
     & (FN,S0F,FBORL,LIFBOL,FLUXF,
     &  SCHCF,CALFLU,MESH3D,MASKEL,NPTFR3,VOLUNPAR,FLUEXT,FLUEXTPAR,
     &  NBOR3,DT,MSK,IELM3,NUMLIQ,DIRFLU,NFRLIQ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CALFLU         |-->| INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
C| DIRFLU         |---| 
C| FBORL          |-->| CONDITIONS AUX LIMITES DIRICHLET
C| FLUEXT         |-->| FLUX EXTERIEUR PAR NOEUD
C| FLUXF          |<->| FLUX GLOBAL A INCREMENTER
C| FN             |-->| VARIABLE AU TEMPS N 
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| LIFBOL         |-->| TYPE DE CONDITIONS LIMITES PHYSIQUES
C| LIFBOS         |---| 
C| LIMDIF         |-->| TYPE DE CONDITIONS LIMITES TECHNIQUES
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH3D         |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES  
C| NBOR3          |-->| NUMEROS GLOBAUX DES POINTS FRONTIERES 3D
C| NFRLIQ         |---| 
C| NPTFR3         |-->| NOMBRE DE POINTS FRONTIERE BORDS LATERAUX
C| NUMLIQ         |---|  
C| S0F            |-->| TERME SOURCE EXPLICITE (DIM=F/T) 
C| SCHCF          |-->| SCHEMA DE CONVECTION DE F
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_PREPARE_ADVECTION => PREPARE_ADVECTION   
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FN,S0F,LIFBOL,FBORL
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXT,FLUEXTPAR
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXF
      INTEGER, INTENT(IN)             :: SCHCF,NPTFR3,NFRLIQ,IELM3
      INTEGER, INTENT(IN)             :: NUMLIQ(*),DIRFLU(*)
      LOGICAL, INTENT(IN)             :: CALFLU,MSK
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,NBOR3,VOLUNPAR
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IP,K,IPTFR,IS,I
!
      DOUBLE PRECISION LAMBDA
!
      LOGICAL YADIRFLU,VELOCITY
!
!***********************************************************************
!
      VELOCITY=.FALSE.
      IF(FN%NAME(1:1).EQ.'U'.OR.
     &   FN%NAME(1:1).EQ.'V'.OR.
     &   FN%NAME(1:1).EQ.'W') VELOCITY=.TRUE.
!
!     EVEN IF.NOT.CALFLU
!
      FLUXF = 0.D0
!
C     WITH DISTRIBUTIVE SCHEMES : COMPUTES PRESCRIBED VALUES THAT
C     WILL ENSURE THE CORRECT FLUX (REAL PRESCRIBED VALUES DISCARDED)
C     THESE CORRECTED PRESCRIBED VALUES ARE SET BEFORE ADVECTION
!
C     YADIRFLU=.TRUE. : THERE IS AT LEAST ONE BOUNDARY WITH
C                       TREATMENT OF FLUXES AT BOUNDARIES = 2
      YADIRFLU=.FALSE.
C     DIRFLU DISCARDED FOR VELOCITIES
      IF(NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO K=1,NFRLIQ
          IF(DIRFLU(K).EQ.2) YADIRFLU=.TRUE.
        ENDDO
      ENDIF
!
!=======================================================================
!
!     FOR TRACERS (=NOT VELOCITY) : DIRICHLET VALUES ARE NOT RESPECTED IF EXIT
!     THERE IS NO NEED TO TEST KENTU OR KADH FOR TRACERS
!
      IF(NPTFR3.GT.0.AND.NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT) THEN
!           EXITS ARE TREATED AS FREE BOUNDARIES
            IP=NBOR3%I(IPTFR)
            IF(FLUEXTPAR%R(IP).GE.0.D0) LIFBOL%I(IPTFR)=KSORT
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
!
!     VELOCITIES ARE ADVECTED FOR THE NEXT TIME STEP
!     HENCE NO CHANGE OF U, V AND W AND BOUNDARY CONDITIONS
!
      IF(.NOT.VELOCITY) THEN
!
!     A PRIORI CORRECTION OF FN FOR REAL ENTRANCES
!     I.E. LIFBOL STILL KENT DESPITE ABOVE CHANGE
!
C     IF((SCHCF.EQ.ADV_SUP   .OR.SCHCF.EQ.ADV_NSC    .OR.
C    &    SCHCF.EQ.ADV_PSI   .OR.SCHCF.EQ.ADV_LPO    .OR.
C    &    SCHCF.EQ.ADV_LPO_TF.OR.SCHCF.EQ.ADV_NSC_TF)
C    &                                              .AND.YADIRFLU) THEN
!
      IF(YADIRFLU) THEN
!
        IF(NPTFR3.GT.0) THEN
!
        DO IP=1,NPTFR3
          IF(NUMLIQ(IP).GE.1) THEN
          IF(DIRFLU(NUMLIQ(IP)).EQ.2.AND.LIFBOL%I(IP).EQ.KENT) THEN
            I=NBOR3%I(IP)
            LAMBDA=-FLUEXTPAR%R(I)*DT/
     &      (MAX(VOLUNPAR%R(I),1.D-10)-FLUEXTPAR%R(I)*DT)
            FN%R(I)=FN%R(I)+LAMBDA*(FBORL%R(IP)-FN%R(I))
!           CORRECTION OF FLUX
!           IN THE PROOF OF MASS-CONSERVATION, FLUEXT IS MULTIPLIED
!           BY FN INSTEAD OF FBOR, TO INTERPRET THE ADDED MASS AS
!           A FLUX THIS CORRECTION IS NECESSARY
!           HERE IT IS THE FN MODIFIED ABOVE
!           EVEN IF NOT CALFLU (CHEAPER)
            FLUXF=FLUXF+(FBORL%R(IP)-FN%R(I))*FLUEXT%R(I)*DT
!           AVOIDS A DIRICHLET TREATMENT HEREAFTER AND BY DIFF3D -
!           WILL BE RESTORED AFTER DIFF3D
            LIFBOL%I(IP)=KSORT
          ENDIF
          ENDIF
        ENDDO
!
        ENDIF
!
      ENDIF
!
!=======================================================================
!
C     PUTS DIRICHLET VALUES IN FN
C     MAY HAVE NO EFFECT IF TREATMENT OF FLUXES AT THE BOUNDARIES=2
C     BECAUSE LIFBOL CHANGED ABOVE
!
      IF(NPTFR3.GT.0) THEN
        DO IPTFR=1,NPTFR3
          IF(LIFBOL%I(IPTFR).EQ.KENT .OR.
     &       LIFBOL%I(IPTFR).EQ.KENTU.OR.
     &       LIFBOL%I(IPTFR).EQ.KADH) THEN
             FN%R(NBOR3%I(IPTFR)) = FBORL%R(IPTFR)
          ENDIF
        ENDDO
      ENDIF
!
!     HERE BOTTOM AND FREE SURFACE SHOULD BE TREATED AS WELL
!
!
!     END OF IF(.NOT.VELOCITY)
      ENDIF
!
!=======================================================================
!
!     3D ADVECTION (OTHER THAN SUPG)
!
!=======================================================================
!
!     WITH DISTRIBUTIVE SCHEMES, RIGHT-HAND SIDE MUST BE
!     IN INTEGRATED FORM (BEWARE, ORIGINAL S0F THUS MODIFIED)
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI.OR.SCHCF.EQ.ADV_LPO.OR.
     &   SCHCF.EQ.ADV_NSC_TF.OR.SCHCF.EQ.ADV_LPO_TF) THEN
!
        IF(S0F%TYPR.NE.'0') THEN
!
          CALL VECTOR(S0F,'=','MASVEC          ',IELM3,1.D0,
     &                S0F,S0F,S0F,S0F,S0F,S0F,MESH3D,MSK,MASKEL)
          IF(NCSIZE.GT.1) CALL PARCOM(S0F,2,MESH3D)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
