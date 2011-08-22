!                    **************************
                     SUBROUTINE POINT_TELEMAC2D
!                    **************************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES THE STRUCTURES.
!
!history
!+        11/07/2008
!+
!+   SIZE FOR LIMPRO
!
!history
!+        02/10/2008
!+
!+   NTR=22
!
!history
!+        02/04/2009
!+
!+   T2D_FILES(T2DGEO)%LU REPLACES NGEO
!
!history
!+        26/11/2009
!+
!+   SPECIFIC ADVECTION IF EQUA='SAINT-VENANT VF', NO
!
!history  J-M HERVOUET (LNHE)
!+        24/03/2010
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
!history  J-M HERVOUET (LNHE)
!+        21/03/2011
!+        V6P1
!+   Allocation of KFRO_B as an integer instead of a real BIEF_OBJ
!+   An overlooked bug for a long time!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
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
      INTEGER MEMW1,NTR,NTRT,NTRKE,I,J,I3,I4,ITRAC
      INTEGER IELMX,IELMC1,IELMC2,IELMUT,IELMHT
      INTEGER IELBU,IELBH,IELBT,IELBK,IELBE,IELB1
      INTEGER IELBX,CFG(2),CFGBOR(2),ERR
!
      CHARACTER*1 TYP
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,20)
         IF(LNG.EQ.2) WRITE(LU,21)
      ENDIF
20    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '* ALLOCATION DE LA MEMOIRE  *',/,
     &26X,              '*****************************',/)
21    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '*    MEMORY ORGANIZATION    *',/,
     &26X,              '*****************************',/)
!
!-----------------------------------------------------------------------
!
!     TYPES OF DISCRETISATIONS
!
      IELM0 = 10*(IELMH/10)
      IELM1 = IELM0 + 1
!
      IELB1 = IELBOR(IELM1,1)
      IELBU = IELBOR(IELMU,1)
      IELBH = IELBOR(IELMH,1)
      IELBT = IELBOR(IELMT,1)
      IELBK = IELBOR(IELMK,1)
      IELBE = IELBOR(IELME,1)
!
      IELMX=MAX(IELMU,IELMH,IELMT,IELMK,IELME)
!
! TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
!
      CFG(1) = OPTASS
      CFG(2) = PRODUC
!     CFG FOR THE BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
!
!=======================================================================
!
!     ALLOCATES THE MESH STRUCTURE
!
      CALL ALMESH(MESH,'MESH  ',IELMX,SPHERI,CFG,T2D_FILES(T2DGEO)%LU,
     &            EQUA,I3=I3,I4=I4,
     &            FILE_FORMAT=T2D_FILES(T2DGEO)%FMT)
!
!     IF COORDINATES OF ORIGIN ARE IN GEOMETRY FILE AND NOT IN STEERING
!     FILE, THE VALUES OF GEOMETRY FILE ARE TAKEN
!
      IF(I3.NE.0.AND.I_ORIG.EQ.0) I_ORIG=I3
      IF(I4.NE.0.AND.J_ORIG.EQ.0) J_ORIG=I4
!
!     ALIAS FOR CERTAIN COMPONENTS OF MESH
!
      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
!
      NELEM => MESH%NELEM
      NELMAX=> MESH%NELMAX
      NPTFR => MESH%NPTFR
      NPTFRX=> MESH%NPTFRX
      DIM   => MESH%DIM
      TYPELM=> MESH%TYPELM
      NPOIN => MESH%NPOIN
      NPMAX => MESH%NPMAX
      MXPTVS=> MESH%MXPTVS
      MXELVS=> MESH%MXELVS
      LV    => MESH%LV
!
!=======================================================================
!
!                     **********************
!                     *   REAL ARRAYS      *
!                     **********************
!
!-----------------------------------------------------------------------
!
      ALLOCATE(W(NPOIN),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'POINT_TELEMAC2D : MAUVAISE ALLOCATION DE W'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'POINT_TELEMAC2D: WRONG ALLOCATION OF W'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!                       ******************
!                       *   STRUCTURES   *
!                       ******************
!
!-----------------------------------------------------------------------
!
!  ALLOCATES AN EMPTY STRUCTURE
!
      CALL BIEF_ALLVEC(1,S,'S     ',0,1,1,MESH)
!
!  ARRAYS CONTAINING THE VARIABLES WHICH WILL BE OUTPUT TO THE RESULT FILE:
!
      CALL BIEF_ALLVEC(1,U,'U     ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,V,'V     ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,H,'H     ',IELMH,1,1,MESH)
!
!  ARRAYS CONTAINING THE ADVECTED VARIABLES U, V, T, K AND EPSILON
!
      CALL BIEF_ALLVEC(1,UTILD,'UTILD ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,VTILD,'VTILD ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,HTILD,'HTILD ',IELMH,1,2,MESH)
!
!  ARRAYS CONTAINING THE VARIABLES U, V, H STORED AT TIME N
!
      CALL BIEF_ALLVEC(1,UN,'UN    ', IELMU,1,1,MESH )
      CALL BIEF_ALLVEC(1,VN,'VN    ', IELMU,1,1,MESH )
      CALL BIEF_ALLVEC(1,HN,'HN    ', IELMH,1,1,MESH )
!
!  ARRAYS STORING THE RELATIVE CHANGES
!
      CALL BIEF_ALLVEC(1,DH  ,'DH    ' , IELMH ,1,2 ,MESH)
      IF(IORDRU.EQ.2) THEN
        CALL BIEF_ALLVEC(1,DU  ,'DU    ' , IELMU , 1,2,MESH )
        CALL BIEF_ALLVEC(1,DV  ,'DV    ' , IELMU , 1,2,MESH )
      ELSE
        CALL BIEF_ALLVEC(1,DU  ,'DU    ' , 0     , 1,0,MESH )
        CALL BIEF_ALLVEC(1,DV  ,'DV    ' , 0     , 1,0,MESH )
      ENDIF
      IF(IORDRH.EQ.2) THEN
        CALL BIEF_ALLVEC(1,DHN ,'DHN   ' , IELMH , 1,2,MESH )
      ELSE
        CALL BIEF_ALLVEC(1,DHN ,'DHN   ' , 0     , 1,0,MESH )
      ENDIF
!
!  BLOCK OF THE UNKNOWNS IN PROPAG
!
      CALL ALLBLO(UNK,'UNK   ')
      CALL ADDBLO(UNK,DH)
      CALL ADDBLO(UNK, U)
      CALL ADDBLO(UNK, V)
!
!  BOUNDARY CONDITIONS ARRAYS (BOUNDARY ARRAYS)
!  FOR UBOR AND VBOR, SIZE 2 TO ALLOW VELOCITIES
!  OR FLOWRATES IMPOSED BY FUNCTION
!
      CALL BIEF_ALLVEC(1,UBOR    ,'UBOR  ',IELBU,2,1,MESH)
      CALL BIEF_ALLVEC(1,VBOR    ,'VBOR  ',IELBU,2,1,MESH)
      CALL BIEF_ALLVEC(1,HBOR    ,'HBOR  ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(1,AUBOR   ,'AUBOR ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,CFBOR   ,'CFBOR ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,UETUTA  ,'UETUTA',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(1,FLBOR   ,'FLBOR ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(1,FLBORTRA,'FLBTRA',IELBT,1,1,MESH)
!
      IF(TIDALTYPE.EQ.0) THEN
        CALL BIEF_ALLVEC(1,HBTIDE ,'HBTIDE',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,UBTIDE ,'UBTIDE',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,VBTIDE ,'VBTIDE',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMTIDE,'NUMTID',0,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,HBTIDE ,'HBTIDE',IELB1 ,1,1,MESH)
        CALL BIEF_ALLVEC(1,UBTIDE ,'UBTIDE',IELB1 ,1,1,MESH)
        CALL BIEF_ALLVEC(1,VBTIDE ,'VBTIDE',IELB1 ,1,1,MESH)
        CALL BIEF_ALLVEC(2,NUMTIDE,'NUMTID',IELB1 ,1,1,MESH)
      ENDIF
!
!  BLOCK OF DIRICHLET CONDITIONS TO PREPARE CALL TO DIRICH
!
      CALL ALLBLO(DIRBOR,'DIRBOR')
      CALL ADDBLO(DIRBOR,HBOR)
      CALL ADDBLO(DIRBOR,UBOR)
      CALL ADDBLO(DIRBOR,VBOR)
!
! BOTTOM ELEVATION ARRAY:
!
      CALL BIEF_ALLVEC(1,ZF,'ZF    ',IELMH,1,1,MESH)
!
! BOTTOM ELEVATION ARRAY BY ELEMENT (TIDAL FLATS)
!
      IF(MSK) THEN
        CALL BIEF_ALLVEC(1,ZFE,'ZFE   ',IELM0,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,ZFE,'ZFE   ',    0,1,0,MESH)
      ENDIF
!
! VISCOSITY : FOR NOW IN P1
!             BUT SIZE 2 TO CATER FOR ELDER'S MODEL
!
      IF(ITURB.EQ.2) THEN
        CALL BIEF_ALLVEC(1,VISC ,'VISC  ',IELM1,3,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,VISC ,'VISC  ',IELM1,1,1,MESH)
      ENDIF
!     BACKUP ARRAY FOR VISCOSITY
      IF(OPDVIT.EQ.2.OR.(NTRAC.GT.0.AND.OPDTRA.EQ.2)) THEN
        IF(ITURB.EQ.2) THEN
          CALL BIEF_ALLVEC(1,VISC_S,'VISC_S',IELM1,3,1,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,VISC_S,'VISC_S',IELM1,1,1,MESH)
        ENDIF
      ENDIF
!
!  FRICTION COEFFICIENT
!
      CALL BIEF_ALLVEC(1,CHESTR,'CHESTR',IELMU,1,1,MESH)
!
!  ARRAYS FOR ATMOSPHERIC AND INCIDENT WAVE CONDITIONS
!
      CALL BIEF_ALLVEC(1,C0    ,'C0    ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(1,COTOND,'COTOND',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(1,PATMOS,'PATMOS',IELMH,1,1,MESH)
      IF(ROVAR) THEN
        CALL BIEF_ALLVEC(1,RO,'RO    ',IELMH,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,RO,'RO    ',    0,1,0,MESH)
      ENDIF
!     WIND GIVEN IN P1
      IF(VENT) THEN
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',IELM1,1,1,MESH)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',IELM1,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,WINDX,'WINDX ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,WINDY,'WINDY ',    0,1,0,MESH)
      ENDIF
!
!  SOURCE TERM ARRAYS
!
      CALL BIEF_ALLVEC(1,FU,'FU    ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,FV,'FV    ',IELMU,1,2,MESH)
!
!  WAVE STRESSES
!
      IF(COUROU) THEN
        CALL BIEF_ALLVEC(1,FXWAVE,'FXWAVE',IELMU,1,2,MESH)
        CALL BIEF_ALLVEC(1,FYWAVE,'FYWAVE',IELMU,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,FXWAVE,'FXWAVE',0    ,1,0,MESH)
        CALL BIEF_ALLVEC(1,FYWAVE,'FYWAVE',0    ,1,0,MESH)
      ENDIF
!
!  POINTERS FOR THE MATRICES
!
      IELMHT = IELMH
!     AM1 USED FOR THE TRACERS
      IF(NTRAC.GT.0) IELMHT = MAX(IELMHT,IELMT)
      CALL BIEF_ALLMAT(AM1,'AM1   ',IELMHT,IELMHT,CFG,'Q','Q',MESH)
!
      TYP='Q'
      IF(ICONVF(1).NE.ADV_SUP    .AND.
     &   ICONVF(1).NE.ADV_NSC_NC .AND.
     &   3*(SLVPRO%PRECON/3).NE.SLVPRO%PRECON) TYP = 'S'
!
      IF(OPDVIT.EQ.2) TYP='Q'
!
      IELMUT = IELMU
!     AM2 AND AM3 USED FOR THE TRACERS
      IF(NTRAC.GT.0) THEN
        IELMUT = MAX(IELMU,IELMT)
        TYP='Q'
      ENDIF
!     AM2 AND AM3 MODIFIED FOR BOUSSINESQ
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        TYP='Q'
      ENDIF
      CALL BIEF_ALLMAT(AM2,'AM2   ',IELMUT,IELMUT,CFG,'Q',TYP,MESH)
      CALL BIEF_ALLMAT(AM3,'AM3   ',IELMUT,IELMUT,CFG,'Q',TYP,MESH)
!
!  BM1 AND BM2:
!
      CALL BIEF_ALLMAT(BM1,'BM1   ',IELMH,IELMU,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(BM2,'BM2   ',IELMH,IELMU,CFG,'Q','Q',MESH)
!
!  STORES CV1, BM1 AND BM2 FOR CORRECTION FOR CONTINUITY
!
      IF(CORCON.AND.SOLSYS.EQ.1) THEN
        CALL BIEF_ALLMAT(BM1S,'BM1S  ',IELMH,IELMU,CFG,'Q','Q',MESH)
        CALL BIEF_ALLMAT(BM2S,'BM2S  ',IELMH,IELMU,CFG,'Q','Q',MESH)
        CALL BIEF_ALLVEC(1,CV1S,'CV1S  ',IELMX,1,2,MESH)
      ELSE
        CALL BIEF_ALLMAT(BM1S,'BM1S  ',IELMH,IELMU,CFG,'0','0',MESH)
        CALL BIEF_ALLMAT(BM2S,'BM2S  ',IELMH,IELMU,CFG,'0','0',MESH)
        CALL BIEF_ALLVEC(1,CV1S,'CV1S  ',0,1,0,MESH)
      ENDIF
!
!  CM1 AND CM2:
!
      IELMC1 = IELMH
      IELMC2 = IELMU
!     CM2 USED FOR U IN SOME CASES
      IF(ICONVF(1).EQ.ADV_SUP.OR.ICONVF(1).EQ.ADV_NSC_NC) THEN
        IELMC1 = MAX(IELMC1,IELMU)
      ENDIF
      IF(EQUA(1:10).EQ.'BOUSSINESQ') IELMC1 = MAX(IELMC1,IELMU)
!
      CALL BIEF_ALLMAT(CM1,'CM1   ',IELMC1,IELMC2,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(CM2,'CM2   ',IELMC1,IELMC2,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(TM1,'TM1   ',IELMU ,IELMU ,CFG,'Q','Q',MESH)
!
!  BOUNDARY MATRIX
!
      IELBX = MAX(IELBU,IELBH,IELBT,IELBK,IELBE)
      CALL BIEF_ALLMAT(MBOR,'MBOR  ',IELBX,IELBX,CFGBOR,'Q','Q',MESH)
!
!  MATRICES A23 AND A32 USED FOR DIAGONAL-BLOCK PRECONDITIONING
!  OR FOR THE BOUSSINESQ EQUATIONS
!
      TYP = '0'
      IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) TYP = 'Q'
      IF(EQUA(1:10).EQ.'BOUSSINESQ') TYP = 'Q'
      CALL BIEF_ALLMAT(A23,'A23   ',IELMU,IELMU,CFG,TYP,TYP,MESH)
      CALL BIEF_ALLMAT(A32,'A32   ',IELMU,IELMU,CFG,TYP,TYP,MESH)
!
! BLOCK OF THE MATRICES IN PROPAG
!
      CALL ALLBLO(MAT,'MAT   ')
      CALL ADDBLO(MAT,AM1)
      CALL ADDBLO(MAT,BM1)
      CALL ADDBLO(MAT,BM2)
      CALL ADDBLO(MAT,CM1)
      CALL ADDBLO(MAT,AM2)
      CALL ADDBLO(MAT,A23)
      CALL ADDBLO(MAT,CM2)
      CALL ADDBLO(MAT,A32)
      CALL ADDBLO(MAT,AM3)
!
! WORKING ARRAY W1 (SIZE TO BE CHECKED)
!
!     NECESSARY MEMORY FOR W1 IN VALIDA
      MEMW1 = 9*NPOIN
!     FINITE VOLUMES
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        MEMW1 = MAX(MEMW1,9*NPOIN+3*NPTFR,2*MXPTVS*NPOIN)
      ENDIF
!     THIS MEMORY SPACE IS RESERVED IN THE FORM OF ONE
!     ARRAY P0 OF SIZE 2
      MEMW1 = MAX(3,1+MEMW1/BIEF_NBMPTS(IELM0,MESH))
      CALL BIEF_ALLVEC(1,W1,'W1    ',IELM0,MEMW1,1,MESH)
!
!_______________________________________________________________________
!
!  POINTERS FOR THE SECOND MEMBERS OF THE PROPAGATION STEP
!
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,CV1,'CV1   ',IELMX,1,2,MESH)
      CALL BIEF_ALLVEC(1,CV2,'CV2   ',IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,CV3,'CV3   ',IELMU,1,2,MESH)
!
!  BLOCK OF THE SECOND MEMBERS IN PROPAG
!
      CALL ALLBLO(RHS,'RHS   ')
      CALL ADDBLO(RHS,CV1)
      CALL ADDBLO(RHS,CV2)
      CALL ADDBLO(RHS,CV3)
!_______________________________________________________________________
!
!  POINTERS FOR THE SOURCE TERMS OF THE PROPAGATION STEP
!
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,SMH,'SMH   ',IELMX,1,2,MESH)
!_______________________________________________________________________
!
!  POINTERS FOR ADVECTION AND PROPAGATION FIELDS
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,UCONV,'UCONV ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VCONV,'VCONV ',IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,HPROP,'HPROP ',IELMH,1,1,MESH)
!_______________________________________________________________________
!
!  POINTERS FOR INTEGRAL OF THE BASES, IN PARALLEL, AND REVERSE
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,VOLU2D,'VOLU2D',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,V2DPAR,'V2DPAR',IELMH,1,1,MESH)
      CALL BIEF_ALLVEC(1,UNSV2D,'UNSV2D',IELMH,1,1,MESH)
!_______________________________________________________________________
!
!  POINTERS USED FOR LAGRANGIAN DRIFTS
!_______________________________________________________________________
!
      CALL BIEF_ALLVEC(1,XLAG  ,'XLAG  ',NPOIN*NLAG,1,0,MESH)
      CALL BIEF_ALLVEC(1,YLAG  ,'YLAG  ',NPOIN*NLAG,1,0,MESH)
      CALL BIEF_ALLVEC(1,SHPLAG,'SHPLAG',
     &            NPOIN*BIEF_NBPEL(IELM1,MESH)*NLAG,1,0,MESH)
!
!-----------------------------------------------------------------------
!
!  POINTERS FOR WORKING ARRAYS:
!
!-----------------------------------------------------------------------
!
!  NUMBER OF ARRAYS TO BE ALLOCATED : NTR
!           21 : POUR CGSTAB =3 X 7, 22 POUR CVDFTR (APPEL DE CVTRVF)
      NTR = 22
      IF(SLVPRO%SLV.EQ.7) NTR = MAX(NTR,6+6*SLVPRO%KRYLOV)
!     6 ADDITIONAL DIAGONALS TO STORE IN BLOCK-DIAGONAL PRECONDITIONING
      IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) NTR = NTR + 6
!
!  MAXIMUM USEFUL SIZE
!
      NTRT=0
      IF(NTRAC.GT.0) THEN
!       NTRT = 7
!       BECAUSE OF THE POSITION OF TRACERS IN VARSOR (WILL BE
!       THE SAME IN TB, USED BY VALIDA)
        NTRT = 31+NTRAC
        IF(SLVTRA%SLV.EQ.7) NTRT = MAX(2+2*SLVTRA%KRYLOV,NTRT)
        NTR = MAX(NTR,NTRT)
      ENDIF
      NTRKE=0
      IF(ITURB.EQ.3) THEN
        NTRKE=7
        IF(SLVK%SLV.EQ.7) NTRKE = MAX(NTRKE,2+2*SLVK%KRYLOV)
        NTR  = MAX(NTR,NTRKE)
      ENDIF
!
!  ALLOCATES NTR WORKING ARRAYS (SIZE: THE MAXIMUM NUMBER OF
!                                      DEGREES OF FREEDOM)
!
!     TB WILL CONTAIN ARRAYS T1,T2,...
!
      CALL ALLBLO(TB ,'TB    ')
!
      CALL BIEF_ALLVEC_IN_BLOCK(TB,NTR,1,'TB    ',IELMX,1,2,MESH)
!
!     ALIAS FOR THE FIRST 22 WORKING ARRAYS OF THE BLOCK: TB
!
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
      T8 =>TB%ADR( 8)%P
      T9 =>TB%ADR( 9)%P
      T10=>TB%ADR(10)%P
      T11=>TB%ADR(11)%P
      T12=>TB%ADR(12)%P
      T13=>TB%ADR(13)%P
      T14=>TB%ADR(14)%P
      T15=>TB%ADR(15)%P
      T16=>TB%ADR(16)%P
      T17=>TB%ADR(17)%P
      T18=>TB%ADR(18)%P
      T19=>TB%ADR(19)%P
      T20=>TB%ADR(20)%P
      T21=>TB%ADR(21)%P
      T22=>TB%ADR(22)%P
!
!  ALLOCATES WORKING ARRAYS (SIZE: THE MAXIMUM NUMBER OF ELEMENTS)
!
!
      CALL BIEF_ALLVEC(1,TE1,'TE1   ',IELM0,1,1,MESH)
      CALL BIEF_ALLVEC(1,TE2,'TE2   ',IELM0,1,1,MESH)
      CALL BIEF_ALLVEC(1,TE3,'TE3   ',IELM0,1,1,MESH)
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
!       PIECE-WISE LINEAR FREE SURFACE
        CALL BIEF_ALLVEC(1,ZFLATS, 'ZFLATS',IELM0,3,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,ZFLATS, 'ZFLATS',    0,1,1,MESH)
      ENDIF
      IF(OPTBAN.EQ.3) THEN
        CALL BIEF_ALLVEC(1,TE4,'TE4   ',IELM0,1,1,MESH)
        CALL BIEF_ALLVEC(1,TE5,'TE5   ',IELM0,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TE4,'TE4   ',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,TE5,'TE5   ',    0,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
! !JAJ #### IF REQUIRED, WE READ HERE THE INPUT SECTIONS FILE
!      AND MODIFY NCP AND CTRLSC(1:NCP) ACCORDINGLY IN READ_SECTIONS
!
      IF (TRIM(T2D_FILES(T2DSEC)%NAME)/='') THEN
        WRITE(LU,*)
     &   'POINT_TELEMAC2D: SECTIONS DEFINED IN THE SECTIONS INPUT FILE'
        CALL READ_SECTIONS_TELEMAC2D
      ELSE ! THE EARLIER WAY OF DOING THINGS
        IF (NCP.NE.0) WRITE(LU,*)
     &   'POINT_TELEMAC2D: SECTIONS DEFINED IN THE PARAMETER FILE'
      ENDIF
!
!     BLOCK OF MASKS FOR THE COMPUTATION OF FLUXES ACCROSS SECTIONS
!     ONLY WITH COMPATIBLE FLUXES
!
      CALL ALLBLO(MSKSEC,'MSKSEC')
      IF(NCP.GT.1.AND.COMFLU) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(MSKSEC,NCP/2,1,'MSKS  ',
     &                            IELM0,1,1,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
! POINTERS OF THE MASKS
!
!     BLOCK OF THE MASKS FOR BOUNDARY CONDITIONS
!     (PROPAGATION)
!
      CALL ALLBLO(MASK,'MASK  ')
      CALL BIEF_ALLVEC_IN_BLOCK(MASK,11,1,'MASK  ',IELBH,1,2,MESH)
!
      IF(MSK) THEN
        CALL BIEF_ALLVEC(1,MASKEL,'MASKEL',IELM0,1,1,MESH)
        CALL BIEF_ALLVEC(1,MASKPT,'MASKPT',IELMX,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MASKEL,'MASKEL',    0,1,0,MESH)
        CALL BIEF_ALLVEC(1,MASKPT,'MASKPT',    0,1,0,MESH)
      ENDIF
!
!  ADDITIONAL ARRAYS IF THERE ARE TRACERS
!
      CALL ALLBLO(T      ,'T     ')
      CALL ALLBLO(TTILD  ,'TTILD ')
      CALL ALLBLO(TN     ,'TN    ')
      CALL ALLBLO(TEXP   ,'TEXP  ')
      CALL ALLBLO(TIMP   ,'TIMP  ')
      CALL ALLBLO(TSCEXP ,'TSCEXP')
      CALL ALLBLO(VISCT  ,'VISCT ')
      CALL ALLBLO(MASKTR ,'MASKTR')
      CALL ALLBLO(TBOR   ,'TBOR  ')
      CALL ALLBLO(ATBOR  ,'ATBOR ')
      CALL ALLBLO(BTBOR  ,'BTBOR ')
      CALL ALLBLO(LITBOR ,'LITBOR')
      IF(NTRAC.GT.0) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(T     ,NTRAC,1,'T     ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TTILD ,NTRAC,1,'TTILD ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TN    ,NTRAC,1,'TN    ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TEXP  ,NTRAC,1,'TEXP  ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TIMP  ,NTRAC,1,'TIMP  ',
     &                            IELMT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TSCEXP,NTRAC,1,'TSCEXP',
     &                            IELMT,1,1,MESH)
        IF(ITURB.EQ.2) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,NTRAC,1,'VISCT ',
     &                              IELMT,3,1,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,NTRAC,1,'VISCT ',
     &                              IELMT,1,1,MESH)
        ENDIF
        CALL BIEF_ALLVEC_IN_BLOCK(MASKTR,5,1,'MSKTR ',IELBH,1,2,MESH)
        IF(THOMFR) THEN
!         SECOND DIMENSION USED AS A WORKING ARRAY
!         IN THOMPS
          CALL BIEF_ALLVEC_IN_BLOCK(TBOR,NTRAC,1,'TBOR  ',
     &                              IELBT,2,1,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(TBOR,NTRAC,1,'TBOR  ',
     &                              IELBT,1,1,MESH)
        ENDIF
        CALL BIEF_ALLVEC_IN_BLOCK(ATBOR  ,NTRAC,1,'ATBOR ',
     &                            IELBT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(BTBOR  ,NTRAC,1,'BTBOR ',
     &                            IELBT,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(LITBOR ,NTRAC,2,'LITBOR',
     &                            IELBT,1,1,MESH)
      ELSE
!       AT LEAST ONE ELEMENT IN BLOCKS, NOT NTRAC
        CALL BIEF_ALLVEC_IN_BLOCK(T     ,1,1,'T     ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TTILD ,1,1,'TTILD ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TN    ,1,1,'TN    ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TEXP  ,1,1,'TEXP  ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TIMP  ,1,1,'TIMP  ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TSCEXP,1,1,'TSCEXP',0,1,0,MESH)
        IF(ITURB.EQ.2) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,1,1,'VISCT ',0,3,0,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(VISCT,1,1,'VISCT ',0,1,0,MESH)
        ENDIF
        CALL BIEF_ALLVEC_IN_BLOCK(MASKTR ,4,1,'MSKTR ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(TBOR   ,1,1,'TBOR  ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(ATBOR  ,1,1,'ATBOR ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(BTBOR  ,1,1,'BTBOR ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(LITBOR ,1,2,'LITBOR',0,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  FRICTION COEFFICIENT CF
!
      CALL BIEF_ALLVEC(1,CF    ,'CF    ',IELMU,1,1,MESH)
!
!  DATA FOR FRICTION SET PER ZONE
!
!  FRICTION LAW USED
!
      CALL BIEF_ALLVEC(2,NKFROT,'NKFROT',IELMU,1,1,MESH)
!
!  CHESTR ON THE BOUNDARY
!
      CALL BIEF_ALLVEC(1,CHBORD,'CHBORD',IELBT,1,1,MESH)
!
      IF(FRICTB) THEN
         ALLOCATE(FRTAB%ADR(NZONMX))
         DO I=1,NZONMX
           ALLOCATE(FRTAB%ADR(I)%P)
         ENDDO
         CALL BIEF_ALLVEC(2,KFROPT,'KFROPT',IELMU,1,1,MESH)
         CALL BIEF_ALLVEC(1,NDEFMA,'NDEFMA',IELMU,1,1,MESH)
         IF(LINDNER) THEN
           CALL BIEF_ALLVEC(1,LINDDP,'LINDDP',IELMU,1,1,MESH)
           CALL BIEF_ALLVEC(1,LINDSP,'LINDSP',IELMU,1,1,MESH)
         ELSE
           CALL BIEF_ALLVEC(1,LINDDP,'LINDDP',0,1,0,MESH)
           CALL BIEF_ALLVEC(1,LINDSP,'LINDSP',0,1,0,MESH)
         ENDIF
         CALL BIEF_ALLVEC(1,NDEF_B,'NDEF_B',IELBT,1,1,MESH)
         CALL BIEF_ALLVEC(2,KFRO_B,'KFRO_B',IELBT,1,1,MESH)
      ELSE
         CALL BIEF_ALLVEC(2,KFROPT,'KFROPT',0,1,0,MESH)
         CALL BIEF_ALLVEC(1,NDEFMA,'NDEFMA',0,1,0,MESH)
         CALL BIEF_ALLVEC(1,LINDDP,'LINDDP',0,1,0,MESH)
         CALL BIEF_ALLVEC(1,LINDSP,'LINDSP',0,1,0,MESH)
         CALL BIEF_ALLVEC(1,NDEF_B,'NDEF_B',0,1,0,MESH)
         CALL BIEF_ALLVEC(2,KFRO_B,'KFRO_B',0,1,0,MESH)
      ENDIF
!
!  END OF DATA FOR FRICTION SET PER ZONE
!
!  ADDITIONAL ARRAY IF THE K-EPSILON MODEL IS USED
!
      IF(ITURB.EQ.3) THEN
        CALL BIEF_ALLVEC(1,AK     ,'AK    ',IELMK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EP     ,'EP    ',IELME,1,1,MESH)
        CALL BIEF_ALLVEC(1,AKN    ,'AKN   ',IELMK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EPN    ,'EPN   ',IELME,1,1,MESH)
        CALL BIEF_ALLVEC(1,AKTILD ,'AKTILD',IELMK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EPTILD ,'EPTILD',IELME,1,1,MESH)
        CALL BIEF_ALLVEC(1,KBOR   ,'KBOR  ',IELBK,1,1,MESH)
        CALL BIEF_ALLVEC(1,EBOR   ,'EBOR  ',IELBE,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,AK     ,'AK    ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EP     ,'EP    ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,AKN    ,'AKN   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EPN    ,'EPN   ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,AKTILD ,'AKTILD',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EPTILD ,'EPTILD',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,KBOR   ,'KBOR  ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,EBOR   ,'EBOR  ',0,1,0,MESH)
      ENDIF
!
      CALL BIEF_ALLVEC(1,UDEL   ,'UDEL  ',    IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,VDEL   ,'VDEL  ',    IELMU,1,1,MESH)
      CALL BIEF_ALLVEC(1,DM1    ,'DM1   ',    IELMU,1,2,MESH)
      CALL BIEF_ALLVEC(1,ZCONV  ,'ZCONV ',       10,3,1,MESH)
      CALL BIEF_ALLVEC(1,FLODEL ,'FLODEL',MESH%NSEG,1,0,MESH)
      CALL BIEF_ALLVEC(1,FLULIM ,'FLULIM',MESH%NSEG,1,0,MESH)
!
!-----------------------------------------------------------------------
!
! ALLOCATES THE BLOCKS
!
!     FUNCTIONS TO ADVECT BY CHARACTERISTICS
!
      CALL ALLBLO(FN    , 'FN    ')
      CALL ALLBLO(F     , 'F     ')
      CALL ALLBLO(FTILD , 'FTILD ')
      CALL ALLBLO(FNCAR , 'FNCAR ')
!
      CALL ADDBLO(FN,UN)
      CALL ADDBLO(FN,VN)
      CALL ADDBLO(FN,HN)
      CALL ADDBLO(F ,U )
      CALL ADDBLO(F ,V )
      CALL ADDBLO(F ,H )
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL ADDBLO(FN   ,TN%ADR(ITRAC)%P   )
          CALL ADDBLO(F    ,T%ADR(ITRAC)%P    )
        ENDDO
      ENDIF
      IF(ITURB.EQ.3) THEN
        CALL ADDBLO(FN    ,AKN)
        CALL ADDBLO(FN    ,EPN)
        CALL ADDBLO(F     ,AK )
        CALL ADDBLO(F     ,EP )
      ENDIF
!
!-----------------------------------------------------------------------
!
!     WITH FINITE VOLUMES OR KINETIC SCHEMES ADVECTION IS DONE
!     IN VOLFIN
!
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
        IF(CONVV(1).AND.ICONVF(1).EQ.ADV_CAR) THEN
          CALL ADDBLO(FTILD,UTILD)
          CALL ADDBLO(FTILD,VTILD)
          CALL ADDBLO(FNCAR,UN   )
          CALL ADDBLO(FNCAR,VN   )
        ENDIF
        IF(CONVV(3).AND.NTRAC.GT.0.AND.ICONVF(3).EQ.ADV_CAR) THEN
          DO ITRAC=1,NTRAC
            CALL ADDBLO(FTILD,TTILD%ADR(ITRAC)%P)
            CALL ADDBLO(FNCAR,TN%ADR(ITRAC)%P)
          ENDDO
        ENDIF
      ENDIF
!
      IF(CONVV(4).AND.ITURB.EQ.3.AND.ICONVF(4).EQ.ADV_CAR) THEN
        CALL ADDBLO(FTILD,AKTILD)
        CALL ADDBLO(FTILD,EPTILD)
        CALL ADDBLO(FNCAR,AKN   )
        CALL ADDBLO(FNCAR,EPN   )
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED TO FOLLOW THE DRIFTS
!
!_______________________________________________________________________
!
      IF(NFLOT.NE.0) THEN
        CALL BIEF_ALLVEC(1,XFLOT ,'XFLOT ',NITFLO*NFLOT,1,0,MESH)
        CALL BIEF_ALLVEC(1,YFLOT ,'YFLOT ',NITFLO*NFLOT,1,0,MESH)
        CALL BIEF_ALLVEC(1,SHPFLO,'SHPFLO',
     &                     BIEF_NBPEL(IELM1,MESH)*NFLOT,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,XFLOT ,'XFLOT ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,YFLOT ,'YFLOT ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,SHPFLO,'SHPFLO',0,1,0,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED FOR WEIRS
!
!-----------------------------------------------------------------------
!
      IF(NWEIRS.NE.0) THEN
!       IN FACT ARRAYS (NWEIRS,NPSMAX) OR NPOIN>NWEIRS*NPSMAX
        CALL BIEF_ALLVEC(1,ZDIG  ,'ZDIG  ',IELM1,1,1,MESH)
        CALL BIEF_ALLVEC(1,PHIDIG,'PHIDIG',IELM1,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,ZDIG  ,'ZDIG  ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,PHIDIG,'PHIDIG',0,1,0,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!  ARRAYS AT THE USER'S DISPOSAL
!
      CALL ALLBLO(PRIVE ,'PRIVE ')
!
      IF(NPRIV.GT.0) THEN
!       THESE ARRAYS MUST EXIST BUT CAN BE EMPTY
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,NPRIV,1,'PRIV  ',IELMX,1,2,MESH)
      ENDIF
!     AT LEAST 4 ARRAYS ARE REQUIRED BUT THEY CAN BE EMPTY
      IF(NPRIV.LT.4) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,4-NPRIV,1,'PRIV  ',0,1,2,MESH)
      ENDIF
!
!     ALIAS FOR THE FIRST 4 'PRIVE' ARRAYS
!
      PRIVE1 => PRIVE%ADR(1)%P%R
      PRIVE2 => PRIVE%ADR(2)%P%R
      PRIVE3 => PRIVE%ADR(3)%P%R
      PRIVE4 => PRIVE%ADR(4)%P%R
!
!  BLOCK OF THE CLANDESTINE VARIABLES
!
      CALL ALLBLO(VARCL,'VARCL ')
      CALL BIEF_ALLVEC_IN_BLOCK(VARCL,NVARCL,1,'CL    ',IELMX,1,2,MESH)
!
!     INITIALISES AT 0
!
!
      IF(NVARCL.GT.0) THEN
        DO I=1,NVARCL
          CALL OS('X=C     ',VARCL%ADR(I)%P,VARCL%ADR(I)%P,
     &                       VARCL%ADR(I)%P,0.D0)
        ENDDO
      ENDIF
!
!_______________________________________________________________________
!
!                         * INTEGER ARRAYS *
!_______________________________________________________________________
!
      IF(MSK) THEN
        CALL BIEF_ALLVEC(2,IFAMAS,'IFAMAS',
     &                   IELM0,BIEF_NBFEL(IELM0,MESH),1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,IFAMAS,'IFAMAS',0,1,0,MESH)
      ENDIF
      CALL BIEF_ALLVEC(2,LIUBOR,'LIUBOR',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIVBOR,'LIVBOR',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIHBOR,'LIHBOR',IELBH,1,1,MESH)
!     CLU, CLV AND CLH ARE WORKING ARRAYS IN PROPIN
      CALL BIEF_ALLVEC(2,CLU            ,'CLU   ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,CLV            ,'CLV   ',IELBU,1,1,MESH)
      CALL BIEF_ALLVEC(2,CLH            ,'CLH   ',IELBH,1,1,MESH)
      CALL BIEF_ALLVEC(2,BOUNDARY_COLOUR,'BNDCOL',IELB1,1,1,MESH)
!
      CALL BIEF_ALLVEC(2,NUMLIQ,'NUMLIQ',IELB1,1,1,MESH)
      IF(ITURB.EQ.3) THEN
        CALL BIEF_ALLVEC(2,LIMKEP,'LIMKEP',IELB1,2,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,LIMKEP,'LIMKEP',    0,2,0,MESH)
      ENDIF
      CALL BIEF_ALLVEC(2,LIMPRO,'LIMPRO',MAX(IELBH,IELBU),6,1,MESH)
      CALL BIEF_ALLVEC(2,LIMTRA,'LIMTRA',IELBT,1,1,MESH)
      CALL BIEF_ALLVEC(2,SECMOU,'SECMOU',IELM0,1,1,MESH)
!
!     INTEGER WORKING ARRAY (MINIMUM SIZE NELEM)
!
      IF(IELMX.GT.11) THEN
        CALL BIEF_ALLVEC(2,IT1,'IT1   ',IELMX,1,2,MESH)
        CALL BIEF_ALLVEC(2,IT2,'IT2   ',IELMX,1,2,MESH)
        CALL BIEF_ALLVEC(2,IT3,'IT3   ',IELMX,1,2,MESH)
        CALL BIEF_ALLVEC(2,IT4,'IT4   ',IELMX,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,IT1,'IT1   ',   10,1,2,MESH)
        CALL BIEF_ALLVEC(2,IT2,'IT2   ',   10,1,2,MESH)
        CALL BIEF_ALLVEC(2,IT3,'IT3   ',   10,1,2,MESH)
        CALL BIEF_ALLVEC(2,IT4,'IT4   ',   10,1,2,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED TO FOLLOW THE DRIFTS
!
!_______________________________________________________________________
!
!     IF THERE ARE NO DRIFTS, NO TEST ON NFLOT
!     IF NFLOT IS 0, THE VECTORS WILL HAVE NO SIZE
      CALL BIEF_ALLVEC(2,DEBFLO,'DEBFLO',NFLOT         ,1,0,MESH)
      CALL BIEF_ALLVEC(2,FINFLO,'FINFLO',NFLOT         ,1,0,MESH)
      CALL BIEF_ALLVEC(2,ELTFLO,'ELTFLO',NFLOT         ,1,0,MESH)
      CALL BIEF_ALLVEC(2,IKLFLO,'IKLFLO',NFLOT*NITFLO*3,1,0,MESH)
!
!_______________________________________________________________________
!
!  ARRAYS USED FOR LAGRANGIAN DRIFTS
!
!-----------------------------------------------------------------------
!
      IF(NLAG.NE.0) THEN
        CALL BIEF_ALLVEC(2,DEBLAG,'DEBLAG',NLAG      ,1,0,MESH)
        CALL BIEF_ALLVEC(2,FINLAG,'FINLAG',NLAG      ,1,0,MESH)
        CALL BIEF_ALLVEC(2,ELTLAG,'ELTLAG',NLAG*NPOIN,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,DEBLAG,'DEBLAG',0         ,1,0,MESH)
        CALL BIEF_ALLVEC(2,FINLAG,'FINLAG',0         ,1,0,MESH)
        CALL BIEF_ALLVEC(2,ELTLAG,'ELTLAG',0         ,1,0,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED FOR WEIRS
!
!-----------------------------------------------------------------------
!
!     NUMDIG (2, NWEIRS, NPSMAX) IN ACTUAL FACT
!     NPOIN IS GREATER THAN NWEIRS * NPSMAX, WHICH ARE BOTH
!     READ IN THE SINGULARITY FILES
      IF(NWEIRS.NE.0) THEN
        CALL BIEF_ALLVEC(2,NUMDIG,'NUMDIG',2*NPOIN,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,NUMDIG,'NUMDIG',    0  ,1,0,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS USED FOR THE ZONE NUMBERS
!
!-----------------------------------------------------------------------
!
      IF(DEFZON) THEN
        CALL BIEF_ALLVEC(2,ZONE,'ZONE  ',IELM1,1,1,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,ZONE,'ZONE  ',0    ,1,0,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!  ARRAYS NOT COMMON TO ALL TYPES OF SOLVED EQUATIONS
!_______________________________________________________________________
!
      CALL ALLBLO(SMTR     ,'SMTR  ')
      CALL ALLBLO(FLUXT    ,'FLUXT ')
      CALL ALLBLO(FLUXTEMP ,'FLUXTE')
      CALL ALLBLO(FLUHBTEMP,'FLUHBT')
      CALL ALLBLO(FLUHBOR  ,'FLUHB ')
      CALL ALLBLO(HT       ,'HT    ')
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL BIEF_ALLVEC(1,QU       ,'QU    ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,QV       ,'QV    ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,HSTOK    ,'HSTOK ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,HCSTOK   ,'HCSTOK',2    ,MESH%NSEG,0,MESH)
        CALL BIEF_ALLVEC(1,SMTR     ,'SMTR  ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(2,LOGFR    ,'LOGFR ',IELM1,1        ,1,MESH)
        CALL BIEF_ALLVEC(1,HC       ,'HC    ',2    ,MESH%NSEG,0,MESH)
        CALL BIEF_ALLVEC(1,DSZ      ,'DSZ   ',2    ,MESH%NSEG,0,MESH)
        CALL BIEF_ALLVEC(1,FLUX_OLD ,'FLUOLD',IELM1,3        ,1,MESH)
        IF(NTRAC.GT.0) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXT    ,NTRAC,1,'FLUXT ',
     &                              MESH%NSEG,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXTEMP ,NTRAC,1,'FLUXTE',
     &                              MESH%NSEG,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUHBTEMP,NTRAC,1,'FLUHBT',
     &                              IELBH    ,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUHBOR  ,NTRAC,1,'FLUHB ',
     &                              IELBH    ,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(HT       ,NTRAC,1,'HT    ',
     &                              IELM1    ,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(SMTR     ,NTRAC,1,'SMTR  ',
     &                              IELM1    ,1,1,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXT    ,1,1,'FLUXT ',0,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUXTEMP ,1,1,'FLUXTE',0,1,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUHBTEMP,1,1,'FLUHBT',0,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(FLUHBOR  ,1,1,'FLUHB ',0,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(HT       ,1,1,'HT    ',0,1,1,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(SMTR     ,1,1,'SMTR  ',0,1,1,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(1,QU       ,'QU    ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,QV       ,'QV    ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,HSTOK    ,'HSTOK ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,HCSTOK   ,'HCSTOK',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(2,LOGFR    ,'LOGFR ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,HC       ,'HC    ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC(1,DSZ      ,'DSZ   ',0 , 1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUXT    ,1,1,'FLUXT ',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUXTEMP ,1,1,'FLUXTE',0,1,0,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUHBTEMP,1,1,'FLUHBT',0,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(FLUHBOR  ,1,1,'FLUHB ',0,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(HT       ,1,1,'HT    ',0,1,1,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(SMTR     ,1,1,'SMTR  ',0,1,1,MESH)
        CALL BIEF_ALLVEC(1,FLUX_OLD ,'FLUOLD',0 , 1,0,MESH)
      ENDIF
!
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        CALL BIEF_ALLVEC(1,H0  ,'H0    ',IELMH,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,H0  ,'H0    ',0    ,1,0 ,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!    FOR MAX FREE SURFACE ELEVATION, MAX SPEEDS
!    AND CORRESPONDING TIMES
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        CALL BIEF_ALLVEC(1,MAXZ,'MAXZ  ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MAXZ,'MAXZ  ',0    ,1,0 ,MESH)
      ENDIF
      IF(SORLEO(28).OR.SORIMP(28)) THEN
        CALL BIEF_ALLVEC(1,TMAXZ,'TMAXZ ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TMAXZ,'TMAXZ ',0    ,1,0 ,MESH)
      ENDIF
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        CALL BIEF_ALLVEC(1,MAXV,'MAXV  ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,MAXV,'MAXV  ',0    ,1,0 ,MESH)
      ENDIF
      IF(SORLEO(30).OR.SORIMP(30)) THEN
        CALL BIEF_ALLVEC(1,TMAXV,'TMAXV ',IELM1,1,1 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,TMAXV,'TMAXV ',0    ,1,0 ,MESH)
      ENDIF
!
!    FOR FOURIER ANALYSES
!
      CALL ALLBLO(AMPL,'AMPL  ')
      CALL ALLBLO(PHAS,'PHAS  ')
      IF(NPERIAF.GT.0) THEN
        CALL BIEF_ALLVEC_IN_BLOCK(AMPL,NPERIAF,1,'AMPL  ',
     &                            IELM1,1,2,MESH)
        CALL BIEF_ALLVEC_IN_BLOCK(PHAS,NPERIAF,1,'PHAS  ',
     &                            IELM1,1,2,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
! COMPUTES THE BLOCK WHICH CONNECTS A VARIABLE NAME
! TO ITS ARRAY
!
      CALL ALLBLO(VARSOR ,'VARSOR')
! 01
      CALL ADDBLO(VARSOR,U)
! 02
      CALL ADDBLO(VARSOR,V)
! 03
      CALL ADDBLO(VARSOR,FU)
! 04
      CALL ADDBLO(VARSOR,H)
! 05
      CALL ADDBLO(VARSOR,FV)
! 06
      CALL ADDBLO(VARSOR,ZF)
! 07
      CALL ADDBLO(VARSOR,T2)
! 08
      CALL ADDBLO(VARSOR,T3)
! 09  OLD TRACER
!     REPEATED HERE BUT NOT USED; MOVED ELSEWHERE
      CALL ADDBLO(VARSOR,T%ADR(1)%P)
! 10
      CALL ADDBLO(VARSOR,AK)
! 11
      CALL ADDBLO(VARSOR,EP)
! 12
      CALL ADDBLO(VARSOR,VISC)
! 13
      CALL ADDBLO(VARSOR,T4)
! 14
      CALL ADDBLO(VARSOR,T5)
! 15
      CALL ADDBLO(VARSOR,T6)
! 16
      CALL ADDBLO(VARSOR,WINDX)
! 17
      CALL ADDBLO(VARSOR,WINDY)
! 18
      CALL ADDBLO(VARSOR,PATMOS)
! 19
      CALL ADDBLO(VARSOR,CHESTR)
! 20
      CALL ADDBLO(VARSOR,T7)
! 21
      CALL ADDBLO(VARSOR,T8)
! 22
      CALL ADDBLO(VARSOR,T9)
! 23
      CALL ADDBLO(VARSOR,PRIVE%ADR(1)%P)
! 24
      CALL ADDBLO(VARSOR,PRIVE%ADR(2)%P)
! 25
      CALL ADDBLO(VARSOR,PRIVE%ADR(3)%P)
! 26
      CALL ADDBLO(VARSOR,PRIVE%ADR(4)%P)
! 27
      CALL ADDBLO(VARSOR,MAXZ)
! 28
      CALL ADDBLO(VARSOR,TMAXZ)
! 29
      CALL ADDBLO(VARSOR,MAXV)
! 30
      CALL ADDBLO(VARSOR,TMAXV)
! 31  FRICTION VELOCITY
      CALL ADDBLO(VARSOR,T7)
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL ADDBLO(VARSOR,T%ADR(ITRAC)%P)
        ENDDO
      ENDIF
!
!     FOURIER ANALYSIS
!
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
!         OUTPUT VARIABLES (TO BE CHECKED)
          SORLEO(32+NTRAC+2*(I-1))=.TRUE.
          SORLEO(33+NTRAC+2*(I-1))=.TRUE.
!         END OF OUTPUT VARIABLES (TO BE CHECKED)
          CALL ADDBLO(VARSOR,AMPL%ADR(I)%P)
          CALL ADDBLO(VARSOR,PHAS%ADR(I)%P)
        ENDDO
      ENDIF
!
!     OTHER POSSIBLE VARIABLES ADDED BY USER
!
      J=32+NTRAC+2*NPERIAF
900   CONTINUE
      IF(SORLEO(J).OR.SORIMP(J)) THEN
        IF(NPRIV.LT.J-27-NTRAC-2*NPERIAF) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'POINT : AUGMENTER LE NOMBRE'
            WRITE(LU,*) '        DE TABLEAUX PRIVES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'POINT : NUMBER OF PRIVATE ARRAYS'
            WRITE(LU,*) '        TOO SMALL'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL ADDBLO(VARSOR,PRIVE%ADR(J-27-NTRAC-2*NPERIAF)%P)
        J=J+1
        IF(J.LE.MAXVAR) GO TO 900
      ENDIF
!
!     CLANDESTINE VARIABLES
!
      IF(VARCL%N.NE.0) THEN
        DO I=1,VARCL%N
          CALL ADDBLO(VARSOR,VARCL%ADR(I)%P)
          SORLEO(J+I-1)=.TRUE.
          TEXTE(J+I-1)=VARCLA(I)
        ENDDO
      ENDIF
!
!=======================================================================
!
! WRITES OUT TO LISTING :
!
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,22)
         IF(LNG.EQ.2) WRITE(LU,23)
      ENDIF
22    FORMAT(1X,///,21X,'****************************************',/,
     &21X,              '* FIN DE L''ALLOCATION DE LA MEMOIRE  : *',/,
     &21X,              '****************************************',/)
23    FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
