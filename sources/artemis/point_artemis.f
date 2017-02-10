!                    ************************
                     SUBROUTINE POINT_ARTEMIS
!                    ************************
!
!
!***********************************************************************
! ARTEMIS   V7P2                                     Nov 2016
!***********************************************************************
!
!brief    ALLOCATES STRUCTURES.
!
!history  J-M HERVOUET (LNH)
!+        24/04/1997
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
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
!history  C.PEYRARD(EDF)
!+        2011
!+        V6P1
!+        NEW WORK TABLES
!
!history  C.PEYRARD(EDF)
!+        2012
!+        V6P2
!+        VARIABLES FOR INCIDENT POTENTIAL ADDED
!
!history  C.PEYRARD(EDF)
!+        2013
!+        V6P3
!+        VARIABLES FOR CURRENT ADDED
!
!history  C.PEYRARD(EDF)
!+        2014
!+        V7P0
!+        VARIABLES FOR AUTOMATIC TETAP CALCULATION ADDED
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!history  N.DURAND (HRW)
!+        November 2016
!+        V7P2
!+   Allocations for TETAPS, ALFAPS and HBS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER ISYM,MEMW1,INOSYM,ISTOP,NTR,NTRBD
      INTEGER, TARGET :: TMP=1029
      INTEGER CFG(2),CFGBOR(2)
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
!  PARAMETERS DEPENDING ON THE TYPE OF ELEMENT: (SEE ALSO NDP)
!  ISYM AND INOSYM ARE USED TO DIMENSION THE ARRAYS CONTAINING THE
!                   EXTRADIAGONAL PART OF THE MATRICES.
!
      ISYM=3
      INOSYM=6
      IELM = 11
      IELM0 = 10
      IELMB = 1
      IELMB0 = 0
!
! TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
!
      CFG(1) = OPTASS
      CFG(2) = PRODUC
!     CFG IS IMPOSED FOR BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
      EQUA = 'ARTEMIS'
!
!=======================================================================
!
!     ALLOCATES THE MESH STRUCTURE
!
      CALL ALMESH(MESH,'MESH  ',IELM,SPHERI,CFG,
     &            ART_FILES(ARTGEO)%FMT,ART_FILES(ARTGEO)%LU,
     &            EQUA,0)
!
!
!     ALIAS FOR CERTAIN COMPONENTS OF MESH
!
      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
!
      TMP=MESH%NPTFR
      NELEM => MESH%NELEM
      NELMAX=> MESH%NELMAX
      NPTFR => MESH%NPTFR
      NPTFRX=> MESH%NPTFRX
      DIM1  => MESH%DIM1
      TYPELM=> MESH%TYPELM
      NPOIN => MESH%NPOIN
      NPMAX => MESH%NPMAX
      MXPTVS=> MESH%MXPTVS
      MXELVS=> MESH%MXELVS
      LV    => MESH%LV
!      WRITE(*,*) 'FRONTIERE',NPTFR,MESH%NPTFR
!
!-----------------------------------------------------------------------
!
!                     ******************
!                     *   REAL ARRAYS  *
!                     ******************
!
!-----------------------------------------------------------------------
!
!
! POTENTIAL
!
      CALL BIEF_ALLVEC(1,PHIR,'PHIR  ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PHII,'PHII  ',IELM, 1 , 2 ,MESH)
!
! WATER DEPTH AT REST
!
      CALL BIEF_ALLVEC(1,H,'H     ',IELM, 1 , 2 ,MESH)
!
! WAVE NUMBER
      CALL BIEF_ALLVEC(1,K,'K     ',IELM, 1 , 2 ,MESH)
!
! PHASE AND GROUP VELOCITIES
      CALL BIEF_ALLVEC(1,C,'C     ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CG,'CG    ',IELM, 1 , 2 ,MESH)
!
! WAVE HEIGHT AND PHASE
      CALL BIEF_ALLVEC(1,HHO,'HHO   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PHAS,'PHAS  ',IELM, 1 , 2 ,MESH)
!
! VELOCITIES
      CALL BIEF_ALLVEC(1,U0,'U0    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,V0,'V0    ',IELM, 1 , 2 ,MESH)
!
! AVERAGES OF THE SINES AND COSINES OF THE WAVE DIRECTION
      CALL BIEF_ALLVEC(1,MCOS,'MCOS  ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MSIN,'MSIN  ',IELM, 1 , 2 ,MESH)
! WAVE INCIDENCE
      CALL BIEF_ALLVEC(1,INCI,'INCI  ',IELM, 1 , 2 ,MESH)
!
! FREE SURFACE AND BOTTOM ELEVATION
      CALL BIEF_ALLVEC(1,S,'S     ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ZF,'ZF    ',IELM, 1 , 2 ,MESH)
!
! FRICTION COEFFICIENT (VARIABLE IN SPACE)
      CALL BIEF_ALLVEC(1,FW,'FW    ',IELM, 1 , 2 ,MESH)
!
! WAVE HEIGHT (RANDOM SEAS)
! ARRAY STORING DISCRETISED PERIODS FOR MULTIDIRECTIONAL RANDOM
! WAVES
!
      IF(ALEMON .OR. ALEMUL) THEN
        CALL BIEF_ALLVEC(1,HALE,'HALE  ',IELM , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,PALE,'PALE  ',NPALE, 1 , 0 ,MESH)
      ENDIF
!
! REFLEXION COEFFICIENTS, ANGLE OF WAVE ATTACK (FROM X AXIS)
! AND DEPHASING CAUSED BY THE WALLS
!
      CALL BIEF_ALLVEC(1,RP,'RP    ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,TETAP,'TETAP ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ALFAP,'ALFAP ',IELMB, 1 , 2 ,MESH)
!     STORAGE OF TETAP FOR AUTOMATIC REFLEXION ANGLE CALCULATION
      CALL BIEF_ALLVEC(1,TETAPM,'TETAPM',IELMB, 1 , 2 ,MESH)
!     STORAGE OF TETAP AND ALFAP VALUES
!     FOR LECLIM (SECOND DIMENSION=2 FOR THE EQUIVALENT OF UBOR)
      CALL BIEF_ALLVEC(1,TETAPS,'TETAPS',IELMB, 2 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ALFAPS,'ALFAPS',IELMB, 2 , 2 ,MESH)
!     STORAGE OF HB VALUES
      CALL BIEF_ALLVEC(1,HBS,'HBS   ',IELMB, 1 , 2 ,MESH)

! WAVE HEIGHT AND ANGLE OF WAVE ATTACK FOR OPEN BOUNDARIES
! (ANGLE FROM X AXIS)
      CALL BIEF_ALLVEC(1,HB,'HB    ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,TETAB,'TETAB ',IELMB, 1 , 2 ,MESH)
! INCIDENT POTENIAL ALONG THE BOUNDARY
! REAL AND IMAGINARY PART
      CALL BIEF_ALLVEC(1,PRB   ,'PRB   ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PIB   ,'PIB   ',IELMB, 1 , 2 ,MESH)
! GRADIENT OF INCIDENT POTENIAL ALONG THE BOUNDARY
! REAL AND IMAGINARY PART, X AND Y PART
      CALL BIEF_ALLVEC(1,DDXPRB,'DDXPRB',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DDYPRB,'DDYPRB',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DDXPIB,'DDXPIB',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DDYPIB,'DDYPIB',IELMB, 1 , 2 ,MESH)

!
! ARRAY OF POTENTIAL, IMPOSED ALONG THE BOUNDARY (DIRICHLET)
      CALL BIEF_ALLVEC(1,PHIRB,'PHIRB ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,PHIIB,'PHIIB ',IELMB, 1 , 2 ,MESH)
! BLOCK OF THESE VALUES :
      CALL ALLBLO(PHIB,'PHIB  ')
      CALL ADDBLO(PHIB,PHIRB)
      CALL ADDBLO(PHIB,PHIIB)
!
! COEFFICIENTS FOR BOUNDARY CONDITIONS
!
      CALL BIEF_ALLVEC(1,APHI1B,'APHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI1B,'BPHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI1B,'CPHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI1B,'DPHI1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CGRX1B,'CGRX1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CGRY1B,'CGRY1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DGRX1B,'DGRX1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DGRY1B,'DGRY1B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,APHI2B,'APHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI2B,'BPHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI2B,'CPHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI2B,'DPHI2B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,APHI3B,'APHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI3B,'BPHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI3B,'CPHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI3B,'DPHI3B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,APHI4B,'APHI4B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,BPHI4B,'BPHI4B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CPHI4B,'CPHI4B',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,DPHI4B,'DPHI4B',IELMB, 1 , 2 ,MESH)
!
! BIDON TABLE FOR LECLIM (SECOND DIMENSION=2 FOR THE EQUIVALENT OF UBOR)
!
      CALL BIEF_ALLVEC(1,TB1,'TB1   ',IELMB, 2 , 2 ,MESH)

! WORKING ARRAY (SIZE NELEM)  (PROBLEM)
      MEMW1 = 10 + 3*NPOIN/NELEM
!     THIS MEMORY HAS BEEN ALLOCATED IN THE FORM OF AN ARRAY
!     P0 WITH A SECOND DIMENSION
      MEMW1 = 1 + MEMW1/BIEF_NBMPTS(IELM0,MESH)
      CALL BIEF_ALLVEC(1,W1,'W1    ',IELM0,MEMW1,1,MESH)
!
!
! WORKING ARRAY (SIZE NPOIN)
!
!  NUMBER OF ARRAYS TO ALLOCATE : NTR
!           14 : FOR CGSTAB (=2 X 7)
      NTR = 16
!     FOR GMRES: NTR DEPENDS ON THE DIMENSION OF THE KRYLOV SPACE
      IF(SLVART%SLV.EQ.7) NTR = MAX(NTR,4+4*SLVART%KRYLOV)
!     2 ADDITIONAL DIAGONALS TO STORE WITH PRECONDITIONING BLOCK-DIAGONAL
      IF(3*(SLVART%PRECON/3).EQ.SLVART%PRECON) NTR = NTR + 2
!
!  ALLOCATES: NTR WORKING ARRAYS OF DIMENSION THE MAXIMUM NUMBER
!             OF DEGREES OF FREEDOM
!
!     TB STORES ARRAYS T1,T2,...
!
      CALL ALLBLO(TB ,'TB    ')
!
!
      CALL BIEF_ALLVEC_IN_BLOCK(TB,NTR,1,'T     ',IELM,1,2,MESH)
!
!     ALIASES FOR THE FIRST 4 WORKING ARRAYS IN THE BLOCK TB
!
! 16 WORKING ARRAYS
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
      T8 =>TB%ADR( 8)%P
      T9 =>TB%ADR( 9)%P
      T10 =>TB%ADR( 10)%P
      T11 =>TB%ADR( 11)%P
      T12 =>TB%ADR( 12)%P
      T13 =>TB%ADR( 13)%P
      T14 =>TB%ADR( 14)%P
      T15 =>TB%ADR( 15)%P
      T16 =>TB%ADR( 16)%P
!
! WORKING ARRAY (SIZE NPTFR)
!
      NTRBD = 4
!
!     TBBD STORES TBD1,TBD2,...
!
      CALL ALLBLO(TBBD ,'TBBD  ')
!
      CALL BIEF_ALLVEC_IN_BLOCK(TBBD,NTRBD,1,'TBD   ',IELMB,1,2,MESH)
!
!     ALIASES FOR THE FIRST 4 WORKING ARRAYS IN THE BLOCK TBBD
!
      TBD1 =>TBBD%ADR( 1)%P
      TBD2 =>TBBD%ADR( 2)%P
      TBD3 =>TBBD%ADR( 3)%P
      TBD4 =>TBBD%ADR( 4)%P
!
! MATRICES
!
      CALL BIEF_ALLMAT(AM1,'AM1   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(AM2,'AM2   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(AM3,'AM3   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(BM1,'BM1   ',IELM,IELM,CFG,'Q','Q',MESH)
      CALL BIEF_ALLMAT(BM2,'BM2   ',IELM,IELM,CFG,'Q','Q',MESH)
! BOUNDARY MATRIX
      CALL BIEF_ALLMAT(MBOR,'MBOR  ',IELMB,IELMB,CFGBOR,'Q','Q',MESH)
!
! SECOND MEMBERS
!
      CALL BIEF_ALLVEC(1,CV1,'CV1   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,CV2,'CV2   ',IELM, 1 , 2 ,MESH)
!
! ARRAYS FOR THE DISSIPATION :
!
      CALL BIEF_ALLVEC(1,MU    ,'MU    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MU2   ,'MU2   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,QB    ,'QB    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,HMU   ,'HMU   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,HMUANC,'HMUANC',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,UEB   ,'UEB   ',IELM, 1 , 2 ,MESH)
!
!
! ARRAYS FOR THE WAVE-CURRENT INTERACTION :
!
! --> CURRENT VELOCITY : X and Y DIRECTIONS
      CALL BIEF_ALLVEC(1, UC    ,'UC    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1, VC    ,'VC    ',IELM, 1 , 2 ,MESH)
! --> RELATIVE ANGULAR FREQUENCY
      CALL BIEF_ALLVEC(1, WR    ,'WR    ',IELM, 1 , 2 ,MESH)
! --> STORAGE OF WAVE VECTOR COMPONENTS
      CALL BIEF_ALLVEC(1, KANCX ,'KANCX ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1, KANCY ,'KANCY ',IELM, 1 , 2 ,MESH)
!
!
! ARRAYS FOR RADIATION STRESSES
!
      CALL BIEF_ALLVEC(1,SXX   ,'SXX   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,SXY   ,'SXY   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,SYY   ,'SYY   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,FX    ,'FX    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,FY    ,'FY    ',IELM, 1 , 2 ,MESH)
!
! ARRAYS FOR MEAN PERIODS AND PULSATIONS
!
      CALL BIEF_ALLVEC(1,T01   ,'T01   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,T02   ,'T02   ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,TM    ,'TM    ',IELM, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,OMEGAM,'OMEGAM',IELM, 1 , 2 ,MESH)
!
!
! MASKS FOR BOUNDARY NODES
!
      CALL BIEF_ALLVEC(1,MASK1,'MASK1 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK2,'MASK2 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK3,'MASK3 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK4,'MASK4 ',IELMB, 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,MASK5,'MASK5 ',IELMB, 1 , 2 ,MESH)

!
!_V5P6 : CORRECTION FOR LINUX
      CALL BIEF_ALLVEC(1,MASKEL,'MASKEL',NELMAX,1, 0 ,MESH)
      CALL BIEF_ALLVEC(1,SBID  ,'SBID  ' ,1    ,1, 0 ,MESH)
!_V5P6 : END OF CORRECTION FOR LINUX
!
! BLOCK OF MATRICES IN BERKHO
!
      CALL ALLBLO(MAT,'MAT   ')
      CALL ADDBLO(MAT,AM1)
      CALL ADDBLO(MAT,BM1)
      CALL ADDBLO(MAT,BM2)
      CALL ADDBLO(MAT,AM2)
!
!  BLOCK OF  UNKNOWNS IN BERKHO
!
      CALL ALLBLO(UNK,'UNK   ')
      CALL ADDBLO(UNK,PHIR)
      CALL ADDBLO(UNK,PHII)
!
!  BLOCK OF SECOND MEMBERS IN BERKHO
!
      CALL ALLBLO(RHS,'RHS   ')
      CALL ADDBLO(RHS,CV1)
      CALL ADDBLO(RHS,CV2)
!
! ARRAY STORING DISCRETISED DIRECTIONS FOR MULTIDIRECTIONAL
! RANDOM WAVES
!
      IF(ALEMUL) THEN
        CALL BIEF_ALLVEC(1,DALE,'DALE  ',NDALE,1,0,MESH)
      ENDIF
!
      IF(CHAINTWC) THEN
        CALL BIEF_ALLVEC(1,PDALE,'PDALE ',NDALE*NPALE,1,0,MESH)
      ENDIF
!
! ARRAYS AT THE USER'S DISPOSAL
!
      CALL ALLBLO(PRIVE ,'PRIVE ')
!
      IF(NPRIV.GT.0) THEN
!       TO BE OPTIMISED (SPACE LOST FOR 4-NPRIV ARRAYS)
!       THESE ARRAYS MUST EXIST BUT CAN BE EMPTY
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,MAX(NPRIV,4),
     &                            1,'PRIV  ',IELM,1,2,MESH)
      ELSE
        CALL BIEF_ALLVEC_IN_BLOCK(PRIVE,4,1,'PRIV  ',0,1,2,MESH)
      ENDIF
!
!_______________________________________________________________________
!
!                         * INTEGER ARRAYS *
!_______________________________________________________________________
!
!
      CALL BIEF_ALLVEC(2,LIUBOR,'LIUBOR',IELMB,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIVBOR,'LIVBOR',IELMB,1,1,MESH)
      CALL BIEF_ALLVEC(2,LIHBOR,'LIHBOR',IELMB,1,1,MESH)
! BOUNDARY_COLOUR TABLE USED IN PARALLEL
      CALL BIEF_ALLVEC(2,BOUNDARY_COLOUR,'BDNCOL',IELMB,1,1,MESH)
! BIDON TABLE FOR LECLIM
      CALL BIEF_ALLVEC(2,ITB1,'ITB1  ',IELMB,1,1,MESH)
!

      CALL BIEF_ALLVEC(2,NUMLIQ,'NUMLIQ',IELMB,1,1,MESH)
!
      CALL BIEF_ALLVEC(2,IT1   ,'IT1   ',   10,1,2,MESH)
      CALL BIEF_ALLVEC(2,IT2   ,'IT2   ',   10,1,2,MESH)
      CALL BIEF_ALLVEC(2,IT3   ,'IT3   ',   10,1,2,MESH)
!
      CALL BIEF_ALLVEC(2,LIDIR ,'LIDIR ',IELMB,2,1,MESH)
!
! BUILDS THE BLOCK THAT CONNECTS A VARIABLE NAME
! TO ITS ARRAY
!
      CALL ALLBLO(VARSOR,'VARSOR')
! 01
      IF (ALEMON .OR. ALEMUL) THEN
        CALL ADDBLO(VARSOR,HALE)
      ELSE
        CALL ADDBLO(VARSOR,HHO)
      ENDIF
! 02
      CALL ADDBLO(VARSOR,PHAS)
! 03
      CALL ADDBLO(VARSOR,U0)
! 04
      CALL ADDBLO(VARSOR,V0)
! 05
      CALL ADDBLO(VARSOR,S)
! 06
      CALL ADDBLO(VARSOR,ZF)
! 07
      CALL ADDBLO(VARSOR,H)
! 08
      CALL ADDBLO(VARSOR,C)
! 09
      CALL ADDBLO(VARSOR,CG)
! 10
      CALL ADDBLO(VARSOR,K)
! 11
      CALL ADDBLO(VARSOR,PHIR)
! 12
      CALL ADDBLO(VARSOR,PHII)
! 13
      CALL ADDBLO(VARSOR,PRIVE%ADR(1)%P)
! 14
      CALL ADDBLO(VARSOR,PRIVE%ADR(2)%P)
! 15
      CALL ADDBLO(VARSOR,PRIVE%ADR(3)%P)
! 16
      CALL ADDBLO(VARSOR,PRIVE%ADR(4)%P)
! 17
      CALL ADDBLO(VARSOR,T01)
! 18
      CALL ADDBLO(VARSOR,T02)
! 19
      CALL ADDBLO(VARSOR,TM)
! 20
      CALL ADDBLO(VARSOR,FX)
! 21
      CALL ADDBLO(VARSOR,FY)
! 22
      CALL ADDBLO(VARSOR,INCI)
! 23
      CALL ADDBLO(VARSOR,QB)
! 24
      CALL ADDBLO(VARSOR,SXX)
! 25
      CALL ADDBLO(VARSOR,SXY)
! 26
      CALL ADDBLO(VARSOR,SYY)
!
!***********************************************************************
!
! CHECKS :
!
      ISTOP=0
      IF(ISTOP.EQ.1) THEN
        CALL PLANTE(1)
        STOP
      ENDIF
!
! WRITES OUT :
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




