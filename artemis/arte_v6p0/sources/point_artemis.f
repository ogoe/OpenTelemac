C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES STRUCTURES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::ALEMON ALEMON@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALEMUL ALEMUL@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALFAP ALFAP@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALFAPT ALFAPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::AM1 AM1@endlink, 
!> @link DECLARATIONS_ARTEMIS::AM2 AM2@endlink, 
!> @link DECLARATIONS_ARTEMIS::AM3 AM3@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI1B APHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI2B APHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI3B APHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI4B APHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::ART_FILES ART_FILES@endlink, 
!> @link DECLARATIONS_ARTEMIS::BM1 BM1@endlink, 
!> @link DECLARATIONS_ARTEMIS::BM2 BM2@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI1B BPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI2B BPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI3B BPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI4B BPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::CG CG@endlink, 
!> @link DECLARATIONS_ARTEMIS::CGT CGT@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI1B CPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI2B CPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI3B CPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI4B CPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CTT CTT@endlink, 
!> @link DECLARATIONS_ARTEMIS::CV1 CV1@endlink, 
!> @link DECLARATIONS_ARTEMIS::CV2 CV2@endlink, 
!> @link DECLARATIONS_ARTEMIS::DALE DALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI1B DPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI2B DPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI3B DPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI4B DPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::EQUA EQUA@endlink, 
!> @link DECLARATIONS_ARTEMIS::FW FW@endlink, 
!> @link DECLARATIONS_ARTEMIS::FX FX@endlink, 
!> @link DECLARATIONS_ARTEMIS::FY FY@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::HALE HALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::HB HB@endlink, 
!> @link DECLARATIONS_ARTEMIS::HBT HBT@endlink, 
!> @link DECLARATIONS_ARTEMIS::HHO HHO@endlink, 
!> @link DECLARATIONS_ARTEMIS::HMU HMU@endlink, 
!> @link DECLARATIONS_ARTEMIS::HMUANC HMUANC@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM IELM@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM0 IELM0@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELMB IELMB@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELMB0 IELMB0@endlink, 
!> @link DECLARATIONS_ARTEMIS::INCI INCI@endlink, 
!> @link DECLARATIONS_ARTEMIS::IT1 IT1@endlink, 
!> @link DECLARATIONS_ARTEMIS::IT2 IT2@endlink, 
!> @link DECLARATIONS_ARTEMIS::IT3 IT3@endlink, 
!> @link DECLARATIONS_ARTEMIS::K K@endlink, 
!> @link DECLARATIONS_ARTEMIS::KN1 KN1@endlink, 
!> @link DECLARATIONS_ARTEMIS::KN2 KN2@endlink, 
!> @link DECLARATIONS_ARTEMIS::KNANC1 KNANC1@endlink, 
!> @link DECLARATIONS_ARTEMIS::KNANC2 KNANC2@endlink, 
!> @link DECLARATIONS_ARTEMIS::KP1BOR_TOT KP1BOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::KT KT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIDIR LIDIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBORT LIHBORT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIUBOR LIUBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIVBOR LIVBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK1 MASK1@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK1T MASK1T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK2 MASK2@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK2T MASK2T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK3 MASK3@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK3T MASK3T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK4 MASK4@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK4T MASK4T@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_ARTEMIS::MAT MAT@endlink, 
!> @link DECLARATIONS_ARTEMIS::MBOR MBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::MCOS MCOS@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSIN MSIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::MU MU@endlink, 
!> @link DECLARATIONS_ARTEMIS::MU2 MU2@endlink, 
!> @link DECLARATIONS_ARTEMIS::NBOR_TOT NBOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NDALE NDALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPALE NPALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN_TOT NPOIN_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR_TOT NPTFR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NUMLIQ NUMLIQ@endlink, 
!> @link DECLARATIONS_ARTEMIS::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PALE PALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHAS PHAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIB PHIB@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHII PHII@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIIB PHIIB@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIR PHIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIRB PHIRB@endlink, 
!> @link DECLARATIONS_ARTEMIS::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_ARTEMIS::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_ARTEMIS::QB QB@endlink, 
!> @link DECLARATIONS_ARTEMIS::RHS RHS@endlink, 
!> @link DECLARATIONS_ARTEMIS::RP RP@endlink, 
!> @link DECLARATIONS_ARTEMIS::RPT RPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::S S@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::SLVART SLVART@endlink, 
!> @link DECLARATIONS_ARTEMIS::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_ARTEMIS::SXX SXX@endlink, 
!> @link DECLARATIONS_ARTEMIS::SXY SXY@endlink, 
!> @link DECLARATIONS_ARTEMIS::SYY SYY@endlink, 
!> @link DECLARATIONS_ARTEMIS::T01 T01@endlink, 
!> @link DECLARATIONS_ARTEMIS::T02 T02@endlink, 
!> @link DECLARATIONS_ARTEMIS::TB TB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TBBD TBBD@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAB TETAB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETABT TETABT@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAP TETAP@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAPT TETAPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::TM TM@endlink, 
!> @link DECLARATIONS_ARTEMIS::U0 U0@endlink, 
!> @link DECLARATIONS_ARTEMIS::UC UC@endlink, 
!> @link DECLARATIONS_ARTEMIS::UNK UNK@endlink, 
!> @link DECLARATIONS_ARTEMIS::V0 V0@endlink, 
!> @link DECLARATIONS_ARTEMIS::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::VC VC@endlink, 
!> @link DECLARATIONS_ARTEMIS::W1 W1@endlink, 
!> @link DECLARATIONS_ARTEMIS::WR WR@endlink, 
!> @link DECLARATIONS_ARTEMIS::XT XT@endlink, 
!> @link DECLARATIONS_ARTEMIS::YT YT@endlink, 
!> @link DECLARATIONS_ARTEMIS::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CFG, CFGBOR, DBID, I, INOSYM, ISTOP, ISYM, J, MEMW1, NTR, NTRBD, TMP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> DIM, IKLE, LV, MXELVS, MXPTVS, NELEM, NELMAX, NPMAX, NPOIN, NPTFR, NPTFRX, T1, T2, T3, T4, TBD1, TBD2, TBD3, TBD4, TYPELM, X, Y
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ADDBLO(), ALLBLO(), ALLMAT(), ALLVEC(), ALLVEC_IN_BLOCK(), ALMESH(), NBMPTS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ARTEMIS()

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
!>  <tr>
!>    <td><center> 6.0                                    </center></td>
!>    <td> 21/08/2000                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 24/04/1997                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE POINT_ARTEMIS
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
      INTEGER ISYM,MEMW1,INOSYM,ISTOP,NTR,NTRBD
      INTEGER, TARGET :: TMP=1029
      INTEGER CFG(2),CFGBOR(2),I,J
      DOUBLE PRECISION :: DBID
C-----------------------------------------------------------------------
C
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
C
C-----------------------------------------------------------------------
C
C  PARAMETERS DEPENDING ON THE TYPE OF ELEMENT: (SEE ALSO NDP)
C  ISYM AND INOSYM ARE USED TO DIMENSION THE ARRAYS CONTAINING THE
C                   EXTRADIAGONAL PART OF THE MATRICES.
C
      ISYM=3
      INOSYM=6
      IELM = 11
      IELM0 = 10
      IELMB = 1
      IELMB0 = 0


C TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
C
      CFG(1) = OPTASS
      CFG(2) = PRODUC
C     CFG IS IMPOSED FOR BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
      EQUA = 'ARTEMIS'
C
C=======================================================================
C
C     ALLOCATES THE MESH STRUCTURE
C
       CALL ALMESH(MESH,'MESH  ',IELM,SPHERI,CFG,ART_FILES(ARTGEO)%LU,
     &     EQUA)
C

      IF (NCSIZE .GT. 1) THEN
         OPEN(UNIT=25,FILE='FRONT_GLOB.DAT')
         READ(25,*) NPOIN_TOT
         READ(25,*) NPTFR_TOT
         ALLOCATE(KP1BOR_TOT(NPTFR_TOT*2))
         ALLOCATE(NBOR_TOT(NPTFR_TOT))
         DO I=1,NPTFR_TOT
            READ(25,*) NBOR_TOT(I)
         END DO
         DO I=1,2*NPTFR_TOT
            READ(25,*) KP1BOR_TOT(I)
         END DO
         CLOSE(25)
         ALLOCATE(LIHBORT(NPTFR_TOT))
         ALLOCATE(RPT(NPTFR_TOT))
         ALLOCATE(TETAPT(NPTFR_TOT))
         ALLOCATE(TETABT(NPTFR_TOT))
         ALLOCATE(HBT(NPTFR_TOT))
         ALLOCATE(ALFAPT(NPTFR_TOT))
         ALLOCATE(MASK1T(NPTFR_TOT))
         ALLOCATE(MASK2T(NPTFR_TOT))
         ALLOCATE(MASK3T(NPTFR_TOT))
         ALLOCATE(MASK4T(NPTFR_TOT))
         ALLOCATE(XT(NPOIN_TOT))
         ALLOCATE(YT(NPOIN_TOT))
         ALLOCATE(CGT(NPOIN_TOT))
         ALLOCATE(CTT(NPOIN_TOT))
         ALLOCATE(KT(NPOIN_TOT))
      ELSE
         NPTFR_TOT=MESH%NPTFR
         ALLOCATE(RPT(NPTFR_TOT))
         ALLOCATE(TETAPT(NPTFR_TOT))
         ALLOCATE(TETABT(NPTFR_TOT))
         ALLOCATE(HBT(NPTFR_TOT))
         ALLOCATE(ALFAPT(NPTFR_TOT))
         ALLOCATE(LIHBORT(NPTFR_TOT))

         ALLOCATE(NBOR_TOT(NPTFR_TOT))
      END IF
C REPLACE WITH NPTFR_TOT
C
C     ALIAS FOR CERTAIN COMPONENTS OF MESH
C
      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
C
      TMP=MESH%NPTFR
C      MESH%NPTFR=1029
C      MESH%NPTFRX=1029
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
C      WRITE(*,*) 'FRONTIERE',NPTFR,MESH%NPTFR
C
C-----------------------------------------------------------------------
C
C                     ******************
C                     *   REAL ARRAYS  *
C                     ******************
C
C-----------------------------------------------------------------------
C
C
C POTENTIAL
C
      CALL ALLVEC(1,PHIR,'PHIR  ',IELM, 1 , 2 )
      CALL ALLVEC(1,PHII,'PHII  ',IELM, 1 , 2 )
C
C WATER DEPTH AT REST
C
      CALL ALLVEC(1,H,'H     ',IELM, 1 , 2 )
C
C WAVE NUMBER
      CALL ALLVEC(1,K,'K     ',IELM, 1 , 2 )
C
C PHASE AND GROUP VELOCITIES
      CALL ALLVEC(1,C,'C     ',IELM, 1 , 2 )
      CALL ALLVEC(1,CG,'CG    ',IELM, 1 , 2 )
C
C WAVE HEIGHT AND PHASE
      CALL ALLVEC(1,HHO,'HHO   ',IELM, 1 , 2 )
      CALL ALLVEC(1,PHAS,'PHAS  ',IELM, 1 , 2 )
C
C VELOCITIES
      CALL ALLVEC(1,U0,'U0    ',IELM, 1 , 2 )
      CALL ALLVEC(1,V0,'V0    ',IELM, 1 , 2 )
C
C AVERAGES OF THE SINES AND COSINES OF THE WAVE DIRECTION
      CALL ALLVEC(1,MCOS,'MCOS  ',IELM, 1 , 2 )
      CALL ALLVEC(1,MSIN,'MSIN  ',IELM, 1 , 2 )
C WAVE INCIDENCE
      CALL ALLVEC(1,INCI,'INCI  ',IELM, 1 , 2 )
C
C FREE SURFACE AND BOTTOM ELEVATION
      CALL ALLVEC(1,S,'S     ',IELM, 1 , 2 )
      CALL ALLVEC(1,ZF,'ZF    ',IELM, 1 , 2 )
C
C FRICTION COEFFICIENT (VARIABLE IN SPACE)
      CALL ALLVEC(1,FW,'FW    ',IELM, 1 , 2 )
C
C WAVE HEIGHT (RANDOM SEAS)
C ARRAY STORING DISCRETISED PERIODS FOR MULTIDIRECTIONAL RANDOM
C WAVES
C
      IF (ALEMON .OR. ALEMUL) THEN
         CALL ALLVEC(1,HALE,'HALE  ',IELM, 1 , 2 )
         CALL ALLVEC(1,PALE,'PALE  ',NPALE, 1 , 0 )
      ENDIF
C
C REFLEXION COEFFICIENTS, ANGLE OF WAVE ATTACK (FROM X AXIS)
C AND DEPHASING CAUSED BY THE WALLS
C
      CALL ALLVEC(1,RP,'RP    ',IELMB, 1 , 2 )
      CALL ALLVEC(1,TETAP,'TETAP ',IELMB, 1 , 2 )
      CALL ALLVEC(1,ALFAP,'ALFAP ',IELMB, 1 , 2 )
C
C WAVE HEIGHT AND ANGLE OF WAVE ATTACK FOR OPEN BOUNDARIES
C (ANGLE FROM X AXIS)
      CALL ALLVEC(1,HB,'HB    ',IELMB, 1 , 2 )
      CALL ALLVEC(1,TETAB,'TETAB ',IELMB, 1 , 2 )
C
C ARRAY OF POTENTIAL, IMPOSED ALONG THE BOUNDARY (DIRICHLET)
      CALL ALLVEC(1,PHIRB,'PHIRB ',IELMB, 1 , 2 )
      CALL ALLVEC(1,PHIIB,'PHIIB ',IELMB, 1 , 2 )
C BLOCK OF THESE VALUES :
      CALL ALLBLO(PHIB,'PHIB  ')
      CALL ADDBLO(PHIB,PHIRB)
      CALL ADDBLO(PHIB,PHIIB)
C
C COEFFICIENTS FOR BOUNDARY CONDITIONS
C

      CALL ALLVEC(1,APHI1B,'APHI1B',IELMB, 1 , 2 )
      CALL ALLVEC(1,BPHI1B,'BPHI1B',IELMB, 1 , 2 )
      CALL ALLVEC(1,CPHI1B,'CPHI1B',IELMB, 1 , 2 )
      CALL ALLVEC(1,DPHI1B,'DPHI1B',IELMB, 1 , 2 )
      CALL ALLVEC(1,APHI2B,'APHI2B',IELMB, 1 , 2 )
      CALL ALLVEC(1,BPHI2B,'BPHI2B',IELMB, 1 , 2 )
      CALL ALLVEC(1,CPHI2B,'CPHI2B',IELMB, 1 , 2 )
      CALL ALLVEC(1,DPHI2B,'DPHI2B',IELMB, 1 , 2 )
      CALL ALLVEC(1,APHI3B,'APHI3B',IELMB, 1 , 2 )
      CALL ALLVEC(1,BPHI3B,'BPHI3B',IELMB, 1 , 2 )
      CALL ALLVEC(1,CPHI3B,'CPHI3B',IELMB, 1 , 2 )
      CALL ALLVEC(1,DPHI3B,'DPHI3B',IELMB, 1 , 2 )
      CALL ALLVEC(1,APHI4B,'APHI4B',IELMB, 1 , 2 )
      CALL ALLVEC(1,BPHI4B,'BPHI4B',IELMB, 1 , 2 )
      CALL ALLVEC(1,CPHI4B,'CPHI4B',IELMB, 1 , 2 )
      CALL ALLVEC(1,DPHI4B,'DPHI4B',IELMB, 1 , 2 )
C
C
C WORKING ARRAY (SIZE NELEM)  (PROBLEM)
      MEMW1 = 10 + 3*NPOIN/NELEM
C     THIS MEMORY HAS BEEN ALLOCATED IN THE FORM OF AN ARRAY
C     P0 WITH A SECOND DIMENSION
      MEMW1 = 1 + MEMW1/NBMPTS(IELM0)
      CALL ALLVEC(1,W1,'W1    ',IELM0,MEMW1,1)
C
C
C WORKING ARRAY (SIZE NPOIN)
C
C  NUMBER OF ARRAYS TO ALLOCATE : NTR
C           14 : FOR CGSTAB (=2 X 7)
      NTR = 14
C     FOR GMRES: NTR DEPENDS ON THE DIMENSION OF THE KRYLOV SPACE
      IF(SLVART%SLV.EQ.7) NTR = MAX(NTR,4+4*SLVART%KRYLOV)
C     2 ADDITIONAL DIAGONALS TO STORE WITH PRECONDITIONING BLOCK-DIAGONAL
      IF(3*(SLVART%PRECON/3).EQ.SLVART%PRECON) NTR = NTR + 2
C
C  ALLOCATES: NTR WORKING ARRAYS OF DIMENSION THE MAXIMUM NUMBER
C             OF DEGREES OF FREEDOM
C
C     TB STORES ARRAYS T1,T2,...
C
      CALL ALLBLO(TB ,'TB    ')
C
C
      CALL ALLVEC_IN_BLOCK(TB,NTR,1,'T     ',IELM,1,2)
C
C     ALIASES FOR THE FIRST 4 WORKING ARRAYS IN THE BLOCK TB
C
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
C
C WORKING ARRAY (SIZE NPTFR)
C
      NTRBD = 4
C
C     TBBD STORES TBD1,TBD2,...
C
      CALL ALLBLO(TBBD ,'TBBD  ')
C
      CALL ALLVEC_IN_BLOCK(TBBD,NTRBD,1,'TBD   ',IELMB,1,2)
C
C     ALIASES FOR THE FIRST 4 WORKING ARRAYS IN THE BLOCK TBBD
C
      TBD1 =>TBBD%ADR( 1)%P
      TBD2 =>TBBD%ADR( 2)%P
      TBD3 =>TBBD%ADR( 3)%P
      TBD4 =>TBBD%ADR( 4)%P
C
C MATRICES
C
      CALL ALLMAT(AM1,'AM1   ',IELM,IELM,CFG,'Q','Q')
      CALL ALLMAT(AM2,'AM2   ',IELM,IELM,CFG,'Q','Q')
      CALL ALLMAT(AM3,'AM3   ',IELM,IELM,CFG,'Q','Q')
      CALL ALLMAT(BM1,'BM1   ',IELM,IELM,CFG,'Q','Q')
      CALL ALLMAT(BM2,'BM2   ',IELM,IELM,CFG,'Q','Q')
C BOUNDARY MATRIX
      CALL ALLMAT(MBOR,'MBOR  ',IELMB,IELMB,CFGBOR,'Q','Q')
C
C SECOND MEMBERS
C
      CALL ALLVEC(1,CV1,'CV1   ',IELM, 1 , 2 )
      CALL ALLVEC(1,CV2,'CV2   ',IELM, 1 , 2 )
C
C ARRAYS FOR THE DISSIPATION :
C
      CALL ALLVEC(1,MU    ,'MU    ',IELM, 1 , 2 )
      CALL ALLVEC(1,MU2   ,'MU2   ',IELM, 1 , 2 )
      CALL ALLVEC(1,QB    ,'QB    ',IELM, 1 , 2 )
      CALL ALLVEC(1,HMU   ,'HMU   ',IELM, 1 , 2 )
      CALL ALLVEC(1,HMUANC,'HMUANC',IELM, 1 , 2 )
C
C ARRAYS FOR RADIATION STRESSES
C
      CALL ALLVEC(1,SXX   ,'SXX   ',IELM, 1 , 2 )
      CALL ALLVEC(1,SXY   ,'SXY   ',IELM, 1 , 2 )
      CALL ALLVEC(1,SYY   ,'SYY   ',IELM, 1 , 2 )
      CALL ALLVEC(1,FX    ,'FX    ',IELM, 1 , 2 )
      CALL ALLVEC(1,FY    ,'FY    ',IELM, 1 , 2 )
C
C ARRAYS FOR MEAN PERIODS
C
      CALL ALLVEC(1,T01   ,'T01   ',IELM, 1 , 2 )
      CALL ALLVEC(1,T02   ,'T02   ',IELM, 1 , 2 )
      CALL ALLVEC(1,TM    ,'TM    ',IELM, 1 , 2 )
C
C
C MASKS FOR BOUNDARY NODES
C
      CALL ALLVEC(1,MASK1,'MASK1 ',IELMB, 1 , 2 )
      CALL ALLVEC(1,MASK2,'MASK2 ',IELMB, 1 , 2 )
      CALL ALLVEC(1,MASK3,'MASK3 ',IELMB, 1 , 2 )
      CALL ALLVEC(1,MASK4,'MASK4 ',IELMB, 1 , 2 )
C
C_V5P6 : CORRECTION FOR LINUX
      CALL ALLVEC(1,MASKEL,'MASKEL',NELMAX, 1 , 0 )
      CALL ALLVEC(1,SBID  ,'SBID  ' ,1     , 1 , 0 )
C_V5P6 : END OF CORRECTION FOR LINUX
C
C BLOCK OF MATRICES IN BERKHO
C
      CALL ALLBLO(MAT,'MAT   ')
      CALL ADDBLO(MAT,AM1)
      CALL ADDBLO(MAT,BM1)
      CALL ADDBLO(MAT,BM2)
      CALL ADDBLO(MAT,AM2)
C
C  BLOCK OF  UNKNOWNS IN BERKHO
C
      CALL ALLBLO(UNK,'UNK   ')
      CALL ADDBLO(UNK,PHIR)
      CALL ADDBLO(UNK,PHII)
C
C  BLOCK OF SECOND MEMBERS IN BERKHO
C
      CALL ALLBLO(RHS,'RHS   ')
      CALL ADDBLO(RHS,CV1)
      CALL ADDBLO(RHS,CV2)
C
C ARRAY STORING DISCRETISED DIRECTIONS FOR MULTIDIRECTIONAL
C RANDOM WAVES
C
      IF (ALEMUL) THEN
         CALL ALLVEC(1,DALE,'DALE  ',NDALE, 1 , 0 )
      ENDIF
C
C ARRAYS AT THE USER'S DISPOSAL
C
      CALL ALLBLO(PRIVE ,'PRIVE ')
C
      IF(NPRIV.GT.0) THEN
C       TO BE OPTIMISED (SPACE LOST FOR 4-NPRIV ARRAYS)
C       THESE ARRAYS MUST EXIST BUT CAN BE EMPTY
        CALL ALLVEC_IN_BLOCK(PRIVE,MAX(NPRIV,4),1,'PRIV  ',IELM,1,2)
      ELSE
        CALL ALLVEC_IN_BLOCK(PRIVE,           4,1,'PRIV  ',    0,1,2)
      ENDIF
C
C
C
C --> ER : START
C --> FLOW
C      IF (COURANT) THEN
         CALL ALLVEC(1, UC ,'UC     ',IELM, 1 , 2 )
         CALL ALLVEC(1, VC ,'VC     ',IELM, 1 , 2 )
C --> RELATIVE ANGULAR FREQUENCY
         CALL ALLVEC(1, WR ,'WR     ',IELM, 1 , 2 )

C --> INTERMEDIATE REAL VECTOR: WAVE VECTOR AND ERROR
        CALL ALLVEC(1, KN1 ,'KN1     ',IELM, 1 , 2 )
        CALL ALLVEC(1, KN2 ,'KN2     ',IELM, 1 , 2 )
        CALL ALLVEC(1, KNANC1 ,'KNANC1     ',IELM, 1 , 2 )
        CALL ALLVEC(1, KNANC2 ,'KNANC2     ',IELM, 1 , 2 )
C      ENDIF

C --> ER : END

C  END FOR REALS
C
C
C_______________________________________________________________________
C
C                         * INTEGER ARRAYS *
C_______________________________________________________________________
C
C
      CALL ALLVEC(2,LIUBOR,'LIUBOR',IELMB,1,1)
      CALL ALLVEC(2,LIVBOR,'LIVBOR',IELMB,1,1)
      CALL ALLVEC(2,LIHBOR,'LIHBOR',IELMB,1,1)
      CALL ALLVEC(2,NUMLIQ,'NUMLIQ',IELMB,1,1)
C
      CALL ALLVEC(2,IT1   ,'IT1   ',   10,1,2)
      CALL ALLVEC(2,IT2   ,'IT2   ',   10,1,2)
      CALL ALLVEC(2,IT3   ,'IT3   ',   10,1,2)
C
      CALL ALLVEC(2,LIDIR ,'LIDIR ',IELMB,2,1)
C
C BUILDS THE BLOCK THAT CONNECTS A VARIABLE NAME
C TO ITS ARRAY
C
      CALL ALLBLO(VARSOR,'VARSOR')
C 01
      IF (ALEMON .OR. ALEMUL) THEN
         CALL ADDBLO(VARSOR,HALE)
      ELSE
         CALL ADDBLO(VARSOR,HHO)
      ENDIF
C 02
      CALL ADDBLO(VARSOR,PHAS)
C 03
      CALL ADDBLO(VARSOR,U0)
C 04
      CALL ADDBLO(VARSOR,V0)
C 05
      CALL ADDBLO(VARSOR,S)
C 06
      CALL ADDBLO(VARSOR,ZF)
C 07
      CALL ADDBLO(VARSOR,H)
C 08
      CALL ADDBLO(VARSOR,C)
C 09
      CALL ADDBLO(VARSOR,CG)
C 10
      CALL ADDBLO(VARSOR,K)
C 11
      CALL ADDBLO(VARSOR,PHIR)
C 12
      CALL ADDBLO(VARSOR,PHII)
C 13
      CALL ADDBLO(VARSOR,PRIVE%ADR(1)%P)
C 14
      CALL ADDBLO(VARSOR,PRIVE%ADR(2)%P)
C 15
      CALL ADDBLO(VARSOR,PRIVE%ADR(3)%P)
C 16
      CALL ADDBLO(VARSOR,PRIVE%ADR(4)%P)
C 17
      CALL ADDBLO(VARSOR,T01)
C 18
      CALL ADDBLO(VARSOR,T02)
C 19
      CALL ADDBLO(VARSOR,TM)
C 20
      CALL ADDBLO(VARSOR,FX)
C 21
      CALL ADDBLO(VARSOR,FY)
C 22
      CALL ADDBLO(VARSOR,INCI)
C 23
      CALL ADDBLO(VARSOR,QB)
C 24
      CALL ADDBLO(VARSOR,SXX)
C 25
      CALL ADDBLO(VARSOR,SXY)
C 26
      CALL ADDBLO(VARSOR,SYY)
C
C***********************************************************************
C
C CHECKS :
C
      ISTOP=0
      IF(ISTOP.EQ.1) STOP
C
C WRITES OUT :
C
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
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C