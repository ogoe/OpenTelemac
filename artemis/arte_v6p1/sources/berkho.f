C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE BERKHOFF EQUATION MODIFIED BY
!>                THE INTRODUCTION OF DISSIPATION TERMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>      DIV (C*CG*GRAD(PHI)) + C*CG*( K**2 + I*K*MU ) * PHI = 0
!>                                           ------
!>
!> PHI IS A COMPLEX FUNCTION (REAL COMPONENT: PHIR AND IMAGINARY
!> COMPONENT: PHII)
!>
!> MU IS A DISSIPATION COEFFICIENT (A PRIORI UNKNOWN)
!>
!> THE BOUNDARY CONDITIONS COUPLE THE EQUATIONS IN PHIR AND PHII
!> THEY ARE:
!>
!> D (PHI) /DN - I*K*PHI = D (F) /DN - I*K*F (N: EXTERNAL NORMAL)
!> FOR A LIQUID BOUNDARY WITH INCIDENT WAVE CONDITION DEFINED
!> BY THE POTENTIAL F (F=0 FOR A FREE EXIT)
!>
!> D (PHI) /DN - I* (1-R*EXP (I*ALFA))/(1 + R*EXP (I*ALFA))*K*COS (TETA) *PHI = 0
!> FOR A SOLID BOUNDARY, WITH WALL REFLEXION COEFFICIENT: R,
!> ANGLE OF INCIDENCE OF THE WAVES ON THE WALL: TETA, AND DEPHASING
!> CAUSED BY THE WALL: ALFA.
!>
!> THUS GENERALLY :
!> D(PHIR)/DN = APHIRB*PHII + BPHIRB*PHIR + CPHIRB
!> D(PHII)/DN =-APHIRB*PHIR + BPHIRB*PHII + DPHIRB
!>
!>
!> AFTER VARIATIONAL FORMULATION :
!>
!>         (  AM1          BM1     )  ( PHIR )   ( CV1 )
!>         (                       )  (      ) = (     )
!>         (                       )  (      )   (     )
!>         (  -BM1         AM1     )  ( PHII )   ( CV2 )
!>
!>           /
!> AM1 =    / C*CG * GRAD(PSII)*GRAD(PSIJ) DS
!>         /S
!>
!>           /
!>       -  / OMEGA**2 * CG/C * PSII*PSIJ  DS
!>         /S
!>
!>           /
!>       -  /  BPHIRB * PSII*PSIJ  DB
!>         /B
!>
!>           /                         /
!> BM1 =  - /  APHIR * PSII*PSIJ DB + /  C*CG* K * MU * PSII * PSIJ DS
!>         /B                        /S
!>
!>          /
!> CV1 =   /   CPHIR * PSII DB
!>        /B
!>
!>          /
!> CV2 =   /   CPHII * PSII DB
!>        /B
!>
!>
!> WHERE S IS THE COMPUTATIONAL DOMAIN AND B ITS BOUNDARY
!>       PSII AND PSIJ ARE THE BASIC FUNCTIONS AT NODES I AND J
!>
!> GIVEN THAT APHII=-APHIR, BM1 IS ALSO IN THE EQUATION IN PHII.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> LT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::ALEMON ALEMON@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALEMUL ALEMUL@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALFABJ ALFABJ@endlink, 
!> @link DECLARATIONS_ARTEMIS::AM1 AM1@endlink, 
!> @link DECLARATIONS_ARTEMIS::AM2 AM2@endlink, 
!> @link DECLARATIONS_ARTEMIS::AM3 AM3@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI1B APHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI2B APHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI3B APHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI4B APHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BM1 BM1@endlink, 
!> @link DECLARATIONS_ARTEMIS::BM2 BM2@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI1B BPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI2B BPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI3B BPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::BPHI4B BPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::CG CG@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI1B CPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI2B CPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI3B CPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CPHI4B CPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::CV1 CV1@endlink, 
!> @link DECLARATIONS_ARTEMIS::CV2 CV2@endlink, 
!> @link DECLARATIONS_ARTEMIS::DEFERL DEFERL@endlink, 
!> @link DECLARATIONS_ARTEMIS::DIAM50 DIAM50@endlink, 
!> @link DECLARATIONS_ARTEMIS::DIAM90 DIAM90@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI1B DPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI2B DPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI3B DPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI4B DPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::ENTFW ENTFW@endlink, 
!> @link DECLARATIONS_ARTEMIS::ENTREG ENTREG@endlink, 
!> @link DECLARATIONS_ARTEMIS::ENTRUG ENTRUG@endlink, 
!> @link DECLARATIONS_ARTEMIS::EPSDIS EPSDIS@endlink, 
!> @link DECLARATIONS_ARTEMIS::FORMFR FORMFR@endlink, 
!> @link DECLARATIONS_ARTEMIS::FROTTE FROTTE@endlink, 
!> @link DECLARATIONS_ARTEMIS::FW FW@endlink, 
!> @link DECLARATIONS_ARTEMIS::FWCOEF FWCOEF@endlink, 
!> @link DECLARATIONS_ARTEMIS::GAMMAS GAMMAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::GDALLY GDALLY@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::HMU HMU@endlink, 
!> @link DECLARATIONS_ARTEMIS::HMUANC HMUANC@endlink, 
!> @link DECLARATIONS_ARTEMIS::IBREAK IBREAK@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM IELM@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELMB IELMB@endlink, 
!> @link DECLARATIONS_ARTEMIS::INFOGR INFOGR@endlink, 
!> @link DECLARATIONS_ARTEMIS::K K@endlink, 
!> @link DECLARATIONS_ARTEMIS::KDALLY KDALLY@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIDIR LIDIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK1 MASK1@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK2 MASK2@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK3 MASK3@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASK4 MASK4@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_ARTEMIS::MAT MAT@endlink, 
!> @link DECLARATIONS_ARTEMIS::MBOR MBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSK MSK@endlink, 
!> @link DECLARATIONS_ARTEMIS::MU MU@endlink, 
!> @link DECLARATIONS_ARTEMIS::MU2 MU2@endlink, 
!> @link DECLARATIONS_ARTEMIS::MVEAU MVEAU@endlink, 
!> @link DECLARATIONS_ARTEMIS::MVSED MVSED@endlink, 
!> @link DECLARATIONS_ARTEMIS::NITDIS NITDIS@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_ARTEMIS::OMEGA OMEGA@endlink, 
!> @link DECLARATIONS_ARTEMIS::PER PER@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIB PHIB@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHII PHII@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIR PHIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_ARTEMIS::QB QB@endlink, 
!> @link DECLARATIONS_ARTEMIS::REGIDO REGIDO@endlink, 
!> @link DECLARATIONS_ARTEMIS::RELDIS RELDIS@endlink, 
!> @link DECLARATIONS_ARTEMIS::RHS RHS@endlink, 
!> @link DECLARATIONS_ARTEMIS::RICOEF RICOEF@endlink, 
!> @link DECLARATIONS_ARTEMIS::S S@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::SLVART SLVART@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::T3 T3@endlink, 
!> @link DECLARATIONS_ARTEMIS::T4 T4@endlink, 
!> @link DECLARATIONS_ARTEMIS::TB TB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAB TETAB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAP TETAP@endlink, 
!> @link DECLARATIONS_ARTEMIS::UNK UNK@endlink, 
!> @link DECLARATIONS_ARTEMIS::VISCO VISCO@endlink, 
!> @link DECLARATIONS_ARTEMIS::ZF ZF@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CBID, DEGRAD, ECRHMU, FFW, HEFF, HM, HMUE, I, ITERMU, MAX_ECRHMU, MAX_MODHMU, MODHMU, PI, Q1, Q2, Q3, RADDEG, TDEB1, TFIN1
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BERKHO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CALCFW(), CALCQB(), CALCUE(), CNTPRE(), DIRICH(), FWSPEC(), LUMP(), MATRIX(), OM(), OS(), OSDB(), P_DMAX(), SOLVE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>    <td> 18/03/2010                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 02/04/2007                                              </td>
!>    <td>                                                         </td>
!>    <td> INVERSION OF THE SECOND EQUATION BEFORE CALL TO SOLVE IF DIRECT SOLVEUR IS USED  </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IELMB
!></td><td>--></td><td>TYPE D'ELEMENT DE BORD
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>INDICE DU CALCUL COURANT
!>    </td></tr>
!>          <tr><td>OMEGA
!></td><td>--></td><td>PULSATION DE LA HOULE
!>    </td></tr>
!>          <tr><td>PER
!></td><td>--></td><td>PERIODE DE LA HOULE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BERKHO
     &(LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IELM           |-->| TYPE D'ELEMENT
C| IELMB          |-->| TYPE D'ELEMENT DE BORD
C| LT             |-->| INDICE DU CALCUL COURANT
C| OMEGA          |-->| PULSATION DE LA HOULE
C| PER            |-->| PERIODE DE LA HOULE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_ARTEMIS, EX_BERKHO => BERKHO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C
      INTEGER I,LT
      INTEGER ITERMU
      DOUBLE PRECISION MAX_ECRHMU
      DOUBLE PRECISION MAX_MODHMU

      DOUBLE PRECISION HM,HMUE,HEFF,ECRHMU,MODHMU
      DOUBLE PRECISION Q1,Q2,Q3
C
      DOUBLE PRECISION CBID,FFW
      DOUBLE PRECISION PI,DEGRAD,RADDEG
C
C-----------------------------------------------------------------------
C
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
C
C-----------------------------------------------------------------------
C
      INTRINSIC ABS,MIN,MAX,LOG
      DOUBLE PRECISION P_DMAX
      EXTERNAL P_DMAX
      DOUBLE PRECISION TDEB1,TFIN1
C
C----------------------------------------------------------------------
C
C INITIALISES MU AND FW: SET TO 0
C         FOR THE FIRST ITERATION
C
      ITERMU=0
      IF (LT.EQ.0) THEN
         CALL OS( 'X=C     ' , MU , SBID , SBID , 0.D0 )
         CALL OS( 'X=C     ' , FW , SBID , SBID , 0.D0 )
      ENDIF
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
C ITERATIVE LOOP ON THE VARIABLE MU (DISSIPATION)
C
C
C     =========================================
C
C     COMPUTES MATRICES AND SECOND MEMBERS
C
C     =========================================
C
C     ---------------------------
C     DIFFUSION MATRIX FOR AM1
C     ---------------------------
C
98      CALL OS( 'X=YZ    ' , T1 , C , CG , CBID )
      CALL MATRIX(AM1,'M=N     ','MATDIF          ',IELM,IELM,
     &            1.D0,S,S,S,T1,T1,S,MESH,MSK,MASKEL)
C
C-----------------------------------------------------------------------
C
C PANCHANG, TO BE REVISITED: 7 IS GMRES
C
C THE DIFFUSION MATRIX USED FOR PRECONDITIONING IS STORED
C IF THE METHOD IS THAT OF PANCHANG ET AL. (ISOLVE(1) =7)
C
C     IF (ISOLVE(1).EQ.7) THEN
C
C        CALL OM('M=CN    ',AM3,AM1,Z,1.D0/(RELAX*(2.D0-RELAX)),MESH)
C
C     ENDIF
C
C-----------------------------------------------------------------------
C
C     -----------------------
C     MASS MATRIX FOR AM1
C     -----------------------
C


      CALL OS( 'X=Y/Z   ' , T1 , CG , C , CBID )
      CALL MATRIX(AM2,'M=N     ','FMATMA          ', IELM , IELM ,
     &            OMEGA**2 , T1,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     --------------------------------------------------
C     COMPUTES DIFFUSION MATRIX - MASS MATRIX
C     --------------------------------------------------
C
      CALL OM( 'M=M+CN  ' , AM1 , AM2 , C , -1.D0 , MESH )
C
C     --------------------------------
C     ADDS THE BOUNDARY TERM TO AM1
C     --------------------------------
C
C     (HERE T1 IS A DUMMY STRUCTURE)
C
C        ------------------------------
C        BOUNDARY TERM: INCIDENT WAVE
C     ------------------------------
C
      IF (NPTFR .GT. 0) THEN

         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI1B,S,S,S,S,S,MESH,.TRUE.,MASK1)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
C     ------------------------------
C     BOUNDARY TERM: FREE EXIT
C     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
C     ------------------------------
C     BOUNDARY TERM : SOLID BOUNDARY
C     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)

         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
C     ------------------------------
C     BOUNDARY TERM: IMPOSED WAVE
C     ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,BPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
         CALL OM( 'M=M+N   ' , AM1 , MBOR , T1 , CBID , MESH )
      END IF
C
C     ---------------------
C     SECOND MEMBERS : CV1
C     ---------------------
C
         CALL OS( 'X=C     ' , CV1, SBID , SBID , 0.D0 )
C     ------------------------------
C     BOUNDARY TERM: INCIDENT WAVE
C     ------------------------------
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,CPHI1B,T2,0.D0)

         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         CALL OS( 'X=CY    ' , T1,TETAB,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=SIN(Y)' , T3,T1,SBID,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,CPHI1B,S,S,T2,T3,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
C     ------------------------------
C     BOUNDARY TERM: FREE EXIT
C     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
C     END IF
            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
C     ------------------------------
C     BOUNDARY TERM: SOLID BOUNDARY
C     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)

            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF
C     ------------------------------
C     BOUNDARY TERM: IMPOSED WAVE
C     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,CPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)

            CALL OSDB( 'X=X+Y   ' , CV1 , T1 , SBID , CBID , MESH )
         END IF


C     ---------------------
C     SECOND MEMBERS : CV2
C     ---------------------
C
         CALL OS( 'X=C     ' , CV2, SBID , SBID , 0.D0 )
C     ------------------------------
C     BOUNDARY TERM: INCIDENT WAVE
C     ------------------------------
         CALL OS( 'X=CY    ' , T1,TETAP,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=YZ    ' , T3,DPHI1B,T2,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           -1.D0,T3,S,S,S,S,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         CALL OS( 'X=CY    ' , T1,TETAB,SBID,DEGRAD)
         CALL OS( 'X=COS(Y)' , T2,T1,SBID,0.D0)
         CALL OS( 'X=SIN(Y)' , T3,T1,SBID,0.D0)
         CALL OS( 'X=C     ' , T1, SBID , SBID , 0.D0 )
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','FLUBDF          ',IELMB,
     &           1.D0,DPHI1B,S,S,T2,T3,S,MESH,.TRUE.,MASK1)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )



C        ------------------------------
C        BOUNDARY TERM: FREE EXIT
C        ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         END IF
         CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
C     ------------------------------
C     BOUNDARY TERM: SOLID BOUNDARY
C     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
            CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )
         END IF
C     ------------------------------
C     BOUNDARY TERM: IMPOSED WAVE
C     ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL VECTOR(T1,'=','MASVEC          ',IELMB,
     &           1.D0,DPHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)
            CALL OSDB( 'X=X+Y   ' , CV2 , T1 , SBID , CBID , MESH )

         END IF

C
C     ----------------------------------------------------------
C     COMPUTES THE MATRIX BM1 FOR THE MU VALUES SPECIFIED
C     FOR THE ITERATION 'ITERMU'
C     ----------------------------------------------------------
C
      CALL OS( 'X=YZ    ' , T1 , C  , CG , CBID )
      CALL OS( 'X=YZ    ' , T2 , K  , MU , CBID )
      CALL OS( 'X=YZ    ' , T1 , T1 , T2 , CBID )
      CALL MATRIX(BM1,'M=N     ','FMATMA          ', IELM , IELM ,
     &            1.D0 , T1,S,S,S,S,S,MESH,MSK,MASKEL)
C
C     -------------------------------------------
C     ADDS THE BOUNDARY TERM TO BM1
C     -------------------------------------------
C
C      IF (NPTFR .GT. 0) THEN
      IF (NPTFR .GT. 0) THEN
C        ------------------------------
C        BOUNDARY TERM: INCIDENT WAVE
C        ------------------------------
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI1B,S,S,S,S,S,MESH,.TRUE.,MASK1)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
C     ------------------------------
C        BOUNDARY TERM: FREE EXIT
C        ------------------------------
      IF (NPTFR .GT. 0) THEN
            CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &           -1.D0,APHI2B,S,S,S,S,S,MESH,.TRUE.,MASK2)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
C        ------------------------------
C        BOUNDARY TERM: SOLID BOUNDARY
C        ------------------------------
      IF (NPTFR .GT. 0) THEN
         CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &        -1.D0,APHI3B,S,S,S,S,S,MESH,.TRUE.,MASK3)
         CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
      END IF
C        ------------------------------
C        BOUNDARY TERM: IMPOSED WAVE
C        ------------------------------
         IF (NPTFR .GT. 0) THEN
            CALL MATRIX(MBOR,'M=N     ','FMATMA          ',IELMB,IELMB,
     &           -1.D0,APHI4B,S,S,S,S,S,MESH,.TRUE.,MASK4)

            CALL OM( 'M=M+N   ' , BM1 , MBOR , T1 , CBID , MESH )
         END IF
C     ---------
C     AM2 = AM1
C     ---------
C
      CALL OM( 'M=N     ' , AM2 , AM1 , SBID , CBID , MESH )
C
C     --------------------------
C     BM1 BECOMES NONSYMMETRICAL
C     --------------------------
C
      CALL OM( 'M=X(M)  ' , BM1 , BM1 , SBID , CBID , MESH )
C
C     ----------------------------
C     TRIES MASS-LUMPING OF BM1
C     ----------------------------
C
C     MASLU = 1.D0
C     CALL LUMP(T1,BM1,MESH,XMESH,MASLU,MSK,MASKEL)
C     CALL OM( 'M=CN    ' , BM1 , BM1 , T1 , 1.D0-MASLU , MESH )
C     CALL OM( 'M=M+D   ' , BM1 , BM1 , T1 , C          , MESH )
C
C     ----------
C     BM2 = -BM1
C     ----------
C
      CALL OM( 'M=CN    ' , BM2 , BM1 , C , -1.D0 , MESH )
C
C     =======================================
C
C     TAKES INTO ACCOUNT DIRICHLET POINTS
C
C     =======================================
C
      IF (DEFERL .OR. FROTTE) THEN
         IF (LNG.EQ.1) WRITE(LU,220) ITERMU+1
         IF (LNG.EQ.2) WRITE(LU,221) ITERMU+1
 220     FORMAT(/,1X,'SOUS-ITERATION NUMERO :',1X,I3,/)
 221     FORMAT(/,1X,'SUB-ITERATION NUMBER :',1X,I3,/)
      ENDIF
      CALL DIRICH(UNK,MAT,RHS,PHIB,LIDIR%I,TB,MESH,KENT,MSK,MASKEL)
C
C     ===============================================================
C
C     INHIBITS POSSIBLE DIAGONAL PRECONDITIONING
C     IF AN ELEMENT OF DAM1 IS NEGATIVE OR NULL
C
C     ===============================================================
C
      CALL CNTPRE(AM1%D%R,NPOIN,SLVART%PRECON,SLVART%PRECON)
C      IF (LNG.EQ.1) WRITE(LU,230) SLVART%PRECON
C      IF (LNG.EQ.2) WRITE(LU,231) SLVART%PRECON
C 230  FORMAT(/,1X,'PRECONDITIONNEMENT APRES CONTROLE :',1X,I3)
C 231  FORMAT(/,1X,'PRECONDITIONNING AFTER CONTROL :',1X,I3)
C
C     ==========================================================
C
C     PRECONDITIONING BLOCK-DIAGONAL:
C                 THE MATRICES BECOME NONSYMMETRICAL.
C
C     ==========================================================
C
      IF (3*(SLVART%PRECON/3).EQ.SLVART%PRECON) THEN
       CALL OM( 'M=X(M)  ' , AM1 , AM1 , SBID , CBID , MESH )
        CALL OM( 'M=X(M)  ' , AM2 , AM2 , SBID , CBID , MESH )
      ENDIF
C
C     ==============================
C
C     SOLVES THE LINEAR SYSTEM
C
C     ==============================
C
C     ----------------------------
C     INITIALISES THE UNKNOWN
C     ----------------------------
C
      IF(ITERMU.EQ.0.AND.LT.EQ.0) THEN
        CALL LUMP(T1,AM1,MESH,1.D0)
        CALL OS( 'X=Y/Z   ' , PHIR , CV1 , T1 , CBID )
        CALL LUMP(T1,AM2,MESH,1.D0)
        CALL OS( 'X=Y/Z   ' , PHII , CV2 , T1 , CBID )
      ENDIF
C
      IF (LNG.EQ.1) WRITE(LU,240)
      IF (LNG.EQ.2) WRITE(LU,241)
 240  FORMAT(/,1X,'RESOLUTION DU SYSTEME LINEAIRE (SOLVE)',/)
 241  FORMAT(/,1X,'LINEAR SYSTEM SOLVING (SOLVE)',/)
C
      IF(SLVART%SLV.EQ.8 .OR. SLVART%SLV.EQ.9 ) THEN
C
C      CHANGES THE SIGN OF THE SECOND EQUATION
C
       CALL OS('X=-Y    ',X=MAT%ADR(3)%P%D,Y=MAT%ADR(3)%P%D)
       CALL OS('X=-Y    ',X=MAT%ADR(4)%P%D,Y=MAT%ADR(4)%P%D)
       CALL OS('X=-Y    ',X=MAT%ADR(3)%P%X,Y=MAT%ADR(3)%P%X)
       CALL OS('X=-Y    ',X=MAT%ADR(4)%P%X,Y=MAT%ADR(4)%P%X)
       CALL OS('X=-Y    ',X=RHS%ADR(2)%P,Y=RHS%ADR(2)%P)

      ENDIF
C



      CALL SOLVE(UNK,MAT,RHS,TB,SLVART,INFOGR,MESH,AM3)

C
C
C     ============================================================
C
C     COMPUTES THE TOTAL DISSIPATION COEFFICIENT MU_DEFERL + MU_FROTTE
C                                                  (MU2)       (T1)
C     IF BREAKING OR BOTTOM FRICTION TAKEN INTO ACCOUNT
C     ============================================================
C


      IF (DEFERL .OR. FROTTE) THEN
         ECRHMU = 0.D0
         MODHMU = 0.D0
C
C     --------------------------------------------
C     INITIALISES MU2 AND T3: SET TO 0
C     MU2: NEW DISSIPATION COEFFICIENT
C     T3: QB FOR THE CURRENT PERIOD
C     --------------------------------------------
C
         CALL OS( 'X=C     ' , MU2 , SBID , SBID , 0.D0 )
         CALL OS( 'X=C     ' , T3  , SBID , SBID , 0.D0 )
C
C        ----------------------------------------------------
C        COMPUTES THE WAVE HEIGHT HMU CORRESPONDING TO
C        THE SOLUTION OF THE SYSTEM
C

         CALL OS( 'X=N(Y,Z)', T1  , PHIR , PHII , CBID )
         CALL OS( 'X=CY    ', HMU , T1   , SBID , 2.D0*OMEGA/GRAV )


C
C        --------------
C        IF BREAKING
C        --------------
C
         IF (DEFERL) THEN
C
C        ------------------------------------------------------
C        TESTS IF HMU > HM (THERE IS BREAKING) OR NOT,
C        AND CALCULATES MU2 ACCORDING TO DALLY OR BATTJES & JANSSEN
C        (IF REGULAR WAVES)
C        ------------------------------------------------------
C
            IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
               DO 20 I = 1,NPOIN
                  HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
C
C     HMUE = HMU/SQRT(2)
C
                  HMUE = HMU%R(I)/1.4142D0
                  HEFF=MIN(HMUE,HM)
                  HEFF=MAX(HEFF,1.D-5)
                  Q1 = 1.D-10
                  Q2 = (HEFF/HM)**2.D0
C     ADDED BY JMH BECAUSE OF THE LOG FUNCTION, LATER ON
                  Q2 = MAX(Q2,1.D-9)
C
C     ------------
C     COMPUTES QB
C     ------------
C
                  CALL CALCQB(Q1,Q2,Q3)
C

C     ALGORITHM SPECIFIC TO REGULAR WAVES
C     FOR THE COMPUTATION OF THE RATE OF BREAKING
C
                  IF (ITERMU.EQ.0) THEN
                     IF (Q3.LT.0.19D0) THEN
                        T3%R(I) = 0.D0
                     ELSE
                        T3%R(I) = 1.D0
                     ENDIF
C
C                 T3 COMPUTED AT ITERMU = 0
C                 IS TEMPORARILY STORED IN QB
C
                     QB%R(I) = T3%R(I)
                  ELSE
                     IF (QB%R(I).EQ.1.D0) THEN
                        IF (Q3.LT.0.1D0) THEN
                           T3%R(I) = 0.D0
                        ELSE
                           T3%R(I) = 1.D0
                        ENDIF
                     ENDIF
                  ENDIF
 20            CONTINUE


C
C           --------------------------------
C           DALLY AND AL 1985
C           --------------------------------
C
               IF (IBREAK.EQ.2) THEN
                  DO 30 I = 1,NPOIN
                    HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
                     HEFF=MIN(HMU%R(I),HM)
                     HEFF=MAX(HEFF,1.D-5)
                     MU2%R(I)=T3%R(I)*KDALLY*
     &                    (1.D0-(GDALLY*H%R(I)/HEFF)**2.D0)/H%R(I)
 30               CONTINUE
               ENDIF
C
C     -------------------------------------
C     BATTJES & JANSSEN 1978
C     -------------------------------------
C
               IF (IBREAK.EQ.1) THEN
                  DO 40 I = 1,NPOIN
                   HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
                     HEFF=MIN(HMU%R(I),HM)
                     MU2%R(I) = T3%R(I)*2.D0*HEFF/(H%R(I)*CG%R(I)*PER)
 40               CONTINUE
               ENDIF

C
C     -------------------------------------------------------------
C     COMPUTES FIRST QB=T3, PROPORTION OF BREAKING OR BROKEN WAVES,
C     THEN MU2 ACCORDING TO B&J 78 (RANDOM SEAS)
C     -------------------------------------------------------------
C
            ELSE
               DO 50 I = 1,NPOIN
                  HM = 0.88D0/K%R(I)*TANH(GAMMAS*K%R(I)*H%R(I)/0.88D0)
C
C     HMUE = HMU/SQRT (2)
C
                  HMUE = HMU%R(I)/1.4142D0
               HEFF=MIN(HMUE,HM)
               HEFF=MAX(HEFF,1.D-5)
               Q1 = 1.D-10
               Q2 = (HEFF/HM)**2.D0
C     ADDED BY JMH BECAUSE OF THE LOG FUNCTION, LATER ON
               Q2 = MAX(Q2,1.D-9)
C
C              ------------
C              COMPUTES QB
C              ------------
C
               CALL CALCQB(Q1,Q2,Q3)
               T3%R(I) = Q3
C
C              -------------------------
C              COMPUTES MU2
C              -------------------------
C
               HEFF = MIN((HMU%R(I)/1.4142D0),HM)
               MU2%R(I)=ALFABJ*OMEGA*T3%R(I)*((HM/HEFF)**2.D0)/
     &                (3.14159D0*CG%R(I))
 50         CONTINUE


         END IF

C
C        ------------------
C        END 'IF BREAKING'
C        ------------------
C
         ENDIF
C
C        --------------------------------
C        RE-INITIALISES T1 = 0 BECAUSE
C        T1 REPRESENTS MU_FROTTEMENT IN THE FOLLOWING
C        --------------------------------
C
         CALL OS( 'X=C     ' , T1 , C , CG , 0.D0 )
C
C        ---------------------
C        IF BOTTOM FRICTION
C        ---------------------
C
         IF (FROTTE) THEN
C
C           ------------------------------------------------
C           IF ENTFW=TRUE, THE FRICTION COEFFICIENT FW
C           IS THE SAME EVERYWHERE IN THE DOMAIN
C           ------------------------------------------------
C
            IF (ENTFW) THEN
               CALL FWSPEC(FW%R,FWCOEF,MESH%X%R,MESH%Y%R,
     &                     NPOIN,PRIVE,ZF%R)
            ELSE
               DO 70 I = 1,NPOIN
                  CALL CALCFW
     &                   (I,H%R,C%R,CG%R,K%R,HMU%R,
     &                    NPOIN,OMEGA,GRAV,
     &                    VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     &                    FORMFR,REGIDO,RICOEF,
     &                    ENTREG,ENTRUG,FFW)
                  FW%R(I) = FFW
 70            CONTINUE
            ENDIF
C
C           -----------------------------------------
C           COMPUTES THE DISSIPATION COEFFICIENT FOR
C           BOTTOM FRICTION
C           -----------------------------------------
C
            IF (FORMFR .EQ. 1) THEN
C
C           ---------------------------------------------------
C           COMPUTES AN EFFECTIVE SPEED
C           UE = 1.2*(0.5*((DPHIR/DX)**2 + (DPHIR/DY)**2
C                         +(DPHII/DX)**2 + (DPHII/DY)**2))**0.5
C           UE IS STORED IN T4 HERE
C           ---------------------------------------------------
C
               CALL CALCUE
C
C              ----------------------------------------
C              THE DISSIPATION COEFFICIENT MU FOR
C              FRICTION IS STORED IN T1
C              ----------------------------------------
C
               CALL OS( 'X=C     ' , T1 , SBID , SBID , 0.D0 )
C
               DO 80 I = 1,NPOIN
                  T1%R(I) = (0.5D0*FW%R(I)*T4%R(I))/
     &                    (H%R(I)*((COSH(K%R(I)*H%R(I)))**2.D0))
                  T1%R(I) = T1%R(I)/CG%R(I)
 80            CONTINUE
            ENDIF
C
            IF (FORMFR .EQ. 2) THEN
               CALL OS( 'X=C     ' , T1 , SBID , SBID , 0.D0 )
               DO 90 I = 1,NPOIN
                  T1%R(I) = (2*FW%R(I)*HMU%R(I)*
     &                    ((OMEGA/SINH(K%R(I)*H%R(I)))**3.D0))
                  T1%R(I) = T1%R(I)/(3.D0*3.14159D0*GRAV)
                  T1%R(I) = T1%R(I)/CG%R(I)
 90            CONTINUE
            ENDIF
C
C        -------------------------
C        END 'IF BOTTOM FRICTION'
C        -------------------------
C
         END IF
C
C        -------------------------------------------------------
C        RELAXATION ON MU2 TO TRY AND AVOID OSCILLATIONS IN THE
C        CONVERGENCE OF THE SOLVEUR
C        -------------------------------------------------------
C


         DO 60 I = 1,NPOIN
C
C           --------------------------
C           MU = MU_DEFERL + MU_FROTTE
C           --------------------------
C
            MU2%R(I) = MU2%R(I) + T1%R(I)
C
C           ----------
C           RELAXATION
C           ----------
C
            MU2%R(I) = MU%R(I) + RELDIS * (MU2%R(I) - MU%R(I))
            IF (ITERMU.EQ.0) THEN
               HMUANC%R(I) = HMU%R(I)
               ECRHMU = 1000000.D0
               MODHMU = 1.D0
               MU%R(I) = MU2%R(I)
            ELSE
               ECRHMU = MAX(ECRHMU,ABS(HMU%R(I)-HMUANC%R(I)))
               MODHMU = MAX(MODHMU,ABS(HMU%R(I)))
               MU%R(I) = MU2%R(I)
               HMUANC%R(I) = HMU%R(I)
            ENDIF
 60      CONTINUE




C        RELAXES THE RELAXATION AT EACH SUB-ITERATION
C        TO FACILITATE CONVERGENCE OF THE ALGORITHM USED TO
C        COMPUTE DISSIPATION (REGULAR WAVES)
C
         IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
            RELDIS = RELDIS * 0.85D0
         ENDIF
C
         IF (NCSIZE .NE. 0) THEN
            ECRHMU = P_DMAX(ECRHMU)
            MODHMU = P_DMAX(MODHMU)
         END IF
         IF (LNG.EQ.1) WRITE(LU,*) 'ECART ENTRE DEUX
     &        SOUS-ITERATIONS (%)',
     &        100*ECRHMU/MODHMU
         IF (LNG.EQ.2) WRITE(LU,*) 'DIFF. BETWEEN TWO
     &        SUB-ITERATIONS (%) ',
     &        100*ECRHMU/MODHMU
         ITERMU = ITERMU + 1


C
C        -----------------------------------------------------------
C        IF NUMBER OF SUB-ITERATIONS FOR MU >= MAX NUMBER OF SUB-ITERATIONS
C        EXITS THE LOOP OVER MU AND SETS THE RELATIVE DIFFERENCE
C        ECRHMU/MODHMU TO 10 % OF EPSDIS
C        -----------------------------------------------------------
C
         IF (ITERMU.GE.NITDIS) THEN
            IF (LNG.EQ.1) WRITE(LU,100) ITERMU
            IF (LNG.EQ.2) WRITE(LU,101) ITERMU
 100        FORMAT(/,1X,'BERKHO (ARTEMIS): NOMBRE DE SOUS-ITERATIONS',
     & 1X,'MAXIMUM ATTEINT :',1X,I3)
 101        FORMAT(/,1X,'BERKHO (ARTEMIS): YOU REACHED THE MAXIMUM',
     & 1X,'NUMBER OF SUB-ITERATIONS :)',1X,I3)
            ECRHMU = EPSDIS*MODHMU/10.D0
         ENDIF
C
C        ------------------------------------------------
C        CHECKS CONVERGENCE ON THE DISSIPATION LOOP
C        ------------------------------------------------
C
         WRITE(LU,*) ' '
         WRITE(LU,*) '----------------------------------------------- '
         IF (ECRHMU.GT.EPSDIS*MODHMU) GOTO 98
C
         IF (.NOT. ALEMON .AND. .NOT. ALEMUL) THEN
            CALL OS( 'X=Y     ', QB,T3,SBID,CBID)
         ELSE
            CALL OS( 'X=X+Y   ', QB,T3,SBID,CBID)
         ENDIF

C
         IF (LNG.EQ.1) WRITE(LU,200) ITERMU
         IF (LNG.EQ.2) WRITE(LU,201) ITERMU
 200     FORMAT(/,1X,'NOMBRE DE SOUS-ITERATIONS POUR LA DISSIPATION:',
     &   1X,I3)
 201     FORMAT(/,1X,'NUMBER OF SUB-ITERATIONS FOR DISSIPATION:',
     &   1X,I3)
C
C     ========================================
C
C     END 'IF BREAKING OR BOTTOM FRICTION'
C
C     ========================================
C
      ENDIF
C
C END OF THE ITERATIVE LOOP ON THE DISSIPATION TERM MU
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C