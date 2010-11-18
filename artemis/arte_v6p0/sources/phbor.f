C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TRANSLATES THE BOUNDARY CONDITIONS SPECIFIED
!>                BY THE USER,
!>                I.E. COMPUTES THE COEFFICIENTS
!>                APHIR, APHII, ... FOR EACH BOUNDARY SEGMENT.

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
!> @link DECLARATIONS_ARTEMIS::ALFAP ALFAP@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALFAPT ALFAPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI1B APHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI2B APHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI3B APHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::APHI4B APHI4B@endlink, 
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
!> @link DECLARATIONS_ARTEMIS::DPHI1B DPHI1B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI2B DPHI2B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI3B DPHI3B@endlink, 
!> @link DECLARATIONS_ARTEMIS::DPHI4B DPHI4B@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::HB HB@endlink, 
!> @link DECLARATIONS_ARTEMIS::HBT HBT@endlink, 
!> @link DECLARATIONS_ARTEMIS::K K@endlink, 
!> @link DECLARATIONS_ARTEMIS::KP1BOR_TOT KP1BOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::KT KT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIDIR LIDIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIDIRT LIDIRT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBORT LIHBORT@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::NBOR_TOT NBOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR_TOT NPTFR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::OMEGA OMEGA@endlink, 
!> @link DECLARATIONS_ARTEMIS::RP RP@endlink, 
!> @link DECLARATIONS_ARTEMIS::RPT RPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAB TETAB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETABT TETABT@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAP TETAP@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAPT TETAPT@endlink, 
!> @link DECLARATIONS_ARTEMIS::X X@endlink, 
!> @link DECLARATIONS_ARTEMIS::XT XT@endlink, 
!> @link DECLARATIONS_ARTEMIS::Y Y@endlink, 
!> @link DECLARATIONS_ARTEMIS::YT YT@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KINC KINC@endlink, 
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> APHI1BT, APHI2BT, APHI3BT, APHI4BT, AUXI1, AUXI2, AUXIC, AUXIS, BID, BPHI1BT, BPHI2BT, BPHI3BT, BPHI4BT, CPHI1BT, CPHI2BT, CPHI3BT, CPHI4BT, DEGRAD, DPHI1BT, DPHI2BT, DPHI3BT, DPHI4BT, I, IFR, IFROI, IG, IG0, IGP1, IOIDEB, IOIFIN, IPREC, ITOTO, J, PHASOI, PI, RADDEG, TRVDEB
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GLOBAL_TO_LOCAL_BOUND(), GLOBAL_TO_LOCAL_BOUNDI()
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
!>    <td><center> 5.1                                    </center></td>
!>    <td> 21/08/2000                                              </td>
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PHBOR
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
      LOGICAL TRVDEB
C
      INTEGER I,IPREC,IG,IG0,IGP1,IFR,IOIDEB(5),IOIFIN(5),ITOTO,IFROI
C
      DOUBLE PRECISION PI,DEGRAD
      DOUBLE PRECISION AUXI1,AUXI2,PHASOI,AUXIC,AUXIS,RADDEG,BID
C
      DOUBLE PRECISION, ALLOCATABLE ::  APHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI1BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  APHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI2BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  APHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI3BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  APHI4BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  BPHI4BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  CPHI4BT(:)
      DOUBLE PRECISION, ALLOCATABLE ::  DPHI4BT(:)


      INTRINSIC COS,SIN
C
C-----------------------------------------------------------------------
C
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
C
C-----------------------------------------------------------------------
C
C INITIALISES LIDIR TO KSORT (A DIFFERENT VALUE FROM KENT)
C IN ORDER NOT TO TAKE NODES IMPOSED IN PRIDIH INTO ACCOUNT,
C WHEN IT HAS NOT BEEN REQUESTED.
C


      IF (NCSIZE .GT. 1) THEN

      ALLOCATE(APHI1BT(NPTFR_TOT))
      ALLOCATE(BPHI1BT(NPTFR_TOT))
      ALLOCATE(CPHI1BT(NPTFR_TOT))
      ALLOCATE(DPHI1BT(NPTFR_TOT))
      ALLOCATE(APHI2BT(NPTFR_TOT))
      ALLOCATE(BPHI2BT(NPTFR_TOT))
      ALLOCATE(CPHI2BT(NPTFR_TOT))
      ALLOCATE(DPHI2BT(NPTFR_TOT))
      ALLOCATE(APHI3BT(NPTFR_TOT))
      ALLOCATE(BPHI3BT(NPTFR_TOT))
      ALLOCATE(CPHI3BT(NPTFR_TOT))
      ALLOCATE(DPHI3BT(NPTFR_TOT))
      ALLOCATE(APHI4BT(NPTFR_TOT))
      ALLOCATE(BPHI4BT(NPTFR_TOT))
      ALLOCATE(CPHI4BT(NPTFR_TOT))
      ALLOCATE(DPHI4BT(NPTFR_TOT))
      ALLOCATE(LIDIRT(2*NPTFR_TOT))


      DO I=1,MESH%NPTFR
         APHI1B%R(I) = 0.D0
         BPHI1B%R(I) = 0.D0
         CPHI1B%R(I) = 0.D0
         DPHI1B%R(I) = 0.D0
         APHI2B%R(I) = 0.D0
         BPHI2B%R(I) = 0.D0
         CPHI2B%R(I) = 0.D0
         DPHI2B%R(I) = 0.D0
         APHI3B%R(I) = 0.D0
         BPHI3B%R(I) = 0.D0
         CPHI3B%R(I) = 0.D0
         DPHI3B%R(I) = 0.D0
         APHI4B%R(I) = 0.D0
         BPHI4B%R(I) = 0.D0
         CPHI4B%R(I) = 0.D0
         DPHI4B%R(I) = 0.D0
      END DO




        DO I=1,NPTFR_TOT
           LIDIRT(I) = KSORT
C     BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
           LIDIRT(I+NPTFR_TOT) = KSORT
           IF (LIHBORT(I).EQ.KENT) THEN
              LIHBORT(I) = KINC
           ENDIF
           APHI1BT(I) = 0.D0
           BPHI1BT(I) = 0.D0
           CPHI1BT(I) = 0.D0
           DPHI1BT(I) = 0.D0
           APHI2BT(I) = 0.D0
           BPHI2BT(I) = 0.D0
           CPHI2BT(I) = 0.D0
           DPHI2BT(I) = 0.D0
           APHI3BT(I) = 0.D0
           BPHI3BT(I) = 0.D0
           CPHI3BT(I) = 0.D0
           DPHI3BT(I) = 0.D0
           APHI4BT(I) = 0.D0
           BPHI4BT(I) = 0.D0
           CPHI4BT(I) = 0.D0
           DPHI4BT(I) = 0.D0

        END DO



C
C-----------------------------------------------------------------------
C
C
C     ************************************************
C     INITIALISES THE PHASE FOR INCIDENT WAVES
C     ************************************************
C
      PHASOI = 0.D0
C
C     ******************************************
C     PARTICULAR TREATMENT FOR INCIDENT WAVES
C     ******************************************
C
C     -------------------------------------------------
C     LOCATES THE BEGINNINGS OF THE INCIDENT WAVE BOUNDARY
C     -------------------------------------------------
C
      TRVDEB = .TRUE.
      IFROI = 0
C
      DO 10 I=1,NPTFR_TOT
         IF (LIHBORT(I).EQ.KINC) THEN
            ITOTO = KP1BOR_TOT(I+NPTFR_TOT)
               IF (LIHBORT(ITOTO).NE.KINC) THEN
                  IFROI = IFROI + 1
                  IOIDEB(IFROI) = I
               ENDIF
         ENDIF
 10   CONTINUE

C
      IF(LNG.EQ.1) WRITE(LU,11) IFROI
      IF(LNG.EQ.2) WRITE(LU,12) IFROI
11    FORMAT(1X,'PHBOR : IL Y A : ',1I3,' FRONTIERE(S) ',
     &       1X,'DE TYPE ONDE INCIDENTE ')
12    FORMAT(1X,'PHBOR : THERE ARE :',1I3,' BOUNDARIE(S) ',
     &       1X,'OF INCIDENT WAVE TYPE ')
C
C     --------------------------------------------------------------
C     COMPUTES THE COEFFICIENTS FOR INCIDENT WAVE BOUNDARIES
C     FROM IOIDEB (BEGINNING OF INCIDENT WAVE)
C     --------------------------------------------------------------
C


      DO 15 IFR=1,IFROI
         I = IOIDEB(IFR)

C
 20   CONTINUE
C
C        ********************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE I
C        ********************************
C
C         IG   = MESH%NBOR%I(I)
      IG   = NBOR_TOT(I)
C
C        ******************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
C        ******************************************
C
      IG0  = NBOR_TOT(KP1BOR_TOT(I+NPTFR_TOT))
C
C        ****************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
C        ****************************************
C

      IGP1 = NBOR_TOT(KP1BOR_TOT(I))


         AUXIC      = COS(TETABT(I)*DEGRAD)
         AUXIS      = SIN(TETABT(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HBT(I)/2.D0 *
     &                CTT(IG) * CGT(IG) * KT(IG)
C
C
C           DEVELOPMENT FOR NON-UNIFORM DIRECTION
C           PHASOI IS THE PHASE
C
C           PREVIOUS FORMULATION : PHASE = K * X (PLANE WAVE) :
C
C                     PHASOI = KM * ( XM*AUXIC + YM*AUXIS )
C
C           NEW FORMULATION (NON PLANE WAVE) :
C
C                                   M
C                                 /
C                    PHASOI(M) = /        K(N) * DX(N)
C                               /
C                                MDEB
C
C           WHERE MDEB IF THE BEGINNING (NODE) OF AN INCIDENT WAVE SEGMENT,
C           FROM WHICH THE PHASE IS COMPUTED. THE ABOVE INTEGRAL IS
C           COMPUTED BY LINEAR APPROXIMATION.
C
C           THE POSSIBLE DEPHASING ALFAP IS ADDED TO ENSURE COHERENCE
C           BETWEEN THE PHASES OF THE DIFFERENT WAVE  CRESTS IF SEVERAL
C           NONCONTINUOUS INCIDENT WAVE BOUNDARIES EXIST
C
      PHASOI = PHASOI + KT(IG)*AUXIC*(XT(IG) - XT(IG0))
     &                + KT(IG)*AUXIS*(YT(IG) - YT(IG0))
C
      APHI1BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &             * COS(TETAPT(I)*DEGRAD)
C
      BPHI1BT(I)  = 0.D0
C
      CPHI1BT(I)  = AUXI1 * COS( PHASOI + ALFAPT(I)*DEGRAD )
C
      DPHI1BT(I)  = AUXI1 * SIN( PHASOI + ALFAPT(I)*DEGRAD )
C
      I = KP1BOR_TOT(I)
C
C     UNTIL THE NODE FOLLOWING THE END OF AN INCIDENT WAVE BND IS REACHED
C
      IF (LIHBORT(I).NE.KINC) THEN
         IOIFIN(IFR) = I

         IPREC      = KP1BOR_TOT(I+NPTFR_TOT)

         TETABT(I) = TETABT(IPREC)
         HBT(I)    = HBT(IPREC)
         AUXIC      = COS(TETABT(I)*DEGRAD)
         AUXIS      = SIN(TETABT(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HBT(I)/2.D0 *
     &                CTT(IG) * CGT(IG) * KT(IG)
         PHASOI = PHASOI + KT(IG)*AUXIC*(XT(IG) - XT(IG0))
     &                   + KT(IG)*AUXIS*(YT(IG) - YT(IG0))
C
         APHI1BT(I) = - KT(IG) * CTT(IG) * CGT(IG)
     &                 * COS(TETAPT(IPREC)*DEGRAD)
C
         BPHI1BT(I) = 0.D0
C
         CPHI1BT(I) = AUXI1*COS(PHASOI + ALFAPT(IPREC)*DEGRAD)
C
         DPHI1BT(I) = AUXI1*SIN(PHASOI + ALFAPT(IPREC)*DEGRAD)
C
         GOTO 15
C
      ELSE
         GOTO 20
      ENDIF
C
15    CONTINUE
C
C     ******************************************
C     END OF TREATMENT OF THE INCIDENT WAVE BOUNDARY
C     ******************************************
C
      DO 100 I=1,NPTFR_TOT
C
C        ********************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE I
C        ********************************
C
         IG   = NBOR_TOT(I)
C
C        ******************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
C        ******************************************
C
         IG0  = NBOR_TOT(KP1BOR_TOT(I+NPTFR_TOT))
C
C        ****************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
C        ****************************************
C
         IGP1 = NBOR_TOT(KP1BOR_TOT(I))
CCPHI1B%R
C        -------------------------------------------------
C        COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
C        -------------------------------------------------
C
         IF (LIHBORT(I).EQ.KSORT) THEN
            APHI2BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &                   * COS(TETAPT(I)*DEGRAD)
C
             BPHI2BT(I)  = 0.D0
C
            CPHI2BT(I)  = 0.D0
C
            DPHI2BT(I)  = 0.D0
C
         ELSEIF (LIHBORT(KP1BOR_TOT(I)).EQ.KSORT) THEN
            APHI2BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &                   * COS(TETAPT(KP1BOR_TOT(I))*DEGRAD)
C
             BPHI2BT(I)  = 0.D0
C
            CPHI2BT(I)  = 0.D0
C
            DPHI2BT(I)  = 0.D0
C
         ELSEIF (LIHBORT(KP1BOR_TOT(I+NPTFR_TOT)).EQ.KSORT) THEN
            APHI2BT(I)  = - KT(IG) * CTT(IG) * CGT(IG)
     &              * COS(TETAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD)
C
             BPHI2BT(I)  = 0.D0
C
            CPHI2BT(I)  = 0.D0
C
            DPHI2BT(I)  = 0.D0
C
         ELSE
            APHI2BT(I)  = 0.D0
C
             BPHI2BT(I)  = 0.D0
C
            CPHI2BT(I)  = 0.D0
C
            DPHI2BT(I)  = 0.D0
C
         ENDIF
C
C        -------------------------------------------
C        COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
C        -------------------------------------------
C
         IF (LIHBORT(I).EQ.KLOG) THEN
          AUXI1 = KT(IG) * CTT(IG) * CGT(IG) *
     &      COS(TETAPT(I)*DEGRAD) /
     &      ( 1.D0 + RPT(I)*RPT(I) +
     &        2.D0*RPT(I)*COS(ALFAPT(I)*DEGRAD) )
C
          APHI3BT(I) = - (1.D0 - RPT(I) * RPT(I) ) * AUXI1
C
          BPHI3BT(I) = 2.D0*RPT(I)*SIN(ALFAPT(I)*DEGRAD) * AUXI1
C
          CPHI3BT(I)  = 0.D0
C
          DPHI3BT(I)  = 0.D0
C
         ELSEIF (LIHBORT(KP1BOR_TOT(I)).EQ.KLOG) THEN
          AUXI1 = KT(IG) * CTT(IG) * CGT(IG) *
     &      COS(TETAPT(KP1BOR_TOT(I))*DEGRAD) /
     &      (1.D0 + RPT(KP1BOR_TOT(I))*RPT(KP1BOR_TOT(I))
     &      +2.D0 * RPT(KP1BOR_TOT(I))*
     &       COS(ALFAPT(KP1BOR_TOT(I))*DEGRAD))
C
          APHI3BT(I) = - (1.D0-RPT(KP1BOR_TOT(I))*
     &      RPT(KP1BOR_TOT(I))) * AUXI1
C
          BPHI3BT(I) = 2.D0*RPT(KP1BOR_TOT(I))
     &                * SIN(ALFAPT(KP1BOR_TOT(I))*DEGRAD) * AUXI1
C
          CPHI3BT(I)  = 0.D0
C
          DPHI3BT(I)  = 0.D0
C
         ELSEIF (LIHBORT(KP1BOR_TOT(I+NPTFR_TOT)).EQ.KLOG) THEN
          AUXI1 = KT(IG) * CTT(IG) * CGT(IG) *
     &     COS(TETAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD) /
     &     (1.D0 + RPT(KP1BOR_TOT(I+NPTFR_TOT))
     &      *RPT(KP1BOR_TOT(I+NPTFR_TOT))
     &      +2.D0 * RPT(KP1BOR_TOT(I+NPTFR_TOT))*
     &       COS(ALFAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD))
C
          APHI3BT(I) = - (1.D0-RPT(KP1BOR_TOT(I+NPTFR_TOT))*
     &      RPT(KP1BOR_TOT(I+NPTFR_TOT))) * AUXI1
C
          BPHI3BT(I) = 2.D0*RPT(KP1BOR_TOT(I+NPTFR_TOT))
     &      * SIN(ALFAPT(KP1BOR_TOT(I+NPTFR_TOT))*DEGRAD) * AUXI1
C
          CPHI3BT(I)  = 0.D0
C
          DPHI3BT(I)  = 0.D0
C
         ELSE
          APHI3BT(I)  = 0.D0
C
          BPHI3BT(I)  = 0.D0
C
          CPHI3BT(I)  = 0.D0
C
          DPHI3BT(I)  = 0.D0
C
         ENDIF
C
C        -------------------------------------------------
C        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
C        -------------------------------------------------
CDA      -----------------------------------
CDA      KEPT FOR MEMORY!
CDA      -----------------------------------
CDA
CDA         IF (LIHBOR(I).EQ.KENT) THEN
CDA         AUXIC      = COS(TETAB(I)*DEGRAD)
CDA         AUXIS      = SIN(TETAB(I)*DEGRAD)
CDA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
CDA     *                K(IG) * ( AUXIC *XSGBOR(I) +
CDA     *                          AUXIS *YSGBOR(I) )
CDA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
CDA     *                          Y(IG)*AUXIS )
CDA
CDA         APHI4B(I)  = 0.D0
CDA
CDA         BPHI4B(I)  = 0.D0
CDA
CDA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
CDA
CDA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
CDA
CDA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
CDA         LIDIR(I)         = KENT
CDA
CDA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
CDA         AUXI2 = K(IG) * (X(IG)*AUXIC +
CDA     *                    Y(IG)*AUXIS )
CDA
CDA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
CDA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
CDA         ENDIF
C
C
100   CONTINUE
C-----------------------------------------------------------------------
C
        CALL GLOBAL_TO_LOCAL_BOUND(APHI1BT,APHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI1BT,BPHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI1BT,CPHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI1BT,DPHI1B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(APHI2BT,APHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI2BT,BPHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI2BT,CPHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI2BT,DPHI2B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(APHI3BT,APHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI3BT,BPHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI3BT,CPHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI3BT,DPHI3B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(APHI4BT,APHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(BPHI4BT,BPHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(CPHI4BT,CPHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(DPHI4BT,DPHI4B,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUNDI(LIDIRT,LIDIR,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(TETAPT,TETAP,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(TETABT,TETAB,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(RPT,RP,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(HBT,HB,MESH%NPTFR,NPTFR_TOT)
        CALL GLOBAL_TO_LOCAL_BOUND(ALFAPT,ALFAP,MESH%NPTFR,NPTFR_TOT)
C        CALL GLOBAL_TO_LOCAL_BOUND2(CTT,C,MESH%NPOIN,NPOIN_TOT)
C        CALL GLOBAL_TO_LOCAL_BOUND2(CGT,CG,MESH%NPOIN,NPOIN_TOT)
C        CALL GLOBAL_TO_LOCAL_BOUND2(KT,K,MESH%NPOIN,NPOIN_TOT)


        DEALLOCATE(APHI1BT)
        DEALLOCATE(BPHI1BT)
        DEALLOCATE(CPHI1BT)
        DEALLOCATE(DPHI1BT)
        DEALLOCATE(APHI2BT)
        DEALLOCATE(BPHI2BT)
        DEALLOCATE(CPHI2BT)
        DEALLOCATE(DPHI2BT)
        DEALLOCATE(APHI3BT)
        DEALLOCATE(BPHI3BT)
        DEALLOCATE(CPHI3BT)
        DEALLOCATE(DPHI3BT)
        DEALLOCATE(APHI4BT)
        DEALLOCATE(BPHI4BT)
        DEALLOCATE(CPHI4BT)
        DEALLOCATE(DPHI4BT)

        DEALLOCATE(LIDIRT)
C        DEALLOCATE(XT)
C        DEALLOCATE(YT)
C        DEALLOCATE(CTT)
C        DEALLOCATE(KT)
C        DEALLOCATE(CGT)
      DO 110 IFR = 1,IFROI
         I          = IOIFIN(IFR)
         IPREC      = KP1BOR_TOT(I+NPTFR_TOT)
         TETAPT(I) = TETAPT(IPREC)
110   CONTINUE
C
      ELSE




        DO I=1,NPTFR
                LIHBOR%I(I)=LIHBORT(I)
                RP%R(I)=RPT(I)
                HB%R(I)=HBT(I)
                ALFAP%R(I)=ALFAPT(I)
                TETAB%R(I)=TETABT(I)
                TETAP%R(I)=TETAPT(I)
         END DO



         DO 501 I=1,NPTFR
        LIDIR%I(I) = KSORT
C       BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
        LIDIR%I(I+NPTFR) = KSORT
        IF (LIHBOR%I(I).EQ.KENT) THEN
           LIHBOR%I(I) = KINC
        ENDIF
        APHI1B%R(I) = 0.D0
        BPHI1B%R(I) = 0.D0
        CPHI1B%R(I) = 0.D0
        DPHI1B%R(I) = 0.D0
        APHI2B%R(I) = 0.D0
        BPHI2B%R(I) = 0.D0
        CPHI2B%R(I) = 0.D0
        DPHI2B%R(I) = 0.D0
        APHI3B%R(I) = 0.D0
        BPHI3B%R(I) = 0.D0
        CPHI3B%R(I) = 0.D0
        DPHI3B%R(I) = 0.D0
        APHI4B%R(I) = 0.D0
        BPHI4B%R(I) = 0.D0
        CPHI4B%R(I) = 0.D0
        DPHI4B%R(I) = 0.D0
 501    CONTINUE
C
C-----------------------------------------------------------------------
C
C
C     ************************************************
C     INITIALISES THE PHASE FOR INCIDENT WAVES
C     ************************************************
C
      PHASOI = 0.D0
C
C     ******************************************
C     PARTICULAR TREATMENT FOR INCIDENT WAVES
C     ******************************************
C
C     -------------------------------------------------
C     LOCATES THE BEGINNINGS OF THE INCIDENT WAVE BOUNDARY
C     -------------------------------------------------
C
      TRVDEB = .TRUE.
      IFROI = 0
C
      DO 101 I=1,NPTFR
         IF (LIHBOR%I(I).EQ.KINC) THEN
               ITOTO = MESH%KP1BOR%I(I+NPTFR)
               IF (LIHBOR%I(ITOTO).NE.KINC) THEN
                  IFROI = IFROI + 1
                  IOIDEB(IFROI) = I
               ENDIF
         ENDIF
 101  CONTINUE


C
      IF(LNG.EQ.1) WRITE(LU,111) IFROI
      IF(LNG.EQ.2) WRITE(LU,121) IFROI
 111  FORMAT(1X,'PHBOR : IL Y A : ',1I3,' FRONTIERE(S) ',
     &       1X,'DE TYPE ONDE INCIDENTE ')
 121  FORMAT(1X,'PHBOR : THERE ARE :',1I3,' BOUNDARIE(S) ',
     &       1X,'OF INCIDENT WAVE TYPE ')
C
C     --------------------------------------------------------------
C     COMPUTES THE COEFFICIENTS FOR INCIDENT WAVE BOUNDARIES
C     FROM IOIDEB (BEGINNING OF INCIDENT WAVE)
C     --------------------------------------------------------------
C


      DO 151 IFR=1,IFROI
         I = IOIDEB(IFR)
C
 201     CONTINUE
C
C        ********************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE I
C        ********************************
C
         IG   = MESH%NBOR%I(I)
C
C        ******************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
C        ******************************************
C
         IG0  = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
C
C        ****************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
C        ****************************************
C
         IGP1 = MESH%NBOR%I(MESH%KP1BOR%I(I))
C


         AUXIC      = COS(TETAB%R(I)*DEGRAD)
         AUXIS      = SIN(TETAB%R(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HB%R(I)/2.D0 *
     &                C%R(IG) * CG%R(IG) * K%R(IG)
C
C
C           DEVELOPMENT FOR NON-UNIFORM DIRECTION
C           PHASOI IS THE PHASE
C
C           PREVIOUS FORMULATION : PHASE = K * X (PLANE WAVE) :
C
C                     PHASOI = KM * ( XM*AUXIC + YM*AUXIS )
C
C           NEW FORMULATION (NON PLANE WAVE) :
C
C                                   M
C                                 /
C                    PHASOI(M) = /        K(N) * DX(N)
C                               /
C                                MDEB
C
C           WHERE MDEB IF THE BEGINNING (NODE) OF AN INCIDENT WAVE SEGMENT,
C           FROM WHICH THE PHASE IS COMPUTED. THE ABOVE INTEGRAL IS
C           COMPUTED BY LINEAR APPROXIMATION.
C
C           THE POSSIBLE DEPHASING ALFAP IS ADDED TO ENSURE COHERENCE
C           BETWEEN THE PHASES OF THE DIFFERENT WAVE  CRESTS IF SEVERAL
C           NONCONTINUOUS INCIDENT WAVE BOUNDARIES EXIST
C
      PHASOI = PHASOI + K%R(IG)*AUXIC*(X(IG) - X(IG0))
     &                + K%R(IG)*AUXIS*(Y(IG) - Y(IG0))
C
      APHI1B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &             * COS(TETAP%R(I)*DEGRAD)
C
      BPHI1B%R(I)  = 0.D0
C
      CPHI1B%R(I)  = AUXI1 * COS( PHASOI + ALFAP%R(I)*DEGRAD )
C
      DPHI1B%R(I)  = AUXI1 * SIN( PHASOI + ALFAP%R(I)*DEGRAD )
C
      I = MESH%KP1BOR%I(I)
C
C     UNTIL THE NODE FOLLOWING THE END OF AN INCIDENT WAVE BND IS REACHED
C
      IF (LIHBOR%I(I).NE.KINC) THEN
         IOIFIN(IFR) = I
         IPREC      = MESH%KP1BOR%I(I+NPTFR)
         TETAB%R(I) = TETAB%R(IPREC)
         HB%R(I)    = HB%R(IPREC)
         AUXIC      = COS(TETAB%R(I)*DEGRAD)
         AUXIS      = SIN(TETAB%R(I)*DEGRAD)
         AUXI1      = GRAV/OMEGA * HB%R(I)/2.D0 *
     &                C%R(IG) * CG%R(IG) * K%R(IG)
         PHASOI = PHASOI + K%R(IG)*AUXIC*(X(IG) - X(IG0))
     &                   + K%R(IG)*AUXIS*(Y(IG) - Y(IG0))
C
         APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                 * COS(TETAP%R(IPREC)*DEGRAD)
C
         BPHI1B%R(I) = 0.D0
C
         CPHI1B%R(I) = AUXI1*COS(PHASOI + ALFAP%R(IPREC)*DEGRAD)
C
         DPHI1B%R(I) = AUXI1*SIN(PHASOI + ALFAP%R(IPREC)*DEGRAD)
C
         GOTO 151
C
      ELSE
         GOTO 201
      ENDIF
C
 151  CONTINUE
C
C     ******************************************
C     END OF TREATMENT OF THE INCIDENT WAVE BOUNDARY
C     ******************************************
C
      DO 1001 I=1,NPTFR
C
C        ********************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE I
C        ********************************
C
         IG   = MESH%NBOR%I(I)
C
C        ******************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
C        ******************************************
C
         IG0  = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
C
C        ****************************************
C        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
C        ****************************************
C
         IGP1 = MESH%NBOR%I(MESH%KP1BOR%I(I))
C
C        -------------------------------------------------
C        COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
C        -------------------------------------------------
C
         IF (LIHBOR%I(I).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
C
            BPHI2B%R(I)  = 0.D0
C
            CPHI2B%R(I)  = 0.D0
C
            DPHI2B%R(I)  = 0.D0
C
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD)
C
            BPHI2B%R(I)  = 0.D0
C
            CPHI2B%R(I)  = 0.D0
C
            DPHI2B%R(I)  = 0.D0
C
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &              * COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD)
C
            BPHI2B%R(I)  = 0.D0
C
            CPHI2B%R(I)  = 0.D0
C
            DPHI2B%R(I)  = 0.D0
C
         ELSE
            APHI2B%R(I)  = 0.D0
C
            BPHI2B%R(I)  = 0.D0
C
            CPHI2B%R(I)  = 0.D0
C
            DPHI2B%R(I)  = 0.D0
C
         ENDIF
C
C        -------------------------------------------
C        COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
C        -------------------------------------------
C
         IF (LIHBOR%I(I).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(I)*DEGRAD) /
     &      ( 1.D0 + RP%R(I)*RP%R(I) +
     &        2.D0*RP%R(I)*COS(ALFAP%R(I)*DEGRAD) )
C
          APHI3B%R(I) = - (1.D0 - RP%R(I) * RP%R(I) ) * AUXI1
C
          BPHI3B%R(I) = 2.D0*RP%R(I)*SIN(ALFAP%R(I)*DEGRAD) * AUXI1
C
          CPHI3B%R(I)  = 0.D0
C
          DPHI3B%R(I)  = 0.D0
C
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD) /
     &      (1.D0 + RP%R(MESH%KP1BOR%I(I))*RP%R(MESH%KP1BOR%I(I))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD))
C
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I))*
     &      RP%R(MESH%KP1BOR%I(I))) * AUXI1
C
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I))
     &                * SIN(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD) * AUXI1
C
          CPHI3B%R(I)  = 0.D0
C
          DPHI3B%R(I)  = 0.D0
C
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &     COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) /
     &     (1.D0 + RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      *RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD))
C
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &      RP%R(MESH%KP1BOR%I(I+NPTFR))) * AUXI1
C
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      * SIN(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) * AUXI1
C
          CPHI3B%R(I)  = 0.D0
C
          DPHI3B%R(I)  = 0.D0
C
         ELSE
          APHI3B%R(I)  = 0.D0
C
          BPHI3B%R(I)  = 0.D0
C
          CPHI3B%R(I)  = 0.D0
C
          DPHI3B%R(I)  = 0.D0
C
         ENDIF
C
C        -------------------------------------------------
C        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
C        -------------------------------------------------
CDA      -----------------------------------
CDA      KEPT FOR MEMORY !
CDA      -----------------------------------
CDA
CDA         IF (LIHBOR(I).EQ.KENT) THEN
CDA         AUXIC      = COS(TETAB(I)*DEGRAD)
CDA         AUXIS      = SIN(TETAB(I)*DEGRAD)
CDA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
CDA     *                K(IG) * ( AUXIC *XSGBOR(I) +
CDA     *                          AUXIS *YSGBOR(I) )
CDA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
CDA     *                          Y(IG)*AUXIS )
CDA
CDA         APHI4B(I)  = 0.D0
CDA
CDA         BPHI4B(I)  = 0.D0
CDA
CDA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
CDA
CDA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
CDA
CDA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
CDA         LIDIR(I)         = KENT
CDA
CDA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
CDA         AUXI2 = K(IG) * (X(IG)*AUXIC +
CDA     *                    Y(IG)*AUXIS )
CDA
CDA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
CDA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
CDA         ENDIF
C
C
 1001 CONTINUE
C-----------------------------------------------------------------------





      DO 1101 IFR = 1,IFROI
         I          = IOIFIN(IFR)
         IPREC      = MESH%KP1BOR%I(I+NPTFR)
         TETAP%R(I) = TETAP%R(IPREC)
 1101 CONTINUE
C
C      DO I=1,NPTFR
C         ALFAP%R(I)=ALFAPT(I)
C         TETAP%R(I)=TETAPT(I)
C         TETAB%R(I)=TETABT(I)
C         RP%R(I)=RPT(I)
C         HB%R(I)=HBT(I)
C      END DO



      END IF


      WRITE(LU,*) 'END PHBOR'
      RETURN
      CONTAINS
      SUBROUTINE GLOBAL_TO_LOCAL_BOUND(TAB1,OBJ,NPTFR,NPTFR_TOT)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
      INTEGER :: I,J
      OBJ%R=0.0
      DO I=1,NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%R(J)=TAB1(I)
            END IF
         END DO
      END DO
      OBJ%DIM1=NPTFR


      END SUBROUTINE

      SUBROUTINE GLOBAL_TO_LOCAL_BOUNDI(TAB1,OBJ,NPTFR,NPTFR_TOT)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPTFR
      INTEGER, INTENT(IN) :: NPTFR_TOT
      INTEGER :: I,J
      OBJ%I=0.0
      DO I=1,NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%I(J)=TAB1(I)
            END IF
         END DO
      END DO
      DO  I=NPTFR_TOT+1,2*NPTFR_TOT
         DO J=1,MESH%NPTFR
            IF (NBOR_TOT(I/2) .EQ. MESH%KNOLG%I(MESH%NBOR%I(J))) THEN
               OBJ%I(2*J)=TAB1(I)
            END IF
         END DO
      END DO
      END SUBROUTINE

       SUBROUTINE GLOBAL_TO_LOCAL_BOUND2(TAB1,OBJ,NPOIN,NPOIN_TOT)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT)  :: TAB1(:)
      TYPE (BIEF_OBJ), INTENT(INOUT) :: OBJ
      INTEGER, INTENT(IN) :: NPOIN_TOT
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER :: I,J
      OBJ%R=0.0
      DO I=1,NPOIN_TOT
         DO J=1,NPOIN
            IF (I .EQ. MESH%KNOLG%I(J)) THEN
               OBJ%R(J)=TAB1(I)
            END IF
         END DO
      END DO
      OBJ%DIM1=NPOIN

      END SUBROUTINE

      END
C
C#######################################################################
C