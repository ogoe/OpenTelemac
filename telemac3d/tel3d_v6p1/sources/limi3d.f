
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES TYPES OF 3D BOUNDARY CONDITIONS.
!><br>            SETS THE VALUE OF SOME COEFFICIENTS.

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
!> </td><td>
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE LIMI3D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A,B)PBOR(F,L,S |<--| NEUMANN BC VALUES FOR DYNAMIC PRESSURE
C| A,B)WBOR(F,L,S |<--| NEUMANN BC VALUES FOR W VELOCITY COMPONENT
C| AT             |-->| TIME
C| ATABO,BTABO    |<--| LOG LAW: NU*DTA/DN = ATABO*TA + BTABO
C|                |   | TYPES OF BOUNDARY CONDITIONS
C| AUBOR,BUBOR    |<--| LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
C| AUBOR,BVBOR    |<--| LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
C| COTIMP         |-->| ARRAY OF PRESCRIBED ELEVATIONS
C| DEBIMP         |-->| ARRAY OF PRESCRIBED DISCHARGES
C| DT             |-->| TIME-STEP
C| FAIR           |-->| DRAG COEFFICIENT OF WIND
C| HBOR           |-->| PRESCRIBED DEPTH ON LATERAL BOUNDARIES
C| HN             |-->| DEPTH AT TIME TN
C| IKLE3          |-->| CONNECTIVITY TABLE IN 3D
C| ITURBV         |-->| TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
C| KADH           |-->| NO SLIP CONDITION
C| KENT           |-->| PRESCRIBED VALUE
C| KENTU          |-->| PRESCRIBED VELOCITY
C| KLOG           |-->| SOLID BOUNDARY
C| KP1BOR         |-->| IN 2D : NEXT POINT ON THE BOUNDARY
C|                |   | (BOUNDARY NUMBERING)
C| KSORT          |-->| FREE (E.G. AT AN OUTPUT)
C| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
C| LIPBO(F,L,S)   |<--| BC TYPE FOR DYNAMIC PRESSURE
C| LITA,BF        |<->| ON BOTTOM FOR TRACERS
C| LITA,BL        |<->| ON LATERAL BOUNDARIES FOR TRACERS
C| LITA,BS        |<->| AT FREE SURFACE FOR TRACERS
C| LIU,V,WBOF     |<->| ON BOTTOM FOR U,V,W
C| LIU,V,WBOL     |<->| ON LATERAL BOUNDARIES FOR U,V,W
C| LIU,V,WBOS     |<->| AT FREE SURFACE FOR U,V,W
C| LT             |-->| CURRENT NUMBER OF TIME-STEP
C| NBOR           |-->| GLOBAL NUMBER OF 2D BOUNDARY POINTS
C| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
C| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
C| NELEM3         |-->| NUMBER OF 3D ELEMENTS
C| NETAGE         |-->| NUMBER OF LAYERS OF 3D ELEMENTS
C| NONHYD         |-->| NON-HYDROSTATIC FLAG
C| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF POINTS IN 3D
C| NPRIV          |-->| NUMBER OF ARRAYS IN BLOCK PRIVE
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
C| NPTFR3         |-->| NUMBER OF BOUNDARY POINTS IN 3D
C| NTRAC          |-->| NUMBER OF TRACERS
C| NVIT           |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
C| PBOR(F,L,S)    |<--| DIRICHLET BC VALUES FOR DYNAMIC PRESSURE
C| PRIVE          |-->| BLOCK OF ARRAYS FOR THE USER
C| SEDI           |-->| IF YES, THERE IS SEDIMENT
C| TA             |-->| CONCENTRATION OF TRACERS
C| TABORF         |<--| PRESCRIBED TRACERS ON THE BOTTOM
C| TABORL         |<--| PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
C| TABORS         |<--| PRESCRIBED TRACERS AT FREE SURFACE
C|                |   | LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
C|                |   | FOR VELOCITIES
C|                |   | ATABO,L,S AND BTABO,L,S
C|                |   | FOR TRACERS
C| U,V,W          |-->| COMPONENTS OF VELOCITY
C| UBORF          |<--| PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
C| UBORL          |<--| PRESCRIBED VELOCITY ALONG X ON THE LATERAL
C|                |   | BOUNDARY
C| UBORS          |<--| PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
C| UMOY,VMOY      |-->| DEPTH AVERAGED VELOCITY
C| VBORF          |<--| PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
C| VBORL          |<--| PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
C|                |   | BOUNDARY
C| VBORS          |<--| PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
C| VENT           |-->| WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
C| VENTX          |-->| WIND VELOCITY ALONG X
C| VENTY          |-->| WIND VELOCITY ALONG Y
C| VITIMP         |-->| ARRAY OF PRESCRIBED VELOCITIES
C| WBORF          |<--| PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
C| WBORL          |<--| PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
C|                |   | BOUNDARY
C| WBORS          |<--| PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
C| X,Y,Z          |-->| MESH COORDINATES
C| XSGBOR,YSGBOR  |-->| 2D NORMAL VECTORS TO THE SEGMENTS
C| ZF             |-->| BOTTOM ELEVATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN2, IPLAN, IPTFR, IPTFR3, ITRAC
!
!***********************************************************************
!
!     BOUNDARY CONDITIONS ON VELOCITIES
!     *********************************
!
!     BOTTOM
!     ======
!
!     DEFAULT: IMPERMEABILITY AND LOG LAW (SEE ALSO BORD3D)
!
      IF(BC_BOTTOM.EQ.1) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KLOG
          LIVBOF%I(IPOIN2) = KLOG
          LIWBOF%I(IPOIN2) = KLOG
!         USEFUL ? SHOULD NOT BE USED ANYWAY
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSEIF(BC_BOTTOM.EQ.2) THEN
!
        DO IPOIN2 = 1,NPOIN2
          LIUBOF%I(IPOIN2) = KADH
          LIVBOF%I(IPOIN2) = KADH
          LIWBOF%I(IPOIN2) = KADH
!         USEFUL ? KADH SAYS IT IS 0.D0
          UBORF%R(IPOIN2)  = 0.D0
          VBORF%R(IPOIN2)  = 0.D0
          WBORF%R(IPOIN2)  = 0.D0
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LIMI3D : FAUSSE CONDITION A LA LIMTE AU FOND'
          WRITE(LU,*) '         VALEUR ',BC_BOTTOM,' INCONNUE'
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LIMI3D : BAD BOUNDARY CONDITION ON THE BOTTOM'
          WRITE(LU,*) '         VALUE ',BC_BOTTOM,' UNKNOWN'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     COEFFICIENTS SET TO 0 BY MEANS OF THEIR COMPONENT TYPR
!     NO DIFFUSION FLUX THROUGH BOTTOM
!
      AUBORF%TYPR='0'
      AVBORF%TYPR='0'
      BUBORF%TYPR='0'
      BVBORF%TYPR='0'
      IF(NONHYD) THEN
        AWBORF%TYPR='0'
        BWBORF%TYPR='0'
      ENDIF
!
C     LATERAL BOUNDARIES
!     ==================
!
C     DEFAULT: 2D CONDITIONS DUPLICATED ON THE VERTICAL
C              FREE FOR W
C              NO FRICTION
!
      DO IPLAN = 2,NPLAN
         DO IPTFR = 1,NPTFR2
            IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
            LIUBOL%I(IPTFR3) = LIUBOL%I(IPTFR)
            LIVBOL%I(IPTFR3) = LIVBOL%I(IPTFR)
            UBORL%R(IPTFR3)  = UBORL%R(IPTFR)
            VBORL%R(IPTFR3)  = VBORL%R(IPTFR)
            AUBORL%R(IPTFR3) = AUBORL%R(IPTFR)
         ENDDO
      ENDDO
!
!     IDEA OF OPTIMISATION (BEWARE PARALLELISM)
!
!     IF(DOTS(AUBORL,AUBORL).GT.1.D-8) THEN
!       AUBORL%TYPR='Q'
!     ELSE
!       AUBORL%TYPR='0'
!     ENDIF
!
      DO IPTFR3 = 1,NPTFR3
C                           KSORT: W FREE ON LATERAL BOUNDARIES
         LIWBOL%I(IPTFR3)        = KSORT
!        VALUES SAVED IN SECOND DIMENSION BECAUSE ADVECTION
!        SCHEMES MAY CHANGE THE VALUES 
         LIUBOL%I(IPTFR3+NPTFR3) = LIUBOL%I(IPTFR3)
         LIVBOL%I(IPTFR3+NPTFR3) = LIVBOL%I(IPTFR3)
         LIWBOL%I(IPTFR3+NPTFR3) = LIWBOL%I(IPTFR3)
         WBORL%R(IPTFR3)  = 0.D0
C        BUBORL%R(IPTFR3) = 0.D0
C        BVBORL%R(IPTFR3) = 0.D0
      ENDDO
      BUBORL%TYPR='0'
      BVBORL%TYPR='0'
!
      IF(NONHYD) THEN
C       DO IPTFR3 = 1,NPTFR3
C         AWBORL%R(IPTFR3) = 0.D0
C         BWBORL%R(IPTFR3) = 0.D0
C       ENDDO
        AWBORL%TYPR='0'
        BWBORL%TYPR='0'
      ENDIF
!
C     FREE SURFACE
!     ============
!
C     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
         LIUBOS%I(IPOIN2) = KLOG
         LIVBOS%I(IPOIN2) = KLOG
         LIWBOS%I(IPOIN2) = KLOG
         UBORS%R(IPOIN2)  = 0.D0
         VBORS%R(IPOIN2)  = 0.D0
         WBORS%R(IPOIN2)  = 0.D0
C        AUBORS%R(IPOIN2) = 0.D0
C        BUBORS%R(IPOIN2) = 0.D0
C        BVBORS%R(IPOIN2) = 0.D0
      ENDDO
      AUBORS%TYPR='0'
      BUBORS%TYPR='0'
      AVBORS%TYPR='0'
      BVBORS%TYPR='0'
!
      IF(NONHYD) THEN
C       DO IPOIN2 = 1,NPOIN2
C         AWBORS%R(IPOIN2) = 0.D0
C         BWBORS%R(IPOIN2) = 0.D0
C       ENDDO
        AWBORS%TYPR='0'
        BWBORS%TYPR='0'
      ENDIF
!
!     **************
C     TRACERS BC'S
!     **************
!
      IF (NTRAC.NE.0) THEN
         DO ITRAC = 1,NTRAC
!
C     BOTTOM
!     ======
!
C     DEFAULT: NEUMANN BC'S
!
            DO IPOIN2 = 1,NPOIN2
               LITABF%ADR(ITRAC)%P%I(IPOIN2) = KLOG
               TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              ATABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              BTABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
            ENDDO
            ATABOF%ADR(ITRAC)%P%TYPR='0'
            BTABOF%ADR(ITRAC)%P%TYPR='0'
!
C     SIDES
!     =====
!
C     DEFAULT: NEUMANN BC'S
!
C           WHAT HAS BEEN READ IN THE BOUNDARY CONDITIONS FILE
C           FOR 1 TRACER IS DUPLICATED ON THE VERTICAL AND FOR
C           ALL TRACERS
!
            DO IPLAN = 1,NPLAN
              DO IPTFR = 1,NPTFR2
                IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
                LITABL%ADR(ITRAC)%P%I(IPTFR3) = LITABL%ADR(1)%P%I(IPTFR)
!               SAVING ON SECOND DIMENSION BECAUSE ADVECTION SCHEMES
!               MAY CHANGE THIS VALUE
                LITABL%ADR(ITRAC)%P%I(IPTFR3+NPTFR3) = 
     &                                          LITABL%ADR(1)%P%I(IPTFR)
                TABORL%ADR(ITRAC)%P%R(IPTFR3) = TABORL%ADR(1)%P%R(IPTFR)
C               ATABOL%ADR(ITRAC)%P%R(IPTFR3) = ATABOL%ADR(1)%P%R(IPTFR)
C               BTABOL%ADR(ITRAC)%P%R(IPTFR3) = BTABOL%ADR(1)%P%R(IPTFR)
              ENDDO
            ENDDO
            ATABOL%ADR(ITRAC)%P%TYPR='0'
            BTABOL%ADR(ITRAC)%P%TYPR='0'
!
C     FREE SURFACE
!     =============
!
C     DEFAULT: NEUMANN BC'S
!
            DO IPOIN2 = 1,NPOIN2
               LITABS%ADR(ITRAC)%P%I(IPOIN2) = KLOG
               TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              ATABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              BTABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
            ENDDO
            ATABOS%ADR(ITRAC)%P%TYPR='0'
            BTABOS%ADR(ITRAC)%P%TYPR='0'
!
         ENDDO
      ENDIF
!
C     SOLID BOUNDARIES FOR K AND EPSILON
!     **********************************
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
C     BOTTOM
!     ======
!
C     DEFAULT : NO GRADIENT
!
         DO IPOIN2 = 1,NPOIN2
           AKBORF%R(IPOIN2) = 0.D0
           BKBORF%R(IPOIN2) = 0.D0
           AEBORF%R(IPOIN2) = 0.D0
           BEBORF%R(IPOIN2) = 0.D0
         ENDDO
         AKBORF%TYPR = '0'
         BKBORF%TYPR = '0'
         AEBORF%TYPR = '0'
         BEBORF%TYPR = '0'
!
C     SIDES
!     =====
!
C     DEFAULT : NO GRADIENT
!
         DO IPTFR3 = 1,NPTFR3
           AKBORL%R(IPTFR3) = 0.D0
           BKBORL%R(IPTFR3) = 0.D0
           AEBORL%R(IPTFR3) = 0.D0
           BEBORL%R(IPTFR3) = 0.D0
         ENDDO
!
C     FREE SURFACE
!     ============
!
C     DEFAULT : NO GRADIENT
!
         DO IPOIN2 = 1,NPOIN2
            AKBORS%R(IPOIN2) = 0.D0
            BKBORS%R(IPOIN2) = 0.D0
            AEBORS%R(IPOIN2) = 0.D0
            BEBORS%R(IPOIN2) = 0.D0
         ENDDO
!
      ENDIF
!
!
C     FRICTION COEFFICIENTS
!     *********************
!
C     DEFAULT: VALUE GIVEN IN STEERING FILE
!
      CALL OV('X=C     ',RUGOL%R,RUGOL%R,RUGOL%R,RUGOL0,NPTFR2*NPLAN)
!
!======================================================================
C DEFAULT BOUNDARY CONDITION TYPES AND VALUES FOR THE
C PRESSURE POISSON EQUATION
!======================================================================
!
      IF(NONHYD) THEN
!
!-----------------------------------------------------------------------
!
C DEFAULT TYPES AND VALUES FOR THE PRESSURE BOUNDARY CONDITIONS
C BOTTOM AND FREE SURFACE
!
C       AT ALL LATERAL BOUNDARIES AND BOTTOM DP/DN = 0;
C       DIRICHLET = 0 AT THE SURFACE; DIRICHLET CONDITIONS SET TO 0 ALL OVER
C       (CORRECT AT THE SURFACE ONLY)
!
C       BOTTOM AND SURFACE
C       CHECK KLOG BOTTOM
!
        DO IPOIN2=1,NPOIN2
          LIPBOF%I(IPOIN2) = KLOG
          LIPBOS%I(IPOIN2) = KENT
          PBORF%R(IPOIN2)  = 0.D0
          PBORS%R(IPOIN2)  = 0.D0
        ENDDO
!
C       LATERAL SURFACES: ALL TREATED AS NEUMANN
!
        DO IPTFR3=1,NPTFR3
          LIPBOL%I(IPTFR3) = KLOG
          PBORL%R(IPTFR3)  = 0.D0
        ENDDO
!
C       LATERAL SURFACES: DIRICHLET ON ENTRANCES, NEUMANN ELSEWHERE
!
C       DO IPTFR3=1,NPTFR3
C         PBORL%R(IPTFR3)  = 0.D0
C         IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
C    *       LIUBOL%I(IPTFR3).EQ.KENTU) THEN
C           LIPBOL%I(IPTFR3) = KENT
C         ELSE
C           LIPBOL%I(IPTFR3) = KLOG
C         ENDIF
C       ENDDO
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
C
C#######################################################################
C
