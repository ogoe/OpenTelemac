C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SUPPRESSES NEGATIVE DEPTHS BY A LIMITATION OF FLUXES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COMPUTE_FLODEL, DT, FLBOR, FLODEL, FLOPOINT, FLULIM, GLOSEG1, GLOSEG2, H, HBOR, HN, INFO, KDIR, LIMPRO, MESH, NBOR, NPOIN, NPTFR, OPTSOU, SMH, T1, T2, UNSV2D, YAFLODEL, YASMH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NBMAXNSHARE NBMAXNSHARE@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::NAMECODE NAMECODE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, CINIT, CPREV, DEJA, HFL1, HFL2, I, I1, I2, INDIC, IOPT1, IPTFR, IR, NEWREMAIN, NITER, NITMAX, REMAIN_SEG, TESTING, TET
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_POSITIVE_DEPTHS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), FLUX_EF_VF(), MULT_INTERFACE_SEG(), OS(), OSDB(), PARCOM(), PARCOM2_SEG(), P_DMIN(), P_DSUM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_SOLVS_FE(), CORRECTION_DEPTH_2D(), CORRECTION_DEPTH_3D()

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
!> </td><td> 12/03/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> ADDED COMPUTE_FLODEL
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COMPUTE_FLODEL
!></td><td>--></td><td>IF YES, COMPUTE FLODEL WITH FLOPOINT
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME STEP
!>    </td></tr>
!>          <tr><td>FLBOR
!></td><td><-></td><td>BOUNDARY FLUXES
!>    </td></tr>
!>          <tr><td>FLODEL
!></td><td><-></td><td>FLUXES GIVEN BY SEGMENT
!>    </td></tr>
!>          <tr><td>FLOPOINT
!></td><td>--></td><td>FLUXES GIVEN BY POINTS
!>    </td></tr>
!>          <tr><td>FLULIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG1,2
!></td><td>--></td><td>FIRST (SECOND) POINT OF SEGMENTS
!>    </td></tr>
!>          <tr><td>GLOSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td><-></td><td>NEW DEPTH
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>OLD DEPTH
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>--></td><td>PROPAGATION DEPTH
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>IF YES, PRINTING INFORMATION ON LISTING
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>GLOBAL NUMBERS OF BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>--></td><td>OPTION FOR SOURCES 1: NORMAL 2: DIRAC
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>SOURCE TERMS
!>    </td></tr>
!>          <tr><td>T1,T2
!></td><td>--></td><td>WORK ARRAYS
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>--></td><td>INVERSE OF INTEGRAL OF BASIS FUNCTIONS
!>    </td></tr>
!>          <tr><td>YAFLODEL
!></td><td>--></td><td>IF(YES) FLUXES IN FLODEL WILL NOT BE
!>                  INITIALISED BY TEL4DEL (THUS WE PUT HERE
!>                  THE DISCARDED FLUXES WITH A MINUS SIGN)
!>                  AND TEL4DEL WILL ADD THE COMPLETE FLUXES.
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE POSITIVE_DEPTHS
     &(T1,T2,H,HN,MESH,FLODEL,COMPUTE_FLODEL,FLBOR,DT,
     & UNSV2D,NPOIN,GLOSEG1,GLOSEG2,NBOR,NPTFR,YAFLODEL,
     & SMH,YASMH,OPTSOU,FLULIM,LIMPRO,HBOR,KDIR,INFO,FLOPOINT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COMPUTE_FLODEL |-->| IF YES, COMPUTE FLODEL WITH FLOPOINT
C| DT             |-->| TIME STEP
C| FLBOR          |<->| BOUNDARY FLUXES
C| FLODEL         |<->| FLUXES GIVEN BY SEGMENT
C| FLOPOINT       |-->| FLUXES GIVEN BY POINTS
C| FLULIM         |---| 
C| GLOSEG1,2      |-->| FIRST (SECOND) POINT OF SEGMENTS
C| GLOSEG2        |---| 
C| H             |<->| NEW DEPTH
C| HBOR           |---| 
C| HN             |-->| OLD DEPTH
C| HPROP          |-->| PROPAGATION DEPTH
C| INFO           |-->| IF YES, PRINTING INFORMATION ON LISTING
C| KDIR           |---| 
C| LIMPRO         |---| 
C| MESH           |---| 
C| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
C| OPTSOU         |-->| OPTION FOR SOURCES 1: NORMAL 2: DIRAC
C| SMH            |-->| SOURCE TERMS
C| T1,T2          |-->| WORK ARRAYS
C| UNSV2D         |-->| INVERSE OF INTEGRAL OF BASIS FUNCTIONS
C| YAFLODEL       |-->| IF(YES) FLUXES IN FLODEL WILL NOT BE
C|                |   | INITIALISED BY TEL4DEL (THUS WE PUT HERE
C|                |   | THE DISCARDED FLUXES WITH A MINUS SIGN)
C|                |   | AND TEL4DEL WILL ADD THE COMPLETE FLUXES.
C| YASMH          |-->| IF(YES) SMH MUST BE TAKEN INTO ACCOUNT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_POSITIVE_DEPTHS => POSITIVE_DEPTHS
      USE DECLARATIONS_TELEMAC, ONLY : NAMECODE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,OPTSOU,KDIR
      INTEGER, INTENT(IN)             :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,FLOPOINT(NPOIN),HBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(*)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2,FLODEL,H,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: UNSV2D,HN,SMH
      LOGICAL, INTENT(IN)             :: YAFLODEL,YASMH,INFO
      LOGICAL, INTENT(IN)             :: COMPUTE_FLODEL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DSUM,P_DMIN
      EXTERNAL         P_DSUM,P_DMIN
C
      INTEGER I,I1,I2,IPTFR,IOPT1,REMAIN_SEG,NEWREMAIN,IR,NITER
      DOUBLE PRECISION C,CPREV,CINIT,TET,HFL1,HFL2
C
      LOGICAL TESTING
      DATA TESTING/.FALSE./
      INTEGER NITMAX
      DATA NITMAX/10000/
C
C-----------------------------------------------------------------------
C
C     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER, ALLOCATABLE :: INDIC(:)
      SAVE
      IF(.NOT.DEJA) THEN
        ALLOCATE(INDIC(MESH%NSEG))
        DEJA=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(TESTING) THEN
        C=1.D99
        CINIT=1.D99
        DO I=1,NPOIN
          C    =MIN(C    ,H%R(I))
          CINIT=MIN(CINIT,HN%R(I))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          C=P_DMIN(C)
          CINIT=P_DMIN(CINIT)
        ENDIF
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, H MIN=',C
        WRITE(LU,*) 'AVANT TRAITEMENT HAUTEURS NEGATIVES, HN MIN=',CINIT
      ENDIF
C
C     COMPUTES THE FLUXES BY SEGMENT (T1 FOLLOWED BY FALSE NOT USED)
C     FLODEL IS NOT ASSEMBLED IN PARALLEL MODE
C
      IF(COMPUTE_FLODEL) THEN
C       HARD-CODED OPTION SO FAR
        IOPT1=2
        CALL FLUX_EF_VF(FLODEL%R,FLOPOINT,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,IOPT1)
      ENDIF
C
C     AVERAGES FLUXES ON INTERFACE SEGMENTS BY ASSEMBLING AND
C     DIVIDING BY 2. THIS IS NOT MANDATORY HERE BUT WILL GIVE
C     THE UPWINDING INFORMATION FOR TRACERS, AND THEY MUST BE TREATED
C     IN THE SAME WAY.
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(FLODEL%R,FLODEL%R,FLODEL%R,
     &                   MESH%NSEG,1,2,1,MESH,1)
        CALL MULT_INTERFACE_SEG(FLODEL%R,MESH%NH_COM_SEG%I,
     &                          MESH%NH_COM_SEG%DIM1,
     &                          MESH%NB_NEIGHB_SEG,
     &                          MESH%NB_NEIGHB_PT_SEG%I,
     &                          0.5D0,MESH%NSEG)
      ENDIF
C
      CALL CPSTVC(H,T2)
C
C     CHECKS THE CONTINUITY EQUATION BY NODES
C
      IF(TESTING) THEN
C     T1 RESULTS FROM CALL TO VECTOR
      DO I=1,NPOIN
        T2%R(I)=DT*T1%R(I)*UNSV2D%R(I)
      ENDDO
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        T2%R(I)=T2%R(I)+DT*FLBOR%R(IPTFR)*UNSV2D%R(I)
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
C     NOTE: SMH IS ALREADY ASSEMBLED IN PARALLEL MODE
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            T2%R(I)=T2%R(I)-DT*SMH%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            T2%R(I)=T2%R(I)-DT*SMH%R(I)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ENDIF
      C=0.D0
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPOIN
          C=C+MESH%FAC%R(I)*(T2%R(I)+H%R(I)-HN%R(I))**2
        ENDDO
        C=P_DSUM(C)
      ELSE
        DO I=1,NPOIN
          C=C+(T2%R(I)+H%R(I)-HN%R(I))**2
        ENDDO
      ENDIF
      WRITE(LU,*) 'ERREUR SUR LA CONTINUITE PAR NOEUDS=',C
C
C     CHECKS THE CONTINUITY EQUATION BY SEGMENTS
C
      CALL CPSTVC(H,T1)
      CALL OS('X=0     ',X=T1)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        T1%R(I)=T1%R(I)-DT*FLBOR%R(IPTFR)*UNSV2D%R(I)
      ENDDO
      DO I=1,MESH%NSEG
        I1=GLOSEG1(I)
        I2=GLOSEG2(I)
        T1%R(I1)=T1%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
        T1%R(I2)=T1%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
C     NOTE: SMH IS ALREADY ASSEMBLED IN PARALLEL MODE
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            T1%R(I)=T1%R(I)+DT*SMH%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            T1%R(I)=T1%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ENDIF
      C=0.D0
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPOIN
          C=C+MESH%FAC%R(I)*(T1%R(I)+HN%R(I)-H%R(I))**2
        ENDDO
        C=P_DSUM(C)
      ELSE
        DO I=1,NPOIN
          C=C+(T1%R(I)+HN%R(I)-H%R(I))**2
        ENDDO
      ENDIF
      WRITE(LU,*) 'ERREUR SUR LA CONTINUITE PAR SEGMENTS=',C
      ENDIF
C
C     END OF CHECKS
C
      CPREV=0.D0
      DO I=1,MESH%NSEG
        CPREV=CPREV+ABS(FLODEL%R(I))
      ENDDO
      IF(NCSIZE.GT.1) CPREV=P_DSUM(CPREV)
      CINIT=CPREV
      IF(TESTING) WRITE(LU,*) 'SOMME INITIALE DES FLUX=',CPREV
C
C     LOOP ON THE SEGMENTS, TO TAKE INTO ACCOUNT ELIGIBLE FLUXES
C
C     ADDS THE SOURCES (SMH IS NATURALLY ASSEMBLED IN PARALLEL MODE)
C
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*SMH%R(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
            H%R(I)=HN%R(I)+DT*SMH%R(I)*UNSV2D%R(I)
          ENDDO
        ENDIF
      ELSE
        DO I=1,NPOIN
          H%R(I)=HN%R(I)
        ENDDO
      ENDIF
C
C     BOUNDARY FLUXES : SUMS THE ENTERING (NEGATIVE) FLUXES
C     FIRST PUTS FLBOR (BOUNDARY) IN T2 (DOMAIN)
      CALL OSDB( 'X=Y     ' ,T2,FLBOR,FLBOR,0.D0,MESH)
C     ASSEMBLES T2 (FLBOR IS NOT ASSEMBLED)
      IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
        H%R(I)=H%R(I)-DT*UNSV2D%R(I)*MIN(T2%R(I),0.D0)
      ENDDO
C
C     TO OPTIMISE THE LOOP ON SEGMENTS, ONLY SEGMENTS WITH
C     NON ZERO FLUXES WILL BE CONSIDERED, THIS LIST WILL BE
C     UPDATED. TO START WITH, ALL FLODEL ASSUMED NON ZERO.
C
      REMAIN_SEG=MESH%NSEG
      DO I=1,REMAIN_SEG
        INDIC(I)=I
C       INITIAL PERCENTAGE OF FLODEL THAT IS NOT TRANSMITTED
        FLULIM(I)=1.D0
      ENDDO
C
      NITER = 0
777   CONTINUE
      NITER = NITER + 1
C
C     AT THIS LEVEL H THE SAME AT INTERFACE POINTS
C
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
C         AVAILABLE DEPTH IS SHARED BETWEEN PROCESSORS
C         NACHB(1,IPTFR) WITH DIMENSION NACHB(NBMAXNSHARE,NPTIR)
          I=MESH%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          H%R(I)=H%R(I)*MESH%FAC%R(I)
        ENDDO
      ENDIF
C
      C=0.D0
C     DO I=1,MESH%NSEG
      NEWREMAIN=0
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        IF(FLODEL%R(I).GT.1.D-15) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL1= DT*UNSV2D%R(I1)*FLODEL%R(I)
          IF(HFL1.GT.H%R(I1)) THEN
            TET=H%R(I1)/HFL1
            H%R(I1)=0.D0
            H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)*TET
            FLULIM(I)=FLULIM(I)*(1.D0-TET)
            FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
            C=C+ABS(FLODEL%R(I))
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            H%R(I1)=H%R(I1)-HFL1
            H%R(I2)=H%R(I2)+DT*UNSV2D%R(I2)*FLODEL%R(I)
            FLULIM(I)  =0.D0
            FLODEL%R(I)=0.D0
          ENDIF
        ELSEIF(FLODEL%R(I).LT.-1.D-15) THEN
          I1=GLOSEG1(I)
          I2=GLOSEG2(I)
          HFL2=-DT*UNSV2D%R(I2)*FLODEL%R(I)
          IF(HFL2.GT.H%R(I2)) THEN
            TET=H%R(I2)/HFL2
            H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)*TET
            H%R(I2)=0.D0
            FLULIM(I)=FLULIM(I)*(1.D0-TET)
            FLODEL%R(I)=FLODEL%R(I)*(1.D0-TET)
            C=C+ABS(FLODEL%R(I))
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ELSE
            H%R(I1)=H%R(I1)-DT*UNSV2D%R(I1)*FLODEL%R(I)
            H%R(I2)=H%R(I2)-HFL2
            FLULIM(I)=0.D0
            FLODEL%R(I)=0.D0
          ENDIF
        ELSE
          FLULIM(I)=0.D0
        ENDIF
      ENDDO
C
      REMAIN_SEG=NEWREMAIN
C
C     SUMS THE NEW POSITIVE PARTIAL DEPTHS OF INTERFACE POINTS
      IF(NCSIZE.GT.1) CALL PARCOM(H,2,MESH)
C
      IF(NCSIZE.GT.1) C=P_DSUM(C)
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
      IF(C.NE.CPREV.AND.ABS(C-CPREV).GT.CINIT*1.D-9
     &             .AND.C.NE.0.D0) THEN
        CPREV=C
        IF(NITER.LT.NITMAX) GO TO 777
      ENDIF
C
      DO I=1,MESH%NSEG
        FLULIM(I)=1.D0-FLULIM(I)
      ENDDO
C
C     BOUNDARY FLUXES : ADDS THE EXITING (POSITIVE) FLUXES
C                       WITH A POSSIBLE LIMITATION
C
      DO IPTFR=1,NPTFR
        I=NBOR(IPTFR)
C                               T2 = // ASSEMBLED FLBOR
        HFL1=DT*UNSV2D%R(I)*MAX(T2%R(I),0.D0)
        IF(HFL1.GT.H%R(I)) THEN
C         FLBOR ACTUALLY TAKEN INTO ACCOUNT
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)*H%R(I)/HFL1
          H%R(I)=0.D0
        ELSE
          H%R(I)=H%R(I)-HFL1
        ENDIF
        IF(LIMPRO(IPTFR).EQ.KDIR) THEN
          FLBOR%R(IPTFR)=FLBOR%R(IPTFR)
     &                  +(H%R(I)-HBOR(IPTFR))/(DT*UNSV2D%R(I))
          H%R(I)= HBOR(IPTFR)
        ENDIF
      ENDDO
C
      IF(TESTING) THEN
        C=1.D99
        DO I=1,NPOIN
          C=MIN(C,H%R(I))
        ENDDO
        IF(NCSIZE.GT.1) C=P_DMIN(C)
        WRITE(LU,*) 'APRES TRAITEMENT HAUTEURS NEGATIVES, HMIN=',C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     FLUXES CORRESPONDING TO LIMITATION ARE KEPT IN FLODEL
C     THEY WILL BE THE INITIAL VALUE IN TEL4DEL
C
      IF(YAFLODEL) THEN
        DO I=1,MESH%NSEG
          FLODEL%R(I)=-FLODEL%R(I)
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(INFO) THEN
        IF(NAMECODE(1:7).EQ.'SISYPHE') THEN
          IF(LNG.EQ.1) WRITE(LU,101) NITER
          IF(LNG.EQ.2) WRITE(LU,102) NITER
        ELSE
          IF(LNG.EQ.1) WRITE(LU,201) NITER
          IF(LNG.EQ.2) WRITE(LU,202) NITER
        ENDIF
      ENDIF
!
101   FORMAT(' EQUATION DE CHARRIAGE RESOLUE EN ',1I5,' ITERATIONS')
102   FORMAT(' BEDLOAD EQUATION SOLVED IN ',1I5,' ITERATIONS')
201   FORMAT(' HAUTEURS POSITIVES OBTENUES EN ',1I5,' ITERATIONS')
202   FORMAT(' POSITIVE DEPTHS OBTAINED IN ',1I5,' ITERATIONS')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C