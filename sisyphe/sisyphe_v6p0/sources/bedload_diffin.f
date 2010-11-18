C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE BOUNDARY CONDITIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CLT, KDDL, KDIR, KENT, KINC, KLOG, KNEU, KP1BOR, KSORT, LIMTRA, LITBOR, MASKEL, MASKTR, MSK, NBOR, NELBOR, NPTFR, U, V, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, DDL, DIR, K, K1, K2, NEU, OND, USCALN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_DIFFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_MAIN()

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
!> </td><td> 17/08/2004
!> </td><td> FRANCOIS MENARD (PLACEMENT @ LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CLT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKTR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_DIFFIN !
     &  (U, V, NBOR, XNEBOR, YNEBOR, KP1BOR, MASKEL, NELBOR, NPTFR,
     &   KENT, KSORT, KLOG, KINC, KDIR, KDDL, KNEU, MSK, CLT, LITBOR,
     &   MASKTR, LIMTRA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CLT            |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| KENT           |---| 
C| KINC           |---| 
C| KLOG           |---| 
C| KNEU           |---| 
C| KP1BOR         |---| 
C| KSORT          |---| 
C| LIMTRA         |---| 
C| LITBOR         |---| 
C| MASKEL         |---| 
C| MASKTR         |---| 
C| MSK            |---| 
C| NBOR           |---| 
C| NELBOR         |---| 
C| NPTFR          |---| 
C| U             |---| 
C| V             |---| 
C| XNEBOR         |---| 
C| YNEBOR         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_BEDLOAD_DIFFIN => BEDLOAD_DIFFIN
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ), INTENT(IN)    :: U, V, NBOR, XNEBOR, YNEBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: KP1BOR, MASKEL, NELBOR
      INTEGER,        INTENT(IN)    :: NPTFR, KENT, KSORT, KLOG
      INTEGER,        INTENT(IN)    :: KINC, KDIR, KDDL, KNEU
      LOGICAL,        INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CLT
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: LITBOR, MASKTR, LIMTRA


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: K, K1, K2
      DOUBLE PRECISION   :: USCALN,C
      INTEGER, PARAMETER :: DIR = 1
      INTEGER, PARAMETER :: DDL = 2
      INTEGER, PARAMETER :: NEU = 3
      INTEGER, PARAMETER :: OND = 4


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ****************************************************** !
      ! I - TYPES OF BOUNDARY CONDITIONS FOR THE TRACER        ! (_IMP_)
      !     MAY BE MODIFIED DEPENDING ON THE SIGN OF U.N       ! (_IMP_)
      !     FOR THE LIQUID BOUNDARIES (N : OUTGOING NORMAL)    ! (_IMP_)
      ! ****************************************************** !
      DO K = 1, NPTFR

         CLT%I(K) = LITBOR%I(K)

         ! I.1 - LIQUID BOUNDARIES (_IMP_)
         ! --------------------------------------
         IF (CLT%I(K) == KENT) THEN
            USCALN = U%R(NBOR%I(K))*XNEBOR%R(K)
     &             + V%R(NBOR%I(K))*YNEBOR%R(K)

            ! OUTGOING VELOCITY, FREE TRACER
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF (USCALN >= 0.D0) CLT%I(K) = KSORT

         ELSEIF(CLT%I(K) == KSORT) THEN

            USCALN = U%R(NBOR%I(K))*XNEBOR%R(K)
     &             + V%R(NBOR%I(K))*YNEBOR%R(K)

            ! ENTERING VELOCITY, FREE TRACER
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            IF (USCALN <= 0.D0) CLT%I(K) = KENT
         ENDIF

      ENDDO


      ! **************************************************************** !
      ! II - MASKTR ARRAY DEFINED AS A FUNCTION OF CLT                   ! (_IMP_)
      !      EQUALS 1 FOR A SEGMENT OF NEUMANN TYPE, AND 0 OTHERWISE     ! (_IMP_)
      !      A SEGMENT IS OF NEUMANN TYPE IF THE USER SPECIFIES AT LEAST ! (_IMP_)
      !      ONE OF ITS NODES AS NEUMANN.                                ! (_IMP_)
      ! **************************************************************** !
      CALL OS('X=0     ', X=MASKTR)

      DO K1 = 1 , NPTFR

         K2 = KP1BOR%I(K1)

         ! II.1 - NEUMANN TYPE SEGMENTS
         ! -------------------------------
         IF (CLT%I(K1).EQ.KLOG.OR.CLT%I(K2).EQ.KLOG) THEN
            MASKTR%ADR(NEU)%P%R(K1) = 1.D0

         ! II.2 - OUTGOING TYPE SEGMENTS (_IMP_)
         ! ------------------------------
         ELSEIF ((CLT%I(K1) == KENT) .AND. (CLT%I(K2) == KSORT)) THEN
            MASKTR%ADR(DDL)%P%R(K1) = 1.D0

         ELSEIF ((CLT%I(K1) == KSORT) .OR. (CLT%I(K2) == KSORT)) THEN
            MASKTR%ADR(DDL)%P%R(K1) = 1.D0

         ! II.3 - OUTGOING TYPE SEGMENTS (_IMP_)
         ! ------------------------------
         ELSEIF ((CLT%I(K1) == KSORT) .AND. (CLT%I(K2) == KENT)) THEN
            MASKTR%ADR(DDL)%P%R(K1) = 1.D0
         ELSEIF ((CLT%I(K1) == KENT) .OR. (CLT%I(K2) == KENT)) THEN
            MASKTR%ADR(DIR)%P%R(K1) = 1.D0
         ELSEIF ((CLT%I(K1) == KINC) .OR. (CLT%I(K2) == KINC)) THEN
            MASKTR%ADR(OND)%P%R(K1)=1.D0
         ELSE
            IF (LNG == 1) WRITE(LU,101)
            IF (LNG == 2) WRITE(LU,102)
            CALL PLANTE(1)
         ENDIF
      ENDDO


      ! *********************** !
      ! III - POTENTIAL MASKING !
      ! *********************** !
      IF(MSK) THEN
        DO K1 = 1 , NPTFR
          C=MASKEL%R(NELBOR%I(K1))
          MASKTR%ADR(DIR)%P%R(K1) = MASKTR%ADR(DIR)%P%R(K1)*C
          MASKTR%ADR(DDL)%P%R(K1) = MASKTR%ADR(DDL)%P%R(K1)*C
          MASKTR%ADR(NEU)%P%R(K1) = MASKTR%ADR(NEU)%P%R(K1)*C
          MASKTR%ADR(OND)%P%R(K1) = MASKTR%ADR(OND)%P%R(K1)*C
        ENDDO
      ENDIF


      ! ************************************************************** !
      ! IV - FROM PHYSICAL CONDITION TO TECHNICAL CONDITIONS           !
      ! ************************************************************** !
      DO K = 1, NPTFR

         ! IV.1 - 'INCOMING' BOUNDARY : IMPOSED TRACER (_IMP_)
         ! -----------------------------------------
         IF(CLT%I(K).EQ.KENT) THEN
            LIMTRA%I(K) = KDIR

         ELSEIF(CLT%I(K).EQ.KSORT) THEN
            LIMTRA%I(K) = KDDL

         ! IV.2 - SOLID BOUNDARY : NEUMANN CONDITIONS (_IMP_)
         ! ------------------------------------
         ELSEIF(CLT%I(K).EQ.KLOG ) THEN
            LIMTRA%I(K) = KNEU

         ! IV.3 - ERROR: UNKNOWN LITBOR VALUE (_IMP_)
         ! ----------------------------------------
         ELSE
            IF (LNG == 1) WRITE(LU,11) K, LITBOR%I(K)
            IF (LNG == 2) WRITE(LU,12) K, LITBOR%I(K)
            CALL PLANTE(1)
            STOP
         ENDIF

      ENDDO

      !----------------------------------------------------------------!
101   FORMAT(' DIFFIN_SISYPHE : CAS NON PREVU')
11    FORMAT(' DIFFIN_SISYPHE : POINT ',1I8,' LITBOR= ',1I8,' ?')
      !----------------------------------------------------------------!
102   FORMAT(' DIFFIN_SISYPHE: UNEXPECTED CASE')
12    FORMAT(' DIFFIN_SISYPHE : POINT ',1I8,' LITBOR= ',1I8,' ?')
      !----------------------------------------------------------------!


!======================================================================!
!======================================================================!

      RETURN
      END
C
C#######################################################################
C