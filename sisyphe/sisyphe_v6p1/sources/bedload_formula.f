C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE BED-LOAD TRANSPORT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AVA, BIJK, CF, COEFPN, D90, DM, FW, GRAV, HIDFAC, HIDING, HMIN, HN, HOULE, ICF, IELMT, KARMAN, KSP, KSR, MU, NPOIN, PI, QSC, QSS, SECCURRENT, SLOPEFF, SUSP, T1, T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, TETAP, THETAW, TOB, TOBW, TW, U2D, UCMOY, UNLADM, UW, V2D, VCE, XKV, XMVE, XMVS, XWC, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALPHA, C1, DENS, DSTAR, I, ZERO_LOCAL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_FORMULA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BEDLOAD_BAILARD(), BEDLOAD_BIJKER(), BEDLOAD_DIBWAT(), BEDLOAD_EINST(), BEDLOAD_ENGEL(), BEDLOAD_ENGEL_OLD(), BEDLOAD_HUNZ_MEYER(), BEDLOAD_MEYER(), BEDLOAD_SECCURRENT(), BEDLOAD_SOULSBY(), BEDLOAD_VANRIJN(), OS(), PLANTE(), QSFORM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_SOLIDISCHARGE(), INIT_TRANSPORT()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 12/01/2005
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> **/10/2003
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> **/01/2002
!> </td><td> BUI MINH DUC
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BIJK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFPN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D90
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDFAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDING
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HOULE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SECCURRENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLOPEFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SUSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T10
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T11
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T9
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETAP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>THETAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_FORMULA
     &(U2D,V2D,UCMOY,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,
     & ACLADM, UNLADM,KSP,KSR,AVA,NPOIN,ICF,HIDFAC,XMVS,XMVE,
     & DM,GRAV,VCE,XKV,HMIN,XWC,D90,KARMAN,ZERO,
     & PI,SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10,
     & T11,TETAP, QSC, QSS,IELMT,SECCURRENT,SLOPEFF,
     & COEFPN,BIJK,HOULE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AVA            |---| 
C| BIJK           |---| 
C| CF             |---| 
C| COEFPN         |---| 
C| D90            |---| 
C| DM             |---| 
C| FW             |---| 
C| GRAV           |---| 
C| HIDFAC         |---| 
C| HIDING         |---| 
C| HMIN           |---| 
C| HN             |---| 
C| HOULE          |---| 
C| ICF            |---| 
C| IELMT          |---| 
C| KARMAN         |---| 
C| KSP            |---| 
C| KSR            |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| PI             |---| 
C| QSC            |---| 
C| QSS            |---| 
C| SECCURRENT     |---| 
C| SLOPEFF        |---| 
C| SUSP           |---| 
C| T1             |---| 
C| T10            |---| 
C| T11            |---| 
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| T7             |---| 
C| T8             |---| 
C| T9             |---| 
C| TETAP          |---| 
C| THETAW         |---| 
C| TOB            |---| 
C| TOBW           |---| 
C| TW             |---| 
C| U2D            |---| 
C| UCMOY          |---| 
C| UNLADM         |---| 
C| UW             |---| 
C| V2D            |---| 
C| VCE            |---| 
C| XKV            |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_FORMULA => BEDLOAD_FORMULA
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D, UCMOY,HN, CF, TOB
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MU,TOBW, UW, TW, THETAW, FW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM,UNLADM,KSR,KSP
      INTEGER,          INTENT(IN)    :: NPOIN, ICF, HIDFAC,IELMT
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, DM, GRAV, VCE
      DOUBLE PRECISION, INTENT(IN)    :: XKV, HMIN, XWC, D90
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      LOGICAL,          INTENT(IN)    :: SUSP,SECCURRENT,HOULE
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10,T11
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TETAP ! WORK ARRAY T12
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC, QSS
      TYPE(BIEF_OBJ),   INTENT(INOUT) ::  COEFPN
      INTEGER,          INTENT(IN)    :: SLOPEFF
C
      DOUBLE PRECISION, INTENT (IN) :: BIJK,AVA(NPOIN)
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: I
      DOUBLE PRECISION            :: DENS,DSTAR,ALPHA
      DOUBLE PRECISION, PARAMETER :: ZERO_LOCAL = 1.D-6
      DOUBLE PRECISION            :: C1


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!


      ! *************************** !
      ! I - ADIMENSIONAL PARAMETERS !
      ! *************************** !

      DENS  = (XMVS - XMVE )/ XMVE
      DSTAR = DM*(GRAV*DENS/VCE**2)**(1.D0/3.D0)

      ! ************************ !
      ! II -  SKIN FRICTION      !
      ! ************************ !
!
      C1 = 1.D0/(DENS*XMVE*GRAV*DM)
      CALL OS('X=CYZ   ', X=TETAP, Y=TOB,Z=MU,  C=C1)
      CALL OS('X=+(Y,C)', X= TETAP,Y=TETAP, C=ZERO_LOCAL)
!
      ! *********************************************** !
      ! III - CRITICAL SHIELDS NUMBER FOR ENTRAINMENT   !
      !       VAN RIJN FORMULATION                      !
      ! *********************************************** !
C MOVED TO INIT_SEDIMENT.F
!
C      IF(ICF.EQ.7.OR.ICF.EQ.6.OR.AC.LE.0.D0) THEN
C         IF (DSTAR
C            AC = 0.24*DSTAR**(-1.0D0)
C         ELSEIF (DSTAR
C            AC = 0.14D0*DSTAR**(-0.64D0)
C         ELSEIF (DSTAR
C            AC = 0.04D0*DSTAR**(-0.1D0)
C         ELSEIF (DSTAR
C            AC = 0.013D0*DSTAR**(0.29D0)
C         ELSE
C            AC = 0.055D0
C         ENDIF
C      ENDIF

      IF(SECCURRENT) CALL BEDLOAD_SECCURRENT(IELMT)

      ! ****************************************** !
      ! IV - COMPUTES 2 TRANSPORT TERMS            !
      !      QSS : SUSPENSION                      !
      !      QSC : BEDLOAD                         !
      ! ****************************************** !

      ! ===================================== !
      ! IV(1) - MEYER-PETER-MULLER FORMULATION!
      !         FOR BEDLOAD ONLY              !
      ! ===================================== !

      IF(ICF == 1) THEN

          CALL BEDLOAD_MEYER(TETAP,HIDING,HIDFAC,DENS,GRAV,DM,AC,
     &                       T1,QSC,SLOPEFF,COEFPN)
          DO I=1,NPOIN
            QSC%R(I)=XKV*QSC%R(I)*AVA(I)
          ENDDO
          ALPHA = -3.D0

      ! =========================== !
      ! IV(2) - EINSTEIN FORMULATION!
      !         FOR BEDLOAD ONLY    !
      ! =========================== !

      ELSEIF(ICF == 2) THEN

         CALL BEDLOAD_EINST(TETAP,NPOIN,DENS,GRAV,DM,DSTAR,QSC)
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -6.D0

      ! =================================== !
      ! IV(30) - ENGELUND-HANSEN FORMULATION!
      !          FOR TOTAL TRANSPORT        !
      ! =================================== !

      ELSEIF(ICF == 30) THEN
C V6P0 MU IS USED INSTEAD OF CF
C BEWARE: DIFFERENCES
C         CALL BEDLOAD_ENGEL(TETAP,DENS,GRAV,DM,QSC)
C BACK TO EARLIER VERSION OF BEDLOAD_ENGEL
         CALL BEDLOAD_ENGEL(TOB,CF,DENS,GRAV,DM,XMVE,T1,QSC)
C        ARBITRARY DISTRIBUTION
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -5.D0

      ! ======================================== !
      ! IV(3) - ENGELUND-HANSEN FORMULATION      !
      !         MODIFIED: CHOLLET ET CUNGE       !
      !         FOR TOTAL TRANSPORT              !
      ! ======================================== !

      ELSEIF(ICF == 3) THEN
C        KSP IS USED INSTEAD OF CFP
         CALL BEDLOAD_ENGEL_OLD
     &        (TETAP,CF,NPOIN,GRAV,DM,DENS,T1,QSC)
C        ARBITRARY DISTRIBUTION
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -5.D0

      ! ============================== !
      ! IV(4) - BIJKER FORMULATION     !
      !         FOR BEDLOAD + SUSPENSION !
      ! ============================== !

      ELSEIF (ICF == 4) THEN

         CALL BEDLOAD_BIJKER
     &    (TOBW,TOB,MU,KSP,KSR,HN,NPOIN,DM,DENS,XMVE,GRAV,
     &     XWC,KARMAN,ZERO,T4,T7,T8,T9,QSC,QSS,BIJK,HOULE)
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
           QSS%R(I)=XKV*QSS%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -1.D0

      ! ============================== !
      ! IV(5) - SOULSBY FORMULATION    !
      !         FOR BEDLOAD + SUSPENSION !
      ! ============================== !

      ELSEIF (ICF == 5) THEN

         CALL BEDLOAD_SOULSBY
     &        (UCMOY,HN,UW,NPOIN,DENS,GRAV,DM,DSTAR,HMIN,
     &         D90,QSC,QSS)
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
           QSS%R(I)=XKV*QSS%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -4.6D0

      ! ================================================== !
      ! IV(6) - HUNZIKER / MEYER-PETER & MULLER FORMULATION!
      !         FOR BEDLOAD ONLY                           !
      ! ================================================== !

      ELSEIF (ICF == 6) THEN

         CALL BEDLOAD_HUNZ_MEYER
     &        (TOB, MU, ACLADM, UNLADM, NPOIN, DENS, XMVE, GRAV,
     &         DM, AC, T1, T2, T3, HIDING, QSC)
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)
         ENDDO
         ALPHA = -3.D0

      ! =========================== !
      ! IV(7) - VAN RIJN FORMULATION!
      !         FOR BEDLOAD ONLY    !
      ! =========================== !

      ELSEIF (ICF == 7) THEN
C
         CALL BEDLOAD_VANRIJN
C     &        (TOB,MU,NPOIN,DM,DENS,GRAV,DSTAR,AC,QSC)
     &        (TETAP,MU,NPOIN,DM,DENS,GRAV,DSTAR,AC,QSC)
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -4.2D0

      ! ============================== !
      ! IV(8) - BAILARD FORMULATION    !
      !         FOR BEDLOAD + SUSPENSION !
      ! ============================== !

      ELSEIF (ICF == 8) THEN
C
         CALL BEDLOAD_BAILARD
     &        (U2D,V2D,UCMOY,TOB,TOBW,THETAW,UW,FW,CF,NPOIN,
     &         PI,XMVE,GRAV,DENS,XWC,T1,T2,T3,T4,T5,T6,T7,
     &         T8,T9,T10,T11,QSC,QSS,HOULE)
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
           QSS%R(I)=XKV*QSS%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -3.D0

      ! ======================================= !
      ! IV(9) - DIBAJNIA AND WATANABE FORMULATION!
      !         FOR TOTAL TRANSPORT             !
      ! ======================================= !

      ELSEIF(ICF == 9) THEN
C
         CALL BEDLOAD_DIBWAT
     &        (U2D,V2D,UCMOY, CF, TOB, TOBW, UW, TW, FW, THETAW,
     &         NPOIN, XMVE, DENS, GRAV, DM, XWC, PI, T1, T2, T3, T4,
     &         T5, T6, T7, T8, T9, T10, T11, QSC,HOULE)
C        ARBITRARY DISTRIBUTION
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
         ENDDO
         ALPHA = -3.D0

      ! ============================================ !
      ! IV(0) - USER-DEFINED FORMULATION             !
      ! ============================================ !

      ELSEIF (ICF == 0) THEN

         ALPHA = -1.D0 ! INITIALISES ALPHA
         CALL QSFORM
         DO I=1,NPOIN
           QSC%R(I)=XKV*QSC%R(I)*AVA(I)*HIDING%R(I)
           QSS%R(I)=XKV*QSS%R(I)*AVA(I)*HIDING%R(I)
         ENDDO

      ! ================= !
      ! IV(ELSE) - ERROR  !
      ! ================= !

      ELSE
        IF(LNG == 1) WRITE(LU,200) ICF
        IF(LNG == 2) WRITE(LU,201) ICF
200     FORMAT(1X,'TRANSP : FORMULE DE TRANSPORT INCONNUE :',1I6)
201     FORMAT(1X,'TRANSP : TRANSPORT FORMULA UNKNOWN:',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
C     WHEN SUSPENSION IS NOT ASKED SPECIFICALLY, SOME BEDLOAD TRANSPORT
C     FORMULAS GIVE A VALUE
!
      IF(.NOT.SUSP) THEN
        IF(ICF.EQ.4.OR.ICF.EQ.5.OR.ICF.EQ.8.OR.ICF.EQ.0) THEN
          DO I = 1,NPOIN
            QSC%R(I) = QSC%R(I) + QSS%R(I)
          ENDDO
        ELSE
C         NOTE JMH: IS THIS REALLY USEFUL ???
          DO I = 1,NPOIN
            QSS%R(I) = 0.D0
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!=======================================================================
!
      RETURN
      END
C
C#######################################################################
C