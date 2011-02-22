C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODELS EROSION
!>               (WITHIN MULTI-LAYER CONSOLIDATION MODEL).
!><br>            THE USER PROVIDES THE LAW DEFINING THE CRITICAL
!>                EROSION VELOCITY AS A FUNCTION OF THE CONCENTRATION.
!><br>            THE EROSION LAW CAN BE CHANGED BY THE USER
!>               (PARTHENIADES FORMULATION BY DEFAULT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CONC, DENSI, DT, EPAI, FLUER, MPART, NCOUCH, NPOIN2, TOB
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IC, IPOIN, QERODE, QS, TEMPS, TOCE, VITCE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CLSEDI()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 04/05/93
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CONC
!></td><td>--></td><td>CONCENTRATION  OF BED LAYER
!>    </td></tr>
!>          <tr><td>DENSI
!></td><td>--></td><td>FLUID DENSITY
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME STEP
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><--</td><td>THICKNESS OF SOLID BED LAYER
!>                  (EPAI=DZ/(1+IVIDE), DZ total bed thickness)
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td><--</td><td>EROSION  FLUX
!>    </td></tr>
!>          <tr><td>MPART
!></td><td>--></td><td>EMPIRICAL COEFFICIENT (PARTHENIADES)
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NUMBER OF LAYERS WITHIN THE BED
!>                  (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>BOTTOM FRICTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE ERODC
     &( CONC  , EPAI   , FLUER  , TOB    , DENSI  ,
     &  MPART , DT     , NPOIN2 , NCOUCH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CONC           |-->| CONCENTRATION  OF BED LAYER
C| DENSI          |-->| FLUID DENSITY
C| DT             |-->| TIME STEP
C| EPAI           |<--| THICKNESS OF SOLID BED LAYER
C|                |   | (EPAI=DZ/(1+IVIDE), DZ total bed thickness)
C| FLUER          |<--| EROSION  FLUX
C| MPART          |-->| EMPIRICAL COEFFICIENT (PARTHENIADES)
C| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
C|                |   | (GIBSON MODEL)
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| TOB            |-->| BOTTOM FRICTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN2, NCOUCH
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2),DENSI(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: MPART, DT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IC, IPOIN
      DOUBLE PRECISION VITCE, TOCE , QS, TEMPS , QERODE
      INTRINSIC MIN , MAX
!
!-----------------------------------------------------------------------
!
      DO IPOIN=1,NPOIN2
!
C     ---- TEMPS:TIME COUNTER FOR EROSION ----
!
           TEMPS=DT
!
C     ---- QERODE: ERODED QUANTITY ----
!
           QERODE=0.D0
!
          DO IC=NCOUCH,1,-1
!
            IF (TEMPS.LE.1.D-6) GO TO 20
!
C     ---- EROSION OF TOP LAYER IF TOB > CRITICAL SHEAR STRESS ----
!
C   *******    COMPUTES CRITICAL FRICTION VELOCITY
C              AS A FUNCTION OF CONCENTRATION
C             (EMPIRICAL RELATION : TO BE MODIFIED BY USER)
!
            IF (CONC(IC).LT.240.D0) THEN
              VITCE=3.2D-5*(CONC(IC)**1.175D0)
            ELSE
              VITCE=5.06D-8*(CONC(IC)**2.35D0)
            ENDIF
!
C THE 4 LINES ARE ERROR MESSAGES TO BE SUPPRESSED
!
         WRITE(LU,*)
         IF (LNG.EQ.1) WRITE(LU,11)
         IF (LNG.EQ.2) WRITE(LU,12)
         CALL PLANTE(1)
         STOP
!
11    FORMAT('SOUS PROGRAMME ERODC : DONNER LA VITESSE CRITIQUE',/,
     &       'D''EROSION FONCTION DE LA CONCENTRATION')
12    FORMAT('SUBROUTINE ERODC : EXPRESS THE CRITICAL SHEAR STRESS',/,
     &       'FUNCTION OF THE CONCENTRATION')
!
C    ---- CRITICAL BED SHEAR STRENGTH -----
!
            TOCE=DENSI(IPOIN)*VITCE**2
            IF (TOB(IPOIN).LE.TOCE) GOTO 20
!
C    ---- EROSION FLUX COMPUTATION ------
!
            FLUER(IPOIN)=MPART*((TOB(IPOIN)/TOCE)-1.D0)
!
C    ---- SOLID QUANTITY WITHIN THE LAYER BEFORE EROSION ----
!
            QS=CONC(IC)*EPAI(IC,IPOIN)
!
C    ---- LAYER THICKNESS AFTER EROSION ----
!
            EPAI(IC,IPOIN)=MAX(0.D0,EPAI(IC,IPOIN)-
     &                             (FLUER(IPOIN)*TEMPS/CONC(IC)))
!
C    ---- ERODED QUANTITY ----
!
            QERODE=QERODE+MIN(FLUER(IPOIN)*TEMPS,QS)
!
C    ---- TIME LEFT AFTER EROSION OF LAYER ----
!
            TEMPS=DMAX1(0.D0,TEMPS-(QS/FLUER(IPOIN)))
!
         ENDDO
!
20     CONTINUE
!
C     -----END OF THE EROSION STEP-----
!
       FLUER(IPOIN)=QERODE/DT
!
      END DO
!
      RETURN
      END SUBROUTINE ERODC

C
C#######################################################################
C