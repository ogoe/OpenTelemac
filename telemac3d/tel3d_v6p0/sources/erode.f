C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODELS EROSION.
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
!>    </th><td> CFDEP, DENSI, DT, EPAI, FLUER, GIBSON, HDEP, IVIDE, MPART, NPF, NPFMAX, NPOIN2, RHOS, TOB, TOCE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ECOUCH, IPF, IPOIN, NCOUCH, NERODE, QERODE, QS, TEMPS, TOCEC, TS, VITCE
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
!>      <td><center> 5.7                                       </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 13/05/92
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION OF DEPOSIT
!>    </td></tr>
!>          <tr><td>DENSI
!></td><td>--></td><td>FLUID DENSITY
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME STEP
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><-></td><td>THICKNESS OF SOLID BED LAYER
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td><--</td><td>EROSION FLUX
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGICAL FOR SETTLING MODEL
!>                  (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><-></td><td>THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td>--></td><td>VOID RATIO
!>    </td></tr>
!>          <tr><td>MPART
!></td><td>--></td><td>EMPIRICAL COEFFICIENT PARTHENIADES
!>    </td></tr>
!>          <tr><td>NPF
!></td><td><-></td><td>NUMBER OF POINTS WITHIN THE BED
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
!>                  (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>DENSITY OF  SEDIMENT
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>BOTTOM FRICTION
!>    </td></tr>
!>          <tr><td>TOCE
!></td><td>--></td><td>CRITICAL  EROSION SHEAR STRENGTH (FRESH DEPOSIT)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE ERODE
     &( IVIDE  , EPAI   ,
     &  HDEP   , FLUER  , TOB   , DENSI,
     &  NPOIN2 , NPFMAX , NPF   , MPART  ,
     &  TOCE   , CFDEP  , RHOS  , DT   , GIBSON)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFDEP          |-->| CONCENTRATION OF DEPOSIT
C| DENSI          |-->| FLUID DENSITY
C| DT             |-->| TIME STEP
C| EPAI           |<->| THICKNESS OF SOLID BED LAYER
C| FLUER          |<--| EROSION FLUX
C| GIBSON         |-->| LOGICAL FOR SETTLING MODEL
C|                |   | (GIBSON MODEL)
C| HDEP           |<->| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
C| IVIDE          |-->| VOID RATIO
C| MPART          |-->| EMPIRICAL COEFFICIENT PARTHENIADES
C| NPF            |<->| NUMBER OF POINTS WITHIN THE BED
C| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
C|                |   | (GIBSON MODEL)
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| RHOS           |-->| DENSITY OF  SEDIMENT
C| TOB            |-->| BOTTOM FRICTION
C| TOCE           |-->| CRITICAL  EROSION SHEAR STRENGTH (FRESH DEPOSIT)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NPOIN2, NPFMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX, NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1, NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2), FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TOB(NPOIN2),  DENSI(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: DT, RHOS, CFDEP
      DOUBLE PRECISION, INTENT(IN) :: MPART,TOCE
!
      INTEGER, INTENT(INOUT) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: GIBSON
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN, IPF, NERODE, NCOUCH
      DOUBLE PRECISION VITCE , TOCEC , QERODE , ECOUCH , TS , QS, TEMPS
      INTRINSIC MIN , MAX
!
!-----------------------------------------------------------------------
C     ---- START OF LOOP ON 2D MESH POINTS ----
!
C JAJ CLUMSY GOTO'S FROM A LOOP AND AN IF-STRUCTURE...
!
      DO IPOIN = 1, NPOIN2
!
C      ---- TEMPS: TIME COUNTER FOR EROSION ----
!
           TEMPS = DT
!
C      ---- QERODE : ERODED QUANTITY OF SEDIMENT ----
!
           QERODE = 0.D0
!
C      ---- NERODE : NUMBER OF ERODED LAYERS ----
!
          NERODE = 0
!
C      ---- EROSION OF FRESH DEPOSIT ----
!
        IF (HDEP(IPOIN).GE.1.D-10) THEN
!
C         TESTS IF TOB > CRITICAL EROSION FRICTION OF SURFACE LAYER
!
          IF ((TOB(IPOIN)-TOCE).LE.1.D-8) GOTO 20
!
          FLUER(IPOIN)=MPART*((TOB(IPOIN)/TOCE)-1.D0)
          QERODE=MIN(FLUER(IPOIN)*TEMPS,HDEP(IPOIN)*CFDEP)
!
C      ---- REACTUALISES DEPOSIT THICKNESS AFTER EROSION ----
!
          HDEP(IPOIN)=
     &     MAX(HDEP(IPOIN)-(FLUER(IPOIN)*TEMPS/CFDEP),0.D0)
!
C      ---- TIME LEFT TO ERODE UNDERLAYERS ----
!
          TEMPS=TEMPS-QERODE/FLUER(IPOIN)
!
        ENDIF
!
C      ---- EROSION OF MUD BED ----
!
         IF (GIBSON) THEN
          DO IPF=2,NPF(IPOIN)
!
C      ---- NCOUCH: NUMBER OF SUPERFICIAL LAYER ----
!
C         BUG CORRECTED 29/06/2006 AFTER WARNING BY CHC-NRC (THANKS)
C         NCOUCH=-IPF+NPF(NPOIN2)+1
          NCOUCH=-IPF+NPF(IPOIN)+1
!
            IF (TEMPS.LE.1.D-8) GOTO 20
!
C      ---- CONCENTRATION OF SUPERFICIAL LAYER ----
!
            ECOUCH=(IVIDE(NCOUCH,IPOIN)+IVIDE(NCOUCH+1,IPOIN))/2.D0
            TS=RHOS/(1.D0+ECOUCH)
!
C      ---- CRITICAL FRICTION VELOCITY AS A FUNCTION OF CONCENTRATION ----
!
C   (EMPIRICAL RELATION, LOIRE ESTUARY, FRITSCH ET AL. 1989 )
            IF(TS.LT.240.D0) THEN
              VITCE=3.2D-5*(TS**1.175D0)
            ELSE
              VITCE=5.06D-8*(TS**2.35D0)
            ENDIF
!
C SUPPRESS 4 FOLLOWING LINES TO ACTIVATE THE SUBROUTINE
!
         WRITE(LU,*)
         IF (LNG.EQ.1) WRITE(LU,11)
         IF (LNG.EQ.2) WRITE(LU,12)
         CALL PLANTE(1)
         STOP
!
11    FORMAT('SOUS-PROGRAMME ERODE : DONNER LA VITESSE CRITIQUE',/,
     &       'D''EROSION DU LIT CONSOLIDE FONCTION DE LA CONCENTRATION')
12    FORMAT('SUBROUTINE ERODE : EXPRESS THE CRITICAL SHEAR STRESS FOR',
     & /, 'EROSION (CONSOLIDATED BED) FUNCTION OF THE CONCENTRATION')
!
!-----------------------------------------------------------------------
!
C        ---- COMPUTES THE CRITICAL BED SHEAR STRENGTH FOR EROSION
C                          FOR CONSOLIDATED BED , TOCEC
            TOCEC=DENSI(IPOIN)*VITCE**2
!
C        ---- EROSION FLUX
!
            FLUER(IPOIN)=MPART*MAX((TOB(IPOIN)/TOCEC)-1.D0,0.D0)
!
C        ---- MAXIMAL AMOUNT OF SEDIMENT AVAILABLE FOR EROSION
!
            QS=RHOS*EPAI(NCOUCH,IPOIN)
!
            IF ((FLUER(IPOIN)*TEMPS).GE.QS) THEN
!
C        ---- EROSION OF THE WHOLE BED LAYER
!
             EPAI(NCOUCH,IPOIN)=0.D0
!
C        ---- ERODED QUANTITY
!
             QERODE=QERODE+QS
!
C        ---- TIME LEFT AFTER EROSION OF LAYER
!
             TEMPS=TEMPS-(QS/FLUER(IPOIN))
!
C        ---- NUMBER OF ERODED LAYERS
!
             NERODE=NERODE+1
!
            ELSE
!
C        ---- PARTIAL EROSION OF BED LAYER
!
             EPAI(NCOUCH,IPOIN)=EPAI(NCOUCH,IPOIN)-
     &                                        (FLUER(IPOIN)*TEMPS/RHOS)
!
C        ---- ERODED QUANTITY
!
             QERODE=QERODE+(FLUER(IPOIN)*TEMPS)
!
             GOTO 20
!
            ENDIF
           ENDDO
         ENDIF
!
20     CONTINUE
C GOTO TARGET
!
C     ----- END OF EROSION STEP -----
!
        FLUER(IPOIN)=QERODE/DT
        NPF(IPOIN)=NPF(IPOIN)-NERODE
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE ERODE

C
C#######################################################################
C