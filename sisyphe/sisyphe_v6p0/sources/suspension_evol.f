C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE EVOLUTION FOR MUD ACCORDING TO FLUDP
!>                AND FLUER; AND UPDATES THE MASS OF THE LAYERS +
!>                EACH LAYER THICKNESS + TOTAL THICKNESS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  COMPUTE ES AGAIN AT THE END AND
!>         VERIFY THE CRITERION ELAY=ZF-ZR

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CONC_VASE, CSF, DT, FLUDP, FLUER, MS, NCOUCH_TASS, NPOIN, QFLUX, SEDCO, XMVS, ZFCL_S
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CONC, I, J, MER, ZERO
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION()

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
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CONC_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUDP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCOUCH_TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QFLUX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL_S
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_EVOL
     &  (ZFCL_S,FLUDP,FLUER,DT, NPOIN,CSF,XMVS, QFLUX,MS,
     &   SEDCO,CONC_VASE,NCOUCH_TASS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CONC_VASE      |---| 
C| CSF            |---| 
C| DT             |---| 
C| FLUDP          |---| 
C| FLUER          |---| 
C| MS             |---| 
C| NCOUCH_TASS    |---| 
C| NPOIN          |---| 
C| QFLUX          |---| 
C| SEDCO          |---| 
C| XMVS           |---| 
C| ZFCL_S         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,FLUDP,FLUER,QFLUX
      DOUBLE PRECISION, INTENT(IN)    :: DT, XMVS, CSF
      INTEGER, INTENT(IN) :: NPOIN,NCOUCH_TASS
      LOGICAL, INTENT(IN) :: SEDCO
      DOUBLE PRECISION, INTENT(IN) :: CONC_VASE(NCOUCH_TASS)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS(NPOIN,NCOUCH_TASS)

C
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I,J
C
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION CONC,MER
C
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
       ZERO = 1.D-08
C
C COMPUTES THE SEDIMENT FLUX AT EACH TIMESTEP
C
           CALL OS('X=Y-Z   ', X=QFLUX, Y=FLUDP, Z=FLUER)
           CALL OS('X=CX    ', X=QFLUX, C=DT)
           IF(NCOUCH_TASS.EQ.1)   CALL OS('X=CY    ',
     &         X=ZFCL_S, Y= QFLUX, C=1.D0/CSF)

           IF(NCOUCH_TASS.GT.1) THEN

             DO I = 1, NPOIN
C
C DEPOSITION IN THE FIRST LAYER
C
             IF (QFLUX%R(I).GT.ZERO) THEN
                ZFCL_S%R(I) = QFLUX%R(I) / CSF
                MS(I,1) = MS (I,1) +QFLUX%R(I)*XMVS
!
              ELSEIF(QFLUX%R(I).LT.ZERO) THEN
C
C EROSION OF SUCCESSIVE LAYERS
C
C
                ZFCL_S%R(I) = 0.D0
                MER = - QFLUX%R(I) *XMVS
C
                DO J = 1, NCOUCH_TASS
C
                 IF(.NOT.SEDCO) CONC= XMVS * CSF
                 IF(SEDCO) CONC=XMVS*CONC_VASE(J)
C
                 IF (MER.LE.MS(I,J)) THEN
                   MS(I,J)= MS(I,J) - MER
                   ZFCL_S%R(I)= ZFCL_S%R(I) - MER/CONC
                   GO TO 40
C
                ELSE
C
C EROSION OF THE WHOLE UNDER-LAYER
C
                   MER= MER - MS(I,J)
                   ZFCL_S%R(I)= ZFCL_S%R(I) -
     &                MS(I,J)/CONC
                   MS(I,J)=0.D0
C
               ENDIF
C END OF THE LOOP ON THE LAYERS
             ENDDO
C END EROSION
          ENDIF
C
  40      CONTINUE
C
C END OF THE LOOP ON THE NODES
C
        ENDDO
      ENDIF
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C