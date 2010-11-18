C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AVA0, AVAIL, CONC_VASE, ELAY, ES, MS_SABLE, MS_VASE, NCOUCH_TASS, NPOIN, NSICLA, XMVS, ZF, ZR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIFF, EPAI_SABLE, EPAI_VASE, I, J
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIT_SEDIMENT()

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
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AVA0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CONC_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MS_SABLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MS_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCOUCH_TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INIT_MIXTE
     &(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY,NCOUCH_TASS,CONC_VASE,
     & MS_SABLE,MS_VASE,ZF,ZR,AVA0)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AVA0           |---| 
C| AVAIL          |---| 
C| CONC_VASE      |---| 
C| ELAY           |---| 
C| ES             |---| 
C| MS_SABLE       |---| 
C| MS_VASE        |---| 
C| NCOUCH_TASS    |---| 
C| NPOIN          |---| 
C| NSICLA         |---| 
C| XMVS           |---| 
C| ZF             |---| 
C| ZR             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NSICLA,NCOUCH_TASS
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10),ELAY(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(10)
      DOUBLE PRECISION, INTENT(IN)   :: AVA0(NSICLA)
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER I,J
C
      DOUBLE PRECISION EPAI_VASE(10),EPAI_SABLE(10)
      DOUBLE PRECISION DIFF
C
C ------------------------------------------------------------------------
C
C*******INITIAL SEDIMENT COMPOSITION IS IDENTICAL AT EACH NODE
C DEFAULT INITIALISATION: ALL LAYERS ARE EMPTY EXCEPT BED LAYER
C OTHERWISE SET THICKNESS OF THE MUD LAYERS IN EPAI_VASE(I= 1, NCOUCH_TASS-1)

      IF(NCOUCH_TASS.GT.1) THEN

        DO J= 1,NCOUCH_TASS-1
          EPAI_VASE(J) = 0.D0
        ENDDO
C
C       EXAMPLE FOR NCOUCH_TASS= 10
C
C       EPAI_VASE(1)=0.0525D0
C       EPAI_VASE(2)=0.0385D0
C       EPAI_VASE(3)=0.03995D0
C       EPAI_VASE(4)=0.0437D0
C       EPAI_VASE(5)=0.0517D0
C       EPAI_VASE(6)=0.1259D0
C       EPAI_VASE(7)=0.4889D0
C       EPAI_VASE(8)=1.5071D0
C       EPAI_VASE(9)=0.86410D0
C
        DO J=1,NCOUCH_TASS-1
          IF(NSICLA.GT.1) THEN
            EPAI_SABLE(J) = AVA0(1)/AVA0(2)*EPAI_VASE(J)
          ENDIF
        ENDDO
C
      ENDIF
C
C     COMPLETING THE LAST LAYER SO THAT SUM OF LAYERS = ZF-ZR
C
      DO I=1,NPOIN
        ELAY(I)=0.D0
        IF(NCOUCH_TASS.GT.1) THEN
          DO J=1,NCOUCH_TASS-1
            ES(I,J)= EPAI_VASE(J)
            IF(NSICLA.GT.1) THEN
              ES(I,J)= ES(I,J)  + EPAI_SABLE(J)
            ENDIF
            ELAY(I)=ELAY(I)+ES(I,J)
          ENDDO
        ENDIF
        DIFF= (ZF(I)-ZR(I)) - ELAY(I)
        IF(DIFF.GE.0.D0) THEN
          ES(I,NCOUCH_TASS) = DIFF
          ELAY(I) = ZF(I)-ZR(I)
        ELSE
          ES(I,NCOUCH_TASS) = 0.D0
          WRITE(LU,*) 'ERROR IN INIT-MIXTE: THE SUM OF THICKNESS'
          WRITE(LU,*) 'OF BED LAYERS > ERODIBLE BED MATERIAL'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
C
C     COMPUTING THE INITIAL MASSES OF MUD AND SAND
C
      DO I=1,NPOIN
C
        DO J=1,NCOUCH_TASS
C
C         FILLING VOIDS BETWEEN SAND GRAINS
C
          IF(NSICLA.EQ.1) THEN
C
C           PURE MUD
            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)
            AVAIL(I,J,1) = 1.D0
C
          ELSE
C
C           IF MIXED
            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)*AVA0(2)
            MS_SABLE(I,J)= ES(I,J)*XMVS*AVA0(1)
            IF(ES(I,J).GE.1.D-6) THEN
              AVAIL(I,J,1)= AVA0(1)
              AVAIL(I,J,2)= AVA0(2)
            ELSE
              AVAIL(I,J,1)= 0.D0
              AVAIL(I,J,2)= 0.D0
            ENDIF
C
          ENDIF
        ENDDO
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C