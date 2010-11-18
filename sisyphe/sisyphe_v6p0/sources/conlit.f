C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOWS TO IMPOSE TIME VARYING BOUNDARY CONDITIONS
!>               (CONSTANT VALUES CAN BE DIRECTLY IMPOSED IN CONDIM
!>                INPUT FILE).<br>
!><br>            ALLOWS TO IMPOSE A SAND TRANSPORT RATE AT SOME
!>                BOUNDARY NODES (QBOR AND LIQBOR). IT IS THEN NECESSARY
!>                TO ALSO IMPOSE LIEBOR = KSORT AT THESE NODES !

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NBOR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::CBOR CBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::CBOR_CLASSE CBOR_CLASSE@endlink, 
!> @link DECLARATIONS_SISYPHE::LICBOR LICBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::LIEBOR LIEBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::NUMLIQ NUMLIQ@endlink, 
!> @link DECLARATIONS_SISYPHE::SUSP SUSP@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KADH KADH@endlink, 
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IFRLIQ, IRANK, K
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 19/06/2008
!> </td><td> CV
!> </td><td> TAKES INTO ACCOUNT CBOR_VASE AND CBOR_SABLE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 07/06/2002
!> </td><td> C. MACHET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/09/1995
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CBOR
!></td><td><-></td><td>IMPOSED SUSPENDED SAND CONC AT THE BOUNDARY
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td><-></td><td>IMPOSED BED EVOLUTION AT THE BOUNDARY
!>    </td></tr>
!>          <tr><td>KADH,KLOG
!></td><td>--></td><td>BOUNDARY
!>    </td></tr>
!>          <tr><td>KENT,KSORT
!></td><td>--></td><td>TYPES OF
!>    </td></tr>
!>          <tr><td>KNEU,KDIR,KDDL
!></td><td>--></td><td>CONDITIONS
!>    </td></tr>
!>          <tr><td>LICBOR
!></td><td><-></td><td>TYPE OF BOUNDARY CONDITIONS ON SUSPENDED SAND CONC
!>    </td></tr>
!>          <tr><td>LIEBOR
!></td><td><-></td><td>TYPE OF BOUNDARY CONDITIONS ON BED EVOLUTION
!>    </td></tr>
!>          <tr><td>LIQBOR
!></td><td><-></td><td>TYPE OF BOUNDARY CONDITIONS ON SAND TRANSPORT RATE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>GLOBAL NUMBER OF BOUNDARY POINT
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF 2D POINTS
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>QBOR
!></td><td><-></td><td>IMPOSED SOLID TRANSPORT AT THE BOUNDARY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONLIT
     &(NBOR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CBOR           |<->| IMPOSED SUSPENDED SAND CONC AT THE BOUNDARY
C| EBOR           |<->| IMPOSED BED EVOLUTION AT THE BOUNDARY
C| KADH,KLOG      |-->| BOUNDARY
C| KENT,KSORT     |-->| TYPES OF
C| KNEU,KDIR,KDDL |-->| CONDITIONS
C| LICBOR         |<->| TYPE OF BOUNDARY CONDITIONS ON SUSPENDED SAND CONC
C| LIEBOR         |<->| TYPE OF BOUNDARY CONDITIONS ON BED EVOLUTION
C| LIQBOR         |<->| TYPE OF BOUNDARY CONDITIONS ON SAND TRANSPORT RATE
C| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
C| NPOIN          |-->| NUMBER OF 2D POINTS
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
C| QBOR           |<->| IMPOSED SOLID TRANSPORT AT THE BOUNDARY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN):: NBOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,K,IFRLIQ,IRANK
C
C-----------------------------------------------------------------------
C
      DO  K=1,NPTFR
C
        I = NBOR(K)
C
C
C  DIRICHLET CONDITIONS
C  +++++++++++++++++++++
C
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
C
C IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
C ++++++++++++++++++++++++++++++++++++++++++++
C QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
C                   NODE K , CLASS OF SEDIMENT J
C
C               LIEBOR%I(K)=KSORT
C               LIQBOR%I(K)=KENT
C
C               QBOR%ADR(1)%P%R(K)=1.D-4
C               QBOR%ADR(2)%P%R(K)=1.D-4 .....
C
C  IMPOSED BED EVOLUTON
C +++++++++++++++++++++
C          IF (LIEBOR%I(K).EQ.KENT) THEN
C               EBOR%ADR(1)%P%R(K)=1.D-4
C               EBOR%ADR(2)%P%R(K)=1.D-4.....
C         ENDIF
C
       ENDDO
C
C-----------------------------------------------------------------------
C     LICBOR : BOUNDARY CONDITION FOR SEDIMENT CONCENTRATION
C-----------------------------------------------------------------------

      IF(SUSP) THEN
C
        DO K=1,NPTFR
C
C         SO FAR LICBOR=LIEBOR (WITH KADH CHANGED INTO KLOG, SEE ABOVE,
C                               BUT CAN BE CHANGED)
C
          LICBOR%I(K) = LIEBOR%I(K)
C
C         ENTRANCE : IMPOSED CONCENTRATION
C         -------------------------------
C
C         NOTE JMH: KSORT MUST BE TREATED ALSO BECAUSE SUBROUTINE DIFFIN
C                   MAY CHANGE A KSORT INTO KENT, DEPENDING OF FLOW
C
          IFRLIQ=NUMLIQ%I(K)
          IF(LIEBOR%I(K).EQ.KENT.OR.LIEBOR%I(K).EQ.KSORT) THEN
            DO I=1,NSICLA
               IRANK=I+(IFRLIQ-1)*NSICLA
               CBOR%ADR(I)%P%R(K) = CBOR_CLASSE(IRANK)
            ENDDO
          ENDIF
C
        ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE CONLIT
C
C#######################################################################
C