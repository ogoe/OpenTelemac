C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AVAIL, CONC_VASE, DTS, DZF_TASS, ELAY, ES, LT, MS_SABLE, MS_VASE, NCOUCH_TASS, NPOIN, NSICLA, T2, TRANS_MASS, XKV, XMVS, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CONC_SABLE, EPAI_SABLE, EPAI_VASE, I, J, TAUX, TRANSFERT_MASSE_SABLE, TRANSFERT_MASSE_VASE
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
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AVAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CONC_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DZF_TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
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
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRANS_MASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE TASSEMENT
     &(ZF,NPOIN,DTS,ELAY,DZF_TASS,T2,LT,AVAIL,NSICLA,ES,XMVS,
     & XKV,TRANS_MASS,CONC_VASE,NCOUCH_TASS,MS_SABLE,MS_VASE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AVAIL          |---| 
C| CONC_VASE      |---| 
C| DTS            |---| 
C| DZF_TASS       |---| 
C| ELAY           |---| 
C| ES             |---| 
C| LT             |---| 
C| MS_SABLE       |---| 
C| MS_VASE        |---| 
C| NCOUCH_TASS    |---| 
C| NPOIN          |---| 
C| NSICLA         |---| 
C| T2             |---| 
C| TRANS_MASS     |---| 
C| XKV            |---| 
C| XMVS           |---| 
C| ZF             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN)    :: NPOIN,NSICLA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DZF_TASS,ZF,ELAY,T2
      DOUBLE PRECISION, INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      INTEGER, INTENT(IN)             :: LT,NCOUCH_TASS
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)    :: TRANS_MASS(10),CONC_VASE(10)
      DOUBLE PRECISION, INTENT(IN)    :: XMVS,XKV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J
      DOUBLE PRECISION CONC_SABLE
C
      DOUBLE PRECISION TAUX(10),TRANSFERT_MASSE_VASE(10)
      DOUBLE PRECISION TRANSFERT_MASSE_SABLE(10)
      DOUBLE PRECISION EPAI_SABLE(10),EPAI_VASE(10)
C
C COMPUTES THE TOTAL SEDIMENT THICKNESS (SAND + MUD) BEFORE CONSOLIDATION
C
      CONC_SABLE=XMVS/XKV
C
C T2: MUD THICKNESS BEFORE CONSOLIDATION
C
      DO I=1,NPOIN
C
        T2%R(I)=0.D0
        DO J=1,NCOUCH_TASS
          EPAI_VASE(J)=MS_VASE(I,J)/CONC_VASE(J)
          ES(I,J)=EPAI_VASE(J)
          IF(NSICLA.GT.1) THEN
            EPAI_SABLE(J)=MS_SABLE(I,J)/XMVS
            ES(I,J)=EPAI_VASE(J)+EPAI_SABLE(J)
          ENDIF
          T2%R(I)=T2%R(I)+ES(I,J)
        ENDDO
C
        DO J=1,NCOUCH_TASS
          IF(MS_VASE(I,J).GE.1.D-6) THEN
            TRANSFERT_MASSE_VASE(J)=MIN(MS_VASE(I,J),
     &              MS_VASE(I,J)*DTS*TRANS_MASS(J))
            IF(NSICLA.GT.1) THEN
              TAUX(J)=TRANSFERT_MASSE_VASE(J)/MS_VASE(I,J)
              TRANSFERT_MASSE_SABLE(J)=TAUX(J)*MS_SABLE(I,J)
            ENDIF
          ELSE
            TRANSFERT_MASSE_VASE(J)=0.D0
            IF(NSICLA.GT.1) TRANSFERT_MASSE_SABLE(J)=0.D0
          ENDIF
C**************ARRET DE TASSEMENT SI LA VASE A REMPLI LES INTERSTICES
C**************   ENTRE LES GRAINS DE SABLE
          IF(NSICLA.GT.1.AND.EPAI_SABLE(J).GE.ES(I,J)) THEN
            TRANSFERT_MASSE_VASE(J) =0.D0
            TRANSFERT_MASSE_SABLE(J)=0.D0
          ENDIF
        ENDDO
C
        DO J=1,NCOUCH_TASS
          IF(J.EQ.NCOUCH_TASS) THEN
             MS_VASE(I,J)=MAX(0.D0,MS_VASE(I,J)
     &            +TRANSFERT_MASSE_VASE(J-1))
             IF(NSICLA.GT.1) THEN
                MS_SABLE(I,J)=MAX(0.D0,MS_SABLE(I,J)
     &                                  +TRANSFERT_MASSE_SABLE(J-1))
             ENDIF
          ELSEIF(J.EQ.1) THEN
             MS_VASE(I,J)=MAX(0.D0,MS_VASE(I,J)
     &            -TRANSFERT_MASSE_VASE(J))
            IF(NSICLA.GT.1) THEN
              MS_SABLE(I,J)=MAX(0.D0,MS_SABLE(I,J)
     &            -TRANSFERT_MASSE_SABLE(J))
            ENDIF
          ELSE
             MS_VASE(I,J)=MAX(0.D0,MS_VASE(I,J)
     &            +TRANSFERT_MASSE_VASE(J-1)-TRANSFERT_MASSE_VASE(J))
             IF(NSICLA.GT.1) THEN
               MS_SABLE(I,J)=MAX(0.D0,MS_SABLE(I,J)
     &         +TRANSFERT_MASSE_SABLE(J-1)-TRANSFERT_MASSE_SABLE(J))
             ENDIF
          ENDIF
        ENDDO
C
        ELAY%R(I)=0.D0
C
        DO J=1,NCOUCH_TASS
          EPAI_VASE(J)=MS_VASE(I,J)/CONC_VASE(J)
          ES(I,J) = EPAI_VASE (J)
          IF(NSICLA.GT.1) THEN
            EPAI_SABLE(J)=MS_SABLE(I,J)/XMVS
            ES(I,J)=EPAI_VASE(J)+EPAI_SABLE(J)
          ENDIF
          ELAY%R(I)=ELAY%R(I) + ES(I,J)
        ENDDO
C
C       BED EVOLUTION DUE TO CONSOLIDATION
C
        DZF_TASS%R(I)=ELAY%R(I)-T2%R(I)
C
C NOTE JMH : I HAS UNDERSTOOD THAT CLASS 1 = MUD
C            AND FROM 2 ON: SAND; WHAT ARE WE DOING HERE ??
C
        IF(NSICLA.GT.1) THEN
          DO J=1,NCOUCH_TASS
           IF(ES(I,J).GE.1.D-6) THEN
             AVAIL(I,J,1)=MS_SABLE(I,J)/XMVS/ES(I,J)
             AVAIL(I,J,2)=MS_VASE(I,J)/CONC_VASE(J)/ES(I,J)
           ELSE
             AVAIL(I,J,1)=0.D0
             AVAIL(I,J,2)=0.D0
           ENDIF
          ENDDO
        ENDIF
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