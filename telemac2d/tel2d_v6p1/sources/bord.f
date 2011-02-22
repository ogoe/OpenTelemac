C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!>                WHEN THEY VARY IN TIME.
!>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THIS SUBROUTINE CAN BE COMPLETED BY THE USER DIRECTLY,
!>         OR THROUGH THE FUNCTIONS: Q , SL , TR , VIT

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
!> </td><td> 27/03/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C
C#######################################################################
C
                        SUBROUTINE BORD
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,
     & ZF,NBOR,TRA05,TRA06,LIHBOR,LIUBOR,LITBOR,
     & XNEBOR,YNEBOR,NPOIN,NPTFR,NPTFR2,TEMPS,NDEBIT,NCOTE,NVITES,
     & NTRAC,NTRACE,NFRLIQ,NUMLIQ,KENT,KENTU,PROVEL,MASK,MESH,EQUA,
     & NOMIMP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COTE           |<->| TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
C| COTINI         |-->| COTE INITIALE
C| DEBIT          |<->| TABLEAU DE DEBITS IMPOSES
C| EQUA           |---| 
C| H              |-->| HAUTEUR AU TEMPS N
C| HBOR           |<--| HAUTEUR IMPOSEE.
C| KENT,KENTU,    |-->| CONVENTION POUR LES TYPES DE CONDITIONS AUX
C|                |   | KENTU:U ET V IMPOSES
C| LIHBOR         |-->| CONDITIONS AUX LIMITES SUR H
C| LITBOR         |-->| CONDITIONS AUX LIMITES SUR LE TRACEUR
C| LIUBOR         |-->| CONDITIONS AUX LIMITES SUR U
C| MASK           |---| 
C| MESH           |---| 
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NCOTE          |-->| NOMBRE DE FRONTIERES A COTE IMPOSEE
C| NDEBIT         |-->| NOMBRE DE FRONTIERES A DEBIT IMPOSE
C| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
C| NOMIMP         |---| 
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
C| NPTFR2         |---| 
C| NTRAC          |---| 
C| NTRACE         |-->| NOMBRE DE FRONTIERES A TRACEUR IMPOSE
C| NUMLIQ         |---| 
C| NVITES         |-->| NOMBRE DE FRONTIERES A VITESSE IMPOSEE
C| PROVEL         |-->| OPTION POUR LES PROFILS DE VITESSE
C| TBOR           |<--| TRACEUR IMPOSE AU BORD
C| TEMPS          |-->| TEMPS
C| TRA05,TRA06    |-->| TABLEAUX DE TRAVAIL
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| TRACER         |<->| TABLEAU DE VALEURS DU TRACEUR IMPOSEES
C| U,V            |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
C| UBOR           |<--| VITESSE U IMPOSEE.
C| VBOR           |<--| VITESSE V IMPOSEE.
C| VITES          |<->| TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
C|                |   | IMPOSEES
C| XNEBOR         |---| 
C| YNEBOR         |---| 
C| ZF             |-->| FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_BORD => BORD
      USE DECLARATIONS_TELEMAC2D, ONLY : STA_DIS_CURVES,PTS_CURVES,QZ,
     &                                   FLUX_BOUNDARIES,MAXFRO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NPTFR,NDEBIT,NCOTE,NVITES,NTRACE
      INTEGER, INTENT(IN) :: KENT,KENTU,NFRLIQ,NTRAC,NPTFR2
      INTEGER, INTENT(IN) :: PROVEL(*)
      INTEGER, INTENT(IN) :: LIHBOR(NPTFR),LIUBOR(NPTFR2)
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR),NBOR(NPTFR2)
      DOUBLE PRECISION, INTENT(IN) :: TEMPS
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      CHARACTER(LEN=20), INTENT(IN)   :: EQUA
      CHARACTER(LEN=144), INTENT(IN)  :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASK,LITBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,MSK8,IFRLIQ,YADEB(MAXFRO),IERR,ITRAC,IFR
C
      DOUBLE PRECISION Z,ZMIN(MAXFRO)
C
      LOGICAL YAZMIN
C
      DOUBLE PRECISION P_DMIN
      INTEGER  P_IMAX
      EXTERNAL P_IMAX,P_DMIN
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
C     IF VELOCITY PROFILE OPTION 5: MINIMUM ELEVATION OF EVERY BOUNDARY
C
      YAZMIN=.FALSE.
      DO IFR=1,NFRLIQ
        ZMIN(IFR)=1.D99
        IF(PROVEL(IFR).EQ.5) YAZMIN=.TRUE.
      ENDDO
      IF(YAZMIN) THEN
        DO K=1,NPTFR
          IFR=NUMLIQ(K)
          ZMIN(IFR)=MIN(ZMIN(IFR),ZF(NBOR(K))+H%R(NBOR(K)))
        ENDDO
        IF(NCSIZE.GT.1) THEN
          DO IFR=1,NFRLIQ
            ZMIN(IFR)=P_DMIN(ZMIN(IFR))
          ENDDO
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      MSK8 = 8
C
C  INITIALISATION OF YADEB
C
      IF(NFRLIQ.GE.1) THEN
        DO K=1,NFRLIQ
          YADEB(K)=0
        ENDDO
      ENDIF
C
C  LOOP ON ALL BOUNDARY POINTS
C
      DO 5 K=1,NPTFR
C
C  LEVEL IMPOSED WITH VALUE GIVEN IN THE CAS FILE (NCOTE0)
C
      IF(LIHBOR(K).EQ.KENT) THEN
C
        IFRLIQ=NUMLIQ(K)
C
        IF(STA_DIS_CURVES(IFRLIQ).EQ.1) THEN
          Z = STA_DIS_CUR(IFRLIQ,FLUX_BOUNDARIES(IFRLIQ),
     &                    PTS_CURVES(IFRLIQ),QZ,NFRLIQ,
     &                    ZF(NBOR(K))+H%R(NBOR(K)))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
        ELSEIF(NCOTE.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
          Z = SL(IFRLIQ,NBOR(K))
          HBOR(K) = MAX( 0.D0 , Z-ZF(NBOR(K)) )
          H%R(NBOR(K))=HBOR(K)
C       ELSE HBOR TAKEN IN BOUNDARY CONDITIONS FILE
        ENDIF
C
      ENDIF
C
C  DISCHARGE IMPOSED: VARIOUS OPTIONS ACCORDING TO PROVEL
C                 ONE USES THE VALUES PROVIDED BY THE USER
C                 AS VELOCITY PROFILE.
C                 UBOR(K,2) AND VBOR(K,2) ARE THE VALUES OF
C                 THE CONLIM FILE, AND ARE CONSERVED.
C
      IF(LIUBOR(K).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        IFR=NUMLIQ(K)
        IF(PROVEL(IFR).EQ.1) THEN
C         CONSTANT NORMAL PROFILE
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(IFR).EQ.2) THEN
C         PROFILE PROVIDED BY THE USER
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.3) THEN
C         NORMAL VELOCITY PROVIDED IN UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(IFR).EQ.4) THEN
C         NORMAL PROFILE IN SQUARE ROOT OF H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
        ELSEIF(PROVEL(IFR).EQ.5) THEN
C         NORMAL PROFILE IN SQUARE ROOT OF H, BUT VIRTUAL H
C         DEDUCED FROM LOWEST FREE SURFACE OF THE BOUNDARY
          UBOR(K,1)=-XNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
          VBOR(K,1)=-YNEBOR(K)*SQRT(MAX(ZMIN(IFR)-ZF(NBOR(K)),0.D0))
        ENDIF
C       ONE DOES NOT SET VELOCITY IF THERE IS NO WATER.
        IF(H%R(NBOR(K)).LT.1.D-3) THEN
          UBOR(K,1) = 0.D0
          VBOR(K,1) = 0.D0
        ENDIF
C       U AND V INITIALISED WITH THE IMPOSED VALUES
        U%R(NBOR(K)) = UBOR(K,1)
        V%R(NBOR(K)) = VBOR(K,1)
        YADEB(NUMLIQ(K))=1
      ENDIF
C
C  VELOCITY IMPOSED: ONE USES THE OUTGOING DIRECTION
C                    PROVIDED BY THE USER.
C
      IF(LIUBOR(K).EQ.KENTU.AND.
     &  (NVITES.NE.0.OR.NOMIMP(1:1).NE.' ')) THEN
C       POINTS ON WEIRS HAVE NUMLIQ(K)=0
        IF(NUMLIQ(K).GT.0) THEN
          IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
            UBOR(K,1) = - XNEBOR(K) * VIT(NUMLIQ(K),NBOR(K))
            VBOR(K,1) = - YNEBOR(K) * VIT(NUMLIQ(K),NBOR(K))
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
            UBOR(K,1) = UBOR(K,2)
            VBOR(K,1) = VBOR(K,2)
          ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
            UBOR(K,1) = - XNEBOR(K) * UBOR(K,2)
            VBOR(K,1) = - YNEBOR(K) * UBOR(K,2)
          ENDIF
        ENDIF
      ENDIF
C
C  IMPOSED TRACER
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
        IF(LITBOR%ADR(ITRAC)%P%I(K).EQ.KENT.AND.
     &    (NTRACE.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
C         THE CASE NUMLIQ(K)=0 CORRESPONDS TO A SINGULARITY INITIALLY
C         DECLARED AS A SOLID BOUNDARY AND FOR WHICH
C         TBOR IS FILLED IN CLHUVT
          IF(NUMLIQ(K).GT.0) THEN
            Z = TR(NUMLIQ(K),ITRAC,NBOR(K),IERR)
            IF(IERR.EQ.0) TBOR%ADR(ITRAC)%P%R(K) = Z
          ENDIF
        ENDIF
        ENDDO
      ENDIF
C
5     CONTINUE
C
C  QUADRATIC VELOCITIES
C
      IF(U%ELM .EQ.13)THEN
        DO K=1,NPTFR
          IF(LIUBOR(K+NPTFR).EQ.KENT.AND.
     &  (NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ')) THEN
        U%R(NBOR(K+NPTFR)) = (UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))/2.D0
        V%R(NBOR(K+NPTFR)) = (VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))/2.D0
          ENDIF
        ENDDO
      ENDIF
C
C  CASE OF DISCHARGE IMPOSED:
C
C  LOOP ON LIQUID BOUNDARIES
C
      IF(NFRLIQ.NE.0) THEN
C
      DO 10 IFRLIQ = 1 , NFRLIQ
C
      IF(NDEBIT.GT.0.OR.NOMIMP(1:1).NE.' ') THEN
C
C         ONE TAKES THE MASK OF LIQUID BOUNDARIES MSK8, WHICH IS
C         EQUAL TO THE MASK OF THE DISCHARGE IMPOSED ON A DISCHARGE
C         IMPOSED BOUNDARY. THIS MAKES IT POSSIBLE TO CHANGE A FREE
C         VELOCITY BOUNDARY TO A DISCHARGE IMPOSED TO A LEVEL IMPOSED
C         BOUNDARY, IN SPITE OF THE FACT THAT THE MASKS ARE MADE IN
C         PROPIN BEFORE THE CALL TO BORD
C
          IF(NCSIZE.GT.1) YADEB(IFRLIQ)=P_IMAX(YADEB(IFRLIQ))
          IF(YADEB(IFRLIQ).EQ.1) THEN
            CALL DEBIMP(Q(IFRLIQ),UBOR,VBOR,U,V,H,NUMLIQ,
     &                  IFRLIQ,TRA05,TRA06,
     &                  NPTFR,MASK%ADR(MSK8)%P%R,MESH,MESH%KP1BOR%I,
     &                  EQUA)
          ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
10    CONTINUE
C
      ENDIF
C
C QUADRATIC VELOCITIES
C
      IF(U%ELM.EQ.13) THEN
        DO K=1,NPTFR
          UBOR(K+NPTFR,1) =(UBOR(K,1)+UBOR(MESH%KP1BOR%I(K),1))*0.5D0
          VBOR(K+NPTFR,1) =(VBOR(K,1)+VBOR(MESH%KP1BOR%I(K),1))*0.5D0
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
