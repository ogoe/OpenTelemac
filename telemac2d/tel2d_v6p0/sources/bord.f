C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE BOUNDARY CONDITIONS ARRAYS
!>                WHEN THEY VARY IN TIME.
!>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THIS SUBROUTINE CAN BE COMPLETED BY THE USER DIRECTLY,
!>         OR THROUGH THE FUNCTIONS: Q , SL , TR , VIT

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EQUA, H, HBOR, KENT, KENTU, LIHBOR, LITBOR, LIUBOR, MASK, MESH, NBOR, NCOTE, NDEBIT, NFRLIQ, NOMIMP, NPOIN, NPTFR, NPTFR2, NTRAC, NTRACE, NUMLIQ, NVITES, PROVEL, TBOR, TEMPS, TRA05, TRA06, U, UBOR, V, VBOR, XNEBOR, YNEBOR, ZF
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::FLUX_BOUNDARIES FLUX_BOUNDARIES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PTS_CURVES PTS_CURVES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QZ QZ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::STA_DIS_CURVES STA_DIS_CURVES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IERR, IFRLIQ, ITRAC, K, MSK8, YADEB, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BORD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DEBIMP(), P_IMAX(), Q(), SL(), STA_DIS_CUR(), TR(), VIT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COTE
!></td><td><-></td><td>TABLEAU DE COTES DE LA SURFACE LIBRE IMPOSEES
!>    </td></tr>
!>          <tr><td>COTINI
!></td><td>--></td><td>COTE INITIALE
!>    </td></tr>
!>          <tr><td>DEBIT
!></td><td><-></td><td>TABLEAU DE DEBITS IMPOSES
!>    </td></tr>
!>          <tr><td>EQUA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td><--</td><td>HAUTEUR IMPOSEE.
!>    </td></tr>
!>          <tr><td>KENT,KENTU,
!></td><td>--></td><td>CONVENTION POUR LES TYPES DE CONDITIONS AUX
!>                  KENTU:U ET V IMPOSES
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR LE TRACEUR
!>    </td></tr>
!>          <tr><td>LIUBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NCOTE
!></td><td>--></td><td>NOMBRE DE FRONTIERES A COTE IMPOSEE
!>    </td></tr>
!>          <tr><td>NDEBIT
!></td><td>--></td><td>NOMBRE DE FRONTIERES A DEBIT IMPOSE
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>--></td><td>NOMBRE DE FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>NOMIMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE.
!>    </td></tr>
!>          <tr><td>NPTFR2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRACE
!></td><td>--></td><td>NOMBRE DE FRONTIERES A TRACEUR IMPOSE
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVITES
!></td><td>--></td><td>NOMBRE DE FRONTIERES A VITESSE IMPOSEE
!>    </td></tr>
!>          <tr><td>PROVEL
!></td><td>--></td><td>OPTION POUR LES PROFILS DE VITESSE
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td><--</td><td>TRACEUR IMPOSE AU BORD
!>    </td></tr>
!>          <tr><td>TEMPS
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>TRA05,TRA06
!></td><td>--></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>TRACER
!></td><td><-></td><td>TABLEAU DE VALEURS DU TRACEUR IMPOSEES
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE AU TEMPS N
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td><--</td><td>VITESSE U IMPOSEE.
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td><--</td><td>VITESSE V IMPOSEE.
!>    </td></tr>
!>          <tr><td>VITES
!></td><td><-></td><td>TABLEAU DE COMPOSANTES NORMALES DE LA VITESSE
!>                  IMPOSEES
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>FOND
!>    </td></tr>
!>     </table>
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
C| H             |-->| HAUTEUR AU TEMPS N
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
     &                                   FLUX_BOUNDARIES
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
      CHARACTER(LEN=20), INTENT(IN) :: EQUA
      CHARACTER(LEN=144), INTENT(IN) :: NOMIMP
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFR2,2),VBOR(NPTFR2,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR)
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: H,U,V,TRA05,TRA06,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)  :: MASK,LITBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,MSK8,IFRLIQ,YADEB(300),IERR,ITRAC
C
      DOUBLE PRECISION Z
C
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
      MSK8 = 8
C
C  INITIALISATION OF YADEB
C
      IF(NFRLIQ.GE.1) THEN
        DO 1 K=1,NFRLIQ
          YADEB(K)=0
1       CONTINUE
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
        IF(PROVEL(NUMLIQ(K)).EQ.1) THEN
C         CONSTANT NORMAL PROFILE
          UBOR(K,1) = -XNEBOR(K)
          VBOR(K,1) = -YNEBOR(K)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.2) THEN
C         PROFILE PROVIDED BY THE USER
          UBOR(K,1) = UBOR(K,2)
          VBOR(K,1) = VBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.3) THEN
C         NORMAL VELOCITY PROVIDED IN UBOR
          UBOR(K,1) = -XNEBOR(K)*UBOR(K,2)
          VBOR(K,1) = -YNEBOR(K)*UBOR(K,2)
        ELSEIF(PROVEL(NUMLIQ(K)).EQ.4) THEN
C         NORMAL PROFILE IN SQUARE ROOT OF H
          UBOR(K,1) = -XNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
          VBOR(K,1) = -YNEBOR(K) * SQRT(MAX(H%R(NBOR(K)),0.D0))
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