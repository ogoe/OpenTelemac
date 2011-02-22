C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SUPPLEMENTS THE BOUNDARY CONDITION FILE
!>                FOR THE QUADRATIC ELEMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ATBOR, AUBOR, BTBOR, IELMT, IELMU, IELMV, KADH, KENT, KENTU, KINC, KLOG, KSORT, LITBOR, LIUBOR, LIVBOR, MESH, NBOR, NPOIN, NPTFR, TBOR, TRAC, UBOR, VBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K, KP1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
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
!> </td><td> 23/10/2008
!> </td><td> ALGIANE FROEHLY (MATMECA PLACEMENT)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ATBOR,BTBOR
!></td><td><-></td><td>COEFFICIENTS D'ECHANGE THERMIQUE.
!>    </td></tr>
!>          <tr><td>AUBOR
!></td><td><-></td><td>COEFFICIENT DE FROTTEMENT AU BORD
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE PAROI (ADHERENCE)
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>TYPE DE CONDITION LIMITE D'ENTREE.
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>--></td><td>TYPE DE CONDITION LIMITE : VITESSES IMPOSEES
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>--></td><td>TYPE DE CONDITION LIMITE D'ONDE INCIDENTE
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE PAROI (PAROI)
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>TYPE DE CONDITION LIMITE DE SORTIE LIBRE
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td><-></td><td>TYPES DE CONDITIONS AUX LIMITES EN TEMPERA-
!>                  TURE POUR LES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>LIUBOR,LIVBOR
!></td><td><-></td><td>TYPES DE CONDITIONS AUX LIMITES POUR LES
!>                  POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td><-></td><td>TRACEUR AUX BORDS
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>INDICATEUR DE TRACEUR .
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td><-></td><td>CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td><-></td><td>CONDITIONS AUX LIMITES SUR V
!>                  (COEFFICIENTS DE LA LOI LOG)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COMPLIM
     &(LIUBOR,LIVBOR,LITBOR,UBOR,VBOR,TBOR,
     & AUBOR,ATBOR,BTBOR,NBOR,NPTFR,NPOIN,TRAC,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,IELMU,IELMV,IELMT,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ATBOR,BTBOR    |<->| COEFFICIENTS D'ECHANGE THERMIQUE.
C| AUBOR          |<->| COEFFICIENT DE FROTTEMENT AU BORD
C| IELMT          |---| 
C| IELMU          |---| 
C| IELMV          |---| 
C| KADH           |-->| TYPE DE CONDITION LIMITE DE PAROI (ADHERENCE)
C| KENT           |-->| TYPE DE CONDITION LIMITE D'ENTREE.
C| KENTU          |-->| TYPE DE CONDITION LIMITE : VITESSES IMPOSEES
C| KINC           |-->| TYPE DE CONDITION LIMITE D'ONDE INCIDENTE
C| KLOG           |-->| TYPE DE CONDITION LIMITE DE PAROI (PAROI)
C| KSORT          |-->| TYPE DE CONDITION LIMITE DE SORTIE LIBRE
C| LITBOR         |<->| TYPES DE CONDITIONS AUX LIMITES EN TEMPERA-
C|                |   | TURE POUR LES POINTS DE BORD.
C| LIUBOR,LIVBOR  |<->| TYPES DE CONDITIONS AUX LIMITES POUR LES
C|                |   | POINTS DE BORD.
C| MESH           |---| 
C| NBOR           |---| 
C| NPOIN          |---| 
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| TBOR           |<->| TRACEUR AUX BORDS
C| TRAC           |-->| INDICATEUR DE TRACEUR .
C| UBOR           |<->| CONDITIONS AUX LIMITES SUR U
C| VBOR           |<->| CONDITIONS AUX LIMITES SUR V
C|                |   | (COEFFICIENTS DE LA LOI LOG)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,NPOIN,KENT,KSORT,KADH,KLOG,KINC,KENTU
      INTEGER, INTENT(IN) :: IELMU,IELMV,IELMT
      LOGICAL, INTENT(IN) :: TRAC
      INTEGER, INTENT(INOUT) :: LIUBOR(*),LIVBOR(*)
      INTEGER, INTENT(INOUT) :: LITBOR(*)!,NBOR(2*NPTFR)
      INTEGER, INTENT(INOUT) :: NBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(2*NPTFR,2),VBOR(2*NPTFR,2)
      DOUBLE PRECISION, INTENT(INOUT) :: AUBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(*),ATBOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BTBOR(*)
      TYPE(BIEF_MESH),INTENT(INOUT)   :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,KP1
C
C----------------------------------------------------------------------
C
C     VELOCITY WITH QUADRATIC U-COMPONENT
C
      IF(IELMU.EQ.13) THEN
C
        DO K=1,NPTFR
C
        KP1=MESH%KP1BOR%I(K)
C
        IF(KP1.NE.K) THEN
        IF(LIUBOR(K).EQ.LIUBOR(KP1)) THEN
          LIUBOR(K+NPTFR) = LIUBOR(K)
        ELSEIF( LIUBOR(K  ).EQ.KLOG .OR.
     &          LIUBOR(KP1).EQ.KLOG       ) THEN
          LIUBOR(K+NPTFR) = KLOG
        ELSEIF( LIUBOR(K  ).EQ.KADH .OR.
     &          LIUBOR(KP1).EQ.KADH       ) THEN
          LIUBOR(K+NPTFR) = KADH
        ELSEIF( LIUBOR(K  ).EQ.KENTU .OR.
     &          LIUBOR(KP1).EQ.KENTU      ) THEN
          LIUBOR(K+NPTFR) = KENTU
        ELSEIF( LIUBOR(K  ).EQ.KSORT .OR.
     &          LIUBOR(KP1).EQ.KSORT      ) THEN
          LIUBOR(K+NPTFR) = KSORT
        ELSEIF( LIUBOR(K  ).EQ.KINC .OR.
     &          LIUBOR(KP1).EQ.KINC       ) THEN
          LIUBOR(K+NPTFR) = KINC
        ELSE
          WRITE(LU,*) 'CONDITION INITIALE QUADRATIQUE DE U ','K ',K,
     &                ' NON PREVUE POUR LIUBOR = ',LIUBOR(K),
     &                ' ET LIUBOR(K+1) = ',LIUBOR(MESH%KP1BOR%I(K))
          CALL PLANTE(1)
          STOP
        ENDIF
        UBOR(K+NPTFR,1) = (UBOR(K,1)+UBOR(KP1,1))*0.5D0
C
        ENDIF
C
        ENDDO
C
      ENDIF
C
C     VELOCITY WITH QUADRATIC V-COMPONENT
C
      IF(IELMV.EQ.13) THEN
C
        DO K=1,NPTFR
C
        KP1=MESH%KP1BOR%I(K)
C
        IF(KP1.NE.K) THEN
        IF(LIVBOR(K).EQ.LIVBOR(KP1)) THEN
          LIVBOR(K+NPTFR) = LIVBOR(K)
        ELSEIF( LIVBOR(K  ).EQ.KLOG .OR.
     &          LIVBOR(KP1).EQ.KLOG       ) THEN
          LIVBOR(K+NPTFR) = KLOG
        ELSEIF( LIVBOR(K  ).EQ.KADH .OR.
     &          LIVBOR(KP1).EQ.KADH       ) THEN
          LIVBOR(K+NPTFR) = KADH
        ELSEIF( LIVBOR(K  ).EQ.KENTU .OR.
     &          LIVBOR(KP1).EQ.KENTU      ) THEN
          LIVBOR(K+NPTFR) = KENTU
        ELSEIF( LIVBOR(K  ).EQ.KSORT .OR.
     &          LIVBOR(KP1).EQ.KSORT      ) THEN
          LIVBOR(K+NPTFR) = KSORT
        ELSEIF( LIVBOR(K  ).EQ.KINC .OR.
     &          LIVBOR(KP1).EQ.KINC       ) THEN
          LIVBOR(K+NPTFR) = KINC
        ELSE
          WRITE(LU,*) 'CONDITION INITIALE QUADRATIQUE DE U ','K ',K,
     &                ' NON PREVUE POUR LIUBOR = ',LIUBOR(K),
     &                ' ET LIUBOR(K+1) = ',LIUBOR(MESH%KP1BOR%I(K))
          CALL PLANTE(1)
          STOP
        ENDIF
        VBOR(K+NPTFR,1) = (VBOR(K,1)+VBOR(KP1,1))*0.5D0
        ENDIF
C
        ENDDO
C
      ENDIF
C
      IF(IELMV.EQ.13.OR.IELMU.EQ.13) THEN
        DO K=1,NPTFR
          AUBOR(K+NPTFR) = (AUBOR(K)+AUBOR(MESH%KP1BOR%I(K)))*0.5D0
        ENDDO
      ENDIF
C
C     WITH QUADRATIC TRACER T
C
      IF(TRAC.AND.IELMT.EQ.13) THEN
C
        DO K=1,NPTFR
C
        KP1=MESH%KP1BOR%I(K)
C
        IF(KP1.NE.K) THEN
        IF(LITBOR(K).EQ.LITBOR(KP1)) THEN
          LITBOR(K+NPTFR) = LITBOR(K)
        ELSEIF( LITBOR(K  ).EQ.KLOG .OR.
     &          LITBOR(KP1).EQ.KLOG       ) THEN
          LITBOR(K+NPTFR) = KLOG
        ELSEIF( LITBOR(K  ).EQ.KADH .OR.
     &          LITBOR(KP1).EQ.KADH       ) THEN
          LITBOR(K+NPTFR) = KADH
        ELSEIF( LITBOR(K  ).EQ.KENTU .OR.
     &          LITBOR(KP1).EQ.KENTU      ) THEN
          LITBOR(K+NPTFR) = KENTU
        ELSEIF( LITBOR(K  ).EQ.KSORT .OR.
     &          LITBOR(KP1).EQ.KSORT      ) THEN
          LITBOR(K+NPTFR) = KSORT
        ELSEIF( LITBOR(K  ).EQ.KINC  .OR.
     &          LITBOR(KP1).EQ.KINC       ) THEN
          LITBOR(K+NPTFR) = KINC
        ELSE
          WRITE(LU,*) 'CONDITION INITIALE QUADRATIQUE DE U ','K ',K,
     &                ' NON PREVUE POUR LIUBOR = ',LIUBOR(K),
     &                ' ET LIUBOR(K+1) = ',LIUBOR(MESH%KP1BOR%I(K))
          CALL PLANTE(1)
          STOP
        ENDIF
        TBOR(K+NPTFR)  = (TBOR(K)+TBOR(KP1))  *0.5D0
        ATBOR(K+NPTFR) = (ATBOR(K)+ATBOR(KP1))*0.5D0
        BTBOR(K+NPTFR) = (BTBOR(K)+BTBOR(KP1))*0.5D0
        ENDIF
C
        ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  CHECKS, CORRECTS AND SAVES:
C
      IF(IELMU.EQ.13.OR.IELMV.EQ.13) THEN
C
      DO K=NPTFR+1,2*NPTFR
C
C     FRICTION COEFFICIENT SET TO 0 WHEN NOT NEEDED
C
      IF(LIUBOR(K).NE.KLOG.AND.LIVBOR(K).NE.KLOG) AUBOR(K) = 0.D0
C
C     WALL ADHERENCE MODIFIED FOR H
C
      IF(AUBOR(K).GT.0.D0) THEN
        IF(LNG.EQ.1) WRITE(LU,48) K
        IF(LNG.EQ.2) WRITE(LU,49) K
48      FORMAT(1X,'COMPLIM : AUBOR DOIT ETRE NEGATIF OU NUL',/,1X,
     &            '         IL VAUT ',F10.3,' AU POINT DE BORD ',1I6)
49      FORMAT(1X,'COMPLIM : AUBOR MUST BE NEGATIVE',/,1X,
     &            '         IT IS ',F10.3,' AT BOUNDARY POINT ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     DIRICHLET VALUES SET TO 0 WHEN THE POINT IS NOT A DIRICHLET
C     FOR THE NODES WITH WALL ADHERENCE, UBOR OR VBOR =0 IS REQUIRED
C
      IF(LIUBOR(K).NE.KENT.AND.LIUBOR(K).NE.KENTU) UBOR(K,1)=0.D0
      IF(LIVBOR(K).NE.KENT.AND.LIVBOR(K).NE.KENTU) VBOR(K,1)=0.D0
C
C     SAVES UBOR AND VBOR ON THEIR SECOND DIMENSION
C
      UBOR(K,2) = UBOR(K,1)
      VBOR(K,2) = VBOR(K,1)
C
      ENDDO
C
      IF(TRAC) THEN
        DO K=1,NPTFR
          IF(LITBOR(K).NE.KENT) TBOR(K)=0.D0
        ENDDO
      ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C