C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS THE NORMALS TO THE NODES IN ACCORDANCE WITH
!>                THE BOUNDARY CONDITIONS TO HAVE NORMALS TO ADJACENT
!>                LIQUID SEGMENTS IN THE CASE OF A TRANSITION BETWEEN
!>                LIQUID AND SOLID.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> KLOG, KP1BOR, LIHBOR, MESH, NPTFR, T1, T2, XNEBOR, XSGBOR, YNEBOR, YSGBOR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K, KM1, KP1, XNORM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM_BORD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!> </td><td> 19/09/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES TYPES DE CONDITIONS AUX
!>                  LIMITES TECHNIQUES.
!>                  KDIR:DIRICHLET
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES SUR H
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSE GLOBAL DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XSGBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YSGBOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORNOR
     &(XNEBOR,YNEBOR,XSGBOR,YSGBOR,KP1BOR,NPTFR,KLOG,
     & LIHBOR,T1,T2,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KDIR           |-->| CONVENTION POUR LES TYPES DE CONDITIONS AUX
C|                |   | LIMITES TECHNIQUES.
C|                |   | KDIR:DIRICHLET
C| KLOG           |---| 
C| KP1BOR         |---| 
C| LIHBOR         |-->| CONDITIONS AUX LIMITES SUR H
C| MESH           |---| 
C| NBOR           |-->| ADRESSE GLOBAL DES POINTS DE BORD
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| T1             |---| 
C| T2             |---| 
C| XNEBOR         |---| 
C| XSGBOR         |---| 
C| YNEBOR         |---| 
C| YSGBOR         |---| 
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
      INTEGER, INTENT(IN)             :: NPTFR,KLOG
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR)  ,KP1BOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
      DOUBLE PRECISION, INTENT(INOUT) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
C
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,KP1,KM1
      DOUBLE PRECISION XNORM
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
      IF(NCSIZE.LE.1) THEN
C
C     IN SCALAR MODE
C
      DO K = 1 , NPTFR
C
C     LOOP OVER THE BOUNDARY POINTS
C
C     IF THE NODE IS BETWEEN A LIQUID SEGMENT AND A SOLID SEGMENT
C     ONLY CONSIDERS THE NORMAL TO THE ADJACENT LIQUID SEGMENT.
C
      KP1 = KP1BOR(K,1)
      KM1 = KP1BOR(K,2)
C
      IF( LIHBOR(KM1).EQ.KLOG.AND.
     &    LIHBOR(K  ).NE.KLOG.AND.
     &    LIHBOR(KP1).NE.KLOG      ) THEN
C
        XNEBOR(K,1) = XSGBOR(K,1)
        YNEBOR(K,1) = YSGBOR(K,1)
C
      ELSEIF( LIHBOR(KM1).NE.KLOG.AND.
     &        LIHBOR(K  ).NE.KLOG.AND.
     &        LIHBOR(KP1).EQ.KLOG      ) THEN
C
        XNEBOR(K,1) = XSGBOR(KM1,1)
        YNEBOR(K,1) = YSGBOR(KM1,1)
C
      ENDIF
C
      ENDDO
C
      ELSE
C
C     IN PARALLEL MODE
C
C     CONSIDERS ONLY THE NORMALS TO LIQUID BOUNDARIES HERE
C     THE ONLY ONES USED
C
C     COPIES THEN CANCELS T1 AND T2 FOR SOLID BOUNDARIES
C
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
C         NON NORMALISED VERSION OF XSGBOR AND YSGBOR
          T1%R(K)=XSGBOR(K,3)
          T2%R(K)=YSGBOR(K,3)
          XNEBOR(K,1)=0.D0
          YNEBOR(K,1)=0.D0
          KP1 = KP1BOR(K,1)
C         IF KP1 NOT IN DOMAIN: KP1=K, IT WORKS
          IF(LIHBOR(K).EQ.KLOG.OR.LIHBOR(KP1).EQ.KLOG) THEN
            T1%R(K)=0.D0
            T2%R(K)=0.D0
          ENDIF
        ENDDO
      ENDIF
C
C     START OF COMPUTATION OF XNEBOR AND YNEBOR FOR THE LIQUID BOUNDARIES
C
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
          KP1 = KP1BOR(K,1)
C         IF SEGMENT IN DOMAIN
          IF(K.NE.KP1) THEN
            XNEBOR(K  ,1)=XNEBOR(K  ,1)+T1%R(K)
            YNEBOR(K  ,1)=YNEBOR(K  ,1)+T2%R(K)
            XNEBOR(KP1,1)=XNEBOR(KP1,1)+T1%R(K)
            YNEBOR(KP1,1)=YNEBOR(KP1,1)+T2%R(K)
          ENDIF
        ENDDO
      ENDIF
C
C     ASSEMBLY IN PARALLEL
C
      CALL PARCOM_BORD(XNEBOR(1:NPTFR,1),2,MESH)
      CALL PARCOM_BORD(YNEBOR(1:NPTFR,1),2,MESH)
C
C     RENORMALISATION
C
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
          XNORM=SQRT(XNEBOR(K,1)**2+YNEBOR(K,1)**2)
          IF(XNORM.GT.1.D-10) THEN
            XNEBOR(K,1)=XNEBOR(K,1)/XNORM
            YNEBOR(K,1)=YNEBOR(K,1)/XNORM
          ELSE
C           POINT BETWEEN TWO SOLID SEGMENTS
C           TAKES THE COMPUTATION DONE IN NORMAB
            XNORM=SQRT(XNEBOR(K,2)**2+YNEBOR(K,2)**2)
            XNEBOR(K,1)=XNEBOR(K,2)/XNORM
            YNEBOR(K,1)=YNEBOR(K,2)/XNORM
          ENDIF
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