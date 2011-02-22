C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FILLS 'IFABOR' OF MESH3D.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  WITH TIDAL FLATS, WILL BE MODIFIED BY MASK3D

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IFABOR, IKLE2, KLOG, KP1BOR, LIUBOF, LIUBOL, LIUBOS, NELBOR, NELEM2, NETAGE, NPLAN, NPLINT, NPOIN2, NPTFR, NULONE, TRANSF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FLAG, IELBR, IELEM2, IETAGE, IFACE, IPTFR1, IPTFR2, LOC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 22/07/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; J-M JANIN (LNH) 30 87 72 84; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN 95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>CORRESPONDANCE FACE DE BORD - ELEMENT 2D
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>CORRESPONDANCE LOCALE - GLOGALE EN 2D
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>INDICATEUR DE PAROI SOLIDE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
!>    </td></tr>
!>          <tr><td>LIUBOF,L,S
!></td><td><-></td><td>TYPE DE CONDITIONS AUX LIMITES SUR U
!>    </td></tr>
!>          <tr><td>LIUBOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIUBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS EN 2D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NPLAN - 1
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPLINT
!></td><td>--></td><td>NUMERO DU PLAN INTERMEDIAIRE
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD 2D
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!>                  NUMEROTATION LOCALE 3D
!>    </td></tr>
!>          <tr><td>TRANSF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE IFAB3D
     &(IFABOR,LIUBOF,LIUBOL,LIUBOS,KP1BOR,NELBOR,NULONE,
     & IKLE2,NELEM2,NPOIN2,NPTFR,NPLAN,NPLINT,NETAGE,KLOG,TRANSF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IFABOR         |-->| CORRESPONDANCE FACE DE BORD - ELEMENT 2D
C| IKLE2          |-->| CORRESPONDANCE LOCALE - GLOGALE EN 2D
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
C| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
C| LIUBOF,L,S     |<->| TYPE DE CONDITIONS AUX LIMITES SUR U
C| LIUBOL         |---| 
C| LIUBOS         |---| 
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPLINT         |-->| NUMERO DU PLAN INTERMEDIAIRE
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
C| NULONE         |-->| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C| TRANSF         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NELEM2,NPOIN2,NETAGE,NPLAN,TRANSF
      INTEGER, INTENT(IN)          :: NPTFR,NPLINT,KLOG
      INTEGER, INTENT(INOUT)       :: IFABOR(NELEM2,5,NETAGE)
      INTEGER, INTENT(IN)          :: LIUBOF(NPOIN2),LIUBOS(NPOIN2)
      INTEGER, INTENT(IN)          :: LIUBOL(NPTFR,NPLAN)
      INTEGER, INTENT(IN)          :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN)          :: NULONE(NPTFR,NETAGE,4)
      INTEGER, INTENT(IN)          :: KP1BOR(NPTFR), NELBOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM2, IETAGE, IPTFR1, IPTFR2, IELBR, IFACE,LOC
      LOGICAL FLAG
!
!=======================================================================
C  IFABOR ARRAYS
!=======================================================================
!
C     SEE CHAR41 FOR THE MEANING OF LOC
C     1 IS : DO NOT RECOMPUTE VELOCITIES WHEN CROSSING A MESH PLANE
C     2 IS : RECOMPUTE VELOCITIES WHEN CROSSING A MESH PLANE
!
C     IF(TRANSF.EQ.1.OR.TRANSF.EQ.2.OR.TRANSF.EQ.3) THEN
!
C     JMH ON 18/02/2010: WITH A FIXED PLANE
C                        THE VELOCITY MUST BE RECOMPUTED
      IF(TRANSF.EQ.1.OR.TRANSF.EQ.2) THEN
        LOC = 1
      ELSEIF(TRANSF.EQ.0.OR.TRANSF.EQ.3) THEN
        LOC = 2
      ELSE
        WRITE(LU,*) 'WRONG VALUE OF TRANSF IN IFAB3D: ',TRANSF
        CALL PLANTE(1)
        STOP
      ENDIF
!
C  IFABOR ON THE VERTICAL FACES OF THE ELEMENTS OF THE FIRST LAYER
!
      DO 40 IELEM2 = 1,NELEM2
C        ALREADY DONE BY 2D
C        IFABOR(IELEM2,1,IETAGE) = IFABOR(IELEM2,1,1)
C        IFABOR(IELEM2,2,IETAGE) = IFABOR(IELEM2,2,1)
C        IFABOR(IELEM2,3,IETAGE) = IFABOR(IELEM2,3,1)
         IFABOR(IELEM2,4,1) = LOC
         IFABOR(IELEM2,5,1) = LOC
40    CONTINUE

      IF(NETAGE.GE.2) THEN
        DO IETAGE = 2,NETAGE
          DO IELEM2 = 1,NELEM2
            IFABOR(IELEM2,1,IETAGE) = IFABOR(IELEM2,1,1)
            IFABOR(IELEM2,2,IETAGE) = IFABOR(IELEM2,2,1)
            IFABOR(IELEM2,3,IETAGE) = IFABOR(IELEM2,3,1)
            IFABOR(IELEM2,4,IETAGE) = LOC
            IFABOR(IELEM2,5,IETAGE) = LOC
          ENDDO
        ENDDO
      ENDIF
!
!=======================================================================
!
C TYPES OF BOUNDARY CONDITIONS IMPOSED ON IFABOR
!
!=======================================================================
!
C  IFABOR ON THE LATERAL WALLS
!
      DO IPTFR1 = 1,NPTFR
!
         IPTFR2 = KP1BOR(IPTFR1)
C        IF NEXT POINT IN THE SUBDOMAIN
         IF(IPTFR2.NE.IPTFR1) THEN
           IELBR = NELBOR(IPTFR1)
           IFACE = NULONE(IPTFR1,1,1)
           IF(IFABOR(IELBR,IFACE,1).NE.-2) THEN
             FLAG = .FALSE.
             IF(LIUBOL(IPTFR1,1).NE.KLOG .AND.
     &          LIUBOL(IPTFR2,1).NE.KLOG) FLAG = .TRUE.
             DO IETAGE = 1,NETAGE
               IFABOR(IELBR,IFACE,IETAGE) = -1
               IF(LIUBOL(IPTFR1,IETAGE+1).NE.KLOG .AND.
     &            LIUBOL(IPTFR2,IETAGE+1).NE.KLOG) THEN
                 IF(FLAG) IFABOR(IELBR,IFACE,IETAGE) = 0
                 FLAG = .TRUE.
               ELSE
                 FLAG = .FALSE.
               ENDIF
             ENDDO
           ELSE
             IF(NETAGE.GT.1) THEN
               DO IETAGE = 2,NETAGE
                 IFABOR(IELBR,IFACE,IETAGE) = -2
               ENDDO
             ENDIF
           ENDIF
         ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
C  IFABOR ON THE BOTTOM, SURFACE AND EITHER SIDE OF THE INTERMEDIATE PLANE
!
      DO IELEM2 = 1,NELEM2
!
        IFABOR(IELEM2,4,1) = -1
        IF(LIUBOF(IKLE2(IELEM2,1)).NE.KLOG .AND.
     &     LIUBOF(IKLE2(IELEM2,2)).NE.KLOG .AND.
     &     LIUBOF(IKLE2(IELEM2,3)).NE.KLOG) IFABOR(IELEM2,4,1)=0
!
        IFABOR(IELEM2,5,NETAGE) = -1
        IF(LIUBOS(IKLE2(IELEM2,1)).NE.KLOG .OR.
     &     LIUBOS(IKLE2(IELEM2,2)).NE.KLOG .OR.
     &     LIUBOS(IKLE2(IELEM2,3)).NE.KLOG) IFABOR(IELEM2,5,NETAGE)=0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C