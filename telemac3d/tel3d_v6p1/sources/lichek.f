C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       HARMONISES THE BOUNDARY CONDITIONS, INITIALISES 'IBOR'.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE ORDER OF THE INDICES HAS CHANGED FOR THIS VARIABLE
!>            BECAUSE OF THE 2D ADVECTION BY CHARACTERISTICS OF H
!>            WITH TIDAL FLATS (PASSES IBOR INSTEAD OF IFABOR)
!>  @code
!>      BEFORE :   IBOR(IELEM,IETAGE,IFACE)
!>      NOW :      IBOR(IELEM,IFACE,IETAGE)
!>  @endcode

!>  @warning  JAJ: DOES NOT HARMONISE THE BC'S IN THE NON-HYDROSTATIC
!>            CASE. IN PRINCIPLE, BC TYPES CAN BE DIFFERENT FOR U, V
!>            AND W VELOCITY COMPONENTS

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
!> </td><td> 15/11/08
!> </td><td> JMH (LNHE)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 15/09/2008
!> </td><td> JMH
!> </td><td> TREATS KP1BOR IN PARALLEL
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/12/98
!> </td><td> JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!> </td><td> NON-HYDROSTATIC VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE LICHEK
     & (LIMPRP, NPTFR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| KADH           |-->| INDICATEUR DE POINT DIRICHLET
C| KDDL           |-->| INDICATEUR DE DEGRE DE LIBERTE AU BORD
C| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
C| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
C|                |   | CONDITIONS AUX LIMITES TECHNIQUES:
C| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
C| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
C| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE
C| LIHBOR         |<->| TYPES DE CONDITIONS AUX LIMITES SUR H
C| LIMPRP         |<--| TYPES DE CONDITIONS AUX LIMITES POUR LA
C|                |   | PROPAGATION
C|                |   | PAR POINTS   :    .1:H  .2:U  .3:V
C|                |   | PAR SEGMENTS :    .4:H  .5:U  .6:V
C| LIUBOR         |<->| TYPES DE CONDITIONS AUX LIMITES SUR U
C| LIVBOR         |<->| TYPES DE CONDITIONS AUX LIMITES SUR V
C| LIWBOR         |<->| TYPES DE CONDITIONS AUX LIMITES SUR W
C| MASK           |<--| MASQUES POUR LES SEGMENTS 2D
C|                |   | MASK(NPTFR2,1): 1. SI KDIR SUR U 0. SINON
C|                |   | MASK(NPTFR2,2): 1. SI KDIR SUR V 0. SINON
C|                |   | MASK(NPTFR2,3): 1. SI KDDL SUR U 0. SINON
C|                |   | MASK(NPTFR2,4): 1. SI KDDL SUR V 0. SINON
C|                |   | MASK(NPTFR2,5): 1. SI KNEU SUR U 0. SINON
C|                |   | MASK(NPTFR2,6): 1. SI KNEU SUR V 0. SINON
C|                |   | MASK(NPTFR2,7): 1. SI KOND 0. SINON
C|                |   | (KOND N'EST PAS DEFINI DANS TELEMAC-3D,
C|                |   | CAR IL N Y A PAS D'ONDE INCIDENTE. EN
C|                |   | CONSEQUENCE, MASK(*,7)=0)
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NPTFR
      INTEGER, INTENT(INOUT) :: LIMPRP(NPTFR,6)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN2, IPTFR, IPTFR3, IPTFRX
      DOUBLE PRECISION C
!
!***********************************************************************
!
C HARMONISES THE BOUNDARY CONDITIONS
!
!=======================================================================
!
      IF (.NOT.NONHYD) THEN
!
        DO IPOIN2 = 1,NPOIN2
          IF(LIUBOF%I(IPOIN2).EQ.KLOG.OR.
     &       LIVBOF%I(IPOIN2).EQ.KLOG.OR.
     &       LIWBOF%I(IPOIN2).EQ.KLOG) THEN
             LIUBOF%I(IPOIN2) = KLOG
             LIVBOF%I(IPOIN2) = KLOG
             LIWBOF%I(IPOIN2) = KLOG
          ENDIF
          IF(LIUBOS%I(IPOIN2).EQ.KLOG.OR.
     &       LIVBOS%I(IPOIN2).EQ.KLOG.OR.
     &       LIWBOS%I(IPOIN2).EQ.KLOG) THEN
             LIUBOS%I(IPOIN2) = KLOG
             LIVBOS%I(IPOIN2) = KLOG
             LIWBOS%I(IPOIN2) = KLOG
          ENDIF
        ENDDO
!
        DO IPTFR3 = 1,NPTFR3
          IF(LIUBOL%I(IPTFR3).EQ.KLOG .OR.
     &       LIVBOL%I(IPTFR3).EQ.KLOG .OR.
     &       LIWBOL%I(IPTFR3).EQ.KLOG) THEN
             LIUBOL%I(IPTFR3) = KLOG
             LIVBOL%I(IPTFR3) = KLOG
             LIWBOL%I(IPTFR3) = KLOG
          ENDIF
        ENDDO
!
      ENDIF  ! (IF .NOT.NONHYD)
!
!=======================================================================
!
C INITIALISES THE BOUNDARY CONDITIONS FOR PROPAGATION IN 2D:
!
!=======================================================================
!
      DO IPTFR = 1,NPTFR
!
C   BOUNDARY CONDITIONS ON H
!
      IF(LIHBOR%I(IPTFR).EQ.KENT ) THEN
         LIMPRP(IPTFR,1) = KDIR
         LIMPRP(IPTFR,4) = KDDL
         IPTFRX = MESH2D%KP1BOR%I(IPTFR)
         IF(LIHBOR%I(IPTFRX).EQ.KENT) LIMPRP(IPTFR,4) = KDIR
         IF(LIHBOR%I(IPTFRX).EQ.KLOG) LIMPRP(IPTFR,4) = KNEU
      ELSEIF(LIHBOR%I(IPTFR).EQ.KSORT) THEN
         LIMPRP(IPTFR,1) = KDDL
         LIMPRP(IPTFR,4) = KDDL
         IPTFRX = MESH2D%KP1BOR%I(IPTFR)
         IF(LIHBOR%I(IPTFRX).EQ.KLOG) LIMPRP(IPTFR,4) = KNEU
      ELSEIF(LIHBOR%I(IPTFR).EQ.KLOG ) THEN
         LIMPRP(IPTFR,1) = KDDL
         LIMPRP(IPTFR,4) = KNEU
      ELSE
         IF (LNG.EQ.1) WRITE(LU,101) IPTFR,LIHBOR%I(IPTFR)
         IF (LNG.EQ.2) WRITE(LU,102) IPTFR,LIHBOR%I(IPTFR)
         CALL PLANTE(1)
         STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
C   BOUNDARY CONDITIONS ON U
C  (TAKES THE BOUNDARY CONDITIONS DEFINED ON THE SECOND PLANE
C   AS MEAN 2D VELOCITY)
!
      IPTFR3 = NPTFR + IPTFR
      IPTFRX = NPTFR + MESH2D%KP1BOR%I(IPTFR)

      IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &   LIUBOL%I(IPTFR3).EQ.KENTU.OR.
     &   LIUBOL%I(IPTFR3).EQ.KADH) THEN
         LIMPRP(IPTFR,2) = KDIR
         LIMPRP(IPTFR,5) = KDDL
         IF(LIUBOL%I(IPTFRX).EQ.KENT.OR.
     &     LIUBOL%I(IPTFRX).EQ.KENTU) LIMPRP(IPTFR,5) = KDIR
         IF(LIUBOL%I(IPTFRX).EQ.KADH) LIMPRP(IPTFR,5) = KDIR
         IF(LIUBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IPTFR,5) = KNEU
      ELSEIF(LIUBOL%I(IPTFR3).EQ.KSORT) THEN
         LIMPRP(IPTFR,2) = KDDL
         LIMPRP(IPTFR,5) = KDDL
         IF(LIUBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IPTFR,5) = KNEU
      ELSEIF(LIUBOL%I(IPTFR3).EQ.KLOG ) THEN
         LIMPRP(IPTFR,2) = KDDL
         LIMPRP(IPTFR,5) = KNEU
      ELSE
         IF (LNG.EQ.1) WRITE(LU,201) IPTFR3,LIUBOL%I(IPTFR3)
         IF (LNG.EQ.2) WRITE(LU,202) IPTFR3,LIUBOL%I(IPTFR3)
         CALL PLANTE(1)
         STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   BOUNDARY CONDITIONS ON V
!  (TAKES THE BOUNDARY CONDITIONS DEFINED ON THE SECOND PLANE
!   AS MEAN 2D VELOCITY)
!
      IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &   LIVBOL%I(IPTFR3).EQ.KENTU.OR.
     &   LIVBOL%I(IPTFR3).EQ.KADH) THEN
         LIMPRP(IPTFR,3) = KDIR
         LIMPRP(IPTFR,6) = KDDL
         IF(LIVBOL%I(IPTFRX).EQ.KENT.OR.
     &      LIVBOL%I(IPTFRX).EQ.KENTU) LIMPRP(IPTFR,6) = KDIR
         IF(LIVBOL%I(IPTFRX).EQ.KADH) LIMPRP(IPTFR,6) = KDIR
         IF(LIVBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IPTFR,6) = KNEU
      ELSEIF(LIVBOL%I(IPTFR3).EQ.KSORT) THEN
         LIMPRP(IPTFR,3) = KDDL
         LIMPRP(IPTFR,6) = KDDL
         IF(LIVBOL%I(IPTFRX).EQ.KLOG) LIMPRP(IPTFR,6) = KNEU
      ELSEIF(LIVBOL%I(IPTFR3).EQ.KLOG ) THEN
         LIMPRP(IPTFR,3) = KDDL
         LIMPRP(IPTFR,6) = KNEU
      ELSE
         IF(LNG.EQ.1) WRITE(LU,301) IPTFR3,LIVBOL%I(IPTFR3)
         IF(LNG.EQ.2) WRITE(LU,302) IPTFR3,LIVBOL%I(IPTFR3)
         CALL PLANTE(1)
         STOP
      ENDIF
!
!     IN PARALLEL, IF THE SEGMENT IS NOT IN THE SUB-DOMAIN
!     WRONG VALUES INTENTIONALLY GIVEN (SHOULD NOT BE USED)
!
      IPTFRX = MESH2D%KP1BOR%I(IPTFR)
      IF(IPTFR.EQ.IPTFRX) THEN
        LIMPRP(IPTFR,4) = 999
        LIMPRP(IPTFR,5) = 999
        LIMPRP(IPTFR,6) = 999
      ENDIF
!
      ENDDO
!
!=======================================================================
! FILLS MASK
!=======================================================================
!
      CALL OS('X=0     ',X=MASK)
!
      IF (.NOT. MSK) THEN
C
       C = 1.D0
       DO IPTFR = 1,NPTFR
!
!       MASKEL, BD ELEMENT FROM THE *FIRST* ETAGE
!
        IF(LIMPRP(IPTFR,5).EQ.KDIR) MASK%ADR(1)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,6).EQ.KDIR) MASK%ADR(2)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,5).EQ.KDDL) MASK%ADR(3)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,6).EQ.KDDL) MASK%ADR(4)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,5).EQ.KNEU) MASK%ADR(5)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,6).EQ.KNEU) MASK%ADR(6)%P%R(IPTFR) = C
!
        MASK%ADR(7)%P%R(IPTFR) = 0.D0
        MASK%ADR(8)%P%R(IPTFR) = (1.D0 - MASK%ADR(5)%P%R(IPTFR)) * C
!
      ENDDO

      ELSE
      DO IPTFR = 1,NPTFR
!
!       MASKEL, BD ELEMENT FROM THE *FIRST* ETAGE
!
        C = MASKEL%R(MESH2D%NELBOR%I(IPTFR))
!
        IF(LIMPRP(IPTFR,5).EQ.KDIR) MASK%ADR(1)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,6).EQ.KDIR) MASK%ADR(2)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,5).EQ.KDDL) MASK%ADR(3)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,6).EQ.KDDL) MASK%ADR(4)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,5).EQ.KNEU) MASK%ADR(5)%P%R(IPTFR) = C
        IF(LIMPRP(IPTFR,6).EQ.KNEU) MASK%ADR(6)%P%R(IPTFR) = C
!
        MASK%ADR(7)%P%R(IPTFR) = 0.D0
        MASK%ADR(8)%P%R(IPTFR) = (1.D0 - MASK%ADR(5)%P%R(IPTFR)) * C
!
      ENDDO
!
      ENDIF
!
!     PARALLELISM: MASKS SET TO 0 FOR SEGMENTS OUTSIDE THE SUB-DOMAIN
!                  IT COULD BE DONE SIMPLY BY INITIALISATION TO 0
!                  BUT BEWARE MASK%ADR(8)%P%R(IPTFR) = 1.D0-  ....
!
      IF(NCSIZE.GT.1) THEN
        DO IPTFR = 1,NPTFR
          IF(LIMPRP(IPTFR,4).EQ.999) THEN
            MASK%ADR(1)%P%R(IPTFR) = 0.D0
            MASK%ADR(2)%P%R(IPTFR) = 0.D0
            MASK%ADR(3)%P%R(IPTFR) = 0.D0
            MASK%ADR(4)%P%R(IPTFR) = 0.D0
            MASK%ADR(5)%P%R(IPTFR) = 0.D0
            MASK%ADR(6)%P%R(IPTFR) = 0.D0
            MASK%ADR(7)%P%R(IPTFR) = 0.D0
            MASK%ADR(8)%P%R(IPTFR) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
101   FORMAT(' LICHEK: POINT DE BORD',I5,'LIHBOR = ',1I6)
102   FORMAT(' LICHEK: BOUNDARY NODE',I5,'LIHBOR = ',1I6)
201   FORMAT(' LICHEK: POINT DE BORD',I5,'LIUBOL = ',1I6)
202   FORMAT(' LICHEK: BOUNDARY NODE',I5,'LIUBOL = ',1I6)
301   FORMAT(' LICHEK: POINT DE BORD',I5,'LIVBOL = ',1I6)
302   FORMAT(' LICHEK: BOUNDARY NODE',I5,'LIVBOL = ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
