C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE 3D MESH.
!><br>            INPUT: 3D MESH ARRAYS FILLED BY A PRELIMINARY CALL
!>                       TO ELEBD.
!><br>            OUTPUT: ARRAYS COMPLETE IN 3D.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLBOR, IKLE3, KP1BOR, NBOR, NELBOR, NELEM2, NETAGE, NPLAN, NPOIN2, NPTFR, NULONE
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
!>    </th><td> IELEM, IETAGE, IPOIN, IPTFR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ELEB3D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 23/06/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLBOR
!></td><td><--</td><td>TABLE DE CONNECTIVITE ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>CORRESPONDANCE LOCALE - GLOGALE EN 2D
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td><--</td><td>CORRESPONDANCE LOCALE - GLOBALE EN 3D
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
!>                  ET LA NUMEROTATION GLOBALE (2D)
!>    </td></tr>
!>          <tr><td>NBOR3
!></td><td><--</td><td>CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
!>                  ET LA NUMEROTATION GLOBALE (3D)
!>    </td></tr>
!>          <tr><td>NELBO3
!></td><td><--</td><td>ASSOCIE A CHAQUE FACE DE BORD L'ELEMENT 3D
!>                  AUQUEL ELLE APPARTIENT
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
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD 2D
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td><--</td><td>ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!>                  NUMEROTATION LOCALE 3D
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td><-></td><td>SURFACE DES TRIANGLES ETENDUE EN 3D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ELEB3D
     &(IKLE3 , NBOR , KP1BOR , NELBOR, IKLBOR, NULONE,
     & NELEM2, NPOIN2, NPLAN, NETAGE, NPTFR )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLBOR         |<--| TABLE DE CONNECTIVITE ELEMENTS DE BORD
C| IKLE2          |-->| CORRESPONDANCE LOCALE - GLOGALE EN 2D
C| IKLE3          |<--| CORRESPONDANCE LOCALE - GLOBALE EN 3D
C| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
C| NBOR           |-->| CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
C|                |   | ET LA NUMEROTATION GLOBALE (2D)
C| NBOR3          |<--| CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
C|                |   | ET LA NUMEROTATION GLOBALE (3D)
C| NELBO3         |<--| ASSOCIE A CHAQUE FACE DE BORD L'ELEMENT 3D
C|                |   | AUQUEL ELLE APPARTIENT
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
C| NULONE         |<--| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C| SURFAC         |<->| SURFACE DES TRIANGLES ETENDUE EN 3D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ELEB3D => ELEB3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM2, NPOIN2, NPLAN, NETAGE, NPTFR
      INTEGER, INTENT(INOUT) :: IKLE3(NELEM2,NETAGE,6)
      INTEGER, INTENT(INOUT) :: IKLBOR(NPTFR,NETAGE,4)
      INTEGER, INTENT(INOUT) :: NULONE(NPTFR,NETAGE,4)
      INTEGER, INTENT(INOUT) :: NELBOR(NPTFR*NETAGE), NBOR(NPTFR*NPLAN)
      INTEGER, INTENT(INOUT) :: KP1BOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IPOIN
      INTEGER IETAGE,IPTFR
!
!***********************************************************************
!
C CONNECTIVITY TABLES FOR BOUNDARY FACES --> IKLBOR , NBOR3 ,
C CORRESPONDENCE BETWEEN LOCAL BOUNDARY NUMBERS AND 3D LOCAL NUMBERS --> NULONE
C
C COMPUTES NELBO3
!
C     LATERAL BOUNDARIES
!
      DO IETAGE = 1,NETAGE
        DO IPTFR = 1,NPTFR
          IKLBOR(IPTFR,IETAGE,1) =        IPTFR  + (IETAGE-1)*NPTFR
          IKLBOR(IPTFR,IETAGE,2) = KP1BOR(IPTFR) + (IETAGE-1)*NPTFR
          IKLBOR(IPTFR,IETAGE,3) = IKLBOR(IPTFR,IETAGE,2) + NPTFR
          IKLBOR(IPTFR,IETAGE,4) = IKLBOR(IPTFR,IETAGE,1) + NPTFR
          IPOIN = NBOR(IPTFR)
          NBOR(IPTFR +(IETAGE-1)*NPTFR)=IPOIN+(IETAGE-1)*NPOIN2
          IELEM = NELBOR(IPTFR)
          IF(IELEM.GT.0) THEN
            NELBOR(IPTFR+(IETAGE-1)*NPTFR)=IELEM+(IETAGE-1)*NELEM2
            IF(IPOIN.EQ.IKLE3(IELEM,1,1)) THEN
              NULONE(IPTFR,IETAGE,1) = 1
              NULONE(IPTFR,IETAGE,2) = 2
              NULONE(IPTFR,IETAGE,3) = 5
              NULONE(IPTFR,IETAGE,4) = 4
            ELSEIF(IPOIN.EQ.IKLE3(IELEM,1,2)) THEN
              NULONE(IPTFR,IETAGE,1) = 2
              NULONE(IPTFR,IETAGE,2) = 3
              NULONE(IPTFR,IETAGE,3) = 6
              NULONE(IPTFR,IETAGE,4) = 5
            ELSEIF(IPOIN.EQ.IKLE3(IELEM,1,3)) THEN
              NULONE(IPTFR,IETAGE,1) = 3
              NULONE(IPTFR,IETAGE,2) = 1
              NULONE(IPTFR,IETAGE,3) = 4
              NULONE(IPTFR,IETAGE,4) = 6
            ELSE
              IF(LNG.EQ.1) WRITE(LU,101) IPOIN
              IF(LNG.EQ.2) WRITE(LU,102) IPOIN
              CALL PLANTE(1)
              STOP
            ENDIF
          ELSEIF(NCSIZE.GT.1) THEN
            NULONE(IPTFR,IETAGE,1) = 0
            NULONE(IPTFR,IETAGE,2) = 0
            NULONE(IPTFR,IETAGE,3) = 0
            NULONE(IPTFR,IETAGE,4) = 0
            NELBOR(IPTFR+(IETAGE-1)*NPTFR)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,101) IPOIN
            IF(LNG.EQ.2) WRITE(LU,102) IPOIN
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDDO
!
C     COMPLETES NBOR IN VIEW OF 2D VALUES
!
      DO IPTFR = 1,NPTFR
         NBOR(IPTFR +(NPLAN-1)*NPTFR) = NBOR(IPTFR) + NETAGE*NPOIN2
      END DO
!
!-----------------------------------------------------------------------
!
101   FORMAT(' ELEB3D : PROBLEME A LA CONSTRUCTION DE NULONE, IPOIN =',
     &  I6)
102   FORMAT(' ELEB3D: PROBLEM WHEN BUILDING NULONE, IPOIN =',I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C