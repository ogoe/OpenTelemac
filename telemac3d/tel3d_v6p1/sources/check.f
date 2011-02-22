C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHECKS FOR COMMON ERRORS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLBOR, IKLE2, IKLE3, INFO, NBOR, NBOR3, NELBO3, NELBOR, NELEM2, NELEM3, NETAGE, NPOIN2, NPTFR, NPTFR3, NTRAC, NULONE
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
!>    </th><td> IEL, IERR, IETAGE, ILOC, IPTFR, N1, N2
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
!>      <td><center> 5.1                                       </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLBOR
!></td><td>--></td><td>TABLE DE CONNECTIVITE ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES ELEMENTS 3D
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES GLOBALES DES POINTS FRONTIERES 2D
!>    </td></tr>
!>          <tr><td>NBOR3
!></td><td>--></td><td>ADRESSES GLOBALES DES POINTS FRONTIERES 3D
!>    </td></tr>
!>          <tr><td>NELBO3
!></td><td>--></td><td>ASSOCIE A CHAQUE FACE DE BORD L'ELEMENT 3D
!>                  AUQUEL ELLE APPARTIENT
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO DE L'ELEMENT ADJACENT AU K IEME
!>                  SEGMENT DE BORD
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE 2D
!>    </td></tr>
!>          <tr><td>NPTFR3
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE 3D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!>                  NUMEROTATION LOCALE 3D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHECK
     &(IKLE2,NBOR,NELBOR,IKLBOR,IKLE3,NELBO3,NULONE,NBOR3,NELEM2,NPOIN2,
     & NPTFR,NETAGE,NELEM3,NPTFR3,NTRAC,INFO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLBOR         |-->| TABLE DE CONNECTIVITE ELEMENTS DE BORD
C| IKLE2          |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D
C| IKLE3          |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 3D
C| INFO           |---| 
C| NBOR           |-->| ADRESSES GLOBALES DES POINTS FRONTIERES 2D
C| NBOR3          |-->| ADRESSES GLOBALES DES POINTS FRONTIERES 3D
C| NELBO3         |-->| ASSOCIE A CHAQUE FACE DE BORD L'ELEMENT 3D
C|                |   | AUQUEL ELLE APPARTIENT
C| NELBOR         |-->| NUMERO DE L'ELEMENT ADJACENT AU K IEME
C|                |   | SEGMENT DE BORD
C| NELEM2         |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NELEM3         |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 3D
C| NETAGE         |-->| NOMBRE D'ETAGES
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE 2D
C| NPTFR3         |-->| NOMBRE DE POINTS FRONTIERE 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| NULONE         |-->| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN) :: NELEM2, NPOIN2, NPTFR, NETAGE, NELEM3
      INTEGER, INTENT(IN) :: NPTFR3, NTRAC
      INTEGER, INTENT(IN) :: NELBOR(NPTFR), NBOR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE2(NELEM2,3)
      INTEGER, INTENT(IN) :: IKLBOR(NPTFR,NETAGE,4),IKLE3(NELEM3,6)
      INTEGER, INTENT(IN) :: NELBO3(NPTFR,NETAGE)
      INTEGER, INTENT(IN) :: NULONE(NPTFR,NETAGE,4)
      INTEGER, INTENT(IN) :: NBOR3(NPTFR3)
      LOGICAL, INTENT(IN) :: INFO
!
!-----------------------------------------------------------------------
!
      INTEGER IERR,IEL,N1,N2,IPTFR,IETAGE,ILOC
!
!***********************************************************************
!
C INITIALISES
!
C FATAL ERROR COUNT:
      IERR  = 0
!
!-----------------------------------------------------------------------
C CHECKS ARRAY NELBOR
!
      IF (NCSIZE.LE.1) THEN
        DO IPTFR = 1,NPTFR
          IEL = NELBOR(IPTFR)
          N1  = NBOR(IPTFR)
          IF (N1.NE.IKLE2(IEL,1).AND.N1.NE.IKLE2(IEL,2).AND.
     &        N1.NE.IKLE2(IEL,3)) THEN
            IF (LNG.EQ.1) WRITE(LU,11) IEL,IPTFR
            IF (LNG.EQ.2) WRITE(LU,12) IEL,IPTFR
            IERR = IERR + 1
          ENDIF
        END DO
!
!-----------------------------------------------------------------------
C CHECKS ARRAYS IKLBOR,NELBO3,NULONE
!
        DO ILOC = 1,4
          DO IETAGE = 1,NETAGE
            DO IPTFR = 1,NPTFR
              N1=NBOR3(IKLBOR(IPTFR,IETAGE,ILOC))
              N2=IKLE3(NELBO3(IPTFR,IETAGE),NULONE(IPTFR,IETAGE,ILOC))
              IF (N1.NE.N2) THEN
                IF (LNG.EQ.1) WRITE(LU,51) IPTFR,IETAGE,ILOC,N1,N2
                IF (LNG.EQ.2) WRITE(LU,52) IPTFR,IETAGE,ILOC,N1,N2
                IERR = IERR + 1
              ENDIF
            END DO
          END DO
        END DO
      ENDIF
!
!-----------------------------------------------------------------------
!
C PRINTS OUT THE RESULTS
!
      IF(IERR.EQ.0) THEN
         IF (LNG.EQ.1 .AND. INFO) WRITE(LU,111)
         IF (LNG.EQ.2 .AND. INFO) WRITE(LU,112)
      ELSEIF(IERR.EQ.1) THEN
         IF (LNG.EQ.1) WRITE(LU,121)
         IF (LNG.EQ.2) WRITE(LU,122)
         CALL PLANTE(1)
         STOP
      ELSE
         IF (LNG.EQ.1) WRITE(LU,131) IERR
         IF (LNG.EQ.2) WRITE(LU,132) IERR
         CALL PLANTE(1)
         STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(' CHECK: L''ELEMENT',
     &   I6,' N''EST PAS ADJACENT AU POINT DE BORD',I5)
12    FORMAT(' CHECK: ELEMENT',
     &   I6,' IS NOT ADJACENT TO BOUNDARY NODE',I5)
51    FORMAT(' CHECK: ERREUR SUR LA STRUCTURE DE DONNEES',/,
     &       'IPTFR,IETAGE,ILOC,N1,N2 :',5I5)
52    FORMAT(' CHECK: ERROR ON DATA STRUCTURE',/,
     &       'IPTFR,IETAGE,ILOC,N1,N2 :',5I5)
111   FORMAT(' CHECK: AUCUNE ERREUR N''A ETE DETECTEE',////)
112   FORMAT(' CHECK: NO ERROR HAS BEEN DETECTED',////)
121   FORMAT(' CHECK: 1 ERREUR FATALE . ARRET DU PROGRAMME',////)
122   FORMAT(' CHECK: 1 FATALE ERROR . BREAK IN THE PROGRAM',////)
131   FORMAT(' CHECK: ',I4,' ERREURS FATALES . ARRET DU PROGRAMME',////)
132   FORMAT(' CHECK: ',I4,' FATALE ERRORS . BREAK IN THE PROGRAM',////)
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CHECK
C
C#######################################################################
C