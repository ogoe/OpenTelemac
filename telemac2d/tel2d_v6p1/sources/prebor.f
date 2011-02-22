C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE BOUNDARY CONDITIONS FOR TREATMENT BY THOMPSON.
!>     UBOR, VBOR, HBOR, TBOR ARE INITIALISED HERE WITH VALUES AT TIME N
!>     AFTER BORD, THESE ARRAYS THEREFORE CONTAIN EITHER THE VALUE AT
!>     TIME N OR THE IMPOSED VALUE.<br>
!><br>            STORES H IN A TEMPORARY ARRAY TO SAVE ITS VALUE AT
!>     THE BOUNDARY AT TIME N (MODIFIED IN BORD).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBLIQ, FINLIQ, H, HBOR, HN, KP1BOR, NBOR, NFRLIQ, NPOIN, NPTFR, NTRAC, T, TBOR, U, UBOR, V, VBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, DEP, IFRLIQ, ITRAC, K
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
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
!>      <td><center> 5.8                                       </center>
!> </td><td> 03/09/2007
!> </td><td> E DAVID (LHF) 04 76 33 42 36
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEBLIQ
!></td><td>--></td><td>TABLEAU D'INDICES DE DEBUT DE FRONTIERE LIQ.
!>    </td></tr>
!>          <tr><td>FINLIQ
!></td><td>--></td><td>TABLEAU D'INDICES DE FIN DE FRONTIERE LIQUI.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td><--</td><td>HAUTEUR IMPOSEE.
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR DE PROPAGATION (OPTION H-U)
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMERO DU POINT FRONTIERE SUIVANT
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>--></td><td>NOMBRE DE FRONTIERES LIQUIDES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TRACEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td><--</td><td>TRACEUR IMPOSE AU BORD
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREBOR
     &(HBOR,UBOR,VBOR,TBOR,U,V,H,HN,T,NBOR,KP1BOR,NPOIN,NPTFR,
     & NTRAC,DEBLIQ,FINLIQ,NFRLIQ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEBLIQ         |-->| TABLEAU D'INDICES DE DEBUT DE FRONTIERE LIQ.
C| FINLIQ         |-->| TABLEAU D'INDICES DE FIN DE FRONTIERE LIQUI.
C| H             |-->| HAUTEUR AU TEMPS N
C| HBOR           |<--| HAUTEUR IMPOSEE.
C| HN             |-->| HAUTEUR DE PROPAGATION (OPTION H-U)
C| KP1BOR         |-->| NUMERO DU POINT FRONTIERE SUIVANT
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NFRLIQ         |-->| NOMBRE DE FRONTIERES LIQUIDES
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NTRAC          |-->| NOMBRE DE TRACEURS
C| T             |-->| TRACEUR AU TEMPS N
C| TBOR           |<--| TRACEUR IMPOSE AU BORD
C| U,V            |-->| COMPOSANTES DE LA VITESSE AU TEMPS N
C| UBOR           |<--| VITESSE U IMPOSEE.
C| VBOR           |<--| VITESSE V IMPOSEE.
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
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,NFRLIQ,NTRAC
      INTEGER, INTENT(IN)             :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),KP1BOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFR),UBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBOR(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: HN(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,IFRLIQ,ITRAC
C
      DOUBLE PRECISION C
C
      LOGICAL DEP
C
C-----------------------------------------------------------------------
C
C  LOOP ON THE LIQUID BOUNDARIES
C
      IF(NFRLIQ.NE.0) THEN
C
      DO 10 IFRLIQ = 1 , NFRLIQ
C
        DEP = .FALSE.
        K = DEBLIQ(IFRLIQ)
11      CONTINUE
        UBOR(K)=U(NBOR(K))
        VBOR(K)=V(NBOR(K))
        HBOR(K)=H(NBOR(K))
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
            TBOR%ADR(ITRAC)%P%R(K)=T%ADR(ITRAC)%P%R(NBOR(K))
          ENDDO
        ENDIF
        IF(K.EQ.FINLIQ(IFRLIQ).AND.DEP) THEN
          GO TO 12
        ELSE
          DEP=.TRUE.
          K = KP1BOR(K,1)
          GO TO 11
        ENDIF
12      CONTINUE
C
10    CONTINUE
C
      ENDIF
C
      CALL OV( 'X=Y     ' , HN , H , H , C , NPOIN )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C