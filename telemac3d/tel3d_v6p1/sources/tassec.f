C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MULTI-LAYER MODEL FOR CONSOLIDATION OF THE MUDDY BED.
!>  @code
!>      DETERMINES IF THE QUANTITY OF MUD IN A GIVEN LAYER HAS
!>      BEEN THERE FOR A LONG ENOUGH TIME (I.E TEMP>TREST) TO
!>      BE MOVED TO THE (MORE CONSOLIDATED) NEXT LAYER DOWN
!>      IF THAT IS THE CASE, THE THICKNESS OF THE LATTER (EPAI)
!>      INCREASES.
!>      STARTS WITH THE LAYER BEFORE LAST (THE LAST ONE HAVING
!>      AN INFINITE RESIDENCE TIME), AND ENDS WITH THE LAYER
!>      CLOSEST TO THE BED SURFACE.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CONC, DTC, EPAI, NCOUCH, NPOIN2, TEMP, TREST
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IC, IPOIN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FONVAS()

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
!> </td><td> 04/05/92
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
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
!>          <tr><td>CONC
!></td><td>--></td><td>CONCENTRATION DES COUCHES DU FOND VASEUX
!>    </td></tr>
!>          <tr><td>DTC
!></td><td>--></td><td>PAS DE TEMPS DU TASSEMENT
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>TAILLE DES MAILLES DU FOND
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>TEMP
!></td><td><-></td><td>COMPTEUR DE TEMPS DANS LE CAS D'UN
!>                  MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TREST
!></td><td>--></td><td>TEMPS DE RESIDENCE DE LA VASE DANS LES COUCHES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE TASSEC
     &( CONC   , EPAI , TREST , TEMP , DTC ,
     &  NPOIN2 , NCOUCH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CONC           |-->| CONCENTRATION DES COUCHES DU FOND VASEUX
C| DTC            |-->| PAS DE TEMPS DU TASSEMENT
C| EPAI           |-->| TAILLE DES MAILLES DU FOND
C| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| TEMP           |<->| COMPTEUR DE TEMPS DANS LE CAS D'UN
C|                |   | MODELE DE TASSEMENT MULTICOUCHES
C| TREST          |-->| TEMPS DE RESIDENCE DE LA VASE DANS LES COUCHES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN2, NCOUCH
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH), TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: DTC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN, IC
!
!======================================================================
!
       DO IPOIN=1,NPOIN2
         DO IC=2,NCOUCH
           IF (EPAI(IC,IPOIN).GT.0.D0)
     &       TEMP(IC,IPOIN) = TEMP(IC,IPOIN) +DTC
           IF (TEMP(IC,IPOIN).GE.TREST(IC)) THEN
             EPAI(IC-1,IPOIN) = EPAI(IC-1,IPOIN) +
     &                EPAI(IC,IPOIN)*CONC(IC)/CONC(IC-1)
             EPAI(IC,IPOIN)=0.D0
             TEMP(IC,IPOIN)=0.D0
          ENDIF
         END DO
       END DO
!
       RETURN
       END SUBROUTINE TASSEC

C
C#######################################################################
C