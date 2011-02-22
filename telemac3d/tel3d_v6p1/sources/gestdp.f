C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MANAGES THE DEPOSITED QUANTITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFDEP, EPAI, EPAI0, HDEP, IVIDE, NPF, NPFMAX, NPOIN2, RHOS
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EDEPOT, EPAIX, ICOUCH, IPF, IPOIN, MANQUE, NAJOUT, NCOUCH
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIS(), FONVAS()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 18/07/06
!> </td><td> N. DURAND (CHC-NRC)
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> C LE NORMANT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><-></td><td>TAILLES DES MAILLES DU FOND EN
!>                  COORDONNEES MATERIELLES (EPAI=DZ/(1+E)
!>    </td></tr>
!>          <tr><td>EPAI0
!></td><td>--></td><td>EPAISSEUR DE REFERENCE POUR CREER UNE MAILLE
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><-></td><td>HAUTEUR DES DEPOTS FRAIS  (COUCHE TAMPON)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><-></td><td>INDICE DES VIDES AUX POINTS DU FOND
!>    </td></tr>
!>          <tr><td>NPF
!></td><td><-></td><td>NOMBRE DE POINTS DU FOND SUR UNE VERTICALE
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td><-></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE GESTDP
     &        ( IVIDE  , EPAI   , HDEP    ,
     &          NPOIN2 , NPFMAX , NPF     ,
     &          EPAI0  , CFDEP  , RHOS    )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
C| EPAI           |<->| TAILLES DES MAILLES DU FOND EN
C|                |   | COORDONNEES MATERIELLES (EPAI=DZ/(1+E)
C| EPAI0          |-->| EPAISSEUR DE REFERENCE POUR CREER UNE MAILLE
C| HDEP           |<->| HAUTEUR DES DEPOTS FRAIS  (COUCHE TAMPON)
C| IVIDE          |<->| INDICE DES VIDES AUX POINTS DU FOND
C| NPF            |<->| NOMBRE DE POINTS DU FOND SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX
C| NPOIN2         |<->| NOMBRE DE POINTS DU MAILLAGE 2D
C| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPOIN2 , NPFMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI0
      DOUBLE PRECISION, INTENT(IN)    :: CFDEP , RHOS
!
      INTEGER, INTENT(INOUT) :: NPF(NPOIN2)
!
      DOUBLE PRECISION EDEPOT
      INTEGER IPOIN , NCOUCH , NAJOUT
      INTEGER MANQUE , IPF , ICOUCH
C#####> NOE-CHANGES
      DOUBLE PRECISION EPAIX
C#####< NOE-CHANGES
      INTRINSIC MAX,INT
!
!=======================================================================
!
C     -----VOIDS INDEX FOR FRESH DEPOSITS-----
!
      EDEPOT=RHOS/CFDEP-1.D0
!
C     -----LOOP ON THE BOTTOM POINTS-----
!
        DO IPOIN=1,NPOIN2
!
C     -----IF DEPOSIT OCCURS ON NOT ERODABLE BOTTOM----
          IF(NPF(IPOIN).EQ.0) THEN
            IVIDE(1,IPOIN)=EDEPOT
            NPF(IPOIN)=1
          ENDIF
!
C     -----NUMBER OF THE LAST LAYER OF THE MUDDY BED-----
          NCOUCH=MAX(NPF(IPOIN)-1,0)
!
C     -----COMPUTES THE NUMBER OF LAYERS TO CREATE-----
!
C#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C EPAI0 SHOULD NOT BE CHANGED - PARTICULARLY, IT CANNOT VARY WITH IPOIN
C          EPAI0=EPAI0*(INT(HDEP(IPOIN)/((NPFMAX-1)*EPAI0))+1)
!
C IF YOU NEED TO HAVE THICKER LAYERS BECAUSE NPFMAX IS NOT LARGE ENOUGH
C  TO SPLIT HDEP, YOU OUGHT TO INTRODUCE EPAIX
          EPAIX=EPAI0*(INT(HDEP(IPOIN)/((NPFMAX-1)*EPAI0))+1)
!
C SEE ADDITIONAL CHANGES BELOW WHERE EPAI0 HAS BEEN REPLACED WITH EPAIX
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< NOE-CHANGES
          NAJOUT=INT(HDEP(IPOIN)/EPAIX)           !>NOE: EPAIX <= EPAI0
          MANQUE=NPF(IPOIN)-NPFMAX+NAJOUT         !>NOE: EPAIX <= EPAI0
          HDEP(IPOIN)=HDEP(IPOIN)-EPAIX*NAJOUT    !>NOE: EPAIX <= EPAI0
!
C      DEPENDING ON THE QUANTITY OF MUD DEPOSITS
          IF (MANQUE.LE.0) THEN
C      SIMPLY ADDS NEW LAYERS TO THE MUDDY BED
!
             DO ICOUCH=1+NCOUCH,NAJOUT+NCOUCH
C              --THEIR THICKNESS IS:              !>NOE: EPAIX <= EPAI0
                 EPAI(ICOUCH,IPOIN)=EPAIX/(1.+EDEPOT)
C              --THE VOIDS INDEX OF THE 'UPPER BOUNDARY' POINT IS:
                 IVIDE(ICOUCH+1,IPOIN)=EDEPOT
             END DO
C            ----THE NUMBER OF POINTS DISCRETISING THE BED IS:
                 NPF(IPOIN)=NCOUCH+1+NAJOUT
          ELSE
C      OR MODIFIES THE DISCRETISATION OF THE MUDDY BED BEFOREHAND
!
             DO IPF=2,MANQUE+1
C#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C EPAI BECOMES ARTIFICIALLY LARGE BY DOING SO
C HENCE EPAI IS NO LONGER REPRESENTATIVE OF THE 2 LAYERS
C               IVIDE(IPF,IPOIN)=IVIDE(2*IPF-1,IPOIN)
C               EPAI(IPF-1,IPOIN)=
C     &                    EPAI(2*(IPF-1)-1,IPOIN)+EPAI(2*(IPF-1),IPOIN)
               EPAI(IPF-1,IPOIN)=
     &        ( EPAI(2*(IPF-1)-1,IPOIN)*
     &           (2.+IVIDE(2*(IPF-1)-1,IPOIN)+IVIDE(2*(IPF-1),IPOIN))/2.
     &        + EPAI(2*(IPF-1),IPOIN)*
     &        (2.+IVIDE(2*(IPF-1),IPOIN)+IVIDE(2*(IPF-1)+1,IPOIN))/2.)/
     &       (1.+(IVIDE(2*(IPF-1)-1,IPOIN)+IVIDE(2*(IPF-1)+1,IPOIN))/2.)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< NOE-CHANGES
             ENDDO
             DO IPF=MANQUE+2,NPF(IPOIN)-MANQUE
                 IVIDE(IPF,IPOIN)=IVIDE(IPF+MANQUE,IPOIN)
                 EPAI(IPF-1,IPOIN)=EPAI(IPF-1+MANQUE,IPOIN)
             ENDDO
             DO IPF=NPF(IPOIN)-MANQUE+1,NPF(IPOIN)+NAJOUT-MANQUE
                 IVIDE(IPF,IPOIN)=EDEPOT          !>NOE: EPAIX
                 EPAI(IPF-1,IPOIN)=EPAIX/(1.+EDEPOT)
             ENDDO
!
C            ----NUMBER OF POINTS DISCRETISING THE BED:
                 NPF(IPOIN)=NPF(IPOIN)+NAJOUT-MANQUE
          ENDIF
!
C     IF THE DEPOSITED QUANTITY IS NOT SUFFICIENT TO
C     CREATE A NEW LAYER, STORES IT IN A BUFFER LAYER
C     AND WAITS THE NEXT ITERATION!
!
       ENDDO
!
      RETURN
      END
C
C#######################################################################
C