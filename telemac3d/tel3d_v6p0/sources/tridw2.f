C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES AN AVERAGED VALUE OF H * WSTAR IN A WAY
!>                COMPATIBLE WITH THE PSI SCHEME.
!>  @code
!>      IN WSS (WHICH IS WSCONV) WE FIRST PUT (LOOP 1) :
!>
!>      STARTING FROM THE FIRST PLANE (BOTTOM) :
!>
!>      (H WSTAR) LEVEL 3/2 - (H WSTAR) LEVEL 1/2     (THE LATTER=0)
!>      (H WSTAR) LEVEL 5/2 - (H WSTAR) LEVEL 3/2
!>      ......
!>
!>      (H WSTAR) LEVEL NPLAN - 1/2   -  (H WSTAR) LEVEL NPLAN - 3/2
!>
!>      THE FOLLOWING IS NOT SOLVED
!>      (WOULD ONLY GIVE (H WSTAR) LEVEL NPLAN + 1/2 = 0
!>
!>      (H WSTAR) LEVEL NPLAN + 1/2   -  (H WSTAR) LEVEL NPLAN - 1/2
!>
!>
!>      THEN BY SUCCESSIVE SUMS WE GET IN WSS (LOOP 2):
!>
!>      (H WSTAR) LEVEL 3/2
!>      (H WSTAR) LEVEL 5/2
!>      ......
!>
!>      (H WSTAR) LEVEL NPLAN - 1/2
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference PHD OF J-M HERVOUET, EQUATION 5.58

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> WSS
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLUEXT FLUEXT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLUINT FLUINT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH2D MESH2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NETAGE NETAGE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NSCE NSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEM2D SEM2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SOURCES SOURCES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLU VOLU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLUN VOLUN@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IAD1, IAD2, IAD3, IETAGE, IS, SURDT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 04/12/09
!> </td><td> J-M HERVOUET  (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/11/2008
!> </td><td> JMH
!> </td><td> CALLS TO OV REPLACED BY LOOPS
!>          (INTEL COMPILER SAYS IT ALLOCATES TEMPORARY ARRAYS)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/10/2004
!> </td><td>
!> </td><td> DONE WITH MASS-LUMPING, SEE ALSO MT14PP FOR COMPATIBILITY
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
!>          <tr><td>AM1,AM2
!></td><td><-></td><td>MATRICES DE TRAVAIL
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>EPSW
!></td><td>--></td><td>PRECISION POUR LE CALCUL DE WS
!>    </td></tr>
!>          <tr><td>FLUEXT
!></td><td><--</td><td>FLUX EXTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>FLUINT
!></td><td><--</td><td>FLUX INTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DH
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>INFORMATIONS SUR LES SOLVEURS
!>    </td></tr>
!>          <tr><td>LIWBOF,L,S
!></td><td>--></td><td>TYPE DE CONDITIONS LIMITES POUR WS
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES GLOBALES DES POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NPLAN - 1
!>    </td></tr>
!>          <tr><td>NITW
!></td><td>--></td><td>NOMBRE D'ITERATIONS POUR LE CALCUL DE WS
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>PREW
!></td><td>--></td><td>PRECONDITIONNEMENT POUR LE CALCUL DE WS
!>    </td></tr>
!>          <tr><td>SM
!></td><td><-></td><td>SECOND MEMBRE POUR LA VITESSE VERTICALE
!>    </td></tr>
!>          <tr><td>SOLW
!></td><td>--></td><td>SOLVEUR POUR LE CALCUL DE WS
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>TBB
!></td><td>--></td><td>BLOC DE BLOCS DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 2D
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!>    </td></tr>
!>          <tr><td>WSBORF,L,S
!></td><td>--></td><td>CONDITIONS AUX LIMITES DIRICHLET POUR WS
!>    </td></tr>
!>          <tr><td>WSS
!></td><td><--</td><td>COMPOSANTE WSTAR DE LA VITESSE A N+1
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>--></td><td>PLUS PETITE VALEUR NON NULLE AUTORISEE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TRIDW2
     &(WSS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AM1,AM2        |<->| MATRICES DE TRAVAIL
C| DT             |-->| PAS DE TEMPS
C| EPSW           |-->| PRECISION POUR LE CALCUL DE WS
C| FLUEXT         |<--| FLUX EXTERIEUR PAR NOEUD
C| FLUINT         |<--| FLUX INTERIEUR PAR NOEUD
C| IELM2          |-->| TYPE DE DISCRETISATION 2DH
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| INFO           |-->| INFORMATIONS SUR LES SOLVEURS
C| LIWBOF,L,S     |-->| TYPE DE CONDITIONS LIMITES POUR WS
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NBOR           |-->| ADRESSES GLOBALES DES POINTS FRONTIERES.
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NETAGE         |-->| NPLAN - 1
C| NITW           |-->| NOMBRE D'ITERATIONS POUR LE CALCUL DE WS
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
C| PREW           |-->| PRECONDITIONNEMENT POUR LE CALCUL DE WS
C| SM             |<->| SECOND MEMBRE POUR LA VITESSE VERTICALE
C| SOLW           |-->| SOLVEUR POUR LE CALCUL DE WS
C| SVIDE          |-->| STRUCTURE VIDE
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C| TRAV2          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 2D
C| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
C| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
C| WSBORF,L,S     |-->| CONDITIONS AUX LIMITES DIRICHLET POUR WS
C| WSS            |<--| COMPOSANTE WSTAR DE LA VITESSE A N+1
C| ZERO           |-->| PLUS PETITE VALEUR NON NULLE AUTORISEE
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WSS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IETAGE, I,IAD1,IAD2,IAD3,IS
!
      DOUBLE PRECISION :: SURDT
!
!***********************************************************************
!
C   SOLVES THE LINEAR SYSTEM
!
!=======================================================================
!
      SURDT=1.D0/DT
!
!-----------------------------------------------------------------------
!
C     LOOP 1
!
C     WSCONV OF LAST LEVEL WILL BE 0 (CHECKED,IT WORKS !)
C     BECAUSE SUM ON THE VERTICAL=2D CONTINUITY EQUATION
C     HENCE LAST LEVEL NOT SOLVED, SO LOOP UP TO NETAGE
C     A CONSEQUENCE IS THAT RAIN AND EVAPORATION IS NOT SEEN HERE
!
      IAD1=0
      IAD3=0
      DO IETAGE = 1,NETAGE
!
         DO I=1,NPOIN2
           IAD1=IAD1+1
           SEM2D%ADR(1)%P%R(I) = FLUINT%R(IAD1)-FLUEXT%R(IAD1)
     &                          +SURDT*(VOLUN%R(IAD1)-VOLU%R(IAD1))
         ENDDO
!
C        PARALLELISM
!
         IF(NCSIZE.GT.1) CALL PARCOM(SEM2D%ADR(1)%P,2,MESH2D)
!
C        WITH SOURCES (DONE AFTER CALL PARCOM BECAUSE
C        CALL PARCOM ON SOURCES IS ALREADY DONE IN SOURCES_SINKS)
!
         IF(NSCE.GT.0) THEN
C          WITH SOURCES
           DO IS=1,NSCE
           DO I=1,NPOIN2
             IAD2=(IETAGE-1)*NPOIN2+I
             SEM2D%ADR(1)%P%R(I)
     &      =SEM2D%ADR(1)%P%R(I)+SOURCES%ADR(IS)%P%R(IAD2)
           ENDDO
           ENDDO
         ENDIF
!
C        SOLVES THE SYSTEM (JUST A DIVISION BY DIAGONAL)
!
         DO I=1,NPOIN2
           IAD3=IAD3+1
           WSS%R(IAD3)=SEM2D%ADR(1)%P%R(I)*UNSV2D%R(I)
         ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
C     LOOP 2
!
      IF(NETAGE.GT.1) THEN
        IAD1=0
        IAD2=NPOIN2
        DO IETAGE = 2,NETAGE
          DO I=1,NPOIN2
            IAD1=IAD1+1
            IAD2=IAD2+1
            WSS%R(IAD2)=WSS%R(IAD2)+WSS%R(IAD1)
          ENDDO
        ENDDO
      ENDIF
!
C  LAST LEVEL : WSCONV = 0 (JMH : NOT USEFUL, BECAUSE INITIALISED AT 0,
C                           AT THE BEGINNING OF TELEMAC3D.F
C                           HOWEVER WSCONV IS MODIFIED AFTER BECAUSE WSTAR
C                           IS COPIED INTO IT, BUT IN FACT SET TO 0. ALSO)
!
C     CALL OV ('X=C     ',WSS%R((NPLAN-1)*NPOIN2+1:NPLAN*NPOIN2),
C    &                    WSS%R((NPLAN-1)*NPOIN2+1:NPLAN*NPOIN2),
C    &                    WSS%R((NPLAN-1)*NPOIN2+1:NPLAN*NPOIN2),
C    &                    0.D0, NPOIN2 )
!
!=======================================================================
!
C DIRICHLET CONDITIONS ON LATERAL BOUNDARIES
C     WSS = WSBORL
!
!=======================================================================
C FORTRAN77:
!
C THE WAY IN WHICH THE EQUATIONS ARE TREATED DOES NOT ALLOW TAKING INTO
C ACCOUNT SUCH CONDITIONS (OR MASS ERRORS)
!
C     DO 60 IPTFR = 1,NPTFR
C        IPOIN2 = NBOR(IPTFR)
C        DO 70 IETAGE = 1,NETAGE
C           C = 0.D0
C           IF (LIWBOL(IPTFR,IETAGE).EQ.KENT.OR.
C    *          LIWBOL(IPTFR,IETAGE).EQ.KADH)
C    *          C = 0.5D0*(WSBORL(IPTFR,IETAGE  )-WS(IPOIN2,IETAGE))
C           IF (LIWBOL(IPTFR,IETAGE).EQ.KENT.OR.
C    *          LIWBOL(IPTFR,IETAGE).EQ.KADH)
C    *          C = 0.5D0*(WSBORL(IPTFR,IETAGE+1)-WS(IPOIN2,IETAGE)) + C
C           WS(IPOIN2,IETAGE) = WS(IPOIN2,IETAGE) + C
C70      CONTINUE
C60   CONTINUE
!
C     PRINT*,'WSS=',DOTS(WSS,WSS)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C