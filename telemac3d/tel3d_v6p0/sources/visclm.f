C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES VISCOSITIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DAMPING, DELTAR, DNUTAV, DNUVIV, GRAV, HN, IELM3, IND_T, KARMAN, MASKEL, MESH3D, MIXING, MSK, NPLAN, NPOIN2, NPOIN3, NTRAC, PRANDTL, RI, SVIDE, TA, TRAV1, TRAV2, TRAV3, TRAV4, TRAV5, TRAV6, TRAV7, U, UETCAR, V, VISCTA, VISCVI, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ITRAC, SDELTAZ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VISCLM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), DRIALG(), DRICV(), DRIUTI(), LONGMB(), LONGML(), OS(), PLANTE()
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
!>      <td><center> 5.8                                       </center>
!> </td><td> **/01/01
!> </td><td> AG (LNHE)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/11/97
!> </td><td> A MALCHEREK (HANOVRE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DAMPING
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DELTAR
!></td><td>--></td><td>(RHO-RHO0)/RHO0
!>    </td></tr>
!>          <tr><td>DNUTAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>IND_T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>--></td><td>MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>MIXING
!></td><td>--></td><td>MODELE DE LONGUEUR DE MELANGE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>PRANDTL
!></td><td>--></td><td>NOMBRE DE PRANDTL
!>    </td></tr>
!>          <tr><td>RI
!></td><td><--</td><td>NOMBRE DE RICHARDSON
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>CONCENTRATION DES TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>TRAV1,
!></td><td>--></td><td>TABLEAU DE TRAVAIL (PAR POINT)
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES HORIZONTALES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>UETCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td><--</td><td>VISCOSITE DYNAMIQUE DES TRACEURS
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td><--</td><td>VISCOSITE DYNAMIQUE DE LA VITESSE
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VISCLM
     & (VISCVI,VISCTA,RI,U,V,DELTAR,X,Y,Z,HN,TRAV1,TRAV2,TRAV3,
     &  TRAV4,TRAV5,TRAV6,TRAV7,SVIDE,MESH3D,IELM3,GRAV,
     &  NPLAN,NPOIN3,NPOIN2,NTRAC,MSK,MASKEL,TA,MIXING,
     &  DAMPING,IND_T,DNUVIV,DNUTAV,KARMAN,PRANDTL,UETCAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DAMPING        |---| 
C| DELTAR         |-->| (RHO-RHO0)/RHO0
C| DNUTAV         |---| 
C| DNUVIV         |---| 
C| GRAV           |-->| GRAVITE
C| HN             |-->| HAUTEUR D'EAU
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IND_T          |---| 
C| KARMAN         |---| 
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH3D         |-->| MAILLAGE 3D
C| MIXING         |-->| MODELE DE LONGUEUR DE MELANGE
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| PRANDTL        |-->| NOMBRE DE PRANDTL
C| RI             |<--| NOMBRE DE RICHARDSON
C| SVIDE          |-->| STRUCTURE VIDE
C| TA             |-->| CONCENTRATION DES TRACEURS ACTIFS
C| TRAV1,         |-->| TABLEAU DE TRAVAIL (PAR POINT)
C| TRAV2          |---| 
C| TRAV3          |---| 
C| TRAV4          |---| 
C| TRAV5          |---| 
C| TRAV6          |---| 
C| TRAV7          |---| 
C| U,V            |-->| COMPOSANTES HORIZONTALES DE LA VITESSE
C| UETCAR         |---| 
C| VISCTA         |<--| VISCOSITE DYNAMIQUE DES TRACEURS
C| VISCVI         |<--| VISCOSITE DYNAMIQUE DE LA VITESSE
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISCLM => VISCLM
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN3, NPOIN2, NPLAN
      INTEGER, INTENT(IN)            :: NTRAC,DAMPING
      INTEGER, INTENT(IN)            :: IELM3, MIXING,IND_T
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,DNUVIV,DNUTAV,KARMAN
      DOUBLE PRECISION, INTENT(IN)   :: PRANDTL
      LOGICAL, INTENT(IN)            :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(IN)    :: TA,UETCAR
      TYPE (BIEF_OBJ), INTENT(INOUT) :: RI
      TYPE (BIEF_OBJ), INTENT(IN)    :: U, V, DELTAR, X, Y, Z, HN
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3, TRAV4
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV5, TRAV6, TRAV7
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SVIDE
      TYPE (BIEF_MESH)               :: MESH3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC,I
      DOUBLE PRECISION SDELTAZ
!
!***********************************************************************
!
C     DISCRETISES VISCOSITY ON THE VERTICAL
C     THEY ARE DECLARED AS P1 TRIANGLES WITH SECOND DIMENSION NPLAN-1
C     TO HAVE A P0 DISCRETISATION ON THE VERTICAL AND LINEAR IN THE PLANE
!
C
C     THIS IS DONE WITH COMPONENT DIMDISC AT 4111 IN MT02PP
!
      VISCVI%ADR(3)%P%DIMDISC=4111
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          VISCTA%ADR(ITRAC)%P%ADR(3)%P%DIMDISC=4111
        ENDDO
      ENDIF
!
      CALL CPSTVC(DELTAR,TRAV1)
      CALL CPSTVC(DELTAR,TRAV2)
      CALL CPSTVC(DELTAR,TRAV3)
      CALL CPSTVC(DELTAR,TRAV4)
      CALL CPSTVC(DELTAR,TRAV5)
!
C     U, V AND DELTAR DERIVATIVES WRT Z: IN TRAV1, TRAV2 AND TRAV3
!
      DO I=1,NPOIN3-NPOIN2
        SDELTAZ=1.D0/MAX(Z%R(I+NPOIN2)-Z%R(I),1.D-4)
        TRAV1%R(I)=(     U%R(I+NPOIN2)-     U%R(I))*SDELTAZ
        TRAV2%R(I)=(     V%R(I+NPOIN2)-     V%R(I))*SDELTAZ
        TRAV3%R(I)=(DELTAR%R(I+NPOIN2)-DELTAR%R(I))*SDELTAZ
      ENDDO
!
C     SURFACE VALUE SET TO 0, IT IS ACTUALLY NOT USED
C     EXCEPT IN LONGML OR LONGMB (FOR NOTHING)
!
      DO I=NPOIN3-NPOIN2+1,NPOIN3
        TRAV1%R(I)=0.D0
        TRAV2%R(I)=0.D0
        TRAV3%R(I)=0.D0
        TRAV4%R(I)=0.D0
        RI%R(I)=0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
C     COMPUTES THE RICHARDSON NUMBER
!
      DO I=1,NPOIN3-NPOIN2
        TRAV4%R(I)=TRAV1%R(I)**2+TRAV2%R(I)**2
        RI%R(I)=-GRAV*TRAV3%R(I)/MAX(TRAV4%R(I),1.D-10)
        TRAV1%R(I)=SQRT(TRAV4%R(I))
      ENDDO
!
!-----------------------------------------------------------------------
!
C     SQUARE OF THE MIXING LENGTH : IN TRAV2
!
      IF(MIXING.EQ.1.OR.MIXING.EQ.3.OR.MIXING.EQ.5.OR.MIXING.EQ.6) THEN
        CALL LONGML(TRAV2%R,Z%R,HN%R,NPOIN3,NPOIN2,NPLAN,MIXING,KARMAN)
      ELSEIF(MIXING.EQ.4) THEN
!
        IF(IND_T.GT.0) THEN
          CALL LONGMB(TRAV2%R,Z%R,HN%R,NPOIN3,NPOIN2,NPLAN,
     &                U%R,V%R,X%R,Y%R,TRAV5%R,TRAV6%R,
     &                TRAV7%R,NTRAC,TA%ADR(IND_T)%P%R,KARMAN)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'TEMPERATURE NECESSAIRE POUR LE MODELE DE JET'
            WRITE(LU,*) 'RENSEIGNER LE MOT-CLE : NOMS DES TRACEURS'
            WRITE(LU,*) 'POUR DIRE LEQUEL EST LA TEMPERATURE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'TEMPERATURE IS NECESSARY FOR THE JET MODEL'
            WRITE(LU,*) 'USE THE KEY-WORD: NAMES OF TRACERS'
            WRITE(LU,*) 'TO SAY WHICH TRACER IS THE TEMPERATURE'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'MODELE INCONNU DANS VISCLM : ',MIXING
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN MODEL IN VISCLM : ',MIXING
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
C     AVERAGE OF THE MIXING LENGTH SQUARED, BY LAYER
!
      DO I=1,NPOIN3-NPOIN2
C       AVERAGE OF LM**2 (WHAT WAS DONE BEFORE)
        TRAV2%R(I)=(TRAV2%R(I)+TRAV2%R(I+NPOIN2))*0.5D0
C      (AVERAGE OF LM)**2
C       TRAV2%R(I)=((SQRT(TRAV2%R(I))+SQRT(TRAV2%R(I+NPOIN2)))*0.5D0)**2
C       MAGIC : AVERAGE OF THE OTHER TWO
C       TRAV2%R(I)=(5.D0*TRAV2%R(I)+3.D0*TRAV2%R(I+NPOIN2))/8.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
C     TRADITIONAL VISCOSITY WITHOUT DAMPING FUNCTION : TRAV4
!
      CALL OS('X=YZ    ',X=TRAV4,Y=TRAV1,Z=TRAV2)
!
C     TEST : AT THE BOTTOM : THEORETICAL VISCOSITY TAKEN AT DELTAZ/2
C                            I.E. THE AVERAGE OF LAYER1
!
C     DO I=1,NPOIN2
C       TRAV4%R(I)=KARMAN*(Z%R(I+NPOIN2)-Z%R(I))*SQRT(UETCAR%R(I))*0.5D0
C     ENDDO
!
!-----------------------------------------------------------------------
!
C DAMPING FUNCTION FOR
C THE VERTICAL TURBULENT VISCOSITY OF THE VELOCITIES : TRAV5
!
      IF(NTRAC.GT.0) THEN
!
      IF(DAMPING.EQ.1) THEN
C       USER DEFINED
        CALL DRIUTI(TRAV5%R,RI%R,1,0,NPOIN3)
      ELSEIF(DAMPING.EQ.2) THEN
C       ALGEBRAIC MODEL
        CALL DRIALG(TRAV5%R,TRAV3%R,RI%R,NPOIN3)
      ELSEIF(DAMPING.EQ.3) THEN
C       MUNK AND ANDERSON
        CALL DRICV(TRAV5%R,TRAV3%R,RI%R,NPOIN3)
      ELSEIF(DAMPING.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FCT. D''AMORTISSEMENT INCONNUE DANS VISCLM : ',
     &                DAMPING
        ENDIF
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN DUMPING FUNCTION IN VISCLM: ',
     &                DAMPING
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C COMPUTES VISCVI(1,3): VERTICAL VISCOSITY OF THE VELOCITIES
!
!-----------------------------------------------------------------------
!
      IF(DAMPING.EQ.0.OR.NTRAC.EQ.0) THEN
C       WITHOUT DAMPING FUNCTION
        CALL OS('X=Y     ',X=VISCVI%ADR(3)%P,Y=TRAV4)
      ELSE
C       WITH DAMPING FUNCTION
        CALL OS('X=YZ    ',X=VISCVI%ADR(3)%P,Y=TRAV5,Z=TRAV4)
      ENDIF
!
C     ADDS THE MOLECULAR VISCOSITY
!
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
C COMPUTES VISCTA(1,3,ITRAC): VERTICAL VISCOSITY OF THE TRACERS
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
          IF(DAMPING.EQ.1) THEN
C           USER DEFINED
            CALL DRIUTI(TRAV3%R,RI%R,2,ITRAC,NPOIN3)
          ELSEIF(DAMPING.EQ.2) THEN
C           VIOLLET (SEE CALL DRIALG ABOVE)
!
          ELSEIF(DAMPING.EQ.3) THEN
C           CV (SEE CALL DRICV ABOVE)
!
          ENDIF
!
          IF(DAMPING.EQ.0) THEN
            CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,Y=TRAV4)
          ELSE
            CALL OS('X=YZ    ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         Y=TRAV3,Z=TRAV4)
          ENDIF
!
C         DIVIDES BY THE PRANDTL NUMBER
!
          IF(ABS(PRANDTL-1.D0).GT.1.D-4) THEN
            CALL OS('X=CX    ',VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         C=1.D0/PRANDTL)
          ENDIF
!
C         ADDS THE MOLECULAR VISCOSITY
!
          CALL OS('X=X+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,C=DNUTAV)
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C