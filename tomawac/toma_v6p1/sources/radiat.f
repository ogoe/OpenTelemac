C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE RADIATION STRESSES AND DRIVING FORCES
!>                FOR THE GENERATION OF WAVE-INDUCED CURRENTS.
!><br>           (SEE NOTES FOR METHODOLOGY)
!>  @code
!>  THE RESULT OF THIS COMPUTATION IS GIVEN AS :
!>       FI = - 1/D D( SIJ )/D( XJ )    UNIT : M/S**2
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   COMPUTATION ACCORDING TO THE "THEORICAL" FORMULATION, WITH
!>          COMPUTATION OF THE TERMS IN THE TENSOR OF THE RADIATION
!>          STRESSES, AND THEN THEIR GRADIENTS IN SPACE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CG1, CGSUC1, DEPTH1, DSXXDX, DSXYDX, DSXYDY, DSYYDY, FS, FX, FY, NPOIN2_DIM, SXX, SXY, SYY, XK1
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::COSTET COSTET@endlink, 
!> @link DECLARATIONS_TOMAWAC::DFREQ DFREQ@endlink, 
!> @link DECLARATIONS_TOMAWAC::FREQ FREQ@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRAVIT GRAVIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::IELM2 IELM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink, 
!> @link DECLARATIONS_TOMAWAC::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NF NF@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ROEAU ROEAU@endlink, 
!> @link DECLARATIONS_TOMAWAC::SINTET SINTET@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST1 ST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST2 ST2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST3 ST3@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST4 ST4@endlink, 
!> @link DECLARATIONS_TOMAWAC::SW1 SW1@endlink, 
!> @link DECLARATIONS_TOMAWAC::T1 T1@endlink, 
!> @link DECLARATIONS_TOMAWAC::T2 T2@endlink, 
!> @link DECLARATIONS_TOMAWAC::T3 T3@endlink, 
!> @link DECLARATIONS_TOMAWAC::T4 T4@endlink, 
!> @link DECLARATIONS_TOMAWAC::W1 W1@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, COCO, COEF, DEUPI, DTETAR, IP, JF, JP, OMEGA, ROGER, SICO, SISI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DUMP2D()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 13/12/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CG(
!></td><td>--></td><td>TABLEAU DES VITESSES DE GROUPE
!>    </td></tr>
!>          <tr><td>CG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CGSUC1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CGSURC(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2*NF)
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>TABLEAU DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS
!>    </td></tr>
!>          <tr><td>DEPTH1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEUPI
!></td><td>--></td><td>2.PI
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCE
!>    </td></tr>
!>          <tr><td>DSXXDX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DSXXDX(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>DSXYDX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DSXYDX(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>DSXYDY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DSXYDY(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>DSYYDY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DSYYDY(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>FS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FX(
!></td><td><--</td><td>COMPOSANTE EN X DE LA FORCE MOTRICE
!>    </td></tr>
!>          <tr><td>FY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FY(
!></td><td><--</td><td>COMPOSANTE EN Y DE LA FORCE MOTRICE
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>MESH,XMESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>NPOIN2_DIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>TABLEAU DES   SINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>SXX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SXX(
!></td><td><--</td><td>CONTRAINTE DE RADIATION SXX
!>    </td></tr>
!>          <tr><td>SXY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SXY(
!></td><td><--</td><td>CONTRAINTE DE RADIATION SXY
!>    </td></tr>
!>          <tr><td>SYY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SYY(
!></td><td><--</td><td>CONTRAINTE DE RADIATION SYY
!>    </td></tr>
!>          <tr><td>T1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
!>    </td></tr>
!>          <tr><td>T2(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
!>    </td></tr>
!>          <tr><td>T3(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
!>    </td></tr>
!>          <tr><td>T4(
!></td><td>---</td><td>TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
!>    </td></tr>
!>          <tr><td>W1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NELEM2*4)
!>    </td></tr>
!>          <tr><td>X(
!></td><td>--></td><td>ABSCISSES DES POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE
!>    </td></tr>
!>          <tr><td>XK1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y(
!></td><td>--></td><td>ORDONNEES DES POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE RADIAT
     &( FX    , FY    , SXX   , SXY   , SYY   , XK1   , FS    , CG1   ,
     &  DEPTH1, CGSUC1, DSXXDX, DSXYDX, DSXYDY, DSYYDY, NPOIN2_DIM    )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CG(            |-->| TABLEAU DES VITESSES DE GROUPE
C| CG1            |---| 
C| CGSUC1         |---| 
C| CGSURC(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2*NF)
C| COSTET(        |-->| TABLEAU DES COSINUS DES DIRECTIONS
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS
C| DEPTH1         |---| 
C| DEUPI          |-->| 2.PI
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCE
C| DSXXDX         |---| 
C| DSXXDX(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DSXYDX         |---| 
C| DSXYDX(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DSXYDY         |---| 
C| DSXYDY(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DSYYDY         |---| 
C| DSYYDY(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| FS             |---| 
C| FX             |---| 
C| FX(            |<--| COMPOSANTE EN X DE LA FORCE MOTRICE
C| FY             |---| 
C| FY(            |<--| COMPOSANTE EN Y DE LA FORCE MOTRICE
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| IELM2          |-->| TYPE D'ELEMENT
C| MESH,XMESH     |-->| STRUCTURE DE MAILLAGE
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE SPATIAL
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| NPOIN2_DIM     |---| 
C| ROEAU          |-->| MASSE VOLUMIQUE DE L'EAU
C| SINTET(        |-->| TABLEAU DES   SINUS DES DIRECTIONS
C| SXX            |---| 
C| SXX(           |<--| CONTRAINTE DE RADIATION SXX
C| SXY            |---| 
C| SXY(           |<--| CONTRAINTE DE RADIATION SXY
C| SYY            |---| 
C| SYY(           |<--| CONTRAINTE DE RADIATION SYY
C| T1(            |---| TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
C| T2(            |---| TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
C| T3(            |---| TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
C| T4(            |---| TABLEAU DE TRAVAIL STRUCTURE (NPOIN2)
C| W1(            |---| TABLEAU DE TRAVAIL (DIMENSION NELEM2*4)
C| X(             |-->| ABSCISSES DES POINTS DU MAILLAGE SPATIAL
C| XK(            |-->| TABLEAU DES NOMBRES D'ONDE
C| XK1            |---| 
C| Y(             |-->| ORDONNEES DES POINTS DU MAILLAGE SPATIAL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      DOUBLE PRECISION DEUPI
C
      INTEGER NPOIN2_DIM
      DOUBLE PRECISION FS(NPOIN2_DIM,NPLAN,NF),CG1(NPOIN2_DIM,NF)
      DOUBLE PRECISION DEPTH1(NPOIN2_DIM), CGSUC1(NPOIN2_DIM,NF)
      DOUBLE PRECISION XK1(NPOIN2_DIM,NF)
      DOUBLE PRECISION DSXXDX(NPOIN2_DIM),DSXYDX(NPOIN2_DIM),
     &                 FX(NPOIN2_DIM)
      DOUBLE PRECISION DSXYDY(NPOIN2_DIM),DSYYDY(NPOIN2_DIM),
     &                 FY(NPOIN2_DIM)
      DOUBLE PRECISION SXX(NPOIN2_DIM),SXY(NPOIN2_DIM),SYY(NPOIN2_DIM)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION COEF  , COCO  , SISI  , SICO  , OMEGA , ROGER
      DOUBLE PRECISION DTETAR, C
C
C
      DEUPI=6.283185307D0
      ROGER=ROEAU*GRAVIT
      DTETAR=DEUPI/DBLE(NPLAN)
C
      DO IP=1,NPOIN2
        SXX(IP) = 0.D0
        SXY(IP) = 0.D0
        SYY(IP) = 0.D0
      ENDDO
C
C.....COMPUTES THE WORKING ARRAY N = CG/C
C     """"""""""""""""""""""""""""""""""""""""""""
      DO JF = 1,NF
        OMEGA=DEUPI*FREQ(JF)
        DO IP=1,NPOIN2
          CGSUC1(IP,JF)=CG1(IP,JF)*XK1(IP,JF)/OMEGA
        ENDDO
      ENDDO
C
C.....COMPUTES THE RADIATION STRESSES INTEGRATED OVER THE SPECTRUM
C.....(SUMS UP THE DISCRETISED PART OF THE SPECTRUM)
C     """""""""""""""""""""""""""""""""""""""""""""""""
      DO JP=1,NPLAN
        COCO=COSTET(JP)*COSTET(JP)
        SICO=SINTET(JP)*COSTET(JP)
        SISI=SINTET(JP)*SINTET(JP)
        DO JF=1,NF
          COEF=GRAVIT*DFREQ(JF)*DTETAR
          DO IP=1,NPOIN2
            SXX(IP)=SXX(IP)
     &             +(CGSUC1(IP,JF)*(1.D0+SISI)-0.5D0)*FS(IP,JP,JF)*COEF
            SXY(IP)=SXY(IP)
     &             +(CGSUC1(IP,JF)*SICO)*FS(IP,JP,JF)*COEF
            SYY(IP)=SYY(IP)
     &             +(CGSUC1(IP,JF)*(1.D0+COCO)-0.5D0)*FS(IP,JP,JF)*COEF
          ENDDO
        ENDDO
      ENDDO
C
C.....COMPUTES THE GRADIENTS IN SPACE OF THE RADIATION STRESSES
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
C.....NO MASKING
      DO IP=1,NELEM2
        W1(IP)=1.D0
      ENDDO
C
C.....DERIVATIVE IN X
      CALL OV('X=Y     ',T4,SXX,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
C
      CALL OV('X=Y     ',T4,SXY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          X',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
C
      CALL VECTOR
     & (ST3,'=','GRADF          X',IELM2,1.D0,MESH%X,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
C
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      CALL OV('X=Y/Z   ',DSXXDX,T1,T3,C,NPOIN2)
      CALL OV('X=Y/Z   ',DSXYDX,T2,T3,C,NPOIN2)
C
C.....DERIVATIVE IN Y
      CALL OV('X=Y     ',T4,SYY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST1,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
C
      CALL OV('X=Y     ',T4,SXY,T3,C,NPOIN2)
      CALL VECTOR
     & (ST2,'=','GRADF          Y',IELM2,1.D0,ST4,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
C
      CALL VECTOR
     & (ST3,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     &  ST3,ST3,ST3,ST3,ST3,MESH,.FALSE.,SW1)
C
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      CALL OV('X=Y/Z   ',DSYYDY,T1,T3,C,NPOIN2)
      CALL OV('X=Y/Z   ',DSXYDY,T2,T3,C,NPOIN2)
C
C
C.....COMPUTES THE DRIVING FORCES FOR WAVE-INDUCED CURRENTS
C     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DO IP=1,NPOIN2
        FX(IP)= - (DSXXDX(IP)+DSXYDY(IP))/DEPTH1(IP)
        FY(IP)= - (DSXYDX(IP)+DSYYDY(IP))/DEPTH1(IP)
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C