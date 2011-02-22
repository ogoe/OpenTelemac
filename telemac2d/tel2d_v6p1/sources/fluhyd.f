C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES AT TIME N.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRE, AIRS, AIRST, BETA, CE, CFLWTD, CMI, CVIS, DJX, DJY, DPX, DPY, DSZ, DT, DTHAUT, DX, DY, FLUENT, FLUHBTEMP, FLUSORT, FLUXTEMP, G, HBOR, HC, IVIS, JMI, KDDL, KDIR, KNEU, LIMPRO, NBOR, NORDRE, NPTFR, NS, NSEG, NT, NTRAC, NU, NUBO, UA, UBOR, VBOR, VNOIN, X, XNEBOR, Y, YNEBOR, ZF
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IS, IVAR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUHYD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CDL(), FLUCIN(), GRADNOD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRE
!></td><td>--></td><td>AIRES DES TRIANGLES
!>    </td></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES
!>    </td></tr>
!>          <tr><td>AIRST
!></td><td>--></td><td>AIRES DES SOUS-TRIANGLES DANS CELLULES
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>COEFFICIENT EXTRAPOLATION POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>CE
!></td><td><--</td><td>FLUX   +  TERMES DIFFUSION
!>    </td></tr>
!>          <tr><td>CFLWTD
!></td><td>--></td><td>NOMBRE DE CFL
!>    </td></tr>
!>          <tr><td>CMI
!></td><td>--></td><td>COORDONNEES DES POINTS MILIEUX D'INTERFACE
!>    </td></tr>
!>          <tr><td>CVIS
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION DES VITESSES
!>    </td></tr>
!>          <tr><td>DJX,DJY
!></td><td>---</td><td>GRADIENTS PAR TRIANGLES
!>    </td></tr>
!>          <tr><td>DPX, DPY
!></td><td>--></td><td>GRADIENTS DES FONCTIONS DE BASE
!>    </td></tr>
!>          <tr><td>DSZ
!></td><td>--></td><td>VARIATIONS DE Z POUR ORDRE 2
!>    </td></tr>
!>          <tr><td>DT
!></td><td><-></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>DTHAUT
!></td><td>--></td><td>UTILISE POUR CONDITION CFL
!>    </td></tr>
!>          <tr><td>DX,DY
!></td><td>---</td><td>GRADIENTS PAR NOEUDS
!>    </td></tr>
!>          <tr><td>FLUENT,FLUSORT
!></td><td><--</td><td>FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
!>    </td></tr>
!>          <tr><td>FLUHBTEMP
!></td><td><--</td><td>FLUX BORD POUR TRACEUR
!>    </td></tr>
!>          <tr><td>FLUXTEMP
!></td><td><--</td><td>FLUX POUR TRACEUR
!>    </td></tr>
!>          <tr><td>G
!></td><td>--></td><td>CONSTANTE DE GRAVITE
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE H
!>    </td></tr>
!>          <tr><td>HC
!></td><td><--</td><td>H RECONSTRUIT ORDRE 2   CORRIGE
!>    </td></tr>
!>          <tr><td>IVIS
!></td><td>--></td><td>OPTION DIFFUSION DES VITESSES
!>    </td></tr>
!>          <tr><td>JMI
!></td><td>--></td><td>NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
!>                  POINT MILIEU DE L'INTERFACE
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>CONVENTION POUR LES POINTS LIBRES
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES POINTS DIRICHLET
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>CONVENTION POUR LES POINTS NEUMANN
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NORDRE
!></td><td>--></td><td>ORDRE DU SCHEMA
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE D'ARETES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NT
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NU
!></td><td>--></td><td>NUMEROS DES NOEUDS PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>--></td><td>NUMEROS GLOBAUX DES EXTREMITES DES ARETES
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
!>    </td></tr>
!>          <tr><td>UA
!></td><td>--></td><td>UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE U
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td>--></td><td>VALEURS IMPOSEES DE V
!>    </td></tr>
!>          <tr><td>VNOIN
!></td><td>--></td><td>NORMALE A L'INTERFACE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>NORMALE AUX POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTES DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUHYD
     &(NS,NT,NSEG,NPTFR,NUBO,G,DT,X,Y,AIRS,NU,AIRE,
     & UA,ZF,VNOIN,CE,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     & KDDL,HBOR,UBOR,VBOR,FLUENT,FLUSORT,NORDRE,CMI,JMI,
     & DJX,DJY,DX,DY,DTHAUT,CFLWTD,FLBOR,
     & DPX,DPY,IVIS,CVIS,FLUHBTEMP,BETA,DSZ,AIRST,HC,FLUXTEMP,NTRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRE           |-->| AIRES DES TRIANGLES
C| AIRS           |-->| AIRES DES CELLULES
C| AIRST          |-->| AIRES DES SOUS-TRIANGLES DANS CELLULES
C| BETA           |---| COEFFICIENT EXTRAPOLATION POUR ORDRE 2
C| CE             |<--| FLUX   +  TERMES DIFFUSION
C| CFLWTD         |-->| NOMBRE DE CFL
C| CMI            |-->| COORDONNEES DES POINTS MILIEUX D'INTERFACE
C| CVIS           |-->| COEFFICIENT DE DIFFUSION DES VITESSES
C| DJX,DJY        |---| GRADIENTS PAR TRIANGLES
C| DPX, DPY       |-->| GRADIENTS DES FONCTIONS DE BASE
C| DSZ            |-->| VARIATIONS DE Z POUR ORDRE 2
C| DT             |<->| PAS DE TEMPS
C| DTHAUT         |-->| UTILISE POUR CONDITION CFL
C| DX,DY          |---| GRADIENTS PAR NOEUDS
C| FLUENT,FLUSORT |<--| FLUX MASSE ENTREE ET SORTIE DE TN A TN+1
C| FLUHBTEMP      |<--| FLUX BORD POUR TRACEUR
C| FLUXTEMP       |<--| FLUX POUR TRACEUR
C| G             |-->| CONSTANTE DE GRAVITE
C| HBOR           |-->| VALEURS IMPOSEES DE H
C| HC             |<--| H RECONSTRUIT ORDRE 2   CORRIGE
C| IVIS           |-->| OPTION DIFFUSION DES VITESSES
C| JMI            |-->| NUMERO DU TRIANGLE AUQUEL APPARTIENT LE
C|                |   | POINT MILIEU DE L'INTERFACE
C| KDDL           |-->| CONVENTION POUR LES POINTS LIBRES
C| KDIR           |-->| CONVENTION POUR LES POINTS DIRICHLET
C| KNEU           |-->| CONVENTION POUR LES POINTS NEUMANN
C| LIMPRO         |-->| TYPES DE CONDITIONS AUX LIMITES
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NORDRE         |-->| ORDRE DU SCHEMA
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NSEG           |-->| NOMBRE D'ARETES DU MAILLAGE
C| NT             |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NTRAC          |---| 
C| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| NUBO           |-->| NUMEROS GLOBAUX DES EXTREMITES DES ARETES
C| TRAC           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN TRACEUR
C| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
C| UBOR           |-->| VALEURS IMPOSEES DE U
C| VBOR           |-->| VALEURS IMPOSEES DE V
C| VNOIN          |-->| NORMALE A L'INTERFACE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
C| XNEBOR,YNEBOR  |-->| NORMALE AUX POINTS FRONTIERE
C| ZF             |-->| COTES DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FLUHYD => FLUHYD
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NS,NT,NSEG,NPTFR,KDIR,KNEU,KDDL,NORDRE
      INTEGER, INTENT(IN) :: NBOR(NPTFR),LIMPRO(NPTFR,6),NU(NT,3)
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),JMI(*),IVIS,NTRAC
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(2*NPTFR),YNEBOR(2*NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: HBOR(NPTFR),G,CFLWTD,DTHAUT(*)
      DOUBLE PRECISION, INTENT(IN) :: UBOR(NPTFR),VBOR(NPTFR),CMI(2,*)
      DOUBLE PRECISION, INTENT(IN) :: AIRST(2,*),CVIS
      DOUBLE PRECISION, INTENT(IN) :: X(NS),Y(NS),AIRS(NS),AIRE(NT)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA,DT,HC(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(3,NS),FLUENT,FLUSORT
      DOUBLE PRECISION, INTENT(IN) :: UA(3,NS),ZF(NS),VNOIN(3,NSEG)
      DOUBLE PRECISION, INTENT(IN) :: DSZ(2,*),DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,*),DJY(3,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,*),DY(3,*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUXTEMP,FLUHBTEMP,FLBOR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IS,IVAR
C
C-----------------------------------------------------------------------
C
C     EXPLICIT RESOLUTION
C
      DO IS=1,NS
        DO IVAR=1,3
          CE(IVAR,IS) = 0.D0
        ENDDO
      ENDDO
C
C   COMPUTES GRADIENTS AT NODES AS WELL AS DIFFUSION TERMS
C
      IF(NORDRE.EQ.2.OR.IVIS.EQ.1) CALL GRADNOD(NS,NT,NU,AIRE,AIRS,
     &                        UA,DPX,DPY,DJX,DJY,DX,DY,IVIS,CVIS,CE,ZF)
C
      CALL FLUCIN(NS,NSEG,NUBO,G,X,Y,CFLWTD,DT,UA,ZF,VNOIN,CE,NORDRE,
     &            CMI,JMI,DJX,DJY,DX,DY,BETA,DSZ,AIRS,
     &            AIRST,HC,FLUXTEMP,NPTFR,NBOR,XNEBOR,YNEBOR,NTRAC)
C
C        BOUNDARY CONDITIONS TREATMENT
C
      CALL CDL(NS,NPTFR,NBOR,LIMPRO,XNEBOR,YNEBOR,KDIR,KNEU,
     &         G,HBOR,UBOR,VBOR,UA,CE,FLUENT,FLUSORT,FLBOR,
     &         DTHAUT,DT,CFLWTD,FLUHBTEMP,NTRAC)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
