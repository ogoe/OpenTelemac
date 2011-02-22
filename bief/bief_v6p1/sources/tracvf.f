C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TRACER FOR FINITE VOLUME SCHEME.
!>                TO COMPLETE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DDT, DT, F, FBOR, FLBORTRA, FN, FSCEXP, FXBOR, FXMAT, FXMATPAR, H, HN, IOPT2, IT, KDDL, KDIR, LIMTRA, MESH, MSK, OPTSOU, SMH, T1, T2, T4, T5, T6, T7, T8, TDT, UNSV2D, V2DPAR, YASMH
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TRACVF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> HVF(), PLANTE(), TVF()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVTRVF()

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
!> </td><td> 06/02/09
!> </td><td> C-T PHAM (LNHE) 01 30 87 85 93
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DDT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>VALEURS DU TRACEUR A L'ETAPE N+1.
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td>--></td><td>VALEURS DU TRACEUR SUR LE BORD.
!>    </td></tr>
!>          <tr><td>FLBORTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>VALEURS DU TRACEUR A L'ETAPE N.
!>    </td></tr>
!>          <tr><td>FSCEXP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FXBOR
!></td><td>--></td><td>MATRICE DES FLUX SUR LE BORD.
!>    </td></tr>
!>          <tr><td>FXMAT
!></td><td>--></td><td>MATRICE DE STOCKAGE DES FLUX.
!>    </td></tr>
!>          <tr><td>FXMATPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N.
!>    </td></tr>
!>          <tr><td>IOPT2
!></td><td>--></td><td>0 : UCONV RESPECTE LA CONTINUITE
!>                  1 : UCONV NE RESPECTE PAS LA CONTINUITE
!>    </td></tr>
!>          <tr><td>IT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAS
!></td><td>--></td><td>VECTEUR MASS ASSEMBLE LUMPE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>IF YES, MASKING OF DRY ELEMENTS
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SM
!></td><td>--></td><td>TERMES SOURCES.
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERME SOURCE DE L'EQUATION DE CONTINUITE.
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TDT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TRACVF
     &(F,FN,FSCEXP,H,HN,FXMAT,FXMATPAR,
     & V2DPAR,UNSV2D,DDT,FXBOR,FBOR,SMH,YASMH,T1,T2,T4,T5,T6,T7,T8,
     & MESH,LIMTRA,KDIR,KDDL,OPTSOU,IOPT2,FLBORTRA,MSK,IT,DT,TDT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DDT            |---| 
C| DT             |-->| PAS DE TEMPS.
C| F             |<--| VALEURS DU TRACEUR A L'ETAPE N+1.
C| FBOR           |-->| VALEURS DU TRACEUR SUR LE BORD.
C| FLBORTRA       |---| 
C| FN             |-->| VALEURS DU TRACEUR A L'ETAPE N.
C| FSCEXP         |---| 
C| FXBOR          |-->| MATRICE DES FLUX SUR LE BORD.
C| FXMAT          |-->| MATRICE DE STOCKAGE DES FLUX.
C| FXMATPAR       |---| 
C| H             |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
C| HN             |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N.
C| IOPT2          |-->| 0 : UCONV RESPECTE LA CONTINUITE
C|                |   | 1 : UCONV NE RESPECTE PAS LA CONTINUITE
C| IT             |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| LIMTRA         |---| 
C| MAS            |-->| VECTEUR MASS ASSEMBLE LUMPE.
C| MESH           |-->| STRUCTURE DE MAILLAGE.
C| MSK            |-->| IF YES, MASKING OF DRY ELEMENTS
C| OPTSOU         |---| 
C| SM             |-->| TERMES SOURCES.
C| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE.
C| T1             |---| 
C| T2             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| T7             |---| 
C| T8             |---| 
C| TDT            |---| 
C| UNSV2D         |---| 
C| V2DPAR         |---| 
C| YASMH          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_TRACVF => TRACVF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: KDIR,KDDL,OPTSOU,LIMTRA(*)
      INTEGER, INTENT(IN)           :: IOPT2,IT
      DOUBLE PRECISION, INTENT(IN)  :: DDT,DT,TDT
      TYPE(BIEF_OBJ), INTENT(INOUT) :: F,T1,T2,T4,T5,T6,T7,T8,FLBORTRA
      TYPE(BIEF_OBJ), INTENT(IN)    :: FN,H,HN,V2DPAR,SMH,FBOR,FSCEXP
      TYPE(BIEF_OBJ), INTENT(IN)    :: FXBOR,UNSV2D
      DOUBLE PRECISION, INTENT(IN)  :: FXMAT(*),FXMATPAR(*)
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      LOGICAL, INTENT(IN)           :: YASMH,MSK
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
      IF(IOPT2.EQ.0) THEN
C
C-----------------------------------------------------------------------
C
C     CASE WHERE THE ADVECTION FIELD SATISFIES THE CONTINUITY EQUATION
C     (THE DEPTH COULD BE COMPUTED BY INTERPOLATION IN TIME)
C
C     T4 WILL TAKE THE SUCCESSIVE VALUES OF F (INITIALISED IN CVTRVF)
C
      CALL TVF(F%R,FN%R,T4%R,T5%R,FXMAT,FXMATPAR,UNSV2D%R,DDT,
     &         FXBOR%R,T7%R,T8,FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &         MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &         MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &         MESH%NBOR%I,LIMTRA,KDIR,KDDL,
     &         OPTSOU,T5%R,IOPT2,FLBORTRA%R,DDT/DT,MESH,F)
C
C-----------------------------------------------------------------------
C
C     CASE WHERE THE ADVECTION FIELD DOES NOT SATISFY THE CONTINUITY EQUATION
C
      ELSEIF(IOPT2.EQ.1) THEN
C
C     T1 WILL TAKE THE SUCCESSIVE VALUES OF HN COMPUTED WITH CONTINUITY
C     T2 WILL TAKE THE SUCCESSIVE VALUES OF H COMPUTED WITH CONTINUITY
C     T4 WILL TAKE THE SUCCESSIVE VALUES OF F
C     T5 WILL TAKE THE SUCCESSIVE VALUES OF TRUE DEPTH
C
C     H2 DEPTH BY CONTINUITY EQUATION
C
      CALL HVF(T2%R,T1%R,FXMAT,UNSV2D%R,DDT,T7%R,SMH%R,
     &         YASMH,MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &         MESH%GLOSEG%I,MESH%GLOSEG%DIM1,MESH%NBOR%I,OPTSOU,
     &         T8,MESH,MSK)
C
      CALL TVF(F%R,FN%R,T4%R,T2%R,FXMAT,FXMATPAR,UNSV2D%R,DDT,
     &         FXBOR%R,T7%R,T8,FBOR%R,SMH%R,YASMH,FSCEXP%R,
     &         MESH%NSEG,MESH%NPOIN,MESH%NPTFR,
     &         MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &         MESH%NBOR%I,LIMTRA,KDIR,KDDL,
     &         OPTSOU,T5%R,IOPT2,FLBORTRA%R,DDT/DT,MESH,F)
C
C-----------------------------------------------------------------------
C
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TRACVF : OPTION INCONNUE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TRACVF: UNKNOWN OPTION'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C