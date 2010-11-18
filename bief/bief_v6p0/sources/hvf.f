C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES AN INTERMEDIATE DEPTH IF THERE ARE
!>                SUB-ITERATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, FXBOR, FXMAT, GLOSEG, H, HN, MESH, MSK, NBOR, NPOIN, NPTFR, NSEG, OPTSOU, SIZGLO, SMH, T7, UNSV2D, YASMH
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
!>    </th><td> I, N
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_HVF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TRACVF()

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
!> </td><td> 09/02/2009
!> </td><td> CHI-TUAN PHAM (LNHE) 01 30 87 85 93
!> </td><td> JMH : SEQUENCE IF(MSK) : AVOIDS NEGATIVE DEPTHS
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>FXBOR
!></td><td>--></td><td>FLUX AU BORD (DEFINI SUR TOUT LE DOMAINE
!>                  ET ASSEMBLE EN PARALLELE)
!>    </td></tr>
!>          <tr><td>FXMAT
!></td><td>--></td><td>MATRICE DE STOCKAGE DES FLUX.
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!>    </td></tr>
!>          <tr><td>H
!></td><td><--</td><td>VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N.
!>    </td></tr>
!>          <tr><td>MAS
!></td><td>--></td><td>VECTEUR MASS ASSEMBLE LUMPE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>MSK : IF YES, MASKING OF DRY ELEMENTS
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>TABLEAU D'INDICES DE NOEUDS SUR LE BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE NOEUDS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE NOEUDS SUR LA FRONTIERE.
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE DE SEGMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>--></td><td>OPTION FOR THE TREATMENT OF SOURCES
!>                  1: NORMAL  2: DIRAC
!>                  SEE PROPAG IN TELEMAC-2D
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERME SOURCE DE L'EQUATION DE CONTINUITE.
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>IF YES, SMH MUST BE TAKEN INTO ACCOUNT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                            SUBROUTINE HVF
     &(H,HN,FXMAT,UNSV2D,DT,FXBOR,SMH,YASMH,NSEG,NPOIN,NPTFR,GLOSEG,
     & SIZGLO,NBOR,OPTSOU,T7,MESH,MSK)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS.
C| FXBOR          |-->| FLUX AU BORD (DEFINI SUR TOUT LE DOMAINE
C|                |   | ET ASSEMBLE EN PARALLELE)
C| FXMAT          |-->| MATRICE DE STOCKAGE DES FLUX.
C| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
C| H             |<--| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
C| HN             |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N.
C| MAS            |-->| VECTEUR MASS ASSEMBLE LUMPE.
C| MESH           |---| 
C| MSK            |-->| MSK : IF YES, MASKING OF DRY ELEMENTS
C| NBOR           |-->| TABLEAU D'INDICES DE NOEUDS SUR LE BORD.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NPOIN          |-->| NOMBRE DE NOEUDS DANS LE MAILLAGE.
C| NPTFR          |-->| NOMBRE DE NOEUDS SUR LA FRONTIERE.
C| NSEG           |-->| NOMBRE DE SEGMENTS DANS LE MAILLAGE.
C| OPTSOU         |-->| OPTION FOR THE TREATMENT OF SOURCES
C|                |   | 1: NORMAL  2: DIRAC
C|                |   | SEE PROPAG IN TELEMAC-2D
C| SIZGLO         |---| 
C| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE.
C| T7             |---| 
C| UNSV2D         |---| 
C| YASMH          |-->| IF YES, SMH MUST BE TAKEN INTO ACCOUNT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_HVF => HVF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,OPTSOU,SIZGLO
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG*2)
      LOGICAL, INTENT(IN)             :: YASMH,MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T7
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,N
C
C-----------------------------------------------------------------------
C
      DO I = 1,NPOIN
        H(I) = HN(I)
      ENDDO
C
C     SOURCES TERMS
C
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I = 1,NPOIN
            H(I) = H(I) + DT*SMH(I)
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I = 1,NPOIN
            H(I) = H(I) + DT*UNSV2D(I)*SMH(I)
          ENDDO
        ENDIF
      ENDIF
C
      IF(NCSIZE.GT.1) THEN
        DO I = 1,NPOIN
          T7%R(I) = 0.D0
        ENDDO
        DO I = 1,NSEG
          T7%R(GLOSEG(I,1))=T7%R(GLOSEG(I,1))
     &                     -DT*UNSV2D(GLOSEG(I,1))*FXMAT(I)
          T7%R(GLOSEG(I,2))=T7%R(GLOSEG(I,2))
     &                     +DT*UNSV2D(GLOSEG(I,2))*FXMAT(I)
        ENDDO
        CALL PARCOM(T7,2,MESH)
        DO I = 1,NPOIN
          H(I) = H(I) + T7%R(I)
        ENDDO
      ELSE
        DO I = 1,NSEG
          H(GLOSEG(I,1))=H(GLOSEG(I,1))-DT*UNSV2D(GLOSEG(I,1))*FXMAT(I)
          H(GLOSEG(I,2))=H(GLOSEG(I,2))+DT*UNSV2D(GLOSEG(I,2))*FXMAT(I)
        ENDDO
      ENDIF
C
C     ON THE BOUNDARIES : BOUNDARY FLUX TERMS
C
      DO I=1,NPTFR
        N=NBOR(I)
        H(N) = H(N) - DT*UNSV2D(N)*FXBOR(N)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     WHEN NEGATIVE DEPTHS APPEAR WHILE COMPUTING H, THE PREVIOUS
C     VALUE OF H IS KEPT
C
      IF(MSK) THEN
        DO I = 1,NPOIN
          IF(H(I).LT.0.D0) H(I) = MAX(1.D-2,HN(I))
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C