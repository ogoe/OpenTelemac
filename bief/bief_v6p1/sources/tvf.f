C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TRACER FOR FINITE VOLUME SCHEME.
!>                TO COMPLETE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, F, FBOR, FC, FLBORTRA, FN, FSCEXP, FXBOR, FXBORPAR, FXMAT, FXMATPAR, GLOSEG, H, HLIN, IOPT2, KDDL, KDIR, LIMTRA, MESH, NBOR, NPOIN, NPTFR, NSEG, OPTSOU, SF, SIZGLO, SMH, SURNIT, T7, UNSV2D, YASMH
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
!>    </th><td> EX_TVF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM(), PLANTE()
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
!> </td><td> 27/02/09
!> </td><td> C-T PHAM (LNHE) 01 30 87 85 93
!> </td><td> JMH : DISTINGUISHES BETWEEN FXBOR AND FXBORTRA
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
!>          <tr><td>F
!></td><td><--</td><td>VALEURS DU TRACEUR A L'ETAPE N+1
!>                  DE LA SOUS-ITERATION
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td>--></td><td>VALEURS DU TRACEUR SUR LE BORD.
!>    </td></tr>
!>          <tr><td>FC
!></td><td>--></td><td>VALEURS DU TRACEUR A L'ETAPE N
!>                  DE LA SOUS-ITERATION
!>    </td></tr>
!>          <tr><td>FLBORTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>VALEURS DU TRACEUR A L'ETAPE N
!>    </td></tr>
!>          <tr><td>FSCEXP
!></td><td>--></td><td>FSCE-(1-TETAT)*FN, SEE DIFSOU
!>                  SO HERE FSCE-FN, THIS IS NOT VERY CONVENIENT
!>                  AS WE NEED HERE FSCE-FC (LOOK UNDER IF(YASMH))
!>    </td></tr>
!>          <tr><td>FXBOR
!></td><td>--></td><td>FLUX SUR LE BORD (DEFINI SUR LE BORD)
!>                  NON ASSEMBLE
!>    </td></tr>
!>          <tr><td>FXBORPAR
!></td><td>--></td><td>FLUX SUR LE BORD (DEFINI SUR TOUT LE DOMAINE
!>                  ET ASSEMBLE EN PARALLELE)
!>    </td></tr>
!>          <tr><td>FXMAT
!></td><td>--></td><td>MATRICE DE STOCKAGE DES FLUX.
!>    </td></tr>
!>          <tr><td>FXMATPAR
!></td><td>--></td><td>IDEM, ASSEMBLE EN PARALLELE.
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!>                  EN SUPPOSANT LA CONTINUITE RESOLUE
!>    </td></tr>
!>          <tr><td>HLIN
!></td><td>--></td><td>VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
!>                  AVEC INTERPOLATION LINEAIRE EN TEMPS
!>                  ENTRE HN ET H
!>    </td></tr>
!>          <tr><td>IOPT2
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
!></td><td>---</td><td>
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
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SM
!></td><td>--></td><td>TERMES SOURCES.
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>TERME SOURCE DE L'EQUATION DE CONTINUITE.
!>    </td></tr>
!>          <tr><td>SURNIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>IF YES, SOURCE TERMS IN SMH
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TVF
     &(F,FN,FC,H,FXMAT,FXMATPAR,
     & UNSV2D,DT,FXBOR,FXBORPAR,T7,FBOR,SMH,YASMH,FSCEXP,
     & NSEG,NPOIN,NPTFR,GLOSEG,SIZGLO,NBOR,LIMTRA,KDIR,KDDL,OPTSOU,HLIN,
     & IOPT2,FLBORTRA,SURNIT,MESH,SF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS.
C| F             |<--| VALEURS DU TRACEUR A L'ETAPE N+1
C|                |   | DE LA SOUS-ITERATION
C| FBOR           |-->| VALEURS DU TRACEUR SUR LE BORD.
C| FC             |-->| VALEURS DU TRACEUR A L'ETAPE N
C|                |   | DE LA SOUS-ITERATION
C| FLBORTRA       |---| 
C| FN             |-->| VALEURS DU TRACEUR A L'ETAPE N
C| FSCEXP         |-->| FSCE-(1-TETAT)*FN, SEE DIFSOU
C|                |   | SO HERE FSCE-FN, THIS IS NOT VERY CONVENIENT
C|                |   | AS WE NEED HERE FSCE-FC (LOOK UNDER IF(YASMH))
C| FXBOR          |-->| FLUX SUR LE BORD (DEFINI SUR LE BORD)
C|                |   | NON ASSEMBLE
C| FXBORPAR       |-->| FLUX SUR LE BORD (DEFINI SUR TOUT LE DOMAINE
C|                |   | ET ASSEMBLE EN PARALLELE)
C| FXMAT          |-->| MATRICE DE STOCKAGE DES FLUX.
C| FXMATPAR       |-->| IDEM, ASSEMBLE EN PARALLELE.
C| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT.
C| H             |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
C|                |   | EN SUPPOSANT LA CONTINUITE RESOLUE
C| HLIN           |-->| VALEURS DE LA HAUTEUR D'EAU A L'ETAPE N+1.
C|                |   | AVEC INTERPOLATION LINEAIRE EN TEMPS
C|                |   | ENTRE HN ET H
C| IOPT2          |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| LIMTRA         |---| 
C| MAS            |-->| VECTEUR MASS ASSEMBLE LUMPE.
C| MESH           |---| 
C| NBOR           |-->| TABLEAU D'INDICES DE NOEUDS SUR LE BORD.
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NPOIN          |-->| NOMBRE DE NOEUDS DANS LE MAILLAGE.
C| NPTFR          |-->| NOMBRE DE NOEUDS SUR LA FRONTIERE.
C| NSEG           |-->| NOMBRE DE SEGMENTS DANS LE MAILLAGE.
C| OPTSOU         |---| 
C| SF             |---| 
C| SIZGLO         |---| 
C| SM             |-->| TERMES SOURCES.
C| SMH            |-->| TERME SOURCE DE L'EQUATION DE CONTINUITE.
C| SURNIT         |---| 
C| T7             |---| 
C| UNSV2D         |---| 
C| YASMH          |-->| IF YES, SOURCE TERMS IN SMH
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_TVF => TVF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: SIZGLO,OPTSOU,IOPT2
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN)             :: NBOR(NPTFR),LIMTRA(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: DT,SURNIT
      DOUBLE PRECISION, INTENT(INOUT) :: FLBORTRA(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: FC(NPOIN),H(NPOIN),HLIN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SMH(NPOIN),UNSV2D(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FSCEXP(NPOIN),FN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FBOR(NPTFR),FXBORPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T7,SF
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,N
C
C-----------------------------------------------------------------------
C
      IF(IOPT2.EQ.0) THEN
C       CONSERVATIVE ADVECTION FIELD
        DO I = 1,NPOIN
          F(I) = FC(I)
        ENDDO
      ELSEIF(IOPT2.EQ.1) THEN
C       NON CONSERVATIVE ADVECTION FIELD
        DO I = 1,NPOIN
          F(I) = FC(I)*MAX(H(I),1.D-8)/MAX(HLIN(I),1.D-8)
        ENDDO
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TVF : OPTION INCONNUE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TVF: UNKNOWN OPTION'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IF(NCSIZE.GT.1) THEN
C       THE CONTRIBUTION OF FLUXES IS BUILT APART FOR
C       PRELIMINARY PARALLEL ASSEMBLING BEFORE ADDING ON F
        DO I = 1,NPOIN
          T7%R(I) = 0.D0
        ENDDO
        DO I = 1,NSEG
          IF(FXMATPAR(I).LT.0.D0) THEN
            T7%R(GLOSEG(I,1)) = T7%R(GLOSEG(I,1))
     &      - DT/HLIN(GLOSEG(I,1))*UNSV2D(GLOSEG(I,1))
     &      *FXMAT(I)*(FC(GLOSEG(I,2))-FC(GLOSEG(I,1)))
          ELSEIF(FXMATPAR(I).GT.0.D0) THEN
            T7%R(GLOSEG(I,2)) = T7%R(GLOSEG(I,2))
     &      + DT/HLIN(GLOSEG(I,2))*UNSV2D(GLOSEG(I,2))
     &      *FXMAT(I)*(FC(GLOSEG(I,1))-FC(GLOSEG(I,2)))
          ENDIF
        ENDDO
        CALL PARCOM(T7,2,MESH)
        DO I = 1,NPOIN
          F(I) = F(I)+T7%R(I)
        ENDDO
      ELSE
        DO I = 1,NSEG
          IF(FXMATPAR(I).LT.0.D0) THEN
            F(GLOSEG(I,1)) = F(GLOSEG(I,1))
     &      - DT/HLIN(GLOSEG(I,1))*UNSV2D(GLOSEG(I,1))
     &      *FXMAT(I)*(FC(GLOSEG(I,2))-FC(GLOSEG(I,1)))
          ELSEIF(FXMATPAR(I).GT.0.D0) THEN
            F(GLOSEG(I,2)) = F(GLOSEG(I,2))
     &      + DT/HLIN(GLOSEG(I,2))*UNSV2D(GLOSEG(I,2))
     &      *FXMAT(I)*(FC(GLOSEG(I,1))-FC(GLOSEG(I,2)))
          ENDIF
        ENDDO
      ENDIF
C
C     SOURCE TERMS
C
      IF(YASMH) THEN
        IF(OPTSOU.EQ.1) THEN
          DO I=1,NPOIN
            F(I)=F(I)+DT/HLIN(I)*SMH(I)*(FSCEXP(I)+FN(I)-FC(I))
          ENDDO
        ELSEIF(OPTSOU.EQ.2) THEN
          DO I=1,NPOIN
         F(I)=F(I)+DT/HLIN(I)*UNSV2D(I)*SMH(I)*(FSCEXP(I)+FN(I)-FC(I))
          ENDDO
        ENDIF
      ENDIF
C
C ON THE DIRICHLET BOUNDARIES, FLUX TERMS TAKEN INTO ACCOUNT
C ON OTHERS, FBOR IS TAKEN AS FN, SO NO CONTRIBUTION
C
      DO I=1,NPTFR
        IF(LIMTRA(I).EQ.KDIR) THEN
          N=NBOR(I)
          F(N)=F(N)-DT/HLIN(N)*UNSV2D(N)*FXBORPAR(N)*(FBOR(I)-FC(N))
        ELSEIF(LIMTRA(I).EQ.KDDL) THEN
          N=NBOR(I)
          FLBORTRA(I)=FLBORTRA(I)+FXBOR(I)*FC(N)*SURNIT
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C