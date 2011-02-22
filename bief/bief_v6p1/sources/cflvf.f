C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE MAXIMUM TIMESTEP THAT ENABLES
!>                MONOTONICITY IN THE ADVECTION STEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, DTMAX, FXBOR, FXMAT, FXMATPAR, GLOSEG, H, HSTART, MAS, MASKPT, MESH, MSK, NPOIN, NPTFR, NSEG, SIZGLO, SMH, TAB1, YASMH
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, B, DENOM, I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CFLVF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM()
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
!> </td><td> 30/11/2009
!> </td><td> C-T PHAM (LNHE) 01 30 87 85 93
!> </td><td> REFINED COMPUTATION OF DTMAX (AS IN 3D)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/10/2008
!> </td><td> JMH
!> </td><td> PARALLEL MODE (ADDED FXMATPAR, ETC.)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 10/06/2008
!> </td><td> JMH
!> </td><td> ADDED SIZGLO
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/04/2008
!> </td><td> JMH
!> </td><td> ADDED YASMH
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME STEP
!>    </td></tr>
!>          <tr><td>DTMAX
!></td><td><--</td><td>MAXIMUM TIME STEP FOR STABILITY
!>    </td></tr>
!>          <tr><td>FXBOR
!></td><td>--></td><td>BOUNDARY FLUXES
!>    </td></tr>
!>          <tr><td>FXMAT
!></td><td>--></td><td>FLUXES
!>    </td></tr>
!>          <tr><td>FXMATPAR
!></td><td>--></td><td>FLUXES ASSEMBLED IN PARALLEL
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>H AT THE END OF FULL TIME STEP
!>    </td></tr>
!>          <tr><td>HSTART
!></td><td>--></td><td>H AT BEGINNING OF SUB TIME STEP
!>    </td></tr>
!>          <tr><td>MAS
!></td><td>--></td><td>INTEGRAL OF TEST FUNCTIONS (=AREA AROUND POINTS)
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MESH STRUCTURE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NUMBER OF SEGMENTS
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>--></td><td>FIRST DIMENSION OF GLOSEG
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>--></td><td>RIGHT HAND SIDE OF CONTINUITY EQUATION
!>    </td></tr>
!>          <tr><td>TAB1
!></td><td>--></td><td>WORK ARRAY
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>--></td><td>IF YES, TAKE SHM INTO ACCOUNT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CFLVF
     &(DTMAX,HSTART,H,FXMAT,FXMATPAR,MAS,DT,FXBOR,SMH,YASMH,TAB1,NSEG,
     & NPOIN,NPTFR,GLOSEG,SIZGLO,MESH,MSK,MASKPT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| TIME STEP
C| DTMAX          |<--| MAXIMUM TIME STEP FOR STABILITY
C| FXBOR          |-->| BOUNDARY FLUXES
C| FXMAT          |-->| FLUXES
C| FXMATPAR       |-->| FLUXES ASSEMBLED IN PARALLEL
C| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
C| H             |-->| H AT THE END OF FULL TIME STEP
C| HSTART         |-->| H AT BEGINNING OF SUB TIME STEP
C| MAS            |-->| INTEGRAL OF TEST FUNCTIONS (=AREA AROUND POINTS)
C| MASKPT         |---| 
C| MESH           |-->| MESH STRUCTURE
C| MSK            |---| 
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
C| NSEG           |-->| NUMBER OF SEGMENTS
C| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
C| SMH            |-->| RIGHT HAND SIDE OF CONTINUITY EQUATION
C| TAB1           |-->| WORK ARRAY
C| YASMH          |-->| IF YES, TAKE SHM INTO ACCOUNT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CFLVF => CFLVF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,SIZGLO
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      DOUBLE PRECISION, INTENT(INOUT) :: DTMAX
      DOUBLE PRECISION, INTENT(IN)    :: DT,HSTART(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: H(NPOIN),MAS(NPOIN),SMH(NPOIN)
C                                              NOT NPTFR, SEE TRACVF
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH,MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TAB1
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION DENOM,A,B
C
C-----------------------------------------------------------------------
C
C COMPUTES THE CRITERION FOR COURANT NUMBER
C
      DO I = 1,NPOIN
        TAB1%R(I) = 0.D0
      ENDDO
C
C USES HERE FXMAT ASSEMBLED IN PARALLEL FOR UPWINDING
C
      DO I = 1,NSEG
        IF(FXMATPAR(I).LT.0.D0) THEN
          TAB1%R(GLOSEG(I,1)) = TAB1%R(GLOSEG(I,1)) + FXMAT(I)
        ELSEIF(FXMATPAR(I).GT.0.D0) THEN
          TAB1%R(GLOSEG(I,2)) = TAB1%R(GLOSEG(I,2)) - FXMAT(I)
        ENDIF
      ENDDO
C
      IF(NCSIZE.GT.1) CALL PARCOM(TAB1,2,MESH)
C
C     MASKS TAB1
C
      IF(MSK) THEN
        CALL OS('X=XY    ',X=TAB1,Y=MASKPT)
      ENDIF
C
C STABILITY (AND MONOTONICITY) CRITERION
C
C NOTE THAT TAB1(I)<0 MIN(FXBOR(I),0.D0)<0 AND -MAX(SMH(I),0.D0)<0
C           SO ABS(TAB1(I)+MIN(FXBOR(I),0.D0)-MAX(SMH(I),0.D0))=
C              -(TAB1(I)+MIN(FXBOR(I),0.D0)-MAX(SMH(I),0.D0))
C
C     ANY TIME LARGER THAN THE REMAINING DT
      DTMAX = 2.D0*DT
C
C     SEE RELEASE NOTES 5.7, CRITERION AT THE END OF 4.4 PAGE 33
C     BUT HERE THE FINAL H IS NOT H(N+1) BUT A FUNCTION OF DTMAX ITSELF
C     H FINAL = HSTART + DTMAX/DT *(H-HSTART)
C
      IF(YASMH) THEN
        DO I = 1,NPOIN
          DENOM=TAB1%R(I)+MIN(FXBOR(I),0.D0)-MAX(SMH(I),0.D0)
          A=-MAS(I)/MIN(DENOM,-1.D-12)
          B=DT+A*(HSTART(I)-H(I))
          IF(B.GT.0.D0) THEN
            DTMAX = MIN(DTMAX,A*HSTART(I)*DT/B)
          ENDIF
        ENDDO
      ELSE
        DO I = 1,NPOIN
          DENOM=TAB1%R(I)+MIN(FXBOR(I),0.D0)
          A=-MAS(I)/MIN(DENOM,-1.D-12)
          B=DT+A*(HSTART(I)-H(I))
          IF(B.GT.0.D0) THEN
            DTMAX = MIN(DTMAX,A*HSTART(I)*DT/B)
          ENDIF
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