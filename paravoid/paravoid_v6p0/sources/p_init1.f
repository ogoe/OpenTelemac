C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES PARAMETERS FOR TOMAWAC.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IPID, IREP, ITID, NCSIZE, NPTIR, REP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> TOMAPA : PREPA, NPID1, NDEMM
!>   </td></tr>
!>     </table>

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
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 11/12/1997                                              </td>
!>    <td> F MARCOS (LNH)                                          </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 17/12/1996                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 08/06/1996                                              </td>
!>    <td> REINHARD HINKELMANN (HANOVER)                           </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> **/06/1996                                              </td>
!>    <td> HANS HERRMANN (HANOVER)                                 </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IPID
!></td><td>---</td><td>PROCESSOR NUMBER
!>    </td></tr>
!>          <tr><td>IREP
!></td><td>--></td><td>NOMBRE MAX DE CARACTERE DE LA CHAINE REP
!>    </td></tr>
!>          <tr><td>ITID
!></td><td>---</td><td>PROCESS NUMBER FIELD (NOT NEEDED ON NCUBE)
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>---</td><td>NUMBER OF PROCESSORS
!>    </td></tr>
!>          <tr><td>NPTIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>REP
!></td><td>--></td><td>NOM DU REPERTOIRE DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE P_INIT1
     &(REP,IREP,IPID,NCSIZE,ITID,NPTIR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IPID           |---| PROCESSOR NUMBER
C| IREP           |-->| NOMBRE MAX DE CARACTERE DE LA CHAINE REP
C| ITID           |---| PROCESS NUMBER FIELD (NOT NEEDED ON NCUBE)
C| NCSIZE         |---| NUMBER OF PROCESSORS
C| NPTIR          |---| 
C| REP            |-->| NOM DU REPERTOIRE DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPTIR,IPID,NCSIZE,ITID(0:255),NPID1,NDEMM
      LOGICAL PREPA
      COMMON/TOMAPA/PREPA,NPID1,NDEMM
C
      CHARACTER*144 REP
      INTEGER IREP
C
      REP = ' '
      IREP = 0
      NCSIZE = 1
      NPTIR=0
      IPID=0
      ITID(0)=0
C
C     IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_INIT1 VERSION VIDE'
C     IF(LNG.EQ.2) WRITE(LU,*) 'CALLING P_INIT1 IN ITS VOID VERSION'
C
C-----------------------------------------------------------------------
C
       RETURN
       END
C
C#######################################################################
C