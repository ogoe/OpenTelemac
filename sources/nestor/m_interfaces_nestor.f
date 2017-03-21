!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      MODULE  m_Interfaces_Nestor

      !> To benefit from the Fortran INTERFACE feature (parameter list checking)
      !  without the disadvantage of "double coding" we automatically
      !  generate interfaces to subroutines by using the preprocessor
      !
      !  0. start the INTERFACE block:  INTERFACE subroutine_name
      !  1. include the subroutine
      !  2. using the preprocessor we make
      !         all declarations of local variable and
      !         all instructions
      !     invisible to the compiler
      !  3. code that will be seen by the compiler is what belongs
      !     to a interface block
      !  4. end the INTERFACE block:  END INTERFACE

      USE m_typedefs_interface
      USE m_typedefs_nestor
      USE m_nestor


#define NESTOR_INTERFACES

      INTERFACE CalcDigVolumeInRadius
#include "calcdigvolumeinradius.f"
      END INTERFACE

      INTERFACE Calculate_PlanarLevel
#include "calculate_planarlevel.f"
      END INTERFACE

      INTERFACE DateStringToSeconds
#include "datestringtoseconds.f"
      END INTERFACE

      INTERFACE Diff_Time
#include "diff_time.f"
      END INTERFACE

      INTERFACE Dig_by_Criterion
#include "dig_by_criterion.f"
      END INTERFACE

      INTERFACE Dig_by_Time
#include "dig_by_time.f"
      END INTERFACE

      INTERFACE Dump_by_Rate
#include "dump_by_rate.f"
      END INTERFACE

      INTERFACE Dump_by_Time
#include "dump_by_time.f"
      END INTERFACE

      INTERFACE ErrMsgAndStop
#include "errmsgandstop.f"
      END INTERFACE

      INTERFACE InfoMessage
#include "infomessage.f"
      END INTERFACE

      INTERFACE InitialiseNestor
#include "initialisenestor.f"
      END INTERFACE

      INTERFACE inside_point_2d_d
#include "inside_point_2d_d.f"
      END INTERFACE

      INTERFACE InterFaceInitNestor
#include "interfaceinitnestor.f"
      END INTERFACE

      INTERFACE InterFaceRunNestor
#include "interfacerunnestor.f"
      END INTERFACE

      INTERFACE Intersection
#include "intersection.f"
      END INTERFACE

      INTERFACE Intpol_Z_angular_Profils
#include "intpol_z_angular_profils.f"
      END INTERFACE

      INTERFACE Intpol_Z_parallel_Profils
#include "intpol_z_parallel_profils.f"
      END INTERFACE

      INTERFACE IsActionCompletelyDefined
#include "isactioncompletelydefined.f"
      END INTERFACE

      INTERFACE MainNestor
#include "mainnestor.f"
      END INTERFACE

      INTERFACE my_FLUSH
#include "my_flush.f"
      END INTERFACE

      INTERFACE open_File
#include "open_file.f"
      END INTERFACE

      INTERFACE ParseSteerLine
#include "parsesteerline.f"
      END INTERFACE

      INTERFACE ReadActionToRestart
#include "readactiontorestart.f"
      END INTERFACE

      INTERFACE ReadDigActions
#include "readdigactions.f"
      END INTERFACE

      INTERFACE ReadFieldToRestart
#include "readfieldtorestart.f"
      END INTERFACE

      INTERFACE ReadPolygons
#include "readpolygons.f"
      END INTERFACE

      INTERFACE ReadWriteRestart
#include "readwriterestart.f"
      END INTERFACE

      INTERFACE Set_ReflevelByProfiles
#include "set_reflevelbyprofiles.f"
      END INTERFACE

      INTERFACE ThreeDigitsNumeral
#include "threedigitsnumeral.f"
      END INTERFACE

      INTERFACE WriteActionToRestart
#include "writeactiontorestart.f"
      END INTERFACE

      INTERFACE WriteDigAction
#include "writedigaction.f"
      END INTERFACE

      INTERFACE WriteField
#include "writefield.f"
      END INTERFACE

      INTERFACE WriteFieldToRestart
#include "writefieldtorestart.f"
      END INTERFACE



#undef NESTOR_INTERFACES

!***                                              ********************************************
!***                                              ********************************************
      END MODULE  m_Interfaces_Nestor            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

